(*
 * A token-preserving lexer for the shell.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2006 Mojave Group, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_printf

open Omake_env
open Omake_eval
open Omake_shell_type
open Omake_shell_parse
open Omake_command_type

module Pos = MakePos (struct let name = "Omake_shell_lex" end);;
open Pos;;

(************************************************************************
 * Lexing.
 *)

(*
 * Locations.
 *)
let shell_sym = Lm_symbol.add "shell"

let syntax_error s loc =
   raise (OmakeException (loc_exp_pos loc, SyntaxError s))

(*
 * Tokenizer.
 *)
let flatten_items items loc =
   let rec collect tokens vl vll =
      match vl, vll with
         v :: vl, _ ->
            (match v with
                Omake_env.TokGroup vl' ->
                   collect tokens vl' (vl :: vll)
              | Omake_env.TokString v ->
                   collect (v :: tokens) vl vll
              | Omake_env.TokToken s ->
                   syntax_error ("illegal token: " ^ s) loc)
       | [], vl :: vll ->
            collect tokens vl vll
       | [], [] ->
            List.rev tokens
   in
      collect [] items []

let lex_tok token loc =
   match token with
      Omake_env.TokToken s ->
         (match s with
             "<"  -> TokLessThan (s, loc)
           | ">"  -> TokGreaterThan (s, loc)
           | ">>" -> TokGreaterGreaterThan (s, loc)
           | "&"  -> TokAmp (s, loc)
           | ";"  -> TokSemiColon (s, loc)
           | "&&" -> TokAnd (s, loc)
           | "|"  -> TokPipe (s, loc)
           | "||" -> TokOr (s, loc)
           | "("  -> TokLeftParen (s, loc)
           | ")"  -> TokRightParen (s, loc)
           | _    -> syntax_error ("illegal operator: " ^ s) loc)
    | Omake_env.TokGroup items ->
         TokValues (flatten_items items loc, loc)
    | Omake_env.TokString v ->
         TokValues ([v], loc)

(*
 * Token buffer.
 *)
type lexinfo =
   { mutable lex_tokens : Omake_env.tok list;
     mutable lex_pos    : int;
     lex_loc            : Lm_location.loc
   }

let create_lexinfo loc tokens =
   { lex_tokens = tokens;
     lex_pos    = 1;
     lex_loc    = loc
   }

let lex_main lexinfo _lexbuf =
   let { lex_tokens = tokens;
         lex_pos    = pos;
         lex_loc    = loc
       } = lexinfo
   in
      match tokens with
         [] ->
            TokEof loc
       | token :: tokens ->
            let tok = lex_tok token loc in
               lexinfo.lex_tokens <- tokens;
               lexinfo.lex_pos <- pos + 1;
               tok

(*
 * Lexer from a token list.
 *)
let lexbuf = Lexing.from_string "dummy lexbuf"

let parse loc tokens =
   let lexinfo = create_lexinfo loc tokens in
      try Omake_shell_parse.prog (lex_main lexinfo) lexbuf with
         Parsing.Parse_error ->
            syntax_error "parse error" loc

(************************************************************************
 * Token lexer.
 *)
let check_next c s off len =
   let off = succ off in
      if off < len && s.[off] = c then
         2
      else
         1

let lexer s off len =
   match s.[off] with
      '<'
    | '('
    | ')'
    | ';' ->
         Some 1
    | '&' ->
         Some (check_next '&' s off len)
    | '|' ->
         Some (check_next '|' s off len)
    | '>' ->
         Some (check_next '>' s off len)
    | _ ->
         None

(************************************************************************
 * Strip the initial flags.
 *)
let collect_flags toks =
   let rec collect_flags flags toks =
      match toks with
         tok :: toks' ->
            (match tok with
                TokString (ValString s) ->
                   let len = String.length s in
                   let rec scan flags i =
                      if i = len then
                         collect_flags flags toks'
                      else
                         match s.[i] with
                            '@'  -> scan (QuietFlag :: flags) (succ i)
                          | '-'  -> scan (AllowFailureFlag :: flags) (succ i)
                          | '+'  -> scan flags (succ i)
                          | _    ->
                               let toks =
                                  if succ i = len then
                                     toks'
                                  else
                                     TokString (ValString (String.sub s i (len - i))) :: toks'
                               in
                                  flags, toks
                   in
                      scan flags 0
              | _ ->
                   flags, toks)
       | [] ->
            flags, toks
   in
      collect_flags [] toks

(************************************************************************
 * Command-line parsing.
 *)
let rec flatten_value v =
   match v with
      ValArray [v]
    | ValSequence [v] ->
         flatten_value v
    | v ->
         v

let flatten_values vl =
   match vl with
      [v] ->
         [flatten_value v]
    | _ ->
         vl

let arg_of_redirect venv pos v =
   match v with
      RedirectArg v ->
         (match flatten_values v with
             [ValNode node] ->
                RedirectNode node
           | v ->
                RedirectArg (arg_of_values venv pos v))
    | RedirectNode _
    | RedirectNone as v ->
         v

(*
 * When parsing the command line, collect all environment definitions.
 *)
let scan_define arg =
   match arg with
      ArgString s :: args ->
         (try
             let i = String.index s '=' in
             let v = Lm_symbol.add (String.sub s 0 i) in
             let i = succ i in
             let len = String.length s in
             let args =
                if i = len then
                   args
                else
                   ArgString (String.sub s i (len - i)) :: args
             in
                Some (v, args)
          with
             Not_found ->
                None)
    | _ ->
         None

(*
 * For a command, scan forward, collecting the env.
 *)
let rec scan_argv_aux venv pos env argv =
   match argv with
      arg :: argv' ->
         (match flatten_values arg with
             [ValNode node] ->
               env, ExeNode node, argv
           | v ->
                let arg = arg_of_values venv pos v in
                   (match scan_define arg with
                       Some (v, s) ->
                          scan_argv_aux venv pos ((v, s) :: env) argv'
                     | None ->
                          (* exe selection happens after globbing *)
                          env, ExeDelayed, argv))
    | [] ->
         raise (OmakeException (pos, StringError "invalid null command"))

let scan_argv venv pos argv =
   let env, exe, argv = scan_argv_aux venv pos [] argv in
   let env = List.rev env in
   let argv = argv_of_values venv pos argv in
      env, exe, argv

let arg_pipe_command_of_value_pipe_command venv pos info =
   let { cmd_argv    = argv;
         cmd_stdin   = stdin;
         cmd_stdout  = stdout
       } = info
   in
   let env, exe, argv = scan_argv venv pos argv in
      { info with cmd_env = env;
                  cmd_exe = exe;
                  cmd_argv = argv;
                  cmd_stdin = arg_of_redirect venv pos stdin;
                  cmd_stdout = arg_of_redirect venv pos stdout
      }

let arg_pipe_apply_of_value_pipe_apply venv pos info =
   let { apply_args  = args;
         apply_stdin = stdin;
         apply_stdout = stdout
       } = info
   in
      { info with apply_args = argv_of_values venv pos args;
                  apply_stdin = arg_of_redirect venv pos stdin;
                  apply_stdout = arg_of_redirect venv pos stdout
      }

let rec arg_pipe_of_value_pipe venv pos pipe =
   match pipe with
      PipeApply (loc, info) ->
         PipeApply (loc, arg_pipe_apply_of_value_pipe_apply venv pos info)
    | PipeCommand (loc, info) ->
         PipeCommand (loc, arg_pipe_command_of_value_pipe_command venv pos info)
    | PipeCond (loc, op, pipe1, pipe2) ->
         PipeCond (loc, op, arg_pipe_of_value_pipe venv pos pipe1, arg_pipe_of_value_pipe venv pos pipe2)
    | PipeCompose (loc, b, pipe1, pipe2) ->
         PipeCompose (loc, b, arg_pipe_of_value_pipe venv pos pipe1, arg_pipe_of_value_pipe venv pos pipe2)
    | PipeGroup (loc, info) ->
         PipeGroup (loc, arg_pipe_group_of_value_pipe_group venv pos info)
    | PipeBackground (loc, pipe) ->
         PipeBackground (loc, arg_pipe_of_value_pipe venv pos pipe)

and arg_pipe_group_of_value_pipe_group venv pos info =
   let { group_stdin   = stdin;
         group_stdout  = stdout;
         group_pipe    = pipe
       } = info
   in
      { info with group_stdin  = arg_of_redirect venv pos stdin;
                  group_stdout = arg_of_redirect venv pos stdout;
                  group_pipe   = arg_pipe_of_value_pipe venv pos pipe
      }

(*
 * Do the whole command-line parsing process.
 *)
let pipe_of_value venv pos loc v =
   let pos = string_pos "pipe_of_value" pos in
   let argv = tokens_of_value venv pos lexer v in
   let flags, argv = collect_flags argv in
   let pipe = parse loc argv in
   let pipe = arg_pipe_of_value_pipe venv pos pipe in
      flags, pipe

(*
 * Commands with a leading \ are quoted.
 *)
let parse_command_string s =
   let len = String.length s in
      if len <> 0 && s.[0] = '\\' then
         ExeQuote (String.sub s 1 (pred len))
      else
         ExeString s

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
