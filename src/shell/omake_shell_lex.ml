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
open Omake_shell_type
open Omake_shell_parse
open Omake_command_type

module Pos = MakePos (struct let name = "Omake_shell_lex" end);;
open Pos;;

(************************************************************************
 * String parsing.
 *)
let parse_command_string s =
   let len = String.length s in
      if len >= 1 && s.[0] = '\\' then
         ExeQuote (String.sub s 1 (pred len))
      else if len >= 2 && (s.[0] = '\'' && s.[pred len] = '\'' || s.[0] = '"' && s.[pred len] = '"') then
         ExeQuote (String.sub s 1 (len - 2))
      else
         ExeString s

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

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
