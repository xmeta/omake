(*
 * Shell expressions.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
open Lm_location
open Lm_symbol
open Lm_debug
open Lm_printf

open Omake_node

(*
 * A shell command.
 *)
type exe =
   ExeDelayed
 | ExeNode of Node.t
 | ExeString of string
 | ExeQuote of string

type 'arg redirect =
   RedirectNode of Node.t
 | RedirectArg of 'arg
 | RedirectNone

type 'arg poly_cmd =
   { cmd_loc     : loc;
     cmd_env     : (symbol * 'arg) list;
     cmd_exe     : exe;
     cmd_argv    : 'arg list;
     cmd_stdin   : 'arg redirect;
     cmd_stdout  : 'arg redirect;
     cmd_stderr  : bool;
     cmd_append  : bool
   }

(*
 * An internal command.
 *
 * 'apply with be: venv -> Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> string list -> int * value
 *)
type ('arg, 'apply) poly_apply =
   { apply_loc      : loc;
     apply_name     : symbol;
     apply_fun      : 'apply;
     apply_args     : 'arg list;
     apply_stdin    : 'arg redirect;
     apply_stdout   : 'arg redirect;
     apply_stderr   : bool;
     apply_append   : bool
   }

(*
 * A pipe may have several cmds in sequence.
 *)
type pipe_op =
   PipeAnd
 | PipeOr
 | PipeSequence

(*
 * A pipe with redirection.
 *)
type ('arg, 'apply) poly_group =
   { group_stdin    : 'arg redirect;
     group_stdout   : 'arg redirect;
     group_stderr   : bool;
     group_append   : bool;
     group_pipe     : ('arg, 'apply) poly_pipe
   }

and ('arg, 'apply) poly_pipe =
   PipeApply      of loc * ('arg, 'apply) poly_apply
 | PipeCommand    of loc * 'arg poly_cmd
 | PipeCond       of loc * pipe_op * ('arg, 'apply) poly_pipe * ('arg, 'apply) poly_pipe
 | PipeCompose    of loc * bool * ('arg, 'apply) poly_pipe * ('arg, 'apply) poly_pipe
 | PipeGroup      of loc * ('arg, 'apply) poly_group
 | PipeBackground of loc * ('arg, 'apply) poly_pipe

(*
 * Signals.
 *)
type signal =
   SigAbrt
 | SigAlrm
 | SigFPE
 | SigHup
 | SigIll
 | SigInt
 | SigKill
 | SigPipe
 | SigQuit
 | SigSegv
 | SigTerm
 | SigUsr1
 | SigUsr2
 | SigChld
 | SigCont
 | SigStop
 | SigTstp
 | SigTtin
 | SigTtou
 | SigVTAlrm
 | SigProf
 | SigNum of int

(*
 * Debug flag.
 *)
let debug_shell =
   create_debug (**)
      { debug_name = "shell";
        debug_description = "print debugging information for the shell";
        debug_value = false
      }

(*
 * Operators.
 *)
let pp_print_pipe_op buf op =
   let s =
      match op with
         PipeAnd -> "&&"
       | PipeOr -> "||"
       | PipeSequence -> ";"
   in
      pp_print_string buf s

(*
 * Parameterized printing.
 *)
module type PrintArgSig =
sig
   type arg
   val pp_print_arg : formatter -> arg -> unit
end;;

module MakePrintPipe (PrintArg : PrintArgSig) =
struct
   open PrintArg

   (*
    * Print redirects.
    *)
   let pp_print_stdin buf stdin =
      match stdin with
         RedirectNode node ->
            fprintf buf " < %a" pp_print_node node
       | RedirectArg name ->
            fprintf buf " < %a" pp_print_arg name
       | RedirectNone ->
            ()

   let token_of_stdout stderr append =
      match stderr, append with
         true, true   -> ">>&"
       | true, false  -> ">&"
       | false, true  -> ">>"
       | false, false -> ">"

   let pp_print_stdout buf (stdout, stderr, append) =
      match stdout with
         RedirectNode name ->
            let dir = token_of_stdout stderr append in
               fprintf buf " %s %a" dir pp_print_node name
       | RedirectArg name ->
            let dir = token_of_stdout stderr append in
               fprintf buf " %s %a" dir pp_print_arg name
       | RedirectNone ->
            ()

   (*
    * Print the argument lists.
    *)
   let pp_print_args buf args =
      List.iter (fun arg ->
            fprintf buf " %a" pp_print_arg arg) args

   let rec pp_print_argv buf argv =
      match argv with
         [arg] ->
            pp_print_arg buf arg
       | arg :: argv ->
            pp_print_arg buf arg;
            pp_print_char buf ' ';
            pp_print_argv buf argv
       | [] ->
            ()

   (*
    * Print the environment.
    *)
   let pp_print_env buf env =
      List.iter (fun (v, arg) ->
            fprintf buf "%a=%a " pp_print_symbol v pp_print_arg arg) env

   (*
    * Executable.
    *)
   let pp_print_exe buf exe =
      match exe with
         ExeDelayed ->
            pp_print_string buf "<delayed>"
       | ExeNode node ->
            fprintf buf "<node %a>" pp_print_node node
       | ExeString s ->
            pp_print_string buf s
       | ExeQuote s ->
            fprintf buf "\"%s\"" s

   (*
    * An internal function/alias.
    *)
   let pp_print_apply buf apply =
      let { apply_name = f;
            apply_args = args;
            apply_stdin = stdin;
            apply_stdout = stdout;
            apply_stderr = stderr;
            apply_append = append
          } = apply
      in
         fprintf buf "@[<hv 3>%a%a%a%a@]" (**)
            pp_print_symbol f
            pp_print_args args
            pp_print_stdin stdin
            pp_print_stdout (stdout, stderr, append)

   (*
    * Print a command.
    *)
   let pp_print_command buf command =
      let { cmd_exe = exe;
            cmd_env = env;
            cmd_argv = argv;
            cmd_stdin = stdin;
            cmd_stdout = stdout;
            cmd_stderr = stderr;
            cmd_append = append
          } = command
      in
         fprintf buf "@[<hv 3>%a%a%a%a@]" (**)
            pp_print_env env
            pp_print_argv argv
            pp_print_stdin stdin
            pp_print_stdout (stdout, stderr, append)

   (*
    * Print a pipe.
    *)
   let rec pp_print_pipe buf pipe =
      match pipe with
         PipeApply (_, apply) ->
            pp_print_apply buf apply
       | PipeCommand (_, command) ->
            pp_print_command buf command
       | PipeCond (_, op, pipe1, pipe2) ->
            fprintf buf "@[<hv 3>%a@ %a %a@]" (**)
               pp_print_pipe pipe1
               pp_print_pipe_op op
               pp_print_pipe pipe2
       | PipeCompose (_, divert_stderr, pipe1, pipe2) ->
            fprintf buf "@[<hv 3>%a@ %s %a@]" (**)
               pp_print_pipe pipe1
               (if divert_stderr then "|&" else "|")
               pp_print_pipe pipe2
       | PipeGroup (_, group) ->
            pp_print_group buf group
       | PipeBackground (_, pipe) ->
            fprintf buf "%a &" pp_print_pipe pipe

   and pp_print_group buf group =
      let { group_stdin  = stdin;
            group_stdout = stdout;
            group_stderr = stderr;
            group_append = append;
            group_pipe   = pipe
          } = group
      in
         fprintf buf "@[<hv 3>(%a)%a%a@]" (**)
            pp_print_pipe pipe
            pp_print_stdin stdin
            pp_print_stdout (stdout, stderr, append)
end

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
