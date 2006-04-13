(*
 * Command lines.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Mojave Group, Caltech
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
open Lm_location

open Omake_node

(*
 * Individual command arguments have three forms:
 *    - value lists
 *    - arg_string lists
 *    - string
 *
 * The arg_string is like a string, but various parts of it are quoted.
 *)
type arg_string =
   ArgString of string
 | ArgData   of string

type arg =
   arg_string list

(*
 * Command digest.
 *)
type command_digest = Digest.t option

(*
 * A command line is a string, together with come flags.
 *)
type command_flag =
   QuietFlag
 | AllowFailureFlag
 | AllowOutputFlag

(*
 * The command line has some flags,
 * and a string to be executed internally
 * or passed to the shell.
 *)
type ('exp, 'argv, 'value) poly_command_inst =
   CommandEval   of 'exp
 | CommandPipe   of 'argv
 | CommandValues of 'value list

type ('venv, 'exp, 'argv, 'value) poly_command_line =
   { command_loc    : loc;
     command_dir    : Dir.t;
     command_target : Node.t;
     command_flags  : command_flag list;
     command_venv   : 'venv;
     command_inst   : ('exp, 'argv, 'value) poly_command_inst
   }

(************************************************************************
 * Printing.
 *)
let simple_string_of_arg arg =
   match arg with
      [ArgString s]
    | [ArgData s] ->
         s
    | _ ->
         let buf = Buffer.create 32 in
            List.iter (fun arg ->
                  let s =
                     match arg with
                        ArgString s -> s
                      | ArgData s -> s
                  in
                     Buffer.add_string buf s) arg;
            Buffer.contents buf

let glob_string_of_arg options arg =
   let buf = Buffer.create 32 in
      List.iter (fun arg ->
            match arg with
               ArgString s ->
                  Buffer.add_string buf s
             | ArgData s ->
                  Lm_glob.glob_add_escaped options buf s) arg;
      Buffer.contents buf

let is_glob_arg options arg =
   List.exists (fun arg ->
         match arg with
            ArgString s ->
               Lm_glob.is_glob_string options s
          | ArgData _ ->
               false) arg

let pp_print_arg buf arg =
   pp_print_string buf (glob_string_of_arg Lm_glob.default_glob_options arg)

let pp_print_command_flag buf flag =
   let c =
      match flag with
         QuietFlag        -> '@'
       | AllowFailureFlag -> '-'
       | AllowOutputFlag  -> '*'
   in
      pp_print_char buf c

let pp_print_command_flags buf flags =
   List.iter (pp_print_command_flag buf) flags

module type PrintArgvSig =
sig
   type argv

   val pp_print_argv : formatter -> argv -> unit
end;;

module MakePrintCommand (PrintArgv : PrintArgvSig) =
struct
   open PrintArgv

   let pp_print_command_inst buf inst =
      match inst with
         CommandPipe argv ->
            pp_print_argv buf argv
       | CommandEval _ ->
            pp_print_string buf "<exp>"
       | CommandValues _ ->
            pp_print_string buf "<values>"

   let pp_print_command_line buf line =
      pp_print_command_inst buf line.command_inst

   let pp_print_command_lines buf lines =
      List.iter (fun line -> fprintf buf "@ %a" pp_print_command_line line) lines
end;;

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
