(*
 * Command utilities.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Jason Hickey, Caltech
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

open Fmarshal

open Omake_node
open Omake_marshal
open Omake_ir_print
open Omake_shell_type
open Omake_command_type

(************************************************************************
 * Command utilities
 *)

(*
 * Parse the command lines from the strings.
 *)
let parse_command venv dir target loc flags line =
   { command_loc    = loc;
     command_dir    = dir;
     command_target = target;
     command_flags  = flags;
     command_venv   = venv;
     command_inst   = line
   }

let parse_commands venv dir target loc lines =
   let lines =
      List.fold_left (fun lines (flags, line) ->
            match line with
               CommandPipe (PipeCommand (_, { cmd_argv = [] })) ->
                  lines
             | _ ->
                  parse_command venv dir target loc flags line :: lines) [] lines
   in
      List.rev lines

(*
 * Allow output in the command.
 *)
let command_allow_output command =
   { command with command_flags = AllowOutputFlag :: command.command_flags }

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
