(*
 * Utilities on the command line.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Jason Hickey, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; version 2
 * of the License.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 * 
 * Additional permission is given to link this library with the
 * with the Objective Caml runtime, and to redistribute the
 * linked executables.  See the file LICENSE.OMake for more details.
 *
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_printf

open Om_symbol
open Lm_location

open Omake_node
open Omake_marshal
open Omake_shell_type
open Omake_command_type

(*
 * Argument parser.
 *)
type arg_buffer

val arg_buffer_empty      : arg_buffer
val arg_buffer_add_string : arg_buffer -> string -> arg_buffer
val arg_buffer_add_data   : arg_buffer -> string -> arg_buffer
val arg_buffer_contents   : arg_buffer -> arg_string list

(*
 * Parse commands.
 *)
val parse_commands : 'venv -> Dir.t -> Node.t -> loc ->
   (command_flag list * ('exp, ('exe, 'arg_command, 'arg_apply, 'arg_other, 'apply) poly_pipe, 'value) poly_command_inst) list ->
   ('venv, 'exp, ('exe, 'arg_command, 'arg_apply, 'arg_other, 'apply) poly_pipe, 'value) poly_command_line list

(*
 * Add the output flag.
 *)
val command_allow_output :
   ('venv, 'exp, 'argv, 'value) poly_command_line ->
   ('venv, 'exp, 'argv, 'value) poly_command_line

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
