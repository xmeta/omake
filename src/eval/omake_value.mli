(*
 * Utilities on values.
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
open Lm_glob
open Lm_location

open Omake_ir
open Omake_env
open Omake_node
open Omake_lexer
open Omake_parser

(*
 * If there is only one value,
 * do not create the array.
 *)
val concat_array : value list -> value

(*
 * Concatenate some strings.
 * If there is only 1 value, do not create the array.
 *)
val concat_strings : string list -> value

(*
 * Expand a value so that the outermost constructor
 * is not an application.
 *)
val eval_value        : venv -> pos -> value -> value
val eval_single_value : venv -> pos -> value -> value
val eval_prim_value   : venv -> pos -> value -> value
val eval_object_value : venv -> pos -> obj -> value

val add_object_value  : obj -> value -> obj

(*
 * Convert to a string.
 *)
val string_of_value  : venv -> pos -> value -> string
val strings_of_value : venv -> pos -> value -> string list
val values_of_value  : venv -> pos -> value -> value list

(*
 * Coercions.
 *)
val bool_of_value       : venv -> pos -> value -> bool
val int_of_value        : venv -> pos -> value -> int
val float_of_value      : venv -> pos -> value -> float
val number_of_value     : venv -> pos -> value -> value
val key_of_value        : venv -> pos -> value -> value

val dir_of_value        : venv -> pos -> value -> Dir.t
val file_of_value       : venv -> pos -> value -> Node.t
val node_value_of_value : venv -> pos -> value -> value
val dir_value_of_value  : venv -> pos -> value -> value
val filename_of_value   : venv -> pos -> value -> string

val prim_channel_of_value    : venv -> pos -> value -> prim_channel
val prim_channel_of_var      : venv -> pos -> loc -> var -> prim_channel
val channel_of_var           : venv -> pos -> loc -> var -> Lm_channel.t
val channel_of_value         : venv -> pos -> value -> Lm_channel.t
val in_channel_of_any_value  : venv -> pos -> value -> prim_channel * bool
val out_channel_of_any_value : venv -> pos -> value -> prim_channel * bool

val is_glob_value            : glob_options -> value -> bool
val is_glob_value_list       : glob_options -> value list -> bool

val current_lexer            : venv -> pos -> Lexer.t
val current_parser           : venv -> pos -> Parser.t
val loc_of_value             : venv -> pos -> value -> loc

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
