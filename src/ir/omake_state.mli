(*
 * Configuration.
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
open Omake_options_type

(*
 * Error codes for various actions.
 *)
val signal_error_code   : int
val fork_error_code     : int
val internal_error_code : int
val deadlock_error_code : int
val exn_error_code      : int
val scanner_error_code  : int

(*
 * Name of the database.
 *)
val db_name : string

(*
 * Name of the makefiles.
 *)
val makefile_name : string
val makeroot_name : string
val makeroot_short_name : string
val omake_file_suffix : string

(*
 * Initial options.
 *)
val default_options : omake_options

(*
 * Check if the options are a parallel build.
 *)
val is_parallel : omake_options -> bool

(*
 * Cache management.
 *)
val always_use_dotomake : bool ref
val set_omake_dir : string -> unit

(*
 * Argument specifier.
 *)
val options_spec : (string * omake_options Lm_arg.poly_spec * string) list
val output_spec  : (string * omake_options Lm_arg.poly_spec * string) list

(*
 * Files.
 *)
val lib_dir         : string
val lib_dir_reason  : string
val home_dir        : string
val application_dir : string
val omake_dir       : unit -> string
val db_file         : unit -> string
val history_file    : unit -> string

val omakeinit_file : string
val omakerc_file   : string
val oshrc_file     : string

val get_cache_file  : string -> string -> string * Unix.file_descr
val lock_file       : Unix.file_descr -> Unix.lock_command -> unit

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
