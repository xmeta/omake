(*
 * Status printing.
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
open Omake_node
open Omake_exec_id
open Omake_exec_type
open Omake_cache_type
open Omake_options_type

(*
 * Flush the progress meter.
 *)
val print_flush : unit -> unit

(*
 * Print a progress indicator.
 *)
val print_progress : omake_options -> int -> int -> unit

(*
 * Saving the cache messages.
 *)
val print_saving   : omake_options -> unit

(*
 * Directory changes.
 *)
val print_entering_current_directory : omake_options -> Dir.t -> unit
val print_leaving_current_directory  : omake_options -> unit

(*
 * Print a status line.
 *)
val print_status :
   omake_options ->                     (* Options currently in effect *)
   ('exp, 'pid, 'value) shell ->        (* The context *)
   string option ->                     (* Remote host name *)
   string ->                            (* Name of operation being performed *)
   ('exp, 'pid, 'value) print_flag ->   (* What to print *)
   unit

(*
 * Print a status lines.
 *)
val print_status_lines :
   omake_options ->                     (* Options currently in effect *)
   ('exp, 'pid, 'value) shell ->        (* The current shell *)
   string ->                            (* Name of operation being performed *)
   'exp list ->                         (* What to print *)
   unit

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
