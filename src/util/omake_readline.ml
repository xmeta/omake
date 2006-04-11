(*
 * Simple readline implementation.
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
external flush           : unit -> unit                          = "omake_readline_flush"
external isatty          : unit -> bool                          = "omake_isatty"
external is_interactive  : unit -> bool                          = "omake_is_interactive"
external set_interactive : bool -> unit                          = "omake_interactive"
external init            : unit -> unit                          = "omake_readline_init"
external where           : unit -> int                           = "omake_where_history"
external history         : unit -> string array                  = "omake_readline_history"
external load            : string -> unit                        = "omake_readline_load_file"
external save            : unit -> unit                          = "omake_readline_save_file"
external set_length      : int -> unit                           = "omake_readline_set_length"
external set_directory   : string -> unit                        = "omake_readline_set_directory"

let () = init ()

external ext_readline    : string -> string                      = "omake_readline"
external ext_readstring  : string -> string -> int -> int -> int = "omake_readstring"

let readline s =
   Lm_thread_pool.blocking_section ext_readline s

let readstring s buf off len =
   Lm_thread_pool.blocking_section (ext_readstring s buf off) len

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
