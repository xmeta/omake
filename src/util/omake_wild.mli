(*
 * Utilities, mostly on filenames.
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

(*
 * Wildcard string.
 *)
val wild_string : string

(*
 * Wildcard matching.
 *)
type wild
type wild_subst
type wild_value

(*
 * Printing.
 *)
val pp_print_wild : formatter -> wild -> unit

(*
 * Check if a string is a wild pattern.
 *)
val is_wild : string -> bool

(*
 * Compile a pattern.
 *)
val wild_compile : string -> wild

(*
 * Perform a match.  Returns None if there
 * was no match.
 *)
val wild_matches : wild -> string -> bool
val wild_match : wild -> string -> wild_subst option
val wild_core : wild_subst -> string
val wild_of_core : string -> wild_subst

(*
 * Perform a substitution.
 *)
val wild_subst : wild_subst -> wild -> string

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
