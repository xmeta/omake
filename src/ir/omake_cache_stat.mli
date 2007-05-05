(*
 * Case-sensitivity utilities for the cache.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2007 Mojave Group, Caltech
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

(*
 * Mode flags.
 *)
val warn_case_mismatch : bool ref
val check_case         : bool ref

(*
 * The state.
 *)
type t

val create     : unit -> t

(*
 * Statistics.
 *)
val stat_count : t -> int

(*
 * A case-sensitive version of Unix.LargeFile.stat
 *)
val stat : t -> Node.t -> Unix.LargeFile.stats

(*
 * Directories, without case checking.
 *)
val stat_dir_nocheck : t -> Dir.t -> Unix.LargeFile.stats

(*
 * Resolve the real names.
 *)
val real_dir  : t -> Dir.t -> Dir.t
val real_node : t -> Node.t -> Node.t

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
