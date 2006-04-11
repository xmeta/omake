(*
 * Types used to represent commands and the cache.
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
open Lm_location
open Lm_string_set

open Omake_ir
open Omake_node

(* %%MAGICBEGIN%% *)
(*
 * File digest is an option, in case the file does not exist.
 *)
type digest = Digest.t option

(*
 * The memo result is used only for the scanner,
 * whice produces a table of dependencies.
 *)
type memo_result =
   MemoSuccess
 | MemoFailure of int
 | MemoResult of NodeSet.t NodeTable.t

(*
 * Status query.
 *)
type memo_status =
   StatusSuccess
 | StatusFailure of int
 | StatusUnknown

(*
 * A directory entry is a node or directory.
 *)
type dir_entry =
   NodeEntry of Node.t
 | DirEntry of Dir.t
(* %%MAGICEND%% *)

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
