(*
 * Abstract representation of files.
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
open Lm_hash
open Lm_printf

open Omake_node_sig

module Dir : DirSig
module DirCompare : CompareSig with type t = Dir.t
module DirSet : Lm_set_sig.LmSet with type elt = Dir.t
module DirTable : Lm_map_sig.LmMap with type key = Dir.t

module Node : NodeSig with type dir = Dir.t
module NodeCompare : CompareSig with type t = Node.t
module NodeSet : Lm_set_sig.LmSet with type elt = Node.t
module NodeTable : Lm_map_sig.LmMap with type key = Node.t
module NodeMTable : Lm_map_sig.LmMapList with type key = Node.t

module Mount : MountSig with type dir = Dir.t and type node = Node.t and type t = Node.mount;;

val no_mount_points : Mount.t

(*
 * Hash-cons versions.
 *)
module DirHash      : HashConsSig with type elt = Dir.t
module DirHashSet   : Lm_set_sig.LmSet with type elt = DirHash.t
module DirHashTable : Lm_map_sig.LmMap with type key = DirHash.t

module DirListHash  : HashConsSig with type elt = DirHash.t list
module DirListSet   : Lm_set_sig.LmSet with type elt = DirListHash.t
module DirListTable : Lm_map_sig.LmMap with type key = DirListHash.t

(*
 * For debugging.
 *)
val pp_print_dir  : formatter -> Dir.t -> unit
val pp_print_node : formatter -> Node.t -> unit
val pp_print_node_kind : formatter -> node_kind -> unit

val pp_print_string_list : formatter -> string list -> unit
val pp_print_node_set : formatter -> NodeSet.t -> unit
val pp_print_node_list : formatter -> Node.t list -> unit
val pp_print_node_table : formatter -> 'a NodeTable.t -> unit
val pp_print_node_set_table : formatter -> NodeSet.t NodeTable.t -> unit
val pp_print_node_set_table_opt : formatter -> NodeSet.t NodeTable.t option -> unit

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
