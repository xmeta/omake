(*
 * Signatures for the Dir, Node modules.
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
open Omake_marshal

(*
 * Comparisons.
 *)
module type CompareSig =
sig
   type t
   val compare : t -> t -> int
   val compare_alpha : t -> t -> int
end

(*
 * A directory node.
 *)
module type DirSig =
sig
   type t

   (*
    * Some standard directories.
    *)
   val lib  : t
   val home : t
   val root : t

   (*
    * Current working directory.
    *)
   val cwd : unit -> t
   val reset_cwd : unit -> unit

   (*
    * A new directory.
    *)
   val chdir : t -> string -> t

   (*
    * Name of this directory relative to another directory.
    *)
   val name : t -> t -> string

   (*
    * Get the full name relative to the cwd.
    *)
   val fullname : t -> string

   (*
    * Get the absolute name.
    * Try not to use this, except for printing.
    *)
   val absname : t -> string

   (*
    * Check if two directories are equal.
    *)
   val equal : t -> t -> bool

   (*
    * Marshaling.
    *)
   val marshal : t -> msg
   val unmarshal : msg -> t
end

(*
 * Mount flags.
 *)
type mount_option =
   MountCopy
 | MountLink

(*
 * A "mount" specifies a virtual search path for files.
 * It doesn't specify directories.
 *)
module type MountSig =
sig
   type node
   type dir
   type t

   type mount_info =
      { mount_file_exists : node -> bool;
        mount_file_reset  : node -> unit;
        mount_is_dir      : node -> bool;
        mount_stat        : node -> Unix.LargeFile.stats;
        mount_digest      : node -> string option
      }

   (*
    * Default mount.
    *)
   val create : mount_info -> t

   (*
    * Virtual mount of one directory onto another.
    *    mount src dst copy
    *       src: source directory
    *       dst: target directory
    *       copy: if true, auto-copy the files from the source to the target
    *)
   val mount : t -> mount_option list -> dir -> dir -> t
end

(*
 * Generic kinds of nodes.
 *)
type node_kind =
   NodePhony
 | NodeOptional
 | NodeExists
 | NodeSquashed
 | NodeScanner
 | NodeNormal

type phony_ok =
   PhonyOK             (* It is OK to return a phony node *)
 | PhonyExplicit       (* It is OK to return a phony node only when an explicit syntax was used *)
 | PhonyProhibited     (* Returning phony nodes is not allowed *)

(*
 * A file node.  There are two kinds of nodes.
 * "Phony" nodes do not correspond to files.
 * Regular file nodes have a directory and
 * filename part.
 *)
module type NodeSig =
sig
   type t
   type dir
   type db
   type mount

   (*
    * Get a set of all the important nodes for marshaling.
    *)
   val marshal_base : unit -> db
   val unmarshal_base : db -> unit

   (*
    * Build a regular filename from a directory and string.
    * It is legal for the string to contain / chars.
    *)
   val intern : mount -> phony_ok -> dir -> string -> t

   (*
    * Escape a node.
    *)
   val escape : node_kind -> t -> t

   (*
    * What kind of node is this?
    *)
   val kind : t -> node_kind

   (*
    * Get the node, without any modifiers.
    *)
   val core : t -> t

   (*
    * Get the core node if the node is squashed.
    *)
   val unsquash : t -> t

   (*
    * Does the node always exist?
    *)
   val always_exists : t -> bool

   (*
    * Is this node phony?
    * This is derivable from the "kind" function.
    *)
   val is_phony : t -> bool

   (*
    * Is this node a real file?
    * This is derivable from the "kind" function.
    *)
   val is_real : t -> bool

   (*
    * Get the filename relative to a directory,
    * in escaped format.
    *)
   val name : dir -> t -> string

   (*
    * Get the filename relative to the cwd, in escaped format.
    *)
   val fullname : t -> string

   (*
    * Get the absolute name in escaped format.
    *)
   val absname : t -> string

   (*
    * A phony node does not correspond to a file.
    *)
   val phony_global : string -> t

   (*
    * A phony entry in a directory.  The string is not a path.
    *)
   val phony_dir : dir -> string -> t
   val phony_chdir : t -> dir -> t

   (*
    * Build a phony node based on a file.
    *)
   val phony_node : t -> string -> t

   (*
    * Equality testing.
    *)
   val equal : t -> t -> bool
   val compare : t -> t -> int
   val compare_alpha : t -> t -> int

   (*
    * Just the tail.
    *)
   val tail : t -> string

   (*
    * Directory of the node.
    *)
   val dir : t -> dir

   (*
    * Hash code for a node.
    *)
   val hash : t -> int

   (*
    * In some cases, you may need to use a directory
    * where a node is expected.  This produces the node
    * dir/.
    *)
   val node_of_dir : dir -> t

   (*
    * Marshaling.
    *)
   val marshal : t -> msg
   val unmarshal : msg -> t
end

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
