(*
 * This is the base part of the build system.
 * Each file in the system is represented as a node.
 * Node may be virtual: the node may exist before the file
 * does.  For each file, we maintain stat and MD5 information
 * (if they exist).
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2007 Mojave Group, Caltech and HRL Laboratories, LLC
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
 * Author: Jason Hickey <jyh@cs.caltech.edu>
 * Modified by: Aleksey Nogin <nogin@cs.caltech.edu>, <anogin@hrl.com>
 * @end[license]
 *)
open Lm_hash
open Lm_printf
open Lm_symbol
open Lm_hash_sig
open Lm_location
open Lm_string_util
open Lm_filename_util

open Fmarshal

open Omake_state
open Omake_marshal
open Omake_node_sig
open Omake_print_util

(************************************************************************
 * Directories.
 *)

(*
 * Internally, we represent pathnames as absolute paths.
 * We keep a hashed integer for quick equality testing.
 *    dir_root : the root of this name
 *    dir_name : the actual path will full capitalization
 *)
(* %%MAGICBEGIN%% *)
type dir_elt =
   DirRoot of Lm_filename_util.root
 | DirSub of string * dir_elt hash_marshal_item
(* %%MAGICEND%% *)

(*
 * Sets and tables.
 *)
module rec DirCompare : HashMarshalArgSig with type t = dir_elt =
struct
   type t = dir_elt

   let debug = "Dir"

   let hash = function
      DirRoot root ->
         Hashtbl.hash root
    | DirSub (raw_name, parent) ->
         let buf = HashCode.create () in
            HashCode.add_int buf (DirHash.hash parent);
            HashCode.add_string buf raw_name;
            HashCode.code buf

   let rec compare dir1 dir2 =
      match dir1, dir2 with
         DirRoot root1, DirRoot root2 ->
            Pervasives.compare root1 root2
       | DirSub (name1, parent1), DirSub (name2, parent2) ->
            let cmp = Lm_string_util.string_compare name1 name2 in
               if cmp = 0 then
                  DirHash.compare parent1 parent2
               else
                  cmp
       | DirRoot _, DirSub _ ->
            -1
       | DirSub _, DirRoot _ ->
            1

   let reintern dir =
      match dir with
         DirRoot _ ->
            dir
       | DirSub (name1, parent1) ->
            let parent2 = DirHash.reintern parent1 in
               if parent2 == parent1 then
                  dir
               else
                  DirSub (name1, parent2)
end

(* %%MAGICBEGIN%% *)
and DirHash :
   HashMarshalSig
   with type elt = dir_elt
   with type t = dir_elt hash_marshal_item
=
   MakeHashMarshal (DirCompare)

and DirSet   : Lm_set_sig.LmSet with type elt = DirHash.t = Lm_set.LmMake (DirHash)
and DirTable : Lm_map_sig.LmMap with type key = DirHash.t = Lm_map.LmMake (DirHash)

type dir = DirHash.t
(* %%MAGICEND%% *)

(*
 * Lists of directories.
 *)
module rec DirListCompare : HashMarshalArgSig with type t = dir list =
struct
   type t = dir list

   let debug = "DirList"

   let hash l =
      let buf = HashCode.create () in
         List.iter (fun dir -> HashCode.add_int buf (DirHash.hash dir)) l;
         HashCode.code buf

   let rec compare l1 l2 =
      match l1, l2 with
         d1 :: l1, d2 :: l2 ->
            let cmp = DirHash.compare d1 d2 in
               if cmp = 0 then
                  compare l1 l2
               else
                  cmp
       | [], [] ->
            0
       | [], _ :: _ ->
            -1
       | _ :: _, [] ->
            1

   let reintern l =
      Lm_list_util.smap DirHash.reintern l
end

and DirListHash : HashMarshalSig with type elt = dir list =
   MakeHashMarshal (DirListCompare);;

module DirListSet = Lm_set.LmMake (DirListHash);;
module DirListTable = Lm_map.LmMake (DirListHash);;

(************************************************************************
 * Nodes.
 *)

(*
 * Possible node flags.
 *)
(* %%MAGICBEGIN%% *)
type node_flag =
   NodeIsOptional
 | NodeIsExisting
 | NodeIsSquashed
 | NodeIsScanner

(*
 * A node is a phony, or it is a filename.
 *)
type node_elt =
   NodeFile        of dir * string
 | NodePhonyGlobal of string
 | NodePhonyDir    of dir * string
 | NodePhonyFile   of dir * string * string
 | NodeFlagged     of node_flag * node_elt hash_marshal_item
(* %%MAGICEND%% *)

module rec NodeCompare : HashMarshalArgSig with type t = node_elt
=
struct
   type t = node_elt;;

   let debug = "Node"

   type code =
      CodeSpace
    | CodeEnd
    | CodeNodeFile
    | CodeNodePhonyGlobal
    | CodeNodePhonyDir
    | CodeNodePhonyFile
    | CodeNodeFlagged
    | CodeNodeIsOptional
    | CodeNodeIsExisting
    | CodeNodeIsSquashed
    | CodeNodeIsScanner

   let add_code buf (code : code) =
      HashCode.add_bits buf (Obj.magic code)

   let add_flag_code buf code =
      let code =
         match code with
            NodeIsOptional ->
               CodeNodeIsOptional
          | NodeIsExisting ->
               CodeNodeIsExisting
          | NodeIsSquashed ->
               CodeNodeIsSquashed
          | NodeIsScanner ->
               CodeNodeIsScanner
      in
         add_code buf code

   let add_dir buf dir =
      HashCode.add_int buf (DirHash.hash dir )

   let add_node buf node =
      HashCode.add_int buf (NodeHash.hash node)

   let add_filename buf raw_name =
      HashCode.add_string buf raw_name

   let filename_compare = String.compare

   let node_compare = NodeHash.compare

   let dir_compare = DirHash.compare

   let add_node buf node =
      match node with
         NodeFile (dir, raw_name) ->
            add_code buf CodeNodeFile;
            add_dir buf dir;
            add_code buf CodeSpace;
            add_filename buf raw_name;
            add_code buf CodeEnd
       | NodePhonyGlobal name ->
            add_code buf CodeNodePhonyGlobal;
            HashCode.add_string buf name;
            add_code buf CodeEnd
       | NodePhonyDir (dir, raw_name) ->
            add_code buf CodeNodePhonyDir;
            add_dir buf dir;
            add_code buf CodeSpace;
            add_filename buf raw_name;
            add_code buf CodeEnd
       | NodePhonyFile (dir, raw_name, name) ->
            add_code buf CodeNodePhonyFile;
            add_dir buf dir;
            add_code buf CodeSpace;
            add_filename buf raw_name;
            add_code buf CodeSpace;
            HashCode.add_string buf name;
            add_code buf CodeEnd
       | NodeFlagged (flag, node) ->
            add_code buf CodeNodeFlagged;
            add_flag_code buf flag;
            add_code buf CodeSpace;
            add_node buf node;
            add_code buf CodeEnd

   let hash node =
      let buf = HashCode.create () in
         add_node buf node;
         HashCode.code buf

   let compare_flags flag1 flag2 =
      match flag1, flag2 with
         NodeIsOptional, NodeIsOptional
       | NodeIsExisting, NodeIsExisting
       | NodeIsSquashed, NodeIsSquashed
       | NodeIsScanner,  NodeIsScanner ->
            0
       | NodeIsOptional, NodeIsExisting
       | NodeIsOptional, NodeIsSquashed
       | NodeIsOptional, NodeIsScanner
       | NodeIsExisting, NodeIsSquashed
       | NodeIsExisting, NodeIsScanner
       | NodeIsSquashed, NodeIsScanner ->
            -1
       | NodeIsExisting, NodeIsOptional
       | NodeIsSquashed, NodeIsOptional
       | NodeIsScanner,  NodeIsOptional
       | NodeIsSquashed, NodeIsExisting
       | NodeIsScanner,  NodeIsExisting
       | NodeIsScanner,  NodeIsSquashed ->
            1

   let compare node1 node2 =
      match node1, node2 with
         NodeFile (dir1, name1), NodeFile (dir2, name2)
       | NodePhonyDir (dir1, name1), NodePhonyDir (dir2, name2) ->
            let cmp = filename_compare name1 name2 in
               if cmp = 0 then
                  dir_compare dir1 dir2
               else
                  cmp
       | NodePhonyGlobal name1, NodePhonyGlobal name2 ->
            Lm_string_util.string_compare name1 name2
       | NodePhonyFile (dir1, name1, exname1), NodePhonyFile (dir2, name2, exname2) ->
            let cmp = Lm_string_util.string_compare exname1 exname2 in
               if cmp = 0 then
                  let cmp = filename_compare name1 name2 in
                     if cmp = 0 then
                        dir_compare dir1 dir2
                     else
                        cmp
               else
                  cmp
       | NodeFlagged (flag1, node1), NodeFlagged (flag2, node2) ->
            let cmp = compare_flags flag1 flag2 in
               if cmp = 0 then
                  node_compare node1 node1
               else
                  cmp
       | NodeFile _,        NodePhonyGlobal _
       | NodeFile _,        NodePhonyDir _
       | NodeFile _,        NodePhonyFile _
       | NodeFile _,        NodeFlagged _
       | NodePhonyGlobal _, NodePhonyDir _
       | NodePhonyGlobal _, NodePhonyFile _
       | NodePhonyGlobal _, NodeFlagged _
       | NodePhonyDir _,    NodePhonyFile _
       | NodePhonyDir _,    NodeFlagged _
       | NodePhonyFile _,   NodeFlagged _ ->
            -1
       | NodeFlagged _,      NodeFile _
       | NodePhonyGlobal _,  NodeFile _
       | NodePhonyDir _,     NodeFile _
       | NodePhonyFile _,    NodeFile _
       | NodeFlagged _,      NodePhonyGlobal _
       | NodePhonyDir _,     NodePhonyGlobal _
       | NodePhonyFile _,    NodePhonyGlobal _
       | NodeFlagged _,      NodePhonyDir _
       | NodePhonyFile _,    NodePhonyDir _
       | NodeFlagged _,      NodePhonyFile _ ->
            1

   let reintern node =
      match node with
         NodeFile (dir1, name) ->
            let dir2 = DirHash.reintern dir1 in
               if dir2 == dir1 then
                  node
               else
                  NodeFile (dir2, name)
       | NodePhonyDir (dir1, name) ->
            let dir2 = DirHash.reintern dir1 in
               if dir2 == dir1 then
                  node
               else
                  NodePhonyDir (dir2, name)
       | NodePhonyFile (dir1, name1, name2) ->
            let dir2 = DirHash.reintern dir1 in
               if dir2 == dir1 then
                  node
               else
                  NodePhonyFile (dir2, name1, name2)
       | NodePhonyGlobal _ ->
            node
       | NodeFlagged (flag, node1) ->
            let node2 = NodeHash.reintern node1 in
               if node2 == node1 then
                  node
               else
                  NodeFlagged (flag, node2)
end

(* %%MAGICBEGIN%% *)
and NodeHash :
   HashMarshalSig
   with type elt = node_elt
   with type t = node_elt hash_marshal_item
=
   MakeHashMarshal (NodeCompare);;

type node = NodeHash.t
(* %%MAGICEND%% *)

module NodeSet = Lm_set.LmMake (NodeHash);;
module NodeTable = Lm_map.LmMake (NodeHash);;
module NodeMTable = Lm_map.LmMakeList (NodeHash);;

module PreNodeSet = Lm_set.LmMake (NodeCompare);;

(************************************************************************
 * Implementation.
 *)

(*
 * Get a pathname from a directory.
 * The name must be reversed.
 *)
let rec path_of_dir_aux path dir =
   match DirHash.get dir with
      DirRoot root ->
         root, path
    | DirSub (name, parent) ->
         path_of_dir_aux (name :: path) parent

let path_of_dir dir =
   path_of_dir_aux [] dir

(*
 * Build a list of the directories, in reverse order.
 *)
let dir_list_of_dir dir =
   let rec dir_list_of_dir path dir =
      match DirHash.get dir with
         DirRoot _ ->
            dir :: path
       | DirSub (_, parent) ->
            dir_list_of_dir (dir :: path) parent
   in
      dir_list_of_dir [] dir

(*
 * Produce a path (a string list) from the dir list.
 *)
let rec path_of_dir_list dirs =
   match dirs with
      [] ->
         []
    | dir :: dirs ->
         match DirHash.get dir with
            DirSub (name, _) ->
               name :: path_of_dir_list dirs
          | DirRoot _ ->
               raise (Invalid_argument "path_of_dir_list")

(*
 * Make a directory node from the pathname.
 *)
let make_subdir parent name =
   DirHash.create (DirSub (name, parent))

let make_dir root path =
   List.fold_left make_subdir (DirHash.create (DirRoot root)) path

(*
 * Get the current absolute name of the working directory.
 *)
let getcwd () =
   let cwd = Unix.getcwd () in
      match Lm_filename_util.filename_path cwd with
         AbsolutePath (root, dir) ->
            make_dir root dir
       | RelativePath dir ->
            raise (Invalid_argument "Unix.getcwd returned a relative path")

(*
 * A null root directory for globals.
 *)
let null_root =
   make_dir null_root []

(*
 * Split the directory name into a path.
 *)
let rec path_simplify dir = function
   "" :: path
 | "." :: path ->
      path_simplify dir path
 | ".." :: path ->
      let dir =
         match DirHash.get dir with
            DirSub (_, parent) ->
               parent
          | DirRoot _ ->
               dir
      in
         path_simplify dir path
 | [name] ->
      dir, Some name
 | name :: path ->
      path_simplify (make_subdir dir name) path
 | [] ->
      dir, None

let new_path dir path =
   match Lm_filename_util.filename_path path with
      AbsolutePath (root, path) ->
         (* This is an absolute path, so ignore the directory *)
         path_simplify (make_dir root []) path
    | RelativePath path ->
         (* This is relative to the directory *)
         path_simplify dir path

let new_dir dir path =
   match new_path dir path with
      dir, None ->
         dir
    | dir, Some name ->
         make_subdir dir name

let new_file dir path =
   let dir, name = new_path dir path in
      match name with
         Some name ->
            dir, name
       | None ->
            begin match DirHash.get dir with
               DirSub (name, dir) ->
                  dir, name
             | DirRoot _ ->
                  dir, "."
            end

(*
 * Check if .. works in a directory.
 *)
let dotdot_table = ref DirTable.empty

(*
 * Force dotdot to fail.
 *)
let make_dotdot_fail dir =
   dotdot_table := DirTable.add !dotdot_table dir true

let abs_dir_name dir =
   let buf = Buffer.create 17 in
   let rec name dir =
      match DirHash.get dir with
         DirRoot root ->
            Buffer.add_string buf (Lm_filename_util.string_of_root root)
       | DirSub (key, parent) ->
            name parent;
            begin match DirHash.get parent with
               DirRoot _ ->
                  ()
             | _ ->
                  Buffer.add_char buf Lm_filename_util.separator_char
            end;
            Buffer.add_string buf key
   in
   let () = name dir in
      Buffer.contents buf

let dotdot_fails dir =
   let table = !dotdot_table in
      try DirTable.find table dir with
         Not_found ->
            let name = abs_dir_name dir in
            let islink =
               try (Unix.lstat name).Unix.st_kind = Unix.S_LNK with
                  Unix.Unix_error _ ->
                     false
            in
            let table = DirTable.add table dir islink in
               dotdot_table := table;
               islink

(*
 * Produce string filename for the path,
 * relative to a particular directory.
 *
 * Algorithm:
 *    1. Compute the common prefix between the directory and file
 *    2. Add as many ".." as there are remaining names in the directory
 *       and concatenate the rest of the path.
 *
 * Example:
 *       dir = /a/b/c/d
 *       path = /a/b/e/f/g
 *       result = ../../e/f/g
 *)

(*
 * Create a string from the list of strings.
 *)
let rec flatten_generic (add_string : 'a -> string -> 'a) (contents : 'a -> string) (buf : 'a) (path : string list) =
   match path with
      [path] ->
         let buf = add_string buf path in
            contents buf
    | [] ->
         contents buf
    | name :: path ->
         let buf = add_string buf name in
         let buf = add_string buf Lm_filename_util.separator_string in
            flatten_generic add_string contents buf path

(*
 * Add .. to get out of the directory and down into the path.
 *)
let updirs_generic add_string contents buf dirs1 dirs2 =
   (* Abort if any of the dotdots fail *)
   if List.exists dotdot_fails dirs1 then
      None
   else
      (* Prepend the .. sequence *)
      let rec updirs dirs path =
         match dirs with
            _ :: dirs ->
               updirs dirs (".." :: path)
          | [] ->
               path
      in
      let path = path_of_dir_list dirs2 in
      let path = updirs dirs1 path in
         Some (flatten_generic add_string contents buf path)

(*
 * Compute the path of dir2 relative to dir1.
 *)
let rec relocate_generic add_string contents buf (dirs1 : dir list) (dirs2 : dir list) =
   match dirs1, dirs2 with
    | [], _ ->
         Some (flatten_generic add_string contents buf (path_of_dir_list dirs2))
    | _, [] ->
         updirs_generic add_string contents buf dirs1 dirs2
    | dir1 :: dirs1', dir2 :: dirs2'  ->
         if DirHash.equal dir1 dir2 then
            relocate_generic add_string contents buf dirs1' dirs2'
         else
            updirs_generic add_string contents buf dirs1 dirs2

(*
 * If the files differ in the root directory, just use the absolute path.
 *)
let relocate_generic add_string contents buf dir1 dir2 =
   let dirs1 = dir_list_of_dir dir1 in
   let dirs2 = dir_list_of_dir dir2 in
      match dirs1, dirs2 with
         dir1 :: dirs1, dir2 :: dirs2 ->
            (match DirHash.get dir1, DirHash.get dir2 with
                DirRoot root1, DirRoot root2 ->
                   (let s =
                       if dirs1 = [] || root1 <> root2 then
                          None
                       else
                          relocate_generic add_string contents buf dirs1 dirs2
                    in
                       match s with
                          Some s ->
                             s
                        | None ->
                             let buf = add_string buf (Lm_filename_util.string_of_root root2) in
                             let path2 = path_of_dir_list dirs2 in
                                flatten_generic add_string contents buf path2)
              | _ ->
                   raise (Invalid_argument "relocate_generic"))
       | _ ->
            raise (Invalid_argument "relocate_generic")

(*
 * Directory versions.
 *)
let dir_buffer = Buffer.create 17

let dir_add_string buf s =
   Buffer.add_string buf s;
   buf

let dir_contents buf =
   let s = Buffer.contents buf in
      Buffer.clear buf;
      s

let flatten_dir dir =
   flatten_generic dir_add_string dir_contents dir_buffer dir

let relocate_dir dir1 dir2 =
   relocate_generic dir_add_string dir_contents dir_buffer dir1 dir2

(*
 * File version.
 *)
let file_contents name buf =
   let buf =
      if Buffer.length buf = 0 then
         buf
      else
         dir_add_string buf Lm_filename_util.separator_string
   in
   let buf = dir_add_string buf name in
      dir_contents buf

let flatten_file dir name =
   let buf = dir_add_string dir_buffer Lm_filename_util.separator_string in
      flatten_generic dir_add_string (file_contents name) buf dir

let relocate_file dir1 dir2 name =
   if DirHash.equal dir1 dir2 then
      name
   else
      relocate_generic dir_add_string (file_contents name) dir_buffer dir1 dir2

(*
 * Apply a mount point.
 *)
let rec resolve_mount_dir dir_dst dir_src dir =
   if DirHash.compare dir dir_dst = 0 then
      dir_src
   else
      match DirHash.get dir with
         DirRoot _ ->
            raise Not_found
       | DirSub (name, parent) ->
            let parent = resolve_mount_dir dir_dst dir_src parent in
               DirHash.create (DirSub (name, parent))

let rec resolve_mount_node dir_dst dir_src node =
   let node =
      match NodeHash.get node with
         NodeFile (dir, name) ->
            let dir = resolve_mount_dir dir_dst dir_src dir in
               NodeFile (dir, name)
       | NodePhonyDir (dir, name) ->
            let dir = resolve_mount_dir dir_dst dir_src dir in
               NodePhonyDir (dir, name)
       | NodePhonyFile (dir, name1, name) ->
            let dir = resolve_mount_dir dir_dst dir_src dir in
               NodePhonyFile (dir, name1, name)
       | NodePhonyGlobal _ ->
            raise Not_found
       | NodeFlagged (flag, node) ->
            NodeFlagged (flag, resolve_mount_node dir_dst dir_src node)
   in
      NodeHash.create node

(*
 * A name can stand for a global phony only if it has no slashes,
 * or it only leads with a slash.  Raises Not_found if the name
 * contains any non-leading slashes.
 *)
type phony_name =
   PhonyGlobalString of string
 | PhonyDirString of string
 | PhonySimpleString
 | PhonyPathString

(* Starting at position i, s begins with ".PHONY/" *)
let string_prefix_phony s i len =
   len >= i + 7 &&
      s.[i  ] = '.' &&
      s.[i+1] = 'P' &&
      s.[i+2] = 'H' &&
      s.[i+3] = 'O' &&
      s.[i+4] = 'N' &&
      s.[i+5] = 'Y' &&
      (s.[i+6] = '/' || s.[i+6] = '\\')

let rec is_simple_string s len i =
   (i = len) ||
      match s.[i] with
         '/'
       | '\\' ->
            false
       | _ ->
            is_simple_string s len (succ i)

let parse_phony_name s =
   let len = String.length s in
      if len = 0 then
         PhonySimpleString
      else match s.[0] with
         '/'
       | '\\' ->
         if string_prefix_phony s 1 len && is_simple_string s len 8 then
            (* /.PHONY/foo *)
            PhonyGlobalString (String.sub s 8 (len - 8))
         else
            PhonyPathString
       | '.' when string_prefix_phony s 0 len ->
            (* .PHONY/foo/bar *)
            PhonyDirString (String.sub s 7 (len - 7))
       | _ ->
            if is_simple_string s len 1 then
               PhonySimpleString
            else
               PhonyPathString

(************************************************************************
 * Modules.
 *)
module Dir =
struct
   type t = dir

   (*
    * We assume the cwd does not change
    * once we first get it.
    *)
   let cwd_ref =
      let dir =
         try
            getcwd ()
         with Unix.Unix_error (err, _, _) ->
            eprintf "@[<v3>*** omake: warning:@ Can not find out the current directory:@ %s;@ Using the root directory instead.@]@." (Unix.error_message err);
            null_root
      in
         ref dir

   (*
    * Default is current working directory.
    *)
   let cwd () = !cwd_ref

   let reset_cwd () =
      let cwd = getcwd () in
         cwd_ref := getcwd ();
         make_dotdot_fail cwd

   (*
    * Building a new path.
    *)
   let chdir = new_dir

   (*
    * Name, relative to the cwd.
    *)
   let name dir1 dir2 =
      if DirHash.equal dir1 dir2 then
         "."
      else
         relocate_dir dir1 dir2

   (*
    * Name relative to the root.
    *)
   let fullname dir =
      name !cwd_ref dir

   (*
    * Absolute name.
    *)
   let root = null_root

   let absname dir =
      name root dir

   (*
    * Directory representation.
    *)
   type dir_info =
      DirInfoRoot of Lm_filename_util.root
    | DirInfoSub of string * t

   let dest dir =
      match DirHash.get dir with
         DirRoot root -> DirInfoRoot root
       | DirSub (raw_name, dir) -> DirInfoSub (raw_name, dir)

   (*
    * Library directory is relative to the root.
    *)
   let lib =
      match Lm_filename_util.filename_path Omake_state.lib_dir with
         AbsolutePath (root, dir) ->
            make_dir root dir
       | RelativePath _ ->
            raise (Invalid_argument ("Omake_node.lib_dir specified as relative path: " ^ Omake_state.lib_dir))

   (*
    * home directory is also relative to the root.
    *)
   let home =
      match Lm_filename_util.filename_path home_dir with
         AbsolutePath (root, dir) ->
            make_dir root dir
       | RelativePath _ ->
            raise (Invalid_argument ("Omake_node.home_dir specified as relative path: " ^ home_dir))

   let () = make_dotdot_fail home

   (*
    * Equality.
    *)
   let compare = DirHash.compare
   let equal = DirHash.equal

   (*
    * Marshaling.
    *)
   let marshal_root root =
      match root with
         NullRoot ->
            Magic NullRootMagic
       | DriveRoot c ->
            List [Magic DriveRootMagic; Char c]

   let unmarshal_root l =
      match l with
         Magic NullRootMagic ->
            NullRoot
       | List [Magic DriveRootMagic; Char c] ->
            DriveRoot c
       | _ ->
            raise MarshalError

   let rec marshal dir =
      match DirHash.get dir with
         DirRoot root ->
            List [Magic DirRootMagic; marshal_root root]
       | DirSub (name, parent) ->
            List [Magic DirSubMagic; String name; marshal parent]

   let rec unmarshal l =
      let dir =
         match l with
            List [Magic DirRootMagic; root] ->
               DirRoot (unmarshal_root root)
          | List [Magic DirSubMagic; String name; parent] ->
               DirSub (name, unmarshal parent)
          | _ ->
               raise MarshalError
      in
         DirHash.create dir
end;;

(*
 * Virtual mounts.
 * We need a function that checks if a file exists.
 *)
module Mount =
struct
   type t = (Dir.t * Dir.t * mount_option list) list

   type dir_tmp = dir
   type node_tmp = node
   type dir = dir_tmp
   type node = node_tmp

   (*
    * Create a new mount state.
    *)
   let empty = []

   (*
    * Add a mount point.
    *)
   let mount info options dir_src dir_dst =
      (dir_dst, dir_src, options) :: info
end;;

type mount_info = node poly_mount_info

let no_mount_info =
   { mount_file_exists = (fun _ -> false);
     mount_file_reset  = (fun _ -> ());
     mount_is_dir      = (fun _ -> false);
     mount_stat        = (fun _ -> raise (Invalid_argument "no_mount_info"));
     mount_digest      = (fun _ -> None)
   }

(*
 * Nodes.
 *)
module Node =
struct
   type pre   = node_elt
   type t     = node
   type dir   = Dir.t
   type mount = Mount.t

   open Mount;;

   let dest = NodeHash.get

   (*
    * Get the name.
    *)
   let phony_name name =
      "<" ^ name ^ ">"

   (*
    * Name of the node.
    *)
   let rec name dir1 node =
      match NodeHash.get node with
         NodePhonyGlobal name ->
            phony_name name
       | NodePhonyDir (dir2, name) ->
            phony_name (relocate_file dir1 dir2 name)
       | NodePhonyFile (dir2, name1, name2) ->
            phony_name (relocate_file dir1 dir2 name1 ^ ":" ^ name2)
       | NodeFile (dir2, name) ->
            relocate_file dir1 dir2 name
       | NodeFlagged (_, node) ->
            name dir1 node

   (*
    * A normal node, ignoring mounts.
    *)
   let create_normal dir name =
      NodeHash.create (NodeFile (dir, name))

   (*
    * Create a phony name.
    *)
   let create_phony_global name =
      NodeHash.create (NodePhonyGlobal name)

   (*
    * Create a phony from a dir.
    *)
   let create_phony_dir dir name =
      NodeHash.create (NodePhonyDir (dir, name))

   (*
    * Create a phony with a new directory.
    *)
   let create_phony_chdir node dir =
      match NodeHash.get node with
         NodePhonyDir (_, name) ->
            create_phony_dir dir name
       | _ ->
            node

   (*
    * Create a new phony node from a previous node.
    * These are not interned.
    *)
   let rec create_phony_node node name =
      match NodeHash.get node with
         NodeFile (dir, name1) ->
            NodeHash.create (NodePhonyFile (dir, name1, name))
       | NodePhonyGlobal name1 ->
            NodeHash.create (NodePhonyFile (null_root, name1, name))
       | NodePhonyDir (dir, name1)
       | NodePhonyFile (dir, name1, _) ->
            NodeHash.create (NodePhonyFile (dir, name1, name))
       | NodeFlagged (_, node) ->
            create_phony_node node name

   (*
    * Get the core node.
    *)
   let rec core node =
      match NodeHash.get node with
         NodePhonyGlobal _
       | NodePhonyDir _
       | NodePhonyFile _
       | NodeFile _ ->
            node
       | NodeFlagged (_, node) ->
            core node

   (*
    * Escape a node.
    *)
   let create_escape kind node =
      let node = core node in
         match kind with
            NodeNormal ->
               node
          | NodePhony ->
               raise (Invalid_argument "Omake_node.Node.escape: NodePhony is not allowed")
          | NodeOptional ->
               NodeHash.create (NodeFlagged (NodeIsOptional, node))
          | NodeExists ->
               NodeHash.create (NodeFlagged (NodeIsExisting, node))
          | NodeSquashed ->
               NodeHash.create (NodeFlagged (NodeIsSquashed, node))
          | NodeScanner ->
               NodeHash.create (NodeFlagged (NodeIsScanner, node))

   (*
    * Hash code for a node.
    *)
   let hash = NodeHash.hash
   let reintern = NodeHash.reintern

   (*
    * For building targets, we sometimes want to know the
    * original node.
    *)
   let rec unsquash node =
      match NodeHash.get node with
         NodePhonyGlobal _
       | NodePhonyDir _
       | NodePhonyFile _
       | NodeFile _
       | NodeFlagged (NodeIsOptional, _)
       | NodeFlagged (NodeIsScanner, _)
       | NodeFlagged (NodeIsExisting, _) ->
            node

       | NodeFlagged (NodeIsSquashed, node) ->
            unsquash node

   (*
    * Kind of the node.
    *)
   let kind node =
      match NodeHash.get node with
         NodePhonyGlobal _
       | NodePhonyDir _
       | NodePhonyFile _ ->
            NodePhony
       | NodeFile _ ->
            NodeNormal
       | NodeFlagged (NodeIsOptional, _) ->
            NodeOptional
       | NodeFlagged (NodeIsExisting, _) ->
            NodeExists
       | NodeFlagged (NodeIsSquashed, _) ->
            NodeSquashed
       | NodeFlagged (NodeIsScanner, _) ->
            NodeScanner

   (*
    * Phony nodes.
    *)
   let rec is_phony node =
      match NodeHash.get node with
         NodePhonyGlobal _
       | NodePhonyDir _
       | NodePhonyFile _ ->
            true
       | NodeFile _
       | NodeFlagged _ ->
            false

   let rec phony_name node =
      match NodeHash.get node with
         NodePhonyGlobal name
       | NodePhonyDir (_, name)
       | NodePhonyFile (_, _, name) ->
            name
       | NodeFile _ ->
            raise (Invalid_argument "phony_name")
       | NodeFlagged (_, node) ->
            phony_name node

   let rec is_real node =
      match NodeHash.get node with
         NodePhonyGlobal _
       | NodePhonyDir _
       | NodePhonyFile _
       | NodeFlagged (NodeIsOptional, _)
       | NodeFlagged (NodeIsExisting, _) ->
            false
       | NodeFile _ ->
            true
       | NodeFlagged (NodeIsSquashed, node)
       | NodeFlagged (NodeIsScanner, node) ->
            is_real node

   (*
    * Existential flag.
    *)
   let always_exists node =
      match NodeHash.get node with
         NodeFlagged (NodeIsOptional, _)
       | NodeFlagged (NodeIsExisting, _) ->
            true
       | NodeFlagged (NodeIsSquashed, _)
       | NodeFlagged (NodeIsScanner, _)
       | NodePhonyGlobal _
       | NodePhonyDir _
       | NodePhonyFile _
       | NodeFile _ ->
            false

   (*
    * Just the tail name.
    *)
   let rec tail node =
      match NodeHash.get node with
         NodePhonyGlobal name
       | NodePhonyDir (_, name)
       | NodePhonyFile (_, _, name)
       | NodeFile (_, name) ->
            name
       | NodeFlagged (_, node) ->
            tail node

   (*
    * Get the name of the directory.
    *)
   let rec dir node =
      match NodeHash.get node with
         NodePhonyGlobal _ ->
            null_root
       | NodePhonyDir (dir, _)
       | NodePhonyFile (dir, _, _)
       | NodeFile (dir, _) ->
            dir
       | NodeFlagged (_, node) ->
            dir node

   (*
    * Equality testing.
    *)
   let compare = NodeHash.compare
   let equal = NodeHash.equal

   (*
    * Flags.
    *)
   let marshal_flag = function
      NodeIsOptional ->
         Magic NodeIsOptionalMagic
    | NodeIsExisting ->
         Magic NodeIsExistingMagic
    | NodeIsSquashed ->
         Magic NodeIsSquashedMagic
    | NodeIsScanner ->
         Magic NodeIsScannerMagic

   let unmarshal_flag flag =
      match flag with
         Magic NodeIsOptionalMagic ->
            NodeIsOptional
       | Magic NodeIsExistingMagic ->
            NodeIsExisting
       | Magic NodeIsSquashedMagic ->
            NodeIsSquashed
       | Magic NodeIsScannerMagic ->
            NodeIsScanner
       | _ ->
            raise MarshalError

   (*
    * Marshaling.
    *)
   let rec marshal node =
      match NodeHash.get node with
         NodeFile (dir, name) ->
            List [Magic NodeFileMagic; Dir.marshal dir; String name]
       | NodePhonyGlobal s ->
            List [Magic NodePhonyGlobalMagic; String s]
       | NodePhonyDir (dir, name) ->
            List [Magic NodePhonyDirMagic; Dir.marshal dir; String name]
       | NodePhonyFile (dir, name1, name2) ->
            List [Magic NodePhonyFileMagic; Dir.marshal dir; String name1; String name2]
       | NodeFlagged (flag, node) ->
            List [Magic NodeFlaggedMagic; marshal_flag flag; marshal node]

   let rec unmarshal l =
      let node =
         match l with
            List [Magic NodeFileMagic; dir; String name] ->
               NodeFile (Dir.unmarshal dir, name)
          | List [Magic NodePhonyGlobalMagic; String s] ->
               NodePhonyGlobal s
          | List [Magic NodePhonyDirMagic; dir; String name] ->
               NodePhonyDir (Dir.unmarshal dir, name)
          | List [Magic NodePhonyFileMagic; dir; String name1; String name2] ->
               NodePhonyFile (Dir.unmarshal dir, name1, name2)
          | List [Magic NodeFlaggedMagic; flag; node] ->
               NodeFlagged (unmarshal_flag flag, unmarshal node)
          | _ ->
               raise MarshalError
      in
         NodeHash.create node

   (*
    * This is a hack to allow Omake_cache to take stats of directories.
    *)
   let node_of_dir dir =
      let name = "." in
         NodeHash.create (NodeFile (dir, name))

   (*
    * Full name is relative to the cwd.
    *)
   let fullname node =
      name (Dir.cwd ()) node

   let absname node =
      name Dir.root node

   (************************************************************************
    * Mount point handling.
    *)
   let unlink_file filename =
      try Unix.unlink filename with
         Unix.Unix_error _ ->
            ()

   let copy_file mount_info src dst =
      if mount_info.mount_is_dir src then begin
         if not (mount_info.mount_is_dir dst) then begin
            Lm_filename_util.mkdirhier (fullname dst) 0o777;
            mount_info.mount_file_reset dst
         end
      end
      else
         let src_digest = mount_info.mount_digest src in
         let dst_digest = mount_info.mount_digest dst in
            if src_digest <> dst_digest then
               let dir = dir dst in
               let mode = (mount_info.mount_stat src).Unix.LargeFile.st_perm in
                  Lm_filename_util.mkdirhier (Dir.fullname dir) 0o777;
                  Lm_unix_util.copy_file (absname src) (absname dst) mode;
                  mount_info.mount_file_reset dst

   let symlink_file_unix mount_info src dst =
      if mount_info.mount_is_dir src then begin
         if not (mount_info.mount_is_dir dst) then begin
            Lm_filename_util.mkdirhier (fullname dst) 0o777;
            mount_info.mount_file_reset dst
         end
      end
      else
         let src_digest = mount_info.mount_digest src in
         let dst_digest = mount_info.mount_digest dst in
            if src_digest <> dst_digest then
               let dir = dir dst in
               let src_name = name dir src in
               let dst_name = fullname dst in
                  Lm_filename_util.mkdirhier (Dir.fullname dir) 0o777;
                  unlink_file dst_name;
                  Unix.symlink src_name dst_name;
                  mount_info.mount_file_reset dst

   let symlink_file =
      if Sys.os_type = "Win32" then
         copy_file
      else
         symlink_file_unix

   let create_node mount_info mounts dir name =
      let { mount_file_exists = file_exists;
            mount_file_reset = reset_file
          } = mount_info
      in
      let dir, name = new_file dir name in
      let node = NodeHash.create (NodeFile (dir, name)) in
      let rec search mounts =
         match mounts with
            (dir_dst, dir_src, options) :: mounts ->
               (try
                   let node' = resolve_mount_node dir_dst dir_src node in
                      if file_exists node' then
                         if List.mem MountCopy options then begin
                            copy_file mount_info node' node;
                            node
                         end
                         else if List.mem MountLink options then begin
                            symlink_file mount_info node' node;
                            node
                         end
                         else
                            node'
                      else
                         raise Not_found
                with
                   Not_found ->
                      search mounts)
          | [] ->
               node
      in
         search mounts
end

(*
 * Intern a string with no escapes.
 * This version ignores mount points.
 * Check for existing phonies first.
 * NOTE: NodeHash.intern will not create the
 * node if it does not already exist.
 *)
let create_node_or_phony phonies mount_info mount phony_ok dir name =
   match parse_phony_name name, phony_ok with
      PhonyDirString name, PhonyOK
    | PhonyDirString name, PhonyExplicit ->
         let dir, name = new_file dir name in
            NodeHash.create (NodePhonyDir (dir, name))
    | PhonyGlobalString name, PhonyOK
    | PhonyGlobalString name, PhonyExplicit ->
         NodeHash.create (NodePhonyGlobal name)
    | PhonyDirString _, PhonyProhibited
    | PhonyGlobalString _, PhonyProhibited ->
         raise (Invalid_argument "Omake_node.Node.intern: NodePhony is not allowed");
    | PhonySimpleString, PhonyOK ->
         (* Try PhonyDir first *)
         let node = NodePhonyDir (dir, name) in
            if PreNodeSet.mem phonies node then
               NodeHash.create node
            else
               (* Try PhonyGlobal next *)
               let node = NodePhonyGlobal name in
                  if PreNodeSet.mem phonies node then
                     NodeHash.create node
                  else
                     Node.create_node mount_info mount dir name
    | PhonySimpleString, PhonyExplicit
    | PhonySimpleString, PhonyProhibited
    | PhonyPathString, _ ->
         Node.create_node mount_info mount dir name

(*
 * Print the directory, for debugging.
 *)
let pp_print_dir buf dir =
   let root, path = path_of_dir dir in
      fprintf buf "%s%s" (**)
         (Lm_filename_util.string_of_root root)
         (flatten_dir path)

(*
 * Print the kind.
 *)
let pp_print_node_kind buf kind =
   let s =
      match kind with
         NodePhony     -> "phony"
       | NodeOptional  -> "optional"
       | NodeExists    -> "exists"
       | NodeSquashed  -> "squashed"
       | NodeScanner   -> "scanner"
       | NodeNormal    -> "normal"
   in
      pp_print_string buf s

(*
 * Print the node, for debugging.
 *)
let pp_print_node buf node =
   match Node.kind node with
      NodePhony ->
         fprintf buf "<phony %s>" (Node.fullname node)
    | NodeOptional ->
         fprintf buf "<optional %s>" (Node.fullname node)
    | NodeExists ->
         fprintf buf "<exists %s>" (Node.fullname node)
    | NodeSquashed ->
         fprintf buf "<squash %s>" (Node.fullname node)
    | NodeScanner ->
         fprintf buf "<scanner %s>" (Node.fullname node)
    | NodeNormal ->
         pp_print_string buf (Node.fullname node)

let pp_print_string_list buf sources =
   List.iter (fun s -> fprintf buf "@ %s" s) (List.sort String.compare sources)

let pp_compare_nodes n1 n2 =
   let cmp = Pervasives.compare (Node.kind n1) (Node.kind n2) in
      if cmp = 0 then
         let cmp = String.compare (Node.fullname n1) (Node.fullname n2) in
            if cmp = 0 then
               NodeHash.compare n1 n2
            else
               cmp
      else
         cmp

let pp_print_node_sorted buf nodes =
   let nodes = List.sort pp_compare_nodes nodes in
      List.iter (fun node -> fprintf buf "@ %a" pp_print_node node) nodes

let pp_print_node_list buf nodes =
   List.iter (fun node -> fprintf buf "@ %a" pp_print_node node) nodes

let pp_print_node_set buf set =
   pp_print_node_list buf (NodeSet.elements set)

let pp_print_node_table buf table =
   pp_print_node_list buf (NodeTable.keys table)

let pp_print_node_set_table buf table =
   NodeTable.iter (fun node set ->
         fprintf buf "@ @[<b 3>%a:%a@]" (**)
            pp_print_node node
            pp_print_node_set set) table

let pp_print_node_set_table_opt buf table_opt =
   match table_opt with
      Some table ->
         pp_print_node_set_table buf table
    | None ->
         pp_print_string buf "<none>"


(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
