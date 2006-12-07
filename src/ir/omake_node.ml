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
 * Copyright (C) 2003-2006 Mojave Group, Caltech
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
 * Modified by: Aleksey Nogin <nogin@cs.caltech.edu>
 * @end[license]
 *)
open Lm_hash
open Lm_printf
open Lm_location
open Lm_string_util
open Lm_filename_util

open Fmarshal

open Omake_state
open Omake_marshal
open Omake_node_type
open Omake_node_sig

(*
 * Sets and tables.
 *)
module DirCompare =
struct
   type t = dir

   let rec compare dir1 dir2 =
      match dir1, dir2 with
         DirRoot (hash1, root1), DirRoot (hash2, root2) ->
            if hash1 = hash2 then
               Pervasives.compare root1 root2
            else if hash1 < hash2 then
               -1
            else
               1
       | DirSub (hash1, key1, _, parent1), DirSub (hash2, key2, _, parent2) ->
            if hash1 = hash2 then
               let cmp = Pervasives.compare key1 key2 in
                  if cmp = 0 then
                     compare parent1 parent2
                  else
                     cmp
            else if hash1 < hash2 then
               -1
            else
               1
       | DirRoot _, DirSub _ ->
            -1
       | DirSub _, DirRoot _ ->
            1

   let rec compare_alpha dir1 dir2 =
      match dir1, dir2 with
         DirRoot (hash1, root1), DirRoot (hash2, root2) ->
            if hash1 = hash2 then
               Pervasives.compare root1 root2
            else if hash1 < hash2 then
               -1
            else
               1
       | DirSub (_, _, name1, parent1), DirSub (_, _, name2, parent2) ->
            let cmp = Pervasives.compare name1 name2 in
               if cmp = 0 then
                  compare parent1 parent2
               else
                  cmp
       | DirRoot _, DirSub _ ->
            -1
       | DirSub _, DirRoot _ ->
            1
end

module DirSet = Lm_set.LmMake (DirCompare);;
module DirTable = Lm_map.LmMake (DirCompare);;

(*
 * Hash-cons entries.
 *)
module DirArg =
struct
   type t = dir

   let debug = "Dir"

   let hash = function
      DirRoot (hash, _)
    | DirSub (hash, _, _, _) ->
         hash

   let compare = DirCompare.compare
end;;

module DirHash = MakeHashCons (DirArg);;
module DirHashSet = Lm_set.LmMake (DirHash);;
module DirHashTable = Lm_map.LmMake (DirHash);;

(*
 * Lists of directories.
 *)
module DirListArg =
struct
   type t = DirHash.t list

   let debug = "DirList"

   let hash l =
      List.fold_left (fun hash dir ->
            hash_combine hash (DirHash.hash dir)) 0x3b71dd11 l

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
end;;

module DirListHash = MakeHashCons (DirListArg);;
module DirListSet = Lm_set.LmMake (DirListHash);;
module DirListTable = Lm_map.LmMake (DirListHash);;

(************************************************************************
 * Nodes.
 *)
module NodeCompare =
struct
   type t = node

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

   let rec compare node1 node2 =
      if node1 == node2 then
         0
      else
         match node1, node2 with
            NodeFile (hash1, dir1, key1, _), NodeFile (hash2, dir2, key2, _)
          | NodePhonyDir (hash1, dir1, key1, _), NodePhonyDir (hash2, dir2, key2, _) ->
               if hash1 = hash2 then
                  let cmp = DirCompare.compare dir1 dir2 in
                     if cmp = 0 then
                        Pervasives.compare key1 key2
                     else
                        cmp
               else if hash1 < hash2 then
                  -1
               else
                  1
          | NodePhonyGlobal (hash1, name1), NodePhonyGlobal (hash2, name2) ->
               if hash1 = hash2 then
                  Pervasives.compare name1 name2
               else if hash1 < hash2 then
                  -1
               else
                  1
          | NodePhonyFile (hash1, dir1, key1, _, name1), NodePhonyFile (hash2, dir2, key2, _, name2) ->
               if hash1 = hash2 then
                  let cmp = DirCompare.compare dir1 dir2 in
                      if cmp = 0 then
                          let cmp = Pervasives.compare key1 key2 in
                              if cmp = 0 then
                                  Pervasives.compare name1 name2
                              else
                                 cmp
                      else
                         cmp
               else if hash1 < hash2 then
                  -1
               else
                  1
          | NodeFlagged (flag1, node1), NodeFlagged (flag2, node2) ->
               if flag1 = flag2 then
                  compare node1 node2
               else
                  compare_flags flag1 flag2
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

   let rec compare_alpha node1 node2 =
      match node1, node2 with
         NodeFile (_, dir1, key1, _), NodeFile (_, dir2, key2, _)
       | NodePhonyDir (_, dir1, key1, _), NodePhonyDir (_, dir2, key2, _) ->
            let cmp = DirCompare.compare_alpha dir1 dir2 in
               if cmp = 0 then
                  Pervasives.compare key1 key2
               else
                  cmp
       | NodePhonyGlobal (_, name1), NodePhonyGlobal (_, name2) ->
            Pervasives.compare name1 name2
       | NodePhonyFile (_, dir1, key1, _, name1), NodePhonyFile (_, dir2, key2, _, name2) ->
            let cmp = DirCompare.compare_alpha dir1 dir2 in
               if cmp = 0 then
                  let cmp = Pervasives.compare key1 key2 in
                     if cmp = 0 then
                        Pervasives.compare name1 name2
                     else
                        cmp
               else
                  cmp
       | NodeFlagged (flag1, node1), NodeFlagged (flag2, node2) ->
            if flag1 = flag2 then
               compare_alpha node1 node2
            else
               compare_flags flag1 flag2
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
end

module NodeSet = Lm_set.LmMake (NodeCompare)
module NodeTable = Lm_map.LmMake (NodeCompare)
module NodeMTable = Lm_map.LmMakeList (NodeCompare)

(*
 * Hashing.
 *)
module type HashSig =
sig
   val hash_root : root -> int
   val hash_dir : dir -> string -> int
   val hash_file : dir -> string -> int
   val hash_global_phony : string -> int
   val hash_file_phony : dir -> string -> string -> int
end

module Hash : HashSig =
struct
   let hash_root root =
      Hashtbl.hash root

   let hash_of_dir dir =
      match dir with
         DirRoot (hash, _)
       | DirSub (hash, _, _, _) ->
            hash

   let hash_dir parent key =
      let hash = hash_of_dir parent in
         Hashtbl.hash (hash, key)

   let hash_file dir key =
      let hash = hash_of_dir dir in
         Hashtbl.hash (hash, key)

   let hash_global_phony name =
      Hashtbl.hash name

   let hash_file_phony dir key name =
      let hash = hash_of_dir dir in
         Hashtbl.hash (hash, key, name)
end

(*
 * Get a pathname from a directory.
 * The name must be reversed.
 *)
let path_of_dir dir =
   let rec path_of_dir keypath path dir =
      match dir with
         DirRoot (_, root) ->
            root, keypath, path
       | DirSub (_, key, name, parent) ->
            path_of_dir (key :: keypath) (name :: path) parent
   in
      path_of_dir [] [] dir

(*
 * Build a list of the directories, in reverse order.
 *)
let dir_list_of_dir dir =
   let rec dir_list_of_dir path dir =
      match dir with
         DirRoot _ ->
            dir :: path
       | DirSub (_, _, _, parent) ->
            dir_list_of_dir (dir :: path) parent
   in
      dir_list_of_dir [] dir

(*
 * Produce a path (a string list) from the dir list.
 *)
let rec path_of_dir_list dirs =
   match dirs with
      DirSub (_, _, name, _) :: dirs ->
         name :: path_of_dir_list dirs
    | DirRoot _ :: dirs ->
         raise (Invalid_argument "path_of_dir_list")
    | [] ->
         []

(*
 * Make a directory node from the pathname.
 *)
let make_dir root path =
   let rec make parent path =
      match path with
         [] ->
            parent
       | name :: path ->
            let key = Lm_filename_util.normalize_string name in
            let hash = Hash.hash_dir parent key in
            let dir = DirSub (hash, key, name, parent) in
               make dir path
   in
   let hash = Hash.hash_root root in
   let root = DirRoot (hash, root) in
      make root path

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
let new_path dir path =
   let root, _, dir = path_of_dir dir in
   let path = Lm_filename_util.filename_path path in
   let root, stack, path =
      match path with
         AbsolutePath (root, path) ->
            (* This is an absolute path, so ignore the directory *)
            root, [], path
       | RelativePath path ->
            (* This is relative to the directory *)
            root, List.rev dir, path
   in
   let rec simplify stack path =
      match path with
         "" :: path
       | "." :: path ->
            simplify stack path
       | ".." :: path ->
            let stack =
               match stack with
                  _ :: stack ->
                     stack
                | [] ->
                     stack
            in
               simplify stack path
       | name :: path ->
            simplify (name :: stack) path
       | [] ->
            root, stack
   in
      simplify stack path

let new_dir dir path =
   let root, stack = new_path dir path in
      make_dir root (List.rev stack)

(*
 * Make a file from the directory and the path.
 *)
let new_file dir path =
   let root, stack = new_path dir path in
   let dir, name =
      match stack with
         [] ->
            make_dir root [], "."
       | name :: stack ->
            make_dir root (List.rev stack), name
   in
   let key = Lm_filename_util.normalize_string name in
   let hash = Hash.hash_file dir key in
      hash, dir, key, name

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
      match dir with
         DirRoot (_, root) ->
            Buffer.add_string buf (Lm_filename_util.string_of_root root)
       | DirSub (_, key, _, (DirRoot _ as parent)) ->
            name parent;
            Buffer.add_string buf key
       | DirSub (_, key, _, parent) ->
            name parent;
            Buffer.add_char buf Lm_filename_util.separator_char;
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
let rec relocate_generic add_string contents buf dirs1 dirs2 =
   match dirs1, dirs2 with
      DirSub (_, key1, _, _) :: dirs1', DirSub (_, key2, _, _) :: dirs2'  ->
         if key1 = key2 then
            relocate_generic add_string contents buf dirs1' dirs2'
         else
            updirs_generic add_string contents buf dirs1 dirs2
    | [], _ ->
         Some (flatten_generic add_string contents buf (path_of_dir_list dirs2))
    | _, [] ->
         updirs_generic add_string contents buf dirs1 dirs2
    | DirRoot _ :: _, _
    | _, DirRoot _ :: _ ->
         raise (Invalid_argument "relocate_generic")

(*
 * If the files differ in the root directory, just use the absolute path.
 *)
let relocate_generic add_string contents buf dir1 dir2 =
   let dirs1 = dir_list_of_dir dir1 in
   let dirs2 = dir_list_of_dir dir2 in
      match dirs1, dirs2 with
         DirRoot (_, root1) :: dirs1, DirRoot (_, root2) :: dirs2 ->
            begin
               let s =
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
                           flatten_generic add_string contents buf path2
            end
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
   relocate_generic dir_add_string (file_contents name) dir_buffer dir1 dir2

(*
 * Apply a mount point.
 *)
let rec resolve_mount_dir dir_dst dir_src dir =
   if DirCompare.compare_alpha dir dir_dst = 0 then
      dir_src
   else
      match dir with
         DirRoot _ ->
            raise Not_found
       | DirSub (_, key, name, parent) ->
            let parent = resolve_mount_dir dir_dst dir_src parent in
            let hash = Hash.hash_dir parent key in
               DirSub (hash, key, name, parent)

let rec resolve_mount_node dir_dst dir_src node =
   match node with
      NodeFile (_, dir, key, name) ->
         let dir = resolve_mount_dir dir_dst dir_src dir in
         let hash = Hash.hash_file  dir key in
            NodeFile (hash, dir, key, name)
    | NodePhonyDir (_, dir, key, name) ->
         let dir = resolve_mount_dir dir_dst dir_src dir in
         let hash = Hash.hash_file dir key in
            NodePhonyDir (hash, dir, key, name)
    | NodePhonyFile (_, dir, key1, name1, name) ->
         let dir = resolve_mount_dir dir_dst dir_src dir in
         let hash = Hash.hash_file_phony dir key1 name in
            NodePhonyFile (hash, dir, key1, name1, name)
    | NodePhonyGlobal _ ->
         raise Not_found
    | NodeFlagged (flag, node) ->
         NodeFlagged (flag, resolve_mount_node dir_dst dir_src node)

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

let string_prefix_equal s1 s2 =
   let len1 = String.length s1 in
   let len2 = String.length s2 in
      if len1 < len2 then
         false
      else
         let rec search i =
            if i = len2 then
               true
            else
               match s1.[i], s2.[i] with
                  '/', '/'
                | '\\', '/' ->
                     search (succ i)
                | c1, c2 ->
                     c1 = c2 && search (succ i)
         in
            search 0

let is_simple_string s i =
   let len = String.length s in
   let rec search i =
      if i = len then
         true
      else
         match s.[i] with
            '/'
          | '\\' ->
               false
          | _ ->
               search (succ i)
   in
      search i

let parse_phony_name s =
   let len = String.length s in
      if string_prefix_equal s "/.PHONY/" && is_simple_string s 8 then
         PhonyGlobalString (String.sub s 8 (len - 8))
      else if string_prefix_equal s ".PHONY/" then
         PhonyDirString (String.sub s 7 (len - 7))
      else if is_simple_string s 0 then
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
   let chdir dir1 dir2 =
      new_dir dir1 dir2

   (*
    * Name, relative to the cwd.
    *)
   let name dir1 dir2 =
      let s = relocate_dir dir1 dir2 in
         if s = "" then
            "."
         else
            s

   (*
    * Name relative to the root.
    *)
   let fullname dir =
      name !cwd_ref dir

   (*
    * Absolute name.
    *)
   let root = make_dir Lm_filename_util.null_root []

   let absname dir =
      name root dir

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
   let equal dir1 dir2 =
      DirCompare.compare dir1 dir2 = 0

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
      match dir with
         DirRoot (hash, root) ->
            List [Magic DirRootMagic; Int hash; marshal_root root]
       | DirSub (hash, key, name, parent) ->
            List [Magic DirSubMagic; Int hash; String key; String name; marshal parent]

   let rec unmarshal l =
      match l with
         List [Magic DirRootMagic; Int hash; root] ->
            DirRoot (hash, unmarshal_root root)
       | List [Magic DirSubMagic; Int hash; String key; String name; parent] ->
            DirSub (hash, key, name, unmarshal parent)
       | _ ->
            raise MarshalError
end;;

(*
 * Virtual mounts.
 * We need a function that checks if a file exists.
 *)
module Mount =
struct
   type mount_info =
      { mount_file_exists : node -> bool;
        mount_file_reset  : node -> unit;
        mount_is_dir      : node -> bool;
        mount_stat        : node -> Unix.LargeFile.stats;
        mount_digest      : node -> string option
      }

   type t =
      { mount_info        : mount_info;
        mount_points      : (Dir.t * Dir.t * mount_option list) list
      }

   type dir = Omake_node_type.dir
   type node = Omake_node_type.node

   (*
    * Create a new mount state.
    *)
   let create mount_info =
      { mount_info = mount_info;
        mount_points = []
      }

   (*
    * Add a mount point.
    *)
   let mount info options dir_src dir_dst =
      { info with mount_points = (dir_dst, dir_src, options) :: info.mount_points }
end;;

let no_mount_info =
   { Mount.mount_file_exists = (fun _ -> false);
     Mount.mount_file_reset  = (fun _ -> ());
     Mount.mount_is_dir      = (fun _ -> false);
     Mount.mount_stat        = (fun _ -> raise (Invalid_argument "no_mount_info"));
     Mount.mount_digest      = (fun _ -> None)
   }

let no_mount_points = Mount.create no_mount_info

(*
 * Nodes.
 *)
module Node =
struct
   type t = node
   type dir = Dir.t
   type mount = Mount.t
   (* %%MAGICBEGIN%% *)
   type db = node list
   (* %%MAGICEND%% *)

   open Mount

   (*
    * Create a database that remembers the names
    * so that we get some sharing.  The sharing
    * does not have to be preserved for correctness.
    *)
   type base =
      { mutable base_nodes : node NodeTable.t }

   let base =
      { base_nodes = NodeTable.empty }

   (*
    * Marshal the node database.
    *)
   let marshal_base =
      let save nodes node _ =
         match node with
            (* We do not want to keep the phonys around in case they are no longer phony *)
            NodePhonyGlobal _
          | NodePhonyDir _
          | NodePhonyFile _ ->
               nodes
          | NodeFlagged (_, node) ->
               raise (Invalid_argument "Omake_node internal error: NodeFlagged in base")
          | NodeFile _ ->
               node :: nodes
      in
         fun () -> NodeTable.fold save [] base.base_nodes

   (*
    * Re-intern, after marshaling.
    *)
   let unmarshal_base =
      let rec intern_node nodes node =
         match node with
            NodeFile _ ->
               NodeTable.filter_add nodes node (function
                  Some old -> old
                | None -> node)
            (* We do not want to keep the phonys around in case they are no longer phony *)
          | NodePhonyGlobal _
          | NodePhonyDir _
          | NodePhonyFile _ ->
               nodes
          | NodeFlagged (_, node) ->
               raise (Invalid_argument "Omake_node internal error: NodeFlagged in base")
      in
         (fun loaded_nodes ->
            let nodes = List.fold_left intern_node base.base_nodes loaded_nodes in
               base.base_nodes <- nodes)

   (*
    * Get the name.
    *)
   let phony_name name =
      "<" ^ name ^ ">"

   (*
    * Name of the node.
    *)
   let rec name dir1 node =
      match node with
         NodePhonyGlobal (_, name) ->
            phony_name name
       | NodePhonyDir (_, dir2, _, name) ->
            phony_name (relocate_file dir1 dir2 name)
       | NodePhonyFile (_, dir2, _, name1, name2) ->
            phony_name (relocate_file dir1 dir2 name1 ^ ":" ^ name2)
       | NodeFile (_, dir2, _, name) ->
            relocate_file dir1 dir2 name
       | NodeFlagged (_, node) ->
            name dir1 node

   (*
    * Create a phony name.
    *)
   let phony_global name =
      let hash = Hash.hash_global_phony name in
      let node = NodePhonyGlobal (hash, name) in
      let nodes = base.base_nodes in
         try NodeTable.find nodes node with
            Not_found ->
               base.base_nodes <- NodeTable.add nodes node node;
               node

   (*
    * Create a phony from a dir.
    *)
   let phony_dir dir name =
      let key = Lm_filename_util.normalize_string name in
      let hash = Hash.hash_file dir key in
      let node = NodePhonyDir (hash, dir, key, name) in
      let nodes = base.base_nodes in
         try NodeTable.find nodes node with
            Not_found ->
               base.base_nodes <- NodeTable.add nodes node node;
               node

   (*
    * Create a phony with a new directory.
    *)
   let phony_chdir node dir =
      match node with
         NodePhonyDir (_, _, _, name) ->
            phony_dir dir name
       | _ ->
            node

   (*
    * Create a new phony node from a previous node.
    * These are not interned.
    *)
   let rec phony_node node name =
      match node with
         NodeFile (_, dir, key1, name1) ->
            let hash = Hash.hash_file_phony dir key1 name in
               NodePhonyFile (hash, dir, key1, name1, name)
       | NodePhonyGlobal (_, name1) ->
            let key1 = Lm_filename_util.normalize_string name1 in
            let hash = Hash.hash_file_phony null_root key1 name in
               NodePhonyFile (hash, null_root, key1, name1, name)
       | NodePhonyDir (_, dir, key1, name1)
       | NodePhonyFile (_, dir, key1, name1, _) ->
            let hash = Hash.hash_file_phony dir key1 name in
               NodePhonyFile (hash, dir, key1, name1, name)
       | NodeFlagged (_, node) ->
            phony_node node name

   (*
    * Intern a string with no escapes.
    * This version ignores mount points.
    *)
   let intern_simple phony_ok dir name =
      let { base_nodes = nodes } = base in

      (* Parse the .PHONY syntax *)
         match parse_phony_name name, phony_ok with
            PhonyDirString name, PhonyOK
          | PhonyDirString name, PhonyExplicit ->
               (* Name is .PHONY/... *)
               let hash, dir, key, name = new_file dir name in
               let node = NodePhonyDir (hash, dir, key, name) in
                  (try NodeTable.find nodes node with
                      Not_found ->
                         base.base_nodes <- NodeTable.add nodes node node;
                         node)

          | PhonyGlobalString name, PhonyOK
          | PhonyGlobalString name, PhonyExplicit ->
               (* Name is /.PHONY/name *)
               let hash = Hash.hash_global_phony name in
               let node = NodePhonyGlobal (hash, name) in
                  (try NodeTable.find nodes node with
                      Not_found ->
                         base.base_nodes <- NodeTable.add nodes node node;
                         node)

          | PhonyDirString _, PhonyProhibited
          | PhonyGlobalString _, PhonyProhibited ->
               raise (Invalid_argument "Omake_node.Node.intern: NodePhony is not allowed");

          | PhonySimpleString, PhonyOK ->
               (* Name is simple, without path separators *)
               let key  = Lm_filename_util.normalize_string name in
               let hash = Hash.hash_file dir key in
               let node = NodePhonyDir (hash, dir, key, name) in
                  (try NodeTable.find nodes node with
                      Not_found ->
                         (* Try GlobalPhony next *)
                         let glob_hash = Hash.hash_global_phony name in
                         let node = NodePhonyGlobal (glob_hash, name) in
                            try NodeTable.find nodes node with
                               Not_found ->
                                  (* Otherwise, it is a normal file *)
                                  let node = NodeFile (hash, dir, key, name) in
                                     try NodeTable.find nodes node with
                                        Not_found ->
                                           base.base_nodes <- NodeTable.add nodes node node;
                                           node)

          | PhonySimpleString, PhonyExplicit
          | PhonySimpleString, PhonyProhibited
          | PhonyPathString, _ ->
               (* Otherwise, it is a normal file *)
               let hash, dir, key, name = new_file dir name in
               let node = NodeFile (hash, dir, key, name) in
                  try NodeTable.find nodes node with
                     Not_found ->
                        base.base_nodes <- NodeTable.add nodes node node;
                        node

   (*
    * Escape a node.
    *)
   let escape kind node =
      let node =
         match node with
            NodeFlagged (_, node) ->
               node
          | _ ->
               node
      in
         match kind with
            NodeNormal ->
               node
          | NodePhony ->
               raise (Invalid_argument "Omake_node.Node.escape: NodePhony is not allowed")
          | NodeOptional ->
               NodeFlagged (NodeIsOptional, node)
          | NodeExists ->
               NodeFlagged (NodeIsExisting, node)
          | NodeSquashed ->
               NodeFlagged (NodeIsSquashed, node)
          | NodeScanner ->
               NodeFlagged (NodeIsScanner, node)

   (*
    * Hash code for a node.
    *)
   let rec hash = function
      NodePhonyGlobal (hash, _)
    | NodePhonyDir (hash, _, _, _)
    | NodePhonyFile (hash, _, _, _, _)
    | NodeFile (hash, _, _, _) ->
         hash
    | NodeFlagged (_, node) ->
         hash node

   (*
    * Get the core node.
    *)
   let rec core node =
      match node with
         NodePhonyGlobal _
       | NodePhonyDir _
       | NodePhonyFile _
       | NodeFile _ ->
            node
       | NodeFlagged (_, node) ->
            core node

   (*
    * For building targets, we sometimes want to know the
    * original node.
    *)
   let rec unsquash node =
      match node with
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
   let kind = function
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
   let rec is_phony = function
      NodePhonyGlobal _
    | NodePhonyDir _
    | NodePhonyFile _ ->
         true
    | NodeFile _
    | NodeFlagged _ ->
         false

   let rec is_real = function
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
   let always_exists = function
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
      match node with
         NodePhonyGlobal (_, name)
       | NodePhonyDir (_, _, _, name)
       | NodePhonyFile (_, _, _, _, name)
       | NodeFile (_, _, _, name) ->
            name
       | NodeFlagged (_, node) ->
            tail node

   (*
    * Get the name of the directory.
    *)
   let rec dir node =
      match node with
         NodePhonyGlobal _ ->
            null_root
       | NodePhonyDir (_, dir, _, _)
       | NodePhonyFile (_, dir, _, _, _)
       | NodeFile (_, dir, _, _) ->
            dir
       | NodeFlagged (_, node) ->
            dir node

   (*
    * Equality testing.
    *)
   let compare = NodeCompare.compare

   let compare_alpha = NodeCompare.compare_alpha

   let equal node1 node2 =
      compare node1 node2 = 0

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
      match node with
         NodeFile (i, dir, name1, name2) ->
            List [Magic NodeFileMagic; Int i; Dir.marshal dir; String name1; String name2]
       | NodePhonyGlobal (i, s) ->
            List [Magic NodePhonyGlobalMagic; Int i; String s]
       | NodePhonyDir (i, dir, name1, name2) ->
            List [Magic NodePhonyDirMagic; Int i; Dir.marshal dir; String name1; String name2]
       | NodePhonyFile (i, dir, name1, name2, name3) ->
            List [Magic NodePhonyFileMagic; Int i; Dir.marshal dir; String name1; String name2; String name3]
       | NodeFlagged (flag, node) ->
            List [Magic NodeFlaggedMagic; marshal_flag flag; marshal node]

   let rec unmarshal l =
      match l with
         List [Magic NodeFileMagic; Int i; dir; String name1; String name2] ->
            NodeFile (i, Dir.unmarshal dir, name1, name2)
       | List [Magic NodePhonyGlobalMagic; Int i; String s] ->
            NodePhonyGlobal (i, s)
       | List [Magic NodePhonyDirMagic; Int i; dir; String name1; String name2] ->
            NodePhonyDir (i, Dir.unmarshal dir, name1, name2)
       | List [Magic NodePhonyFileMagic; Int i; dir; String name1; String name2; String name3] ->
            NodePhonyFile (i, Dir.unmarshal dir, name1, name2, name3)
       | List [Magic NodeFlaggedMagic; flag; node] ->
            NodeFlagged (unmarshal_flag flag, unmarshal node)
       | _ ->
            raise MarshalError

   (*
    * This is a hack to allow Omake_cache to take stats of directories.
    *)
   let node_of_dir dir =
      let name = "." in
      let hash = Hash.hash_file dir name in
         NodeFile (hash, dir, name, name)

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

   let intern mount phony_ok dir name =
      let { mount_info = mount_info;
            mount_points = mounts
          } = mount
      in
      let { mount_file_exists = file_exists;
            mount_file_reset = reset_file
          } = mount_info
      in
      let node = intern_simple phony_ok dir name in
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
         match node with
            NodeFile _ ->
               search mounts
          | _ ->
               node
end

(*
 * Print the directory, for debugging.
 *)
let pp_print_dir buf dir =
   let root, _, path = path_of_dir dir in
      fprintf buf "%s%s" (**)
         (Lm_filename_util.string_of_root root)
         (flatten_dir path)

(*
 * Print the kind.
 *)
let pp_print_node_kind buf kind =
   let s =
      match kind with
         NodePhony    -> "phony"
       | NodeOptional -> "optional"
       | NodeExists   -> "exists"
       | NodeSquashed -> "squashed"
       | NodeScanner  -> "scanner"
       | NodeNormal   -> "normal"
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
   match Pervasives.compare (Node.kind n1) (Node.kind n2) with
      0 ->
         (match String.compare (Node.fullname n1) (Node.fullname n2) with
             0 -> NodeCompare.compare_alpha n1 n2
           | i -> i)
    | i ->
         i

let pp_print_node_list buf nodes =
   List.iter (fun node -> fprintf buf "@ %a" pp_print_node node) (List.sort pp_compare_nodes nodes)

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

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
