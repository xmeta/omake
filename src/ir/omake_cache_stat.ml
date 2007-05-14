(*
 * Utilities for the cache.  The main purpose here is force
 * the stat-operation on case-insenstive filesystems to look
 * as it were case-sensitive.
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
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified By: Aleksey Nogin @email{anogin@hrl.com}
 * @end[license]
 *)
open Lm_debug
open Lm_printf
open Lm_string_set

open Omake_node
open Omake_command
open Omake_node_sig
open Omake_cache_type
open Omake_value_type

(*
 * Warn about case mismatches; they are not errors.
 *)
let warn_case_mismatch = ref false
let check_case = ref true

(*
 * For directories, keep track of case-sensitivity.
 * The option is None iff the dir is case-sensitive
 *)
type dir_info = string StringTable.t option

(*
 * Our sub-cache.
 *)
type t =
   { mutable cache_file_stat_count : int;
     mutable cache_case_table      : bool DirTable.t;
     mutable cache_dirs            : dir_info DirTable.t;
     mutable cache_dir_real        : Dir.t DirTable.t;
     mutable cache_node_real       : Node.t NodeTable.t
   }

let create () =
   { cache_file_stat_count   = 0;
     cache_case_table        = DirTable.empty;
     cache_dirs              = DirTable.empty;
     cache_dir_real          = DirTable.empty;
     cache_node_real         = NodeTable.empty
   }

let stat_count cache =
   cache.cache_file_stat_count

(************************************************************************
 * Case-sensitivity check.
 *)

(* We'll use randomly generated names *)
let fs_random = Random.State.make_self_init ()

(*
 * Test whether stats are equal enough that we think that it's the same file.
 *
 * Note: don't use ctime (because ctime is the time of last
 * inode modification).
 *)
let case_stats_equal stat1 stat2 =
   (stat1.Unix.LargeFile.st_dev      = stat2.Unix.LargeFile.st_dev)
   && (stat1.Unix.LargeFile.st_ino   = stat2.Unix.LargeFile.st_ino)
   && (stat1.Unix.LargeFile.st_kind  = stat2.Unix.LargeFile.st_kind)
   && (stat1.Unix.LargeFile.st_rdev  = stat2.Unix.LargeFile.st_rdev)
   && (stat1.Unix.LargeFile.st_nlink = stat2.Unix.LargeFile.st_nlink)
   && (stat1.Unix.LargeFile.st_size  = stat2.Unix.LargeFile.st_size)
   && (stat1.Unix.LargeFile.st_mtime = stat2.Unix.LargeFile.st_mtime)
   && (stat1.Unix.LargeFile.st_atime = stat2.Unix.LargeFile.st_atime)

(*
 * Toggle the case of the name.
 * Raises Not_found if the name contains no alphabetic letters.
 *)
let rec toggle_name_case name len i =
   if i = len then
      raise Not_found
   else
      match name.[i] with
         'A'..'Z' -> String.lowercase name
       | 'a'..'z' -> String.uppercase name
       | _ -> toggle_name_case name len (succ i)

(*
 * Stat, does not fail.
 *)
let do_stat cache absname =
   try
      let stats = Unix.LargeFile.lstat absname in
         cache.cache_file_stat_count <- cache.cache_file_stat_count + 1;
         Some stats
   with
      Unix.Unix_error _ ->
         None

(*
 * Unlink, does not fail.
 *)
let do_unlink absname =
   try Unix.unlink absname with
      Unix.Unix_error _ ->
         ()

(*
 * Create a file, raising Unix_error if the file can't be created.
 *)
let do_create absname =
   Unix.close (Unix.openfile absname [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] 0o600)

(*
 * Given two filenames that differ only in case,
 * stat them both.  If the stats are different,
 * the directory is on a case-sensitive fs.
 *
 * XXX: try to detect race conditions by performing
 * a stat on the first file before and after.
 * Raise Not_found if a race condition is detected.
 *)
let stats_not_equal cache name1 name2 =
   let stat1 = do_stat cache name1 in
   let stat2 = do_stat cache name2 in
   let stat3 = do_stat cache name1 in
      match stat1, stat3 with
         Some s1, Some s3 when case_stats_equal s1 s3 ->
            (match stat2 with
                Some s2 when case_stats_equal s2 s1 -> false
              | _ -> true)
       | _ ->
            raise Not_found

(*
 * If we have an alphabetic name, just toggle the case.
 *)
let stat_with_toggle_case cache absdir name =
   let alternate_name = toggle_name_case name (String.length name) 0 in
      stats_not_equal cache (Filename.concat absdir name) (Filename.concat absdir alternate_name)

(*
 * Look through the entire directory for a name with alphabetic characters.
 * A check for case-sensitivity base on that.
 *
 * Raises Not_found if there are no such filenames.
 *)
let rec dir_test_all_entries_exn cache absdir entries =
   let rec test names =
      match names with
         [name] ->
            stat_with_toggle_case cache absdir name
       | [] ->
            raise Not_found
       | name :: names ->
            try stat_with_toggle_case cache absdir name with
               Not_found ->
                  test names
   in
      test entries

(*
 * Check for sensativity by creating a dummy file.
 *)
let dir_test_new_entry_exn cache absdir =
   let name = sprintf "OM%06x.tmp" (Random.State.bits fs_random land 0xFFFFFF) in
   let absname = Filename.concat absdir name in
      do_create absname;
      try
         let flag = stat_with_toggle_case cache absdir name in
            do_unlink absname;
            flag
      with
         Not_found as exn ->
            do_unlink absname;
            raise exn

(*
 * To check for case sensitivity, try these tests in order,
 * stopping when one is successful.
 *    1. Try the name being created itself.
 *    2. Try looking at the directory entries.
 *    3. Create a dummy file, and test.
 *    4. Test the parent.
 *)
let rec dir_test_sensitivity cache dir absdir names =
   try dir_test_all_entries_exn cache absdir names with
      Unix.Unix_error _ | Not_found | End_of_file ->
         try dir_test_new_entry_exn cache absdir with
            Unix.Unix_error _ | Not_found | End_of_file ->
               match Dir.dest dir with
                  Dir.DirInfoRoot _ ->
                     (* Nothing else we can do, assume sensitive *)
                     true
                | Dir.DirInfoSub (name, parent) ->
                     dir_is_sensitive cache parent names

(*
 * This is the caching version of the case-sensitivity test.
 *
 * On Unix-like OS (especially on Mac OS X), the test needs
 * to check the fs of the node's parent directory for case
 * sensitivity.  So the test takes a directory, and a
 * listing of the directory.
 *
 * See also http://bugzilla.metaprl.org/show_bug.cgi?id=657
 *)
and dir_is_sensitive cache dir names =
   try DirTable.find cache.cache_case_table dir with
      Not_found ->
         let absdir = Dir.absname dir in
         let sensitive =
            if Lm_fs_case_sensitive.available then
               try
                  Lm_fs_case_sensitive.case_sensitive absdir
               with
                  Failure _ ->
                     dir_test_sensitivity cache dir absdir names
            else
               dir_test_sensitivity cache dir absdir names
         in
            cache.cache_case_table <- DirTable.add cache.cache_case_table dir sensitive;
            sensitive

(*
 * XXX: BUG: Win32 and Cygwin are always case-insensitive.
 * This is not actually true.  It is possible, but very unlikely,
 * to get a case-senstivie filesystem on Win32.
 *)
let dir_is_sensitive =
   match Sys.os_type with
      "Win32"
    | "Cygwin" ->
         (fun _ _ _ -> false)
    | _ ->
         dir_is_sensitive

(************************************************************************
 * Cached stat.
 *
 * Our implementation of stat is forced to be case-sensitive.
 * For this, we use directory listings to determine the real
 * path name.
 *
 * In the following code, we can assume that the directory
 * exists, because we have performed a stat first, and the
 * stat said the file existed.  Of course, handle race
 * conditions too.
 *)

(*
 * Get the actual listing.
 *)
let dir_listing cache dir =
   let names =
      try Lm_filename_util.lsdir (Dir.fullname dir) with
         Unix.Unix_error _ ->
            raise Not_found
   in
   let names =
      if dir_is_sensitive cache dir names then
         None
      else
         let names =
            List.fold_left (fun names name ->
                  StringTable.add names (String.lowercase name) name) StringTable.empty names
         in
            Some names
   in
      cache.cache_dirs <- DirTable.add cache.cache_dirs dir names;
      names

(*
 * When a directory is listed, recursively list all the
 * ancestors.
 *)
let rec ls_dir_path cache ?(force = false) dir =
   try DirTable.find cache.cache_dirs dir with
      Not_found ->
         (*
          * Either we have never looked before,
          * or the case of the directory is wrong.
          * We must fail if the case is wrong.
          *)
         match Dir.dest dir with
            Dir.DirInfoRoot root ->
               dir_listing cache dir
          | Dir.DirInfoSub (name, parent) ->
               (* Note: get_real_tail will list the parent by side-effect *)
               let name' = get_real_tail cache ~force parent name in
                  if name' = name then
                     dir_listing cache dir
                  else
                     raise Not_found

(*
 * Fetch the real tail name of a file.
 * If the entry does not exist, the name is unchanged.
 *)
and get_real_tail cache ?(force = false) dir tail =
   match ls_dir_path cache dir with
      None ->
         tail
    | Some table ->
         let lower_tail = String.lowercase tail in
            if force then
               try StringTable.find table lower_tail with
                  Not_found ->
                     (* The entry is supposed to exist, so rescan the directory *)
                     match dir_listing cache dir with
                        None ->
                           tail
                      | Some table ->
                           StringTable.find table lower_tail
            else
               StringTable.find table lower_tail

(*
 * Get the earliest prefix that is cached.
 *)
let rec get_cached_prefix cache suffix dir =
   if DirTable.mem cache.cache_dirs dir then
      dir, suffix
   else
      match Dir.dest dir with
         Dir.DirInfoRoot _ ->
            ignore (dir_listing cache dir);
            dir, suffix
       | Dir.DirInfoSub (name, dir) ->
            get_cached_prefix cache (name :: suffix) dir

(*
 * Translate to a real name.
 * As soon as a directory listing fails, assume
 * that the rest of the name is preserved.
 *)
let rec concat_suffix dir suffix =
   match suffix with
      [] ->
         dir
    | name :: suffix ->
         concat_suffix (Dir.chdir dir name) suffix

let rec find_max_prefix cache dir suffix =
   match suffix with
      [] ->
         dir
    | name :: suffix ->
         let name =
            try Some (get_real_tail cache dir name) with
               Not_found ->
                  None
         in
            match name with
               Some name ->
                  find_max_prefix cache (Dir.chdir dir name) suffix
             | None ->
                  concat_suffix dir suffix

let real_dir cache dir =
   try DirTable.find cache.cache_dir_real dir with
      Not_found ->
         let dir, suffix = get_cached_prefix cache [] dir in
         let real_dir = find_max_prefix cache dir suffix in
            cache.cache_dir_real <- DirTable.add cache.cache_dir_real dir real_dir;
            real_dir

let real_node cache node =
   try NodeTable.find cache.cache_node_real node with
      Not_found ->
         let dir = Node.dir node in
         let tail = Node.tail node in
         let dir' = real_dir cache dir in
         let real_node =
            try Node.create_normal dir' (get_real_tail cache dir' tail) with
               Not_found ->
                  if dir' == dir then
                     node
                  else
                     Node.create_normal dir' tail
         in
            cache.cache_node_real <- NodeTable.add cache.cache_node_real node real_node;
            real_node

(*
 * Check the path (by performing the listing).
 *)
let check_real_dir cache dir =
   try ignore (ls_dir_path cache ~force:true dir) with
      Not_found ->
         if !warn_case_mismatch then
            eprintf "Case mismatch: %s; realname %s@." (Dir.absname dir) (Dir.absname (real_dir cache dir))
         else
            raise (Sys_error "case mismatch")

let check_real_tail cache node dir tail =
   let mismatch =
      try get_real_tail cache ~force:true dir tail <> tail with
         Not_found ->
            true
   in
      if mismatch then
         if !warn_case_mismatch then
            eprintf "Case mismatch: %s; realname %s@." (Node.absname node) (Node.absname (real_node cache node))
         else
            raise (Sys_error "case mismatch")

let check_real_path cache node =
   let dir = Node.dir node in
   let tail = Node.tail node in
      (* If the tail is "." this is actually a directory *)
      if tail = "." then
         check_real_dir cache dir
      else
         check_real_tail cache node dir tail

(*
 * Force case-sensitivity on stat.
 *)
let stat cache node =
   let name = Node.fullname node in
   let stats = Unix.LargeFile.stat name in
      if !check_case then
         check_real_path cache node;
      stats

(*
 * Forced stat, bypassing checks.
 *)
let stat_dir_nocheck cache dir =
   let stats = Unix.LargeFile.stat (Dir.fullname dir) in
      cache.cache_file_stat_count <- cache.cache_file_stat_count + 1;
      stats

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
