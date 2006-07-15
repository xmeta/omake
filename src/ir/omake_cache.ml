(*
 * Keep a cache that determines whether we _really_ need to run
 * a command to build a file.
 *
 * The basic idea is this: for each file, remember the command
 * that built it, its Digest, and the Digest of the files it
 * depends on.  If we want to build the node again, and all
 * the parts are the same, then we don't actually need to build
 * it.
 *
 * The same principle works for scanners.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Jason Hickey, Caltech
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
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_debug
open Lm_printf
open Lm_string_set

open Omake_node
open Omake_command
open Omake_node_sig
open Omake_cache_type

let debug_cache =
   create_debug (**)
      { debug_name = "cache";
        debug_description = "Display debugging information for the cache";
        debug_value = false
      }

(*
 * An index table.
 * The indexes are small enough that (-) is a correct ordering function.
 *)
module IndexCompare =
struct
   type t = int
   let compare = (-)
end

module IndexMTable = Lm_map.LmMakeList (IndexCompare);;

(*
 * Directory entry is a directory or node.
 *)
type dir_entry_core =
   LazyEntryCore of Dir.t * string
 | DirEntryCore  of dir_entry

type dir_listing_item = dir_entry_core ref StringTable.t

type dir_listing = dir_listing_item list

(*
 * Executable listing.
 *)
type exe_entry_core =
   ExeEntryCore of (Dir.t * string) list
 | ExeEntryNodes of Node.t list

type exe_listing_item = exe_entry_core ref StringTable.t

type exe_listing = exe_listing_item list

(*
 * The memo is a record of what we have done during
 * a previous run of this program.
 *
 * The index is a hash over the deps and the commands.
 * The result is mainly used for the scanner, to return
 * the results of the previous scan.
 *
 * We don't really care what the commands are, we just
 * care what their digest is.
 *)
(* %%MAGICBEGIN%% *)
type memo =
   { memo_index    : int;
     memo_targets  : digest NodeTable.t;
     memo_deps     : digest NodeTable.t;
     memo_commands : digest;
     memo_result   : memo_result
   }
(* %%MAGICEND%% *)

(*
 * Stats of a file.
 * FreshStats: we have taken the stats during this program run
 * OldStats: we took the stats during a prior run of this program
 * NoStats: we tried to take stats, but an error occurred (usually
 *    because the file did not exist).
 *)
(* %%MAGICBEGIN%% *)
type node_stats =
   FreshStats   of Unix.LargeFile.stats
 | OldStats     of Unix.LargeFile.stats
 | PartialStats of Unix.LargeFile.stats
 | NoStats
(* %%MAGICEND%% *)

(*
 * For each file, we keep the stats and the digest.
 *)
(* %%MAGICBEGIN%% *)
type node_memo =
   { nmemo_stats  : node_stats;
     nmemo_digest : digest
   }
(* %%MAGICEND%% *)

(*
 * The memos are classified by functions.
 * Each function has a table of memos, and an index.
 *)
type key = int

type cache_info =
   { mutable cache_memos     : memo NodeTable.t;
     mutable cache_index     : Node.t IndexMTable.t
   }

(*
 * The cache remembers all the build commands we did last time.
 *)
type stat = Unix.LargeFile.stats

type cache =
   { (* State *)
     mutable cache_nodes             : node_memo NodeTable.t;
     mutable cache_info              : cache_info array;
     mutable cache_file_stat_count   : int;  (* only succeeded stats are counted *)
     mutable cache_digest_count      : int;

     (* Path lookups *)
     mutable cache_dirs         : (stat option * dir_listing_item) DirHashTable.t;
     mutable cache_path         : (stat option list * dir_listing_item) DirListTable.t;
     mutable cache_exe_path     : (stat option list * exe_listing_item) DirListTable.t;

     (* Hash-cons versions of directories and paths *)
     cache_dir_hash             : DirHash.state;
     cache_path_hash            : DirListHash.state
   }

(*
 * The version of the cache that is saved in the file also
 * contains a table of all nodes.
 *)
type cache_save =
   { save_nodes              : Node.db;
     save_cache_nodes        : node_memo NodeTable.t;
     save_cache_info         : memo NodeTable.t array
   }

(*
 * Squash stat code.
 *)
let squash_stat = Some "squash"

(*
 * The cache type.
 *)
type t = cache

(*
 * Memo functions for the cache.
 *)
let scanner_fun         = 0
let rule_fun            = 1
let env_fun             = 2
let include_fun         = 3

let env_target  = Node.phony_global ".ENV"

(************************************************************************
 * Printing.
 *)

(*
 * Print a digest.
 *)
let pp_print_digest buf digest =
   let s =
      match digest with
         Some digest ->
            Lm_string_util.hexify digest
       | None ->
            "<none>"
   in
      pp_print_string buf s

(*
 * Print a digest table.
 *)
let pp_print_node_digest_table buf deps =
   NodeTable.iter (fun node digest ->
         fprintf buf "@ %a = %a" (**)
            pp_print_node node
            pp_print_digest digest) deps

(*
 * Print a result.
 *)
let pp_print_memo_result pp_print_result buf result =
   match result with
      MemoSuccess ->
         ()
    | MemoFailure code ->
         fprintf buf "@ failed(%d)" code
    | MemoResult result ->
         pp_print_result buf result

(*
 * Print a memo.
 *)
let pp_print_memo buf memo =
   let { memo_index    = index;
         memo_targets  = targets;
         memo_deps     = deps;
         memo_result   = result;
         memo_commands = commands
       } = memo
   in
      fprintf buf "@[<hv 0>@[<hv 3>memo {@ index = %d;@ @[<b 3>targets =%a@];@ @[<b 3>deps =%a@]@ @[<hv 3>commands =%a@]@]@ }@]" (**)
         index
         pp_print_node_digest_table targets
         pp_print_node_digest_table deps
         pp_print_digest commands

(************************************************************************
 * Persistence.
 *)

(*
 * Create the cache.  We save the cache in a file, so try to
 * load it from that file.
 *)
let magic_number = Omake_magic.cache_magic
let input_magic  = Omake_magic.input_magic
let output_magic = Omake_magic.output_magic

(*
 * Create a new cache.
 *)
let create () =
   { cache_nodes           = NodeTable.empty;
     cache_info            = [||];
     cache_file_stat_count = 0;
     cache_digest_count    = 0;
     cache_dirs            = DirHashTable.empty;
     cache_path            = DirListTable.empty;
     cache_exe_path        = DirListTable.empty;
     cache_dir_hash        = DirHash.create_state ();
     cache_path_hash       = DirListHash.create_state ()
   }

let rehash cache =
   cache.cache_dirs <- DirHashTable.empty;
   cache.cache_path <- DirListTable.empty;
   cache.cache_exe_path <- DirListTable.empty

let stats { cache_file_stat_count = stat_count;
            cache_digest_count = digest_count
    } =
   stat_count, digest_count

(*
 * Clear one of the tables.
 *)
let clear cache index =
   let info = cache.cache_info in
      if index < Array.length info then
         let info = info.(index) in
            info.cache_memos <- NodeTable.empty;
            info.cache_index <- IndexMTable.empty

(*
 * When the file is saved, remove all stat information,
 * as well as any files that don't exist.
 *)
let save_of_cache cache =
   let { cache_nodes = cache_nodes;
         cache_info  = cache_info
       } = cache
   in
   let nodes = Node.marshal_base () in
   let cache_nodes =
      NodeTable.fold (fun nodes target nmemo ->
            match nmemo.nmemo_stats with
               FreshStats stats
             | OldStats stats ->
                  let nmemo = { nmemo with nmemo_stats = OldStats stats } in
                     NodeTable.add nodes target nmemo
             | PartialStats _
             | NoStats ->
                  nodes) NodeTable.empty cache_nodes
   in
   let cache_info = Array.map (fun info -> info.cache_memos) cache_info in
      { save_nodes       = nodes;
        save_cache_nodes = cache_nodes;
        save_cache_info  = cache_info
      }

(*
 * Rebuild the index from the memos.
 *)
let create_index memos =
   NodeTable.fold (fun (memos, index) target memo ->
         let { memo_result = result;
               memo_index = hash
             } = memo
         in
            match result with
               MemoSuccess
             | MemoResult _ ->
                  let index = IndexMTable.add index hash target in
                     memos, index
             | MemoFailure _ ->
                  let memos = NodeTable.remove memos target in
                     memos, index) (memos, IndexMTable.empty) memos

(*
 * Rebuild the cache from the saved version.
 *)
let cache_of_save save =
   let { save_nodes = nodes;
         save_cache_nodes = cache_nodes;
         save_cache_info  = cache_info
       } = save
   in
   let cache_info =
      Array.map (fun memos ->
            let memos, index = create_index memos in
               { cache_memos = memos;
                 cache_index = index
               }) cache_info
   in
      Node.unmarshal_base nodes;
      { (create ()) with
        cache_nodes = cache_nodes;
        cache_info  = cache_info;
      }

(*
 * Load the old cache from a file.
 *)
let from_channel inx =
   let magic = input_magic inx in
   let _ =
      if magic <> magic_number then
         raise (Sys_error "bad magic number")
   in
   let save = Marshal.from_channel inx in
      cache_of_save save

(*
 * Save the cache to the file.
 *)
let to_channel outx cache =
   output_magic outx magic_number;
   Marshal.to_channel outx (save_of_cache cache) []

(************************************************************************
 * Stat.
 *)

(*
 * Reset the information about a node (probably because
 * we ran a command that changed it).
 *)
let reset cache node =
   let nodes = cache.cache_nodes in
   let nodes =
      try
         NodeTable.filter_remove nodes node (fun nmemo ->
               match nmemo.nmemo_stats with
                  FreshStats stats
                | OldStats stats ->
                     Some { nmemo with nmemo_stats = OldStats stats }
                | PartialStats _
                | NoStats ->
                     None)
      with
         Not_found ->
            nodes
   in
      cache.cache_nodes <- nodes

let reset_set cache nodes =
   NodeSet.iter (reset cache) nodes

let reset_table cache nodes =
   NodeTable.iter (fun node _ -> reset cache node) nodes

(*
 * Test whether stats are equal enough that we think the
 * file is up-to-date.
 *)
let stats_equal stat1 stat2 =
   let { Unix.LargeFile.st_ino   = ino1;
         Unix.LargeFile.st_kind  = kind1;
         Unix.LargeFile.st_size  = size1;
         Unix.LargeFile.st_mtime = mtime1
       } = stat1
   in
   let { Unix.LargeFile.st_ino   = ino2;
         Unix.LargeFile.st_kind  = kind2;
         Unix.LargeFile.st_size  = size2;
         Unix.LargeFile.st_mtime = mtime2
       } = stat2
   in
      ino1 = ino2 && kind1 = kind2 && size1 = size2 && mtime1 = mtime2

let pp_print_stat buf stat =
   let { Unix.LargeFile.st_ino  = ino;
         Unix.LargeFile.st_kind = kind;
         Unix.LargeFile.st_size = size;
         Unix.LargeFile.st_mtime = mtime
       } = stat
   in
      fprintf buf "ino = %d, size = %Ld, mtime = %g" ino size mtime

(*
 * Stat a file.
 *)
let stat_file cache node =
   let nodes = cache.cache_nodes in
   let stats =
      try Some (NodeTable.find nodes node) with
         Not_found ->
            None
   in
      match stats with
         Some { nmemo_stats = FreshStats _;
                nmemo_digest = digest
         }
       | Some { nmemo_stats = NoStats;
                nmemo_digest = digest
         } ->
            (*
             * We've recently computed the digest for this file.
             *)
            digest
       | Some ({ nmemo_stats = OldStats stats } as nmemo) ->
            (*
             * We have no recent record of this file.
             * Get current stats.  If they match, then
             * use current digest; otherwise recompute.
             *)
            let name = Node.fullname node in
            let nmemo =
               try
                  let stats' = Unix.LargeFile.stat name in
                     if stats'.Unix.LargeFile.st_kind = Unix.S_DIR then
                        { nmemo_stats = FreshStats stats';
                          nmemo_digest = squash_stat
                        }
                     else begin
                        cache.cache_file_stat_count <- succ cache.cache_file_stat_count;
                        if stats_equal stats' stats then
                           { nmemo with nmemo_stats = FreshStats stats' }
                        else
                           let digest = Digest.file name in
                              cache.cache_digest_count <- succ cache.cache_digest_count;
                              { nmemo_stats = FreshStats stats';
                                nmemo_digest = Some digest
                              }
                     end
               with
                  Unix.Unix_error _
                | Sys_error _ ->
                     { nmemo_stats = NoStats;
                       nmemo_digest = None
                     }
            in
               cache.cache_nodes <- NodeTable.add nodes node nmemo;
               nmemo.nmemo_digest

       | Some { nmemo_stats = PartialStats stats } ->
            (*
             * We have taken stats of this file, and found that they
             * differed from the old value, so the digest is out-of-date.
             *)
            let name = Node.fullname node in
            let nmemo =
               try
                  if stats.Unix.LargeFile.st_kind = Unix.S_DIR then
                     { nmemo_stats = FreshStats stats;
                       nmemo_digest = squash_stat
                     }
                  else
                     let digest = Digest.file name in
                        cache.cache_digest_count <- succ cache.cache_digest_count;
                        { nmemo_stats = FreshStats stats;
                          nmemo_digest = Some digest
                        }
               with
                  Unix.Unix_error _
                | Sys_error _ ->
                     { nmemo_stats = NoStats;
                       nmemo_digest = None
                     }
            in
               cache.cache_nodes <- NodeTable.add nodes node nmemo;
               nmemo.nmemo_digest

       | None ->
            (*
             * We've never seen this file before.
             * Get complete stats, including a digest.
             *)
            let name = Node.fullname node in
            let nmemo =
               try
                  let stats = Unix.LargeFile.stat name in
                     if stats.Unix.LargeFile.st_kind = Unix.S_DIR then
                        { nmemo_stats = FreshStats stats;
                          nmemo_digest = squash_stat
                        }
                     else begin
                        cache.cache_file_stat_count <- succ cache.cache_file_stat_count;
                        let digest = Digest.file name in
                           cache.cache_digest_count <- succ cache.cache_digest_count;
                           { nmemo_stats = FreshStats stats;
                             nmemo_digest = Some digest
                           }
                     end
               with
                  Unix.Unix_error _
                | Sys_error _ ->
                     { nmemo_stats = NoStats;
                       nmemo_digest = None
                     }
            in
               cache.cache_nodes <- NodeTable.add nodes node nmemo;
               nmemo.nmemo_digest

(*
 * Return just the Unix stat information.
 * Avoid taking a full digest in this case.
 *)
let stat_unix_simple stats =
   match stats with
      FreshStats stats
    | PartialStats stats ->
         stats
    | NoStats ->
         raise Not_found
    | OldStats _ ->
         raise (Invalid_argument "Omake_cache.stat_unix_simple")

let stat_unix cache node =
   let nodes = cache.cache_nodes in
   let stats =
      try Some (NodeTable.find nodes node) with
         Not_found ->
            None
   in
      match stats with
         Some { nmemo_stats = FreshStats stats }
       | Some { nmemo_stats = PartialStats stats } ->
            stats
       | Some { nmemo_stats = NoStats } ->
            raise Not_found
       | Some ({ nmemo_stats = OldStats old_stats } as nmemo) ->
            let name = Node.fullname node in
            let nmemo =
               try
                  let new_stats = Unix.LargeFile.stat name in
                     if new_stats.Unix.LargeFile.st_kind = Unix.S_DIR then
                        { nmemo_stats = FreshStats new_stats;
                          nmemo_digest = squash_stat
                        }
                     else begin
                        cache.cache_file_stat_count <- succ cache.cache_file_stat_count;
                        if stats_equal new_stats old_stats then
                           { nmemo with nmemo_stats = FreshStats new_stats }
                        else
                           { nmemo with nmemo_stats = PartialStats new_stats }
                     end
               with
                  Unix.Unix_error _
                | Sys_error _ ->
                     { nmemo_stats = NoStats;
                       nmemo_digest = None
                     }
            in
               cache.cache_nodes <- NodeTable.add nodes node nmemo;
               stat_unix_simple nmemo.nmemo_stats
       | None ->
            let name = Node.fullname node in
            let nmemo =
               try
                  let stats = Unix.LargeFile.stat name in
                     if stats.Unix.LargeFile.st_kind = Unix.S_DIR then
                        { nmemo_stats = FreshStats stats;
                          nmemo_digest = squash_stat
                        }
                     else begin
                        cache.cache_file_stat_count <- succ cache.cache_file_stat_count;
                        { nmemo_stats = PartialStats stats;
                          nmemo_digest = None
                        }
                     end
               with
                  Unix.Unix_error _
                | Sys_error _ ->
                     { nmemo_stats  = NoStats;
                       nmemo_digest = None
                     }
            in
               cache.cache_nodes <- NodeTable.add nodes node nmemo;
               stat_unix_simple nmemo.nmemo_stats

(*
 * Check if a file is a directory.
 *)
let is_dir cache node =
   try (stat_unix cache node).Unix.LargeFile.st_kind = Unix.S_DIR with
      Not_found ->
         false

(*
 * Tests for whether a file is executable.
 * This really only works on Unix.
 *)
let euid =
   try Unix.geteuid () with
      Unix.Unix_error _ ->
         0

let groups =
   try Array.to_list (Unix.getgroups ()) with
      Unix.Unix_error _ ->
         []

(*
 * Check if the node is phony first.
 *)
let stat cache node =
   let core = Node.core node in
      match Node.kind node with
         NodePhony
       | NodeScanner ->
            None
       | NodeOptional
       | NodeNormal ->
            stat_file cache core
       | NodeSquashed
       | NodeExists ->
            (match stat_file cache core with
                Some _ ->
                   squash_stat
              | None ->
                   None)

(*
 * Turn a set into a table of stat info.
 *)
let stat_set cache nodes =
   NodeSet.fold (fun table node ->
         NodeTable.add table node (stat cache node)) NodeTable.empty nodes

let stat_table cache nodes =
   NodeTable.mapi (fun node _ -> stat cache node) nodes

(*
 * Force a stat.
 *)
let force_stat cache node =
   reset cache node;
   stat cache node

let force_stat_set cache nodes =
   NodeSet.fold (fun table node ->
         NodeTable.add table node (force_stat cache node)) NodeTable.empty nodes

let force_stat_table cache nodes =
   NodeTable.mapi (fun node _ -> force_stat cache node) nodes

(*
 * Check if the stat changed.
 *)
let stat_changed cache node =
   let old_digest =
      try (NodeTable.find cache.cache_nodes node).nmemo_digest with
         Not_found ->
            None
   in
   let new_digest = force_stat cache node in
      new_digest <> old_digest

(*
 * Check if a file exists.
 *)
let exists cache node =
   match stat cache node with
      Some _ -> true
    | None -> Node.always_exists node

let exists_dir cache dir =
   exists cache (Node.node_of_dir dir)

(************************************************************************
 * Adding to the cache.
 *)

(*
 * Hash a set of deps and commands.
 * The commands is a digest.
 *)
let hash_index deps commands =
   let index =
      NodeSet.fold (fun index node ->
            index lxor (index lsl 4) lxor (index lsr 4) lxor (Node.hash node)) 0 deps
   in
   let index = index lxor (index lsl 4) lxor (index lsr 4) lxor (Hashtbl.hash commands) in
      index land 0x3fffffff

(*
 * Expand the cache_info if necessary.
 *)
let get_info cache key =
   let cache_info = cache.cache_info in
   let len = Array.length cache_info in
   let cache_info =
      if key >= Array.length cache_info then
         begin
            let cache_info' =
               Array.init (succ key) (fun _ ->
                     { cache_memos = NodeTable.empty;
                       cache_index = IndexMTable.empty
                     })
            in
               Array.blit cache_info 0 cache_info' 0 len;
               cache.cache_info <- cache_info';
               cache_info'
         end
      else
         cache_info
   in
      cache_info.(key)

(*
 * Add a command.
 *)
let add cache key target targets deps commands result =
   let index = hash_index deps commands in
   let memo =
      { memo_index    = index;
        memo_targets  = stat_set cache targets;
        memo_deps     = stat_set cache deps;
        memo_result   = result;
        memo_commands = commands
      }
   in
   let info = get_info cache key in
      info.cache_memos <- NodeTable.add info.cache_memos target memo;
      info.cache_index <- IndexMTable.add info.cache_index index target

(*
 * Check the target digest.
 *)
let targets_equal cache targets =
   NodeTable.forall (fun target digest ->
         stat cache target = digest) targets

(*
 * Check if deps are the same.
 * This returns true if the deps are equal.
 * If not, it either returns false, or raises Not_found.
 *)
let deps_equal cache deps1 deps2 =
   let count1 = NodeSet.cardinal deps1 in
   let count2 = NodeTable.cardinal deps2 in
      (count1 = count2) && NodeSet.for_all (fun dep1 ->
            let digest1 = stat cache dep1 in
            let digest2 = NodeTable.find deps2 dep1 in
               digest1 = digest2) deps1

(*
 * Find a memo from the deps and commands.
 *)
let find_memo cache key deps commands =
   let { cache_memos = memos;
         cache_index = index
       } = get_info cache key
   in
   let hash = hash_index deps commands in
   let rec search = function
      target :: targets ->
         let memo = NodeTable.find memos target in
         let { memo_index = hash';
               memo_deps  = deps';
               memo_commands = commands'
             } = memo
         in
            if hash' = hash && deps_equal cache deps deps' && commands = commands' then
               memo
            else
               search targets
    | [] ->
         raise Not_found
   in
      search (IndexMTable.find_all index hash)

(*
 * A memo has not changed if all the deps and the target
 * have the same digests.
 *)
let up_to_date cache key deps commands =
   try
      let memo = find_memo cache key deps commands in
         if targets_equal cache memo.memo_targets then
            match memo.memo_result with
               MemoSuccess
             | MemoResult _ ->
                  true
             | MemoFailure _ ->
                  false
         else
            false
   with
      Not_found ->
         false

let up_to_date_status cache key deps commands =
   try
      let memo = find_memo cache key deps commands in
         if targets_equal cache memo.memo_targets then
            match memo.memo_result with
               MemoSuccess
             | MemoResult _ ->
                  StatusSuccess
             | MemoFailure code ->
                  StatusFailure code
         else
            StatusUnknown
   with
      Not_found ->
         StatusUnknown

(*
 * A memo has not changed if all the deps and the target
 * have the same digests.
 *)
let target_results results =
   match results with
      MemoFailure _ ->
         raise Not_found
    | MemoSuccess ->
         NodeTable.empty
    | MemoResult results ->
         results

let find_result cache key deps commands =
   let memo = find_memo cache key deps commands in
   let { memo_targets = targets;
         memo_result  = result
       } = memo
   in
      if targets_equal cache targets then
         target_results result
      else
         raise Not_found

(*
 * Find the result of a run, without the commands.
 *)
let find_result_sloppy cache key target =
   let { cache_memos = memos } = get_info cache key in
   let memo = NodeTable.find memos target in
      match memo.memo_result with
         MemoFailure _ ->
            raise Not_found
       | MemoSuccess ->
            NodeTable.empty
       | MemoResult result ->
            result

(************************************************************************
 * Directory listings.
 *)

(*
 * When auto-rehash is in effect, we need to stat the directories
 * on every lookup.
 *)
let stat_dir cache dir =
   let name = Dir.fullname dir in
      try
         let stat = Unix.LargeFile.stat name in
            cache.cache_file_stat_count <- succ cache.cache_file_stat_count;
            Some stat
      with
         Unix.Unix_error _ ->
            None

let stat_dirs cache dirs =
   List.map (stat_dir cache) dirs

let stats_equal_opt stat1 stat2 =
   match stat1, stat2 with
      Some stat1, Some stat2 ->
         stats_equal stat1 stat2
    | None, None ->
         true
    | None, Some _
    | Some _, None ->
         false

let rec stats_equal_opt_list stats1 stats2 =
   match stats1, stats2 with
      stat1 :: stats1, stat2 :: stats2 ->
         stats_equal_opt stat1 stat2 && stats_equal_opt_list stats1 stats2
    | [], [] ->
         true
    | _ :: _, []
    | [], _ :: _ ->
         false

let check_stat auto_rehash (stat_old, entries) stat_new =
   if auto_rehash && not (stats_equal_opt stat_old (Lazy.force stat_new)) then
      raise Not_found
   else
      entries

let check_stats auto_rehash (stats_old, entries) stats_new =
   if auto_rehash && not (stats_equal_opt_list stats_old (Lazy.force stats_new)) then
      raise Not_found
   else
      entries

(*
 * List a directory.
 *)
let rec list_directory cache dir =
   let dirx =
      try Unix.opendir (Dir.fullname dir) with
         Unix.Unix_error _ ->
            raise Not_found
   in
   let rec list entries =
      let name =
         try Some (Unix.readdir dirx) with
            Unix.Unix_error _
          | End_of_file ->
               None
      in
         match name with
            Some "."
          | Some ".." ->
               list entries
          | Some name ->
               let entry = ref (LazyEntryCore (dir, name)) in
               let entries = StringTable.add entries name entry in
                  list entries
          | None ->
               entries
   in
   let entries = list StringTable.empty in
      Unix.closedir dirx;
      entries

(*
 * Get the directory listing as a StringTable.
 *)
let ls_dir cache auto_rehash dir =
   let key = DirHash.create cache.cache_dir_hash dir in
   let stat = lazy (stat_dir cache dir) in
      try check_stat auto_rehash (DirHashTable.find cache.cache_dirs key) stat with
         Not_found ->
            let entries = list_directory cache dir in
            let stat = Lazy.force stat in
               cache.cache_dirs <- DirHashTable.add cache.cache_dirs key (stat, entries);
               entries

(*
 * Path version.
 *)
let ls_path cache auto_rehash dirs =
   let keys = List.map (DirHash.create cache.cache_dir_hash) dirs in
   let key = DirListHash.create cache.cache_path_hash keys in
   let stats = lazy (stat_dirs cache dirs) in
      try check_stats auto_rehash (DirListTable.find cache.cache_path key) stats with
         Not_found ->
            (* Fold together the tables *)
            let stats = Lazy.force stats in
            let entries =
               List.fold_left (fun entries1 dir ->
                     try
                        let entries2 = ls_dir cache auto_rehash dir in
                           StringTable.fold StringTable.add entries1 entries2
                     with
                        Not_found ->
                           entries1) StringTable.empty (List.rev dirs)
            in
               cache.cache_path <- DirListTable.add cache.cache_path key (stats, entries);
               entries

(*
 * Resolve an entry in the listing.
 *)
let listing_find_item cache listing s =
   let entry_ref = StringTable.find listing s in
      match !entry_ref with
         DirEntryCore entry ->
            entry
       | LazyEntryCore (dir, s) ->
            let node = Node.intern no_mount_points PhonyProhibited dir s in
            let entry =
               if is_dir cache node then
                  DirEntry (Dir.chdir dir s)
               else
                  NodeEntry node
            in
               entry_ref := DirEntryCore entry;
               entry

(*
 * The execution path is a little harder, and it is quite different
 * on Win32 and Unix.
 *
 * On Win32:
 *    - File permission doesn't matter
 *    - Files without suffix, and with .exe and .bat suffixes are executable
 *
 * On Cygwin:
 *    - Files without suffix must be executable
 *    - Files with .exe and .bat suffixes are executable
 *
 * On Unix:
 *    - There is no .exe suffix
 *    - Only files that are executable count
 *)
let ls_exe_path_win32 cache auto_rehash dirs =
   let keys = List.map (DirHash.create cache.cache_dir_hash) dirs in
   let key = DirListHash.create cache.cache_path_hash keys in
   let stats = lazy (stat_dirs cache dirs) in
      try check_stats auto_rehash (DirListTable.find cache.cache_exe_path key) stats with
         Not_found ->
            let entries =
               List.fold_left (fun entries1 dir ->
                     try
                        let entries2 = ls_dir cache auto_rehash dir in
                           StringTable.fold (fun entries name _ ->
                                 let info = dir, name in
                                 let name = String.lowercase name in
                                    if Filename.check_suffix name ".exe" || Filename.check_suffix name ".bat" then
                                       let name_core = Filename.chop_extension name in
                                       let entries = StringMTable.add entries name_core info in
                                          StringMTable.add entries name info
                                    else
                                       StringMTable.add entries name info) entries1 entries2
                     with
                        Not_found ->
                           entries1) StringMTable.empty (List.rev dirs)
            in
            let entries =
               StringMTable.fold_all (fun entries name info ->
                     StringTable.add entries name (ref (ExeEntryCore info))) StringTable.empty entries
            in
            let stats = Lazy.force stats in
               cache.cache_exe_path <- DirListTable.add cache.cache_exe_path key (stats, entries);
               entries

let ls_exe_path_unix cache auto_rehash dirs =
   let keys = List.map (DirHash.create cache.cache_dir_hash) dirs in
   let key = DirListHash.create cache.cache_path_hash keys in
   let stats = lazy (stat_dirs cache dirs) in
      try check_stats auto_rehash (DirListTable.find cache.cache_exe_path key) stats with
         Not_found ->
            let entries =
               List.fold_left (fun entries1 dir ->
                     try
                        let entries2 = ls_dir cache auto_rehash dir in
                           StringTable.fold (fun entries name _ ->
                                 StringMTable.add entries name (dir, name)) entries1 entries2
                     with
                        Not_found ->
                           entries1) StringMTable.empty (List.rev dirs)
            in
            let entries =
               StringMTable.fold_all (fun entries name info ->
                     StringTable.add entries name (ref (ExeEntryCore info))) StringTable.empty entries
            in
            let stats = Lazy.force stats in
               cache.cache_exe_path <- DirListTable.add cache.cache_exe_path key (stats, entries);
               entries

(*
 * Find the first entry that is executable.
 *)
let is_exe_file cache node =
   try
      let { Unix.LargeFile.st_kind = kind;
            Unix.LargeFile.st_perm = perm;
            Unix.LargeFile.st_uid = uid;
            Unix.LargeFile.st_gid = gid
          } = Unix.LargeFile.stat (Node.fullname node)
      in
         (kind = Unix.S_REG)
         && ((perm land 0o001) <> 0
             || (List.mem gid groups && (perm land 0o010) <> 0)
             || (uid = euid && (perm land 0o100) <> 0))
   with
      Unix.Unix_error _
    | Not_found ->
         false

let is_exe_win32 cache dir s =
   let node = Node.intern no_mount_points PhonyProhibited dir s in
      if is_dir cache node then
         None
      else
         Some node

let is_exe_unix cache dir s =
   let node = Node.intern no_mount_points PhonyProhibited dir s in
      if is_exe_file cache node then
         Some node
      else
         None

let is_exe_cygwin cache dir s =
   let node = Node.intern no_mount_points PhonyProhibited dir s in
      if Filename.check_suffix s ".exe" || Filename.check_suffix s ".bat" || is_exe_file cache node then
         Some node
      else
         None

let ls_exe_path, is_exe, name_exe =
   if Sys.os_type = "Win32" then
      ls_exe_path_win32, is_exe_win32, String.lowercase
   else if Sys.os_type = "Cygwin" then
      ls_exe_path_win32, is_exe_cygwin, String.lowercase
   else
      ls_exe_path_unix, is_exe_unix, (fun s -> s)

let rec search_exe cache entries =
   Lm_list_util.some_map (fun (dir, s) ->
         is_exe cache dir s) entries

let exe_find_nodes cache listing s =
   let entry_ref = StringTable.find listing (name_exe s) in
      match !entry_ref with
         ExeEntryCore entries ->
            let nodes = search_exe cache entries in
               entry_ref := ExeEntryNodes nodes;
               nodes
       | ExeEntryNodes nodes ->
            nodes

let exe_find_nodes_all cache listing s =
   try exe_find_nodes cache listing s with
      Not_found ->
         []

let exe_find_item cache listing s =
   match exe_find_nodes cache listing s with
      node :: _ ->
         node
    | [] ->
         raise Not_found

(************************************************************************
 * Redefine the functions to work on directory groups, where each group may
 * specify auto-rehashing.
 *)
let rec listing_find cache listings s =
   match listings with
      [listing] ->
         listing_find_item cache listing s
    | listing :: listings ->
         (try listing_find_item cache listing s with
             Not_found ->
                listing_find cache listings s)
    | [] ->
         raise Not_found

let rec exe_find cache listings s =
   match listings with
      [listing] ->
         exe_find_item cache listing s
    | listing :: listings ->
         (try exe_find_item cache listing s with
             Not_found ->
                exe_find cache listings s)
    | [] ->
         raise Not_found

let exe_find_all cache listings s =
   List.flatten (List.map (fun listing -> exe_find_nodes_all cache listing s) listings)

let ls_dir cache auto_rehash dir =
   [ls_dir cache auto_rehash dir]

let ls_path cache groups =
   List.map (fun (auto_rehash, dirs) ->
         ls_path cache auto_rehash dirs) groups

let ls_exe_path cache groups =
   List.map (fun (auto_rehash, dirs) ->
         ls_exe_path cache auto_rehash dirs) groups

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
