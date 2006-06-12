(*
 * Configuration variables.
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
open Lm_filename_util

open Omake_options_type

(*
 * The argument string is a colon-separated list of server specification.
 * A server spec can be:
 *    1. a number: this specifies the job_count
 *    2. a machine: this specified a remote server that will handle 1 job
 *    3. a machine=count: a remote server that will handle <count> jobs
 *)
let set_job_count options s =
   let set_job (job_count, remote_servers) job =
      try
         let index = String.index job '=' in
         let len = String.length job in
         let machine = String.sub job 0 index in
         let count = String.sub job (succ index) (len - index - 1) in
         let count =
            try int_of_string count with
               Failure _ ->
                  1
         in
            job_count, (machine, count) :: remote_servers
      with
         Not_found ->
            try int_of_string job, remote_servers with
               Failure _ ->
                  job_count, (job, 1) :: remote_servers
   in
   let job_count, remote_servers = List.fold_left set_job (1, []) (Lm_string_util.split ":" s) in
      { options with opt_job_count = job_count;
                     opt_remote_servers = List.rev remote_servers
      }

(*
 * Default options.
 *)
let default_options =
   { opt_job_count            = 1;
     opt_remote_servers       = [];
     opt_terminate_on_error   = true;
     opt_dry_run              = false;
     opt_print_command        = EvalEager;
     opt_print_dir            = false;
     opt_print_file           = true;
     opt_print_status         = true;
     opt_print_exit           = false;
     opt_print_progress       = false;
     opt_touch_only           = false;
     opt_flush_cache          = false;
     opt_flush_env            = true;
     opt_flush_dependencies   = false;
     opt_print_dependencies   = false;
     opt_show_dependencies    = [];
     opt_all_dependencies     = false;
     opt_verbose_dependencies = false;
     opt_cd_root              = false;
     opt_project              = false;
     opt_poll                 = false;
     opt_poll_on_done         = false;
     opt_flush_include        = false;
     opt_flush_static         = false;
     opt_allow_exceptions     = false;
     opt_absname              = false
   }

(*
 * Predicate returns true iff there are parallel jobs.
 *)
let is_parallel options =
   options.opt_job_count > 1 || options.opt_remote_servers <> []

(*
 * Error codes for various actions.
 *)
let signal_error_code   = 127
let fork_error_code     = 126
let internal_error_code = 125
let deadlock_error_code = 124
let exn_error_code      = 123
let scanner_error_code  = 122

(*
 * Name of the database.
 *)
let db_name = ".omakedb"

(*
 * Name of the makefiles.
 *)
let makefile_name = "OMakefile"
let makeroot_name = "OMakeroot"
let omake_file_suffix = ".om"
let makeroot_short_name = "Root" ^ omake_file_suffix

let omake_dir_ref = ref None
let cache_dir_ref = ref None
let always_use_dotomake = ref false

let set_omake_dir dir =
   let () =
      try Unix.mkdir dir 0o777 with
         Unix.Unix_error _ ->
            ()
   in
      omake_dir_ref := Some dir;
      cache_dir_ref := None

(*
 * Argument specifier.
 *
 * NOTE!  This set of options is functional and scoped in OMakefiles.
 * Global, non-scoped options that assign to reference cells should be
 * put in the option list in Omake_main, not here.
 *)
let options_spec =
   ["-j", Lm_arg.StringFold set_job_count, (**)
       "Specify parallel jobs and remote servers";
    "-k", Lm_arg.UnitFold (fun options ->  { options with opt_terminate_on_error = false }), (**)
       "Do not stop when an error occurs";
    "-p", Lm_arg.SetFold (fun options b ->
          { options with opt_poll = b;
                         opt_terminate_on_error = false }), (**)
       "Poll filesystem for changes (until build succeeds); implies -k";
    "-P", Lm_arg.SetFold (fun options b ->
          { options with opt_poll = b;
                         opt_terminate_on_error = false;
                         opt_poll_on_done = true }), (**)
       "Poll filesystem for changes (keep polling \"forever\"); implies -k";
    "-n", Lm_arg.SetFold (fun options b -> { options with opt_dry_run = b }), (**)
       "Print commands, but do not execute them";
    "-s", Lm_arg.UnitFold (fun options ->
          { options with opt_print_status = false;
                         opt_print_dir  = false;
                         opt_print_file = false;
                         opt_print_exit = false;
                         opt_print_command = EvalNever }), (**)
       "Do not print commands as they are executed";
    "-S", Lm_arg.UnitFold (fun options -> { options with opt_print_command = EvalLazy }), (**)
       "Print command only if the command prints output";
    "--project", Lm_arg.SetFold (fun options b -> { options with opt_project = b }), (**)
       "Ignore the current directory and build the project";
    "--progress", Lm_arg.SetFold (fun options b -> { options with opt_print_progress = b }), (**)
       "Print a progress indicator";
    "--no-progress", Lm_arg.ClearFold (fun options b -> { options with opt_print_progress = b }), (**)
       "Do not print a progress indicator";
    "--print-status", Lm_arg.SetFold (fun options b -> { options with opt_print_status = b }), (**)
       "Print status lines";
    "--no-print-status", Lm_arg.ClearFold (fun options b -> { options with opt_print_file = b }), (**)
       "Do not print status lines";
    "-w", Lm_arg.SetFold (fun options b -> { options with opt_print_dir = b }), (**)
       "Print the directory in \"make format\" as commands are executed";
    "--no-print-exit", Lm_arg.ClearFold (fun options b -> { options with opt_print_exit = b }), (**)
       "Do not print exit codes";
    "--print-exit", Lm_arg.SetFold (fun options b -> { options with opt_print_exit = b }), (**)
       "Print the exit codes of commands";
    "-t", Lm_arg.SetFold (fun options b -> { options with opt_touch_only = b }), (**)
       "Update database to force files to be up-to-date";
    "--depend", Lm_arg.SetFold (fun options b -> { options with opt_flush_dependencies = b }), (**)
       "Do not trust cached dependecy information";
    "-U", Lm_arg.SetFold (fun options b -> { options with opt_flush_cache = b }), (**)
       "Do not trust the dependency cache or cached OMakefiles";
    "-T", Lm_arg.ClearFold (fun options b -> { options with opt_flush_env = b }), (**)
       "Trust cached OMakefiles";
    "-u", Lm_arg.SetFold (fun options b -> { options with opt_flush_env = b }), (**)
       "Do not trust cached OMakefiles (default)";
    "--flush-includes", Lm_arg.SetFold (fun options b -> { options with opt_flush_include = b }), (**)
       "Do not trust cached .omc files";
    "--configure", Lm_arg.SetFold (fun options b -> { options with opt_flush_static = b }), (**)
       "Recompute static. sections";
    "-R", Lm_arg.SetFold (fun options b -> { options with opt_cd_root = b }), (**)
       "Command-line targets are relative to the root";
    "--print-dependencies", Lm_arg.SetFold (fun options b -> { options with opt_print_dependencies = b }), (**)
       "Build and print dependencies";
    "--show-dependencies", Lm_arg.StringFold (fun options s ->
          { options with opt_show_dependencies = s :: options.opt_show_dependencies }), (**)
       "Show dependencies if the file is built";
    "--all-dependencies", Lm_arg.SetFold (fun options b ->
          { options with opt_all_dependencies = b }), (**)
       "For --print-dependencies and --show-dependencies, print dependencies recursively";
    "--verbose-dependencies", Lm_arg.SetFold (fun options b ->
          { options with opt_verbose_dependencies = b }), (**)
       "For --print-dependencies and --show-dependencies, print all dependencies too";
    "--absname", Lm_arg.SetFold (fun options b -> { options with opt_absname = b }), (**)
       "Filenames are always displayed as absolute paths"]


(*
 * Directories.
 *)
let lib_dir, lib_dir_reason =
   let key_name = "SOFTWARE\\MetaPRL\\OMake" in
   let field_name = "OMAKELIB" in
      try
         Sys.getenv field_name, "OMAKELIB environment variable"
      with Not_found ->
         try
            Lm_unix_util.registry_find Lm_unix_util.HKEY_CURRENT_USER key_name field_name,
               "HKEY_CURRENT_USER\\" ^ key_name ^ "\\" ^ field_name ^ " registry key"
         with Not_found ->
            try
               Lm_unix_util.registry_find Lm_unix_util.HKEY_LOCAL_MACHINE key_name field_name,
                  "HKEY_LOCAL_MACHINE\\" ^ key_name ^ "\\" ^ field_name ^ " registry key"
            with Not_found ->
               Omake_magic.lib_dir, ""

let home_dir = Lm_unix_util.home_dir
let application_dir = Lm_unix_util.application_dir

let omakeinit_file = Filename.concat home_dir ".omakeinit"
let omakerc_file = Filename.concat home_dir ".omakerc"
let oshrc_file = Filename.concat home_dir ".oshrc"

let omake_dir () =
   match !omake_dir_ref with
      Some dir ->
         dir
    | None ->
         let dirname = Filename.concat application_dir ".omake" in
            set_omake_dir dirname;
            dirname

(*
 * Cache directory is separate for each host.
 *)
let cache_dir () =
   match !cache_dir_ref with
      Some dir ->
         dir
    | None ->
         let dirname = Filename.concat (omake_dir ()) "cache" in
         let () =
            try Unix.mkdir dirname 0o777 with
               Unix.Unix_error _ ->
                  ()
         in
            cache_dir_ref := Some dirname;
            dirname

(* Create cache file hierarchy under the HOME directory *)
let cache_file dir name =
   let dir =
      match Lm_filename_util.filename_string dir with
         AbsolutePath (DriveRoot c, name) ->
            Filename.concat (String.make 1 c) name
       | AbsolutePath (NullRoot, name) ->
            name
       | RelativePath path ->
            raise (Invalid_argument ("Omake_state.cache_file: received a relative path: " ^ path))
   in
   let dirname = Filename.concat (cache_dir ()) dir in
      Lm_filename_util.mkdirhier dirname 0o777;
      Filename.concat dirname name

let open_cache_file dir name =
   let filename = cache_file dir name in
      filename, Lm_unix_util.openfile filename [Unix.O_RDWR; Unix.O_CREAT] 0o666

let get_cache_file dir name =
   if !always_use_dotomake then
      open_cache_file dir name
   else
      let filename = Filename.concat dir name in
         try filename, Lm_unix_util.openfile filename [Unix.O_RDWR; Unix.O_CREAT] 0o666 with
            Unix.Unix_error _ ->
               open_cache_file dir name

(*
 * XXX: TODO: We use lockf, but it is not NFS-safe if filesystem is mounted w/o locking.
 * Also, lockf is not always supported, so we may raise an exception for a "wrong" reason.
 * May be we should implement a "sloppy" locking as well - see
 * also the mailing list discussions:
 *    - http://lists.metaprl.org/pipermail/omake/2005-November/thread.html#744
 *    - http://lists.metaprl.org/pipermail/omake-devel/2005-November/thread.html#122
 *)
let lock_cache_file dir name =
   let filename_fd = get_cache_file dir name in
   let _, fd = filename_fd in
      Lm_unix_util.lockf fd Unix.F_LOCK max_int;
      filename_fd

let db_file () =
   Filename.concat (omake_dir ()) db_name

let history_file () =
   Filename.concat (omake_dir ()) "osh_history"

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
