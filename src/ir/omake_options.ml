(*
 * Options for the omake program.
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
 * Authors:
 *    Jason Hickey @email{jyh@cs.caltech.edu}
 *    Aleksey Nogin @email{nogin@metaprl.org}
 * @end[license]
 *)

(*
 * When to print output.
 *)
type eval_flag =
   EvalNever
 | EvalLazy
 | EvalEager

(*
 * Diversion control.
 *)
type divert_flag =
   DivertErrors
 | DivertRepeat
 | DivertOnly
 | DivertDiscardSuccess

(*
 * The basic make flags.
 *)
type omake_options =
   { opt_job_count            : int;
     opt_remote_servers       : (string * int) list;
     opt_terminate_on_error   : bool;
     opt_dry_run              : bool;
     opt_print_command        : eval_flag;
     opt_print_dir            : bool;
     opt_print_file           : bool;
     opt_print_status         : bool;
     opt_print_exit           : bool;
     opt_print_progress       : bool;
     opt_touch_only           : bool;
     opt_flush_cache          : bool;
     opt_flush_dependencies   : bool;
     opt_print_dependencies   : bool;
     opt_show_dependencies    : string list;
     opt_all_dependencies     : bool;
     opt_verbose_dependencies : bool;
     opt_cd_root              : bool;
     opt_project              : bool;
     opt_poll                 : bool;
     opt_poll_on_done         : bool;
     opt_flush_include        : bool;
     opt_flush_static         : bool;
     opt_allow_exceptions     : bool;
     opt_absname              : bool;
     opt_divert               : divert_flag list;
   }

let opt_job_count opts =
   opts.opt_job_count

let opt_remote_servers opts =
   opts.opt_remote_servers

(*
 * Predicate returns true iff there are parallel jobs.
 *)
let opt_parallel options =
   (opt_job_count options) > 1 || (opt_remote_servers options) <> []

let set_job_count_and_servers_opt opts cnt srvs =
   { opts with opt_job_count = cnt; opt_remote_servers = srvs }

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
      set_job_count_and_servers_opt options job_count (List.rev remote_servers)

let opt_terminate_on_error opts =
   opts.opt_terminate_on_error

let set_terminate_on_error_opt opts flag =
   { opts with opt_terminate_on_error = flag }

let opt_dry_run opts =
   opts.opt_dry_run

let set_dry_run_opt opts flag =
   { opts with opt_dry_run = flag }

let opt_print_command opts =
   opts.opt_print_command

let set_print_command_opt opts flag =
   { opts with opt_print_command = flag }

let opt_print_dir opts =
   opts.opt_print_dir

let set_print_dir_opt opts flag =
   { opts with opt_print_dir = flag }

let opt_print_file opts =
   opts.opt_print_file

let set_print_file_opt opts flag =
   { opts with opt_print_file = flag }

let opt_print_status opts =
   opts.opt_print_status

let set_print_status_opt opts flag =
   { opts with opt_print_status = flag }

let opt_print_exit opts =
   opts.opt_print_exit

let set_print_exit_opt opts flag =
   { opts with opt_print_exit = flag }

let opt_print_progress opts =
   opts.opt_print_progress

let set_print_progress_opt opts flag =
   { opts with opt_print_progress = flag }

let opt_touch_only opts =
   opts.opt_touch_only

let set_touch_only_opt opts flag =
   { opts with opt_touch_only = flag }

let opt_flush_cache opts =
   opts.opt_flush_cache

let set_flush_cache_opt opts flag =
   { opts with opt_flush_cache = flag }

let opt_flush_dependencies opts =
   opts.opt_flush_dependencies

let set_flush_dependencies_opt opts flag =
   { opts with opt_flush_dependencies = flag }

let opt_print_dependencies opts =
   opts.opt_print_dependencies

let set_print_dependencies_opt opts flag =
   { opts with opt_print_dependencies = flag }

let opt_show_dependencies opts =
   opts.opt_show_dependencies

let add_show_dependency_opt opts dep =
   { opts with opt_show_dependencies = dep :: opts.opt_show_dependencies }

let opt_all_dependencies opts =
   opts.opt_all_dependencies

let set_all_dependencies_opt opts flag =
   { opts with opt_all_dependencies = flag }

let opt_verbose_dependencies opts =
   opts.opt_verbose_dependencies

let set_verbose_dependencies_opt opts flag =
   { opts with opt_verbose_dependencies = flag }

let opt_cd_root opts =
   opts.opt_cd_root

let set_cd_root_opt opts flag =
   { opts with opt_cd_root = flag }

let opt_project opts =
   opts.opt_project

let set_project_opt opts flag =
   { opts with opt_project = flag }

let opt_poll opts =
   opts.opt_poll

let set_poll_opt opts b =
   { opts with opt_poll = b; opt_terminate_on_error = not b }

let opt_poll_on_done opts =
   opts.opt_poll_on_done

let set_poll_on_done_opt opts b =
   { opts with opt_poll_on_done = b; opt_poll = b; opt_terminate_on_error = not b }

let opt_flush_include opts =
   opts.opt_flush_include

let set_flush_include_opt opts flag =
   { opts with opt_flush_include = flag }

let opt_flush_static opts =
   opts.opt_flush_static

let set_flush_static_opt opts flag =
   { opts with opt_flush_static = flag }

let opt_allow_exceptions opts =
   opts.opt_allow_exceptions

let set_allow_exceptions_opt opts flag =
   { opts with opt_allow_exceptions = flag }

let opt_absname opts =
   opts.opt_absname

let set_absname_opt opts flag =
   { opts with opt_absname = flag }

(*
 * Output control.
 *)
let output_opt_char options c =
   match c with
      '0' ->
         (* -s --divert-only --divert-discard-success *)
         { options with opt_print_status   = false;
                        opt_print_dir      = false;
                        opt_print_file     = false;
                        opt_print_exit     = false;
                        opt_print_command  = EvalNever;
                        opt_divert         = [DivertOnly; DivertDiscardSuccess]
         }
    | '1' ->
         (* -S --progress --divert-only --divert-repeat --divert-discard-success *)
         { options with opt_print_command = EvalLazy;
                        opt_print_progress = true;
                        opt_divert = [DivertOnly; DivertRepeat; DivertDiscardSuccess]
         }
    | '2' ->
         (* --progress --divert-only --divert-repeat *)
         { options with opt_print_progress = true;
                        opt_divert = [DivertOnly; DivertRepeat]
         }
    | 'W' ->
         set_print_dir_opt options true
    | 'w' ->
         set_print_dir_opt options false
    | 'P' ->
         set_print_progress_opt options true
    | 'p' ->
         set_print_progress_opt options false
    | 'X' ->
         set_print_exit_opt options true
    | 'x' ->
         set_print_exit_opt options false
    | 'S' ->
         set_print_status_opt options true
    | 's' ->
         set_print_status_opt options false
    | _ ->
         (* Ignore, for forward compatibility *)
         options

let set_output_opts options s =
   let len = String.length s in
   let rec loop options i =
      if i = len then
         options
      else
         loop (output_opt_char options s.[i]) (succ i)
   in
      loop options 0

let opt_diverts opts =
   opts.opt_divert <> []

let opt_divert opts flag =
   List.mem flag opts.opt_divert

let set_divert_opt flag opts on =
   let flags = Lm_list_util.tryremove flag opts.opt_divert in
   let flags = if on then flag :: flags else flags in
      { opts with opt_divert = flags }

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
     opt_absname              = false;
     opt_divert               = [];
   }

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
    "-k", Lm_arg.ClearFold set_terminate_on_error_opt, (**)
       "Do not stop when an error occurs";
    "-p", Lm_arg.SetFold set_poll_opt, (**)
       "Poll filesystem for changes (until build succeeds); implies -k";
    "-P", Lm_arg.SetFold set_poll_on_done_opt, (**)
       "Poll filesystem for changes (keep polling \"forever\"); implies -k";
    "-n", Lm_arg.SetFold set_dry_run_opt, (**)
       "Print commands, but do not execute them";
    "--project", Lm_arg.SetFold set_project_opt, (**)
       "Ignore the current directory and build the project";
    "-t", Lm_arg.SetFold set_touch_only_opt, (**)
       "Update database to force files to be up-to-date";
    "--depend", Lm_arg.SetFold set_flush_dependencies_opt, (**)
       "Do not trust cached dependecy information";
    "-U", Lm_arg.SetFold set_flush_cache_opt, (**)
       "Do not trust the dependency cache or cached OMakefiles";
    "--flush-includes", Lm_arg.SetFold set_flush_include_opt, (**)
       "Do not trust cached .omc files";
    "--configure", Lm_arg.SetFold set_flush_static_opt, (**)
       "Recompute static. sections";
    "-R", Lm_arg.SetFold set_cd_root_opt, (**)
       "Command-line targets are relative to the root";
    "--print-dependencies", Lm_arg.SetFold set_print_dependencies_opt, (**)
       "Build and print dependencies";
    "--show-dependencies", Lm_arg.StringFold add_show_dependency_opt, (**)
       "Show dependencies if the file is built";
    "--all-dependencies", Lm_arg.SetFold set_all_dependencies_opt, (**)
       "For --print-dependencies and --show-dependencies, print dependencies recursively";
    "--verbose-dependencies", Lm_arg.SetFold set_verbose_dependencies_opt, (**)
       "For --print-dependencies and --show-dependencies, print all dependencies too";
    "--absname", Lm_arg.SetFold set_absname_opt, (**)
       "Filenames are always displayed as absolute paths"]

(*
 * Output control.
 *)
let output_spec =
   ["-s", Lm_arg.ClearFold (fun options b ->
          { options with opt_print_status  = b;
                         opt_print_dir     = b;
                         opt_print_file    = b;
                         opt_print_exit    = b;
                         opt_print_command = if b then EvalEager else EvalNever }), (**)
       "Do not print commands as they are executed";
    "-S", Lm_arg.SetFold (fun options b -> { options with opt_print_command = if b then EvalLazy else EvalEager }), (**)
       "Print command only if the command prints output";
    "--progress", Lm_arg.SetFold set_print_progress_opt, (**)
       "Print a progress indicator";
    "--print-status", Lm_arg.SetFold set_print_status_opt, (**)
       "Print status lines";
    "-w", Lm_arg.SetFold set_print_dir_opt, (**)
       "Print the directory in \"make format\" as commands are executed";
    "--print-exit", Lm_arg.SetFold set_print_exit_opt, (**)
       "Print the exit codes of commands";
    "--divert-errors", Lm_arg.SetFold (set_divert_opt DivertErrors), (**)
       "The final summary will re-print errors";
    "--divert-repeat", Lm_arg.SetFold (set_divert_opt DivertRepeat), (**)
       "Re-print command output when a rule terminates";
    "--divert-only", Lm_arg.SetFold (set_divert_opt DivertOnly), (**)
       "Print only diversions -- be silent otherwise";
    "--divert-discard-success", Lm_arg.SetFold (set_divert_opt DivertDiscardSuccess), (**)
       "Do not print diverted output from commands that are successful";
    "-o", Lm_arg.StringFold set_output_opts, (**)
       "Short diversion options [01jwWpPxXsS] (see the manual)"]

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
