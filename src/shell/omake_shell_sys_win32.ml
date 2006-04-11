(*
 * System calls.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
open Lm_debug

open Omake_shell_type
open Omake_shell_sys_type

(*
 * These functions are directly exported.
 *)
external set_tty_pgrp   : pgrp -> unit                    = "omake_shell_sys_set_tty_pgrp"
external create_process : create_process -> pid           = "omake_shell_sys_create_process"

(*
 * Internal.
 *)
external create_thread_pid  : pgrp -> pid                 = "omake_shell_sys_create_thread_pid"
external release_thread_pid : pid -> int -> unit          = "omake_shell_sys_release_thread_pid"
external init_thread_pid    : pid -> unit                 = "omake_shell_sys_init_thread_pid"
external check_thread       : unit -> bool                = "omake_shell_sys_check_thread"
external suspend            : pgrp -> unit                = "omake_shell_sys_suspend"
external resume             : pgrp -> unit                = "omake_shell_sys_resume"
external kill               : pgrp -> unit                = "omake_shell_sys_kill"
external ext_wait           : pgrp -> bool -> bool -> pid * Unix.process_status = "omake_shell_sys_wait"

external init_shell         : unit -> unit                = "omake_shell_sys_init"
external close              : unit -> unit                = "omake_shell_sys_close"

let () =
   init_shell ()

let set_interactive _ = ()
let set_close_on_exec = Unix.set_close_on_exec
let clear_close_on_exec = Unix.clear_close_on_exec
let close_fd = Unix.close

(*
 * Termination signal.
 *)
exception Terminated

(*
 * The operation depends on the signal number.
 *)
let kill pgrp signo =
   match signo with
      SigStop
    | SigTstp ->
         suspend pgrp
    | SigCont ->
         resume pgrp
    | _ ->
         kill pgrp

(*
 * Wait is blocking.
 *)
let wait pgrp leader nohang =
   Lm_thread_pool.blocking_section (ext_wait pgrp leader) nohang

(*
 * Try to close a descriptor.
 * This is kind of bad, because some other thread
 * may have allocated that descriptor by the time we get
 * to it, but this should never happen because the thread
 * should be catching all its exceptions.
 *)
let try_close fd =
   try close_fd fd with
      Unix.Unix_error _ ->
         ()

(*
 * Create a thread.  This is a real thread, but it
 * should look as much like a process as possible.
 * For this reason, we dup the stdio handles.
 *)
let create_thread info =
   let { create_thread_stdin = stdin;
         create_thread_stdout = stdout;
         create_thread_stderr = stderr;
         create_thread_pgrp = pgrp;
         create_thread_fun = f
       } = info
   in
   let pid    = create_thread_pid pgrp in
   let stdin  = Unix.dup stdin  in
   let stdout = Unix.dup stdout in
   let stderr = Unix.dup stderr in
   let _ =
      Lm_thread_pool.create false (fun () ->
            init_thread_pid pid;
            let code =
               try f stdin stdout stderr pid with
                  exn ->
                     eprintf "@[<v 3>%a@ Thread failed with an exception, cleaning up@]@." Omake_exn_print.pp_print_exn exn;
                     try_close stdin;
                     try_close stdout;
                     try_close stderr;
                     1
            in
               release_thread_pid pid code)
   in
      pid

(*
 * Create a new process group.
 *)
let create_process_group = create_thread

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
