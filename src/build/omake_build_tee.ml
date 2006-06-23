(*
 * Tee operations.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2006 Mojave Group, Caltech
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

open Omake_env
open Omake_node
open Omake_exec_util
open Omake_exec_print
open Omake_build_type
open Omake_options_type

(*
 * The empty tee.
 *)
let tee_none = tee_create false

(*
 * Unlink all the tee files.
 *)
let unlink_file name =
   try Unix.unlink name with
      Unix.Unix_error _ ->
         ()

let unlink_files names =
   List.iter unlink_file names

let env_unlink_tees env =
   unlink_files env.env_success_tees;
   unlink_files env.env_failed_tees;
   env.env_success_tees <- [];
   env.env_failed_tees <- []

(*
 * Print all tees.
 *)
let eprint_file_exn name =
   let buf = String.create 1024 in
   let fd = Unix.openfile name [Unix.O_RDONLY] 0o000 in
   let rec copy () =
      let amount = Unix.read fd buf 0 (String.length buf) in
         if amount > 0 then
            let _ = Unix.write Unix.stderr buf 0 amount in
               copy ()
   in
      copy ();
      Unix.close fd

let eprint_file name =
   try eprint_file_exn name with
      Unix.Unix_error _ ->
         eprintf "*** omake: error reading file %s@." name

(*
 * Close tee channels.
 * For commands that are successful, repeat the diversion.
 *)
let env_close_success_tee env command =
   let { command_venv = venv;
         command_tee  = tee
       } = command
   in
   let options = (venv_options venv).opt_divert in
      match tee_file tee with
         Some name ->
            tee_close tee;
            if not (List.mem DivertDiscardSuccess options)
               && (List.mem DivertRepeat options || List.mem DivertOnly options) then
            begin
               print_flush ();
               eprint_file name
            end;
            unlink_file name;
            command.command_tee <- tee_none
       | None ->
            ()

(*
 * For failed commands, repeat the diversion immediately
 * if the DivertRepeat flag is specified.
 *
 * Don't remove the diversion, we'll print it again
 * at the end of the run.
 *)
let env_close_failed_tee env command =
   let { command_venv = venv;
         command_tee  = tee
       } = command
   in
   let options = (venv_options venv).opt_divert in
      match tee_file tee with
         Some name ->
            tee_close tee;
            if List.mem DivertRepeat options then begin
               print_flush ();
               eprint_file name
            end
       | None ->
            ()

(*
 * Print a diversion.
 *)
let eprint_tee command =
   match tee_file command.command_tee with
      Some name ->
         eprintf "*** omake: failed: %a@." pp_print_node command.command_target;
         eprint_file name
    | None ->
         ()

(*
 * Unlink the file.
 *)
let unlink_tee command =
   match tee_file command.command_tee with
      Some name ->
         unlink_file name;
         command.command_tee <- tee_none
    | None ->
         ()

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)