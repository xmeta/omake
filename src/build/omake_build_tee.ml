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
open Lm_printf

open Omake_env
open Omake_node
open Omake_exec_util
open Omake_exec_print
open Omake_build_type
open Omake_options

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

(*
 * Print all tees.
 *)
let eprint_file_exn copy name =
   let buf = String.create 1024 in
   let fd = Unix.openfile name [Unix.O_RDONLY] 0o000 in
      begin try
         copy buf fd
      with
         Unix.Unix_error _ ->
            Unix.close fd;
            eprintf "*** omake: error reading file %s@." name
       | exn ->
            Unix.close fd;
            raise exn
      end;
      Unix.close fd

let rec copy_stderr buf fd =
   let amount = Unix.read fd buf 0 (String.length buf) in
      if amount > 0 then begin
         let _ = Unix.write Unix.stderr buf 0 amount in
            copy_stderr buf fd
      end

let eprint_file = eprint_file_exn copy_stderr

let rec format_string_with_nl buf s =
   if String.contains s '\n' then begin
      let i = String.index s '\n' in
         pp_print_string buf (String.sub s 0 i);
         pp_force_newline buf ();
         format_string_with_nl buf (String.sub s (i + 1) (String.length s - i - 1))
   end else
      pp_print_string buf s

let rec copy_with_nl_exn out buf fd =
   let amount = Unix.read fd buf 0 (String.length buf) in
      if amount > 0 then begin
         format_string_with_nl out (String.sub buf 0 amount);
         copy_with_nl_exn out buf fd
      end

let format_file_with_nl buf name =
   eprint_file_exn (copy_with_nl_exn buf) name

(*
 * Close tee channels.
 * For commands that are successful, repeat the diversion.
 *)
let env_close_success_tee env command =
   let { command_venv = venv;
         command_tee  = tee
       } = command
   in
      match tee_file tee with
         Some name ->
            tee_close tee;
            if opt_output (venv_options venv) OutputPostponeSuccess then begin
               pp_print_flush stderr ();
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
   let options = venv_options venv in
      match tee_file tee with
         Some name ->
            tee_close tee;
            if opt_output options OutputPostponeError then begin
               pp_print_flush stderr ();
               eprint_file name;
               if not (opt_output options OutputRepeatErrors) then begin
                  unlink_file name;
                  command.command_tee <- tee_none;
               end;
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

let format_tee_with_nl buf command =
   match tee_file command.command_tee with
      Some name ->
         fprintf buf "@ ";
         format_file_with_nl buf name
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
