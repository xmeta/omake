(*
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
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified By: Aleksey Nogin @email{nogin@metaprl.org}
 * @end[license]
 *)
open Lm_printf

open Omake_node
open Omake_state
open Omake_command
open Omake_exec_type
open Omake_exec_util
open Omake_options
open Omake_command_type

(*
 * When the progress bar is printed, it leaves the line with text on it.
 * Remember if this text should be deleted.
 *)
let print_flush_flag = ref false

(*
 * Print the progress bar.
 *)
let message = ref None
let message_timeout = ref 0.0
let progress_width = Lm_termsize.stdout_width - 20

let print_progress options count total =
   if opt_print_progress options then
      let blobs = count * progress_width / total in
      let () = print_char '[' in
      let off =
         match !message with
            Some s ->
               (* The message has a finite lifetime *)
               if Unix.gettimeofday () >= !message_timeout then
                  message := None;

               (* Print the message first *)
               print_string s;
               print_char ' ';
               String.length s + 1

          | None ->
               0
      in
         for i = off to blobs do
            print_char '='
         done;
         for i = 0 to progress_width - max off blobs do
            print_char ' '
         done;
         printf "] %05d / %05d\r@?" count total;
         print_flush_flag := true

(*
 * Flush the print line if needed.
 *)
let flush_buf = String.make Lm_termsize.stdout_width ' '

let print_flush () =
   if !print_flush_flag then  begin
      printf "%s\r@?" flush_buf;
      print_flush_flag := false
   end

(*
 * Print a short message.
 * XXX: Should the message_timeout delay be an option?
 *)
let print_message options s =
   if opt_print_progress options then begin
      message := Some s;
      message_timeout := Unix.gettimeofday () +. 3.0
   end else
      printf "*** omake: %s@." s

(*
 * Print a message saying that the cache is being saved.
 *)
let saving_message = "saved " ^ db_name

let print_saving options =
   print_message options saving_message

(*
 * Print the current directory.
 * Keep track of the directory, to minimize spamming
 * of the omake output.
 *)
let current_dir = ref None

let print_entering_current_directory options dir =
   if opt_print_dir options then
      match !current_dir with
         Some cwd ->
            if not (Dir.equal dir cwd) then begin
                  printf "make[1]: Leaving directory `%s'@." (Dir.absname cwd);
                  current_dir := Some dir;
                  printf "make[1]: Entering directory `%s'@." (Dir.absname dir)
               end
       | None ->
            current_dir := Some dir;
            printf "make[1]: Entering directory `%s'@." (Dir.absname dir)

let print_leaving_current_directory options =
   if opt_print_dir options then
      match !current_dir with
         Some cwd ->
            printf "make[1]: Leaving directory `%s'@." (Dir.absname cwd);
            current_dir := None
       | None ->
            ()

(*
 * Print a status line.
 *)
let should_print options flag =
   match flag, opt_print_command options with
       PrintEager _, EvalEager
     | PrintLazy _, EvalLazy ->
          true
     | PrintExit _, _ ->
          opt_print_exit options
     | _ ->
          false

let print_status handle_out options shell remote name flag =
   let print_flush () = 
      handle_out "" 0 0;
      print_flush ()
   in
   let out = make_formatter handle_out print_flush in
   let pp_print_host buf =
      match remote with
         Some host ->
            fprintf buf "[%s]" host
       | None ->
            ()
   in
      match flag with
         PrintEager exp
       | PrintLazy exp ->
            let flags, dir, target = shell.shell_info exp in
               if should_print options flag then
                  let dirname = Dir.fullname dir in
                     print_flush ();
                     print_entering_current_directory options dir;
                     if opt_print_file options then
                        fprintf out "-%t %s %s %s@." pp_print_host name dirname (Node.name dir target);
                     if not (List.mem QuietFlag flags) then
                        fprintf out "+%t %a@." pp_print_host shell.shell_print_exp exp
       | PrintExit (exp, code, _) ->
            let flags, dir, target = shell.shell_info exp in
            let dirname = Dir.fullname dir in
               if should_print options flag && opt_print_file options then begin
                  print_flush ();
                  fprintf out "-%t exit %s %s, code %d@." pp_print_host dirname (Node.name dir target) code
               end

(*
 * Print a list of lines.
 *)
let print_status_lines options shell name el =
   (* Print the header *)
   print_flush ();

   (* Print the commands *)
   List.iter (fun exp ->
         let flags, dir, target = shell.shell_info exp in
            if opt_print_file options then
               printf "- %s %s %s@." name (Dir.fullname dir) (Node.name dir target);
            printf "+ %a@." shell.shell_print_exp exp) el

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
