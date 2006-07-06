(*
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Mojave Group, Caltech
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

open Omake_node
open Omake_state
open Omake_command
open Omake_exec_type
open Omake_exec_util
open Omake_options_type
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
let message_count = ref 0

let print_progress options count total =
   if options.opt_print_progress then
      let blobs = count * 60 / total in
      let () = print_char '[' in
      let off =
         match !message with
            Some s ->
               (* The message has a finite lifetime *)
               if !message_count = 0 then
                  message := None
               else
                  decr message_count;

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
         for i = 0 to 60 - max off blobs do
            print_char ' '
         done;
         printf "] %05d / %05d\r@?" count total;
         print_flush_flag := true

(*
 * Flush the print line if needed.
 *)
let flush_buf = String.make 80 ' '

let print_flush () =
   if !print_flush_flag then  begin
      printf "%s\r@?" flush_buf;
      print_flush_flag := false
   end

(*
 * Print a short message.
 *)
let print_message options s =
   if options.opt_print_progress then begin
      message := Some s;
      message_count := 2
   end
   else
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
   if options.opt_print_dir then
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
   if options.opt_print_dir then
      match !current_dir with
         Some cwd ->
            printf "make[1]: Leaving directory `%s'@." (Dir.absname cwd);
            current_dir := None
       | None ->
            ()

(*
 * Print a status line.
 *)
let should_print options flags flag =
   match flag, options.opt_print_command with
       PrintEager _, EvalEager ->
          true
     | PrintLazy _, EvalLazy ->
          not (List.mem AllowOutputFlag flags)
     | PrintExit _, _ ->
          options.opt_print_exit
     | _ ->
          false

let print_status_stdout options shell remote name flag =
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
               if should_print options flags flag then
                  let dirname = Dir.fullname dir in
                     print_flush ();
                     print_entering_current_directory options dir;
                     if options.opt_print_file then
                        printf "-%t %s %s %s@." pp_print_host name dirname (Node.name dir target);
                     if not (List.mem QuietFlag flags) then
                        printf "+%t %a@." pp_print_host shell.shell_print_exp exp
       | PrintExit (exp, code, _) ->
            let flags, dir, target = shell.shell_info exp in
            let dirname = Dir.fullname dir in
               if should_print options flags flag && options.opt_print_file then begin
                  print_flush ();
                  printf "-%t exit %s %s, code %d@." pp_print_host dirname (Node.name dir target) code
               end

let print_status_tee tee shell remote name exp =
   let pp_print_host buf =
      match remote with
         Some host ->
            fprintf buf "[%s]" host
       | None ->
            ()
   in
   let tee = formatter_of_out_channel tee in
   let flags, dir, target = shell.shell_info exp in
   let dirname = Dir.fullname dir in
      fprintf tee "-%t %s %s %s@." pp_print_host name dirname (Node.name dir target);
      if not (List.mem QuietFlag flags) then
         fprintf tee "+%t %a@." pp_print_host shell.shell_print_exp exp

let print_status tee options shell remote name flag =
   let () =
      match flag with
         PrintLazy exp ->
            (match tee_channel tee with
                Some tee ->
                   print_status_tee tee shell remote name exp
              | None ->
                   ())
       | PrintEager _
       | PrintExit _ ->
            ()
   in
      if not (List.mem DivertOnly options.opt_divert) then
         print_status_stdout options shell remote name flag

(*
 * Print a list of lines.
 *)
let print_status_lines options shell name el =
   (* Print the header *)
   print_flush ();

   (* Print the commands *)
   List.iter (fun exp ->
         let flags, dir, target = shell.shell_info exp in
            if options.opt_print_file then
               printf "- %s %s %s@." name (Dir.fullname dir) (Node.name dir target);
            printf "+ %a@." shell.shell_print_exp exp) el

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
