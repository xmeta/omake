(*
 * Utilities for execution.
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

open Lm_debug

open Omake_node
open Omake_state
open Omake_cache_type

(*
 * Build debugging.
 *)
let debug_exec =
   create_debug (**)
      { debug_name = "exec";
        debug_description = "Display execution debugging";
        debug_value = false
      }

(*
 * Table based on integers.
 *)
module IntCompare =
struct
   type t = int
   let compare = (-)
end

module IntTable = Lm_map.LmMake (IntCompare)

(*
 * Table based on file descriptor.
 *)
module FdCompare =
struct
   type t = Unix.file_descr
   let compare = Pervasives.compare
end

module FdTable = Lm_map.LmMake (FdCompare);;

(*
 * Create some pipes, and close them if an exception is raised.
 *)
let unix_close fd =
   try Unix.close fd with
      Unix.Unix_error _ ->
         ()

let with_pipe f =
   let read, write = Unix.pipe () in
      try f read write with
         exn ->
            unix_close read;
            unix_close write;
            raise exn

(*
 * Write the data in the buffer to the channel.
 *)
let rec write_all fd id buf off len =
   if len <> 0 then
      let amount =
         try Unix.write fd buf off len with
            Unix.Unix_error _ ->
               0
      in
         if amount <> 0 then
            write_all fd id buf (off + amount) (len - amount)

let copy_stdout = write_all Unix.stdout
let copy_stderr = write_all Unix.stderr

(*
 * Copy output to a file.
 *)
let copy_file name =
   let fd_out = Lm_unix_util.openfile name [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o666 in
   let copy id buf off len =
      if len = 0 then
         Unix.close fd_out
      else
         write_all fd_out id buf off len
   in
      copy

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
