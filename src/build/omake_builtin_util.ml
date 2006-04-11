1(*
 * Some builtin functions.
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

open Lm_symbol
open Lm_location
open Lm_string_set

open Omake_ir
open Omake_env
open Omake_eval
open Omake_wild
open Omake_node
open Omake_value
open Omake_state
open Omake_target
open Omake_node_sig
open Omake_build_type

module Pos = MakePos (struct let name = "Omake_builtin" end)
open Pos

(*
 * Command-line definitions.
 *)
let command_defs = ref []

let add_command_def v s =
   command_defs := (v, s) :: !command_defs

let command_defs_are_nonempty () =
   !command_defs <> []

let venv_add_command_defs venv =
   venv_add_assoc venv !command_defs

(*
 * Fold its in a sequence, and place separators between them.
 *)
let sequence_map f sl =
   let white = ValString " " in
   let rec collect seq sl =
      match sl with
         s :: sl ->
            let s = f s in
            let seq =
               if seq = [] then
                  [s]
            else
                  s :: white :: seq
            in
               collect seq sl
       | [] ->
            List.rev seq
   in
      collect [] sl

(*
 * Add separators to a list.
 *)
let sequence_list sl =
   let white = ValString " " in
   let rec collect sl =
      match sl with
         [s] ->
            [s]
       | s :: sl ->
            s :: white :: collect sl
       | [] ->
            []
   in
      collect sl

(*
 * Default Boolean values.
 *)
let val_true  = ValData "true"
let val_false = ValData "false"

(*
 * Maintaining the environment.
 *)
let saved_env = ref None

let set_env env =
   saved_env := Some env

let get_env pos loc =
   match !saved_env with
      Some env ->
         env
    | None ->
         raise (OmakeException (loc_pos loc pos, StringError "this function can be called only in rule bodies"))

(*
 * Check whether a node is a leaf node.
 *)
let is_leaf_command command =
   let { command_scanner_deps = scanner_deps;
         command_static_deps  = static_deps;
         command_build_deps   = build_deps;
         command_lines        = lines
       } = command
   in
      NodeSet.is_empty scanner_deps
      && NodeSet.is_empty static_deps
      && NodeSet.is_empty build_deps
      && (lines = CommandNone)

let is_leaf_node env node =
   try is_leaf_command (NodeTable.find env.env_commands node) with
      Not_found ->
         false

(*
 * Extend an object with another.
 * The argument may be a file or an object.
 *)
let object_of_file venv pos loc s =
   let pos  = string_pos "extends" pos in
   let node = find_include_file venv pos loc s in
      try venv_find_object_file_exn venv node with
         Not_found ->
            let obj = eval_object_file venv pos loc node in
               venv_add_object_file venv node obj;
               obj

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
