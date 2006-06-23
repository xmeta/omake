(*
 * Utilities on targets.
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

open Omake_env
open Omake_node
open Omake_rule
open Omake_builtin_util

module Pos = MakePos (struct let name = "Omake_target" end)
open Pos

(*
 * Target exists or is phony.
 *)
let target_exists_or_is_phony cache target =
   Omake_cache.exists cache target || Node.is_phony target

(*
 * Target is part of an explicit rule.
 *)
let target_is_explicit cache venv target =
   venv_explicit_exists venv target

(*
 * Target exists, is phony, or there is an explicit rule
 * to build it.
 *)
let target_exists_or_is_phony_or_is_explicit cache venv target =
   if debug debug_implicit then
      eprintf "target_exists_or_is_phony_or_is_explicit: %a: %b, %b@." (**)
         pp_print_node target
         (target_exists_or_is_phony cache target)
         (venv_explicit_exists venv target);
   target_exists_or_is_phony cache target || venv_explicit_exists venv target

(*
 * A target is buildable if it exists, or
 * if there is an implicit rule whose dependencies
 * are all buildable.
 *)
let rec target_is_buildable cache venv pos target =
   let target = Node.unsquash target in
      try venv_find_target_is_buildable_exn venv target with
         Not_found ->
            let flag =
               if target_exists_or_is_phony_or_is_explicit cache venv target then
                  true
               else
                  venv_find_buildable_implicit_rule cache venv pos target <> None
            in
               venv_add_target_is_buildable venv target flag;
               flag

and target_is_buildable_proper cache venv pos target =
   let target = Node.unsquash target in
      try venv_find_target_is_buildable_proper_exn venv target with
         Not_found ->
            let flag =
               if target_is_explicit cache venv target then
                  true
               else
                  venv_find_buildable_implicit_rule cache venv pos target <> None
            in
               venv_add_target_is_buildable_proper venv target flag;
               flag

(* Find an applicable implicit rule with buildable sources *)
and venv_find_buildable_implicit_rule cache venv pos target =
   if not (is_build_phase ()) then
      raise (OmakeException (pos, StringError "this command can only be executed in a rule body"));
   let irules = venv_find_implicit_rules venv target in
      if debug debug_implicit then
         eprintf "venv_find_buildable_implicit_rule %a %a: %d commands to consider@." (**)
            pp_print_dir (venv_dir venv)
            pp_print_node target
            (List.length irules);
      search_irules cache venv pos target irules

and search_irules cache venv pos target irules =
   match irules with
      irule :: irules ->
         let sources = irule.rule_sources in
            if debug debug_implicit then
               eprintf "@[<b 3>venv_find_buildable_implicit_rule: considering implicit rule %a:%a@]@." (**)
                  pp_print_node target
                  pp_print_node_set sources;
            if NodeSet.for_all (target_is_buildable cache venv pos) sources then
               let irule' = expand_rule irule in
                  if irule == irule' || NodeSet.for_all (target_is_buildable cache venv pos) (NodeSet.diff irule'.rule_sources sources) then begin
                     if debug debug_implicit then
                        eprintf "@[<b 3>venv_find_buildable_implicit_rule: accepted implicit rule %a:%a@]@." (**)
                           pp_print_node target
                           pp_print_node_set irule'.rule_sources;
                     Some irule'
                  end
                  else
                     search_irules cache venv pos target irules
            else
               search_irules cache venv pos target irules
    | [] ->
         None

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
