(*
 * Compute the free variables of an expression.
 * NOTE: this is a little sloppy.
 *    1. The language is dynamically scoped;
 *       we don't catch variables not mentioned statically
 *    2. We take the presence of a definition anywhere
 *       as an indication that the variable is not free.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005 Mojave Group, Caltech
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
open Lm_symbol

open Omake_ir

(*
 * Tables of free variables.
 *)
type free_vars = scope_kind list SymbolTable.t

let free_vars_empty = SymbolTable.empty

(*
 * Remove a binding variable only if it was not free before.
 *)
let rec remove_kind kinds kind =
   match kinds with
      kind' :: kinds ->
         if kind' = kind then
            kinds
         else
            kind' :: remove_kind kinds kind
    | [] ->
         []

let remove_kind kinds kind =
   match remove_kind kinds kind with
      [] ->
         None
    | kinds ->
         Some kinds

let free_vars_remove fv v kind =
   try SymbolTable.filter_remove fv v (fun kinds -> remove_kind kinds kind) with
      Not_found ->
         fv

let free_vars_remove_list fv vars kind =
   List.fold_left (fun fv v -> free_vars_remove fv v kind) fv vars

(*
 * Add a binding variable.
 *)
let free_vars_add fv v kind =
   if Lm_symbol.is_numeric_symbol v then
      fv
   else
      SymbolTable.filter_add fv v (fun kinds ->
            match kinds with
               Some kinds ->
                  if List.mem kind kinds then
                     kinds
                  else
                     kind :: kinds
             | None ->
                  [kind])

(*
 * Union of two free variable sets.
 *)
let free_vars_union fv1 fv2 =
   SymbolTable.fold (fun fv1 v kinds2 ->
         SymbolTable.filter_add fv1 v (fun kinds1 ->
               match kinds1 with
                  Some kinds1 ->
                     Lm_list_util.unionq kinds1 kinds2
                | None ->
                     kinds2)) fv1 fv2

(*
 * Calculate free vars.
 * NOTE: this only calculates the static free variables.
 * Since the language is dynamically scoped, this will miss
 * the dynamic free variables.
 *)
let rec free_vars_string_exp fv s =
   match s with
      NoneString _
    | ConstString _
    | ThisString _
    | KeyString _ ->
         fv
    | ApplyString (_, _, kind, v, sl)
    | SuperApplyString (_, _, kind, v, _, sl) ->
         let fv = free_vars_string_exp_list fv sl in
            free_vars_add fv v kind
    | MethodApplyString (_, _, kind, vars, sl) ->
         let fv = free_vars_string_exp_list fv sl in
            free_vars_add fv (List.hd vars) kind
    | SequenceString (_, sl)
    | ArrayString (_, sl)
    | QuoteString (_, sl)
    | QuoteStringString (_, _, sl) ->
         free_vars_string_exp_list fv sl
    | ArrayOfString (_, s) ->
         free_vars_string_exp fv s
    | BodyString (_, e)
    | ExpString (_, e) ->
         free_vars_exp fv e
    | CasesString (loc, cases) ->
         free_vars_cases fv cases

and free_vars_string_exp_list fv sl =
   match sl with
      s :: sl ->
         free_vars_string_exp_list (free_vars_string_exp fv s) sl
    | [] ->
         fv

and free_vars_cases fv cases =
   match cases with
      (_, s, e) :: cases ->
         free_vars_cases (free_vars_string_exp (free_vars_exp fv e) s) cases
    | [] ->
         fv

and free_vars_exp_list fv el =
   match el with
      e :: el ->
         free_vars_exp (free_vars_exp_list fv el) e
    | [] ->
         fv

and free_vars_exp fv e =
   match e with
      LetVarExp (_, kind, v, _, s) ->
         let fv = free_vars_remove fv v kind in
            free_vars_string_exp fv s
    | LetFunExp (_, kind, v, vars, e) ->
         let fv_body = free_vars_exp free_vars_empty e in
         let fv_body = free_vars_remove_list fv_body vars ScopeProtected in
         let fv = free_vars_union fv fv_body in
            free_vars_remove fv v kind
    | LetObjectExp (_, kind, v, el) ->
         let fv = free_vars_exp_list fv el in
            free_vars_remove fv v kind
    | IfExp (_, cases) ->
         free_vars_if_cases fv cases
    | SequenceExp (_, el)
    | StaticExp (_, _, _, el) ->
         free_vars_exp_list fv el
    | SectionExp (_, s, el) ->
         free_vars_string_exp (free_vars_exp_list fv el) s
    | IncludeExp (_, s, sl) ->
         free_vars_string_exp (free_vars_string_exp_list fv sl) s
    | ApplyExp (_, kind, v, sl)
    | SuperApplyExp (_, kind, v, _, sl) ->
         free_vars_string_exp_list (free_vars_add fv v kind) sl
    | MethodApplyExp (_, kind, vars, sl) ->
         free_vars_string_exp_list (free_vars_add fv (List.hd vars) kind) sl
    | ReturnCatchExp (_, e) ->
         free_vars_exp fv e
    | LetKeyExp (_, _, _, s)
    | LetThisExp (_, s)
    | ShellExp (_, s)
    | StringExp (_, s)
    | ReturnExp (_, s)
    | ExportExp (_, s) ->
         free_vars_string_exp fv s
    | KeyExp _
    | ReturnObjectExp _
    | ReturnSaveExp _
    | OpenExp _
    | CancelExportExp _ ->
         fv

and free_vars_if_cases fv cases =
   match cases with
      (s, e) :: cases ->
         free_vars_if_cases (free_vars_string_exp (free_vars_exp fv e) s) cases
    | [] ->
         fv

(*
 * Wrapper.
 *)
let free_vars_exp e =
   free_vars_exp free_vars_empty e

let free_vars_exp_list el =
   free_vars_exp_list free_vars_empty el

let free_vars_fold f x fv =
   SymbolTable.fold (fun x v kinds ->
         List.fold_left (fun x kind ->
               f x v kind) x kinds) x fv

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
