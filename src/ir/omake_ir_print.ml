(*
 * Print IR expressions.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2006 MetaPRL Group, Caltech
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
 * Modified By: Aleksey Nogin @email{nogin@cs.caltech.edu}
 * @end[license]
 *)
open Lm_printf
open Lm_symbol
open Lm_location

open Omake_ir
open Omake_node
open Omake_ir_util
open Omake_node_sig
open Omake_print_util

let print_location = Omake_ast_print.print_location

let string_override s pp_fun complete buf arg =
   if complete then
      pp_fun true buf arg
   else
      pp_print_string buf s

(*
 * Match kind.
 *)
let pp_print_match_kind out kind =
   let s =
      match kind with
         MatchWild -> "switch"
       | MatchRegex -> "match"
   in
      pp_print_string out s

(*
 * Print the evaluation strategy.
 *)
let pp_print_strategy buf s =
   match s with
      LazyApply -> pp_print_char buf '\''
    | EagerApply -> pp_print_char buf ','
    | NormalApply -> ()

(*
 * Arities.
 *)
let pp_print_arity buf arity =
   match arity with
      ArityRange (lower, upper) ->
         fprintf buf "%d..%d" lower upper
    | ArityExact i ->
         pp_print_int buf i
    | ArityAny ->
         pp_print_string buf "varargs"
    | ArityNone ->
         pp_print_int buf 0

(*
 * Print a list of symbols.
 *)
let rec pp_print_symbol_list buf sl =
   match sl with
      [s] ->
         pp_print_symbol buf s
    | [] ->
         ()
    | s :: sl ->
         fprintf buf "%a, %a" pp_print_symbol s pp_print_symbol_list sl

(*
 * Print a variable definition kind.
 *)
let pp_print_var_def_kind buf kind =
   let s =
      match kind with
         VarDefNormal ->
            "="
       | VarDefAppend ->
            "+="
   in
      pp_print_string buf s

(*
 * Scope.
 *)
let pp_print_scope_kind complete buf kind =
   if complete then
      pp_print_string buf begin
         match kind with
            ScopePrivate ->   "private."
          | ScopeDynamic ->   "public."
          | ScopeProtected -> "this."
          | ScopeGlobal -> "global."
      end

(*
 * Print a string expression.
 *)
let rec pp_print_string_exp complete buf s =
   match s with
      NoneString _ ->
         fprintf buf "<none>"
    | ConstString (_, s) ->
         fprintf buf "\"%s\"" (String.escaped s)
    | KeyString (_, strategy, s) ->
         fprintf buf "$%a|%s|" pp_print_strategy strategy s
    | ApplyString (_, strategy, scope, v, []) ->
         fprintf buf "@[<hv 3>$%a(%a%a)@]" (**)
            pp_print_strategy strategy
            (pp_print_scope_kind complete) scope
            pp_print_symbol v
    | ApplyString (_, strategy, scope, v, args) ->
         fprintf buf "@[<hv 3>$%a(%a%a %a)@]" (**)
            pp_print_strategy strategy
            (pp_print_scope_kind complete) scope
            pp_print_symbol v
            (pp_print_string_exp_list complete) args
    | SuperApplyString (_, strategy, scope, super, v, []) ->
         fprintf buf "@[<hv 3>$%a(%a%a::%a)@]" (**)
            pp_print_strategy strategy
            (pp_print_scope_kind complete) scope
            pp_print_symbol super
            pp_print_symbol v
    | SuperApplyString (_, strategy, scope, super, v, args) ->
         fprintf buf "@[<hv 3>$%a(%a%a::%a %a)@]" (**)
            pp_print_strategy strategy
            (pp_print_scope_kind complete) scope
            pp_print_symbol super
            pp_print_symbol v
            (pp_print_string_exp_list complete) args
    | MethodApplyString (_, strategy, scope, vl, []) ->
         fprintf buf "@[<hv 3>$%a(%a%a)@]" (**)
            pp_print_strategy strategy
            (pp_print_scope_kind complete) scope
            pp_print_method_name vl
    | MethodApplyString (_, strategy, scope, vl, args) ->
         fprintf buf "@[<hv 3>$%a(%a%a %a)@]" (**)
            pp_print_strategy strategy
            (pp_print_scope_kind complete) scope
            pp_print_method_name vl
            (pp_print_string_exp_list complete) args
    | SequenceString (_, sl) ->
         fprintf buf "@[<hv 1>(%a)@]" (**)
            (pp_print_string_exp_list complete) sl
    | ArrayOfString (_, s) ->
         fprintf buf "@[<hv 1>(array-of-string@ %a)@]" (**)
            (pp_print_string_exp complete) s
    | ArrayString (_, sl) ->
         fprintf buf "@[<hv 1>[|%a|]@]" (**)
            (pp_print_string_exp_list complete) sl
    | QuoteString (_, sl) ->
         fprintf buf "@[<hv 1>(quote %a)@]" (**)
            (pp_print_string_exp_list complete) sl
    | QuoteStringString (_, c, sl) ->
         fprintf buf "@[<hv 1>(quote %c%a%c)@]" (**)
            c (pp_print_string_exp_list complete) sl c
    | BodyString (_, e) ->
         if complete then
            fprintf buf "@[<hv 3>body@ %a@]" (pp_print_exp complete) e
         else
            pp_print_string buf "<body...>"
    | ExpString (_, e) ->
         if complete then
            fprintf buf "@[<hv 3>exp@ %a@]" (pp_print_exp complete) e
         else
            pp_print_string buf "<exp...>"
    | CasesString (_, cases) ->
         if complete then begin
            fprintf buf "@[<hv 3>cases:";
            List.iter (fun (v, e1, e2) ->
                  fprintf buf "@ @[<hv 3>%a %a:@ %a@]" (**)
                     pp_print_symbol v
                     (pp_print_string_exp complete) e1
                     (pp_print_exp complete) e2) cases;
            fprintf buf "@]"
         end else
            pp_print_string buf "<cases...>"
    | ThisString (loc, ScopeProtected) ->
         pp_print_string buf "$<this>"
    | ThisString (loc, ScopePrivate) ->
         pp_print_string buf "$<private>"
    | ThisString (loc, ScopeDynamic) ->
         pp_print_string buf "$<public>"
    | ThisString (loc, ScopeGlobal) ->
         pp_print_string buf "$<global>"

and pp_print_string_exp_list complete buf sl =
   match sl with
      [s] ->
         pp_print_string_exp complete buf s
    | [] ->
         ()
    | s :: sl ->
         fprintf buf "%a,@ %a" (pp_print_string_exp complete) s (pp_print_string_exp_list complete) sl

(*
 * Print an expression.
 *)
and pp_print_exp complete buf e =
   if complete && !print_location then
      fprintf buf "<%a>" pp_print_location (loc_of_exp e);
   match e with
      LetVarExp (_, scope, v, kind, s) ->
         fprintf buf "@[<hv 3>%a%a %a@ %a@]" (**)
            (pp_print_scope_kind complete) scope
            pp_print_symbol v
            pp_print_var_def_kind kind
            (pp_print_string_exp complete) s
    | LetFunExp (_, scope, v, params, e) ->
         fprintf buf "@[<hv 3>%a%a(%a) =@ %a@]" (**)
            (pp_print_scope_kind complete) scope
            pp_print_symbol v
            pp_print_symbol_list params
            (string_override "<...>" pp_print_exp complete) e
    | LetObjectExp (_, scope, v, el) ->
         fprintf buf "@[<v 3>%a%a. =@ %a@]" (**)
            (pp_print_scope_kind complete) scope
            pp_print_symbol v
            (string_override "<...>" pp_print_exp_list complete) el
    | LetThisExp (_, e) ->
         fprintf buf "@[<hv 3><this> =@ %a@]" (pp_print_string_exp complete) e
    | ShellExp (_, e) ->
         fprintf buf "@[<hv 3>shell(%a)@]" (pp_print_string_exp complete) e
    | IfExp (_, cases) ->
         if complete then begin
            fprintf buf "@[<hv 0>if";
            List.iter (fun (s, e) ->
                  fprintf buf "@ @[<hv 3>| %a ->@ %a@]" (**)
                     (pp_print_string_exp complete) s
                     (pp_print_exp complete) e) cases;
            fprintf buf "@]"
         end else
            pp_print_string buf "<if ... then ... [else ...]>"
    | SequenceExp (_, el) ->
         fprintf buf "@[<hv 3>sequence@ %a@]" (**)
            (pp_print_exp_list complete) el
    | SectionExp (_, s, el) ->
         fprintf buf "@[<hv 3>section %a@ %a@]" (**)
            (pp_print_string_exp complete) s
            (string_override "<...>" pp_print_exp_list complete) el
    | OpenExp (_, nodes) ->
         fprintf buf "@[<hv 3>open";
         List.iter (fun node -> fprintf buf "@ %a" pp_print_node node) nodes;
         fprintf buf "@]"
    | IncludeExp (_, s, commands) ->
         fprintf buf "@[<hv 3>include %a:%a@]" (**)
            (pp_print_string_exp complete) s
            (pp_print_commands complete) commands
    | ApplyExp (_, scope, v, args) ->
         fprintf buf "@[<hv 3>%a%a(%a)@]" (**)
            (pp_print_scope_kind complete) scope
            pp_print_symbol v
            (pp_print_string_exp_list complete) args
    | SuperApplyExp (_, scope, super, v, args) ->
         fprintf buf "@[<hv 0>%a%a::%a(%a)@]" (**)
            (pp_print_scope_kind complete) scope
            pp_print_symbol super
            pp_print_symbol v
            (pp_print_string_exp_list complete) args
    | MethodApplyExp (_, scope, vl, args) ->
         fprintf buf "@[<hv 3>%a%a(%a)@]" (**)
            (pp_print_scope_kind complete) scope
            pp_print_method_name vl
            (pp_print_string_exp_list complete) args
    | ReturnCatchExp (_, e) ->
         fprintf buf "@[<hv 3>return-catch@ %a@]" (pp_print_exp complete) e
    | StringExp (_, s) ->
         fprintf buf "string(%a)" (pp_print_string_exp complete) s
    | ReturnExp (_, s) ->
         fprintf buf "return(%a)" (pp_print_string_exp complete) s
    | ExportExp (_, s) ->
         fprintf buf "export(%a)" (pp_print_string_exp complete) s
    | CancelExportExp _ ->
         pp_print_string buf "cancel-export"
    | ReturnSaveExp _ ->
         pp_print_string buf "return-current-file"
    | ReturnObjectExp (_, names) ->
         fprintf buf "@[<b 3>return-current-object";
         List.iter (fun v -> fprintf buf "@ %a" pp_print_symbol v) names;
         fprintf buf "@]"
    | KeyExp (_, v) ->
         fprintf buf "$|%s|" v
    | LetKeyExp (_, v, kind, s) ->
         fprintf buf "@[<hv 3>$|%s| %a@ %a@]" (**)
            v
            pp_print_var_def_kind kind
            (pp_print_string_exp complete) s
    | StaticExp (_, node, key, el) ->
         fprintf buf "@[<hv 3>static(%a.%a):@ %a@]" (**)
            pp_print_node node
            pp_print_symbol key
            (string_override "<...>" pp_print_exp_list complete) el

and pp_print_exp_list complete buf el =
   match el with
      [e] ->
         pp_print_exp complete buf e
    | e :: el ->
         pp_print_exp complete buf e;
         pp_print_space buf ();
         pp_print_exp_list complete buf el
    | [] ->
         ()

and pp_print_commands complete buf el =
   List.iter (fun e -> fprintf buf "@ %a" (pp_print_string_exp complete) e) el

(*
 * Print simple parts, abbreviating others as "<exp>"
 *)
let pp_print_exp_simple = pp_print_exp false

(*
 * The complete printers.
 *)
let pp_print_exp = pp_print_exp true
let pp_print_scope_kind = pp_print_scope_kind true
let pp_print_string_exp = pp_print_string_exp true
let pp_print_string_exp_list = pp_print_string_exp_list true

let pp_print_prog buf el =
   fprintf buf "@[<v 0>%a@]" (pp_print_exp_list true) el

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
