(*
 * Convert the AST to an IR representation.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Jason Hickey, Caltech
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
open Lm_location
open Lm_symbol

open Omake_ir
open Omake_env
open Omake_symbol
open Omake_node_sig
open Omake_node

module Pos = MakePos (struct let name = "Omake_ir_ast" end)
open Pos

(*
 * Temporary type for collecting try blocks.
 *
 *     class_names: the names of the current object
 *     mode: the current scope mode
 *     vars: the mode of all the vars in scope
 *     file: the current file
 *     index: the index of the next static block
 *     id: the name of the next static block
 *        (invariant) the index matches the symbol index
 *)
type ast_exp = Omake_ast.exp

(*
 * Environment for parsing AST files.
 *)
type senv_var_scope  = scope_kind SymbolTable.t
type senv_open_file  = string -> pos -> loc -> Node.t * senv_var_scope

type senv_globals =
   { senv_open_file         : senv_open_file;
     senv_file              : Node.t;
     mutable senv_undefined : SymbolSet.t
   }

type senv =
   { senv_class_names    : symbol list;
     senv_mode           : bool * scope_kind;
     senv_vars           : senv_var_scope;
     senv_index          : int;
     senv_id             : symbol;
     senv_globals        : senv_globals
   }

(************************************************************************
 * Utilities.
 *)

(*
 * In a nested object, all currently protected vars become private.
 *)
let nested_object_vars vars =
   SymbolTable.map (function
      ScopeProtected ->
         ScopePrivate
    | kind ->
         kind) vars

(*
 * Collect the cases in a conditional.
 *)
let rec collect_if cases el =
   match el with
      Omake_ast.CommandExp (v, e, body, loc) :: el when Lm_symbol.eq v elseif_sym ->
         collect_if ((e, body) :: cases) el
    | Omake_ast.CommandExp (v, e, body, loc) :: el when Lm_symbol.eq v else_sym ->
         let cases = (Omake_ast.StringExp ("true", loc), body) :: cases in
            List.rev cases, el
    | _ ->
         List.rev cases, el

(*
 * Generic case collection.
 *)
let rec collect_cases cases el =
   match el with
      (Omake_ast.CommandExp (v, e, body, _) :: el) when SymbolSet.mem clauses_set v ->
         collect_cases ((v, e, body) :: cases) el
    | (Omake_ast.CatchExp (v1, v2, body, loc) :: el) ->
         collect_cases ((v1, Omake_ast.StringExp (Lm_symbol.to_string v2, loc), body) :: cases) el
    | _ ->
         List.rev cases, el

(************************************************************************
 * Scoping.
 *)

(*
 * Builtin-vars.
 *)
let builtin_vars =
   [star_sym; gt_sym; at_sym; plus_sym; hat_sym; lt_sym; amp_sym; nf_sym]

(*
 * Default is public.
 *)
let static_string = "$static"

let senv_create open_file vars node =
   let globals =
      { senv_open_file = open_file;
        senv_file      = node;
        senv_undefined = SymbolSet.empty
      }
   in
   let vars = List.fold_left (fun vars v -> SymbolTable.add vars v ScopeGlobal) vars builtin_vars in
      { senv_class_names = [];
        senv_mode        = false, ScopeGlobal;
        senv_vars        = vars;
        senv_index       = 0;
        senv_id          = Lm_symbol.make static_string 0;
        senv_globals     = globals
      }

let senv_class_names senv =
   let { senv_class_names = class_names;
         senv_vars        = vars
       } = senv
   in
      class_names, vars

(*
 * Change the scope mode.
 *)
let senv_scope senv scope =
   let mode = senv.senv_mode in
   let senv = { senv with senv_mode = scope } in
      senv, mode

(*
 * Get the scope for a variable.
 * Numeric symbols are global by default.
 *)
let senv_find_var senv loc pos v =
   try SymbolTable.find senv.senv_vars v with
      Not_found ->
         snd senv.senv_mode

let senv_find_method_var senv loc pos vl =
   match vl with
      v :: _ ->
         senv_find_var senv loc pos v
    | [] ->
         raise (OmakeException (pos, StringError "empty method name"))

let senv_add_var senv v =
   let { senv_mode = force, mode;
         senv_vars = vars
       }  = senv
   in
      if force then
         { senv with senv_vars = SymbolTable.add vars v mode }, mode
      else
         try senv, SymbolTable.find senv.senv_vars v with
            Not_found ->
               { senv with senv_vars = SymbolTable.add vars v mode }, mode

let senv_add_params senv params =
   { senv with senv_vars =
                  List.fold_left (fun vars v ->
                        SymbolTable.add vars v ScopeProtected) senv.senv_vars params
   }

let senv_add_method_var senv pos loc vl =
   match vl with
      [v] ->
         let senv, scope = senv_add_var senv v in
            senv, scope, v
    | [scope_var; v] ->
         let scope =
            if Lm_symbol.eq scope_var this_sym || Lm_symbol.eq scope_var protected_sym then
               ScopeProtected
            else if Lm_symbol.eq scope_var private_sym then
               ScopePrivate
            else if Lm_symbol.eq scope_var public_sym then
               ScopeDynamic
            else
               raise (OmakeException (loc_pos loc pos, StringMethodError ("illegal definition", vl)))
         in
            senv, scope, v
    | _ ->
         raise (OmakeException (loc_pos loc pos, StringMethodError ("illegal definition", vl)))

(*
 * Open a file and include all the symbols.
 *)
let senv_open_file senv pos loc filename =
   let node, vars = senv.senv_globals.senv_open_file filename pos loc in
   let vars = SymbolTable.fold SymbolTable.add senv.senv_vars vars in
      { senv with senv_vars = vars }, node

(************************************************************************
 * Conversion
 *)

(*
 * Strategy conversion.
 *)
let ir_strategy_of_ast_strategy strategy =
   match strategy with
      Omake_ast.LazyApply ->
         LazyApply
    | Omake_ast.EagerApply ->
         EagerApply
    | Omake_ast.NormalApply ->
         NormalApply

(*
 * Literal string.
 *)
let build_literal_argv e pos =
   let buf = Buffer.create 32 in
   let rec collect_exp e =
      match e with
         Omake_ast.NullExp _ ->
            ()
       | Omake_ast.StringExp (s, _) ->
            Buffer.add_string buf s
       | Omake_ast.QuoteExp (el, _)
       | Omake_ast.QuoteStringExp (_, el, _)
       | Omake_ast.SequenceExp (el, _) ->
            collect_exp_list el
       | Omake_ast.ApplyExp (_, _, _, loc)
       | Omake_ast.SuperApplyExp (_, _, _, _, loc)
       | Omake_ast.MethodApplyExp (_, _, _, loc)
       | Omake_ast.BodyExp (_, loc)
       | Omake_ast.KeyExp (_, _, loc)
       | Omake_ast.CommandLineExp (_, loc)
       | Omake_ast.CommandExp (_, _, _, loc)
       | Omake_ast.VarDefExp (_, _, _, _, loc)
       | Omake_ast.VarDefBodyExp (_, _, _, _, loc)
       | Omake_ast.KeyDefExp (_, _, _, _, loc)
       | Omake_ast.KeyDefBodyExp (_, _, _, _, loc)
       | Omake_ast.ObjectDefExp (_, _, _, loc)
       | Omake_ast.FunDefExp (_, _, _, loc)
       | Omake_ast.RuleExp (_, _, _, _, _, loc)
       | Omake_ast.ShellExp (_, loc)
       | Omake_ast.CatchExp (_, _, _, loc)
       | Omake_ast.ClassExp (_, loc) ->
            raise (OmakeException (loc_exp_pos loc, SyntaxError "misplaced expression"))
   and collect_exp_list el =
      List.iter collect_exp el
   in
   let s =
      collect_exp e;
      Buffer.contents buf
   in
      try Lm_string_util.parse_args s with
         Failure _
       | Invalid_argument _ ->
            raise (OmakeException (pos, StringStringError ("syntax error", s)))

(*
 * Conversion.
 *)
let rec build_string senv e pos =
   let pos = string_pos "build_string" pos in
      match e with
         Omake_ast.NullExp loc ->
            NoneString loc
       | Omake_ast.StringExp (s, loc) ->
            ConstString (loc, s)
       | Omake_ast.QuoteExp (el, loc) ->
            build_quote_string senv el pos loc
       | Omake_ast.QuoteStringExp (c, el, loc) ->
            build_quote_string_string senv c el pos loc
       | Omake_ast.SequenceExp ([e], _) ->
            build_string senv e pos
       | Omake_ast.SequenceExp (el, loc) ->
            build_sequence_string senv el pos loc
       | Omake_ast.ApplyExp (strategy, v, args, loc) ->
            build_apply_string senv strategy v args pos loc
       | Omake_ast.SuperApplyExp (strategy, super, v, args, loc) ->
            build_super_apply_string senv strategy super v args pos loc
       | Omake_ast.MethodApplyExp (strategy, vl, args, loc) ->
            build_method_apply_string senv strategy vl args pos loc
       | Omake_ast.BodyExp (el, loc) ->
            build_body_string senv el pos loc
       | Omake_ast.KeyExp (strategy, v, loc) ->
            KeyString (loc, ir_strategy_of_ast_strategy strategy, v)
       | Omake_ast.CommandLineExp (_, loc)
       | Omake_ast.CommandExp (_, _, _, loc)
       | Omake_ast.VarDefExp (_, _, _, _, loc)
       | Omake_ast.VarDefBodyExp (_, _, _, _, loc)
       | Omake_ast.KeyDefExp (_, _, _, _, loc)
       | Omake_ast.KeyDefBodyExp (_, _, _, _, loc)
       | Omake_ast.ObjectDefExp (_, _, _, loc)
       | Omake_ast.FunDefExp (_, _, _, loc)
       | Omake_ast.RuleExp (_, _, _, _, _, loc)
       | Omake_ast.ShellExp (_, loc)
       | Omake_ast.CatchExp (_, _, _, loc)
       | Omake_ast.ClassExp (_, loc) ->
            raise (OmakeException (loc_pos loc pos, SyntaxError "misplaced expression"))

and build_string_list senv el pos =
   let pos = string_pos "build_string_list" pos in
      List.map (fun e -> build_string senv e pos) el

and build_string_opt senv sl pos =
   let pos = string_pos "build_string_opt" pos in
      match sl with
         Some s ->
            Some (build_string senv s pos)
       | None ->
            None

(*
 * When building a sequence, try to collapse adjacent constant strings.
 *)
and build_sequence_string senv el pos loc =
   let pos = string_pos "build_sequence_string" pos in
   let args = build_sequence_string_aux senv el pos loc in
      SequenceString (loc, args)

and build_quote_string senv el pos loc =
   let pos = string_pos "build_quote_string" pos in
   let args = build_sequence_string_aux senv el pos loc in
      QuoteString (loc, args)

and build_quote_string_string senv c el pos loc =
   let pos = string_pos "build_quote_string_string" pos in
   let args = build_sequence_string_aux senv el pos loc in
      QuoteStringString (loc, c, args)

and build_sequence_string_aux senv el pos loc =
   let pos = string_pos "build_sequence_string_aux" pos in
   let buf = Buffer.create 32 in

   (* Flush the buffer *)
   let flush_buffer buf_opt args =
      match buf_opt with
         Some loc ->
            let args = ConstString (loc, Buffer.contents buf) :: args in
               Buffer.clear buf;
               args
       | None ->
            args
   in

   (* Add a constant string to the buffer *)
   let add_string buf_opt s loc =
      Buffer.add_string buf s;
      match buf_opt with
         Some loc' ->
            let loc = union_loc loc' loc in
               Some loc
       | None ->
            Some loc
   in

   (* Collect all the strings in the sequence *)
   let rec collect buf_opt args el =
      match el with
         [] ->
            let args = flush_buffer buf_opt args in
               List.rev args
       | e :: el ->
            let e = build_string senv e pos in
               match e with
                  NoneString _ ->
                     collect buf_opt args el
                | ConstString (loc, s) ->
                     let buf_opt = add_string buf_opt s loc in
                        collect buf_opt args el
                | KeyString _
                | ApplyString _
                | SuperApplyString _
                | MethodApplyString _
                | SequenceString _
                | BodyString _
                | ArrayString _
                | ArrayOfString _
                | QuoteString _
                | QuoteStringString _
                | ExpString _
                | CasesString _
                | ThisString _ ->
                     let args = flush_buffer buf_opt args in
                     let args = e :: args in
                        collect None args el
   in
      collect None [] el

(*
 * Build an application.
 *)
and build_apply_string senv strategy v args pos loc =
   let pos = string_pos "build_apply_string" pos in
   let strategy =
      match strategy with
         Omake_ast.LazyApply ->
            LazyApply
       | Omake_ast.EagerApply ->
            EagerApply
       | Omake_ast.NormalApply ->
            NormalApply
   in
      if Lm_symbol.eq v this_sym || Lm_symbol.eq v protected_sym then
         ThisString (loc, ScopeProtected)
      else if Lm_symbol.eq v public_sym then
         ThisString (loc, ScopeDynamic)
      else if Lm_symbol.eq v private_sym then
         ThisString (loc, ScopePrivate)
      else
         let scope = senv_find_var senv loc pos v in
         let args = build_string_list senv args pos in
            ApplyString (loc, strategy, scope, v, args)

(*
 * Super call.
 *)
and build_super_apply_string senv strategy super v args pos loc =
   let pos = string_pos "build_super_apply_string" pos in
   let strategy =
      match strategy with
         Omake_ast.LazyApply ->
            LazyApply
       | Omake_ast.EagerApply ->
            EagerApply
       | Omake_ast.NormalApply ->
            NormalApply
   in
   let scope = senv_find_var senv loc pos super in
   let args = build_string_list senv args pos in
      SuperApplyString (loc, strategy, scope, super, v, args)

(*
 * Build a method application.
 *)
and build_method_apply_string senv strategy vl args pos loc =
   let pos = string_pos "build_method_apply_string" pos in
   let scope, vl =
      match vl with
         v :: vl' ->
            if Lm_symbol.eq v this_sym || Lm_symbol.eq v protected_sym then
               ScopeProtected, vl'
            else if Lm_symbol.eq v public_sym then
               ScopeDynamic, vl'
            else if Lm_symbol.eq v private_sym then
               ScopePrivate, vl'
            else
               senv_find_var senv loc pos v, vl
       | [] ->
            raise (OmakeException (loc_pos loc pos, StringError "empty method name"))
   in
   let args = build_string_list senv args pos in
   let strategy =
      match strategy with
         Omake_ast.LazyApply ->
            LazyApply
       | Omake_ast.EagerApply ->
            EagerApply
       | Omake_ast.NormalApply ->
            NormalApply
   in
      match vl with
         [v] ->
            ApplyString (loc, strategy, scope, v, args)
       | _ ->
            MethodApplyString (loc, strategy, scope, vl, args)

(*
 * Build a body expression.
 *)
and build_body_string senv el pos loc =
   let pos = string_pos "build_body_string" pos in
   let _, body = build_sequence_exp senv el pos loc in
      BodyString (loc, body)

(*
 * Build an expression.
 *)
and build_exp senv e =
   let pos = string_pos "build_exp" (ast_exp_pos e) in
      match e with
         Omake_ast.NullExp loc
       | Omake_ast.StringExp (_, loc)
       | Omake_ast.QuoteExp (_, loc)
       | Omake_ast.QuoteStringExp (_, _, loc) ->
            senv, SequenceExp (loc, [])
       | Omake_ast.SequenceExp ([e], _)
       | Omake_ast.BodyExp ([e], _) ->
            build_exp senv e
       | Omake_ast.SequenceExp (el, loc)
       | Omake_ast.BodyExp (el, loc) ->
            build_sequence_exp senv el pos loc
       | Omake_ast.ApplyExp (_, v, args, loc) ->
            build_apply_exp senv v args pos loc
       | Omake_ast.SuperApplyExp (_, super, v, args, loc) ->
            build_super_apply_exp senv super v args pos loc
       | Omake_ast.MethodApplyExp (_, vl, args, loc) ->
            build_method_apply_exp senv vl args pos loc
       | Omake_ast.CommandLineExp (argv, loc) ->
            build_command_line_exp senv argv pos loc
       | Omake_ast.CommandExp (v, arg, commands, loc) ->
            build_command_exp senv v arg commands pos loc
       | Omake_ast.VarDefExp (v, kind, flag, e, loc) ->
            build_var_def_exp senv v kind flag e pos loc
       | Omake_ast.VarDefBodyExp (v, kind, flag, [], loc) ->
            build_var_def_exp senv v kind flag (Omake_ast.SequenceExp ([], loc)) pos loc
       | Omake_ast.VarDefBodyExp (v, kind, flag, el, loc) ->
            build_var_def_body_exp senv v kind flag el pos loc
       | Omake_ast.KeyExp (_, v, loc) ->
            senv, KeyExp (loc, v)
       | Omake_ast.KeyDefExp (v, kind, flag, e, loc) ->
            build_key_def_exp senv v kind flag e pos loc
       | Omake_ast.KeyDefBodyExp (v, kind, flag, el, loc) ->
            build_key_def_body_exp senv v kind flag el pos loc
       | Omake_ast.ObjectDefExp (v, flag, el, loc) ->
            build_object_def_exp senv v flag el pos loc
       | Omake_ast.FunDefExp (v, params, e, loc) ->
            build_fun_def_exp senv v params e pos loc
       | Omake_ast.RuleExp (multiple, target, pattern, source, commands, loc) ->
            build_rule_exp senv multiple target pattern source commands pos loc
       | Omake_ast.ShellExp (e, loc) ->
            build_shell_exp senv e pos loc
       | Omake_ast.CatchExp (_, _, _, loc) ->
            raise (OmakeException (pos, StringError "misplaced catch clause"))
       | Omake_ast.ClassExp (names, loc) ->
            build_class_exp senv loc names

(*
 * Add the class names.
 *)
and build_class_exp senv loc names =
   let senv =
      { senv with senv_mode = true, ScopeProtected;
                  senv_class_names = names @ senv.senv_class_names
      }
   in
      senv, SequenceExp (loc, [])

(*
 * Sequence exp.  Build the expression one at a time.
 *)
and build_sequence senv pos rval el =
   match el with
      Omake_ast.CommandExp (v, e, body, loc)
      :: el when Lm_symbol.eq v if_sym ->
         let cases, el = collect_if [e, body] el in
         let pos = loc_pos loc pos in
         let senv, e = build_if_exp senv cases pos loc in
         let senv, el = build_sequence senv pos rval el in
            senv, e :: el

    | Omake_ast.CommandExp (v, e, body, loc)
      :: el when Lm_symbol.eq v while_sym ->
         let cases, el = collect_cases [] el in
         let pos = loc_pos loc pos in
         let senv, e = build_opt_cases_command_exp senv v e cases body pos loc in
         let senv, el = build_sequence senv pos rval el in
            senv, e :: el

    | Omake_ast.CommandExp (v, arg, body, loc) :: el ->
         let cases, el = collect_cases [] el in
         let pos = loc_pos loc pos in
         let senv, e = build_cases_command_exp senv v arg cases body pos loc in
         let senv, el = build_sequence senv pos rval el in
            senv, e :: el

    | Omake_ast.ApplyExp (_, v, args, loc) :: el ->
         let cases, el = collect_cases [] el in
         let pos = loc_pos loc pos in
         let senv, e = build_cases_apply_exp senv v args cases pos loc in
         let senv, el = build_sequence senv pos rval el in
            senv, e :: el

    | e :: el ->
         let senv, e = build_exp senv e in
         let senv, el = build_sequence senv pos rval el in
            senv, e :: el

    | [] ->
         rval senv

(*
 * Normal sequences are always in global mode.
 *)
and build_sequence_exp senv el pos loc =
   let pos = string_pos "build_sequence_exp" pos in
   let _, el = build_sequence senv pos (fun senv -> senv, []) el in
      senv, SequenceExp (loc, el)

and build_scope_exp senv scope el pos loc =
   let pos = string_pos "build_scope_exp" pos in
   let senv, scope = senv_scope senv (true, scope) in
   let senv, el = build_sequence senv pos (fun senv -> senv, []) el in
   let senv, _ = senv_scope senv scope in
      senv, SequenceExp (loc, el)

and build_static_scope_exp senv el pos loc =
   let pos = string_pos "build_scope_exp" pos in
   let { senv_globals = globals; senv_index = index } = senv in
   let { senv_file = file } = globals in
   let index = succ index in
   let id = Lm_symbol.make static_string index in
   let senv = { senv with senv_index = index; senv_id = id } in
   let senv, scope = senv_scope senv (true, ScopeProtected) in
   let senv, el = build_sequence senv pos (fun senv -> senv, [ReturnObjectExp (loc, [])]) el in
   let senv, _ = senv_scope senv scope in
      senv, StaticExp (loc, file, id, el)

(*
 * In an application, turn the arguments into strings.
 *)
and build_apply_exp senv v args pos loc =
   let pos = string_pos "build_apply_exp" pos in
   let result =
      match build_string_list senv args pos with
         [arg] when Lm_symbol.eq v return_sym ->
            ReturnExp (loc, arg)
       | args ->
            let scope = senv_find_var senv loc pos v in
               ApplyExp (loc, scope, v, args)
   in
      senv, result

and build_super_apply_exp senv super v args pos loc =
   let pos = string_pos "build_super_apply_exp" pos in
   let args = build_string_list senv args pos in
   let scope = senv_find_var senv loc pos super in
      senv, SuperApplyExp (loc, scope, super, v, args)

and build_method_apply_exp senv vl args pos loc =
   let pos = string_pos "build_method_apply_exp" pos in
   let args = build_string_list senv args pos in
   let scope, vl =
      match vl with
         v :: vl' ->
            if Lm_symbol.eq v this_sym || Lm_symbol.eq v protected_sym then
               ScopeProtected, vl'
            else if Lm_symbol.eq v public_sym then
               ScopeDynamic, vl'
            else if Lm_symbol.eq v private_sym then
               ScopePrivate, vl'
            else
               senv_find_var senv loc pos v, vl
       | [] ->
            raise (OmakeException (loc_pos loc pos, StringError "empty method name"))
   in
   let e =
      match vl with
         [v] ->
            ApplyExp (loc, scope, v, args)
       | _ ->
            MethodApplyExp (loc, scope, vl, args)
   in
      senv, e

and build_cases_apply_exp senv v args cases pos loc =
   let pos = string_pos "build_cases_apply_exp" pos in
   let args = build_string_list senv args pos in
   let result =
      match args, cases with
         [arg], [] when Lm_symbol.eq v return_sym ->
            ReturnExp (loc, arg)
       | _, [] ->
            let scope = senv_find_var senv loc pos v in
               ApplyExp (loc, scope, v, args)
       | _ ->
            let scope = senv_find_var senv loc pos v in
            let cases =
               List.map (fun (v, e, body) ->
                     let _, body = build_sequence_exp senv body pos loc in
                        v, build_string senv e pos, body) cases
            in
            let args = CasesString (loc, cases) :: args in
               ApplyExp (loc, scope, v, args)
   in
      senv, result

and build_cases_command_exp senv v arg cases commands pos loc =
   let pos = string_pos "build_cases_command_exp" pos in
      match cases with
         [] ->
            build_command_exp senv v arg commands pos loc
       | _ ->
            let arg =
               match commands with
                  [] ->
                     arg
                | _ ->
                     Omake_ast.BodyExp (commands, loc)
            in
               build_cases_apply_exp senv v [arg] cases pos loc

and build_opt_cases_command_exp senv v arg cases commands pos loc =
   let pos = string_pos "build_opt_cases_command_exp" pos in
   let cases =
      if commands = [] then
         cases
      else
         let default = default_sym, Omake_ast.NullExp loc, commands in
            cases @ [default]
   in
      build_cases_apply_exp senv v [arg] cases pos loc

(*
 * The command line is handled at parse time as well as
 * at evaluation time.
 *)
and build_command_line_exp senv argv pos loc =
   let _pos = string_pos "build_apply_exp" pos in
   let argv = List.map (fun s -> ConstString (loc, s)) argv in
   let argv = ArrayString (loc, argv) in
   let e = ApplyExp (loc, ScopeGlobal, omakeflags_sym, [argv]) in
      senv, e

(*
 * Commands.
 *)
and build_command_exp senv v arg commands pos loc =
   let pos = string_pos "build_command_exp" pos in
      if Lm_symbol.eq v include_sym then
         build_include_exp senv arg commands pos loc
      else if Lm_symbol.eq v if_sym then
         build_if_exp senv [arg, commands] pos loc
      else if Lm_symbol.eq v section_sym then
         build_section_exp senv arg commands pos loc
      else if Lm_symbol.eq v value_sym then
         build_value_exp senv arg commands pos loc
      else if commands <> [] then
         raise (OmakeException (loc_pos loc pos, StringVarError ("illegal body for", v)))
      else if Lm_symbol.eq v return_sym then
         build_return_exp senv arg pos loc
      else if Lm_symbol.eq v export_sym then
         build_export_exp senv arg pos loc
      else if Lm_symbol.eq v open_sym then
         build_open_exp senv arg pos loc
      else if Lm_symbol.eq v declare_sym then
         build_declare_exp senv arg pos loc
      else if Lm_symbol.eq v autoload_sym then
         senv, SequenceExp (loc, [])
      else
         build_apply_exp senv v [arg] pos loc

(*
 * Include a file.
 *)
and build_include_exp senv e commands pos loc =
   let pos = string_pos "build_include_exp" pos in
   let s = build_string senv e pos in
   let commands = build_string_list senv commands pos in
      senv, IncludeExp (loc, s, commands)

(*
 * Conditionals.
 *)
and build_if_exp senv cases pos loc =
   let pos = string_pos "build_if_exp" pos in
   let cases =
      List.map (fun (e1, e2) ->
            let s = build_string senv e1 pos in
            let _, e2 = build_sequence_exp senv e2 pos loc in
               s, e2) cases
   in
      senv, IfExp (loc, cases)

(*
 * A section is just an "if true" command.
 *)
and build_section_exp senv e1 e2 pos loc =
   let pos = string_pos "build_section_exp" pos in
   let s = build_string senv e1 pos in
   let _, body = build_sequence senv pos (fun senv -> senv, []) e2 in
      senv, SectionExp (loc, s, body)

(*
 * Export the environment.
 *)
and build_export_exp senv e pos loc =
   let pos = string_pos "build_export_exp" pos in
   let s = build_string senv e pos in
      senv, ExportExp (loc, s)

(*
 * Return a value.
 *)
and build_value_exp senv e commands pos loc =
   let pos = string_pos "build_value_exp" pos in
   let s =
      match e, commands with
         e, [] ->
            build_string senv e pos
       | Omake_ast.NullExp _, el ->
            begin match build_sequence_exp senv el pos loc with
               _, SequenceExp (loc, [e]) ->
                  ExpString (loc, e)
             | _ ->
                  raise (OmakeException (loc_pos loc pos, StringError ("Value is not allowed to have more than one statement in its body")))
            end
       | _, _ :: _  ->
            raise (OmakeException (loc_pos loc pos, StringError ("Value can have an argument or a body, but not both")))
   in
      senv, StringExp (loc, s)

and build_return_exp senv e pos loc =
   let pos = string_pos "build_return_exp" pos in
   let s = build_string senv e pos in
      senv, ReturnExp (loc, s)

(*
 * Open the namespace from another file.
 *)
and build_open_exp senv arg pos loc =
   let pos = string_pos "build_open_exp" pos in
   let argv = build_literal_argv arg pos in
   let senv, nodes =
      List.fold_left (fun (senv, nodes) filename ->
            let senv, node = senv_open_file senv pos loc filename in
            let nodes = node :: nodes in
               senv, nodes) (senv, []) argv
   in
      senv, OpenExp (loc, List.rev nodes)

(*
 * Declare a variable, but dont worry about its definition.
 *)
and build_declare_exp senv arg pos loc =
   let pos = string_pos "build_var_def_exp" pos in
   let argv = build_literal_argv arg pos in
   let senv = List.fold_left (fun senv s -> fst (senv_add_var senv (Lm_symbol.add s))) senv argv in
      senv, SequenceExp (loc, [])

(*
 * The public/protected/private are not really objects.
 * They are scope definitions.
 *)
and build_object_def_exp senv vl flag body pos loc =
   match vl with
      [v] ->
         if Lm_symbol.eq v private_sym then
            build_scope_exp senv ScopePrivate body pos loc
         else if Lm_symbol.eq v protected_sym then
            build_scope_exp senv ScopeProtected body pos loc
         else if Lm_symbol.eq v public_sym then
            build_scope_exp senv ScopeDynamic body pos loc
         else if Lm_symbol.eq v static_sym then
            build_static_scope_exp senv body pos loc
         else
            build_normal_object_def_exp senv vl flag body pos loc
    | _ ->
         build_normal_object_def_exp senv vl flag body pos loc

(*
 * An object is a collection of definitions.
 *)
and build_normal_object_def_exp senv v flag body pos loc =
   let pos = string_pos "build_object_def_exp" pos in
   let senv, scope, v = senv_add_method_var senv pos loc v in
   let pscope, parent =
      match flag with
         Omake_ast.DefineNormal ->
            ScopeGlobal, object_sym
       | Omake_ast.DefineAppend ->
            scope, v
   in
   let rval senv =
      senv, [ReturnObjectExp (loc, senv.senv_class_names)]
   in
   let senv' =
      { senv with senv_mode = true, ScopeProtected;
                  senv_vars = nested_object_vars senv.senv_vars;
                  senv_class_names = []
      }
   in
   let _, body = build_sequence senv' pos rval body in
   let escope = senv_find_var senv loc pos extends_sym in
   let body = ApplyExp (loc, escope, extends_sym, [ApplyString (loc, EagerApply, pscope, parent, [])]) :: body in
      senv, LetObjectExp (loc, scope, v, body)

(*
 * Variable definition.
 *)
and build_var_def_kind flag =
   match flag with
      Omake_ast.DefineNormal ->
         VarDefNormal
    | Omake_ast.DefineAppend ->
         VarDefAppend

(*
 * Variable definitions.
 *)
and build_var_def_exp senv v kind flag e pos loc =
   let pos = string_pos "build_var_def_exp" pos in
   let s = build_string senv e pos in
   let s =
      match kind with
         Omake_ast.DefineString ->
            s
       | Omake_ast.DefineArray ->
            ArrayOfString (loc, s)
   in
   let kind = build_var_def_kind flag in
      match v with
         [v] when Lm_symbol.eq v this_sym ->
            senv, LetThisExp (loc, s)
       | _ ->
            let senv, scope, v = senv_add_method_var senv pos loc v in
               senv, LetVarExp (loc, scope, v, kind, s)

and build_var_def_body_exp senv v kind flag body pos loc =
   let pos = string_pos "build_var_def_body_exp" pos in
   let e =
      match kind with
         Omake_ast.DefineString ->
            let _, el = build_sequence_exp senv body pos loc in
               ExpString (loc, el)
       | Omake_ast.DefineArray ->
            ArrayString (loc, build_string_list senv body pos)
   in
   let kind = build_var_def_kind flag in
   let senv, scope, v = senv_add_method_var senv pos loc v in
      senv, LetVarExp (loc, scope, v, kind, e)

(*
 * Key definitions (for object properties.
 *)
and build_key_def_exp senv v kind flag e pos loc =
   let pos = string_pos "build_key_def_exp" pos in
   let s = build_string senv e pos in
   let s =
      match kind with
         Omake_ast.DefineString ->
            s
       | Omake_ast.DefineArray ->
            ArrayOfString (loc, s)
   in
   let kind = build_var_def_kind flag in
      senv, LetKeyExp (loc, v, kind, s)

and build_key_def_body_exp senv v kind flag body pos loc =
   let pos = string_pos "build_key_def_body_exp" pos in
   let e =
      match kind with
         Omake_ast.DefineString ->
            let _, el = build_sequence_exp senv body pos loc in
               ExpString (loc, el)
       | Omake_ast.DefineArray ->
            ArrayString (loc, build_string_list senv body pos)
   in
   let kind = build_var_def_kind flag in
      senv, LetKeyExp (loc, v, kind, e)

(*
 * Function definition.
 *)
and build_fun_def_exp senv v params el pos loc =
   let pos = string_pos "build_fun_def_exp" pos in
   let _, mode = senv.senv_mode in
   let senv', _ = senv_scope senv (false, mode) in
   let senv' = senv_add_params senv' params in
   let _, body = build_sequence_exp senv' el pos loc in
   let senv, scope, v = senv_add_method_var senv pos loc v in
      senv, LetFunExp (loc, scope, v, params, body)

(*
 * Special rule expressions.
 *)
and build_rule_exp senv multiple target pattern sources body pos loc =
   let pos = string_pos "build_rule_exp" pos in
   let target  = build_string senv target pos in
   let pattern = build_string senv pattern pos in

   (* Get the body *)
   let body =
      let senv = { senv with senv_class_names = [] } in
      let _, body = build_sequence senv pos (fun senv -> senv, []) body in
         BodyString (loc, SequenceExp (loc, body))
   in

   (* Get the option list *)
   let options =
      SymbolTable.fold (fun options v source ->
            if Lm_symbol.eq v normal_sym then
               options
            else
               let key = ConstString (loc, Lm_symbol.to_string v) in
               let value = build_string senv source pos in
                  key :: value :: options) [] sources
   in
   let create_map_sym =
      match options with
         [] -> empty_map_sym
       | _ -> create_lazy_map_sym
   in
   let options = ApplyString (loc, EagerApply, ScopeProtected, create_map_sym, options) in

   (* Add the sources *)
   let source =
      try
         let source = SymbolTable.find sources normal_sym in
            build_string senv source pos
      with
         Not_found ->
            ConstString (loc, "")
   in

   (* Multiple string *)
   let multiple =
      let s =
         if multiple then
            "true"
         else
            "false"
      in
         ConstString (loc, s)
   in

   (* Construct the argument list *)
   let args = [multiple; target; pattern; source; options; body] in
   let exp = StringExp (loc, ApplyString (loc, EagerApply, ScopeProtected, rule_sym, args)) in
      senv, exp

(*
 * Shell command.
 *)
and build_shell_exp senv e pos loc =
   let pos = string_pos "build_shell_exp" pos in
      senv, ShellExp (loc, build_string senv e pos)

(*
 * Build the IR from the AST program.
 *)
let check_senv_errors pos senv =
   if not (SymbolSet.is_empty (senv.senv_globals.senv_undefined)) then
      raise (OmakeException (pos, StringNodeError ("some variables were undefined in", senv.senv_globals.senv_file)))

let compile_exp senv e =
   let loc = bogus_loc "Omake_ir_ast" in
   let pos = string_pos "compile" (loc_exp_pos loc) in
   let senv, e = build_exp senv e in
      check_senv_errors pos senv;
      senv, e

let compile_prog senv el =
   let loc = bogus_loc "Omake_ir_ast" in
   let pos = string_pos "compile" (loc_exp_pos loc) in
   let senv, el = build_sequence senv pos (fun senv -> senv, [ReturnSaveExp loc]) el in
      check_senv_errors pos senv;
      senv, SequenceExp (loc, el)

let compile_exp_list senv el =
   let loc = bogus_loc "Omake_ir_ast" in
   let pos = string_pos "compile" (loc_exp_pos loc) in
   let senv, el = build_sequence senv pos (fun senv -> senv, []) el in
      check_senv_errors pos senv;
      senv, SequenceExp (loc, el)

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
