(*
 * Define an intermediate representation that is a little
 * easier to work with than the AST.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2005 Jason Hickey, Caltech
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
open Lm_location
open Lm_symbol

open Omake_node_sig
open Omake_node

(* %%MAGICBEGIN%% *)
type var = symbol

(*
 * Var lookup may be private (static scoping),
 * or protected (object scoping),
 * or dynamic (dynamic scoping).
 *)
type scope_kind =
   ScopePrivate
 | ScopeDynamic
 | ScopeProtected
 | ScopeGlobal

(*
 * Evaluation of lazy applications is delayed as much as possible.
 * Eager applications are evaluated even in the scope of a lazy
 * application.  Normal applications are evaluated eagerly, except
 * in the scope of a lazy application.
 *)
type apply_strategy =
   LazyApply
 | EagerApply
 | NormalApply

(*
 * Arity of functions.
 *)
type arity =
   ArityRange of int * int
 | ArityExact of int
 | ArityNone
 | ArityAny

(*
 * Kinds of matches.
 *)
type match_kind =
   MatchWild
 | MatchRegex

(*
 * Variable definitions have several forms.
 *    VarDefNormal: normal definition
 *    VarDefAppend: append the text
 *)
type var_def_kind =
   VarDefNormal
 | VarDefAppend

(*
 * Expression that results in a string.
 *)
type string_exp =
   NoneString        of loc
 | ConstString       of loc * string
 | ApplyString       of loc * apply_strategy * scope_kind * var * string_exp list
 | SuperApplyString  of loc * apply_strategy * scope_kind * var * var * string_exp list
 | MethodApplyString of loc * apply_strategy * scope_kind * var list * string_exp list
 | SequenceString    of loc * string_exp list
 | ArrayString       of loc * string_exp list
 | ArrayOfString     of loc * string_exp
 | QuoteString       of loc * string_exp list
 | QuoteStringString of loc * char * string_exp list
 | BodyString        of loc * exp
 | ExpString         of loc * exp
 | CasesString       of loc * (var * string_exp * exp) list
 | ThisString        of loc * scope_kind
 | KeyString         of loc * apply_strategy * string

and source_exp = node_kind * string_exp

and source_table = string_exp SymbolTable.t

(*
 * Commands.
 *)
and rule_command =
   RuleSection of string_exp * exp
 | RuleString of string_exp

and exp =
   LetVarExp        of loc * scope_kind * var * var_def_kind * string_exp
 | LetFunExp        of loc * scope_kind * var * var list * exp
 | LetObjectExp     of loc * scope_kind * var * exp list
 | LetThisExp       of loc * string_exp
 | ShellExp         of loc * string_exp
 | IfExp            of loc * (string_exp * exp) list
 | SequenceExp      of loc * exp list
 | SectionExp       of loc * string_exp * exp list
 | OpenExp          of loc * Node.t list
 | IncludeExp       of loc * string_exp * string_exp list
 | ApplyExp         of loc * scope_kind * var * string_exp list
 | SuperApplyExp    of loc * scope_kind * var * var * string_exp list
 | MethodApplyExp   of loc * scope_kind * var list * string_exp list
 | KeyExp           of loc * string
 | LetKeyExp        of loc * string * var_def_kind * string_exp
 | StaticExp        of loc * Node.t * symbol * exp list
 | ExportExp        of loc * string_exp
 | CancelExportExp  of loc
 | StringExp        of loc * string_exp
 | ReturnExp        of loc * string_exp
 | ReturnCatchExp   of loc * exp
 | ReturnObjectExp  of loc * symbol list
 | ReturnSaveExp    of loc

type prog = exp list
(* %%MAGICEND%% *)

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
