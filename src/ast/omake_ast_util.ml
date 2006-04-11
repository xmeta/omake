(*
 * General utilities on the AST.
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
open Lm_symbol

open Omake_ast

let loc_of_exp = function
   NullExp loc
 | StringExp (_, loc)
 | QuoteExp (_, loc)
 | QuoteStringExp (_, _, loc)
 | SequenceExp (_, loc)
 | ApplyExp (_, _, _, loc)
 | SuperApplyExp (_, _, _, _, loc)
 | MethodApplyExp (_, _, _, loc)
 | CommandExp (_, _, _, loc)
 | VarDefExp (_, _, _, _, loc)
 | VarDefBodyExp (_, _, _, _, loc)
 | KeyExp (_, _, loc)
 | KeyDefExp (_, _, _, _, loc)
 | KeyDefBodyExp (_, _, _, _, loc)
 | FunDefExp (_, _, _, loc)
 | ObjectDefExp (_, _, _, loc)
 | RuleExp (_, _, _, _, _, loc)
 | BodyExp (_, loc)
 | ShellExp (_, loc)
 | CatchExp (_, _, _, loc)
 | ClassExp (_, loc) ->
      loc

(*
 * Get a key word that describes the expression.
 *)
let rec last vl =
   match vl with
      [v] ->
         v
    | _ :: vl ->
         last vl
    | [] ->
         raise (Invalid_argument "last")

let key_of_exp = function
   NullExp _ ->
     "null"
 | StringExp _
 | QuoteExp _
 | QuoteStringExp _
 | SequenceExp _ ->
     "string"
 | ApplyExp (_, v, _, _)
 | CommandExp (v, _, _, _)
 | SuperApplyExp (_, v, _, _, _) ->
      Lm_symbol.to_string v
 | VarDefExp (vl, _, _, _, _)
 | VarDefBodyExp (vl, _, _, _, _)
 | ObjectDefExp (vl, _, _, _)
 | FunDefExp (vl, _, _, _)
 | MethodApplyExp (_, vl, _, _) ->
      Lm_symbol.to_string (last vl)
 | KeyExp _
 | KeyDefExp _
 | KeyDefBodyExp _ ->
     "key"
 | RuleExp _ ->
     "rule"
 | BodyExp _ ->
     "body"
 | ShellExp _ ->
     "shell"
 | CatchExp _ ->
     "catch"
 | ClassExp _ ->
     "class"

(*
 * Update the body of the expression.
 *)
let update_body e body =
   if body = [] then
      e
   else
      match e with
         NullExp _
       | StringExp _
       | QuoteExp _
       | QuoteStringExp _
       | SequenceExp _
       | VarDefExp _
       | KeyExp _
       | KeyDefExp _
       | BodyExp _
       | ShellExp _
       | ClassExp _ ->
            raise (Invalid_argument "update_body")
       | ApplyExp (strategy, v, args, loc) ->
            ApplyExp (strategy, v, BodyExp (body, loc) :: args, loc)
       | SuperApplyExp (strategy, super, v, args, loc) ->
            SuperApplyExp (strategy, super, v, BodyExp (body, loc) :: args, loc)
       | MethodApplyExp (strategy, vl, args, loc) ->
            MethodApplyExp (strategy, vl, BodyExp (body, loc) :: args, loc)
       | CommandExp (v, e, _, loc) ->
            CommandExp (v, e, body, loc)
       | VarDefBodyExp (v, kind, flag, _, loc) ->
            VarDefBodyExp (v, kind, flag, body, loc)
       | KeyDefBodyExp (v, kind, flag, _, loc) ->
            KeyDefBodyExp (v, kind, flag, body, loc)
       | ObjectDefExp (v, flag, _, loc) ->
            ObjectDefExp (v, flag, body, loc)
       | FunDefExp (v, params, _, loc) ->
            FunDefExp (v, params, body, loc)
       | RuleExp (flag, target, pattern, sources, _, loc) ->
            RuleExp (flag, target, pattern, sources, body, loc)
       | CatchExp (name, v, _, loc) ->
            CatchExp (name, v, body, loc)

(*
 * Indicate whether the command may have remaining parts.
 *)
let continue_commands =
   ["if",        "else";
    "elseif",    "else";
    "switch",    "case";
    "match",     "case";
    "lexer",     "case";
    "case",      "case";
    "default",   "case";
    "try",       "catch";
    "catch",     "catch"]

let continue_syms =
   List.fold_left (fun set (s1, s2) ->
         SymbolTable.add set (Lm_symbol.add s1) s2) SymbolTable.empty continue_commands

let can_continue e =
   match e with
      NullExp _
    | StringExp _
    | QuoteExp _
    | QuoteStringExp _
    | SequenceExp _
    | ApplyExp _
    | SuperApplyExp _
    | MethodApplyExp _
    | VarDefExp _
    | VarDefBodyExp _
    | KeyExp _
    | KeyDefExp _
    | KeyDefBodyExp _
    | ObjectDefExp _
    | FunDefExp _
    | RuleExp _
    | BodyExp _
    | ShellExp _
    | ClassExp _ ->
         None
    | CatchExp _ ->
         Some "catch"
    | CommandExp (v, _, _, _) ->
         try Some (SymbolTable.find continue_syms v) with
            Not_found ->
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
