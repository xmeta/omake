(*
 * Compile the AST to IR.
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
open Lm_location

open Omake_ir
open Omake_env
open Omake_node

(*
 * Scope environments.
 *)
type senv

(*
 * Scopes for variables.
 *)
type senv_var_scope  = scope_kind SymbolTable.t

(*
 * A function to return the variable declarations in a
 * file, to implement the "open" operation.
 *)
type senv_open_file  = string -> pos -> loc -> Node.t * senv_var_scope

(*
 * senv_create open_file vars file
 *   open_file: function to return the variable declarations in a file
 *   vars: the current scope kind for all the defined vars
 *   file: the file being read
 *
 * For a normal "include", the vars should be the Pervasives vars.
 * For .SUBDIRS it should be the current environment.
 *)
val senv_create      : senv_open_file -> senv_var_scope -> Node.t -> senv

(*
 * Class declarations in this scope.
 *)
val senv_class_names : senv -> symbol list * scope_kind SymbolTable.t

(*
 * Internal function for converting string expressions.
 *)
val build_string     : senv -> Omake_ast.exp -> pos -> string_exp

(*
 * Compile an AST program.
 *)
val compile_exp      : senv -> Omake_ast.exp -> senv * exp
val compile_exp_list : senv -> Omake_ast.exp list -> senv * exp
val compile_prog     : senv -> Omake_ast.prog -> senv * exp

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
