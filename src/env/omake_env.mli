(*
 * Environment for evaluating OMakefiles.
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

open Lm_string_util
open Lm_string_set
open Lm_location
open Lm_symbol

open Omake_ir
open Omake_node
open Omake_exec
open Omake_cache
open Omake_lexer
open Omake_parser
open Omake_node_sig
open Omake_exec_type
open Omake_shell_type
open Omake_options_type
open Omake_command_type
open Omake_ir_free_vars

(*
 * Debugging.
 *)
val debug_scanner  : bool ref
val debug_implicit : bool ref

(*
 * A source string.
 *)
type 'a source = node_kind * 'a

(*
 * A target value that represents a node in a rule.
 *)
type target =
   TargetNode of Node.t
 | TargetString of string

(*
 * Type of objects.
 *)
type obj

(*
 * Type of maps.
 *)
type map

(*
 * Static scopes.
 *)
type env

(*
 * Type of environments.
 *)
type venv

(*
 * Primitive values.
 *)
type prim_fun

(*
 * Exception positions.
 *)
type pos

(*
 * IoChannels.
 *)
type prim_channel

type channel_mode = Lm_channel.mode =
   InChannel
 | OutChannel
 | InOutChannel

(*
 * Kinds of rules.
 *)
type rule_multiple =
   RuleSingle
 | RuleMultiple
 | RuleScannerSingle
 | RuleScannerMultiple

type rule_kind =
   RuleNormal
 | RuleScanner

(*
 * Export kinds.
 *    ExportFile: this is the result of file evaluation
 *    ExportSymbols syms: export just the symbols in the list
 *    ExportAll: export the entire environment.
 *)
type export =
   ExportFile
 | ExportAll
 | ExportDir
 | ExportRules
 | ExportSymbols of symbol list
 | ExportValue of value

(*
 * Possible values.
 * For the function, the obj is the static scope.
 *)
and value =
   ValNone
 | ValInt         of int
 | ValFloat       of float
 | ValSequence    of value list
 | ValArray       of value list
 | ValString      of string
 | ValData        of string
 | ValQuote       of value list
 | ValQuoteString of char * value list
 | ValApply       of loc * scope_kind * var * value list
 | ValSuperApply  of loc * scope_kind * var * var * value list
 | ValMethodApply of loc * scope_kind * var list * value list
 | ValImplicit    of loc * scope_kind * var
 | ValFun         of arity * env * var list * exp
 | ValFunValue    of arity * env * var list * value
 | ValPrim        of arity * bool * prim_fun
 | ValRules       of erule list
 | ValNode        of Node.t
 | ValDir         of Dir.t
 | ValEnv         of venv * export
 | ValBody        of env * exp
 | ValObject      of obj
 | ValMap         of map
 | ValChannel     of channel_mode * prim_channel
 | ValClass       of obj SymbolTable.t
 | ValCases       of (var * value * value) list
 | ValKey         of loc * string
 | ValOther       of value_other

(*
 * Put all the other stuff here, to keep the primary value type
 * smaller.
 *)
and value_other =
   ValLexer       of Lexer.t
 | ValParser      of Parser.t
 | ValLocation    of loc
 | ValPosition    of pos
 | ValExitCode    of int

(*
 * Command lists are used for rule bodies.
 * They have their environment, a list of sources,
 * and the actual body.  The body is polymorphic
 * for various kinds of commands.
 *)
and command =
   CommandSection of value * free_vars * exp    (* Name of the section, its free variables, and the expression *)
 | CommandValue of loc * value

and command_info =
   { command_env       : venv;
     command_sources   : Node.t list;
     command_values    : value list;
     command_body      : command list
   }

(*
 * A rule description.
 *)
and erule =
   { rule_loc         : loc;
     rule_env         : venv;
     rule_target      : Node.t;
     rule_effects     : NodeSet.t;
     rule_locks       : NodeSet.t;
     rule_sources     : NodeSet.t;
     rule_scanners    : NodeSet.t;
     rule_match       : string option;
     rule_multiple    : rule_multiple;
     rule_commands    : command_info list
   }

(*
 * A listing of all the explicit rules.
 *)
and erule_info =
   { explicit_targets         : erule NodeTable.t;
     explicit_deps            : (NodeSet.t * NodeSet.t * NodeSet.t) NodeTable.t;   (* locks, sources, scanners *)
     explicit_directories     : venv DirTable.t
   }

(*
 * Command lines.
 *)
and arg_command_inst = (exp, arg_pipe, value) poly_command_inst
and arg_command_line = (venv, exp, arg_pipe, value) poly_command_line

and string_command_inst = (exp, string_pipe, value) poly_command_inst
and string_command_line = (venv, exp, string_pipe, value) poly_command_line

and apply        = venv -> Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> (symbol * string) list -> value list -> int * value

and value_cmd    = (unit, value list, value list) poly_cmd
and value_apply  = (value list, value list, apply) poly_apply
and value_group  = (unit, value list, value list, value list, apply) poly_group
and value_pipe   = (unit, value list, value list, value list, apply) poly_pipe

and arg_cmd      = (arg cmd_exe, arg, arg) poly_cmd
and arg_apply    = (value, arg, apply) poly_apply
and arg_group    = (arg cmd_exe, arg, value, arg, apply) poly_group
and arg_pipe     = (arg cmd_exe, arg, value, arg, apply) poly_pipe

and string_cmd   = (simple_exe, string, string) poly_cmd
and string_apply = (value, string, apply) poly_apply
and string_group = (simple_exe, string, value, string, apply) poly_group
and string_pipe  = (simple_exe, string, value, string, apply) poly_pipe

(*
 * Command line parsing.
 *)
type lexer = string -> int -> int -> int option

type tok =
   TokString of value
 | TokToken  of string
 | TokGroup  of tok list

(*
 * Type of execution servers.
 *)
type pid =
   InternalPid of int
 | ExternalPid of int
 | ResultPid of int * value

type exec = (arg_command_line, pid, value) Exec.t

(*
 * Ordering info is abstract.
 *)
type ordering_info

(*
 * Inclusion scope is usually Pervasives,
 * but it may include everything in scope.
 *)
type include_scope =
   IncludePervasives
 | IncludeAll

(*
 * Intermediate code include the names of the classes.
 *)
type ir = symbol list * scope_kind SymbolTable.t * Omake_ir.exp

(*
 * Check if command list does not contain anything to execute.
 *)
val commands_are_trivial : command_info list -> bool

(*
 * Convert a target to a raw string.
 *)
val string_of_target : venv -> target -> string

(*
 * This takes the starting directory.
 *)
val create          : omake_options -> string -> exec -> Omake_cache.t -> venv

(*
 * Pervasives management.
 *)
val venv_set_pervasives : venv -> unit
val venv_get_pervasives : venv -> Node.t -> venv

(*
 * Variables in scope.
 *)
val venv_include_scope : venv -> include_scope -> scope_kind SymbolTable.t

(*
 * Fork, so that a thread can work on a private copy in peace.
 *)
val venv_fork       : venv -> venv

(*
 * Global values.
 *)
val venv_exec       : venv -> exec
val venv_cache      : venv -> Omake_cache.t

(*
 * Add values to environment.
 *)
val venv_chdir            : venv -> loc -> string -> venv
val venv_chdir_dir        : venv -> loc -> Dir.t -> venv
val venv_chdir_tmp        : venv -> Dir.t -> venv
val venv_add_dir          : venv -> unit
val venv_directories      : venv -> venv DirTable.t
val venv_add_explicit_dir : venv -> Dir.t -> unit
val venv_remove_explicit_dir : venv -> Dir.t -> unit
val venv_add_file         : venv -> Node.t -> venv
val venv_intern           : venv -> phony_ok -> string -> Node.t
val venv_intern_cd        : venv -> phony_ok -> Dir.t -> string -> Node.t
val venv_intern_dir       : venv -> string -> Dir.t
val venv_intern_target    : venv -> phony_ok -> target -> Node.t
val venv_dirname          : venv -> Dir.t -> string
val venv_nodename         : venv -> Node.t -> string

val venv_mount       : venv -> mount_option list -> Dir.t -> Dir.t -> venv

val venv_add_var     : venv -> scope_kind -> pos -> var -> value -> venv
val venv_add_args    : venv -> pos -> loc -> env -> var list -> value list -> venv
val venv_add_assoc   : venv -> (var * string) list -> venv
val venv_add_phony   : venv -> loc -> target list -> venv

val venv_add_args_hack : venv -> pos -> loc -> env -> var list -> value list -> venv

val venv_add_wild_match  : venv -> value -> venv
val venv_add_match_args  : venv -> string list -> venv
val venv_add_match       : venv -> string -> string list -> venv
val venv_explicit_target : venv -> Node.t -> venv

val venv_add_rule : venv -> pos -> loc ->
   rule_multiple ->                     (* multiple, scanner, etc *)
   target list ->                       (* targets *)
   target list ->                       (* patterns *)
   target source list ->                (* effects *)
   target source list ->                (* sources *)
   target source list ->                (* scanners *)
   value list ->                        (* additional values the target depends on *)
   command list ->                      (* commands *)
   venv * erule list

(*
 * System environment.
 *)
val venv_environment : venv -> string SymbolTable.t
val venv_setenv : venv -> var -> string -> venv
val venv_getenv : venv -> var -> string
val venv_defined_env : venv -> var -> bool

(*
 * Handle options.
 *)
val venv_options : venv -> omake_options
val venv_set_options : venv -> loc -> pos -> string -> venv

(*
 * Find values.
 *)
val venv_dir          : venv -> Dir.t
val venv_defined      : venv -> scope_kind -> var -> bool
val venv_get_var      : venv -> scope_kind -> pos -> var -> value
val venv_find_var     : venv -> scope_kind -> pos -> loc -> var -> value
val venv_find_var_exn : venv -> scope_kind -> var -> value

(*
 * Static environments.
 *)
val venv_empty_env       : env
val venv_get_env         : venv -> env
val venv_with_env        : venv -> env -> venv

(*
 * During marshaling, all primitive functions are reset.
 *)
val venv_marshal            : venv -> ('a -> 'b) -> 'a -> 'b

(*
 * Static values.
 *)
val venv_find_static_object    : venv -> Node.t -> symbol -> obj
val venv_add_static_object     : venv -> Node.t -> symbol -> obj -> unit
val venv_include_static_object : venv -> obj -> venv
val venv_save_static_values    : venv -> unit

(*
 * Primitive functions.
 *)
val venv_add_prim_fun    : venv -> var -> (venv -> pos -> loc -> value list -> value) -> prim_fun
val venv_apply_prim_fun  : prim_fun -> venv -> pos -> loc -> value list -> value

(*
 * Channels.
 *)
val venv_stdin            : prim_channel
val venv_stdout           : prim_channel
val venv_stderr           : prim_channel

val venv_add_channel      : venv -> string -> Lm_channel.kind -> Lm_channel.mode -> bool -> Unix.file_descr -> prim_channel
val venv_close_channel    : venv -> pos -> prim_channel -> unit
val venv_find_channel     : venv -> pos -> prim_channel -> Lm_channel.t
val venv_find_channel_id  : venv -> pos -> int -> prim_channel
val venv_add_formatter_channel : venv -> Format.formatter -> prim_channel

(*
 * Objects.
 *)
val venv_empty_object    : obj
val venv_this            : venv -> obj
val venv_this_object     : venv -> scope_kind -> obj
val venv_current_object  : venv -> symbol list -> obj
val venv_current_objects : venv -> scope_kind -> obj list
val venv_define_object   : venv -> venv
val venv_with_object     : venv -> obj -> venv
val venv_include_object  : venv -> obj -> venv
val venv_flatten_object  : venv -> obj -> venv
val venv_find_super      : venv -> pos -> loc -> symbol -> obj

val venv_add_field       : obj -> var -> value -> obj
val venv_find_field_exn  : obj -> var -> value
val venv_find_field      : obj -> pos -> var -> value
val venv_object_mem      : obj -> var -> bool
val venv_object_length   : obj -> int
val venv_object_fold     : ('a -> var -> value -> 'a) -> 'a -> obj -> 'a
val venv_instanceof      : obj -> symbol -> bool
val venv_add_class       : obj -> symbol -> obj

val venv_add_included_file    : venv -> Node.t -> venv
val venv_is_included_file     : venv -> Node.t -> bool
val venv_find_ir_file_exn     : venv -> Node.t -> ir
val venv_add_ir_file          : venv -> Node.t -> ir -> unit
val venv_find_object_file_exn : venv -> Node.t -> obj
val venv_add_object_file      : venv -> Node.t -> obj -> unit

(*
 * Maps.
 *)
val venv_map_empty       : map
val venv_map_add         : map -> pos -> value -> value -> map
val venv_map_find        : map -> pos -> value -> value
val venv_map_mem         : map -> pos -> value -> bool
val venv_map_iter        : (value -> value -> unit) -> map -> unit
val venv_map_map         : (value -> value -> value) -> map -> map
val venv_map_fold        : ('a -> value -> value -> 'a) -> 'a -> map -> 'a
val venv_map_length      : map -> int

(*
 * Get a list of all the files that were read.
 *)
val venv_files : venv -> NodeSet.t

(*
 * Get the explicit rules.
 *)
val venv_explicit_exists : venv -> Node.t -> bool
val venv_explicit_rules  : venv -> erule_info

(*
 * Find all the implicit rules and dependencies.
 *    (static_deps, lock_deps, scanner_deps, value_deps)
 *)
val venv_find_implicit_deps  : venv -> Node.t -> NodeSet.t * NodeSet.t * NodeSet.t * value list
val venv_find_implicit_rules : venv -> Node.t -> erule list

(*
 * Ordering.
 *)
val venv_add_orders        : venv -> loc -> target list -> venv
val venv_is_order          : venv -> string -> bool
val venv_add_ordering_rule : venv -> pos -> loc -> var -> target -> target list -> venv
val venv_get_ordering_info : venv -> var -> ordering_info
val venv_get_ordering_deps : venv -> ordering_info -> NodeSet.t -> NodeSet.t

(*
 * Update the environment with a result.
 *)
val add_exports : venv -> pos -> value -> venv * value
val add_include : venv -> pos -> value -> venv * value

(*
 * Remove exports from a value
 *)
val export_none : value -> value

(*
 * In case a value is an export, return the given variables in that
 * export to their state in the original environment.
 * (XXX: variables are updated in all 3 scope classes).
 *)
val unexport : venv -> value -> var list -> value

(*
 * Cached buildable flags.
 *)
val venv_find_target_is_buildable_exn : venv -> Node.t -> bool
val venv_find_target_is_buildable_proper_exn : venv -> Node.t -> bool
val venv_add_target_is_buildable : venv -> Node.t -> bool -> unit
val venv_add_target_is_buildable_proper : venv -> Node.t -> bool -> unit

(*
 * Printing.
 *)
val pp_print_tok : formatter -> tok -> unit

val pp_print_string_pipe : formatter -> string_pipe -> unit
val pp_print_string_command_inst : formatter -> string_command_inst -> unit
val pp_print_string_command_line : formatter -> string_command_line -> unit
val pp_print_string_command_lines : formatter -> string_command_line list -> unit

val pp_print_arg_pipe : formatter -> arg_pipe -> unit
val pp_print_arg_command_inst : formatter -> arg_command_inst -> unit
val pp_print_arg_command_line : formatter -> arg_command_line -> unit
val pp_print_arg_command_lines : formatter -> arg_command_line list -> unit

(************************************************************************
 * For squashing (producing digests).
 *)
val squash_prim_fun : prim_fun -> var
val squash_object : obj -> value SymbolTable.t

(************************************************************************
 * Exceptions.
 *)

(*
 * Errors.
 *)
type omake_error =
   SyntaxError       of string
 | StringError       of string
 | StringStringError of string * string
 | StringDirError    of string * Dir.t
 | StringNodeError   of string * Node.t
 | StringVarError    of string * var
 | StringIntError    of string * int
 | StringMethodError of string * var list
 | StringValueError  of string * value
 | StringTargetError of string * target
 | LazyError         of (formatter -> unit)
 | UnboundVar        of var
 | UnboundFun        of var
 | UnboundMethod     of var list
 | ArityMismatch     of arity * int
 | NotImplemented    of string
 | UnboundKey        of string
 | UnboundValue      of value
 | NullCommand

(*
 * General exception includes debugging info.
 *)
exception OmakeException    of pos * omake_error
exception UncaughtException of pos * exn
exception RaiseException    of pos * obj
exception ExitException     of pos * int
exception Return            of loc * value

(*
 * Omake's internal version of the Invalid_argument
 *)
exception OmakeFatal of string

(*
 * Module for creating positions.
 * You have to specify the name of the module
 * where the exception are being created: use
 * MakePos in each file where Name.name is set
 * to the name of the module.
 *)
module type PosSig =
sig
   val loc_exp_pos    : loc -> pos
   val loc_pos        : loc -> pos -> pos

   val ast_exp_pos    : Omake_ast.exp -> pos
   val ir_exp_pos     : Omake_ir.exp -> pos
   val var_exp_pos    : var -> pos
   val string_exp_pos : string -> pos
   val value_exp_pos  : value -> pos

   val string_pos     : string -> pos -> pos
   val pos_pos        : pos -> pos -> pos
   val int_pos        : int -> pos -> pos
   val var_pos        : var -> pos -> pos
   val error_pos      : omake_error -> pos -> pos

   val del_pos        : (formatter -> unit) -> loc -> pos
   val del_exp_pos    : (formatter -> unit) -> pos -> pos

   (* Utilities *)
   val loc_of_pos     : pos -> loc
   val pp_print_pos   : formatter -> pos -> unit
end

module MakePos (Name : sig val name : string end) : PosSig

(*
 * Exception printing.
 *)
val pp_print_exn : formatter -> omake_error -> unit

(*
 * For debugging.
 *)
val pp_print_env            : formatter -> env -> unit
val pp_print_value          : formatter -> value -> unit
val pp_print_value_list     : formatter -> value list -> unit
val pp_print_explicit_rules : formatter -> venv -> unit
val pp_print_export         : formatter -> export -> unit

(*
 * Static values.
 *)
val debug_db : bool ref

(*
 * Static loading.
 *)
module Static :
sig
   type t

   (*
    * Open a file.  The Node.t is the name of the _source_ file,
    * not the .omc file.  We'll figure out where the .omc file
    * goes on our own.  Raises Not_found if the source file
    * can't be found.
    *)
   val create     : venv -> Node.t -> t
   val close      : t -> unit

   (*
    * Unfortunately, the IR type is delayed because it
    * has type (Omake_ir_ast.senv * Omake_ir.ir), and
    * Omake_ir_ast depends on this file.
    *)

   (*
    * Add the three kinds of entries.
    *)
   val add_ir     : t -> ir -> unit
   val add_object : t -> obj -> unit
   val add_values : t -> obj -> unit

   (*
    * Fetch the three kinds of entries.
    *)
   val find_ir     : t -> ir
   val find_object : t -> obj
   val find_values : t -> obj
end;;

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
