(*
 * The environment for evaluating programs.
 *
 * Scoping is primarily dynamic, but there are issues.
 * Consider the following code.
 *
 *    F(x) =
 *       A = $(x)
 *       G() =
 *          println(A=$(A))
 *       return($(G))
 *    G = $(F 1)
 *    G()
 *
 * With pure dynamic scope, the variable A is unbound in the
 * function call.
 *
 * The same problem occurs with nested objects.
 * Consider the following program.
 *
 *   A. =
 *      X = 1
 *      B. =
 *         F() =
 *            println(X=$(X))
 *
 * The inner object B refers to the variable X defined in
 * the outer object A.
 *
 * We expect the following code to print X=17:
 *
 *    X = 17
 *    A.B.F()
 *
 * What about calling F without defining a value for X?
 *
 *    A.B.F()
 *
 * There are two choices here:
 *    1. X is undefined
 *    2. X gets the value from A (so the program prints X=1)
 *
 * In general we address this by using both static and
 * dynamic scope.  Dynamic scoping takes precedence,
 * but if a dynamic lookup fails, fall back to the
 * the static scope.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2007 Mojave Group, Caltech
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
 * Modified by: Aleksey Nogin @email{nogin@cs.caltech.edu}
 * @end[license]
 *)
open Lm_printf

open Lm_debug
open Lm_symbol
open Lm_location
open Lm_string_set
open Lm_string_util

open Omake_ir
open Omake_util
open Omake_wild
open Omake_node
open Omake_exec
open Omake_state
open Omake_lexer
open Omake_parser
open Omake_symbol
open Omake_ir_print
open Omake_node_sig
open Omake_print_util
open Omake_shell_type
open Omake_command_type
open Omake_options
open Omake_ir_free_vars

(*
 * Debugging.
 *)
let debug_scanner =
   create_debug (**)
      { debug_name = "scanner";
        debug_description = "Display debugging information for scanner selection";
        debug_value = false
      }

(*
 * Debugging.
 *)
let debug_implicit =
   create_debug (**)
      { debug_name = "implicit";
        debug_description = "Display debugging information for implicit rule selection";
        debug_value = false
      }

(*
 * Debug file database (.omc files).
 *)
let debug_db =
   create_debug (**)
      { debug_name = "debug-db";
        debug_description = "Debug the files";
        debug_value = false
      }

(*
 * Environment for parsing AST files.
 *)
type ir = symbol list * scope_kind SymbolTable.t * Omake_ir.exp

(*
 * A source is either
 *   1. A wild string
 *   2. A node
 *   3. An optional source
 *   4. A squashed source
 *)
type source_core =
   SourceWild of wild_out_patt
 | SourceNode of Node.t

type 'a source = node_kind * 'a

(*
 * A target value that represents a node in a rule.
 *)
type target =
   TargetNode of Node.t
 | TargetString of string

(*
 * IoChannels.
 *)
type channel_mode = Lm_channel.mode =
   InChannel
 | OutChannel
 | InOutChannel

type channel_data =
   ChannelDelayed
 | ChannelClosed
 | ChannelValue of Lm_channel.t

type prim_channel =
   { channel_id           : int;
     mutable channel_data : channel_data
   }

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
 * Possible values.
 * Someday we may want to include rules and functions.
 * For the function, the obj is the static scope.
 *)
type value =
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

and value_other =
   ValLexer       of Lexer.t
 | ValParser      of Parser.t
 | ValLocation    of loc
 | ValPosition    of pos
 | ValExitCode    of int

(*
 * Export kinds.
 *    ExportFile: this is the result of file evaluation
 *    ExportList: export just the listed items (variable values, implicit rules/dependencies, phony target set)
 *    ExportAll: export the entire environment.
 *    ExportValue v: just like ExportAll, but return the value too
 *)
and export =
   ExportFile
 | ExportAll
 | ExportDir
 | ExportList of export_elt list
 | ExportValue of value

and export_elt =
   ExportSymbol of symbol
 | ExportRules
 | ExportPhonies

(*
 * Primitives are option refs.
 * We do this so that we can marshal these values.
 * Just before marshaling, all the options are set to None.
 *)
and prim_fun_data = (venv -> pos -> loc -> value list -> value) option

and prim_fun =
   { fun_id           : symbol;
     mutable fun_data : prim_fun_data
   }

(*
 * An object is just an environment.
 *)
and obj = value SymbolTable.t
and env = value SymbolTable.t
and map = (value, value) Lm_map.tree

(*
 * Command lists have source arguments.
 *)
and command =
   CommandSection of value * free_vars * exp
 | CommandValue of loc * value

and command_info =
   { command_env     : venv;
     command_sources : Node.t list;
     command_values  : value list;
     command_body    : command list
   }

(*
 * An implicit rule with a body.
 *
 * In an implicit rule, we compile the targets/sources
 * to wild patterns.
 *)
and irule =
   { irule_loc        : loc;
     irule_multiple   : rule_multiple;
     irule_patterns   : wild_in_patt list;
     irule_locks      : source_core source list;
     irule_sources    : source_core source list;
     irule_scanners   : source_core source list;
     irule_values     : value list;
     irule_body       : command list
   }

(*
 * An implicit dependency.  There is no body, but
 * it may have value dependencies.
 *)
and inrule =
   { inrule_loc        : loc;
     inrule_multiple   : rule_multiple;
     inrule_patterns   : wild_in_patt list;
     inrule_locks      : source_core source list;
     inrule_sources    : source_core source list;
     inrule_scanners   : source_core source list;
     inrule_values     : value list
   }

(*
 * Explicit rules.
 *)
and erule =
   { rule_loc          : loc;
     rule_env          : venv;
     rule_target       : Node.t;
     rule_effects      : NodeSet.t;
     rule_locks        : NodeSet.t;
     rule_sources      : NodeSet.t;
     rule_scanners     : NodeSet.t;
     rule_match        : string option;
     rule_multiple     : rule_multiple;
     rule_commands     : command_info list
   }

(*
 * A listing of all the explicit rules.
 *
 *    explicit_targets     : the collapsed rules for each explicit target
 *    explicit_deps        : the table of explicit rules that are just dependencies
 *    explicit_rules       : the table of all individual explicit rules
 *    explicit_directories : the environment for each directory in the project
 *)
and erule_info =
   { explicit_targets         : erule NodeTable.t;
     explicit_deps            : (NodeSet.t * NodeSet.t * NodeSet.t) NodeTable.t;   (* locks, sources, scanners *)
     explicit_rules           : erule NodeMTable.t;
     explicit_directories     : venv DirTable.t
   }

(*
 * An ordering rule.
 * For now, this just defines an extra dependency
 * of the form:  patt1 -> patt2
 * This means that if a file depends on patt1,
 * then it also depends on patt2.
 *)
and orule =
   { orule_loc      : loc;
     orule_name     : symbol;
     orule_pattern  : wild_in_patt;
     orule_sources  : source_core list
   }

and ordering_info = orule list

(*
 * The environment contains three scopes:
 *    1. The dynamic scope
 *    2. The current object
 *    3. The static scope
 * Lookup occurs in that order, unless the variables
 * have been defined otherwise.
 *
 * Each function has its own static scope.
 * The dynamic scope comes from the caller.
 *)
and venv =
   { venv_dynamic        : env;
     venv_this           : obj;
     venv_static         : env;
     venv_inner          : venv_inner
   }

and venv_inner =
   { venv_environ        : string SymbolTable.t;
     venv_dir            : Dir.t;
     venv_phony          : NodeSet.t;
     venv_implicit_deps  : inrule list;
     venv_implicit_rules : irule list;
     venv_options        : omake_options;
     venv_globals        : venv_globals;
     venv_mount          : Mount.t;
     venv_included_files : NodeSet.t
   }

and venv_globals =
   { (* Execution service *)
     venv_exec                               : exec;

     (* File cache *)
     venv_cache                              : Omake_cache.t;

     (* Mounting functions *)
     venv_mount_info                         : mount_info;

     (* The set of files we have ever read *)
     mutable venv_files                      : NodeSet.t;

     (* Save the environment for each directory in the project *)
     mutable venv_directories                : venv DirTable.t;
     mutable venv_excluded_directories       : DirSet.t;

     (* All the phony targets we have ever generated *)
     mutable venv_phonies                    : PreNodeSet.t;

     (* Explicit rules are global *)
     mutable venv_explicit_rules             : erule list;
     mutable venv_explicit_targets           : NodeSet.t;
     mutable venv_explicit_new               : erule list;

     (* Ordering rules *)
     mutable venv_ordering_rules             : orule list;
     mutable venv_orders                     : StringSet.t;

     (* Cached values for files *)
     mutable venv_ir_files                   : ir NodeTable.t;
     mutable venv_object_files               : obj NodeTable.t;

     (* Cached values for static sections *)
     mutable venv_static_values              : obj SymbolTable.t NodeTable.t;
     mutable venv_modified_values            : obj SymbolTable.t NodeTable.t;

     (* Cached values for the target_is_buildable function *)
     mutable venv_target_is_buildable        : bool NodeTable.t;
     mutable venv_target_is_buildable_proper : bool NodeTable.t;

     (* The state right after Pervasives is evaluated *)
     mutable venv_pervasives_obj             : obj;
     mutable venv_pervasives_vars            : scope_kind SymbolTable.t
   }

(*
 * Type of execution servers.
 *)
and pid =
   InternalPid of int
 | ExternalPid of int
 | ResultPid of int * value

and exec = (arg_command_line, pid, value) Exec.t

(*
 * Execution service.
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
 * Exceptions.
 *)
and item =
   Symbol        of symbol
 | String        of string
 | AstExp        of Omake_ast.exp
 | IrExp         of Omake_ir.exp
 | Location      of loc
 | Value         of value
 | Error         of omake_error

and pos = item Lm_position.pos

and omake_error =
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
 * Error during translation.
 *)
exception OmakeException    of pos * omake_error
exception UncaughtException of pos * exn
exception RaiseException    of pos * obj
exception ExitException     of pos * int
exception Return            of loc * value
exception Break             of loc * venv

(*
 * Omake's internal version of the Invalid_argument
 *)
exception OmakeFatal of string
exception OmakeFatalErr of pos * omake_error

(*
 * Now the stuff that is really global, not saved in venv.
 *)
module IntCompare =
struct
   type t = int
   let compare = (-)
end;;

module IntTable = Lm_map.LmMake (IntCompare);;

type venv_runtime =
   { mutable venv_channel_index  : int;
     mutable venv_channels       : (prim_channel * channel_data) IntTable.t;
     mutable venv_primitives     : (prim_fun * prim_fun_data) SymbolTable.t
   }

let venv_runtime =
   { venv_channel_index  = 0;
     venv_channels       = IntTable.empty;
     venv_primitives     = SymbolTable.empty
   }

(*
 * Command line parsing.
 *)
type lexer = string -> int -> int -> int option

type tok =
   TokString of value
 | TokToken  of string
 | TokGroup  of tok list

(*
 * Inclusion scope is usually Pervasives,
 * but it may include everything in scope.
 *)
type include_scope =
   IncludePervasives
 | IncludeAll

(************************************************************************
 * Access to the globals.
 * This actually performs some computation in 0.9.9
 *)
let venv_globals venv =
   venv.venv_inner.venv_globals

(************************************************************************
 * Tables of values.
 *)
module ValueCompare =
struct
   type t = value

   (*
    * Check for simple values.
    * Arrays cannot be nested.
    *)
   let check_simple pos v =
      match v with
         ValNone
       | ValInt _
       | ValFloat _
       | ValData _
       | ValNode _
       | ValDir _
       | ValOther (ValLocation _)
       | ValOther (ValExitCode _) ->
            ()
       | _ ->
            raise (OmakeException (pos, StringValueError ("illegal Map key", v)))

   let check pos v =
      (match v with
          ValArray vl ->
             List.iter (check_simple pos) vl
        | _ ->
             check_simple pos v);
      v

   (*
    * Compare two simple values.
    *)
   let tag = function
      ValNone                  -> 0
    | ValInt _                 -> 1
    | ValFloat _               -> 2
    | ValArray _               -> 3
    | ValData _                -> 4
    | ValNode _                -> 5
    | ValDir _                 -> 6
    | ValOther (ValExitCode _) -> 7
    | ValOther (ValLocation _) -> 8
    | _ ->
         raise (Invalid_argument "ValueCompare: value not supported")

   let rec compare v1 v2 =
      match v1, v2 with
         ValNone, ValNone ->
            0
       | ValInt i1, ValInt i2
       | ValOther (ValExitCode i1), ValOther (ValExitCode i2) ->
            if i1 < i2 then
               -1
            else if i1 > i2 then
               1
            else
               0
       | ValFloat x1, ValFloat x2 ->
            if x1 < x2 then
               -1
            else if x1 > x2 then
               1
            else
               0
       | ValArray a1, ValArray a2 ->
            compare_list a1 a2
       | ValData s1, ValData s2 ->
            Pervasives.compare s1 s2
       | ValNode node1, ValNode node2 ->
            Node.compare node1 node2
       | ValDir dir1, ValDir dir2 ->
            Dir.compare dir1 dir2
       | ValOther (ValLocation loc1), ValOther (ValLocation loc2) ->
            Lm_location.compare loc1 loc2
       | _ ->
            tag v1 - tag v2

   and compare_list l1 l2 =
      match l1, l2 with
         v1 :: l1, v2 :: l2 ->
            let cmp = compare v1 v2 in
               if cmp = 0 then
                  compare_list l1 l2
               else
                  cmp
       | [], [] ->
            0
       | [], _ :: _ ->
            -1
       | _ :: _, [] ->
            1
end;;

module ValueTable = Lm_map.LmMakeRec (ValueCompare);;

(*
 * Map functions.
 *)
let check_map_key = ValueCompare.check

let venv_map_empty = ValueTable.empty

let venv_map_add map pos v1 v2 =
   ValueTable.add map (check_map_key pos v1) v2

let venv_map_remove map pos v1 =
   ValueTable.remove map (check_map_key pos v1)

let venv_map_find map pos v =
   try ValueTable.find map (check_map_key pos v) with
      Not_found ->
         raise (OmakeException (pos, UnboundValue v))

let venv_map_mem map pos v =
   ValueTable.mem map (check_map_key pos v)

let venv_map_iter   = ValueTable.iter
let venv_map_map    = ValueTable.mapi
let venv_map_fold   = ValueTable.fold
let venv_map_length = ValueTable.cardinal

(************************************************************************
 * Special symbols.
 *)

let class_sym = Lm_symbol.add "$class"

(*
 * Basic utilities.
 *)
let venv_get_class obj =
   match
      try SymbolTable.find obj class_sym with
         Not_found ->
            ValNone
   with
      ValClass table ->
         table
    | _ ->
         SymbolTable.empty

(************************************************************************
 * Printing.
 *)
let pp_print_pos_ref = ref (fun _ _ -> ())

let pp_print_string_list buf sl =
   List.iter (fun s -> fprintf buf "@ %s" s) sl

let pp_print_node_list buf l =
   List.iter (fun s -> fprintf buf "@ %a" pp_print_node s) l

let pp_print_node_set buf set =
   NodeSet.iter (fun s -> fprintf buf "@ %a" pp_print_node s) set

let pp_print_wild_list buf wl =
   List.iter (fun w -> fprintf buf "@ %a" pp_print_wild_in w) wl

let pp_print_source buf (_, source) =
   match source with
      SourceWild wild ->
         pp_print_wild_out buf wild
    | SourceNode node ->
         pp_print_node buf node

let pp_print_source_list buf sources =
   List.iter (fun source -> fprintf buf "@ %a" pp_print_source source) sources

let pp_print_target buf target =
   match target with
      TargetNode node ->
         pp_print_node buf node
    | TargetString s ->
         pp_print_string buf s

let rec pp_print_value buf v =
   match v with
      ValNone ->
         pp_print_string buf "<none>"
    | ValInt i ->
         fprintf buf "%d : Int" i
    | ValFloat x ->
         fprintf buf "%g : Float" x
    | ValData s ->
         fprintf buf "@[<v 3><data \"%s\"> : String@]" (String.escaped s)
    | ValQuote vl ->
         fprintf buf "@[<v 3><string%a>@ : String@]" pp_print_value_list vl
    | ValString s ->
         fprintf buf "\"%s\" : Sequence" (String.escaped s)
    | ValQuoteString (c, vl) ->
         fprintf buf "@[<v 3><string %c%a%c>@ : String@]" c pp_print_value_list vl c
    | ValSequence [v] ->
         pp_print_value buf v
    | ValSequence vl ->
         fprintf buf "@[<hv 3><sequence%a>@ : Sequence@]" pp_print_value_list vl
    | ValArray vl ->
         fprintf buf "@[<v 3><array%a>@ : Array@]" pp_print_value_list vl
    | ValApply (_, scope, f, args) ->
         fprintf buf "@[<hv 3>$(apply %a%a%a)@]" (**)
            pp_print_scope_kind scope
            pp_print_symbol f
            pp_print_value_list args
    | ValSuperApply (_, scope, super, f, args) ->
         fprintf buf "@[<hv 3>$(apply %a%a::%a%a)@]" (**)
            pp_print_scope_kind scope
            pp_print_symbol super
            pp_print_symbol f
            pp_print_value_list args
    | ValMethodApply (_, scope, vl, args) ->
         fprintf buf "@[<hv 3>$(%a%a%a)@]" (**)
            pp_print_scope_kind scope
            pp_print_method_name vl
            pp_print_value_list args
    | ValImplicit (_, scope, v) ->
         fprintf buf "@[<hv 3>ifdefined(%a%a)@]" (**)
            pp_print_scope_kind scope
            pp_print_symbol v
    | ValFun (arity, _, _, _)
    | ValFunValue (arity, _, _, _) ->
         fprintf buf "<fun %a>" pp_print_arity arity
    | ValPrim (arity, special, _) ->
         if special then
            fprintf buf "<special-function %a>" pp_print_arity arity
         else
            fprintf buf "<prim-function %a>" pp_print_arity arity
    | ValRules rules ->
         fprintf buf "<@[<hv 3>rules:";
         List.iter (fun erule -> fprintf buf "@ %a" pp_print_rule erule) rules;
         fprintf buf "@]>"
    | ValDir dir ->
         fprintf buf "%a : Dir" pp_print_dir dir
    | ValNode node ->
         fprintf buf "%a : File" pp_print_node node
    | ValEnv (_, export) ->
         fprintf buf "@[<hv 3><export@ %a>@]" pp_print_export export
    | ValBody (_, e) ->
         fprintf buf "@[<v 0>%a@ : Body@]" pp_print_exp e
    | ValObject env ->
         pp_print_env buf env
    | ValMap map ->
         fprintf buf "@[<hv 3>map";
         ValueTable.iter (fun v e -> fprintf buf "@ %a@ = %a" pp_print_value v pp_print_value e) map;
         fprintf buf "@]"
    | ValChannel (InChannel, _) ->
         fprintf buf "<channel> : InChannel"
    | ValChannel (OutChannel, _) ->
         fprintf buf "<channel> : OutChannel"
    | ValChannel (InOutChannel, _) ->
         fprintf buf "<channel> : InOutChannel"
    | ValClass c ->
         fprintf buf "@[<hv 3>class";
         SymbolTable.iter (fun v _ ->
               fprintf buf "@ %a" pp_print_symbol v) c;
         fprintf buf "@]"
    | ValCases cases ->
         fprintf buf "@[<hv 3>cases";
         List.iter (fun (v, e1, e2) ->
               fprintf buf "@[<hv 3>%a %a:@ %a@]" pp_print_symbol v pp_print_value e1 pp_print_value e2) cases;
         fprintf buf "@]"
    | ValKey (_, v) ->
         fprintf buf "key $|%s|" v
    | ValOther (ValLexer _) ->
         fprintf buf "<lexer> : Lexer"
    | ValOther (ValParser _) ->
         fprintf buf "<parser> : Parser"
    | ValOther (ValLocation loc) ->
         fprintf buf "<location %a> : Location" pp_print_location loc
    | ValOther (ValPosition pos) ->
         fprintf buf "<position %a> : Position" !pp_print_pos_ref pos
    | ValOther (ValExitCode code) ->
         fprintf buf "<exit-code %d> : Int" code

and pp_print_value_list buf vl =
   List.iter (fun v -> fprintf buf "@ %a" pp_print_value v) vl

and pp_print_env buf env =
   let tags = venv_get_class env in
   let env = SymbolTable.remove env class_sym in
      fprintf buf "@[<v 3>@[<hv 3>class";
      SymbolTable.iter (fun v _ -> fprintf buf "@ %a" pp_print_symbol v) tags;
      fprintf buf "@]";
      SymbolTable.iter (fun v e -> fprintf buf "@ %a = %a" pp_print_symbol v pp_print_value e) env;
      fprintf buf "@]"

and pp_print_command buf command =
   match command with
      CommandSection (arg, fv, e) ->
         fprintf buf "@[<hv 3>section %a@ %a@]" pp_print_value arg pp_print_exp e
    | CommandValue (_, v) ->
         pp_print_value buf v

and pp_print_commands buf commands =
   List.iter (fun command -> fprintf buf "@ %a" pp_print_command command) commands

and pp_print_command_info buf info =
   let { command_env     = venv;
         command_sources = sources;
         command_body    = commands
       } = info
   in
      fprintf buf "@[<hv 0>@[<hv 3>{@ command_dir = %a;@ @[<b 3>command_sources =%a@]@ @[<b 3>command_body =%a@]@]@ }@]" (**)
         pp_print_dir venv.venv_inner.venv_dir
         pp_print_node_list sources
         pp_print_commands commands

and pp_print_command_info_list buf infos =
   List.iter (fun info -> fprintf buf "@ %a" pp_print_command_info info) infos

and pp_print_rule buf erule =
   let { rule_target      = target;
         rule_effects     = effects;
         rule_locks       = locks;
         rule_sources     = sources;
         rule_scanners    = scanners;
         rule_commands    = commands
       } = erule
   in
      fprintf buf "@[<hv 0>@[<hv 3>rule {";
      fprintf buf "@ target = %a" pp_print_node target;
      fprintf buf "@ @[<b 3>effects =%a@]" pp_print_node_set effects;
      fprintf buf "@ @[<b 3>locks =%a@]" pp_print_node_set locks;
      fprintf buf "@ @[<b 3>sources =%a@]" pp_print_node_set sources;
      fprintf buf "@ @[<b 3>scanners =%a@]" pp_print_node_set scanners;
      fprintf buf "@ @[<hv 3>commands =%a@]" pp_print_command_info_list commands;
      fprintf buf "@]@ }@]"

and pp_print_export buf = function
   ExportFile ->
      pp_print_string buf "ExportFile"
 | ExportAll ->
      pp_print_string buf "ExportAll"
 | ExportDir ->
      pp_print_string buf "ExportDir"
 | ExportList vars ->
      fprintf buf "@[<hv 3>ExportSymbols";
      List.iter (fun v -> fprintf buf "@ %a" pp_print_export_elt v) vars;
      fprintf buf "@]"
 | ExportValue v ->
      fprintf buf "@[<hv 3>ExportValue@ %a@]" pp_print_value v

and pp_print_export_elt buf = function
   ExportSymbol sym ->
      pp_print_symbol buf sym
 | ExportRules ->
      pp_print_string buf ".RULE"
 | ExportPhonies ->
      pp_print_string buf ".PHONY"

let pp_print_explicit_rules buf venv =
   fprintf buf "@[<hv 3>Explicit rules:";
   List.iter (fun erule -> fprintf buf "@ %a" pp_print_rule erule) venv.venv_inner.venv_globals.venv_explicit_rules;
   fprintf buf "@]"

(************************************************************************
 * Simplified printing.
 *)
let rec pp_print_simple_value buf v =
   match v with
      ValNone ->
         pp_print_string buf "<none>"
    | ValInt i ->
         pp_print_int buf i
    | ValFloat x ->
         pp_print_float buf x
    | ValData s ->
         Omake_command_type.pp_print_arg buf [ArgData s]
    | ValString s ->
         Omake_command_type.pp_print_arg buf [ArgString s]
    | ValQuote vl ->
         fprintf buf "\"%a\"" pp_print_simple_value_list vl
    | ValQuoteString (c, vl) ->
         fprintf buf "%c%a%c" c pp_print_simple_value_list vl c
    | ValSequence vl ->
         pp_print_simple_value_list buf vl
    | ValArray vl ->
         pp_print_simple_arg_list buf vl
    | ValApply (_, scope, f, args) ->
         fprintf buf "$(%a%a%a)" (**)
            pp_print_scope_kind scope
            pp_print_symbol f
            pp_print_simple_arg_list args
    | ValSuperApply (_, scope, super, f, args) ->
         fprintf buf "$(%a%a::%a%a)" (**)
            pp_print_scope_kind scope
            pp_print_symbol super
            pp_print_symbol f
            pp_print_simple_arg_list args
    | ValMethodApply (_, scope, vl, args) ->
         fprintf buf "$(%a%a%a)" (**)
            pp_print_scope_kind scope
            pp_print_method_name vl
            pp_print_value_list args
    | ValImplicit (_, scope, v) ->
         fprintf buf "$?(%a%a)" (**)
            pp_print_scope_kind scope
            pp_print_symbol v
    | ValFun _
    | ValFunValue _ ->
         pp_print_string buf "<fun>"
    | ValPrim _ ->
         pp_print_string buf "<prim>"
    | ValRules _ ->
         pp_print_string buf "<rules>"
    | ValDir dir ->
         pp_print_dir buf dir
    | ValNode node ->
         pp_print_node buf node
    | ValEnv _ ->
         pp_print_string buf "<export>"
    | ValBody _ ->
         pp_print_string buf "<body>"
    | ValObject _ ->
         pp_print_string buf "<object>"
    | ValMap _ ->
         pp_print_string buf "<map>"
    | ValChannel _ ->
         pp_print_string buf "<channel>"
    | ValClass _ ->
         pp_print_string buf "<class>"
    | ValCases _ ->
         pp_print_string buf "<cases>"
    | ValKey (_, v) ->
         fprintf buf "$|%s|" v
    | ValOther (ValLexer _) ->
         pp_print_string buf "<lexer>"
    | ValOther (ValParser _) ->
         pp_print_string buf "<parser>"
    | ValOther (ValLocation _) ->
         pp_print_string buf "<location>"
    | ValOther (ValPosition pos) ->
         pp_print_string buf "<position>"
    | ValOther (ValExitCode i) ->
         pp_print_int buf i

and pp_print_simple_value_list buf vl =
   List.iter (pp_print_simple_value buf) vl

and pp_print_simple_arg_list buf vl =
   match vl with
      [] ->
         ()
    | [v] ->
         pp_print_simple_value buf v
    | v :: vl ->
         pp_print_simple_value buf v;
         pp_print_char buf ' ';
         pp_print_simple_arg_list buf vl

(************************************************************************
 * Pipeline printing.
 *)

(*
 * Token printing.
 *)
let rec pp_print_tok buf tok =
   match tok with
      TokString v ->
         pp_print_value buf v
    | TokToken s ->
         fprintf buf "$%s" s
    | TokGroup toks ->
         fprintf buf "(%a)" pp_print_tok_list toks

and pp_print_tok_list buf toks =
   match toks with
      [tok] ->
         pp_print_tok buf tok
    | tok :: toks ->
         pp_print_tok buf tok;
         pp_print_char buf ' ';
         pp_print_tok_list buf toks
    | [] ->
         ()

let pp_print_simple_exe buf exe =
   match exe with
      ExeString s ->
         pp_print_string buf s
    | ExeQuote s ->
         fprintf buf "\\%s" s
    | ExeNode node ->
         pp_print_node buf node

(*
 * Pipes based on strings.
 *)
module PrintString =
struct
   type arg_command = string
   type arg_apply   = value
   type arg_other   = string
   type exe         = simple_exe

   let pp_print_arg_command = pp_arg_data_string
   let pp_print_arg_apply   = pp_print_value
   let pp_print_arg_other   = pp_arg_data_string
   let pp_print_exe         = pp_print_simple_exe
end;;

module PrintStringPipe = MakePrintPipe (PrintString);;

module PrintStringv =
struct
   type argv = string_pipe

   let pp_print_argv = PrintStringPipe.pp_print_pipe
end;;

module PrintStringCommand = MakePrintCommand (PrintStringv);;

let pp_print_string_pipe = PrintStringPipe.pp_print_pipe
let pp_print_string_command_inst = PrintStringCommand.pp_print_command_inst
let pp_print_string_command_line = PrintStringCommand.pp_print_command_line
let pp_print_string_command_lines = PrintStringCommand.pp_print_command_lines

(*
 * Pipes based on arguments.
 *)
module PrintArg =
struct
   type arg_command = Omake_command_type.arg
   type arg_apply   = value
   type arg_other   = arg_command
   type exe         = arg_command cmd_exe

   let pp_print_arg_command = Omake_command_type.pp_print_arg
   let pp_print_arg_apply   = pp_print_simple_value
   let pp_print_arg_other   = pp_print_arg_command
   let pp_print_exe buf exe =
      match exe with
         CmdArg arg ->
            pp_print_arg_command buf arg
       | CmdNode node ->
            pp_print_node buf node
end;;

module PrintArgPipe = MakePrintPipe (PrintArg);;

module PrintArgv =
struct
   type argv = arg_pipe

   let pp_print_argv = PrintArgPipe.pp_print_pipe
end;;

module PrintArgCommand = MakePrintCommand (PrintArgv);;

let pp_print_arg_pipe = PrintArgPipe.pp_print_pipe
let pp_print_arg_command_inst = PrintArgCommand.pp_print_command_inst
let pp_print_arg_command_line = PrintArgCommand.pp_print_command_line
let pp_print_arg_command_lines = PrintArgCommand.pp_print_command_lines

(************************************************************************
 * Exceptions
 *)

(*
 * Get the source location for an exception.
 *)
let string_loc = bogus_loc "<Omake_env>"

let rec loc_of_item x =
   match x with
      AstExp e ->
         Omake_ast_util.loc_of_exp e
    | IrExp e ->
         Omake_ir_util.loc_of_exp e
    | Location loc ->
         loc
    | Value _
    | Symbol _
    | String _
    | Error _ ->
         string_loc

(*
 * Value printing.
 *)
let rec pp_print_item buf x =
   match x with
      AstExp e ->
         Omake_ast_print.pp_print_exp buf e

    | IrExp e ->
         Omake_ir_print.pp_print_exp buf e

    | Location _ ->
         ()

    | Symbol v ->
         pp_print_symbol buf v

    | String s ->
         pp_print_string buf s

    | Value v ->
         pp_print_value buf v

    | Error e ->
         pp_print_exn buf e

(*
 * Exception printer.
 *)
and pp_print_exn buf = function
   SyntaxError s ->
      fprintf buf "syntax error: %s" s
 | StringError s ->
      pp_print_string buf s
 | StringIntError (s, i) ->
      fprintf buf "%s: %d" s i
 | StringStringError (s1, s2) ->
      fprintf buf "%s: %s" s1 s2
 | StringVarError (s, v) ->
      fprintf buf "%s: %a" s pp_print_symbol v
 | StringMethodError (s, v) ->
      fprintf buf "%s: %a" s pp_print_method_name v
 | StringDirError (s, n)->
      fprintf buf "%s: %a" s pp_print_dir n
 | StringNodeError (s, n)->
      fprintf buf "%s: %a" s pp_print_node n
 | StringValueError (s, v) ->
      fprintf buf "@[<hv 3>%s:@ %a@]" s pp_print_value v
 | StringTargetError (s, t) ->
      fprintf buf "%s: %a" s pp_print_target t
 | LazyError printer ->
      printer buf
 | UnboundVar v ->
      fprintf buf "unbound variable: %a" pp_print_symbol v
 | UnboundKey v ->
      fprintf buf "unbound key: %s" v
 | UnboundValue v ->
      fprintf buf "unbound value: %a" pp_print_value v
 | UnboundFun v ->
      fprintf buf "unbound function: %a" pp_print_symbol v
 | UnboundMethod vl ->
      fprintf buf "unbound method: %a" pp_print_method_name vl
 | ArityMismatch (len1, len2) ->
      fprintf buf "arity mismatch: expected %a args, got %d" pp_print_arity len1 len2
 | NotImplemented s ->
      fprintf buf "not implemented: %s" s
 | NullCommand ->
      pp_print_string buf "invalid null command"

(************************************************************************
 * Positions.
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

module type NameSig =
sig
   val name : string
end

module MakePos (Name : NameSig) : PosSig =
struct
   module Name' =
   struct
      type t = item

      let name = Name.name

      let loc_of_value = loc_of_item
      let pp_print_value = pp_print_item
   end

   module Pos = Lm_position.MakePos (Name')

   include Pos

   let loc_pos_pos loc pos =
      cons_pos (Location loc) pos

   let ast_exp_pos e    = base_pos (AstExp e)
   let ir_exp_pos e     = base_pos (IrExp e)
   let var_exp_pos v    = base_pos (Symbol v)
   let string_exp_pos s = base_pos (String s)
   let value_exp_pos v  = base_pos (Value v)
   let var_pos          = symbol_pos
   let value_pos v pos  = cons_pos (Value v) pos
   let error_pos e pos  = cons_pos (Error e) pos
end

module Pos = MakePos (struct let name = "Omake_env" end)
open Pos;;

let () = pp_print_pos_ref := pp_print_pos

(************************************************************************
 * Static values.
 *)

(*
 * During marshaling, set all the refs to None.
 *)
let venv_marshal venv f x =
   let { venv_channels = channels;
         venv_primitives = primitives
       } = venv_runtime
   in

   (* Prepare by closing channels, and resetting function data *)
   let prepare () =
      IntTable.iter (fun _ (channel, _) ->
            if channel.channel_id <= 2 then
               channel.channel_data <- ChannelDelayed
            else
               channel.channel_data <- ChannelClosed) channels;
      SymbolTable.iter (fun _ (value, _) ->
            value.fun_data <- None) primitives
   in

   (* Restore by reopening channels and readding function data *)
   let restore () =
      IntTable.iter (fun _ (channel, data) ->
            channel.channel_data <- data) channels;
      SymbolTable.iter (fun _ (value, data) ->
            value.fun_data <- data) primitives
   in
      prepare ();
      try
         let y = f x in
            restore ();
            y
      with
         exn ->
            restore ();
            raise exn

(*
 * Static loading.
 *)
module type StaticSig =
sig
   type in_handle
   type out_handle

   (*
    * Open a file.  The Node.t is the name of the _source_ file,
    * not the .omc file.  We'll figure out where the .omc file
    * goes on our own.  Raises Not_found if the source file
    * can't be found.
    *)
   val create_in    : venv -> Node.t -> in_handle
   val close_in     : in_handle -> unit

   val create_out   : venv -> Node.t -> out_handle
   val recreate_out : in_handle -> out_handle
   val close_out    : out_handle -> unit

   (*
    * Unfortunately, the IR type is delayed because it
    * has type (Omake_ir_ast.senv * Omake_ir.ir), and
    * Omake_ir_ast depends on this file.
    *)

   (*
    * Fetch the three kinds of entries.
    *)
   val find_ir     : in_handle -> ir
   val find_object : in_handle -> obj
   val find_values : in_handle -> obj SymbolTable.t

   (*
    * Add the three kinds of entries.
    *)
   val get_ir      : out_handle -> ir
   val get_object  : out_handle -> obj
   val get_values  : out_handle -> obj SymbolTable.t

   val add_ir      : out_handle -> ir -> unit
   val add_object  : out_handle -> obj -> unit
   val add_values  : out_handle -> obj SymbolTable.t -> unit
end;;

module Static : StaticSig =
struct
   (************************************************************************
    * Types.
    *)

   (*
    * A .omc file.
    *)
   type t =
      { db_file         : Unix.file_descr option;
        db_name         : Node.t;
        db_digest       : string;
        db_env          : venv;
        db_flush_ir     : bool;
        db_flush_static : bool
      }
   type in_handle = t
   type out_handle = t

   (*
    * Tags for the various kinds of entries.
    *)
   let ir_tag      = 0, Lm_db.HostIndependent
   let object_tag  = 1, Lm_db.HostDependent
   let values_tag  = 2, Lm_db.HostDependent

   (************************************************************************
    * Operations.
    *)

   (*
    * Open a file.  The Node.t is the name of the _source_ file,
    * not the .omc file.  We'll figure out where the .omc file
    * goes on our own.
    *)
   let create_mode mode venv source =
      (* Get the source digest *)
      let cache = venv.venv_inner.venv_globals.venv_cache in
      let digest =
         match Omake_cache.stat cache source with
            Some digest ->
               digest
          | None ->
               raise Not_found
      in

      (*
       * Open the result file.  The lock_cache_file function
       * will try to use the target directory first, and
       * fall back to ~/.omake/cache if that is not writable.
       *)
      let source_name = Node.absname source in
      let dir = Filename.dirname source_name in
      let name = Filename.basename source_name in
      let name =
         if Filename.check_suffix name ".om" then
            Filename.chop_suffix name ".om"
         else
            name
      in
      let name = name ^ ".omc" in
      let target_fd =
         try
            let target_name, target_fd = Omake_state.get_cache_file dir name in
               if !debug_db then
                  eprintf "@[<v 3>Omake_db.create:@ %a --> %s@]@." pp_print_node source target_name;
               Unix.set_close_on_exec target_fd;
               Omake_state.lock_file target_fd mode;
               Some target_fd
         with
            Unix.Unix_error _
          | Failure _ ->
               eprintf "@[<v 3>OMake warning: could not create and/or lock a cache file for@ %s@]@." source_name;
               None
      in
         { db_file         = target_fd;
           db_name         = source;
           db_digest       = digest;
           db_env          = venv;
           db_flush_ir     = opt_flush_include venv.venv_inner.venv_options;
           db_flush_static = opt_flush_static venv.venv_inner.venv_options;
         }

   let create_in = create_mode Unix.F_RLOCK
   let create_out = create_mode Unix.F_LOCK

   (*
    * Restart with a write lock.
    *)
   let recreate_out info =
      match info.db_file with
         Some fd ->
            let _ = Unix.lseek fd 0 Unix.SEEK_SET in
               Omake_state.lock_file fd Unix.F_ULOCK;
               Omake_state.lock_file fd Unix.F_LOCK;
               info
       | None ->
            info

   (*
    * Close the file.
    *)
   let close info =
      match info with
         { db_file = Some fd; db_name = name } ->
            if !debug_db then
               eprintf "Omake_db.close: %a@." pp_print_node name;
            Unix.close fd
       | { db_file = None } ->
            ()

   let close_in = close
   let close_out = close

   (*
    * Add the three kinds of entries.
    *)
   let add_ir info ir =
      match info with
         { db_file = Some fd; db_name = name; db_digest = digest; db_env = venv } ->
            if !debug_db then
               eprintf "Omake_db.add_ir: %a@." pp_print_node name;
            venv_marshal venv (fun () ->
                  Lm_db.add fd (Node.absname name) ir_tag Omake_magic.ir_magic digest ir) ()
       | { db_file = None } ->
            ()

   let add_object info obj =
      match info with
         { db_file = Some fd; db_name = name; db_digest = digest; db_env = venv } ->
            if !debug_db then
               eprintf "Omake_db.add_object: %a@." pp_print_node name;
            venv_marshal venv (fun () ->
                  Lm_db.add fd (Node.absname name) object_tag Omake_magic.obj_magic digest obj) ()
       | { db_file = None } ->
            ()

   let add_values info obj =
      match info with
         { db_file = Some fd; db_name = name; db_digest = digest; db_env = venv } ->
            if !debug_db then
               eprintf "Omake_db.add_values: %a@." pp_print_node name;
            venv_marshal venv (fun () ->
                  Lm_db.add fd (Node.absname name) values_tag Omake_magic.obj_magic digest obj) ()
       | { db_file = None } ->
            ()

   (*
    * Fetch the three kinds of entries.
    *)
   let find_ir = function
      { db_file = Some fd; db_name = name; db_digest = digest; db_flush_ir = false } ->
         if !debug_db then
            eprintf "Omake_db.find_ir: finding: %a@." pp_print_node name;
         let ir = Lm_db.find fd (Node.absname name) ir_tag Omake_magic.ir_magic digest in
            if !debug_db then
               eprintf "Omake_db.find_ir: found: %a@." pp_print_node name;
            ir
    | { db_file = None }
    | { db_flush_ir = true } ->
         raise Not_found

   let find_object = function
      { db_file = Some fd; db_name = name; db_digest = digest; db_flush_ir = false; db_flush_static = false } ->
         if !debug_db then
            eprintf "Omake_db.find_object: finding: %a@." pp_print_node name;
         let obj = Lm_db.find fd (Node.absname name) object_tag Omake_magic.obj_magic digest in
            if !debug_db then
               eprintf "Omake_db.find_object: found: %a@." pp_print_node name;
            obj
    | { db_file = None }
    | { db_flush_ir = true }
    | { db_flush_static = true } ->
         raise Not_found

   let find_values = function
      { db_file = Some fd; db_name = name; db_digest = digest; db_flush_ir = false; db_flush_static = false } ->
         if !debug_db then
            eprintf "Omake_db.find_values: finding: %a@." pp_print_node name;
         let obj = Lm_db.find fd (Node.absname name) values_tag Omake_magic.obj_magic digest in
            if !debug_db then
               eprintf "Omake_db.find_values: found: %a@." pp_print_node name;
            obj
    | { db_file = None }
    | { db_flush_ir = true }
    | { db_flush_static = true } ->
         raise Not_found

   let get_ir     = find_ir
   let get_object = find_object
   let get_values = find_values
end;;

(************************************************************************
 * Utilities.
 *)

(*
 * Don't make command info if there are no commands.
 *)
let make_command_info venv sources values body =
   match values, body with
      [], [] ->
         []
    | _ ->
         [{ command_env     = venv;
            command_sources = sources;
            command_values  = values;
            command_body    = body
          }]

(*
 * Check if the commands are trivial.
 *)
let commands_are_trivial commands =
   List.for_all (fun command -> command.command_body = []) commands

(*
 * Multiple flags.
 *)
let is_multiple_rule = function
   RuleMultiple
 | RuleScannerMultiple ->
      true
 | RuleSingle
 | RuleScannerSingle ->
      false

let is_scanner_rule = function
   RuleScannerSingle
 | RuleScannerMultiple ->
      true
 | RuleSingle
 | RuleMultiple ->
      false

let rule_kind = function
   RuleScannerSingle
 | RuleScannerMultiple ->
      RuleScanner
 | RuleSingle
 | RuleMultiple ->
      RuleNormal

(************************************************************************
 * Channels.
 *)

(*
 * Add a channel slot.
 *)
let venv_add_channel venv data =
   let { venv_channel_index = index;
         venv_channels      = channels
       } = venv_runtime
   in
   let data =
      Lm_channel.set_id data index;
      ChannelValue data
   in
   let channel =
      { channel_id = index;
        channel_data = data
      }
   in
      venv_runtime.venv_channels <- IntTable.add channels index (channel, data);
      venv_runtime.venv_channel_index <- succ index;
      channel

let add_channel file kind mode binary fd =
   Lm_channel.create file kind mode binary (Some fd)

let venv_stdin  = venv_add_channel () (add_channel "<stdin>"  Lm_channel.PipeChannel Lm_channel.InChannel  false Unix.stdin)
let venv_stdout = venv_add_channel () (add_channel "<stdout>" Lm_channel.PipeChannel Lm_channel.OutChannel false Unix.stdout)
let venv_stderr = venv_add_channel () (add_channel "<stderr>" Lm_channel.PipeChannel Lm_channel.OutChannel false Unix.stderr)

(*
 * A formatting channel.
 *)
let venv_add_formatter_channel venv fmt =
   let { venv_channel_index = index;
         venv_channels      = channels
       } = venv_runtime
   in
   let fd = Lm_channel.create "formatter" Lm_channel.FileChannel Lm_channel.OutChannel true None in
   let () = Lm_channel.set_id fd index in
   let data = ChannelValue fd in
   let channel =
      { channel_id = index;
        channel_data = data
      }
   in
   let reader s off len =
      raise (Unix.Unix_error (Unix.EINVAL, "formatter-channel", ""))
   in
   let writer s off len =
      Format.pp_print_string fmt (String.sub s off len);
      len
   in
      Lm_channel.set_io_functions fd reader writer;
      venv_runtime.venv_channels <- IntTable.add channels index (channel, data);
      venv_runtime.venv_channel_index <- succ index;
      channel

(*
 * Get the channel.
 *)
let venv_channel_data channel =
   match channel with
      { channel_data = ChannelDelayed; channel_id = id } ->
         (try
             let _, data = IntTable.find venv_runtime.venv_channels id in
                channel.channel_data <- data;
                data
          with
             Not_found ->
                (* This should never happen, because we close channels on marshaling *)
                raise (Invalid_argument "expand_channel"))
    | { channel_data = data } ->
         data

(*
 * When a channel is closed, close the buffers too.
 *)
let venv_close_channel venv pos channel =
   try
      let () =
         match venv_channel_data channel with
            ChannelClosed ->
               ()
          | ChannelValue channel ->
               Lm_channel.close channel
          | ChannelDelayed ->
               raise (Invalid_argument "venv_close_channel")
      in
         channel.channel_data <- ChannelClosed;
         venv_runtime.venv_channels <- IntTable.remove venv_runtime.venv_channels channel.channel_id
   with
      Not_found ->
         (* Fail silently *)
         ()

(*
 * Get the channel.
 *)
let venv_find_channel venv pos channel =
   let pos = string_pos "venv_find_in_channel" pos in
      match venv_channel_data channel with
         ChannelClosed ->
            raise (OmakeException (pos, StringError "channel is closed"))
       | ChannelValue channel ->
            channel
       | ChannelDelayed ->
            raise (Invalid_argument "venv_find_in_channel")

(*
 * Finding by identifiers.
 *)
let venv_find_channel_id venv pos id =
   let channel, _ =
      try IntTable.find venv_runtime.venv_channels id with
         Not_found ->
            raise (OmakeException (pos, StringIntError ("channel is closed", id)))
   in
      channel

(************************************************************************
 * Primitive values.
 *)

(*
 * Allocate a function primitive.
 *)
let venv_add_prim_fun venv name f =
   let data = Some f in
   let value = { fun_id = name; fun_data = data } in
      venv_runtime.venv_primitives <- SymbolTable.add venv_runtime.venv_primitives name (value, data);
      value

(*
 * Look up the primitive value if we haven't seen it already.
 *)
let venv_apply_prim_fun p venv pos loc args =
   match p with
      { fun_data = Some f } ->
         f venv pos loc args
    | { fun_id = name; fun_data = None } ->
         let opt =
            try
               let _, data = SymbolTable.find venv_runtime.venv_primitives name in
                  p.fun_data <- data;
                  data
            with
               Not_found ->
                  None
         in
            match opt with
               Some f ->
                  f venv pos loc args
             | None ->
                  raise (OmakeException (loc_pos loc pos, UnboundVar name))

(************************************************************************
 * Methods and objects.
 *)

(*
 * Default empty object.
 *)
let venv_empty_object = SymbolTable.empty

(*
 * Add a class to an object.
 *)
let venv_add_class obj v =
   let table = venv_get_class obj in
   let table = SymbolTable.add table v obj in
      SymbolTable.add obj class_sym (ValClass table)

(*
 * The current object is always in the venv_this field.
 *)
let venv_this venv =
   venv.venv_this

let venv_this_object venv scope =
   match scope with
      ScopeProtected ->
         venv.venv_this
    | ScopeGlobal
    | ScopeDynamic ->
         venv.venv_dynamic
    | ScopePrivate ->
         venv.venv_static

let venv_current_object venv classnames =
   let obj = venv.venv_this in
      if classnames = [] then
         obj
      else
         let table = venv_get_class obj in
         let table = List.fold_left (fun table v -> SymbolTable.add table v obj) table classnames in
            SymbolTable.add obj class_sym (ValClass table)

let venv_current_objects venv scope =
   let { venv_this = this;
         venv_dynamic = dynamic;
         venv_static = static
       } = venv
   in
      match scope with
         ScopePrivate ->
            [static]
       | ScopeDynamic ->
            [dynamic]
       | ScopeProtected ->
            [this; dynamic; static]
       | ScopeGlobal ->
            [dynamic; this; static]

(*
 * Execute a method in an object.
 * If we are currently in the outermost object,
 * push the dynamic scope.
 *)
let venv_with_object venv this =
   { venv with venv_this = this }

(*
 * Define a new object.
 *)
let venv_define_object venv =
   venv_with_object venv SymbolTable.empty

(*
 * Object field lookup.
 *)
let venv_find_field_exn = SymbolTable.find
let venv_add_field      = SymbolTable.add
let venv_object_mem     = SymbolTable.mem
let venv_object_length  = SymbolTable.cardinal
let venv_object_fold    = SymbolTable.fold

let venv_find_field obj pos v =
   try SymbolTable.find obj v with
      Not_found ->
         let pos = string_pos "venv_find_field" pos in
            raise (OmakeException (pos, UnboundVar v))

(*
 * Add the class to the current object.
 *)
let venv_instanceof obj s =
   SymbolTable.mem (venv_get_class obj) s

(*
 * Include the fields in the given class.
 * Be careful to merge classnames.
 *)
let venv_include_object_aux obj1 obj2 =
   let table1 = venv_get_class obj1 in
   let table2 = venv_get_class obj2 in
   let table = SymbolTable.fold SymbolTable.add table1 table2 in
   let obj1 = SymbolTable.fold SymbolTable.add obj1 obj2 in
      SymbolTable.add obj1 class_sym (ValClass table)

let venv_include_object venv obj2 =
   let obj = venv_include_object_aux venv.venv_this obj2 in
      { venv with venv_this = obj }

let venv_flatten_object venv obj2 =
   let obj = venv_include_object_aux venv.venv_dynamic obj2 in
      { venv with venv_dynamic = obj }

(*
 * Get a parent class.
 *)
let venv_find_super venv pos loc v =
   let table = venv_get_class venv.venv_this in
      try SymbolTable.find table v with
         Not_found ->
            let pos = string_pos "venv_find_super" (loc_pos loc pos) in
               raise (OmakeException (pos, StringVarError ("not a super-class", v)))

(*
 * Function scoping.
 *)
let venv_empty_env =
   SymbolTable.empty

let venv_get_env venv =
   venv.venv_static

let venv_with_env venv env =
   { venv with venv_static = env }

(*
 * Cached object files.
 *)
let venv_find_ir_file_exn venv node =
   NodeTable.find venv.venv_inner.venv_globals.venv_ir_files node

let venv_add_ir_file venv node obj =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_ir_files <- NodeTable.add globals.venv_ir_files node obj

let venv_find_object_file_exn venv node =
   NodeTable.find venv.venv_inner.venv_globals.venv_object_files node

let venv_add_object_file venv node obj =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_object_files <- NodeTable.add globals.venv_object_files node obj

(************************************************************************
 * Static objects.
 *)

(*
 * Static values.  Load the values from the file
 * if necessary.  Raises Not_found if the object has not already
 * been loaded.
 *)
let venv_find_static_object venv node v =
   let globals = venv.venv_inner.venv_globals in
   let static = globals.venv_static_values in
   let table =
      try NodeTable.find static node with
         Not_found ->
            (* Load it from the file *)
            let fd = Static.create_in venv node in
            let table =
               try Static.find_values fd with
                  Not_found ->
                     Static.close_in fd;
                     raise Not_found
            in
               Static.close_in fd;
               globals.venv_static_values <- NodeTable.add static node table;
               table
   in
      SymbolTable.find table v

(*
 * Define a static var.
 * Save the object on the modified list so it will get
 * written back to the file.
 *)
let venv_add_static_object venv node key obj =
   let globals = venv.venv_inner.venv_globals in
   let { venv_static_values = static;
         venv_modified_values = modified
       } = globals
   in
   let table =
      try NodeTable.find static node with
         Not_found ->
            SymbolTable.empty
   in
   let table = SymbolTable.add table key obj in
      globals.venv_static_values <- NodeTable.add static node table;
      globals.venv_modified_values <- NodeTable.add modified node table

(*
 * Inline the static variables into the current environment.
 *)
let venv_include_static_object venv obj =
   let { venv_dynamic = dynamic } = venv in
   let dynamic = SymbolTable.fold SymbolTable.add dynamic obj in
      { venv with venv_dynamic = dynamic }

(*
 * Save the modified values.
 *)
let venv_save_static_values venv =
   let globals = venv.venv_inner.venv_globals in
      NodeTable.iter (fun node table ->
            let fd =
               try Some (Static.create_out venv node) with
                  Not_found ->
                     None
            in
               match fd with
                  Some fd ->
                     Static.add_values fd table;
                     Static.close_out fd
                | None ->
                     ()) globals.venv_modified_values;
      globals.venv_modified_values <- NodeTable.empty

(************************************************************************
 * Environment.
 *)

(*
 * Convert a filename to a node.
 *)
let venv_intern venv phony_flag name =
   let { venv_mount   = mount;
         venv_dir     = dir
       } = venv.venv_inner
   in
   let globals = venv_globals venv in
   let { venv_phonies = phonies;
         venv_mount_info = mount_info
       } = globals
   in
      create_node_or_phony phonies mount_info mount phony_flag dir name

let venv_intern_target venv phony_flag target =
   match target with
      TargetNode node -> node
    | TargetString name -> venv_intern venv phony_flag name

let venv_intern_cd venv phony_flag dir name =
   let mount = venv.venv_inner.venv_mount in
   let globals = venv_globals venv in
   let { venv_phonies = phonies;
         venv_mount_info = mount_info
       } = globals
   in
      create_node_or_phony phonies mount_info mount phony_flag dir name

let venv_intern_rule_target venv multiple name =
   let node =
      match name with
         TargetNode node ->
            node
       | TargetString name ->
            venv_intern venv PhonyOK name
   in
      match multiple with
         RuleScannerSingle
       | RuleScannerMultiple ->
            Node.create_escape NodeScanner node
       | RuleSingle
       | RuleMultiple ->
            node

let venv_intern_dir venv name =
   Dir.chdir venv.venv_inner.venv_dir name

let venv_intern_list venv names =
   List.map (venv_intern venv) names

let node_set_of_list nodes =
   List.fold_left NodeSet.add NodeSet.empty nodes

let node_set_add_names venv phony_flag nodes names =
   List.fold_left (fun nodes name ->
         NodeSet.add nodes (venv_intern venv phony_flag name)) nodes names

let node_set_of_names venv phony_flag names =
   node_set_add_names venv phony_flag NodeSet.empty names

(*
 * Convert back to a string.
 *)
let venv_dirname venv dir =
   if opt_absname venv.venv_inner.venv_options then
      Dir.absname dir
   else
      Dir.name venv.venv_inner.venv_dir dir

let venv_nodename venv dir =
   if opt_absname venv.venv_inner.venv_options then
      Node.absname dir
   else
      Node.name venv.venv_inner.venv_dir dir

(*
 * Add a mount point.
 *)
let venv_mount venv options src dst =
   let inner = venv.venv_inner in
   let mount = Mount.mount inner.venv_mount options src dst in
   let inner = { inner with venv_mount = mount } in
      { venv with venv_inner = inner }

(*
 * A target is wild if it is a string with a wild char.
 *)
let target_is_wild target =
   match target with
      TargetNode _ ->
         false
    | TargetString s ->
         is_wild s

let string_of_target venv target =
   match target with
      TargetString s ->
         s
    | TargetNode node ->
         venv_nodename venv node

(*
 * Compile a wild pattern.
 * It is an error if it isn't wild.
 *)
let compile_wild_pattern venv pos loc target =
   match target with
      TargetString s when is_wild s ->
         if Lm_string_util.contains_any s Lm_filename_util.separators then
            raise (OmakeException (loc_pos loc pos, StringStringError ("filename pattern is a path", s)));
         wild_compile_in s
    | _ ->
         raise (OmakeException (loc_pos loc pos, StringTargetError ("patterns must be wildcards", target)))

(*
 * Compile a source.
 *)
let compile_source_core venv s =
   match s with
      TargetNode node ->
         SourceNode node
    | TargetString s ->
         if is_wild s then
            SourceWild (wild_compile_out s)
         else
            SourceNode (venv_intern venv PhonyOK s)

let compile_source venv (kind, s) =
   kind, compile_source_core venv s

(*
 * Perform a wild substitution on a source.
 *)
let subst_source_core venv dir subst source =
   match source with
      SourceWild wild ->
         let s = wild_subst subst wild in
            venv_intern_cd venv PhonyOK dir s
    | SourceNode node ->
         node

let subst_source venv dir subst (kind, source) =
   Node.create_escape kind (subst_source_core venv dir subst source)

(*
 * No wildcard matching.
 *)
let intern_source venv (kind, source) =
   let source =
      match source with
         TargetNode node ->
            node
       | TargetString name ->
            venv_intern venv PhonyOK name
   in
      Node.create_escape kind source

(*
 * For variables, try to look them up as 0-arity functions first.
 *)
let venv_find_var_private venv v =
   SymbolTable.find venv.venv_static v

let venv_find_var_dynamic venv v =
   SymbolTable.find venv.venv_dynamic v

let venv_find_var_protected venv v =
   try SymbolTable.find venv.venv_this v with
      Not_found ->
         try SymbolTable.find venv.venv_dynamic v with
            Not_found ->
               try SymbolTable.find venv.venv_static v with
                  Not_found ->
                     ValString (SymbolTable.find venv.venv_inner.venv_environ v)

let venv_find_var_global venv v =
   try SymbolTable.find venv.venv_dynamic v with
      Not_found ->
         try SymbolTable.find venv.venv_this v with
            Not_found ->
               try SymbolTable.find venv.venv_static v with
                  Not_found ->
                     ValString (SymbolTable.find venv.venv_inner.venv_environ v)

let venv_find_var_exn venv scope v =
   match scope with
      ScopePrivate ->
         venv_find_var_private venv v
    | ScopeDynamic ->
         venv_find_var_dynamic venv v
    | ScopeProtected ->
         venv_find_var_protected venv v
    | ScopeGlobal ->
         venv_find_var_global venv v

let venv_get_var venv scope pos v =
   try venv_find_var_exn venv scope v with
      Not_found ->
         let pos = string_pos "venv_get_var" pos in
            raise (OmakeException (pos, UnboundVar v))

let venv_find_var venv scope pos loc v =
   try venv_find_var_exn venv scope v with
      Not_found ->
         let pos = string_pos "venv_find_var" (loc_pos loc pos) in
            raise (OmakeException (loc_pos loc pos, UnboundVar v))

let venv_defined venv scope v =
   List.exists (fun env -> SymbolTable.mem env v) (venv_current_objects venv scope)

let venv_find_object_or_empty venv scope symbol =
   try
      match venv_find_var_exn venv scope symbol with
         ValObject obj ->
            obj
       | _ ->
            venv_empty_object
   with
      Not_found ->
         venv_empty_object

(*
 * Adding to variable environment.
 * Add to the current object and the static scope.
 *)
let venv_add_var venv scope pos v s =
   let { venv_this = this;
         venv_static = static;
         venv_dynamic = dynamic
       } = venv
   in
      match scope with
         ScopePrivate ->
            { venv with venv_static  = SymbolTable.add static v s }
       | ScopeDynamic ->
            { venv with venv_dynamic = SymbolTable.add dynamic v s }
       | ScopeProtected ->
            { venv with venv_this    = SymbolTable.add this v s;
                        venv_static  = SymbolTable.add static v s
            }
       | ScopeGlobal ->
            { venv with venv_dynamic = SymbolTable.add dynamic v s;
                        venv_static  = SymbolTable.add static v s
            }

let venv_add_var_tmp venv v s =
   { venv with venv_dynamic = SymbolTable.add venv.venv_dynamic v s }

let venv_add_var_dynamic venv v s =
   { venv with venv_dynamic = SymbolTable.add venv.venv_dynamic v s;
               venv_static  = SymbolTable.add venv.venv_static v s
   }

let venv_add_assoc venv values =
   List.fold_left (fun venv (v, s) ->
            venv_add_var_dynamic venv v (ValString s)) venv values

let venv_add_args venv pos loc env vl sl =
   let len1 = List.length vl in
   let len2 = List.length sl in
   let _ =
      if len1 <> len2 then
         raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact len1, len2)))
   in
      { venv with venv_this = List.fold_left2 SymbolTable.add venv.venv_this vl sl;
                  venv_static = List.fold_left2 SymbolTable.add env vl sl
      }

(*
 * !!!HACK!!!
 *
 * This is here only to get around the problem with
 * not finding binding occurrences in argument lists.
 * This should go away!
 *
 * Sat Jul  2 07:16:09 PDT 2005
 *)
let venv_add_args_hack venv pos loc env vl sl =
   let len1 = List.length vl in
   let len2 = List.length sl in
   let _ =
      if len1 <> len2 then
         raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact len1, len2)))
   in
   let { venv_this = this;
         venv_dynamic = dynamic
       } = venv
   in
      { venv with venv_this = List.fold_left2 SymbolTable.add this vl sl;
                  venv_static = List.fold_left2 SymbolTable.add env vl sl;
                  venv_dynamic = List.fold_left2 SymbolTable.add dynamic vl sl
      }

(*
 * The system environment.
 *)
let venv_environment venv =
   venv.venv_inner.venv_environ

let venv_getenv venv v =
   SymbolTable.find venv.venv_inner.venv_environ v

let venv_setenv venv v x =
   { venv with venv_inner = { venv.venv_inner with venv_environ = SymbolTable.add venv.venv_inner.venv_environ v x } }

let venv_unsetenv venv v =
   { venv with venv_inner = { venv.venv_inner with venv_environ = SymbolTable.remove venv.venv_inner.venv_environ v } }

let venv_defined_env venv v =
   SymbolTable.mem venv.venv_inner.venv_environ v

(*
 * Set the options.
 *)
let venv_set_options venv loc pos options =
   let argv = Array.of_list ("omake" :: Lm_string_util.tokens_std options) in
   let add_unknown _ s =
      raise (OmakeException (loc_pos loc pos, StringStringError ("unknown option", s)))
   in
   let options_spec =
      Lm_arg.StrictOptions, (**)
         ["Make options", options_spec;
          "Output options", output_spec]
   in
   let options =
      try Lm_arg.fold_argv argv options_spec venv.venv_inner.venv_options add_unknown "Generic system builder" with
          Lm_arg.BogusArg s ->
            raise (OmakeException (loc_pos loc pos, StringError s))
   in
      { venv with venv_inner = { venv.venv_inner with venv_options = options } }

let venv_options venv =
   venv.venv_inner.venv_options

(************************************************************************
 * Target cache.
 *
 * To keep this up-to-date, entries are added for explicit targets,
 * and the cache is flushed whenever an implicit rule is added.
 *)
let venv_find_target_is_buildable_exn venv target =
   NodeTable.find venv.venv_inner.venv_globals.venv_target_is_buildable target

let venv_find_target_is_buildable_proper_exn venv target =
   NodeTable.find venv.venv_inner.venv_globals.venv_target_is_buildable_proper target

let venv_add_target_is_buildable venv target flag =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_target_is_buildable <- NodeTable.add globals.venv_target_is_buildable target flag

let venv_add_target_is_buildable_proper venv target flag =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_target_is_buildable_proper <- NodeTable.add globals.venv_target_is_buildable_proper target flag

let venv_add_explicit_targets venv rules =
   let globals = venv.venv_inner.venv_globals in
   let { venv_target_is_buildable = cache;
         venv_target_is_buildable_proper = cache_proper
       } = globals
   in
   let cache =
      List.fold_left (fun cache erule ->
            NodeTable.add cache erule.rule_target true) cache rules
   in
   let cache_proper =
      List.fold_left (fun cache erule ->
            NodeTable.add cache erule.rule_target true) cache_proper rules
   in
      globals.venv_target_is_buildable <- cache;
      globals.venv_target_is_buildable_proper <- cache_proper

let venv_flush_target_cache venv =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_target_is_buildable <- NodeTable.empty;
      globals.venv_target_is_buildable_proper <- NodeTable.empty

(************************************************************************
 * Rules
 *)

(*
 * Symbols for directories.
 *)
let wild_sym            = Lm_symbol.add wild_string
let explicit_target_sym = Lm_symbol.add "<EXPLICIT_TARGET>"

(*
 * Don't save explicit rules.
 *)
let venv_explicit_target venv target =
   venv_add_var_dynamic venv explicit_target_sym (ValNode target)

(*
 * Save explicit rules.
 *)
let venv_save_explicit_rules venv loc erules =
   (* Filter out the rules with a different target *)
   let rules =
      try
         match venv_find_var_dynamic venv explicit_target_sym with
            ValNode target ->
               let rules =
                  List.fold_left (fun rules erule ->
                        if Node.equal erule.rule_target target then
                           erule :: rules
                        else
                           rules) [] erules
               in
               let rules = List.rev rules in
               let () =
                  if rules = [] then
                     let print_error buf =
                        fprintf buf "@[<b 3>Computed rule for `%a' produced no useful rules:" pp_print_node target;
                        List.iter (fun erule ->
                              fprintf buf "@ `%a'" pp_print_node erule.rule_target) erules;
                        fprintf buf "@]"
                     in
                        raise (OmakeException (loc_exp_pos loc, LazyError print_error))
               in
                  rules
          | _ ->
               erules
      with
         Not_found ->
            erules
   in
   let globals = venv.venv_inner.venv_globals in
      globals.venv_explicit_new <- List.rev_append rules globals.venv_explicit_new;
      venv_add_explicit_targets venv rules

(*
 * Add an explicit dependency.
 *)
let venv_add_explicit_dep venv loc target source =
   let erule =
      { rule_loc        = loc;
        rule_env        = venv;
        rule_target     = target;
        rule_effects    = NodeSet.singleton target;
        rule_sources    = NodeSet.singleton source;
        rule_locks      = NodeSet.empty;
        rule_scanners   = NodeSet.empty;
        rule_match      = None;
        rule_multiple   = RuleSingle;
        rule_commands   = []
      }
   in
      ignore (venv_save_explicit_rules venv loc [erule])

(*
 * Add the wild target.
 *)
let venv_add_wild_match venv v =
   venv_add_var_tmp venv wild_sym v

(*
 * This is the standard way to add results of a pattern match.
 *)
let venv_add_match_args venv args =
   let venv, _ =
      List.fold_left (fun (venv, i) arg ->
            let v = Lm_symbol.add (string_of_int i) in
            let venv = venv_add_var_tmp venv v (ValData arg) in
               venv, succ i) (venv, 1) args
   in
      venv

let venv_add_match venv line args =
   let args = List.map (fun s -> ValData s) args in
   let venv, _ =
      List.fold_left (fun (venv, i) arg ->
            let v = Lm_symbol.add (string_of_int i) in
            let venv = venv_add_var_tmp venv v arg in
               venv, succ i) (venv, 1) args
   in
   let venv = venv_add_var_tmp venv zero_sym (ValData line) in
   let venv = venv_add_var_tmp venv star_sym (ValArray args) in
   let venv = venv_add_var_tmp venv nf_sym   (ValInt (List.length args)) in
      venv

(*
 * Phony names.
 *)
let venv_add_phony venv loc names =
   if names = [] then
      venv
   else
      let inner = venv.venv_inner in
      let { venv_dir = dir;
            venv_phony = phony
          } = inner
      in
      let globals = venv_globals venv in
      let phonies = globals.venv_phonies in
      let phony, phonies =
         List.fold_left (fun (phony, phonies) name ->
               let name =
                  match name with
                     TargetNode _ ->
                        raise (OmakeException (loc_exp_pos loc, StringError ".PHONY arguments should be names"))
                   | TargetString s ->
                        s
               in
               let gnode = Node.create_phony_global name in
               let dnode = Node.create_phony_dir dir name in
               let phony = NodeSet.add phony dnode in
               let phonies = PreNodeSet.add phonies (Node.dest gnode) in
               let phonies = PreNodeSet.add phonies (Node.dest dnode) in
                  venv_add_explicit_dep venv loc gnode dnode;
                  phony, phonies) (phony, phonies) names
      in
      let inner = { inner with venv_phony = phony } in
      let venv = { venv with venv_inner = inner } in
         globals.venv_phonies <- phonies;
         venv

(*
 * Create an environment.
 *)
let create_environ () =
   let env = Unix.environment () in
   let len = Array.length env in
   let rec collect table i =
      if i = len then
         table
      else
         let s = env.(i) in
         let j = String.index s '=' in
         let name = String.sub s 0 j in
         let name =
            if Sys.os_type = "Win32" then
               String.uppercase name
            else
               name
         in
         let v = Lm_symbol.add name in
         let x = String.sub s (j + 1) (String.length s - j - 1) in
         let table = SymbolTable.add table v x in
            collect table (succ i)
   in
      collect SymbolTable.empty 0

let create options dir exec cache =
   let cwd = Dir.cwd () in
   let env = create_environ () in
   let mount_info =
      { mount_file_exists = Omake_cache.exists cache;
        mount_file_reset  = (fun node -> ignore (Omake_cache.force_stat cache node));
        mount_is_dir      = Omake_cache.is_dir cache;
        mount_digest      = Omake_cache.stat cache;
        mount_stat        = Omake_cache.stat_unix cache
      }
   in
   let globals =
      { venv_exec                       = exec;
        venv_cache                      = cache;
        venv_mount_info                 = mount_info;
        venv_files                      = NodeSet.empty;
        venv_directories                = DirTable.empty;
        venv_excluded_directories       = DirSet.empty;
        venv_phonies                    = PreNodeSet.empty;
        venv_explicit_rules             = [];
        venv_explicit_new               = [];
        venv_explicit_targets           = NodeSet.empty;
        venv_ordering_rules             = [];
        venv_orders                     = StringSet.empty;
        venv_pervasives_obj             = SymbolTable.empty;
        venv_pervasives_vars            = SymbolTable.empty;
        venv_ir_files                   = NodeTable.empty;
        venv_object_files               = NodeTable.empty;
        venv_static_values              = NodeTable.empty;
        venv_modified_values            = NodeTable.empty;
        venv_target_is_buildable        = NodeTable.empty;
        venv_target_is_buildable_proper = NodeTable.empty
      }
   in
   let inner =
      { venv_dir            = cwd;
        venv_environ        = env;
        venv_phony          = NodeSet.empty;
        venv_implicit_deps  = [];
        venv_implicit_rules = [];
        venv_globals        = globals;
        venv_options        = options;
        venv_mount          = Mount.empty;
        venv_included_files = NodeSet.empty
      }
   in
   let venv =
      { venv_this           = SymbolTable.empty;
        venv_dynamic        = SymbolTable.empty;
        venv_static         = SymbolTable.empty;
        venv_inner          = inner
      }
   in
   let venv = venv_add_phony venv (Lm_location.bogus_loc makeroot_name) [TargetString ".PHONY"] in
   let venv = venv_add_var_dynamic venv cwd_sym (ValDir cwd) in
   let venv = venv_add_var_dynamic venv stdlib_sym (ValDir Dir.lib) in
   let venv = venv_add_var_dynamic venv stdroot_sym (ValNode (venv_intern_cd venv PhonyProhibited Dir.lib "OMakeroot")) in
   let venv = venv_add_var_dynamic venv ostype_sym (ValString Sys.os_type) in
   let venv = venv_add_wild_match venv (ValData wild_string) in
   let omakepath =
      try
         let path = Lm_string_util.split pathsep (SymbolTable.find env omakepath_sym) in
            List.map (fun s -> ValString s) path
      with
         Not_found ->
            [ValString "."; ValDir Dir.lib]
   in
   let omakepath = ValArray omakepath in
   let venv = venv_add_var_dynamic venv omakepath_sym omakepath in
   let path =
      try
         let path = Lm_string_util.split pathsep (SymbolTable.find env path_sym) in
            ValArray (List.map (fun s -> ValData s) path)
      with
         Not_found ->
            eprintf "*** omake: PATH environment variable is not set!@.";
            ValArray []
   in
   let venv = venv_add_var_dynamic venv path_sym path in
      venv

(*
 * Create a fresh environment from the pervasives.
 * This is used for compiling objects.
 *)
let venv_set_pervasives venv =
   let globals = venv.venv_inner.venv_globals in
   let obj = venv.venv_dynamic in
   let vars = SymbolTable.map (fun _ -> ScopeGlobal) obj in
      globals.venv_pervasives_obj <- venv.venv_dynamic;
      globals.venv_pervasives_vars <- vars

let venv_get_pervasives venv node =
   let { venv_inner = inner } = venv in
   let { venv_environ = env;
         venv_options = options;
         venv_globals = globals
       } = inner
   in
   let { venv_exec            = exec;
         venv_cache           = cache;
         venv_pervasives_obj  = obj;
         venv_pervasives_vars = vars
       } = globals
   in
   let inner =
      { venv_dir            = Node.dir node;
        venv_environ        = env;
        venv_phony          = NodeSet.empty;
        venv_implicit_deps  = [];
        venv_implicit_rules = [];
        venv_globals        = globals;
        venv_options        = options;
        venv_mount          = Mount.empty;
        venv_included_files = NodeSet.empty
      }
   in
      { venv_this           = SymbolTable.empty;
        venv_dynamic        = obj;
        venv_static         = SymbolTable.empty;
        venv_inner          = inner
      }

(*
 * Fork the environment, so that changes really have no effect on the old one.
 * This is primarly used when a thread wants a private copy of the environment.
 *)
let venv_fork venv =
   let inner = venv.venv_inner in
   let globals = inner.venv_globals in
   let globals = { globals with venv_exec = globals.venv_exec } in
   let inner = { inner with venv_globals = globals } in
      { venv with venv_inner = inner }

(*
 * Get the scope of all variables.
 *)
let venv_include_scope venv mode =
   match mode with
      IncludePervasives ->
         venv.venv_inner.venv_globals.venv_pervasives_vars
    | IncludeAll ->
         let { venv_this = this;
               venv_dynamic = dynamic
             } = venv
         in
         let vars = SymbolTable.map (fun _ -> ScopeProtected) this in
            SymbolTable.fold (fun vars v _ -> SymbolTable.add vars v ScopeGlobal) vars dynamic

(*
 * Add an included file.
 *)
let venv_is_included_file venv node =
   NodeSet.mem venv.venv_inner.venv_included_files node

let venv_add_included_file venv node =
   let inner = venv.venv_inner in
   let inner = { inner with venv_included_files = NodeSet.add inner.venv_included_files node } in
      { venv with venv_inner = inner }

(*
 * Global state.
 *)
let venv_exec venv =
   venv.venv_inner.venv_globals.venv_exec

let venv_cache venv =
   venv.venv_inner.venv_globals.venv_cache

(*
 * Change directories.  Update the CWD var, and all a default
 * rule for all the phonies.
 *)
let venv_chdir_tmp venv dir =
   { venv with venv_inner = { venv.venv_inner with venv_dir = dir } }

let venv_chdir_dir venv loc dir =
   let inner = venv.venv_inner in
   let { venv_dir = cwd;
         venv_phony = phony
       } = inner
   in
      if Dir.equal dir cwd then
         venv
      else
         let venv = venv_add_var_dynamic venv cwd_sym (ValDir dir) in
         let venv = venv_chdir_tmp venv dir in
         let globals = venv_globals venv in
         let phonies = globals.venv_phonies in
         let phony, phonies =
            NodeSet.fold (fun (phony, phonies) node ->
                  let node' = Node.create_phony_chdir node dir in
                  let phony = NodeSet.add phony node' in
                  let phonies = PreNodeSet.add phonies (Node.dest node') in
                     venv_add_explicit_dep venv loc node node';
                     phony, phonies) (NodeSet.empty, phonies) phony
         in
         let inner =
            { inner with venv_dir = dir;
                         venv_phony = phony
            }
         in
         let venv = { venv with venv_inner = inner } in
            globals.venv_phonies <- phonies;
            venv

let venv_chdir venv loc dir =
   let dir = Dir.chdir venv.venv_inner.venv_dir dir in
      venv_chdir_dir venv loc dir

(*
 * The public version does not mess whith the phonies.
 *)
let venv_chdir_tmp venv dir =
   let cwd = venv.venv_inner.venv_dir in
      if Dir.equal dir cwd then
         venv
      else
         let venv = venv_add_var_dynamic venv cwd_sym (ValDir dir) in
            venv_chdir_tmp venv dir

(*
 * Get the dir.
 *)
let venv_dir venv =
   venv.venv_inner.venv_dir

(*
 * When an OMakefile in a dir is read, save the venv
 * to be used for targets that do not have nay explicit target rule.
 *)
let venv_add_dir venv =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_directories <- DirTable.add globals.venv_directories venv.venv_inner.venv_dir venv

let venv_directories venv =
   let globals = venv.venv_inner.venv_globals in
      DirSet.fold DirTable.remove globals.venv_directories globals.venv_excluded_directories

let venv_add_explicit_dir venv dir =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_directories <- DirTable.add globals.venv_directories dir venv;
      globals.venv_excluded_directories <- DirSet.remove globals.venv_excluded_directories dir

let venv_remove_explicit_dir venv dir =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_excluded_directories <- DirSet.add globals.venv_excluded_directories dir

let venv_find_target_dir_opt venv target =
   let target_dir = Node.dir target in
      if Dir.equal venv.venv_inner.venv_dir target_dir then
         Some venv
      else
         try Some (DirTable.find venv.venv_inner.venv_globals.venv_directories target_dir) with
            Not_found ->
               None

(*
 * When a file is read, remember it as a configuration file.
 *)
let venv_add_file venv node =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_files <- NodeSet.add globals.venv_files node;
      venv

(*
 * Get all the configuration files.
 *)
let venv_files venv =
   venv.venv_inner.venv_globals.venv_files

(*
 * Add a null rule.
 *)
let venv_add_implicit_deps venv pos loc multiple patterns locks sources scanners values =
   let pos = string_pos "venv_add_implicit_deps" pos in
   let patterns = List.map (compile_wild_pattern venv pos loc) patterns in
   let locks = List.map (compile_source venv) locks in
   let sources = List.map (compile_source venv) sources in
   let scanners = List.map (compile_source venv) scanners in
   let nrule =
      { inrule_loc        = loc;
        inrule_multiple   = multiple;
        inrule_patterns   = patterns;
        inrule_locks      = locks;
        inrule_sources    = sources;
        inrule_scanners   = scanners;
        inrule_values     = values
      }
   in
   let venv = { venv with venv_inner = { venv.venv_inner with venv_implicit_deps = nrule :: venv.venv_inner.venv_implicit_deps } } in
      venv_flush_target_cache venv;
      venv, []

(*
 * Add an implicit rule.
 *)
let venv_add_implicit_rule venv pos loc multiple patterns locks sources scanners values body =
   let pos = string_pos "venv_add_implicit_rule" pos in
   let patterns = List.map (compile_wild_pattern venv pos loc) patterns in
   let locks = List.map (compile_source venv) locks in
   let sources = List.map (compile_source venv) sources in
   let scanners = List.map (compile_source venv) scanners in
   let irule =
      { irule_loc        = loc;
        irule_multiple   = multiple;
        irule_patterns   = patterns;
        irule_locks      = locks;
        irule_sources    = sources;
        irule_scanners   = scanners;
        irule_values     = values;
        irule_body       = body
      }
   in
   let venv = { venv with venv_inner = { venv.venv_inner with venv_implicit_rules = irule :: venv.venv_inner.venv_implicit_rules } } in
      if debug debug_implicit then
         eprintf "@[<hv 3>venv_add_implicit_rule:@ @[<b 3>patterns =%a@]@ @[<b 3>sources =%a@]@]@." (**)
            pp_print_wild_list patterns
            pp_print_source_list sources;
      venv_flush_target_cache venv;
      venv, []

(*
 * Add a 2-place explicit rule.
 *)
let venv_add_explicit2_rules venv pos loc multiple targets locks sources scanners values body =
   let _pos = string_pos "venv_add_explicit_rule" pos in
   let target_args = List.map (venv_intern_rule_target venv multiple) targets in
   let lock_args = List.map (intern_source venv) locks in
   let source_args = List.map (intern_source venv) sources in
   let scanner_args = List.map (intern_source venv) scanners in
   let effects = node_set_of_list target_args in
   let locks = node_set_of_list lock_args in
   let sources = node_set_of_list source_args in
   let scanners = node_set_of_list scanner_args in
   let commands = make_command_info venv source_args values body in
   let add_target target =
      { rule_loc        = loc;
        rule_env        = venv;
        rule_target     = target;
        rule_effects    = effects;
        rule_locks      = locks;
        rule_sources    = sources;
        rule_scanners   = scanners;
        rule_match      = None;
        rule_multiple   = multiple;
        rule_commands   = commands
      }
   in
   let rules = List.map add_target target_args in
      venv_save_explicit_rules venv loc rules;
      venv, rules

(*
 * Add a 3-place rule.  Add a separate rule for each target.
 *)
let venv_add_explicit3_rules venv pos loc multiple targets locks patterns sources scanners values body =
   let pos = string_pos "venv_add_explicit_rule" pos in
   let patterns_wild = List.map (compile_wild_pattern venv pos loc) patterns in
   let locks = List.map (compile_source venv) locks in
   let sources = List.map (compile_source venv) sources in
   let scanners = List.map (compile_source venv) scanners in
   let target_list = List.map (venv_intern_rule_target venv multiple) targets in
   let add_target rules target =
      let dir = Node.dir target in
      let target_str = Node.tail target in
      let rec search patterns =
         match patterns with
            pattern :: patterns ->
               (match wild_match pattern target_str with
                   Some subst ->
                      let lock_args    = List.map (subst_source venv dir subst) locks in
                      let locks        = node_set_of_list lock_args in
                      let source_args  = List.map (subst_source venv dir subst) sources in
                      let sources      = node_set_of_list source_args in
                      let scanner_args = List.map (subst_source venv dir subst) scanners in
                      let scanners     = node_set_of_list scanner_args in
                      let effects =
                         List.fold_left (fun effects pattern ->
                               let effect = wild_subst_in subst pattern in
                               let effect = venv_intern_rule_target venv multiple (TargetString effect) in
                                  NodeSet.add effects effect) NodeSet.empty patterns_wild
                      in
                      let core = wild_core subst in
                      let venv = venv_add_wild_match venv (ValData core) in
                      let commands = make_command_info venv source_args values body in
                      let erule =
                         { rule_loc        = loc;
                           rule_env        = venv;
                           rule_target     = target;
                           rule_effects    = effects;
                           rule_locks      = locks;
                           rule_match      = Some core;
                           rule_sources    = sources;
                           rule_scanners   = scanners;
                           rule_multiple   = multiple;
                           rule_commands   = commands
                         }
                      in
                         erule :: rules
                 | None ->
                      search patterns)
          | [] ->
               raise (OmakeException (loc_pos loc pos, StringError ("bad match: " ^ target_str)))
      in
         search patterns_wild
   in
   let rules = List.fold_left add_target [] target_list in
      venv_save_explicit_rules venv loc rules;
      venv, rules

(*
 * Figure out what to do based on all the parts.
 * A rule is implicit if it has no targets _or_ if the
 * patterns do not contain a %.
 *)
let venv_add_rule venv pos loc multiple targets patterns locks sources scanners values commands =
   let pos = string_pos "venv_add_rule" pos in
      try match targets, patterns, commands with
         [], [], _ ->
            raise (OmakeException (loc_exp_pos loc, StringError "invalid null rule"))
       | _, [], [] ->
            if List.exists target_is_wild targets then
               venv_add_implicit_deps venv pos loc multiple targets locks sources scanners values
            else
               venv_add_explicit2_rules venv pos loc multiple targets locks sources scanners values commands
       | _, [], _ ->
            if List.exists target_is_wild targets then
               venv_add_implicit_rule venv pos loc multiple targets locks sources scanners values commands
            else
               venv_add_explicit2_rules venv pos loc multiple targets locks sources scanners values commands
       | _ ->
            venv_add_explicit3_rules venv pos loc multiple targets locks patterns sources scanners values commands
      with
         Failure err ->
            raise (OmakeException (loc_exp_pos loc, StringError err))

(*
 * Flush the explicit list.
 *)
let venv_explicit_flush venv =
   let globals = venv.venv_inner.venv_globals in
   let { venv_explicit_rules           = erules;
         venv_explicit_targets         = targets;
         venv_explicit_new             = enew
       } = globals
   in
      if enew <> [] then
         let targets, erules =
            List.fold_left (fun (targets, erules) erule ->
                  let erules = erule :: erules in
                  let targets = NodeSet.add targets erule.rule_target in
                     targets, erules) (targets, erules) (List.rev enew)
         in
            globals.venv_explicit_new <- [];
            globals.venv_explicit_rules <- erules;
            globals.venv_explicit_targets <- targets

(*
 * Check if an explicit rule exists.
 *)
let venv_explicit_exists venv target =
   venv_explicit_flush venv;
   NodeSet.mem venv.venv_inner.venv_globals.venv_explicit_targets target

let multiple_add_error errors target loc1 loc2 =
   let table = !errors in
   let table =
      if NodeMTable.mem table target then
         table
      else
         NodeMTable.add table target loc1
   in
      errors := NodeMTable.add table target loc2

let multiple_print_error errors buf =
   fprintf buf "@[<v 3>Multiple ways to build the following targets";
   NodeMTable.iter_all (fun target locs ->
      let locs = List.sort Lm_location.compare locs in
         fprintf buf "@ @[<v 3>%a:" pp_print_node target;
         List.iter (fun loc -> fprintf buf "@ %a" pp_print_location loc) locs;
         fprintf buf "@]") errors;
   fprintf buf "@]"

let raise_multiple_error errors =
   let _, loc = NodeMTable.choose errors in
      raise (OmakeException (loc_exp_pos loc, LazyError (multiple_print_error errors)))

(*
 * Get the explicit rules.  Build a table indexed by target.
 *)
let venv_explicit_rules venv =
   let errors = ref NodeMTable.empty in
   let add_target table target erule =
      NodeTable.filter_add table target (fun entry ->
            match entry with
               Some erule' ->
                  (*
                   * For .PHONY targets, multiple is ignored.
                   * Otherwise, multiple must be the same for both targets.
                   *)
                  let multiple = is_multiple_rule erule.rule_multiple in
                  let multiple' = is_multiple_rule erule'.rule_multiple in
                     if Node.is_phony target
                        || (multiple && multiple')
                        || ((not multiple && not multiple')
                            && (commands_are_trivial erule.rule_commands || commands_are_trivial erule'.rule_commands))
                     then
                        { erule with rule_commands = erule'.rule_commands @ erule.rule_commands }
                     else begin
                        multiple_add_error errors target erule'.rule_loc erule.rule_loc;
                        erule'
                     end
             | None ->
                  erule)
   in
   let add_deps table target locks sources scanners =
      NodeTable.filter_add table target (fun deps ->
            match deps with
               Some (lock_deps, source_deps, scanner_deps) ->
                  NodeSet.union lock_deps locks, NodeSet.union source_deps sources, NodeSet.union scanner_deps scanners
             | None ->
                  locks, sources, scanners)
   in
   let info =
      { explicit_targets      = NodeTable.empty;
        explicit_deps         = NodeTable.empty;
        explicit_rules        = NodeMTable.empty;
        explicit_directories  = venv_directories venv
      }
   in
   let rules =
      venv_explicit_flush venv;
      List.fold_left (fun info erule ->
            let { explicit_targets          = target_table;
                  explicit_deps             = dep_table;
                  explicit_rules            = rules
                } = info
            in
            let { rule_target   = target;
                  rule_locks    = locks;
                  rule_sources  = sources;
                  rule_scanners = scanners;
                  rule_multiple = multiple
                } = erule
            in
            let target_table   = add_target target_table target erule in
            let dep_table      = add_deps dep_table target locks sources scanners in
               { info with explicit_targets  = target_table;
                           explicit_deps     = dep_table;
                           explicit_rules    = NodeMTable.add rules target erule
               }) info (List.rev venv.venv_inner.venv_globals.venv_explicit_rules)
   in
      if NodeMTable.is_empty !errors then
         rules
      else
         raise_multiple_error !errors

(*
 * Find all the explicit dependencies listed through null
 * rules.
 *)
let venv_find_implicit_deps_inner venv target =
   let target_dir  = Node.dir target in
   let target_name = Node.tail target in
   let is_scanner =
      match Node.kind target with
         NodeScanner -> RuleScanner
       | _ -> RuleNormal
   in
      List.fold_left (fun (lock_deps, source_deps, scanner_deps, value_deps) nrule ->
            let { inrule_multiple = multiple;
                  inrule_patterns = patterns;
                  inrule_locks    = locks;
                  inrule_sources  = sources;
                  inrule_scanners = scanners;
                  inrule_values   = values
                } = nrule
            in
               if rule_kind multiple = is_scanner then
                  let rec search patterns =
                     match patterns with
                        pattern :: patterns ->
                           (match wild_match pattern target_name with
                               Some subst ->
                                  let lock_deps =
                                     List.fold_left (fun lock_deps source ->
                                           let source = subst_source venv target_dir subst source in
                                              NodeSet.add lock_deps source) lock_deps locks
                                  in
                                  let source_deps =
                                     List.fold_left (fun names source ->
                                           let source = subst_source venv target_dir subst source in
                                              NodeSet.add names source) source_deps sources
                                  in
                                  let scanner_deps =
                                     List.fold_left (fun scanner_deps source ->
                                           let source = subst_source venv target_dir subst source in
                                              NodeSet.add scanner_deps source) scanner_deps scanners
                                  in
                                  let value_deps = values @ value_deps in
                                     lock_deps, source_deps, scanner_deps, value_deps
                             | None ->
                                  search patterns)
                      | [] ->
                           lock_deps, source_deps, scanner_deps, value_deps
                  in
                     search patterns
               else
                  lock_deps, source_deps, scanner_deps, value_deps) (**)
         (NodeSet.empty, NodeSet.empty, NodeSet.empty, []) venv.venv_inner.venv_implicit_deps

let venv_find_implicit_deps venv target =
   match venv_find_target_dir_opt venv target with
      Some venv ->
         venv_find_implicit_deps_inner venv target
    | None ->
         NodeSet.empty, NodeSet.empty, NodeSet.empty, []

(*
 * Find the commands from implicit rules.
 *)
let venv_find_implicit_rules_inner venv target =
   let target_dir  = Node.dir target in
   let target_name = Node.tail target in
   let is_scanner =
      match Node.kind target with
         NodeScanner -> RuleScanner
       | _ -> RuleNormal
   in
   let _ =
      if debug debug_implicit then
         eprintf "Finding implicit rules for %s@." target_name
   in
   let rec collect matched = function
      irule :: irules ->
         let { irule_loc      = loc;
               irule_multiple = multiple;
               irule_patterns = patterns;
               irule_locks    = locks;
               irule_sources  = sources;
               irule_scanners = scanners;
               irule_values   = values;
               irule_body     = body
             } = irule
         in
            if rule_kind multiple = is_scanner then
               let _ =
                  if debug debug_implicit then
                     eprintf "@[<hv 3>venv_find_implicit_rules: considering implicit rule for@ target = %s:@ @[<b 3>patterns =%a@]@ @[<b 3>sources =%a@]@]@." (**)
                        target_name
                        pp_print_wild_list patterns
                        pp_print_source_list sources
               in
               let rec search = function
                  pattern :: patterns ->
                     let subst = wild_match pattern target_name in
                        (match subst with
                            Some _ -> subst
                          | None -> search patterns)
                | [] ->
                     None
               in
               let matched =
                  match search patterns with
                     Some subst ->
                        let source_args = List.map (subst_source venv target_dir subst) sources in
                        let sources = node_set_of_list source_args in
                        let lock_args = List.map (subst_source venv target_dir subst) locks in
                        let locks = node_set_of_list lock_args in
                        let scanner_args = List.map (subst_source venv target_dir subst) scanners in
                        let scanners = node_set_of_list scanner_args in
                        let core = wild_core subst in
                        let venv = venv_add_wild_match venv (ValData core) in
                        let commands = make_command_info venv source_args values body in
                        let effects =
                           List.fold_left (fun effects pattern ->
                                 let effect = wild_subst_in subst pattern in
                                 let effect = venv_intern_rule_target venv multiple (TargetString effect) in
                                    NodeSet.add effects effect) NodeSet.empty patterns
                        in
                        let erule =
                           { rule_loc         = loc;
                             rule_env         = venv;
                             rule_target      = target;
                             rule_match       = Some core;
                             rule_effects     = effects;
                             rule_locks       = locks;
                             rule_sources     = sources;
                             rule_scanners    = scanners;
                             rule_multiple    = multiple;
                             rule_commands    = commands
                           }
                        in
                           if debug debug_implicit then
                              eprintf "@[<hv 3>Added implicit rule for %s:%a@]@." (**)
                                 target_name pp_print_command_info_list commands;
                           erule :: matched
                   | None ->
                        matched
               in
                  collect matched irules
            else
               collect matched irules
    | [] ->
         List.rev matched
   in
      collect [] venv.venv_inner.venv_implicit_rules

let venv_find_implicit_rules venv target =
   match venv_find_target_dir_opt venv target with
      Some venv ->
         venv_find_implicit_rules_inner venv target
    | None ->
         []

(************************************************************************
 * Ordering rules.
 *)

(*
 * Add an order.
 *)
let venv_add_orders venv loc targets =
   let globals = venv.venv_inner.venv_globals in
   let orders =
      List.fold_left (fun orders target ->
            let name =
               match target with
                  TargetNode _ ->
                     raise (OmakeException (loc_exp_pos loc, StringTargetError (".ORDER should be a name", target)))
                | TargetString s ->
                     s
            in
               StringSet.add orders name) globals.venv_orders targets
   in
      globals.venv_orders <- orders;
      venv

(*
 * Check for order.
 *)
let venv_is_order venv name =
   StringSet.mem venv.venv_inner.venv_globals.venv_orders name

(*
 * Add an ordering rule.
 *)
let venv_add_ordering_rule venv pos loc name pattern sources =
   let pos = string_pos "venv_add_ordering_deps" pos in
   let pattern = compile_wild_pattern venv pos loc pattern in
   let sources = List.map (compile_source_core venv) sources in
   let orule =
      { orule_loc = loc;
        orule_name = name;
        orule_pattern = pattern;
        orule_sources = sources
      }
   in
   let globals = venv.venv_inner.venv_globals in
      globals.venv_ordering_rules <- orule :: globals.venv_ordering_rules;
      venv

(*
 * Get the ordering dependencies for a name.
 *)
let venv_get_ordering_info venv name =
   List.fold_left (fun orules orule ->
         if Lm_symbol.eq orule.orule_name name then
            orule :: orules
         else
            orules) [] venv.venv_inner.venv_globals.venv_ordering_rules

(*
 * Get extra dependencies.
 *)
let venv_get_ordering_deps venv orules deps =
   let step deps =
      NodeSet.fold (fun deps dep ->
            let target_dir = Node.dir dep in
            let target_str = Node.tail dep in
               List.fold_left (fun deps orule ->
                     let { orule_pattern = pattern;
                           orule_sources = sources
                         } = orule
                     in
                        match wild_match pattern target_str with
                           Some subst ->
                              List.fold_left (fun deps source ->
                                    let source = subst_source_core venv target_dir subst source in
                                       NodeSet.add deps source) deps sources
                         | None ->
                              deps) deps orules) deps deps
   in
   let rec fixpoint deps =
      let deps' = step deps in
         if NodeSet.cardinal deps' = NodeSet.cardinal deps then
            deps
         else
            fixpoint deps'
   in
      fixpoint deps

(************************************************************************
 * Return values.
 *)

(*
 * Don't export from a value.
 *)
let export_none v =
   match v with
      ValEnv _ ->
         ValNone
    | _ ->
         v

let restore_var src dst var =
   { dst with
      venv_dynamic =
         if SymbolTable.mem src.venv_dynamic var then
            SymbolTable.add dst.venv_dynamic var (SymbolTable.find src.venv_dynamic var)
         else
            SymbolTable.remove dst.venv_dynamic var;
      venv_static =
         if SymbolTable.mem src.venv_static var then
            SymbolTable.add dst.venv_static var (SymbolTable.find src.venv_static var)
         else
            SymbolTable.remove dst.venv_static var;
      venv_this =
         if SymbolTable.mem src.venv_this var then
            SymbolTable.add dst.venv_this var (SymbolTable.find src.venv_this var)
         else
            SymbolTable.remove dst.venv_this var
   }

let unexport env v vars =
   match v with
      ValEnv(env', export) ->
         let env' = List.fold_left (restore_var env) env' vars in
            ValEnv(env', export)
    | _ ->
         v

(*
 * Export an item from one environment to another.
 *)
let export_item pos venv_dst venv_src = function
   ExportSymbol v ->
      (*
       * For now, we don't know which scope to use, so we
       * copy them all.
       *)
      let { venv_dynamic = dynamic_src;
            venv_static  = static_src;
            venv_this    = this_src
          } = venv_src
      in
      let { venv_dynamic = dynamic_dst;
            venv_static  = static_dst;
            venv_this    = this_dst
          } = venv_dst
      in
      let dynamic, found =
         try SymbolTable.add dynamic_dst v (SymbolTable.find dynamic_src v), true with
            Not_found ->
               dynamic_dst, false
      in
      let static, found =
         try SymbolTable.add static_dst v (SymbolTable.find static_src v), true with
            Not_found ->
               static_dst, found
      in
      let this, found =
         try SymbolTable.add this_dst v (SymbolTable.find this_src v), true with
            Not_found ->
               this_dst, found
      in
         if not found then
            raise (OmakeException (pos, UnboundVar v));
         { venv_dst with venv_dynamic = dynamic;
                         venv_static = static;
                         venv_this = this
         }
 | ExportRules ->
      (*
       * Export the implicit rules.
       *)
      let inner_src = venv_src.venv_inner in
      let inner_dst =
         { venv_dst.venv_inner with
           venv_implicit_deps = inner_src.venv_implicit_deps;
           venv_implicit_rules = inner_src.venv_implicit_rules;
         }
      in
         { venv_dst with venv_inner = inner_dst }
 | ExportPhonies ->
      (*
       * Export the phony vars.
       *)
      let inner_dst = { venv_dst.venv_inner with venv_phony = venv_src.venv_inner.venv_phony } in
         { venv_dst with venv_inner = inner_dst }

let export_list pos venv_dst venv_src vars =
   List.fold_left (fun venv_dst v ->
         export_item pos venv_dst venv_src v) venv_dst vars

(*
 * Exported environment does not include static values.
 *
 * We want to preserve pointer equality on venv2 to avoid giving unnecessary
 * "these files are targeted separately, but appear as effects of a single rule"
 * warnings.
 *)
let venv_export_venv venv1 venv2 =
   if venv1.venv_static == venv2.venv_static then
      venv2
   else
      { venv2 with venv_static = venv1.venv_static }

(*
 * Add the exported result to the current environment.
 *)
let add_exports venv pos result =
   match result with
      ValEnv (_, ExportFile) ->
         venv, result
    | ValEnv (venv', ExportAll) ->
         venv_export_venv venv venv', result
    | ValEnv (venv', ExportDir) ->
         venv_chdir_tmp venv (venv_dir venv'), result
    | ValEnv (venv', ExportValue v) ->
         venv_export_venv venv venv', v
    | ValEnv (venv', ExportList vars) ->
         let venv = export_list pos venv venv' vars in
            venv, result
    | _ ->
         venv, result

(*
 * The result is different for include files.
 *)
let add_include venv pos result =
   match result with
      ValEnv (venv', ExportFile)
    | ValEnv (venv', ExportAll) ->
         venv_export_venv venv venv', result
    | ValEnv (venv', ExportDir) ->
         venv_chdir_tmp venv (venv_dir venv'), result
    | ValEnv (venv', ExportValue v) ->
         venv_export_venv venv venv', v
    | ValEnv (venv', ExportList vars) ->
         let venv = export_list pos venv venv' vars in
            venv, result
    | _ ->
         venv, result

(************************************************************************
 * Squashing.
 *)

let squash_prim_fun f =
   f.fun_id

let squash_object obj =
   obj

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
