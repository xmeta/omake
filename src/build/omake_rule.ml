(*
 * Rule evaluation.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2006 Mojave Group, Caltech
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

open Lm_glob
open Lm_debug
open Lm_symbol
open Lm_location
open Lm_string_set

open Omake_ir
open Omake_env
open Omake_util
open Omake_node
open Omake_eval
open Omake_exec
open Omake_state
open Omake_value
open Omake_symbol
open Omake_command
open Omake_ir_util
open Omake_ir_print
open Omake_node_sig
open Omake_exec_type
open Omake_exec_util
open Omake_shell_lex
open Omake_cache_type
open Omake_shell_type
open Omake_ir_free_vars
open Omake_command_type
open Omake_command_digest

module Pos = MakePos (struct let name = "Omake_rule" end);;
open Pos;;

type 'a result =
   Success of 'a
 | Exception of exn

(*
 * Debugging.
 *)
let debug_active_rules =
   create_debug (**)
      { debug_name = "active-rules";
        debug_description = "Display debugging information for computed rules";
        debug_value = false
      }

(*
 * A data structure to keep sequences of command_info.
 * We can append a command to the current info, or
 * add another entirely new info.
 *)
module type CommandSig =
sig
   type t
   type resume

   (* Create a buffer *)
   val create : venv -> Node.t -> value -> NodeSet.t -> NodeSet.t -> NodeSet.t -> NodeSet.t -> t

   (* Projections *)
   val target : t -> Node.t * value

   (* Adding to the buffer *)
   val add_command  : t -> command -> t
   val add_locks    : t -> NodeSet.t -> t
   val add_effects  : t -> NodeSet.t -> t
   val add_scanners : t -> NodeSet.t -> t
   val add_deps     : t -> NodeSet.t -> t

   (* Block operations *)
   val enter  : t -> venv -> Node.t list -> value list -> resume * t
   val resume : t -> resume -> t

   (* Get the final info *)
   val contents : t -> NodeSet.t * NodeSet.t * NodeSet.t * NodeSet.t * command_info list
end

module Command : CommandSig =
struct
   (*
    * The command buffer.
    *)
   type t =
      { buf_target   : Node.t;
        buf_core     : value;
        buf_locks    : NodeSet.t;
        buf_effects  : NodeSet.t;
        buf_deps     : NodeSet.t;
        buf_scanners : NodeSet.t;

        (* The state that is being collected *)
        buf_env      : venv;
        buf_sources  : Node.t list;
        buf_values   : value list;
        buf_commands : command list;

        (* The buffers that have already been collected *)
        buf_info     : command_info list
      }

   type resume = venv * Node.t list * value list

   (*
    * Create a new command buffer.
    *)
   let create venv target core locks effects deps scanners =
      { buf_target   = target;
        buf_core     = core;
        buf_locks    = locks;
        buf_effects  = effects;
        buf_deps     = deps;
        buf_scanners = scanners;
        buf_env      = venv;
        buf_sources  = [];
        buf_values   = [];
        buf_commands = [];
        buf_info     = []
      }

   (*
    * Projections.
    *)
   let target buf =
      let { buf_target = target;
            buf_core = core
          } = buf
      in
         target, core

   (*
    * Add a command to the buffer.
    *)
   let add_command buf command =
      { buf with buf_commands = command :: buf.buf_commands }

   let add_locks buf locks =
      { buf with buf_locks = NodeSet.union buf.buf_locks locks }

   let add_effects buf effects =
      { buf with buf_effects = NodeSet.union buf.buf_effects effects }

   let add_deps buf deps =
      { buf with buf_deps = NodeSet.union buf.buf_deps deps }

   let add_scanners buf scanners =
      { buf with buf_scanners = NodeSet.union buf.buf_scanners scanners }

   (*
    * Start a new environment.
    * Return a state that can be used to resume the current environment.
    *)
   let enter buf venv sources values =
      let { buf_env      = venv';
            buf_sources  = sources';
            buf_values   = values';
            buf_commands = commands';
            buf_info     = info'
          } = buf
      in
      let info =
         if commands' = [] && values' = [] then
            info'
         else
            let info =
               { command_env     = venv';
                 command_sources = sources';
                 command_values  = values';
                 command_body    = List.rev commands'
               }
            in
               info :: info'
      in
      let buf =
         { buf with buf_env      = venv;
                    buf_sources  = sources;
                    buf_values   = values;
                    buf_commands = [];
                    buf_info     = info
         }
      in
      let resume = venv', sources', values' in
         resume, buf

   let resume buf (venv, sources, values) =
      snd (enter buf venv sources values)

   (*
    * Get the contents.
    *)
   let contents buf =
      let { buf_env      = venv;
            buf_values   = values;
            buf_sources  = sources;
            buf_locks    = locks;
            buf_effects  = effects;
            buf_scanners = scanners;
            buf_deps     = deps;
            buf_commands = commands;
            buf_info     = info
          } = buf
      in
      let info =
         if commands = [] && values = [] then
            info
         else
            let info' =
               { command_env     = venv;
                 command_sources = sources;
                 command_values  = values;
                 command_body    = List.rev commands
               }
            in
               info' :: info
      in
      let info = List.rev info in
         locks, effects, deps, scanners, info
end

(*
 * Find a rule in a list.
 *)
let rec find_rule loc target rules =
   match rules with
      erule :: rules ->
         if Node.equal target erule.rule_target then
            erule
         else
            find_rule loc target rules
    | [] ->
         raise (OmakeException (loc_exp_pos loc, StringNodeError ("computed rule does not match target", target)))

(*
 * Check if there are any computed commands.
 *)
let commands_are_computed commands =
   List.exists (fun { command_body = body } ->
         List.exists (fun command ->
               match command with
                  CommandSection _ ->
                     true
                | CommandValue _ ->
                     false) body) commands

(*
 * Expand a single command.
 *)
let rec expand_command venv pos loc buf command =
   let pos = string_pos "expand_command" pos in
      match command with
         CommandValue _ ->
            Command.add_command buf command
       | CommandSection (arg, fv, e) ->
            if debug debug_active_rules then
               eprintf "@[<hv 3>section %a@ %a@]@." pp_print_value arg pp_print_exp e;

            (* The section should be either a rule or eval case *)
            match Lm_string_util.trim (string_of_value venv pos arg) with
               "" ->
                  expand_eval_section venv pos loc buf "eval" fv e
             | "eval" as s ->
                  expand_eval_section venv pos loc buf s fv e
             | "rule" ->
                  expand_rule_section venv pos loc buf e
             | s ->
                  raise (OmakeException (loc_exp_pos loc, StringStringError ("invalid section argument, valid arguments are rule, eval", s)))

and expand_commands venv pos loc buf commands =
   match commands with
      command :: commands ->
         let buf = expand_command venv pos loc buf command in
            expand_commands venv pos loc buf commands
    | [] ->
         buf

(*
 * This section computes a new rule for the target.
 *)
and expand_rule_section venv pos loc buf e =
   let pos = string_pos "expand_rule_section" pos in
   let target, core = Command.target buf in
   let venv = venv_explicit_target venv target in
   let venv = venv_add_wild_match venv core in
   let v = eval venv e in
      if debug debug_active_rules then
         eprintf "@[<v 0>%a@ @[<hv 3>*** omake: rule body returned@ @[<hv 3>%a@]@]@]@." (**)
            pp_print_location loc
            pp_print_value v;

      match v with
         ValRules erules ->
            let erule = find_rule loc target erules in
            let { rule_locks    = locks;
                  rule_effects  = effects;
                  rule_sources  = deps;
                  rule_scanners = scanners;
                  rule_commands = commands
                } = erule
            in
            let buf = Command.add_locks buf locks in
            let buf = Command.add_effects buf effects in
            let buf = Command.add_deps buf deps in
            let buf = Command.add_scanners buf scanners in
               expand_command_info_list pos loc buf commands
       | v ->
            eprintf "@[<v 0>%a@ *** omake: section rule: did not compute a rule@]@." (**)
               pp_print_location loc;
            buf

(*
 * This section is to be evaluated when the rule is run.
 *)
and expand_eval_section venv pos loc buf s fv e =
   let _pos = string_pos "expand_eval_section" pos in
      Command.add_command buf (CommandSection (ValString s, fv, e))

(*
 * Expand a buf_info list.
 *)
and expand_command_info pos loc buf command =
   let { command_env = venv;
         command_sources = sources;
         command_values = values;
         command_body = commands
       } = command
   in
   let pos = string_pos "expand_buf_info" pos in
   let resume, buf = Command.enter buf venv sources values in
   let buf = expand_commands venv pos loc buf commands in
      Command.resume buf resume

and expand_command_info_list pos loc buf commands =
   let pos = string_pos "expand_buf_info_list" pos in
      List.fold_left (expand_command_info pos loc) buf commands

(*
 * Expand a rule, so that the commands are a command list.
 *)
let expand_rule erule =
   let { rule_loc      = loc;
         rule_env      = venv;
         rule_target   = target;
         rule_locks    = locks;
         rule_effects  = effects;
         rule_match    = core;
         rule_sources  = deps;
         rule_scanners = scanners;
         rule_commands = commands
       } = erule
   in
      if commands_are_computed commands then
         let core =
            match core with
               Some s ->
                  ValData s
             | None ->
                  ValNode target
         in
         let pos = string_pos "expand_rule" (loc_exp_pos loc) in
         let buf = Command.create venv target core locks effects deps scanners in
         let buf = expand_command_info_list pos loc buf commands in
         let locks, effects, deps, scanners, commands = Command.contents buf in
            if debug debug_active_rules then
               eprintf "@[<v 3>expand_rule: %a@ @[<b 3>locks =%a@]@ @[<b 3>effects =%a@]@ @[<b 3>deps = %a@]@ @[<b 3>scanners = %a@]@]@." (**)
                  pp_print_node target
                  pp_print_node_set locks
                  pp_print_node_set effects
                  pp_print_node_set deps
                  pp_print_node_set scanners;
            { erule with rule_locks = locks;
                         rule_effects = effects;
                         rule_sources = deps;
                         rule_scanners = scanners;
                         rule_commands = commands
            }
      else begin
         if debug debug_active_rules then
            eprintf "@[<v 0>%a@ @[<hv 3>*** omake: static rule@ @[<hv 3>%a@]@]@]@." (**)
               pp_print_location loc
               pp_print_value (ValRules [erule]);
         erule
      end

(************************************************************************
 * Shell utilities.
 *)

(*
 * Get the info for the command.
 *)
let eval_shell_info command =
   let { command_flags = flags;
         command_dir = dir;
         command_target = target
       } = command
   in
      flags, dir, target

(*
 * Kill a process.
 *)
let eval_shell_kill venv pos pid =
   match pid with
      ExternalPid pid ->
         Unix.kill pid Sys.sigterm
    | InternalPid pid ->
         Omake_shell_job.kill venv pos pid SigTerm
    | ResultPid _ ->
         ()

(*
 * Wait for a process to exit.
 *)
let eval_shell_wait venv pos pid =
   let pos = string_pos "eval_shell_wait" pos in
   let _, status, value = Omake_shell_job.waitpid venv pos pid in
      status, value

(************************************************************************
 * Globbing.
 *)
let glob_options_of_string options s =
   let len = String.length s in
   let rec search options i =
      if i = len then
         List.rev options
      else
         let options =
            match s.[i] with
               'b' -> GlobNoBraces    :: options
             | 'e' -> GlobNoEscape    :: options
             | 'c'
             | 'n' -> GlobNoCheck     :: options
             | 'i' -> GlobIgnoreCheck :: options
             | 'A'
             | '.' -> GlobDot         :: options
             | 'F' -> GlobOnlyFiles   :: options
             | 'D' -> GlobOnlyDirs    :: options
             | 'C' -> GlobCVSIgnore   :: options
             | 'P' -> GlobProperSubdirs :: options
             | _ -> options
         in
            search options (succ i)
   in
      search options 0

(*
 * Glob an argument into directories and files.
 *)
let glob_arg venv pos loc options arg =
   let cwd = venv_dir venv in
   let dirs, files = Lm_glob.glob options (Dir.fullname cwd) [glob_string_of_arg options arg] in
   let dirs = List.sort String.compare dirs in
   let files = List.sort String.compare files in
   let dirs = List.map (fun dir -> Dir.chdir cwd dir) dirs in
   let files = List.map (fun file -> venv_intern_cd venv PhonyProhibited cwd file) files in
      dirs, files

(*
 * This is similar to the above, but we interleave the directories
 * with the files when sorting.
 *)
type glob_result =
   GDir of string
 | GFile of string

let glob_result_compare r1 r2 =
   let s1 =
      match r1 with
         GDir s
       | GFile s ->
            s
   in
   let s2 =
      match r2 with
         GDir s
       | GFile s ->
            s
   in
      -(String.compare s1 s2)

let glob_rev_arg venv pos loc options arg argv =
   let cwd = venv_dir venv in
   let dirs, files = Lm_glob.glob options (Dir.fullname cwd) [glob_string_of_arg options arg] in
   let args = List.fold_left (fun args s -> GFile s :: args) [] files in
   let args = List.fold_left (fun args s -> GDir s :: args) args dirs in
   let args = List.sort glob_result_compare args in
      List.fold_left (fun argv arg ->
            let v =
               match arg with
                  GDir s -> ValDir (Dir.chdir cwd s)
                | GFile s -> ValNode (venv_intern_cd venv PhonyProhibited cwd s)
            in
               v :: argv) argv args

(*
 * Glob the executable.
 * We do the standard thing, and allow glob expansions to multiple filenames.
 * In this case, the actual command is a bit ambiguous, so users should be
 * careful when they do it.
 *)
let glob_arg_exe venv pos loc options (arg : arg) : (simple_exe * Node.t list) =
   if is_glob_arg options arg then
      match glob_arg venv pos loc options arg with
         [], exe :: args ->
            ExeNode exe, args
       | [], [] ->
            raise (OmakeException (pos, StringError "null glob expansion"))
       | dir :: _, _ ->
            raise (OmakeException (pos, StringValueError ("is a directory", ValDir dir)))
   else if is_quoted_arg arg then
      ExeQuote (simple_string_of_arg arg), []
   else
      parse_command_string (simple_string_of_arg arg), []

let glob_exe venv pos loc options (exe : arg cmd_exe) : (simple_exe * Node.t list) =
   match exe with
      CmdNode node ->
         ExeNode node, []
    | CmdArg arg ->
         glob_arg_exe venv pos loc options arg

(*
 * Glob expand the glob arguments.
 *)
let glob_value_argv venv pos loc options argv =
   List.fold_left (fun argv v ->
         if is_glob_value options v then
            let arg = arg_of_values venv pos [v] in
               glob_rev_arg venv pos loc options arg argv
         else
            v :: argv) [] (List.rev argv)

(*
 * Glob the command line.
 *)
let glob_command_line venv pos loc options argv =
   let cwd = venv_dir venv in
   let dir = Dir.fullname cwd in
   let argv = List.map (glob_string_of_arg options) argv in
      Lm_glob.glob_argv options dir argv

(*
 * Glob an input or output file.
 *)
let glob_channel venv pos loc options name =
   match name with
      RedirectNone
    | RedirectNode _ as file ->
         file
    | RedirectArg name ->
         match glob_arg venv pos loc options name with
            [], [node] ->
               RedirectNode node
          | dir :: _, _ ->
               raise (OmakeException (pos, StringValueError ("is a directory", ValDir dir)))
          | [], _ :: _ :: _ ->
               raise (OmakeException (pos, StringStringError ("ambiguous redirect", simple_string_of_arg name)))
          | [], [] ->
               raise (OmakeException (pos, StringStringError ("null redirect", simple_string_of_arg name)))

(*
 * Convert the environment strings.
 *)
let string_of_env env =
   List.map (fun (v, arg) ->
         v, simple_string_of_arg arg) env

(************************************************************************
 * Alias expansion.
 *)
let find_alias_exn shell_obj venv pos loc exe =
   (* If this is an internal command, create the PipeApply *)
   let name = Lm_symbol.add exe in
   let v = venv_find_field_exn shell_obj name in
   let _, _, f = eval_fun venv pos v in

   (* Found the function, no exceptions now *)
   let f venv stdin stdout stderr env argv =
      if !debug_eval || !debug_shell then
         eprintf "Running %s, stdin=%i, stdout=%i, stderr=%i@." exe (Obj.magic stdin) (Obj.magic stdout) (Obj.magic stderr);
      let venv   = venv_fork venv in
      let venv   = List.fold_left (fun venv (v, s) -> venv_setenv venv v s) venv env in
      let stdin_chan  = Lm_channel.create "<stdin>"  Lm_channel.PipeChannel Lm_channel.InChannel  false (Some stdin) in
      let stdout_chan = Lm_channel.create "<stdout>" Lm_channel.PipeChannel Lm_channel.OutChannel false (Some stdout) in
      let stderr_chan = Lm_channel.create "<stderr>" Lm_channel.PipeChannel Lm_channel.OutChannel false (Some stderr) in
      let stdin  = venv_add_channel venv stdin_chan in
      let stdout = venv_add_channel venv stdout_chan in
      let stderr = venv_add_channel venv stderr_chan in
      let venv   = venv_add_var venv ScopeGlobal pos stdin_sym  (ValChannel (InChannel,  stdin)) in
      let venv   = venv_add_var venv ScopeGlobal pos stdout_sym (ValChannel (OutChannel, stdout)) in
      let venv   = venv_add_var venv ScopeGlobal pos stderr_sym (ValChannel (OutChannel, stderr)) in
      let v      = ValArray argv in
      let () =
         if !debug_eval then
            eprintf "normalize_apply: evaluating internal function@."
      in
      let code, value, reraise =
         try
            let v = f venv pos loc [v] in
            let code =
               match v with
                  ValOther (ValExitCode code) ->
                     code
                | _ ->
                     0
            in
               code, v, None
         with
            ExitException (_, code) as exn ->
               code, ValNone, Some exn
          | OmakeException _
          | UncaughtException _ as exn ->
               eprintf "%a@." Omake_exn_print.pp_print_exn exn;
               Omake_state.exn_error_code, ValNone, None
          | Unix.Unix_error _
          | Sys_error _
          | Not_found
          | Failure _ as exn ->
               eprintf "%a@." Omake_exn_print.pp_print_exn (UncaughtException (pos, exn));
               Omake_state.exn_error_code, ValNone, None
      in
         if !debug_eval then
            eprintf "normalize_apply: internal function is done: %d, %a@." code pp_print_value value;
         venv_close_channel venv pos stdin;
         venv_close_channel venv pos stdout;
         venv_close_channel venv pos stderr;
         if !debug_eval then
            eprintf "normalize_apply: returning value: %d, %a@." code pp_print_value value;
         match reraise with
            Some exn ->
               raise exn
          | None ->
               code, value
   in
      name, f

let find_alias obj venv pos loc exe =
   try Some (find_alias_exn obj venv pos loc exe) with
      Not_found ->
         None

let find_alias_of_env venv pos =
   try
      let obj = venv_find_var_exn venv ScopeGlobal shell_object_sym in
         match eval_single_value venv pos obj with
            ValObject obj ->
               find_alias obj
          | _ ->
               raise Not_found
   with
      Not_found ->
         (fun _venv _pos _loc _exe -> None)

(************************************************************************
 * Rule evaluation.
 *)

(*
 * Get the target string if there is a single one.
 *)
let target_of_value venv pos v =
   match v with
      ValNode node ->
         TargetNode node
    | _ ->
         TargetString (string_of_value venv pos v)

let targets_of_value venv pos v =
   List.map (target_of_value venv pos) (values_of_value venv pos v)

let pp_print_target buf target =
   match target with
      TargetNode node ->
         fprintf buf "TargetNode %a" pp_print_node node
    | TargetString s ->
         fprintf buf "TargetString %s" s

let pp_print_targets buf targets =
   List.iter (fun target -> fprintf buf " %a" pp_print_target target) targets

(*
 * From Omake_cache.
 *)
let include_fun = Omake_cache.include_fun

(*
 * Collect the different kinds of sources.
 *)
let add_sources sources kind sources' =
   List.fold_left (fun sources source ->
         (kind, source) :: sources) sources sources'

let sources_of_options venv pos loc sources options =
   let options = map_of_value venv pos options in
   let effects, sources, scanners, values =
      venv_map_fold (fun (effects, sources, scanners, values) optname optval ->
            let s = string_of_value venv pos optname in
            let v = Lm_symbol.add s in
               if Lm_symbol.eq v normal_sym then
                  let files = targets_of_value venv pos optval in
                     effects, add_sources sources NodeNormal files, scanners, values
               else if Lm_symbol.eq v optional_sym then
                  let files = targets_of_value venv pos optval in
                     effects, add_sources sources NodeOptional files, scanners, values
               else if Lm_symbol.eq v exists_sym then
                  let files = targets_of_value venv pos optval in
                     effects, add_sources sources NodeExists files, scanners, values
               else if Lm_symbol.eq v squash_sym then
                  let files = targets_of_value venv pos optval in
                     effects, add_sources sources NodeSquashed files, scanners, values
               else if Lm_symbol.eq v scanner_sym then
                  let files = targets_of_value venv pos optval in
                     effects, sources, add_sources scanners NodeScanner files, values
               else if Lm_symbol.eq v effects_sym then
                  let files = targets_of_value venv pos optval in
                     add_sources effects NodeNormal files, sources, scanners, values
               else if Lm_symbol.eq v values_sym then
                  effects, sources, scanners, optval :: values
               else
                  raise (OmakeException (loc_pos loc pos, StringVarError ("unknown rule option", v)))) (**)
         ([], sources, [], []) options
   in
      List.rev effects, List.rev sources, List.rev scanners, List.rev values

(*
 * Get the commands.
 *)
let lazy_command venv pos command =
   match command with
      SectionExp (loc, s, el) ->
         let fv = free_vars_exp_list el in
            CommandSection (eager_string_exp venv pos s, fv, SequenceExp (loc, el))
    | ShellExp (loc, s) ->
         CommandValue (loc, lazy_string_exp venv pos s)
    | _ ->
         let fv = free_vars_exp command in
            CommandSection (ValData "eval", fv, command)

let lazy_commands venv pos commands =
   match eval_value venv pos commands with
      ValBody (env, SequenceExp (_, el)) ->
         List.map (lazy_command (venv_with_env venv env) pos) el
    | ValBody (env, e) ->
         [lazy_command (venv_with_env venv env) pos e]
    | _ ->
         raise (OmakeException (pos, StringValueError ("unknown rule commands", commands)))

let exp_list_of_commands venv pos commands =
   match eval_value venv pos commands with
      ValBody (_, SequenceExp (_, el)) ->
         el
    | ValBody (_, e) ->
         [e]
    | _ ->
         raise (OmakeException (pos, StringValueError ("unknown rule commands", commands)))

(*
 * Evaluate a rule.  This is the most complicated part of evaluation.
 *
 * There are two types of rules.  Implicit rules are 2-place rules that
 * have a % in the target name.  Explicit rules are 2-place rules
 * that do not have a %, or 3-place rules.
 *
 * In a 3-place rule, the targets are always explicit.
 *)
let rec eval_rule_exp venv pos loc multiple target pattern source options body =
   let pos = string_pos "eval_rule_exp" pos in

   (* First, evaluate the parts *)
   let targets  = targets_of_value venv pos target in
   let patterns = targets_of_value venv pos pattern in
   let sources  = targets_of_value venv pos source in
   let sources  = add_sources [] NodeNormal sources in
   let effects, sources, scanners, values = sources_of_options venv pos loc sources options in
   let commands = lazy_commands venv pos body in
   let commands_are_nontrivial = commands <> [] in
      (* Process special rules *)
      match targets with
         [TargetString ".SUBDIRS"] ->
            if effects <> [] || patterns <> [] || scanners <> [] || values <> [] then
               raise (OmakeException (loc_exp_pos loc, SyntaxError ".SUBDIRS rule cannot have patterns, effects, scanners, or values"));
            let venv = eval_subdirs_rule venv loc sources (exp_list_of_commands venv pos body) in
               venv, ValNone
       | [TargetString ".PHONY"]  ->
            if commands_are_nontrivial then
               raise (OmakeException (loc_exp_pos loc, SyntaxError ".PHONY rule cannot have build commands"));
            if effects <> [] || patterns <> [] || scanners <> [] || values <> [] then
               raise (OmakeException (loc_exp_pos loc, SyntaxError ".PHONY rule cannot have patterns, effects, scanners, or values"));
            let sources = List.map snd sources in
            let venv = venv_add_phony venv loc sources in
               venv, ValNone
       | [TargetString ".SCANNER"] ->
            let targets, sources =
               if patterns = [] then
                  List.map snd sources, []
               else
                  patterns, sources
            in
            let multiple =
               if multiple then
                  RuleScannerMultiple
               else
                  RuleScannerSingle
            in
            let venv, rules = venv_add_rule venv pos loc multiple targets [] effects sources scanners values commands in
               venv, ValRules rules
       | [TargetString ".INCLUDE"] ->
            if effects <> [] || scanners <> [] then
               raise (OmakeException (loc_exp_pos loc, SyntaxError ".INCLUDE cannot have effects or scanners"));
            let targets, sources =
               if patterns = [] then
                  List.map snd sources, []
               else
                  patterns, sources
            in
            let venv = eval_include_rule venv pos loc targets sources values commands in
               venv, ValNone
       | [TargetString ".ORDER"] ->
            if commands_are_nontrivial then
               raise (OmakeException (loc_exp_pos loc, SyntaxError ".ORDER rule cannot have build commands"));
            if effects <> [] || patterns <> [] || scanners <> [] || values <> [] then
               raise (OmakeException (loc_exp_pos loc, SyntaxError ".ORDER rule cannot have patterns, effects, scanners, or values"));
            let sources = List.map snd sources in
            let venv = venv_add_phony venv loc sources in
            let venv = venv_add_orders venv loc sources in
               venv, ValNone

         (* .ORDER rules are handled specially *)
       | [TargetString name] when venv_is_order venv name ->
            let name = Lm_symbol.add name in
               if commands_are_nontrivial then
                  raise (OmakeException (loc_exp_pos loc, SyntaxError ".ORDER rule cannot have build commands"));
               if effects <> [] || scanners <> [] || values <> [] then
                  raise (OmakeException (loc_exp_pos loc, SyntaxError ".ORDER rule cannot have effects, scanners, or values"));
               let venv = eval_ordering_rule venv pos loc name patterns sources in
                  venv, ValNone

       | _ ->
            (* Normal rule *)
            let multiple =
               if multiple then
                  RuleMultiple
               else
                  RuleSingle
            in
            let venv, rules = venv_add_rule venv pos loc multiple targets patterns effects sources scanners values commands in
               venv, ValRules rules

(*
 * Read the OMakefiles in the subdirectories too.
 *)
and eval_subdirs_rule venv loc sources commands =
   List.fold_left (fun venv dir -> eval_subdir venv loc dir commands) venv sources

(*
 * Compile an OMakefile.
 *)
and eval_subdir venv loc (kind, dir) commands =
   let pos = string_pos "eval_subdir" (loc_exp_pos loc) in
   let cache = venv_cache venv in
   let dir = venv_intern_dir venv (string_of_target venv dir) in
   let () =
      if kind <> NodeNormal then
         eprintf "*** omake: .SUBDIR kind %a not implemented@." pp_print_node_kind kind;

      (* Check that the directory exists *)
      if not (Omake_cache.exists_dir cache dir) then
         let create_flag =
            try bool_of_value venv pos (venv_find_var_exn venv ScopeGlobal create_subdirs_sym) with
               Not_found ->
                  false
         in
            if create_flag then
               let name = Dir.fullname dir in
                  try Lm_filename_util.mkdirhier name 0o777 with
                     Unix.Unix_error _ ->
                        raise (OmakeException (pos, StringDirError ("can't create directory", dir)))
            else
               raise (OmakeException (pos, StringDirError ("directory does not exist", dir)))
   in
   let cwd = venv_dir venv in
   let venv' = venv_chdir_dir venv loc dir in
   let node = venv_intern venv' PhonyProhibited makefile_name in
   let name = Node.fullname node in
   let result =
      (*
       * Ignore the file if the commands are listed explicity.
       * The OMakefile can always be included explicitly.
       *)
      if commands <> [] then
         let exp = SequenceExp (loc, commands @ [ReturnSaveExp loc]) in
            eval venv' exp

      (* Otherwise, use the file if it exists *)
      else if Omake_cache.exists cache node then
         let venv' = venv_add_file venv' node in
         let loc = bogus_loc name in
            (*
             * Do not allow implicit exports from the OMakefile.
             * If the user wants the export, they have to do it explicitly
             * with a .SUBDIRS body.
             *)
            match eval_include_file venv' IncludeAll pos loc node with
               ValEnv (venv, _) ->
                  ValEnv (venv, ExportFile)
             | result ->
                  result

      (* Otherwise, check if an empty file is acceptable *)
      else
         let allow_empty_subdirs =
            try bool_of_value venv' pos (venv_find_var_exn venv' ScopeGlobal allow_empty_subdirs_sym) with
               Not_found ->
                  false
         in
            if not allow_empty_subdirs then
               raise (OmakeException (pos, StringNodeError ("file does not exist", node)));
            ValEnv (venv', ExportAll)
   in

   (*
    * Save the resulting environment as the default to use
    * for targets in this directory.  Also change back to the
    * current directory.
    *)
   let result =
      match result with
         ValEnv (venv, syms) ->
            venv_add_dir venv;
            ValEnv (venv_chdir_tmp venv cwd, syms)
       | _ ->
            result
   in
      if debug print_rules then
         eprintf "@[<hv 3>Rules:%a@]@." pp_print_explicit_rules venv;
      fst (add_exports venv pos result)

(*
 * Include all the sources.
 *)
and eval_include_rule venv pos loc sources deps values commands =
   let pos = string_pos "eval_include_rule" pos in

   (* Targets and dependencies *)
   let target =
      match sources with
         [source] -> venv_intern_target venv PhonyProhibited source
       | _ -> raise (OmakeException (pos, StringError ".INCLUDE must have a single source"))
   in
   let venv = venv_add_file venv target in
   let deps = List.map (fun (_, dep) -> venv_intern_target venv PhonyOK dep) deps in

   (* Convert the command list *)
   let commands =
      { Omake_env.command_env = venv;
        Omake_env.command_sources = deps;
        Omake_env.command_values = values;
        Omake_env.command_body = commands
      }
   in
   let commands = eval_commands venv loc target NodeSet.empty [commands] in
   let commands_digest = digest_of_commands pos commands in

   (* Ask the cache if this file is up-to-date *)
   let cache = venv_cache venv in
   let deps = List.fold_left NodeSet.add NodeSet.empty deps in
   let up_to_date = Omake_cache.up_to_date cache include_fun deps commands_digest in
   let () =
      (* Run the commands if there are deps, or if the file does not exist *)
      if commands <> [] && (not up_to_date || Omake_cache.stat cache target = None) then
         exec_commands venv pos loc commands;

      (* Check that it exists *)
      if Omake_cache.force_stat cache target = None then
         raise (OmakeException (pos, StringNodeError (".INCLUDE rule failed to build the target", target)));

      (* Tell the cache we did the update *)
      Omake_cache.add cache include_fun target (NodeSet.singleton target) deps commands_digest MemoSuccess
   in
      include_file venv IncludePervasives pos loc target

(*
 * Evaluate the commands NOW.
 *)
and exec_commands venv pos loc commands =
   let stdin  = channel_of_var venv pos loc stdin_sym in
   let stdout = channel_of_var venv pos loc stdout_sym in
   let stdin  = Lm_channel.descr stdin in
   let stdout = Lm_channel.descr stdout in
      List.iter (fun command ->
            let pid = eval_shell_internal stdin stdout command in
            let status, _ = eval_shell_wait venv pos pid in
            let code =
               match status with
                  Unix.WEXITED i
                | Unix.WSIGNALED i
                | Unix.WSTOPPED i ->
                     i
            in
               if code <> 0 then
                  raise (OmakeException (pos, StringIntError ("command exited with code", code)))) commands

(*
 * Evaluate the command lines.
 *)
and eval_commands venv loc target sloppy_deps commands : arg_command_line list =
   let rec collect commands' commands =
      match commands with
         command :: commands ->
            let { Omake_env.command_env     = venv;
                  Omake_env.command_sources = sources;
                  Omake_env.command_values  = values;
                  Omake_env.command_body    = body
                } = command
            in
            let lines = eval_rule venv loc target sources sloppy_deps values body in
            let commands' = List.rev_append lines commands' in
               collect commands' commands
       | [] ->
            List.rev commands'
   in
      collect [] commands

(*
 * Evaluate the rule lines.
 * Add these extra variables.
 *   $@: the target file
 *   $*: the target file, without suffix
 *   $>: the target, without the directory part and without suffixes
 *   $<: the first source
 *   $+: all the sources
 *   $^: the sources, in alphabetical order, with duplicates removed
 *   $&: the scanner dependencies from the last run
 *)
and eval_rule venv loc target sources sloppy_deps values commands =
   let pos          = string_pos "eval_rule" (loc_exp_pos loc) in
   let target_name  = venv_nodename venv target in
   let root         = Lm_filename_util.root target_name in
   let root'        = Lm_filename_util.strip_suffixes target_name in
   let venv         = venv_add_var venv ScopeGlobal pos star_sym (ValData root) in
   let venv         = venv_add_var venv ScopeGlobal pos gt_sym   (ValData root') in
   let venv         = venv_add_var venv ScopeGlobal pos at_sym   (ValNode target) in
   let source_all   = ValArray (List.map (fun v -> ValNode v) sources) in
   let source_names = List.map (venv_nodename venv) sources in
   let source_set   = List.fold_left LexStringSet.add LexStringSet.empty source_names in
   let source_set   = LexStringSet.to_list source_set in
   let source_set   = ValArray (List.map (fun s -> ValData s) source_set) in
   let source =
      match sources with
         source :: _ -> ValNode source
       | [] -> ValNone
   in
   let venv = venv_add_var venv ScopeGlobal pos plus_sym source_all in
   let venv = venv_add_var venv ScopeGlobal pos hat_sym  source_set in
   let venv = venv_add_var venv ScopeGlobal pos lt_sym   source in
   let sloppy_deps = List.map (fun v -> ValNode v) (NodeSet.to_list sloppy_deps) in
   let venv = venv_add_var venv ScopeGlobal pos amp_sym  (ValArray sloppy_deps) in
   let options = Lm_glob.create_options (glob_options_of_env venv pos) in
   let find_alias = find_alias_of_env venv pos in
   let command_line (commands, fv) command =
      match command with
         CommandSection (_, fv', e) ->
            let commands = ([], CommandEval e) :: commands in
            let fv = free_vars_union fv fv' in
               commands, fv
       | CommandValue (loc, v) ->
            let commands =
               try
                  let flags, pipe = pipe_of_value venv find_alias options pos loc v in
                     (flags, CommandPipe pipe) :: commands
               with
                  OmakeException (_, NullCommand) ->
                     commands
            in
               commands, fv
   in
   let commands, fv = List.fold_left command_line ([], free_vars_empty) commands in
   let commands = List.rev commands in
   let values =
      free_vars_fold (fun values v kind ->
            ValImplicit (loc, kind, v) :: values) values fv
   in
   let values =
      List.fold_left (fun values v ->
            List.rev_append (values_of_value venv pos v) values) [] values
   in
   let values = List.map (eval_prim_value venv pos) values in
   let commands =
      if values = [] then
         commands
      else
         ([], CommandValues values) :: commands
   in
   let dir = venv_dir venv in
      parse_commands venv dir target loc commands

(*
 * Add an ordering constraint.
 *)
and eval_ordering_rule venv pos loc name patterns sources =
   let pos = string_pos "eval_ordering_rule" pos in
   let sources = List.map snd sources in
      List.fold_left (fun venv pattern ->
            venv_add_ordering_rule venv pos loc name pattern sources) venv patterns

(************************************************************************
 * Shell.
 *)

(*
 * Get globbing options from the environment.
 *)
and glob_options_of_env venv pos =
   let options = [] in
   let options =
      try
         let s = venv_find_var_exn venv ScopeGlobal glob_options_sym in
         let s = string_of_value venv pos s in
            glob_options_of_string options s
      with
         Not_found ->
            options
   in
   let options =
      try
         let ignore = venv_find_var_exn venv ScopeGlobal glob_ignore_sym in
         let ignore = strings_of_value venv pos ignore in
            GlobIgnore ignore :: options
      with
         Not_found ->
            options
   in
   let options =
      try
         let allow = venv_find_var_exn venv ScopeGlobal glob_allow_sym in
         let allow = strings_of_value venv pos allow in
            GlobAllow allow :: options
      with
         Not_found ->
            options
   in
      options

and compile_glob_options venv pos =
   Lm_glob.create_options (glob_options_of_env venv pos)

(*
 * Set the path environment variable.
 *)
and eval_path venv pos =
   let pos = string_pos "eval_path" pos in
      try
         let path = venv_find_var_exn venv ScopeGlobal path_sym in
         let path = strings_of_value venv pos path in
         let path = String.concat pathsep path in
            venv_setenv venv path_sym path
      with
         Not_found ->
            venv

(*
 * Evaluate a shell expression.
 *)
and eval_shell_exp venv pos loc e =
   let pos    = string_pos "eval_shell_exp" pos in
   let venv   = eval_path venv pos in
   let find_alias = find_alias_of_env venv pos in
   let options = compile_glob_options venv pos in
   let _, pipe = pipe_of_value venv find_alias options pos loc e in
   let pipe   = normalize_pipe venv pos pipe in
   let stdin  = channel_of_var venv pos loc stdin_sym in
   let stdout = channel_of_var venv pos loc stdout_sym in
   let stderr = channel_of_var venv pos loc stderr_sym in
   let stdin  = Lm_channel.descr stdin in
   let stdout = Lm_channel.descr stdout in
   let stderr = Lm_channel.descr stderr in
   let result = Omake_shell_job.create_job venv pipe stdin stdout stderr in

   (* Get the exit code *)
   let code =
      match result with
         ValInt i
       | ValOther (ValExitCode i) ->
            i
       | _ ->
            0
   in

   (* Check exit code *)
   let exit_on_error =
      try bool_of_value venv pos (venv_find_var_exn venv ScopeGlobal abort_on_command_error_sym) with
         Not_found ->
            false
   in
   let () =
      if exit_on_error && code <> 0 then
         let print_error buf =
            fprintf buf "@[<hv 3>command terminated with code %d:@ %a@]@." code pp_print_string_pipe pipe
         in
            raise (OmakeException (loc_pos loc pos, LazyError print_error))
   in
      venv, result

(*
 * Save the output in a file and return the string.
 *)
and eval_shell_output venv pos loc e =
   let pos = string_pos "eval_shell_output" pos in
   let tmpname = Filename.temp_file "omake" ".shell" in
   let fd = Lm_unix_util.openfile tmpname [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
   let channel = Lm_channel.create tmpname Lm_channel.PipeChannel Lm_channel.OutChannel false (Some fd) in
   let channel = venv_add_channel venv channel in
   let venv = venv_add_var venv ScopeGlobal pos stdout_sym (ValChannel (OutChannel, channel)) in
   let result =
      try
         let _ = eval_shell_exp venv pos loc e in
         let len = Unix.lseek fd 0 Unix.SEEK_END in
         let _ = Unix.lseek fd 0 Unix.SEEK_SET in
         let data = String.create len in
            Lm_unix_util.really_read fd data 0 len;
            Success data
      with
         exn ->
            Exception exn
   in
      venv_close_channel venv pos channel;
      Unix.unlink tmpname;
      match result with
         Success result ->
            result
       | Exception exn ->
            raise exn

(*
 * Construct a shell.
 *)
and eval_shell venv pos =
   let pos = string_pos "eval_shell" pos in
   let venv = eval_path venv pos in
      { shell_eval           = eval_shell_internal;
        shell_info           = eval_shell_info;
        shell_kill           = eval_shell_kill venv pos;
        shell_wait           = eval_shell_wait venv pos;
        shell_error_value    = ValNone;
        shell_print_exp      = pp_print_arg_command_line;
        shell_print_exn      = Omake_exn_print.pp_print_exn;
        shell_is_failure_exn = Omake_exn_print.is_shell_exn
      }

(*
 * Evaluate a shell command using the internal shell.
 *)
and eval_shell_internal stdout stderr command =
   let { command_loc  = loc;
         command_dir  = dir;
         command_venv = venv;
         command_inst = inst
       } = command
   in
   let pos = string_pos "eval_shell_internal" (loc_exp_pos loc) in
      match inst with
         CommandEval e ->
            eval_command venv stdout stderr pos loc e
       | CommandValues _ ->
            ResultPid (0, ValNone)
       | CommandPipe pipe ->
            let pipe = normalize_pipe venv pos pipe in
            let pid =
               if !debug_eval then
                  eprintf "eval_shell_internal: creating job@.";
               Omake_shell_job.create_process venv pipe Unix.stdin stdout stderr
            in
               if !debug_eval then
                  eprintf "eval_shell_internal: created job@.";
               pid

(*
 * Used to evaluate expressions.
 *)
and eval_command venv stdout stderr pos loc e =
   let f stdin stdout stderr =
      if !debug_eval || !debug_shell then
         eprintf "eval_command: evaluating internal function: stderr = %d@." (Lm_unix_util.int_of_fd stderr);
      let venv   = venv_fork venv in
      let stdin  = Lm_channel.create "<stdin>"  Lm_channel.PipeChannel Lm_channel.InChannel  false (Some stdin) in
      let stdout = Lm_channel.create "<stdout>" Lm_channel.PipeChannel Lm_channel.OutChannel false (Some stdout) in
      let stderr = Lm_channel.create "<stderr>" Lm_channel.PipeChannel Lm_channel.OutChannel false (Some stderr) in
      let stdin  = venv_add_channel venv stdin in
      let stdout = venv_add_channel venv stdout in
      let stderr = venv_add_channel venv stderr in
      let venv   = venv_add_var venv ScopeGlobal pos stdin_sym  (ValChannel (InChannel,  stdin)) in
      let venv   = venv_add_var venv ScopeGlobal pos stdout_sym (ValChannel (OutChannel, stdout)) in
      let venv   = venv_add_var venv ScopeGlobal pos stderr_sym (ValChannel (OutChannel, stderr)) in
      let code =
         try
            (match eval venv e with
                ValRules _ ->
                   eprintf "@[<hv 3>*** omake warning:@ %a@ Rule value discarded.@]@." (**)
                      pp_print_pos (loc_pos loc pos)
              | _ ->
                   ());
            0
         with
            ExitException (_, code) ->
               code
          | OmakeException _
          | UncaughtException _ as exn ->
               eprintf "%a@." Omake_exn_print.pp_print_exn exn;
               Omake_state.exn_error_code
          | Unix.Unix_error _
          | Sys_error _
          | Not_found
          | Failure _ as exn ->
               eprintf "%a@." Omake_exn_print.pp_print_exn (UncaughtException (pos, exn));
               Omake_state.exn_error_code
      in
         if !debug_eval then
            eprintf "eval_command: internal function is done: %d@." code;
         venv_close_channel venv pos stdin;
         venv_close_channel venv pos stdout;
         venv_close_channel venv pos stderr;
         code
   in
      if !debug_eval then
         eprintf "eval_command: creating thread, stderr = %d@." (Lm_unix_util.int_of_fd stderr);
      Omake_shell_job.create_thread venv f Unix.stdin stdout stderr

(*
 * Normalize the pipe, so the background is only outermost,
 * and translate commands to aliases.
 *
 * The directory must be an absolute name.
 *)
and normalize_pipe venv pos pipe =
   let pos = string_pos "normalize_pipe" pos in
   let options = Lm_glob.create_options (glob_options_of_env venv pos) in
      normalize_pipe_options venv pos false options pipe

and normalize_pipe_options venv pos squash options (pipe : arg_pipe) : string_pipe =
   match pipe with
      PipeApply (loc, apply) ->
         PipeApply (loc, normalize_apply venv pos loc options apply)
    | PipeCommand (loc, command) ->
         PipeCommand (loc, normalize_command venv pos loc options command)
    | PipeCond (loc, op, pipe1, pipe2) ->
         PipeCond (loc, op, (**)
                      normalize_pipe_options venv pos true options pipe1,
                      normalize_pipe_options venv pos true options pipe2)
    | PipeCompose (loc, divert_stderr, pipe1, pipe2) ->
         PipeCompose (loc, divert_stderr, (**)
                         normalize_pipe_options venv pos true options pipe1,
                         normalize_pipe_options venv pos true options pipe2)
    | PipeGroup (loc, group) ->
         normalize_group venv pos loc options group
    | PipeBackground (loc, pipe) ->
         let pipe = normalize_pipe_options venv pos true options pipe in
            if squash then
               pipe
            else
               PipeBackground (loc, pipe)

(*
 * Normalize an alias.
 *)
and normalize_apply venv pos loc options apply =
   let { apply_env    = env;
         apply_args   = argv;
         apply_stdin  = stdin;
         apply_stdout = stdout
       } = apply
   in
      { apply with apply_env = string_of_env env;
                   apply_args = glob_value_argv venv pos loc options argv;
                   apply_stdin = glob_channel venv pos loc options stdin;
                   apply_stdout = glob_channel venv pos loc options stdout
      }

(*
 * Normalize a command.
 * Glob-expand the arguments, and normalize the redirect names.
 *)
and normalize_command venv pos loc options command =
   let pos = string_pos "normalize_command" pos in
   let { cmd_env    = env;
         cmd_exe    = exe;
         cmd_argv   = argv;
         cmd_stdin  = stdin;
         cmd_stdout = stdout
       } = command
   in
   let exe, args = glob_exe venv pos loc options exe in
   let argv = glob_command_line venv pos loc options argv in
   let argv =
      match args with
         [] ->
            argv
       | _ ->
            List.fold_left (fun argv node ->
                  venv_nodename venv node :: argv) argv (List.rev args)
   in
      { command with cmd_env = string_of_env env;
                     cmd_exe = exe;
                     cmd_argv = argv;
                     cmd_stdin = glob_channel venv pos loc options stdin;
                     cmd_stdout = glob_channel venv pos loc options stdout
      }

(*
 * Normalize a group.
 * Normalize the redirect names.
 *)
and normalize_group venv pos loc options group =
   let pos = string_pos "normalize_group" pos in
   let { group_stdin  = stdin;
         group_stdout = stdout;
         group_pipe = pipe
       } = group
   in
   let group =
      { group with group_stdin  = glob_channel venv pos loc options stdin;
                   group_stdout = glob_channel venv pos loc options stdout;
                   group_pipe   = normalize_pipe_options venv pos false options pipe
      }
   in
      PipeGroup (loc, group)

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
