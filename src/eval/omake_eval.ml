(*
 * Predefined set of functions.
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
open Lm_debug
open Lm_printf
open Lm_symbol
open Lm_location
open Lm_string_set

open Omake_ir
open Omake_env
open Omake_exec
open Omake_wild
open Omake_node
open Omake_util
open Omake_state
open Omake_ir_ast
open Omake_command
open Omake_ir_print
open Omake_node_sig
open Omake_exec_type
open Omake_exec_util
open Omake_cache_type
open Omake_command_type
open Omake_command_digest
open Omake_options_type
open Omake_symbol

module Pos = MakePos (struct let name = "Omake_eval" end)
open Pos

let debug_eval =
   create_debug (**)
      { debug_name = "debug-eval";
        debug_description = "Debug the evaluator";
        debug_value = false
      }

let print_ast =
   create_debug (**)
      { debug_name = "print-ast";
        debug_description = "Print the AST after parsing";
        debug_value = false
      }

let print_ir =
   create_debug (**)
      { debug_name = "print-ir";
        debug_description = "Print the IR after parsing";
        debug_value = false
      }

let print_rules =
    create_debug (**)
      { debug_name = "print-rules";
        debug_description = "Print the rules after evaluation";
        debug_value = false
      }

let print_files =
   create_debug (**)
      { debug_name = "print-files";
        debug_description = "Print the files as they are read";
        debug_value = false
      }

(*
 * Including files.
 *)
type include_flag =
   IncludeFile
 | IncludeSubdir

(************************************************************************
 * Utilities.
 *)

(*
 * Add an optional quote.
 *)
let buffer_add_quote buf = function
   Some c -> Buffer.add_char buf c
 | None -> ()

(*
 * The various forms of empty values.
 *)
let rec is_empty_value v =
   match v with
      ValNone
    | ValString ""
    | ValData ""
    | ValQuote []
    | ValArray []
    | ValRules [] ->
         true
    | ValSequence vl ->
         List.for_all is_empty_value vl
    | ValObject obj ->
         (try is_empty_value (venv_find_field_exn obj builtin_sym) with
             Not_found ->
                false)
    | ValInt _
    | ValFloat _
    | ValData _
    | ValQuote _
    | ValQuoteString _
    | ValString _
    | ValArray _
    | ValApply _
    | ValImplicit _
    | ValFun _
    | ValFunValue _
    | ValPrim _
    | ValRules _
    | ValNode _
    | ValDir _
    | ValEnv _
    | ValBody _
    | ValMap _
    | ValSuperApply _
    | ValMethodApply _
    | ValChannel _
    | ValClass _
    | ValCases _
    | ValOther _
    | ValKey _ ->
         false

(*
 * Check whether a value has an embedded array.
 *)
let rec is_array_value v =
   match v with
      ValArray _ ->
         true
    | ValSequence [v]
    | ValQuote [v] ->
         is_array_value v
    | ValObject obj ->
         (try
             match venv_find_field_exn obj builtin_sym with
                ValArray _ ->
                   true
              | _ ->
                   false
          with
             Not_found ->
                false)
    | ValNone
    | ValInt _
    | ValFloat _
    | ValData _
    | ValQuote _
    | ValQuoteString _
    | ValString _
    | ValApply _
    | ValImplicit _
    | ValSequence _
    | ValFun _
    | ValFunValue _
    | ValPrim _
    | ValRules _
    | ValNode _
    | ValDir _
    | ValEnv _
    | ValBody _
    | ValMap _
    | ValSuperApply _
    | ValMethodApply _
    | ValChannel _
    | ValClass _
    | ValCases _
    | ValOther _
    | ValKey _ ->
         false

(*
 * Get a value from a string_exp.
 *)
let strategy_is_eager be_eager strategy scope =
   match strategy with
      EagerApply ->
         true
    | NormalApply ->
         be_eager || scope = ScopePrivate
    | LazyApply ->
         if be_eager then
            false
         else
            scope = ScopePrivate

(************************************************************************
 * Compiling utilities.
 *)
let postprocess_ir ir =
   let () =
      if debug print_ir then
         eprintf "@[<v 3>IR1:@ %a@]@." Omake_ir_print.pp_print_exp ir
   in
   let ir = Omake_ir_semant.build_prog ir in
   let () =
      if debug print_ir then
         eprintf "@[<v 3>IR2:@ %a@]@." Omake_ir_print.pp_print_exp ir
   in
      ir

(*
 * Parse and evaluate a file.
 *)
let rec parse_ir venv scope node =
   let filename = Node.fullname node in
   let ast = Omake_ast_lex.parse_ast filename in
   let () =
      if debug print_ast then
         eprintf "@[<v 3>AST:@ %a@]@." Omake_ast_print.pp_print_prog ast
   in
   let vars = venv_include_scope venv scope in
   let senv, ir = Omake_ir_ast.compile_prog (senv_create (open_ir venv) vars node) ast in
   let ir = postprocess_ir ir in
   let class_names, vars = senv_class_names senv in
      class_names, vars, ir

(*
 * When constructing a path, the relative filenames
 * should be auto-rehash.
 *
 *    values  : the path
 *    dirname : the subdirectory to search (often ".")
 *)
and path_of_values_select venv pos values dirname =
   let rec collect groups auto_rehash items values =
      match values with
         v :: values ->
            let rehash_flag, dir =
               match v with
                  ValDir dir ->
                     false, dir
                | ValNode node ->
                     let dir = venv_intern_dir venv (string_of_value venv pos v) in
                        false, dir
                | _ ->
                     let s = string_of_value venv pos v in
                     let rehash_flag = not (Lm_filename_util.is_absolute s) in
                     let dir = venv_intern_dir venv s in
                        rehash_flag, dir
            in
            let dir = Dir.chdir dir dirname in
            let groups, items =
               if rehash_flag <> auto_rehash && items <> [] then
                  (auto_rehash, List.rev items) :: groups, [dir]
               else
                  groups, dir :: items
            in
               collect groups rehash_flag items values
       | [] ->
            if items <> [] then
               (auto_rehash, List.rev items) :: groups
            else
               groups
   in
      List.rev (collect [] false [] values)

and path_of_values_rehash venv pos values dirname =
   let dir_of_value v =
      let dir =
         match v with
            ValDir dir ->
               dir
          | _ ->
               venv_intern_dir venv (string_of_value venv pos v)
      in
         Dir.chdir dir dirname
   in
      [true, List.map dir_of_value values]

and path_of_values venv pos values dirname =
   let auto_rehash =
      try bool_of_value venv pos (venv_find_var_exn venv ScopeGlobal auto_rehash_sym) with
         Not_found ->
            false
   in
   let f =
      if auto_rehash then
         path_of_values_rehash
      else
         path_of_values_select
   in
      f venv pos values dirname

(*
 * Open the file.
 * Get the IR and return the vars.
 *)
and find_include_file venv pos loc filename =
   let pos = string_pos "find_include_file" pos in
   let cache = venv_cache venv in
      if not (Filename.is_relative filename) || not (Filename.is_implicit filename) then
         let fullname = filename ^ omake_file_suffix in
         let node1 = venv_intern venv PhonyProhibited fullname in
            if Omake_cache.exists cache node1 then
               node1
            else
               let node2 = venv_intern venv PhonyProhibited filename in
                  if Omake_cache.exists cache node2 then
                     node2
                  else
                     let print_error buf =
                        fprintf buf "@[<hv 3>include file not found, neither file exists:@ %a@ %a@]" (**)
                           pp_print_node node1
                           pp_print_node node2
                     in
                        raise (OmakeException (loc_pos loc pos, LazyError print_error))
      else
         let dirname = Filename.dirname filename in
         let basename = Filename.basename filename in
         let fullname = basename ^ omake_file_suffix in
         let path = venv_find_var venv ScopeGlobal pos loc omakepath_sym in
         let full_path = values_of_value venv pos path in
         let path = path_of_values venv pos full_path dirname in
         let cache = venv_cache venv in
         let listing = Omake_cache.ls_path cache path in
            try
               match Omake_cache.listing_find cache listing fullname with
                  DirEntry dir ->
                     raise (OmakeException (loc_pos loc pos, StringDirError ("is a directory", dir)))
                | NodeEntry node ->
                     node
            with
               Not_found ->
                  try
                     match Omake_cache.listing_find cache listing basename with
                        DirEntry dir ->
                           raise (OmakeException (loc_pos loc pos, StringDirError ("is a directory", dir)))
                      | NodeEntry node ->
                           node
                  with
                     Not_found ->
                        let print_error buf =
                           fprintf buf "@[<hv 3>include file %s not found in OMAKEPATH@ (@[<hv3>OMAKEPATH[] =%a@])@]" (**)
                              filename
                              pp_print_value_list full_path
                        in
                           raise (OmakeException (loc_pos loc pos, LazyError print_error))

and open_ir venv filename pos loc =
   let pos = string_pos "open_ir" pos in
   let source = find_include_file venv pos loc filename in
   let _, vars, _ = compile_ir venv IncludePervasives pos loc source in
      if !print_ir then begin
         eprintf "@[<v 3>Vars: %a" pp_print_node source;
         SymbolTable.iter (fun v mode ->
               eprintf "@ %a = %a" pp_print_symbol v pp_print_scope_kind mode) vars;
         eprintf "@]@."
      end;
      source, vars

(*
 * The include file contains the IR for the file.
 * Try to load the old entry.
 * If it fails, compile the file and save the new entry.
 *)
and compile_ir_info venv scope info pos loc source =
   let _pos = string_pos "compile_ir_info" pos in
      try Static.find_ir info with
         Not_found ->
            let ir = parse_ir venv scope source in
               Static.add_ir info ir;
               ir

and compile_ir venv scope pos loc source =
   let pos = string_pos "compile_ir" pos in
      (*
       * Try to get a cached copy.
       *)
      try venv_find_ir_file_exn venv source with
         Not_found ->
            let ir =
               (*
                * Open the database.
                *)
               let info =
                  try Static.create venv source with
                     Not_found ->
                        raise (OmakeException (loc_pos loc pos, StringNodeError ("can't open IR", source)))
               in
               let ir = compile_ir_info venv scope info pos loc source in
                  Static.close info;
                  ir
            in
               venv_add_ir_file venv source ir;
               ir

(*
 * The object file contains the evaluated file.
 *)
and compile_object compile venv pos loc source =
   let pos = string_pos "compile_ast" pos in

      (*
       * Try to get a cached copy.
       *)
      try venv_find_object_file_exn venv source with
         Not_found ->
            let obj =
               (*
                * Open the database.
                *)
               let info =
                  try Static.create venv source with
                     Not_found ->
                        raise (OmakeException (loc_pos loc pos, StringNodeError ("can't open object", source)))
               in

               (*
                * Try to load the old entry.
                * If it fails, compile the file and save the new entry.
                *)
               let obj =
                  try Static.find_object info with
                     Not_found ->
                        let obj = compile info source in
                           Static.add_object info obj;
                           obj
               in
                  Static.close info;
                  obj
            in
               venv_add_object_file venv source obj;
               obj

(************************************************************************
 * Value operations.
 *)

(*
 * Get the string representation of a value.
 * It not legal to convert an array to a string.
 *)
and string_of_value venv pos v =
   let pos = string_pos "string_of_value" pos in
   let scratch_buf = Buffer.create 32 in
   let dir1 = venv_dir venv in
   let rec collect v =
      match eval_prim_value venv pos v with
         (* Values that expand to nothing *)
         ValNone
       | ValFun _
       | ValFunValue _
       | ValPrim _
       | ValRules _
       | ValBody _
       | ValMap _
       | ValObject _
       | ValEnv _
       | ValChannel _
       | ValClass _
       | ValCases _
       | ValOther _
       | ValArray [] ->
            ()
       | ValSequence vl ->
            List.iter collect vl
       | ValQuote vl ->
            string_of_quote_buf scratch_buf venv pos vl
       | ValQuoteString (c, vl) ->
            Buffer.add_char scratch_buf c;
            string_of_quote_buf scratch_buf venv pos vl;
            Buffer.add_char scratch_buf c
       | ValArray [v] ->
            collect v
       | ValArray vl ->
            let print_error buf =
               fprintf buf "@[<v 3>Array value where string expected:";
               fprintf buf "@ Use the $(string ...) function if you really want to do this";
               fprintf buf "@ @[<v 3>The array has length %d:" (List.length vl);
               ignore (List.fold_left (fun index v ->
                             fprintf buf "@ @[<hv 3>[%d] =@ %a@]" index pp_print_value v;
                             succ index) 0 vl);
               fprintf buf "@]@]@."
            in
               raise (OmakeException (pos, LazyError print_error))
       | ValInt i ->
            Buffer.add_string scratch_buf (string_of_int i)
       | ValFloat x ->
            Buffer.add_string scratch_buf (string_of_float x)
       | ValData s
       | ValString s ->
            Buffer.add_string scratch_buf s
       | ValDir dir2 ->
            Buffer.add_string scratch_buf (Dir.name dir1 dir2)
       | ValNode node ->
            Buffer.add_string scratch_buf (Node.name dir1 node)
       | ValKey _
       | ValApply _
       | ValImplicit _
       | ValSuperApply _
       | ValMethodApply _ ->
            raise (Invalid_argument "string_of_value")
   in
      collect v;
      Buffer.contents scratch_buf

(*
 * Collect the values in a quotation into a string.
 * Even array values are flattened without warning.
 *)
and string_of_quote venv pos c vl =
   let pos = string_pos "string_of_quote" pos in
   let scratch_buf = Buffer.create 32 in
      buffer_add_quote scratch_buf c;
      string_of_quote_buf scratch_buf venv pos vl;
      buffer_add_quote scratch_buf c;
      Buffer.contents scratch_buf

and string_of_quote_buf scratch_buf venv pos vl =
   let pos = string_pos "string_of_quote_buf" pos in
   let dir1 = venv_dir venv in
   let rec collect v =
      match eval_value venv pos v with
         (* Values that expand to nothing *)
         ValNone
       | ValFun _
       | ValFunValue _
       | ValPrim _
       | ValRules _
       | ValBody _
       | ValMap _
       | ValObject _
       | ValEnv _
       | ValChannel _
       | ValClass _
       | ValCases _
       | ValOther _
       | ValArray [] ->
            ()
       | ValSequence vl
       | ValQuote vl ->
            List.iter collect vl
       | ValQuoteString (c, vl) ->
            Buffer.add_char scratch_buf c;
            List.iter collect vl;
            Buffer.add_char scratch_buf c
       | ValArray [v] ->
            collect v
       | ValArray vl ->
            collect_array vl
       | ValInt i ->
            Buffer.add_string scratch_buf (string_of_int i)
       | ValFloat x ->
            Buffer.add_string scratch_buf (string_of_float x)
       | ValData s
       | ValString s ->
            Buffer.add_string scratch_buf s
       | ValDir dir2 ->
            Buffer.add_string scratch_buf (Dir.name dir1 dir2)
       | ValNode node ->
            Buffer.add_string scratch_buf (Node.name dir1 node)
       | ValKey _
       | ValApply _
       | ValSuperApply _
       | ValImplicit _
       | ValMethodApply _ ->
            raise (Invalid_argument "string_of_value")
   and collect_array vl =
      match vl with
         [v] ->
            collect v
       | v :: vl ->
            collect v;
            Buffer.add_char scratch_buf ' ';
            collect_array vl
       | [] ->
            ()
   in
      List.iter collect vl

(*
 * Get a list of values from the value.
 * Array elements are always special, and returned as an element.
 * We divide values into two classes:
 *    The "catenable" values are the values that can be concatenated to
 *    form a string.  These include: string, node, dir, int, float.
 *
 *    Nothing else can be concatenated with a string, and is always preserved
 *    in the value list.
 *)
and values_of_value venv pos v =
   let pos = string_pos "values_of_value" pos in

   (*
    * Convert a catenable value to a string
    *)
   let group tokens  = ValSequence tokens in
   let wrap_string s = ValString s in
   let wrap_data s   = ValData s in
   let wrap_token s  = ValData s in
   let lexer _ _ _   = None in
   let tokens = Lm_string_util.tokens_create_lexer ~lexer ~wrap_string ~wrap_data ~wrap_token ~group in

   (*
    * Array elements are always separate values.
    * The arrays are flattened.
    *)
   let rec collect_array tokens vl vll =
      match vl, vll with
         v :: vl, _ ->
            (match eval_value venv pos v with
                ValArray el ->
                   collect_array tokens el (vl :: vll)
              | ValSequence [v] ->
                   collect_array tokens (v :: vl) vll
              | v ->
                   collect_array (Lm_string_util.tokens_atomic tokens v) vl vll)
       | [], vl :: vll ->
            collect_array tokens vl vll
       | [], [] ->
            tokens
   in

   (*
    * Collect_string is used when we have seen whitespace
    * in a sequence.  Collect the values into the string buffer,
    * then parse the string into separate tokens.
    *)
   let rec collect tokens vl vll =
      match vl, vll with
         v :: vl, _ ->
            let v = eval_catenable_value venv pos v in
               (match v with
                   ValNone ->
                      collect tokens vl vll

                   (* Strings *)
                 | ValString s ->
                      collect (Lm_string_util.tokens_string tokens s) vl vll
                 | ValSequence el ->
                      collect tokens el (vl :: vll)

                   (* Other catenable values *)
                 | ValData _
                 | ValInt _
                 | ValFloat _
                 | ValDir _
                 | ValNode _
                 | ValQuote _
                 | ValQuoteString _ ->
                      collect (Lm_string_util.tokens_add tokens v) vl vll

                   (* Atomic values *)
                 | ValArray el ->
                      collect (collect_array (Lm_string_util.tokens_break tokens) el []) vl vll
                 | ValFun _
                 | ValPrim _
                 | ValRules _
                 | ValEnv _
                 | ValBody _
                 | ValMap _
                 | ValObject _
                 | ValChannel _
                 | ValClass _
                 | ValCases _
                 | ValOther _
                 | ValFunValue _
                 | ValKey _ ->
                      collect (Lm_string_util.tokens_atomic tokens v) vl vll
                 | ValApply _
                 | ValImplicit _
                 | ValSuperApply _
                 | ValMethodApply _ ->
                      raise (OmakeException (pos, StringValueError ("illegal application", v))))
       | [], vl :: vll ->
            collect tokens vl vll
       | [], [] ->
            Lm_string_util.tokens_flush tokens
   in
      collect tokens [v] []

(*
 * Get a string list from the value.
 * This is always legal because arrays have been flattened.
 *)
and strings_of_value venv pos v =
   let values = values_of_value venv pos v in
      List.map (string_of_value venv pos) values

(*
 * Get a list of tokens from the value.
 * This is a lot like the previous function, but we use a lexer
 * for parsing special character sequences.
 *)
and tokens_of_value venv pos lexer v =
   let pos = string_pos "tokens_of_value" pos in

   (*
    * Convert a catenable value to a string
    *)
   let group tokens  = TokGroup tokens in
   let wrap_string s = TokString (ValString s) in
   let wrap_data s   = TokString (ValData s) in
   let wrap_token s  = TokToken s in
   let tokens = Lm_string_util.tokens_create_lexer ~lexer ~wrap_string ~wrap_data ~wrap_token ~group in

   (*
    * Array elements are always separate values.
    * The arrays are flattened.
    *)
   let rec collect_array (tokens : tok Lm_string_util.tokens) vl vll =
      match vl, vll with
         v :: vl, _ ->
            (match eval_value venv pos v with
                ValArray el ->
                   collect_array tokens el (vl :: vll)
              | ValSequence [v] ->
                   collect_array tokens (v :: vl) vll
              | v ->
                   collect_array (Lm_string_util.tokens_atomic tokens (TokString v)) vl vll)
       | [], vl :: vll ->
            collect_array tokens vl vll
       | [], [] ->
            tokens
   in

   (*
    * Collect_string is used when we have seen whitespace
    * in a sequence.  Collect the values into the string buffer,
    * then parse the string into separate tokens.
    *)
   let rec collect (tokens : tok Lm_string_util.tokens) vl vll =
      match vl, vll with
         v :: vl, _ ->
            let v = eval_catenable_value venv pos v in
               (match v with
                   ValNone ->
                      collect tokens vl vll

                   (* Strings *)
                 | ValString s ->
                      collect (Lm_string_util.tokens_lex tokens s) vl vll
                 | ValSequence el ->
                      collect tokens el (vl :: vll)

                   (* Other catenable values *)
                 | ValData _
                 | ValInt _
                 | ValFloat _
                 | ValDir _
                 | ValNode _
                 | ValQuote _ ->
                      collect (Lm_string_util.tokens_add tokens (TokString v)) vl vll
                 | ValQuoteString (_, v) ->
                      collect (Lm_string_util.tokens_add tokens (TokString (ValQuote v))) vl vll

                   (* Atomic values *)
                 | ValArray el ->
                      collect (collect_array (Lm_string_util.tokens_break tokens) el []) vl vll
                 | ValFun _
                 | ValPrim _
                 | ValRules _
                 | ValEnv _
                 | ValBody _
                 | ValMap _
                 | ValObject _
                 | ValChannel _
                 | ValClass _
                 | ValCases _
                 | ValOther _
                 | ValFunValue _
                 | ValKey _ ->
                      collect (Lm_string_util.tokens_atomic tokens (TokString v)) vl vll
                 | ValApply _
                 | ValImplicit _
                 | ValSuperApply _
                 | ValMethodApply _ ->
                      raise (OmakeException (pos, StringValueError ("illegal application", v))))
       | [], vl :: vll ->
            collect tokens vl vll
       | [], [] ->
            Lm_string_util.tokens_flush tokens
   in
      collect tokens [v] []

(*
 * Flatten the value list into a arg_string list.
 * Basically just concatenate all the values, being
 * careful to preserve quoting.  In addition, we want to
 * concatenate adjacent strings of the same type.
 *)
and arg_of_values venv pos vl =
   let pos = string_pos "arg_of_values" pos in

   (*
    * Flatten all sequences.
    *)
   let rec collect is_quoted tokens vl vll =
      match vl, vll with
         v :: vl, _ ->
            let v = eval_value venv pos v in
               (match v with
                   ValNone ->
                      collect is_quoted tokens vl vll

                   (* Strings *)
                 | ValString s ->
                      let tokens =
                         if is_quoted then
                            arg_buffer_add_data tokens s
                         else
                            arg_buffer_add_string tokens s
                      in
                         collect is_quoted tokens vl vll
                 | ValData s ->
                      collect is_quoted (arg_buffer_add_data tokens s) vl vll
                 | ValSequence el ->
                      collect is_quoted tokens el (vl :: vll)
                 | ValArray el ->
                      collect true tokens el (vl :: vll)

                   (* Other quoted values *)
                 | ValInt _
                 | ValFloat _
                 | ValDir _
                 | ValNode _
                 | ValQuote _
                 | ValQuoteString _
                 | ValFun _
                 | ValPrim _
                 | ValRules _
                 | ValEnv _
                 | ValBody _
                 | ValMap _
                 | ValObject _
                 | ValChannel _
                 | ValClass _
                 | ValCases _
                 | ValOther _
                 | ValFunValue _
                 | ValKey _ ->
                      let tokens = arg_buffer_add_data tokens (string_of_value venv pos v) in
                         collect is_quoted tokens vl vll

                   (* Illegal values *)
                 | ValApply _
                 | ValImplicit _
                 | ValSuperApply _
                 | ValMethodApply _ ->
                      raise (OmakeException (pos, StringValueError ("illegal application", v))))
       | [], vl :: vll ->
            collect is_quoted tokens vl vll
       | [], [] ->
            arg_buffer_contents tokens
   in
      collect false arg_buffer_empty vl []

and argv_of_values venv pos vll =
   List.map (arg_of_values venv pos) vll

(*
 * Boolean test.
 * Arrays are always true.
 *)
and bool_of_value venv pos v =
   let values = values_of_value venv pos v in
      match values with
         []
       | [ValNone] ->
            false
       | [ValData s]
       | [ValString s] ->
            Omake_util.bool_of_string s
       | [ValQuote vl] ->
            Omake_util.bool_of_string (string_of_quote venv pos None vl)
       | _ ->
            true

(*
 * The value should be a directory.
 *)
and file_of_value venv pos file =
   let pos = string_pos "file_of_value" pos in
   let file = eval_prim_value venv pos file in
      match file with
         ValNode node ->
            node
       | ValDir dir ->
            Node.node_of_dir dir
       | ValData _
       | ValString _
       | ValSequence _
       | ValQuote _
       | ValQuoteString _
       | ValInt _
       | ValFloat _ ->
            venv_intern venv PhonyExplicit (string_of_value venv pos file)
       | ValArray _
       | ValNone
       | ValKey _
       | ValApply _
       | ValImplicit _
       | ValFun _
       | ValFunValue _
       | ValPrim _
       | ValRules _
       | ValEnv _
       | ValBody _
       | ValMap _
       | ValObject _
       | ValSuperApply _
       | ValMethodApply _
       | ValChannel _
       | ValClass _
       | ValCases _
       | ValOther _ ->
            raise (OmakeException (pos, StringError "illegal value"))

(************************************************************************
 * Evaluation.
 *)

(*
 * Unfold the outermost application to get a real value.
 *)
and eval_value_core venv pos v =
   match v with
      ValKey (loc, v) ->
         eval_key venv pos loc v
    | ValApply (loc, scope, v, []) ->
         eval_value_core venv pos (eval_var venv pos loc (venv_find_var venv scope pos loc v))
    | ValApply (loc, scope, v, args) ->
         eval_value_core venv pos (eval_apply venv pos loc (venv_find_var venv scope pos loc v) args)
    | ValImplicit (loc, scope, v) ->
         let v =
            try Some (venv_find_var_exn venv scope v) with
               Not_found ->
                  None
         in
            (match v with
                Some v -> ValArray [eval_value_core venv pos (eval_var venv pos loc v)]
              | None -> ValNone)
    | ValSuperApply (loc, scope, super, v, args) ->
         let obj = venv_find_super venv pos loc super in
         let v = venv_find_field obj pos v in
            eval_value_core venv pos (eval_apply venv pos loc v args)
    | ValMethodApply (loc, scope, vl, args) ->
         let obj, v = eval_method_var venv scope pos loc vl in
         let venv = venv_with_object venv obj in
            eval_value_core venv pos (eval_apply venv pos loc v args)
    | ValSequence [v] ->
         eval_value_core venv pos v
    | _ ->
         v

and eval_value venv pos v =
   let pos = string_pos "eval_value" pos in
      eval_value_core venv pos v

and eval_single_value venv pos v =
   let pos = string_pos "eval_single_value" pos in
      match eval_value venv pos v with
         ValArray [v] ->
            eval_single_value venv pos v
       | _ ->
            v

and eval_prim_value venv pos v =
   let pos = string_pos "eval_prim_value" pos in
   let v = eval_value venv pos v in
      match v with
         ValArray [v] ->
            eval_prim_value venv pos v
       | ValObject obj ->
            (try venv_find_field_exn obj builtin_sym with
                Not_found ->
                   v)
       | _ ->
            v

(*
 * The values are being flattened, so expand all sequences.
 *)
and eval_catenable_value venv pos v =
   let pos = string_pos "eval_catenable_value" pos in
   let v = eval_value venv pos v in
      match v with
         ValObject obj ->
            (try
                match venv_find_field_exn obj builtin_sym with
                   ValNone
                 | ValString _
                 | ValSequence _
                 | ValData _
                 | ValInt _
                 | ValFloat _
                 | ValDir _
                 | ValNode _
                 | ValArray _
                 | ValRules _ as v ->
                      v
                 | _ ->
                      v
             with
                Not_found ->
                   v)
       | _ ->
            v

(*
 * Evaluate the value in a function body.
 * Expand all applications.
 *)
and eval_body_value venv pos v =
   match eval_value venv pos v with
      ValSequence sl ->
         ValSequence (List.map (eval_body_value venv pos) sl)
    | ValArray sl ->
         ValArray (List.map (eval_body_value venv pos) sl)
    | ValBody (env, body) ->
         let venv = venv_with_env venv env in
         let _, result = eval_exp venv ValNone body in
            result
    | ValNone
    | ValInt _
    | ValFloat _
    | ValData _
    | ValString _
    | ValQuote _
    | ValQuoteString _
    | ValDir _
    | ValNode _
    | ValFun _
    | ValFunValue _
    | ValPrim _
    | ValRules _
    | ValMap _
    | ValObject _
    | ValEnv _
    | ValChannel _
    | ValClass _
    | ValCases _
    | ValOther _ as result ->
         result
    | ValKey _
    | ValApply _
    | ValImplicit _
    | ValSuperApply _
    | ValMethodApply _ ->
         raise (Invalid_argument "eval_body_value")

and eval_body_exp venv pos x v =
   match eval_value venv pos v with
      ValSequence sl ->
         venv, ValSequence (List.map (eval_body_value venv pos) sl)
    | ValArray sl ->
         venv, ValArray (List.map (eval_body_value venv pos) sl)
    | ValBody (env, body) ->
         eval_exp (venv_with_env venv env) x body
    | ValNone
    | ValInt _
    | ValFloat _
    | ValData _
    | ValQuote _
    | ValQuoteString _
    | ValString _
    | ValDir _
    | ValNode _
    | ValFun _
    | ValFunValue _
    | ValPrim _
    | ValRules _
    | ValMap _
    | ValObject _
    | ValEnv _
    | ValChannel _
    | ValClass _
    | ValCases _
    | ValOther _ as result ->
         venv, result
    | ValKey _
    | ValApply _
    | ValImplicit _
    | ValSuperApply _
    | ValMethodApply _ ->
         raise (Invalid_argument "eval_body_exp")

(*
 * Evaluate a variable.
 * It is fine for the variable to evaluate to a function.
 * But if the function has arity 0, then evaluate it.
 *)
and eval_var venv pos loc v =
   match eval_value venv pos v with
      ValFun (_, env, [], body) ->
         let venv = venv_with_env venv env in
         let _, result = eval_exp venv ValNone body in
            result
    | ValFunValue (_, env, [], v) ->
         let venv = venv_with_env venv env in
            eval_body_value venv pos v
    | ValPrim (ArityExact 0, _, f) ->
         venv_apply_prim_fun f venv pos loc []
    | _ ->
         v

(*
 * Evaluate a key.
 *)
and eval_key venv pos loc v =
   try
      let map = eval_map venv pos (venv_find_var_exn venv ScopeProtected map_sym) in
         venv_map_find map pos (ValData v)
   with
      Not_found ->
         raise (OmakeException (loc_pos loc pos, UnboundKey v))

(*
 * Evaluate an application.
 *)
and eval_apply venv pos loc v args =
   match eval_value venv pos v with
      ValFun (_, env, params, body) ->
         let venv = venv_add_args venv pos loc env params args in
         let _, result = eval_exp venv ValNone body in
            result
    | ValFunValue (_, env, params, body) ->
         let venv = venv_add_args venv pos loc env params args in
            eval_body_value venv pos body
    | ValPrim (_, _, f) ->
         venv_apply_prim_fun f venv pos loc args
    | v ->
         if args = [] then
            v
         else
            let print_error buf =
               fprintf buf "@[<v 3>illegal function application:@ @[<hv 3>function:@ %a@]" pp_print_value v;
               List.iter (fun arg ->
                     fprintf buf "@ @[<hv 3>arg = %a@]" pp_print_value arg) args;
               fprintf buf "@]"
            in
               raise (OmakeException (pos, LazyError print_error))

(*
 * Evaluate an application with string arguments.
 *)
and eval_apply_string_exp venv_args venv pos loc v args =
   let pos = string_pos "eval_apply_string_exp" pos in
      match eval_value venv pos v with
         ValFun (_, env, params, body) ->
            let args = List.map (eval_string_exp true venv_args pos) args in
            let venv = venv_add_args venv pos loc env params args in
            let _, result = eval_exp venv ValNone body in
               result
       | ValFunValue (_, env, params, body) ->
            let args = List.map (eval_string_exp true venv_args pos) args in
            let venv = venv_add_args venv pos loc env params args in
               eval_body_value venv pos body
       | ValPrim (_, be_eager, f) ->
            let args = List.map (eval_string_exp be_eager venv_args pos) args in
               venv_apply_prim_fun f venv pos loc args
       | v ->
            if args = [] then
               v
            else
               let print_error buf =
                  fprintf buf "@[<v 3>illegal function application:@ @[<hv 3>function:@ %a@]" pp_print_value v;
                  List.iter (fun arg ->
                        fprintf buf "@ @[<hv 3>arg = %a@]" pp_print_string_exp arg) args;
                  fprintf buf "@]"
               in
                  raise (OmakeException (pos, LazyError print_error))

(*
 * Get a function from a value.
 *)
and eval_fun venv pos v =
   match eval_value venv pos v with
      ValFun (arity, env, params, body) ->
         let f venv pos loc args =
            let venv = venv_add_args venv pos loc env params args in
            let _, result = eval_exp venv ValNone body in
               result
         in
            arity, true, f
    | ValFunValue (arity, env, params, body) ->
         let f venv pos loc args =
            let venv = venv_add_args venv pos loc env params args in
               eval_body_value venv pos body
         in
            arity, true, f
    | ValPrim (arity, be_eager, f) ->
         arity, be_eager, venv_apply_prim_fun f
    | ValBody (env, body) ->
         let arity = ArityExact 0 in
         let f venv pos loc args =
            if args <> [] then
               raise (OmakeException (loc_pos loc pos, ArityMismatch (arity, List.length args)));
            let _, result = eval_exp (venv_with_env venv env) ValNone body in
               result
         in
            arity, true, f
    | _ ->
         raise (OmakeException (pos, StringError "not a function"))

(*
 * Get an object from a variable.
 *)
and eval_map venv pos x =
   match eval_value venv pos x with
      ValMap map ->
         map
    | _ ->
         raise (OmakeException (pos, StringError "not a map"))

and eval_object venv pos x =
   try eval_object_exn venv pos x with
      Not_found ->
         raise (OmakeException (pos, StringError "not an object"))

and eval_object_exn venv pos x =
   match eval_value venv pos x with
      ValObject env ->
         env
    | ValEnv (e, l) ->
         raise Not_found
    | ValInt _
    | ValOther (ValExitCode _) ->
         create_object venv x int_object_sym
    | ValFloat _ ->
         create_object venv x float_object_sym
    | ValData _
    | ValQuote _
    | ValQuoteString _ ->
         create_object venv x string_object_sym
    | ValSequence _
    | ValString _
    | ValNone ->
         create_object venv x sequence_object_sym
    | ValArray _ ->
         create_object venv x array_object_sym
    | ValFun _
    | ValFunValue _
    | ValPrim _ ->
         create_object venv x fun_object_sym
    | ValRules _ ->
         create_object venv x rule_object_sym
    | ValNode _ ->
         create_object venv x file_object_sym
    | ValDir _ ->
         create_object venv x dir_object_sym
    | ValBody _ ->
         create_object venv x body_object_sym
    | ValChannel (InChannel, _) ->
         create_object venv x in_channel_object_sym
    | ValChannel (OutChannel, _) ->
         create_object venv x out_channel_object_sym
    | ValChannel (InOutChannel, _) ->
         create_object venv x in_out_channel_object_sym
    | ValOther (ValLexer _) ->
         create_object venv x lexer_object_sym
    | ValOther (ValParser _) ->
         create_object venv x parser_object_sym
    | ValOther (ValLocation _) ->
         create_object venv x location_object_sym
    | ValOther (ValPosition _) ->
         create_object venv x position_object_sym
    | ValClass _ ->
         raise (Invalid_argument "internal error: dereferenced $class")
    | ValCases _ ->
         raise (Invalid_argument "internal error: dereferenced cases")
    | ValMap _ ->
         create_map venv x map_object_sym
    | ValKey _
    | ValApply _
    | ValImplicit _
    | ValSuperApply _
    | ValMethodApply _ ->
         raise (Invalid_argument "find_object")

and create_object venv x v =
   let obj = venv_find_var_exn venv ScopeGlobal v in
      match obj with
         ValObject env ->
            venv_add_field env builtin_sym x
       | _ ->
            raise Not_found

and create_map venv x v =
   let obj = venv_find_var_exn venv ScopeGlobal v in
      match obj with
         ValObject env ->
            venv_add_field env map_sym x
       | _ ->
            raise Not_found

(*
 * Method paths.
 *)
and eval_method_var_exn venv env pos vl =
   match vl with
      [v] ->
         env, venv_find_field_exn env v
    | v :: vl ->
         let v = venv_find_field_exn env v in
         let obj = eval_object_exn venv pos v in
            eval_method_var_exn venv obj pos vl
    | [] ->
         raise (OmakeException (pos, StringError "empty method name"))

and eval_method_var_aux venv envl pos vl =
   match envl with
      [env] ->
         eval_method_var_exn venv env pos vl
    | env :: envl ->
         (try eval_method_var_exn venv env pos vl with
             Not_found ->
                eval_method_var_aux venv envl pos vl)
    | [] ->
         raise Not_found

and eval_method_var venv scope pos loc vl =
   let envl = venv_current_objects venv scope in
      try eval_method_var_aux venv envl pos vl with
         Not_found ->
            let pos = string_pos "eval_method_var" (loc_pos loc pos) in
               raise (OmakeException (pos, UnboundMethod vl))

(*
 * Simplify a quoted value if possible.
 * Strings are concatenated.
 *)
and simplify_quote_val venv pos c el =
   match el with
      [ValString s]
    | [ValData s] ->
         (match c with
             None ->
                ValData s
           | Some c ->
                ValQuoteString (c, [ValData s]))
    | _ ->
         let buf = Buffer.create 32 in
         let flush vl =
            if Buffer.length buf = 0 then
               vl
            else
               let s = Buffer.contents buf in
                  Buffer.clear buf;
                  ValData s :: vl
         in
         let rec collect vl el =
            match el with
               e :: el ->
                  (match eval_value venv pos e with
                      ValString s
                    | ValData s ->
                         Buffer.add_string buf s;
                         collect vl el
                    | v ->
                         collect (v :: flush vl) el)
             | [] ->
                  List.rev (flush vl)
         in
            match collect [] el with
               [ValString s]
             | [ValData s] ->
                  ValData s
             | el ->
                  match c with
                     None ->
                        ValQuote el
                   | Some c ->
                        ValQuoteString (c, el)

(*
 * Evaluate a string expression.
 *)
and eval_string_exp be_eager venv pos s =
   let pos = string_pos "eval_string_exp" pos in
      match s with
         NoneString _ ->
            ValNone
       | ConstString (_, s) ->
            ValString s
       | KeyString (loc, strategy, v) ->
            if strategy_is_eager be_eager strategy ScopeGlobal then
               eval_key venv pos loc v
            else
               ValKey (loc, v)
       | ApplyString (loc, strategy, scope, v, []) ->
            if strategy_is_eager be_eager strategy scope then
               eval_var venv pos loc (venv_find_var venv scope pos loc v)
            else
               ValApply (loc, scope, v, [])
       | ApplyString (loc, strategy, scope, v, args) ->
            if strategy_is_eager be_eager strategy scope then
               eval_apply_string_exp venv venv pos loc (venv_find_var venv scope pos loc v) args
            else
               let args = List.map (eval_string_exp false venv pos) args in
                  ValApply (loc, scope, v, args)
       | SuperApplyString (loc, strategy, scope, super, v, args) ->
            if strategy_is_eager be_eager strategy scope then
               let obj = venv_find_super venv pos loc super in
               let v = venv_find_field obj pos v in
                  eval_apply_string_exp venv venv pos loc v args
            else
               let args = List.map (eval_string_exp false venv pos) args in
                  ValSuperApply (loc, scope, super, v, args)
       | MethodApplyString (loc, strategy, scope, vl, args) ->
            if strategy_is_eager be_eager strategy scope then
               let obj, v = eval_method_var venv scope pos loc vl in
               let venv_obj = venv_with_object venv obj in
                  eval_apply_string_exp venv venv_obj pos loc v args
            else
               let args = List.map (eval_string_exp false venv pos) args in
                  ValMethodApply (loc, scope, vl, args)
       | SequenceString (loc, sl) ->
            ValSequence (List.map (eval_string_exp be_eager venv pos) sl)
       | BodyString (_, e) ->
            ValBody (venv_get_env venv, e)
       | ArrayString (_, el) ->
            ValArray (List.map (eval_string_exp be_eager venv pos) el)
       | ArrayOfString (_, e) ->
            let v = eval_string_exp be_eager venv pos e in
               ValArray (values_of_value venv pos v)
       | ExpString (_, e) ->
            let _, result = eval_exp venv ValNone e in
               result
       | CasesString (_, cases) ->
            let env = venv_get_env venv in
            let cases =
               List.map (fun (v, e1, e2) ->
                     v, eval_string_exp be_eager venv pos e1, ValBody (env, e2)) cases
            in
               ValCases cases
       | QuoteString (_, el) ->
            simplify_quote_val venv pos None (List.map (eval_string_exp be_eager venv pos) el)
       | QuoteStringString (_, c, el) ->
            simplify_quote_val venv pos (Some c) (List.map (eval_string_exp be_eager venv pos) el)
       | ThisString (_, scope) ->
            ValObject (venv_this_object venv scope)

(*
 * Short forms of the string evaluator.
 *)
and lazy_string_exp venv pos s =
   eval_string_exp false venv pos s

and eager_string_exp venv pos s =
   eval_string_exp true venv pos s

(*
 * Evaluate an expression.
 *)
and eval_exp venv result e =
   let pos = string_pos "eval_exp" (ir_exp_pos e) in
   let venv, result =
      match e with
         LetVarExp (_, scope, v, flag, s) ->
            eval_let_var_exp venv pos scope v flag s
       | LetKeyExp (_, v, flag, s) ->
            eval_let_key_exp venv pos v flag s
       | LetFunExp (loc, scope, v, params, body) ->
            eval_let_fun_exp venv pos loc scope v params body
       | LetObjectExp (_, scope, v, e) ->
            eval_let_object_exp venv pos scope v e
       | LetThisExp (_, e) ->
            eval_let_this_exp venv pos e
       | ShellExp (loc, e) ->
            eval_shell_exp venv pos loc e
       | IfExp (_, cases) ->
            eval_if_exp venv pos cases
       | SequenceExp (_, e) ->
            eval_sequence_exp venv pos e
       | SectionExp (_, _, e) ->
            eval_section_exp venv pos e
       | OpenExp (loc, s) ->
            eval_open_exp venv pos loc s
       | IncludeExp (loc, s, e) ->
            eval_include_exp venv pos loc s e
       | ApplyExp (loc, scope, f, args) ->
            eval_apply_exp venv scope pos loc f args
       | SuperApplyExp (loc, scope, super, v, args) ->
            eval_super_apply_exp venv scope pos loc super v args
       | MethodApplyExp (loc, scope, vl, args) ->
            eval_method_apply_exp venv scope pos loc vl args
       | ReturnCatchExp (_, e) ->
            eval_return_catch_exp venv pos e
       | StringExp (_, s) ->
            eval_string_value_exp venv pos s
       | ReturnExp (loc, s) ->
            eval_return_exp venv pos loc s
       | ExportExp (_, s) ->
            eval_export_exp venv pos s
       | CancelExportExp _ ->
            eval_cancel_export_exp venv pos result
       | ReturnSaveExp _ ->
            eval_return_save_exp venv pos
       | ReturnObjectExp (_, names) ->
            eval_return_object_exp venv pos names
       | KeyExp (loc, v) ->
            eval_key_exp venv pos loc v
       | StaticExp (_, node, key, e) ->
            eval_static_exp venv pos node key e
   in
      add_exports venv pos result

(*
 * Variable definitions.
 *)
and eval_let_var_exp venv pos scope v flag s =
   let pos = string_pos "eval_var_exp" pos in
   let s = eager_string_exp venv pos s in
   let s =
      match flag with
         VarDefNormal ->
            s
       | VarDefAppend ->
            let v = venv_get_var venv scope pos v in
               if is_array_value v || is_array_value s then
                  let vl = values_of_value venv pos v in
                  let sl = values_of_value venv pos s in
                     ValArray (vl @ sl)
               else if is_empty_value v then
                  s
               else if is_empty_value s then
                  v
               else
                  ValSequence [v; ValString " "; s]
   in
   let venv = venv_add_var venv scope pos v s in
      venv, export_none s

(*
 * Key (property) definitions.
 *)
and eval_let_key_exp venv pos v flag s =
   let pos = string_pos "eval_let_key_exp" pos in
   let v = ValData v in
   let s = eager_string_exp venv pos s in

   (* Get the current property list *)
   let map =
      try venv_find_var_exn venv ScopeProtected map_sym with
         Not_found ->
            raise (OmakeException (pos, StringError "current object is not a Map"))
   in
   let map = eval_map venv pos map in

   (* Add the new definition *)
   let s =
      match flag with
         VarDefNormal ->
            s
       | VarDefAppend ->
            let v = venv_map_find map pos v in
               if is_array_value v || is_array_value s then
                  let vl = values_of_value venv pos v in
                  let sl = values_of_value venv pos s in
                     ValArray (vl @ sl)
               else
                  ValSequence [v; ValString " "; s]
   in
   let map = venv_map_add map pos v s in
   let venv = venv_add_var venv ScopeProtected pos map_sym (ValMap map) in
      venv, export_none s

(*
 * Function definitions.
 *)
and eval_let_fun_exp venv pos loc scope v params body =
   let pos = string_pos "eval_fun_exp" pos in
   let env = venv_get_env venv in
   let e = ValFun (ArityExact (List.length params), env, params, body) in
   let venv = venv_add_var venv scope pos v e in
      venv, e

(*
 * Shell expression.
 *)
and eval_shell_exp venv pos loc e =
   let pos = string_pos "eval_shell_exp" pos in
   let v = venv_find_var venv ScopeGlobal pos loc system_sym in
   let s = eager_string_exp venv pos e in
   let result = eval_apply venv pos loc v [s] in
      venv, result

(*
 * Conditionals.
 * The test should expand to a Boolean of some form.
 *)
and eval_if_cases venv pos cases =
   match cases with
      (s, e) :: cases ->
         let s = eager_string_exp venv pos s in
         let b = bool_of_value venv pos s in
            if b then
               let _, result = eval_exp venv ValNone e in
                  result
            else
               eval_if_cases venv pos cases
    | [] ->
         ValNone

and eval_if_exp venv pos cases =
   let pos = string_pos "eval_if_exp" pos in
   let result = eval_if_cases venv pos cases in
      venv, result

(*
 * Sequence.
 *)
and eval_sequence venv pos result el =
   match el with
      [ReturnSaveExp _] ->
         (match result with
             ValEnv _ ->
                venv, result
           | _ ->
                venv, ValEnv (venv, ExportFile))
    | [e] ->
         eval_exp venv result e
    | e :: el ->
         let venv, result = eval_exp venv result e in
            eval_sequence venv pos result el
    | [] ->
         venv, result

and eval_sequence_exp venv pos el =
   let pos = string_pos "eval_sequence_exp" pos in
      eval_sequence venv pos ValNone el

and eval_section_exp venv pos el =
   let pos = string_pos "eval_section_exp" pos in
   let _, result = eval_sequence venv pos ValNone el in
      venv, result

(*
 * Look for a cached object.  If it does not exist,
 * then evaluate the body to create the object.
 * Inline all the fields.
 *)
and eval_static_exp venv pos node key el =
   let pos = string_pos "eval_static_exp" pos in
   let obj =
      try venv_find_static_object venv node key with
         Not_found ->
            (* Evaluate the object, and save it *)
            let _, result = eval_sequence (venv_define_object venv) pos ValNone el in
            let obj = eval_object venv pos result in
               venv_add_static_object venv node key obj;
               obj
   in
   let venv = venv_include_static_object venv obj in
      venv, ValEnv (venv, ExportAll)

(*
 * Object.
 * The argument string is ignored.
 * Push a new object.
 *)
and eval_let_object_exp venv pos scope v el =
   let pos = string_pos "eval_let_object_exp" pos in
   let _, result = eval_sequence (venv_define_object venv) pos ValNone el in
   let venv = venv_add_var venv scope pos v result in
      venv, result

(*
 * This.
 * Set the current object to the given object.
 *)
and eval_let_this_exp venv pos s =
   let pos = string_pos "eval_this_exp" pos in
   let obj = eager_string_exp venv pos s in
   let obj = eval_object venv pos obj in
   let venv = venv_with_object venv obj in
      venv, ValObject obj

(*
 * Include a file.
 * The environment after the file is evaluated is used in the rest
 * of this file.
 *)
and eval_include_exp venv pos loc s commands =
   let pos = string_pos "eval_include" pos in
   let name =
      match eager_string_exp venv pos s with
         ValNode node ->
            (* Use an absolute name, preventing path lookup *)
            Node.absname node
       | name ->
            string_of_value venv pos name
   in
   let node = find_include_file venv pos loc name in
   let venv = venv_add_file venv node in
   let venv = include_file venv IncludePervasives pos loc node in
      venv, ValNone

(*
 * Open a file.
 * Include it if it is not already included.
 *)
and eval_open_exp venv pos loc nodes =
   let pos = string_pos "eval_open" pos in
   let venv =
      List.fold_left (fun venv node ->
            if venv_is_included_file venv node then
               venv
            else
               let venv = venv_add_file venv node in
                  include_file venv IncludePervasives pos loc node) venv nodes
   in
      venv, ValNone

(*
 * Key lookup.
 *)
and eval_key_exp venv pos loc v =
   let pos = string_pos "eval_key_exp" pos in
   let result = eval_key venv pos loc v in
      venv, result

(*
 * Function application.
 *)
and eval_apply_exp venv scope pos loc f args =
   let pos = string_pos "eval_apply_exp" pos in
   let result = eval_apply_string_exp venv venv pos loc (venv_find_var venv scope pos loc f) args in
      venv, result

and eval_super_apply_exp venv scope pos loc super v args =
   let pos = string_pos "eval_super_apply_exp" pos in
   let obj = venv_find_super venv pos loc super in
   let v = venv_find_field obj pos v in
   let result = eval_apply_string_exp venv venv pos loc v args in
      venv, result

and eval_method_apply_exp venv scope pos loc vl args =
   let pos = string_pos "eval_method_apply_exp" pos in
   let obj, v = eval_method_var venv scope pos loc vl in
   let venv' = venv_with_object venv obj in
   let result = eval_apply_string_exp venv venv' pos loc v args in
      venv, result

(*
 * Export the environment or some variables from it.
 *)
and eval_export_exp venv pos s =
   let pos = string_pos "eval_export_exp" pos in
   let result = eager_string_exp venv pos s in
   let result =
      match result with
         ValEnv _ ->
            result
       | _ ->
            let args = strings_of_value venv pos result in
            let syms =
               match args with
                  []
                | ["all"] ->
                     ExportAll
                | ["rules"] ->
                     ExportRules
                | _ ->
                     ExportSymbols (List.map Lm_symbol.add args)
            in
               ValEnv (venv, syms)
   in
      venv, result

and eval_cancel_export_exp venv pos s =
   let v =
      match s with
         ValEnv _ ->
            ValNone
       | _ ->
            s
   in
      venv, v

(*
 * Return a value.  This is just the identity.
 *)
and eval_return_catch_exp venv pos e =
   let _pos = string_pos "eval_return_catch_exp" pos in
      try eval_exp venv ValNone e with
         Return (_, v) ->
            venv, v

and eval_return_exp venv pos loc s =
   let pos = string_pos "eval_return_exp" pos in
   let result = eager_string_exp venv pos s in
      raise (Return (loc, result))

and eval_string_value_exp venv pos s =
   let pos = string_pos "eval_return_exp" pos in
   let result = eager_string_exp venv pos s in
      venv, result

and eval_return_save_exp venv pos =
   let _pos = string_pos "eval_return_save_exp" pos in
   let result = ValEnv (venv, ExportFile) in
      venv, result

and eval_return_object_exp venv pos names =
   let result = venv_current_object venv names in
      venv, ValObject result

(*
 * Include a file.
 *)
and eval_include_file venv scope pos loc node =
   let _, _, ir = compile_ir venv scope pos loc node in
   let _, result = eval_exp venv ValNone ir in
      result

and include_file venv scope pos loc target =
   let pos = string_pos "include_file" pos in
   let venv = venv_add_included_file venv target in
   let result = eval_include_file venv scope pos loc target in
      fst (add_include venv pos result)

(*
 * Parse and evaluate a file as if it were an object.
 *)
and eval_object_file venv pos loc node =
   let parse_obj info node =
      let names, _, ir = compile_ir_info venv IncludePervasives info pos loc node in
      let venv = venv_get_pervasives venv node in
      let venv = venv_define_object venv in
      let venv, result = eval_exp venv ValNone ir in
         venv_current_object venv names
   in
      compile_object parse_obj venv pos loc node

(************************************************************************
 * Evaluator.
 *)
and eval venv e =
   let _, result = eval_exp venv ValNone e in
      result

let eval_open_file = open_ir

(************************************************************************
 * Project compiler.
 *)
let compile venv =
   let node = venv_intern venv PhonyProhibited makeroot_name in
   let venv = venv_add_file venv node in
   let loc = bogus_loc (Node.fullname node) in
   let pos = string_pos "compile" (loc_exp_pos loc) in
   let _ = eval_include_file venv IncludePervasives pos loc node in
      if debug print_rules then
         eprintf "@[<hv 3>Rules:%a@]@." pp_print_explicit_rules venv

(************************************************************************
 * Dependencies.
 *)
let compile_deps venv node buf =
   let deps = Omake_ast_lex.parse_deps buf in
   let vars = venv_include_scope venv IncludePervasives in
   let senv_empty = senv_create (open_ir venv) vars node in
      List.map (fun (target, source, loc) ->
            let pos = string_pos "compile_deps" (loc_exp_pos loc) in
            let target = build_string senv_empty target pos in
            let source = build_string senv_empty source pos in
            let target = eager_string_exp venv pos target in
            let source = eager_string_exp venv pos source in
            let targets = strings_of_value venv pos target in
            let sources = strings_of_value venv pos source in
               targets, sources) deps

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
