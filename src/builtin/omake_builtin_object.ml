(*
 * Builtin objects.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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

open Lm_symbol
open Lm_location
open Lm_string_set

open Omake_ir
open Omake_env
open Omake_eval
open Omake_wild
open Omake_node
open Omake_exec
open Omake_value
open Omake_state
open Omake_symbol
open Omake_build_type
open Omake_cache_type
open Omake_builtin
open Omake_builtin_type
open Omake_builtin_util

module Pos = MakePos (struct let name = "Omake_builtin_object" end)
open Pos

(*
 * Extend an object with another.
 * The argument may be a file or an object.
 *)
let extends_fun venv pos loc args =
   let pos = string_pos "extends" pos in
   let extend_arg venv v =
      let obj =
         match eval_value venv pos v with
            ValObject obj ->
               obj
          | v ->
               object_of_file venv pos loc (string_of_value venv pos v)
      in
         venv_include_object venv obj
   in
   let venv = List.fold_left extend_arg venv args in
      ValEnv (venv, ExportAll)

(*
 * Get the object form of a value.
 *)
let object_fun venv pos loc args =
   let pos = string_pos "object" pos in
   let values =
      match args with
         [arg] ->
            values_of_value venv pos arg
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
   let values = List.map (fun v -> ValObject (eval_object venv pos v)) values in
      concat_array values

(*
 * Import an object from a file.
 * And add that object by its name to the environment.
 *
 * The argument may be a file or an object,
 * but if it is an object, this does nothing.
 *)
let import_fun venv pos loc args =
   let pos = string_pos "import" pos in
   let import_val venv v =
      match eval_value venv pos v with
         ValObject _ ->
            venv
       | _ ->
            let name = string_of_value venv pos v in
            let obj = object_of_file venv pos loc name in
            let name = Filename.basename name in
            let name = Lm_filename_util.root name in
            let name = Lm_symbol.add name in
               venv_add_var venv ScopeGlobal pos name (ValObject obj)
   in
   let import_arg venv arg =
      let values = values_of_value venv pos arg in
         List.fold_left import_val venv values
   in
   let venv = List.fold_left import_arg venv args in
      ValEnv (venv, ExportAll)

(************************************************************************
 * Object operations.
 *)

(*
 * Field membership.
 *)
let object_mem venv pos loc args =
   let pos = string_pos "object-mem" pos in
      match args with
         [arg; v] ->
            let obj = eval_object venv pos arg in
            let s = string_of_value venv pos v in
            let v = Lm_symbol.add s in
               if venv_object_mem obj v then
                  val_true
               else
                  val_false
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

let object_find venv pos loc args =
   let pos = string_pos "object-find" pos in
      match args with
         [arg; v] ->
            let obj = eval_object venv pos arg in
            let s = string_of_value venv pos v in
            let v = Lm_symbol.add s in
               venv_find_field obj pos v
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Add a field to an object.
 *)
let object_add venv pos loc args =
   let pos = string_pos "object-add" pos in
      match args with
         [arg; v; x] ->
            let obj = eval_object venv pos arg in
            let s = string_of_value venv pos v in
            let v = Lm_symbol.add s in
               ValObject (venv_add_field obj v x)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Add a field to an object.
 *)
let object_length venv pos loc args =
   let pos = string_pos "object-length" pos in
      match args with
         [arg] ->
            let obj = eval_object venv pos arg in
               ValInt (venv_object_length obj)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Iterate over the object.
 *)
let object_map venv pos loc args =
   let pos = string_pos "map" pos in
   let f, obj =
      match args with
         [arg; fun_val] ->
            let obj = eval_object venv pos arg in
            let _, _, f = eval_fun venv pos fun_val in
               f, obj
       | [arg; param1; param2; ValBody (env, body)] ->
            let params =
               [Lm_symbol.add (string_of_value venv pos param1);
                Lm_symbol.add (string_of_value venv pos param2)]
            in
            let obj = eval_object venv pos arg in
            let f venv pos loc args =
               let venv = venv_add_args venv pos loc env params args in
                  eval venv body
            in
               f, obj
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (3, 4), List.length args)))
   in

   (* If the body exports the environment, preserve it across calls *)
   let venv', obj =
      venv_object_fold (fun (venv, obj) v x ->
            let result = f venv pos loc [ValString (Lm_symbol.to_string v); x] in
            let obj = venv_add_field obj v result in
            let venv, result = add_exports venv pos result in
               venv, obj) (venv, obj) obj
   in
      if venv' == venv then
         ValObject obj
      else
         ValEnv (venv', ExportAll)

(************************************************************************
 * Map operations.
 *)

(*
 * Map manipulation.
 *)
let map_of_object venv pos obj =
   try
      match venv_find_field_exn obj map_sym with
         ValMap map ->
            map
       | _ ->
            raise Not_found
   with
      Not_found ->
         raise (OmakeException (pos, StringError "object is not a Map"))

let map_of_value venv pos arg =
   let obj = eval_object venv pos arg in
      map_of_object venv pos obj

let wrap_map obj map =
   ValObject (venv_add_field obj map_sym (ValMap map))

(*
 * Field membership.
 *)
let map_mem venv pos loc args =
   let pos = string_pos "map-mem" pos in
      match args with
         [arg; v] ->
            let map = map_of_value venv pos arg in
            let v = key_of_value venv pos v in
               if venv_map_mem map pos v then
                  val_true
               else
                  val_false
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

let map_find venv pos loc args =
   let pos = string_pos "map-find" pos in
      match args with
         [arg; v] ->
            let map = map_of_value venv pos arg in
            let v = key_of_value venv pos v in
               venv_map_find map pos v
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Get the number of elements in the map.
 *)
let map_length venv pos loc args =
   let pos = string_pos "map-length" pos in
      match args with
         [arg] ->
            let map = map_of_value venv pos arg in
               ValInt (venv_map_length map)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Add a field to an object.
 *)
let map_add venv pos loc args =
   let pos = string_pos "map-add" pos in
      match args with
         [arg; v; x] ->
            let obj = eval_object venv pos arg in
            let map = map_of_object venv pos obj in
            let key = key_of_value venv pos v in
               wrap_map obj (venv_map_add map pos key x)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Iterate over the object.
 *)
let map_map venv pos loc args =
   let pos = string_pos "map-map" pos in
   let f, obj, map =
      match args with
         [arg; fun_val] ->
            let obj = eval_object venv pos arg in
            let map = map_of_object venv pos obj in
            let _, _, f = eval_fun venv pos fun_val in
               f, obj, map
       | [arg; param1; param2; ValBody (env, body)] ->
            let params =
               [Lm_symbol.add (string_of_value venv pos param1);
                Lm_symbol.add (string_of_value venv pos param2)]
            in
            let obj = eval_object venv pos arg in
            let map = map_of_object venv pos obj in
            let f venv pos loc args =
               let venv = venv_add_args venv pos loc env params args in
                  eval venv body
            in
               f, obj, map
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (3, 4), List.length args)))
   in

   (* If the body exports the environment, preserve it across calls *)
   let venv', map =
      venv_map_fold (fun (venv, map) v x ->
            let result = f venv pos loc [v; x] in
            let map = venv_map_add map pos v result in
            let venv, result = add_exports venv pos result in
               venv, map) (venv, map) map
   in
      if venv' == venv then
         wrap_map obj map
      else
         ValEnv (venv', ExportAll)

(*
 * \begin{doc}
 * \twofuns{create-map}{create-lazy-map}
 *
 * The \verb+create-map+ is a simplified form for creating \verb+Map+ objects.
 * The \verb+create-map+ function takes an even number of arguments that specify
 * key/value pairs.  For example, the following values are equivalent.
 *
 * \begin{verbatim}
 *     X = $(create-map name1, xxx, name2, yyy)
 *
 *     X. =
 *         extends $(Map)
 *         $|name1| = xxx
 *         $|name2| = yyy
 * \end{verbatim}
 *
 * The \verb+create-lazy-map+ function is similar, but the values are computed
 * lazily.  The following two definitions are equivalent.
 *
 * \begin{verbatim}
 *     Y = $(create-lazy-map name1, $(xxx), name2, $(yyy))
 *
 *     Y. =
 *         extends $(Map)
 *         $|name1| = $`(xxx)
 *         $|name2| = $`(yyy)
 * \end{verbatim}
 *
 * The \verb+create-lazy-map+ function is used in rule construction~\ref{fun:rule}.
 * \end{doc}
 *)
let create_map venv pos loc args =
   let pos = string_pos "create-map" pos in
   let rec collect map args =
      match args with
         key :: value :: args ->
            let key = ValData (string_of_value venv pos key) in
            let map = venv_map_add map pos key value in
               collect map args
       | [_] ->
            raise (OmakeException (loc_pos loc pos, StringError ("create-map requires an even number of arguments")))
       | [] ->
            map
   in
      ValMap (collect venv_map_empty args)

(************************************************************************
 * Generic sequence operations.
 *)

(*
 * Return the number of elements in the sequence.
 *)
let sequence_length venv pos loc args =
   let pos = string_pos "length" pos in
      match args with
         [arg] ->
            let obj = eval_object venv pos arg in
            let arg = eval_object_value venv pos obj in
            let len =
               match arg with
                  ValMap map ->
                     venv_map_length map
                | ValObject obj ->
                     venv_object_length obj
                | ValNone
                | ValEnv _ ->
                     0
                | ValInt _
                | ValFloat _
                | ValNode _
                | ValDir _
                | ValBody _
                | ValChannel _ ->
                     1
                | ValData s ->
                     String.length s
                | ValQuote vl ->
                     String.length (string_of_quote venv pos None vl)
                | ValQuoteString (c, vl) ->
                     String.length (string_of_quote venv pos (Some c) vl)
                | ValSequence _
                | ValString _ ->
                     List.length (values_of_value venv pos arg)
                | ValArray a ->
                     List.length a
                | ValFun (arity, _, _, _)
                | ValFunValue (arity, _, _, _)
                | ValPrim (arity, _, _) ->
                     (match arity with
                         ArityExact i
                       | ArityRange (i, _) ->
                            i
                       | ArityNone ->
                            0
                       | ArityAny ->
                            max_int)
                | ValRules l ->
                     List.length l
                | ValCases cases ->
                     List.length cases
                | ValClass _
                | ValOther _ ->
                     0
                | ValKey _
                | ValApply _
                | ValImplicit _
                | ValSuperApply _
                | ValMethodApply _ ->
                     raise (Invalid_argument "Omake_builtin_base.length")
            in
               ValInt len
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Get the nth element of a sequence.
 *)
let sequence_nth venv pos loc args =
   let pos = string_pos "nth" pos in
      match args with
         [arg; i] ->
            let i = int_of_value venv pos i in
            let obj = eval_object venv pos arg in
            let arg = eval_object_value venv pos obj in
               (match arg with
                   ValNone
                 | ValFun _
                 | ValFunValue _
                 | ValPrim _
                 | ValKey _
                 | ValApply _
                 | ValImplicit _
                 | ValSuperApply _
                 | ValMethodApply _
                 | ValEnv _
                 | ValMap _
                 | ValObject _ ->
                      raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)))
                 | ValInt _
                 | ValFloat _
                 | ValNode _
                 | ValDir _
                 | ValBody _
                 | ValChannel _
                 | ValClass _
                 | ValCases _
                 | ValOther _ ->
                      if i = 0 then
                         arg
                      else
                         raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)))
                 | ValData s ->
                      let len = String.length s in
                         if i < 0 || i >= len then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)));
                         ValData (String.sub s i 1)
                 | ValQuote vl ->
                      let s = string_of_quote venv pos None vl in
                      let len = String.length s in
                         if i < 0 || i >= len then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)));
                         ValData (String.sub s i 1)
                 | ValQuoteString (c, vl) ->
                      let s = string_of_quote venv pos (Some c) vl in
                      let len = String.length s in
                         if i < 0 || i >= len then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)));
                         ValData (String.sub s i 1)

                 | ValSequence _
                 | ValString _ ->
                      let values = values_of_value venv pos arg in
                      let len = List.length values in
                         if i < 0 || i >= len then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)));
                         List.nth values i
                 | ValArray a ->
                      let len = List.length a in
                         if i < 0 || i >= len then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)));
                         List.nth a i
                 | ValRules l ->
                      let len = List.length l in
                         if i < 0 || i >= len then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)));
                         ValRules [List.nth l i])
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Reverse the elements in the sequence.
 *)
let sequence_rev venv pos loc args =
   let pos = string_pos "rev" pos in
      match args with
         [arg] ->
            let obj = eval_object venv pos arg in
            let arg = eval_object_value venv pos obj in
               (match arg with
                   ValNone
                 | ValFun _
                 | ValFunValue _
                 | ValPrim _
                 | ValKey _
                 | ValApply _
                 | ValImplicit _
                 | ValSuperApply _
                 | ValMethodApply _
                 | ValEnv _
                 | ValMap _
                 | ValObject _
                 | ValInt _
                 | ValFloat _
                 | ValNode _
                 | ValDir _
                 | ValBody _
                 | ValChannel _
                 | ValClass _
                 | ValOther _ ->
                      arg
                 | ValData s1 ->
                      let len = String.length s1 in
                      let s2 = String.create len in
                         for i = 0 to len - 1 do
                            s2.[i] <- s1.[len - i - 1]
                         done;
                         ValData s2
                 | ValQuote vl ->
                      let s1 = string_of_quote venv pos None vl in
                      let len = String.length s1 in
                      let s2 = String.create len in
                         for i = 0 to len - 1 do
                            s2.[i] <- s1.[len - i - 1]
                         done;
                         ValData s2
                 | ValQuoteString (c, vl) ->
                      let s1 = string_of_quote venv pos (Some c) vl in
                      let len = String.length s1 in
                      let s2 = String.create len in
                         for i = 0 to len - 1 do
                            s2.[i] <- s1.[len - i - 1]
                         done;
                         ValData s2
                 | ValCases cases ->
                      ValCases (List.rev cases)
                 | ValSequence _
                 | ValString _ ->
                      let values = values_of_value venv pos arg in
                         ValArray (List.rev values)
                 | ValArray a ->
                      ValArray (List.rev a)
                 | ValRules l ->
                      ValRules (List.rev l))
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 0, List.length args)))

(*
 * Map.
 *
 * \begin{doc}
 * \section{Iteration and mapping}
 *
 * \fun{foreach}
 *
 * The \verb+foreach+ function maps a function over a sequence.
 *
 * \begin{verbatim}
 *     $(foreach <fun>, <args>)
 *
 *     foreach(<var>, <args>)
 *        <body>
 * \end{verbatim}
 *
 * For example, the following program defines the variable \verb+X+
 * as an array \verb+a.c b.c c.c+.
 *
 * \begin{verbatim}
 *     X =
 *        foreach(x, a b c)
 *           value $(x).c
 *
 *     # Equivalent expression
 *     X = $(foreach $(fun x, $(x).c), abc)
 * \end{verbatim}
 *
 * There is also an abbreviated syntax.
 *
 * The \verb+export+ form can also be used in a \verb+foreach+
 * body.  The final value of \verb+X+ is \verb+a.c b.c c.c+.
 *
 * \begin{verbatim}
 *     X =
 *     foreach(x, a b c)
 *        X += $(x).c
 *        export
 * \end{verbatim}
 * \end{doc}
 *)
let foreach_fun venv pos loc args =
   let pos = string_pos "foreach" pos in
   let f, args =
      match args with
         [fun_val; arg] ->
            let args = values_of_value venv pos arg in
            let _, _, f = eval_fun venv pos fun_val in
               f, args
       | [ValBody (env, body); param; arg] ->
            let params = [Lm_symbol.add (string_of_value venv pos param)] in
            let args = values_of_value venv pos arg in
            let f venv pos loc args =
               let venv = venv_add_args_hack venv pos loc env params args in
                  eval venv body
            in
               f, args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (2, 3), List.length args)))
   in

   (* If the body exports the environment, preserve it across calls *)
   let venv', values =
      List.fold_left (fun (venv, values) v ->
            let result = f venv pos loc [v] in
            let venv, result = add_exports venv pos result in
               venv, result :: values) (venv, []) args
   in
      if venv' == venv then
         ValArray (List.rev values)
      else
         ValEnv (venv', ExportAll)

(************************************************************************
 * Define the functions.
 *)

(*
 * Add only the builtin functions.
 * The Pervasives file defines most of the remaining methods.
 *)
let () =
   let builtin_funs =
      [true, "extends",              extends_fun,         ArityAny;
       true, "object",               object_fun,          ArityExact 1;
       true, "foreach",              foreach_fun,         ArityExact 2;
       true, "obj-add",              object_add,          ArityExact 3;
       true, "obj-find",             object_find,         ArityExact 2;
       true, "obj-mem",              object_mem,          ArityExact 2;
       true, "obj-length",           object_length,       ArityExact 1;
       true, "obj-map",              object_map,          ArityRange (3, 4);
       true, "map-add",              map_add,             ArityExact 3;
       true, "map-find",             map_find,            ArityExact 2;
       true, "map-mem",              map_mem,             ArityExact 2;
       true, "map-length",           map_length,          ArityExact 1;
       true, "map-map",              map_map,             ArityRange (3, 4);
       true, "sequence-map",         foreach_fun,         ArityRange (2, 3);
       true, "sequence-length",      sequence_length,     ArityExact 1;
       true, "sequence-nth",         sequence_nth,        ArityExact 1;
       true, "sequence-rev",         sequence_rev,        ArityExact 1;
       true,  "create-map",          create_map,          ArityAny;
       false, "create-lazy-map",     create_map,          ArityAny]
   in

   let builtin_vars =
      ["empty-map",        (fun _ -> ValMap venv_map_empty)]
   in

   let builtin_objects =
      ["Int",              value_sym, ValInt 0;
       "Float",            value_sym, ValFloat 0.0;
       "String",           value_sym, ValNone;
       "Array",            value_sym, ValArray [];
       "Fun",              value_sym, ValFunValue (ArityExact 0, venv_empty_env, [], ValNone);
       "Rule",             value_sym, ValRules [];
       "File",             value_sym, ValNone;
       "Dir",              value_sym, ValNone;
       "Body",             value_sym, ValNone;
       "InChannel",        value_sym, ValNone;
       "OutChannel",       value_sym, ValNone;
       "InOutChannel",     value_sym, ValNone;
       "Map",              map_sym,   ValMap venv_map_empty]
   in

   let pervasives_objects =
      ["Object";
       "Number";
       "Sequence";
       "Node";
       "Channel";
       "Exception";
       "RuntimeException";
       "Select";
       "Pipe";
       "Stat";
       "Shell";
       "Lexer";
       "Parser";
       "Location";
       "Position"]
   in
   let builtin_info =
      { builtin_empty with builtin_funs = builtin_funs;
                           builtin_vars = builtin_vars;
                           builtin_objects = builtin_objects;
                           pervasives_objects = pervasives_objects
      }
   in
      register_builtin builtin_info

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
