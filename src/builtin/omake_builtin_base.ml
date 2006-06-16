(*
 * Some builtin functions.
 *
 * \begin{doc}
 * \chapter{Base library}
 * \label{chapter:base}
 * \cutname{omake-base.html}
 * \end{doc}
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

open Lm_symbol
open Lm_location
open Lm_string_set

open Omake_ir
open Omake_env
open Omake_eval
open Omake_wild
open Omake_node
open Omake_exec
open Omake_rule
open Omake_lexer
open Omake_value
open Omake_state
open Omake_symbol
open Omake_builtin
open Omake_builtin_type
open Omake_builtin_util
open Omake_command_type

module Pos = MakePos (struct let name = "Omake_builtin_base" end)
open Pos

(*
 * Table of variables.
 *
 * \begin{doc}
 * \section{Builtin variables}
 * \var{OSTYPE}
 *    Set to the machine architecture \Prog{omake} is running on.  Possible values are
 *    \verb+Unix+ (for all  Unix  versions, including Linux and Mac OS X), \verb+Win32+
 *    (for MS-Windows, OMake compiled with MSVC++ or Mingw), and \verb+Cygwin+ (for
 *    MS-Windows, OMake compiled with Cygwin).
 * \var{SYSNAME}
 *    The name of the operating system for the current machine.
 * \var{NODENAME}
 *    The hostname of the current machine.
 * \varlabel{OS_VERSION}{OS\_VERSION}
 *    The operating system release.
 * \var{MACHINE}
 *    The machine architecture, e.g.\ \verb+i386+, \verb+sparc+, etc.
 * \var{HOST}
 *    Same as \verb+NODENAME+.
 * \varlabel{OMAKE_VERSION}{OMAKE\_VERSION}
 *    Version of OMake.
 * \var{USER}
 *    The login name of the user executing the process.
 * \var{HOME}
 *    The home directory of the user executing the process.
 * \var{PID}
 *    The OMake process id.
 * \var{TARGETS}
 *    The command-line target strings.  For example, if \verb+omake+ is invoked with the
 *    following command line,
 *
 * \begin{verbatim}
 *       omake CFLAGS=1 foo bar.c
 * \end{verbatim}
 *
 *    then \verb+TARGETS+ is defined as \verb+foo bar.c+.
 *
 * \end{doc}
 *)

(************************************************************************
 * Negate a boolean.
 *
 * \begin{doc}
 * \section{Logic, Boolean functions, and control flow}
 * \label{section:logic}
 *
 * Boolean values in omake are represented by case-insensitive strings.  The
 * \emph{false} value can be represented by the strings \verb+false+, \verb+no+,
 * \verb+nil+, \verb+undefined+ or \verb+0+, and everything else is true.
 *
 * \fun{not}
 *
 * \begin{verbatim}
 *    $(not e) : String
 *       e : String
 * \end{verbatim}
 *
 * The \verb+not+ function negates a Boolean value.
 *
 * For example, \verb+$(not false)+ expands to the string \verb+true+, and
 * \verb+$(not hello world)+ expands to \verb+false+.
 * \end{doc}
 *)
let not_fun venv pos loc args =
   let pos = string_pos "not" pos in
      match args with
         [s] ->
            if bool_of_value venv pos s then
               val_false
            else
               val_true
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Check if two values are equal.
 *
 * \begin{doc}
 * \fun{equal}
 *
 * \begin{verbatim}
 *    $(equal e1, e2) : String
 *       e1 : String
 *       e2 : String
 * \end{verbatim}
 *
 * The \verb+equal+ function tests for equality of two values.
 *
 * For example \verb+$(equal a, b)+ expands to \verb+false+, and \verb+$(equal hello world, hello world)+ expands to \verb+true+.
 * \end{doc}
 *)
let equal venv pos loc args =
   let _pos = string_pos "equal" pos in
      match args with
         [s1; s2] ->
            if strings_of_value venv pos s1 = strings_of_value venv pos s2 then
               val_true
            else
               val_false
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Conjunction.
 *
 * \begin{doc}
 * \fun{and}
 *
 * \begin{verbatim}
 *     $(and e1, ..., en) : String
 *        e1, ..., en: Sequence
 * \end{verbatim}
 *
 * The \verb+and+ function evaluates to the conjunction of its arguments.
 *
 * For example, in the following code, \verb+X+ is true, and \verb+Y+ is false.
 *
 * \begin{verbatim}
 *     A = a
 *     B = b
 *     X = $(and $(equal $(A), a) true $(equal $(B), b))
 *     Y = $(and $(equal $(A), a) true $(equal $(A), $(B)))
 * \end{verbatim}
 * \end{doc}
 *)
let and_fun venv pos loc args =
   let pos = string_pos "and" pos in
      if List.for_all (fun arg ->
               List.for_all (bool_of_value venv pos) (values_of_value venv pos arg)) args
      then
         val_true
      else
         val_false

(*
 * Disjunction.
 *
 * \begin{doc}
 * \fun{or}
 *
 * \begin{verbatim}
 *    $(or e1, ..., en) : String
 *       e1, ..., en: String Sequence
 * \end{verbatim}
 *
 * The \verb+or+ function evaluates to the disjunction of its arguments.
 *
 * For example, in the following code, \verb+X+ is true, and \verb+Y+ is false.
 *
 * \begin{verbatim}
 *     A = a
 *     B = b
 *     X = $(or $(equal $(A), a) false $(equal $(A), $(B)))
 *     Y = $(or $(equal $(A), $(B)) $(equal $(A), b))
 * \end{verbatim}
 * \end{doc}
 *)
let or_fun venv pos loc args =
   let pos = string_pos "or" pos in
      if List.exists (fun arg ->
               List.exists (bool_of_value venv pos) (values_of_value venv pos arg)) args
      then
         val_true
      else
         val_false

(*
 * Conditionals.
 * The values are computed lazily.
 *
 * \begin{doc}
 * \form{if}
 *
 * \begin{verbatim}
 *     $(if e1, e2[, e3]) : value
 *        e1 : String
 *        e2, e3 : value
 * \end{verbatim}
 *
 * The \verb+if+ function represents a conditional based on a Boolean value.
 * For example \verb+$(if $(equal a, b), c, d)+ evaluates to \verb+d+.
 *
 * Conditionals may also be declared with an alternate syntax.
 *
 * \begin{verbatim}
 *    if e1
 *       body1
 *    elseif e2
 *       body2
 *    ...
 *    else
 *       bodyn
 * \end{verbatim}
 *
 * If the expression \verb+e1+ is not false, then the expressions in \verb+body1+
 * are evaluated and the result is returned as the value of the conditional.  Otherwise,
 * if \verb+e1+ evaluates to false, the evaluation continues with the \verb+e2+
 * expression.  If none of the conditional expressions is true, then the expressions
 * in \verb+bodyn+ are evaluated and the result is returned as the value
 * of the conditional.
 *
 * There can be any number of \verb+elseif+ clauses; the \verb+else+ clause is
 * optional.
 *
 * Note that each branch of the conditional defines its own scope, so variables
 * defined in the branches are normally not visible outside the conditional.
 * The \verb+export+ command may be used to export the variables defined in
 * a scope.  For example, the following expression represents a common idiom
 * for defining the C compiler configuration.
 *
 * \begin{verbatim}
 *    if $(equal $(OSTYPE), Win32)
 *       CC = cl
 *       CFLAGS += /DWIN32
 *       export
 *    else
 *       CC = gcc
 *       CFLAGS += -g -O2
 *       export
 * \end{verbatim}
 * \end{doc}
 *)
let empty_val = ValSequence []

let if_fun venv pos loc args =
   let pos = string_pos "if" pos in
      let test, v1, v2 =
         match args with
            [test; v1; v2] -> test, v1, v2
          | [test; v1] -> test, v1, empty_val
          | _ -> raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (2, 3), List.length args)))
      in
         if bool_of_value venv pos test then
            v1
         else
            v2

(*
 * Match command.
 *
 * \begin{doc}
 * \fun{switch}
 * \fun{match}
 *
 * The \verb+switch+ and \verb+match+ functions perform pattern matching.
 *
 * \verb+$(switch <arg>, <pattern_1>, <value_1>, ..., <pattern_n>, <value_n>)+
 * \verb+$(match <arg>, <pattern_1>, <value_1>, ..., <pattern_n>, <value_n>)+
 *
 * The number of \verb+<pattern>/<value>+ pairs is arbitrary.  They strictly
 * alternate; the total number of arguments to \verb+<match>+ must be odd.
 *
 * The \verb+<arg>+ is evaluated to a string, and compared with \verb+<pattern_1>+.
 * If it matches, the result of the expression is \verb+<value_1>+.  Otherwise
 * evaluation continues with the remaining patterns until a match is found.
 * If no pattern matches, the value is the empty string.
 *
 * The \verb+switch+ function uses string comparison to compare
 * the argument with the patterns.  For example, the following
 * expression defines the \verb+FILE+ variable to be either
 * \verb+foo+, \verb+bar+, or the empty string, depending
 * on the value of the \verb+OSTYPE+ variable.
 *
 * \begin{verbatim}
 *     FILE = $(switch $(OSTYPE), Win32, foo, Unix, bar)
 * \end{verbatim}
 *
 * The \verb+match+ function uses regular expression patterns (see the
 * \verb+grep+ function).   If a match is found, the variables
 * \verb+$1, $2, ...+ are bound to the substrings matched between
 * \verb+\(+ and \verb+\)+ delimiters.
 * The \verb+$0+ variable contains the entire match, and \verb+$*+
 * is an array of the matched substrings.
 * to the matched substrings.
 *
 * \begin{verbatim}
 *     FILE = $(match foo_xyz/bar.a, foo_\\\(.*\\\)/\\\(.*\\\)\.a, foo_$2/$1.o)
 * \end{verbatim}
 *
 * The \verb+switch+ and \verb+match+ functions also have an alternate (more usable)
 * form.
 *
 * \begin{verbatim}
 *    match e
 *    case pattern1
 *       body1
 *    case pattern2
 *       body2
 *    ...
 *    default
 *       bodyd
 * \end{verbatim}
 *
 * If the value of expression \verb+e+ matches \verb+pattern_i+ and no previous pattern,
 * then \verb+body_i+ is evaluated and returned as the result of the \verb+match+.
 * The \verb+switch+ function uses string comparison; the \verb+match+ function
 * uses regular expression matching.
 *
 * \begin{verbatim}
 *    match $(FILE)
 *    case $".*\(\.[^\/.]*\)"
 *       println(The string $(FILE) has suffix $1)
 *    default
 *       println(The string $(FILE) has no suffix)
 * \end{verbatim}
 * \end{doc}
 *)

(*
 * String pattern matching.
 *)
let rec eval_match_cases1 compare venv pos loc s cases =
   match cases with
      (v, pattern, e) :: cases ->
         if Lm_symbol.eq v case_sym then
            let pattern = string_of_value venv pos pattern in
               match compare venv pos loc pattern s with
                  Some venv ->
                     eval_body_value venv pos e
                | None ->
                     eval_match_cases1 compare venv pos loc s cases
         else if Lm_symbol.eq v default_sym then
            eval_body_value venv pos e
         else
            raise (OmakeException (loc_pos loc pos, StringVarError ("unknown case", v)))
    | [] ->
         ValNone

let rec eval_match_cases2 compare venv pos loc s cases =
   match cases with
      pattern :: e :: cases ->
         let pattern = string_of_value venv pos pattern in
            (match compare venv pos loc pattern s with
                Some venv ->
                   eval_body_value venv pos e
              | None ->
                   eval_match_cases2 compare venv pos loc s cases)
    | [v] ->
         raise (OmakeException (loc_pos loc pos, StringValueError ("match requires an odd number of arguments", v)))
    | [] ->
         ValNone

let eval_match_exp compare venv pos loc args =
   let pos = string_pos "eval_match_exp" pos in
      match args with
         [ValCases cases; arg] ->
            let s = string_of_value venv pos arg in
               eval_match_cases1 compare venv pos loc s cases
       | arg :: rest ->
            let s = string_of_value venv pos arg in
               eval_match_cases2 compare venv pos loc s rest
       | [] ->
            ValNone

let switch_fun =
   let compare venv _ _ s1 s2 =
      if s1 = s2 then
         Some venv
      else
         None
   in
      eval_match_exp compare

let match_fun =
   let compare venv pos loc s1 s2 =
      let lex =
         try lexer_of_string s1
         with Failure err ->
            let msg = sprintf "Mailformed regular expression '%s'" s1 in
               raise (OmakeException (loc_pos loc pos, StringStringError (msg, err)))
      in
      let channel = Lm_channel.of_string s2 in
         match Lexer.search lex channel with
            Some (_, _, _, matched, args) ->
               Some (venv_add_match venv matched args)
          | None ->
               None
   in
      eval_match_exp compare

(*
 * \begin{doc}
 * \form{try}
 *
 * \begin{verbatim}
 *    try
 *       try-body
 *    catch class1(v1)
 *       catch-body
 *    when expr
 *       when-body
 *    ...
 *    finally
 *       finally-body
 * \end{verbatim}
 *
 * The \verb+try+ form is used for exception handling.
 * First, the expressions in the \verb+try-body+ are evaluated.
 *
 * If evaluation results in a value \verb+v+ without raising an
 * exception, then the expressions in the \verb+finally-body+
 * are evaluated and the value \verb+v+ is returned as the result.
 *
 * If evaluation of the \verb+try-body+ results in a exception object \verb+obj+,
 * the \verb+catch+ clauses are examined in order.  When examining \verb+catch+
 * clause \verb+catch class(v)+, if the exception object \verb+obj+
 * is an instance of the class name \verb+class+, the variable \verb+v+ is bound
 * to the exception object, and the expressions in the \verb+catch-body+
 * are evaluated.
 *
 * If a \verb+when+ clause is encountered while a \verb+catch+ body is being evaluated,
 * the predicate \verb+expr+ is evaluated.  If the result is true, evaluation continues
 * with the expressions in the \verb+when-body+.  Otherwise, the next \verb+catch+
 * clause is considered for evaluation.
 *
 * If evaluation of a \verb+catch-body+ or \verb+when-body+ completes successfully,
 * returning a value \verb+v+, without encountering another \verb+when+ clause,
 * then the expressions in the \verb+finally-body+
 * are evaluated and the value \verb+v+ is returned as the result.
 *
 * There can be any number of \verb+catch+ clauses; the \verb+finally+ clause
 * is optional.
 * \end{doc}
 *)

(*
 * Temporary type for evaluating try blocks.
 *)
type try_exp =
   TrySuccessExp of value
 | TryFailureExp of pos * obj * exn

(*
 * Build an object from an OmakeException.
 * The default object is RuntimeException.
 *)
let object_of_omake_exception venv pos exp =
   let pos =
      pp_print_pos stdstr pos;
      flush_stdstr ()
   in
   let exp =
      pp_print_exn stdstr exp;
      flush_stdstr ()
   in
   let obj =
      try
         match venv_find_var_exn venv ScopeGlobal runtime_exception_sym with
            ValObject obj ->
               obj
          | _ ->
               venv_empty_object
      with
         Not_found ->
            venv_empty_object
   in
   let obj = venv_add_field obj pos_sym (ValString pos) in
   let obj = venv_add_field obj message_sym (ValString exp) in
   let obj = venv_add_class obj runtime_exception_sym in
      obj

(*
 * Build an object from an OmakeException.
 * The default object is RuntimeException.
 *)
let object_of_uncaught_exception venv pos exn =
   let pos =
      pp_print_pos stdstr pos;
      flush_stdstr ()
   in
   let obj =
      try
         match venv_find_var_exn venv ScopeGlobal runtime_exception_sym with
            ValObject obj ->
               obj
          | _ ->
               venv_empty_object
      with
         Not_found ->
            venv_empty_object
   in
   let obj = venv_add_field obj pos_sym (ValString pos) in
   let obj = venv_add_field obj message_sym (ValString (Printexc.to_string exn)) in
   let obj = venv_add_class obj runtime_exception_sym in
      obj

(*
 * Exception handling.
 *)
let rec eval_finally_case venv pos cases =
   match cases with
      (v, _, e) :: cases when Lm_symbol.eq v finally_sym ->
         ignore (eval_body_value venv pos e)
    | _ :: cases ->
         eval_finally_case venv pos cases
    | [] ->
         ()

(*
 * We have successfully evaluated a CatchCase.
 * Search for the following WhenCases.
 * If a WhenCase fails, go to the following catch case.
 *)
let rec eval_catch_rest venv_orig venv pos obj result cases =
   match cases with
      (v, s, e) :: cases when Lm_symbol.eq v when_sym ->
         let b = bool_of_value venv pos s in
            if b then
               let venv, result = eval_body_exp venv pos result e in
                  eval_catch_rest venv_orig venv pos obj result cases
            else
               eval_exception venv_orig pos obj cases
    | _ ->
         Some result

and eval_catch_case venv_orig pos v obj e cases =
   let venv = venv_add_var venv_orig ScopeGlobal pos v (ValObject obj) in
   let venv, result = eval_body_exp venv pos ValNone e in
      eval_catch_rest venv_orig venv pos obj result cases

(*
 * Find the first CatchCase that matches the object,
 * and evaluate it.  Do not evaluate the finally case.
 *)
and eval_exception venv pos obj cases =
   match cases with
      (v, s, e) :: cases ->
         if Lm_symbol.eq v when_sym then
            eval_exception venv pos obj cases
         else if Lm_symbol.eq v finally_sym then
            None
         else if Lm_symbol.eq v default_sym || venv_instanceof obj v then
            eval_catch_case venv pos (Lm_symbol.add (string_of_value venv pos s)) obj e cases
         else
            eval_exception venv pos obj cases
    | [] ->
         None

(*
 * The try block is a little complicated because of the finally
 * case.  Note that if an exception occurs in a CatchCase, the
 * FinallyCase *still* needs to be evaluated.
 *)
let try_fun venv pos loc args =
   let pos = string_pos "eval_try_exp" pos in
   let cases, e =
      match args with
         [ValCases cases; e] ->
            cases, e
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))
   in
   let e =
      try
         let result = eval_body_value venv pos e in
            TrySuccessExp result
      with
         OmakeException (pos, exp) as exn ->
            TryFailureExp (pos, object_of_omake_exception venv pos exp, exn)
       | UncaughtException (pos, exn) ->
            TryFailureExp (pos, object_of_uncaught_exception venv pos exn, exn)
       | RaiseException (pos, obj) as exn ->
            TryFailureExp (pos, obj, exn)
   in
   let e =
      match e with
         TryFailureExp (pos, obj, _) ->
            (try
                match eval_exception venv pos obj cases with
                   Some result ->
                      TrySuccessExp result
                 | None ->
                      e
             with
                OmakeException (pos, exp) as exn ->
                   TryFailureExp (pos, object_of_omake_exception venv pos exp, exn)
              | UncaughtException (pos, exn) ->
                   TryFailureExp (pos, object_of_uncaught_exception venv pos exn, exn)
              | RaiseException (pos, obj) as exn ->
                   TryFailureExp (pos, obj, exn))
       | TrySuccessExp _ ->
            e
   in
      eval_finally_case venv pos cases;
      match e with
         TrySuccessExp result ->
            result
       | TryFailureExp (_, _, exn) ->
            raise exn

(*
 * Raise an exception.
 *
 * \begin{doc}
 * \form{raise}
 *
 * \begin{verbatim}
 *    raise exn
 *       exn : Exception
 * \end{verbatim}
 *
 * The \verb+raise+ function raises an exception.
 * The \verb+exn+ object can be any object.  However,
 * the normal convention is to raise an \verb+Exception+
 * object.
 * \end{doc}
 *)
let raise_fun venv pos loc args =
   let pos = string_pos "raise" pos in
      match args with
         [arg] ->
            let obj = eval_value venv pos arg in
            let obj = eval_object venv pos obj in
               raise (RaiseException (pos, obj))
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Exit the program.
 *
 * \begin{doc}
 * \fun{exit}
 *
 * \begin{verbatim}
 *    exit(code)
 *       code : Int
 * \end{verbatim}
 *
 * The \verb+exit+ function terminates \Prog{omake} abnormally.
 *
 * \verb+$(exit <code>)+
 *
 * The \verb+exit+ function takes one integer argument, which is exit code.
 * Non-zero values indicate abnormal termination.
 * \end{doc}
 *)
let exit_fun venv pos loc args =
   let pos = string_pos "exit" pos in
   let code =
      flush stdout;
      flush stderr;
      match args with
         [] ->
            0
       | [s] ->
            (match values_of_value venv pos s with
                [i] ->
                   int_of_value venv pos s
              | [] ->
                   0
              | args ->
                   raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args))))
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
      raise (ExitException (loc_pos loc pos, code))

(*
 * Check whether a variable is defined.
 *
 * \begin{doc}
 * \fun{defined}
 *
 * \begin{verbatim}
 *    $(defined sequence) : String
 *       sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+defined+ function test whether all the variables in the sequence are
 * currently defined.  For example, the following code defines the \verb+X+ variable
 * if it is not already defined.
 *
 * \begin{verbatim}
 *     if $(not $(defined X))
 *        X = a b c
 *        export
 * \end{verbatim}
 * \end{doc}
 *)
let defined venv pos loc args =
   let pos = string_pos "defined" pos in
      match args with
         [arg] ->
            let args = strings_of_value venv pos arg in
            let b =
               List.for_all (fun s ->
                     venv_defined venv ScopeGlobal (Lm_symbol.add s)) args
            in
               if b then
                  val_true
               else
                  val_false
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \fun{defined-env}
 *
 * \begin{verbatim}
 *    $(defined-env sequence) : String
 *       sequence : String
 * \end{verbatim}
 *
 * The \verb+defined-env+ function tests whether a variable is defined
 * as part of the process environment.
 *
 * For example, the following code adds the \verb+-g+ compile
 * option if the environment variable \verb+DEBUG+ is defined.
 *
 * \begin{verbatim}
 * if $(defined-env DEBUG)
 *     CFLAGS += -g
 *     export
 * \end{verbatim}
 * \end{doc}
 *)
let defined_env venv pos loc args =
   let pos = string_pos "defined-env" pos in
      match args with
         [arg] ->
            let args = strings_of_value venv pos arg in
            let b =
               List.for_all (fun s ->
                     venv_defined_env venv (Lm_symbol.add s)) args
            in
               if b then
                  val_true
               else
                  val_false
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Get a variable from the environment.
 *
 * \begin{doc}
 * \fun{getenv}
 *
 * \begin{verbatim}
 *    $(getenv name) : String
 *    $(getenv name, default) : String
 * \end{verbatim}
 *
 * The \verb+getenv+ function gets the value of a variable from
 * the process environment.  The function takes one or two arguments.
 *
 * In the single argument form, an exception is raised if the variable
 * variable is not defined in the environment.  In the two-argument form,
 * the second argument is returned as the result if the value is not
 * defined.
 *
 * For example, the following code defines the variable \verb+X+
 * to be a space-separated list of elements of the \verb+PATH+
 * environment variable if it is defined, and to \verb+/bin /usr/bin+
 * otherwise.
 *
 * \begin{verbatim}
 *     X = $(split $(PATHSEP), $(getenv PATH, /bin:/usr/bin))
 * \end{verbatim}
 *
 * You may also use the alternate form.
 * \begin{verbatim}
 *      getenv(NAME)
 *          default
 * \end{verbatim}
 * \end{doc}
 *)
let getenv venv pos loc args =
   let pos = string_pos "getenv" pos in
   let arg, def =
      match args with
         [arg] ->
            arg, None
       | [(ValBody _) as def; arg]
       | [arg; def] ->
            arg, Some def
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
   let s = string_of_value venv pos arg in
      try ValString (venv_getenv venv (Lm_symbol.add s)) with
         Not_found ->
            match def with
               Some def ->
                  eval_body_value venv pos def
             | None ->
                  raise (OmakeException (loc_pos loc pos, StringStringError ("undefined environment variable", s)))

(*
 * \begin{doc}
 * \fun{setenv}
 *
 * \begin{verbatim}
 *    setenv(name, value)
 *       name : String
 *       value : String
 * \end{verbatim}
 *
 * The \verb+setenv+ function sets the value of a variable in
 * the process environment.  Environment variables are scoped
 * like normal variables.
 *
 * \end{doc}
 *)
let setenv venv pos loc args =
   let pos = string_pos "setenv" pos in
      match args with
         [arg1; arg2] ->
            let v = string_of_value venv pos arg1 in
            let s = string_of_value venv pos arg2 in
            let venv = venv_setenv venv (Lm_symbol.add v) s in
               ValEnv (venv, ExportAll)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * \begin{doc}
 * \fun{get-registry}
 *
 * \begin{verbatim}
 *    get-registry(hkey, key, field) : String
 *    get-registry(hkey, key, field, default) : String
 *        hkey : String
 *        key : String
 *        field : String
 * \end{verbatim}
 *
 * The \verb+get-registry+ function retrieves a string value from the
 * system registry on Win32.  On other architectures, there is no
 * registry.
 *
 * The \verb+hive+ (I think that is the right word), indicates which part
 * of the registry to use.  It should be one of the following values.
 *
 * \begin{itemize}
 * \item \verb+HKEY_CLASSES_ROOT+
 * \item \verb+HKEY_CURRENT_CONFIG+
 * \item \verb+HKEY_CURRENT_USER+
 * \item \verb+HKEY_LOCAL_MACHINE+
 * \item \verb+HKEY_USERS+
 * \end{itemize}
 * Refer to the Microsoft documentation if you want to know what these mean.
 *
 * The \verb+key+ is the field you want to get from the registry.
 * It should have a form like \verb+A\B\C+ (if you use forward slashes, they will
 * be converted to backslashes).  The field is the sub-field of the key.
 *
 * In the 4-argument form, the \verb+default+ is returned on failure.
 * You may also use the alternate form.
 *
 * \begin{verbatim}
 *     get-registry(hkey, key, field)
 *        default
 * \end{verbatim}
 *
 * \end{doc}
 *)
let get_registry venv pos loc args =
   let pos = string_pos "get-registry" pos in
   let hkey, key, field, def =
      match args with
         [hkey; key; field] ->
            hkey, key, field, None
       | [(ValBody _) as def; hkey; key; field]
       | [hkey; key; field; def] ->
            hkey, key, field, Some def
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (3, 4), List.length args)))
   in
   let hkey = String.uppercase (string_of_value venv pos hkey) in
   let hkey_code =
      match hkey with
         "HKEY_CLASSES_ROOT"   -> Lm_unix_util.HKEY_CLASSES_ROOT
       | "HKEY_CURRENT_CONFIG" -> Lm_unix_util.HKEY_CURRENT_CONFIG
       | "HKEY_CURRENT_USER"   -> Lm_unix_util.HKEY_CURRENT_USER
       | "HKEY_LOCAL_MACHINE"  -> Lm_unix_util.HKEY_LOCAL_MACHINE
       | "HKEY_USERS"          -> Lm_unix_util.HKEY_USERS
       | s -> raise (OmakeException (loc_pos loc pos, StringStringError ("unknown hkey", s)))
   in
   let key = String.copy (string_of_value venv pos key) in
   let () =
      for i = 0 to String.length key - 1 do
         if key.[i] = '/' then
            key.[i] <- '\\'
      done
   in
   let field = string_of_value venv pos field in
      try ValString (Lm_unix_util.registry_find hkey_code key field) with
         Not_found ->
            match def with
               Some def ->
                  eval_body_value venv pos def
             | None ->
                  let s = Printf.sprintf "%s\\%s\\%s" hkey key field in
                     raise (OmakeException (loc_pos loc pos, StringStringError ("key not found", s)))

(*
 * Get a variable from the environment.
 *
 * \begin{doc}
 * \fun{getvar}
 *
 * \begin{verbatim}
 *    $(getvar name) : String
 * \end{verbatim}
 *
 * The \verb+getvar+ function gets the value of a variable.
 *
 * An exception is raised if the variable
 * variable is not defined.
 *
 * For example, the following code defines X to be the string abc.
 *
 * \begin{verbatim}
 *     NAME = foo
 *     foo_1 = abc
 *     X = $(getvar $(NAME)_1)
 * \end{verbatim}
 * \end{doc}
 *)
let getvar venv pos loc args =
   let pos = string_pos "getvar" pos in
      match args with
         [arg] ->
            let s = string_of_value venv pos arg in
               venv_find_var venv ScopeGlobal pos loc (Lm_symbol.add s)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \fun{setvar}
 *
 * \begin{verbatim}
 *    setvar(name, value)
 *       name : String
 *       value : String
 * \end{verbatim}
 *
 * The \verb+setvar+ function defines a new variable.  For example, the
 * following code defines the variable \verb+X+ to be the string \verb+abc+.
 *
 * \begin{verbatim}
 *    NAME = X
 *    setvar($(NAME), abc)
 * \end{verbatim}
 * \end{doc}
 *)
let setvar venv pos loc args =
   let pos = string_pos "setvar" pos in
      match args with
         [arg1; arg2] ->
            let v = Lm_symbol.add (string_of_value venv pos arg1) in
            let venv = venv_add_var venv ScopeGlobal pos v arg2 in
               ValEnv (venv, ExportAll)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(************************************************************************
 * Arrays.
 *
 * \begin{doc}
 * \section{Arrays and sequences}
 *
 * \fun{array}
 *
 * \begin{verbatim}
 *     $(array elements) : Array
 *        elements : Sequence
 * \end{verbatim}
 *
 * The \verb+array+ function creates an array from a sequence.
 * If the \verb+<arg>+ is a string, the elements of the array
 * are the whitespace-separated elements of the string, respecting
 * quotes.
 *
 * In addition, array variables can be declared as follows.
 *
 * \begin{verbatim}
 *     A[] =
 *        <val1>
 *        ...
 *        <valn>
 * \end{verbatim}
 *
 * In this case, the elements of the array are exactly
 * \verb+<val1>+, ..., \verb+<valn>+, and whitespace is
 * preserved literally.
 * \end{doc}
 *)
let array_fun venv pos loc args =
   let pos = string_pos "array" pos in
   let args =
      List.fold_left (fun args arg ->
            let args' = values_of_value venv pos arg in
               List.rev_append args' args) [] args
   in
      ValArray (List.rev args)

(*
 * Concatenate the strings with a separator.
 *
 * \begin{doc}
 * \fun{split}
 *
 * \begin{verbatim}
 *    $(split sep, elements) : Array
 *       sep : String
 *       elements : Sequence
 * \end{verbatim}
 *
 * The \verb+split+ function takes two arguments, a string of separators, and
 * a string argument.  The result is an array of elements determined by
 * splitting the elements by all occurrence of the separator in the
 * \verb+elements+ sequence.
 *
 * For example, in the following code, the \verb+X+ variable is
 * defined to be the array \verb+/bin /usr/bin /usr/local/bin+.
 *
 * \begin{verbatim}
 *     PATH = /bin:/usr/bin:/usr/local/bin
 *     X = $(split :, $(PATH))
 * \end{verbatim}
 *
 * The \verb+sep+ argument may be omitted. In this case \verb+split+ breaks its
 * arguments along the white space. Quotations are not split.
 * \end{doc}
 *)
let split_fun venv pos loc args =
   let pos = string_pos "split" pos in
   let strings =
      match args with
         [arg] ->
            let args = strings_of_value venv pos arg in
            let args =
               List.fold_left (fun args s ->
                     List.rev_append (Lm_string_util.tokens_std s) args) [] args
            in
               List.rev args
       | [sep; arg] ->
            let sep = string_of_value venv pos sep in
            let args = strings_of_value venv pos arg in
            let args =
               List.fold_left (fun args s ->
                     List.rev_append (Lm_string_util.split sep s) args) [] args
            in
               List.rev args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))
   in
      concat_strings strings

(*
 * Concatenate the strings with a separator.
 *
 * \begin{doc}
 * \fun{concat}
 *
 * \begin{verbatim}
 *    $(concat sep, elements) : String
 *       sep : String
 *       elements : Sequence
 * \end{verbatim}
 *
 * The \verb+concat+ function takes two arguments, a separator string, and
 * a sequence of elements.  The result is a string formed by concatenating
 * the elements, placing the separator between adjacent elements.
 *
 * For example, in the following code, the \verb+X+ variable is
 * defined to be the string \verb+foo_x_bar_x_baz+.
 *
 * \begin{verbatim}
 *     X = foo  bar     baz
 *     Y = $(concat _x_, $(X))
 * \end{verbatim}
 * \end{doc}
 *)
let concat_fun venv pos loc args =
   let pos = string_pos "concat" pos in
      match args with
         [sep; arg] ->
            let sep = string_of_value venv pos sep in
            let args = strings_of_value venv pos arg in
               ValData (String.concat sep args)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Length of a list.
 *
 * \begin{doc}
 * \fun{length}
 *
 * \begin{verbatim}
 *    $(length sequence) : Int
 *       sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+length+ function returns the number of elements in its argument.
 *
 * For example, the expression \verb+$(length a  b "c d")+ evaluates to 3.
 * \end{doc}
 *)
let length_fun venv pos loc args =
   let pos = string_pos "length" pos in
      match args with
         [arg] ->
            let args = values_of_value venv pos arg in
               ValInt (List.length args)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Get the nth element of a list.
 *
 * \begin{doc}
 * \fun{nth}
 *
 * \begin{verbatim}
 *    $(nth i, sequence) : value
 *       i : Int
 *       sequence : Sequence
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+nth+ function returns the nth element of its argument, treated as
 * a list. Counting starts at 0. An exception is raised if the index is not in bounds.
 *
 * For example, the expression \verb+$(nth 1, a "b c" d)+ evaluates to \verb+"b c"+.
 * \end{doc}
 *)
let nth_fun venv pos loc args =
   let pos = string_pos "nth" pos in
      match args with
         [i; arg] ->
            let i = int_of_value venv pos i in
            let args = values_of_value venv pos arg in
            let len = List.length args in
               if i < 0 || i >= len then
                  raise (OmakeException (loc_pos loc pos, StringIntError ("index is out of bounds", i)));
               List.nth args i
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Get a subrange of a list.
 *
 * \begin{doc}
 * \fun{nth-hd}
 *
 * \begin{verbatim}
 *    $(nth-hd i, sequence) : value
 *       i : Int
 *       sequence : Sequence
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+nth-hd+ function returns the first \verb+i+ elements of
 * the sequence.  An exception is raised if the sequence is not
 * at least \verb+i+ elements long.
 *
 * For example, the expression \verb+$(nth-hd 2, a "b c" d)+ evaluates to \verb+a "b c"+.
 *
 * \fun{nth-tl}
 *
 * \begin{verbatim}
 *    $(nth-tl i, sequence) : value
 *       i : Int
 *       sequence : Sequence
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+nth-tl+ function skips \verb+i+ elements of the sequence
 * and returns the rest.  An exception is raised if the sequence is not
 * at least \verb+i+ elements long.
 *
 * For example, the expression \verb+$(nth-tl 1, a "b c" d)+ evaluates to \verb+"b c" d+.
 *
 * \fun{subrange}
 *
 * \begin{verbatim}
 *    $(subrange off, len, sequent) : value
 *       off : Int
 *       len : Int
 *       sequence : Sequence
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+subrange+ function returns a subrange of the sequence.
 * Counting starts at 0.  An exception is raised if the specified
 * range is not in bounds.
 *
 * For example, the expression \verb+$(subrange 1, 2, a "b c" d e)+ evaluates to \verb+"b c" d+.
 * \end{doc}
 *)
let rec nth_hd l_rev l i =
   if i = 0 then
      List.rev l_rev
   else
      match l with
         h :: l ->
            nth_hd (h :: l_rev) l (pred i)
       | [] ->
            raise (Invalid_argument "nth_hd")

let rec nth_tl l i =
   if i = 0 then
      l
   else
      match l with
         _ :: l ->
            nth_tl l (pred i)
       | [] ->
            raise (Invalid_argument "nth_tl")

let sub l off len =
   nth_hd [] (nth_tl l off) len

let nth_hd_fun venv pos loc args =
   let pos = string_pos "nth-hd" pos in
      match args with
         [i; arg] ->
            let i = int_of_value venv pos i in
            let args = values_of_value venv pos arg in
            let len = List.length args in
               if i < 0 || i > len then
                  raise (OmakeException (loc_pos loc pos, StringIntError ("index is out of bounds", i)));
               ValArray (nth_hd [] args i)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

let nth_tl_fun venv pos loc args =
   let pos = string_pos "nth-tl" pos in
      match args with
         [i; arg] ->
            let i = int_of_value venv pos i in
            let args = values_of_value venv pos arg in
            let len = List.length args in
               if i < 0 || i > len then
                  raise (OmakeException (loc_pos loc pos, StringIntError ("index is out of bounds", i)));
               ValArray (nth_tl args i)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

let subrange_fun venv pos loc args =
   let pos = string_pos "subrange" pos in
      match args with
         [off; len; arg] ->
            let off = int_of_value venv pos off in
            let len = int_of_value venv pos len in
            let args = values_of_value venv pos arg in
            let alen = List.length args in
               if off < 0 || len < 0 || off + len > alen then
                  raise (OmakeException (loc_pos loc pos, StringIntError ("one or more indexes are out of bounds", off + len)));
               ValArray (sub args off len)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 3, List.length args)))

(*
 * Reverse a list.
 *
 * \begin{doc}
 * \fun{rev}
 *
 * \begin{verbatim}
 *     $(rev sequence) : Sequence
 *        sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+rev+ function returns the elements of a sequence in reverse order.
 * For example, the expression \verb+$(rev a "b c" d)+ evaluates to \verb+d "b c" a+.
 * \end{doc}
 *)
let rev_fun venv pos loc args =
   let pos = string_pos "rev" pos in
      match args with
         [arg] ->
            let args = values_of_value venv pos arg in
               ValArray (List.rev args)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \fun{string}
 *
 * \begin{verbatim}
 *    $(string sequence) : String
 *       sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+string+ function flattens a sequence into a single string.
 * This is similar to the \verb+concat+ function, but the elements are
 * separated by whitespace.  The result is treated as a unit; whitespace
 * is significant.
 * \end{doc}
 *)
let string venv pos loc args =
   let pos = string_pos "string" pos in
      match args with
         [arg] ->
            let args = strings_of_value venv pos arg in
            let s = String.concat " " args in
               ValData s
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \fun{string-escaped}
 *
 * \begin{verbatim}
 *    $(string-escaped sequence) : String Array
 *       sequence : Array
 * \end{verbatim}
 *
 * The \verb+string-escaped+ function converts each element of its
 * argument to a string, escaping characters that are special to omake.
 * The special characters include \verb+:)(,+ and shitespace.
 *
 * \begin{verbatim}
 *     string-escaped($"a b" $"y:z")
 *     a\ b y\:z
 * \end{verbatim}
 * \end{doc}
 *)
let is_escape c =
   match c with
      ' '
    | '\t'
    | '\n'
    | ':'
    | ')'
    | '('
    | ',' ->
         true
    | _ ->
         false

let escape_length s =
   let len = String.length s in
   let rec collect amount i =
      if i = len then
         amount
      else if is_escape s.[i] then
         collect (amount + 2) (i + 1)
      else
         collect (amount + 1) (i + 1)
   in
      collect 0 0

let copy_string esc_length src_length s =
   let esc_string = String.create esc_length in
   let rec copy esc_index src_index =
      if src_index <> src_length then
         let c = s.[src_index] in
            if is_escape c then begin
               esc_string.[esc_index] <- '\\';
               esc_string.[esc_index + 1] <- c;
               copy (esc_index + 2) (src_index + 1)
            end
            else begin
               esc_string.[esc_index] <- c;
               copy (esc_index + 1) (src_index + 1)
            end
   in
      copy 0 0;
      esc_string

let single_escaped s =
   let src_length = String.length s in
   let esc_length = escape_length s in
      if esc_length = src_length then
         s
      else
         copy_string esc_length src_length s

let string_escaped venv pos loc args =
   let pos = string_pos "string-escaped" pos in
      match args with
         [arg] ->
            let args = strings_of_value venv pos arg in
            let args = List.map (fun s -> ValData (single_escaped s)) args in
               ValArray args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \fun{quote}
 *
 * \begin{verbatim}
 *    $(quote sequence) : String
 *       sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+quote+ function flattens a sequence into a single string
 * and adds quotes around the string.  Inner quotation symbols are
 * escaped.
 *
 * For example, the expression \verb+$(quote a "b c" d)+ evaluates
 * to \verb+"a \"b c\" d"+, and \verb+$(quote abc)+ evaluates to
 * \verb+"abc"+.
 * \end{doc}
 *)
let quote venv pos loc args =
   let pos = string_pos "quote" pos in
      match args with
         [arg] ->
            let argv = strings_of_value venv pos arg in
            let s = Lm_string_util.quote_argv argv in
               ValData s
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \fun{quote-argv}
 *
 * \begin{verbatim}
 *    $(quote-argv sequence) : String
 *       sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+quote-argv+ function flattens a sequence into a single string,
 * and adds quotes around the string.  The quotation is formed so that
 * a command-line parse can separate the string back into its components.
 * \end{doc}
 *)
let quote_argv venv pos loc args =
   let pos = string_pos "quote-argv" pos in
      match args with
         [arg] ->
            let argv = strings_of_value venv pos arg in
            let s = Lm_string_util.concat_argv argv in
               ValData s
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \fun{html-string}
 *
 * \begin{verbatim}
 *    $(html-string sequence) : String
 *       sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+html-string+ function flattens a sequence into a single string,
 * and escaped special HTML characters.
 * This is similar to the \verb+concat+ function, but the elements are
 * separated by whitespace.  The result is treated as a unit; whitespace
 * is significant.
 * \end{doc}
 *)
let html_string venv pos loc args =
   let pos = string_pos "html-string" pos in
      match args with
         [arg] ->
            let args = strings_of_value venv pos arg in
            let s = String.concat " " args in
            let s = Lm_string_util.html_escaped_nonwhite s in
               ValData s
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Add a suffix.
 *
 * \begin{doc}
 * \fun{addsuffix}
 *
 * \begin{verbatim}
 *    $(addsuffix suffix, sequence) : Array
 *       suffix : String
 *       sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+addsuffix+ function adds a suffix to each component of sequence.
 * The number of elements in the array is exactly the same as the number of
 * elements in the sequence.
 *
 * For example, \verb+$(addsuffix .c, a b "c d")+ evaluates to \verb+a.c b.c "c d".c+.
 * \end{doc}
 *)
let addsuffix venv pos loc args =
   let pos = string_pos "addsuffix" pos in
      match args with
         [suffix; arg] ->
            let args = values_of_value venv pos arg in
            let args = List.map (fun v -> ValSequence [v; suffix]) args in
               ValArray args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Add a suffix.
 *
 * \begin{doc}
 * \fun{mapsuffix}
 *
 * \begin{verbatim}
 *    $(mapsuffix suffix, sequence) : Array
 *       suffix : value
 *       sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+mapsuffix+ function adds a suffix to each component of sequence.
 * It is similar to \verb+addsuffix+, but uses array concatenation instead
 * of string concatenation.  The number of elements in the array is
 * twice the number of elements in the sequence.
 *
 * For example, \verb+$(mapsuffix .c, a b "c d")+ evaluates to \verb+a .c b .c "c d" .c+.
 * \end{doc}
 *)
let mapsuffix venv pos loc args =
   let pos = string_pos "mapsuffixe" pos in
      match args with
         [suffix; arg] ->
            let args = values_of_value venv pos arg in
            let args = List.map (fun v -> ValArray [v; suffix]) args in
               ValArray args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Add all suffixes.
 *
 * \begin{doc}
 * \fun{addsuffixes}
 *
 * \begin{verbatim}
 *    $(addsuffixes suffixes, sequence) : Array
 *       suffixes : Sequence
 *       sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+addsuffixes+ function adds all suffixes in its first argument
 * to each component of a sequence.  If \verb+suffixes+ has \verb+n+ elements,
 * and \verb+sequence+ has \verb+m+ elements, the the result has \verb+n * m+ elements.
 *
 * For example, the \verb+$(addsuffixes .c .o, a b c)+ expressions evaluates to
 * \verb+a.c a.o b.c b.o c.o c.a+.
 * \end{doc}
 *)
let addsuffixes venv pos loc args =
   let pos = string_pos "addsuffixes" pos in
      match args with
         [suffix; arg] ->
            let suffixes = strings_of_value venv pos suffix in
            let suffixes = List.map (fun s -> ValString s) suffixes in
            let args = values_of_value venv pos arg in
            let args = List.map (fun suffix -> List.map (fun s -> ValSequence [s; suffix]) args) suffixes in
               ValArray (List.flatten args)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * \begin{doc}
 * \fun{removeprefix}
 *
 * \begin{verbatim}
 *    $(removeprefix prefix, sequence) : Array
 *       prefix : String
 *       sequence : Array
 * \end{verbatim}
 *
 * The \verb+removeprefix+ function removes a prefix from each component
 * of a sequence.
 * \end{doc}
 *)
let removeprefix venv pos loc args =
   let pos = string_pos "removeprefix" pos in
      match args with
         [pre; arg] ->
            let pre = string_of_value venv pos pre in
            let args = strings_of_value venv pos arg in
            let plen = String.length pre in
            let args =
               List.map (fun s ->
                     if Lm_string_util.equal_substring s 0 pre then
                        String.sub s plen (String.length s - plen)
                     else
                        s) args
            in
               concat_strings args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Remove suffixes.
 *
 * \begin{doc}
 * \fun{removesuffix}
 *
 * \begin{verbatim}
 *    $(removesuffix sequence) : Array
 *       sequence : String
 * \end{verbatim}
 *
 * The \verb+removesuffix+ function removes the suffixes from each component
 * of a sequence.
 *
 * For example, \verb+$(removesuffix a.c b.foo "c d")+ expands to \verb+a b "c d"+.
 * \end{doc}
 *)
let removesuffix venv pos loc args =
   let pos = string_pos "removesuffix" pos in
      match args with
         [arg] ->
            let args = strings_of_value venv pos arg in
            let args = List.map Lm_filename_util.root args in
               concat_strings args
       | [suffix; arg] ->
            let suffix = string_of_value venv pos suffix in
            let args = strings_of_value venv pos arg in
            let slen = String.length suffix in
            let args =
               List.map (fun s ->
                     let len = String.length s in
                     let off = len - slen in
                        if Lm_string_util.equal_substring s off suffix then
                           String.sub s 0 off
                        else
                           s) args
            in
               concat_strings args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (1, 2), List.length args)))

(*
 * Replace suffixes.
 *
 * \begin{doc}
 * \fun{replacesuffixes}
 *
 * \begin{verbatim}
 *    $(replacesuffixes old-suffixes, new-suffixes, sequence) : Array
 *       old-suffixes : Sequence
 *       new-suffixes : Sequence
 *       sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+replacesuffixes+ function modifies the suffix of each component
 * in  sequence.  The \verb+old-suffixes+ and \verb+new-suffixes+ sequences
 * should have the same length.
 *
 * For example, \verb+$(replacesuffixes, .h .c, .o .o, a.c b.h c.z)+ expands to \verb+a.o b.o c.z+.
 * \end{doc}
 *)
let replacesuffixes venv pos loc args =
   let pos = string_pos "replacesuffixes" pos in
      match args with
         [old_suffixes; new_suffixes; files] ->
            let old_suffixes = strings_of_value venv pos old_suffixes in
            let new_suffixes = strings_of_value venv pos new_suffixes in
            let files = strings_of_value venv pos files in
            let len1 = List.length old_suffixes in
            let len2 = List.length new_suffixes in
            let _ =
               if len1 <> len2 then
                  raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact len1, len2)))
            in
            let table =
               List.fold_left2 StringTable.add StringTable.empty old_suffixes new_suffixes
            in
            let files =
               List.map (fun file ->
                     let root, old_suffix = Lm_filename_util.split file in
                        try
                           let new_suffix = StringTable.find table old_suffix in
                              root ^ new_suffix
                        with
                           Not_found ->
                              file) files
            in
               concat_strings files
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 3, List.length args)))

(*
 * Add a prefix.
 *
 * \begin{doc}
 * \fun{addprefix}
 *
 * \begin{verbatim}
 *    $(addprefix prefix, sequence) : Array
 *       prefix : String
 *       sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+addprefix+ function adds a prefix to each component of a sequence.
 * The number of element in the result array is exactly the same as the number
 * of elements in the argument sequence.
 *
 * For example, \verb+$(addprefix foo/, a b "c d")+ evaluates to \verb+foo/a foo/b foo/"c d"+.
 * \end{doc}
 *)
let addprefix venv pos loc args =
   let pos = string_pos "addprefix" pos in
      match args with
         [prefix; arg] ->
            let args = values_of_value venv pos arg in
            let args = List.map (fun v -> ValSequence [prefix; v]) args in
               ValArray args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Add a prefix.
 *
 * \begin{doc}
 * \fun{mapprefix}
 *
 * \begin{verbatim}
 *    $(mapprefix prefix, sequence) : Array
 *       prefix : String
 *       sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+mapprefix+ function adds a prefix to each component of a sequence.
 * It is similar to \verb+addprefix+, but array concatenation is used instead of
 * string concatenation.  The result array contains twice as many elements
 * as the argument sequence.
 *
 * For example, \verb+$(mapprefix foo, a b "c d")+ expands to \verb+foo a foo b foo "c d"+.
 * \end{doc}
 *)
let mapprefix venv pos loc args =
   let pos = string_pos "mapprefix" pos in
      match args with
         [prefix; arg] ->
            let args = values_of_value venv pos arg in
            let args = List.map (fun v -> ValArray [prefix; v]) args in
               ValArray args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Add both prefix and suffix.
 *
 * \begin{doc}
 * \fun{add-wrapper}
 *
 * \begin{verbatim}
 *    $(add-wrapper prefix, suffix, sequence) : Array
 *       prefix : String
 *       suffix : String
 *       sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+add-wrapper+ functions adds both a prefix and a suffix to each component of a sequence.
 * For example, the expression \verb+$(add-wrapper dir/, .c, a b)+ evaluates to
 * \verb+dir/a.c dir/b.c+.  String concatenation is used.  The array result
 * has the same number of elements as the argument sequence.
 * \end{doc}
 *)
let add_wrapper venv pos loc args =
   let pos = string_pos "add-wrapper" pos in
      match args with
         [prefix; suffix; arg] ->
            let args = values_of_value venv pos arg in
            let args = List.map (fun s -> ValSequence [prefix; s; suffix]) args in
               ValArray args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 3, List.length args)))

(*
 * Eliminate duplicates.
 *
 * \begin{doc}
 * \fun{set}
 *
 * \begin{verbatim}
 *    $(set sequence) : Array
 *       sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+set+ function sorts a set of string components, eliminating duplicates.
 *
 * For example, \verb+$(set z y z "m n" w a)+ expands to \verb+"m n" a w y z+.
 * \end{doc}
 *)
let set venv pos loc args =
   let pos = string_pos "set" pos in
      match args with
         [files] ->
            let files = strings_of_value venv pos files in
            let files = List.fold_left StringSet.add StringSet.empty files in
            let files = StringSet.to_list files in
               ValArray (List.map (fun s -> ValString s) files)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Set membership.
 *
 * \begin{doc}
 * \fun{mem}
 *
 * \begin{verbatim}
 *    $(mem elem, sequence) : Boolean
 *       elem : String
 *       sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+mem+ function tests for membership in a sequence.
 *
 * For example, \verb+$(mem "m n", y z "m n" w a)+ evaluates to \verb+true+,
 * while \verb+$(mem m n, y z "m n" w a)+ evaluates to \verb+false+.
 * \end{doc}
 *)
let mem venv pos loc args =
   let pos = string_pos "mem" pos in
      match args with
         [s; set] ->
            let s = Lm_string_util.trim (string_of_value venv pos s) in
            let set = strings_of_value venv pos set in
               if List.mem s set then
                  val_true
               else
                  val_false
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Set intersection.
 *
 * \begin{doc}
 * \fun{intersection}
 *
 * \begin{verbatim}
 *    $(intersection sequence1, sequence2) : Array
 *       sequence1 : Sequence
 *       sequence2 : Sequence
 * \end{verbatim}
 *
 * The \verb+intersection+ function takes two arguments, treats them
 * as sets of strings, and computes their intersection.  The order of the result
 * is undefined, and it may contain duplicates.  Use the \verb+set+
 * function to sort the result and eliminate duplicates in the result
 * if desired.
 *
 * For example, the expression \verb+$(intersection c a b a, b a)+ evaluates to
 * \verb+a b a+.
 * \end{doc}
 *)
let intersection venv pos loc args =
   let pos = string_pos "intersection" pos in
   let rec intersect l = function
      h :: t ->
         if List.mem h l then
            h :: intersect l t
         else
            intersect l t
    | [] -> []
   in
      match args with
         [files1; files2] ->
            let files1 = strings_of_value venv pos files1 in
            let files2 = strings_of_value venv pos files2 in
            let files = intersect files1 files2 in
               ValArray (List.map (fun s -> ValString s) files)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * \begin{doc}
 * \fun{intersects}
 *
 * \begin{verbatim}
 *    $(intersects sequence1, sequence2) : Boolean
 *       sequence1 : Sequence
 *       sequence2 : Sequence
 * \end{verbatim}
 *
 * The \verb+intersects+ function tests whether two sets have a non-empty intersection.
 * This is slightly more efficient than computing the intersection and testing whether
 * it is empty.
 *
 * For example, the expression \verb+$(intersects a b c, d c e)+ evaluates to \verb+true+,
 * and \verb+$(intersects a b c a, d e f)+ evaluates to \verb+false+.
 * \end{doc}
 *)
let intersects venv pos loc args =
   let pos = string_pos "intersects" pos in
   let rec intersects l = function
      h::t ->
         List.mem h l || intersects l t
    | [] ->
         false
   in
      match args with
         [files1; files2] ->
            let files1 = strings_of_value venv pos files1 in
            let files2 = strings_of_value venv pos files2 in
               if intersects files1 files2 then
                  val_true
               else
                  val_false
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Set subtraction.
 *
 * \begin{doc}
 * \fun{set-diff}
 *
 * \begin{verbatim}
 *    $(set-diff sequence1, sequence2) : Array
 *       sequence1 : Sequence
 *       sequence2 : Sequence
 * \end{verbatim}
 *
 * The \verb+set-diff+ function takes two arguments, treats them
 * as sets of strings, and computes their difference (all the elements of the
 * first set that are not present in the second one).  The order of the result
 * is undefined and it may contain duplicates.  Use the \verb+set+
 * function to sort the result and eliminate duplicates in the result
 * if desired.
 *
 * For example, the expression \verb+$(set-diff c a b a e, b a)+ evaluates to
 * \verb+c e+.
 * \end{doc}
 *)
let set_diff venv pos loc args =
   let pos = string_pos "set_diff" pos in
   let rec diff l = function
      h :: t ->
         if List.mem h l then
            diff l t
         else
            h :: diff l t
    | [] -> []
   in
      match args with
         [files1; files2] ->
            let files1 = strings_of_value venv pos files1 in
            let files2 = strings_of_value venv pos files2 in
            let files = diff files2 files1 in
               ValArray (List.map (fun s -> ValString s) files)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Include all files that do not match the pattern.
 *
 * \begin{doc}
 * \fun{filter}
 *
 * \begin{verbatim}
 *    $(filter patterns, sequence) : Array
 *       patterns : Sequence
 *       sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+filter+ function picks elements from a sequence.
 * The patterns is a non-empty sequence of patterns, each may contain one occurrence of the wildcard
 * \verb+%+ character.
 *
 * For example \verb+$(filter %.h %.o, a.c x.o b.h y.o "hello world".c)+ evaluates to \verb+x.o b.h y.o+.
 * \end{doc}
 *)
let compile_patterns venv loc pos patterns =
   let patterns = strings_of_value venv pos patterns in
   let rec f = function
      [] ->
         (fun f -> false)
    | pattern :: patterns ->
         let f = f patterns in
            if is_wild pattern then
               let wild = wild_compile pattern in
                  (fun s -> wild_matches wild s || f s)
            else
               (fun s -> s = pattern || f s)
   in
      f patterns

let filter venv pos loc args =
   let pos = string_pos "filter" pos in
      match args with
         [patterns; arg] ->
            let args = strings_of_value venv pos arg in
            let args = List.filter (compile_patterns venv loc pos patterns) args in
               ValArray (List.map (fun s -> ValString s) args)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Include all files that do not match the pattern.
 *
 * \begin{doc}
 * \fun{filter-out}
 *
 * \begin{verbatim}
 *    $(filter-out patterns, sequence) : Array
 *       patterns : Sequence
 *       sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+filter-out+ function removes elements from a sequence.
 * The patterns is a non-empty sequence of patterns, each may contain one occurrence of the wildcard
 * \verb+%+ character.
 *
 * For example \verb+$(filter-out %.c %.h, a.c x.o b.h y.o "hello world".c)+ evaluates to \verb+x.o y.o+.
 * \end{doc}
 *)
let filter_out venv pos loc args =
   let pos = string_pos "filter-out" pos in
      match args with
         [patterns; arg] ->
            let args = strings_of_value venv pos arg in
            let f = compile_patterns venv loc pos patterns in
            let args = List.filter (fun s -> not (f s)) args in
               ValArray (List.map (fun s -> ValString s) args)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Capitalize some words.
 *
 * \begin{doc}
 * \fun{capitalize}
 *
 * \begin{verbatim}
 *    $(capitalize sequence) : Array
 *       sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+capitalize+ function capitalizes each word in a sequence.
 * For example, \verb+$(capitalize through the looking Glass)+ evaluates to
 * \verb+Through The Looking Glass+.
 * \end{doc}
 *)
let capitalize venv pos loc args =
   let pos = string_pos "capitalize" pos in
      match args with
         [arg] ->
            let args = strings_of_value venv pos arg in
            let args = List.map String.capitalize args in
               concat_strings args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Uncapitalize some words.
 *
 * \begin{doc}
 * \fun{uncapitalize}
 *
 * \begin{verbatim}
 *    $(uncapitalize sequence) : Array
 *       sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+uncapitalize+ function uncapitalizes each word in its argument.
 *
 * For example, \verb+$(uncapitalize through the looking Glass)+ evaluates to
 * \verb+through the looking glass+.
 * \end{doc}
 *)
let uncapitalize venv pos loc args =
   let pos = string_pos "uncapitalize" pos in
      match args with
         [arg] ->
            let args = strings_of_value venv pos arg in
            let args = List.map String.uncapitalize args in
               concat_strings args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Capitalize some words.
 *
 * \begin{doc}
 * \fun{uppercase}
 *
 * \begin{verbatim}
 *    $(uppercase sequence) : Array
 *       sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+uppercase+ function converts each word in a sequence to uppercase.
 * For example, \verb+$(uppercase through the looking Glass)+ evaluates to
 * \verb+THROUGH THE LOOKING GLASS+.
 * \end{doc}
 *)
let uppercase venv pos loc args =
   let pos = string_pos "uppercase" pos in
      match args with
         [arg] ->
            let args = strings_of_value venv pos arg in
            let args = List.map String.uppercase args in
               concat_strings args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Uncapitalize some words.
 *
 * \begin{doc}
 * \fun{lowercase}
 *
 * \begin{verbatim}
 *    $(lowercase sequence) : Array
 *       sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+lowercase+ function reduces each word in its argument to lowercase.
 *
 * For example, \verb+$(lowercase through tHe looking Glass)+ evaluates to
 * \verb+through the looking glass+.
 * \end{doc}
 *)
let lowercase venv pos loc args =
   let pos = string_pos "lowercase" pos in
      match args with
         [arg] ->
            let args = strings_of_value venv pos arg in
            let args = List.map String.lowercase args in
               concat_strings args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \fun{system}
 *
 * \begin{verbatim}
 *    system(s)
 *       s : Sequence
 * \end{verbatim}
 *
 * The \verb+system+ function is used to evaluate a shell expression.
 * This function is used internally by \Prog{omake} to evaluate
 * shell commands.
 *
 * For example, the following program is equivalent to the
 * expression \verb+system(ls foo)+.
 *
 * \begin{verbatim}
 *    ls foo
 * \end{verbatim}
 * \end{doc}
 *)
let system venv pos loc args =
   let pos = string_pos "system" pos in
      match args with
         [arg] ->
            let _, result = eval_shell_exp venv pos loc arg in
               result
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Shell command.
 *
 * \begin{doc}
 * \fun{shell}
 *
 * \begin{verbatim}
 *    $(shell command) : Array
 *    $(shella command) : Array
 *    $(shell-code command) : Int
 *       command : Sequence
 * \end{verbatim}
 *
 * The \verb+shell+ function evaluates a command using the command shell,
 * and returns the whitespace-separated words of the standard output as the result.
 *
 * The \verb+shella+ function acts similarly, but it returns the lines
 * as separate items in the array.
 *
 * The \verb+shell-code+ function returns the exit code.  The output is not
 * diverted.
 *
 * For example, if the current directory contains the files \verb+OMakeroot+,
 * \verb+OMakefile+, and \verb+hello.c+, then \verb+$(shell ls)+ evaluates to
 * \verb+hello.c OMakefile OMakeroot+ (on a Unix system).
 * \end{doc}
 *)
let shell_aux venv pos loc args =
   let pos = string_pos "shell" pos in
      match args with
         [arg] ->
            eval_shell_output venv pos loc arg
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let shell venv pos loc args =
   let s = shell_aux venv pos loc args in
   let args = values_of_value venv pos (ValString s) in
      concat_array args

let shella venv pos loc args =
   let s = shell_aux venv pos loc args in
   let len = String.length s in
   let buf = Buffer.create 32 in
   let flush lines =
      let s = Buffer.contents buf in
         Buffer.clear buf;
         if s = "" then
            lines
         else
            ValString s :: lines
   in
   let rec collect lines i =
      if i = len then
         flush lines
      else
         match s.[i] with
            '\r'
          | '\n' ->
               collect (flush lines) (succ i)
          | c ->
               Buffer.add_char buf c;
               collect lines (succ i)
   in
      ValArray (List.rev (collect [] 0))

let shell_code venv pos loc args =
   let pos = string_pos "shell-code" pos in
   let venv = venv_add_var venv ScopeGlobal pos abort_on_command_error_sym val_false in
   let _, result =
      match args with
         [arg] ->
            eval_shell_exp venv pos loc arg
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
      match result with
         ValInt _ ->
            result
       | ValOther (ValExitCode i) ->
            ValInt i
       | _ ->
            ValInt 0

(*
 * Exports.
 *)
let export venv pos loc args =
   let pos = string_pos "export" pos in
      match args with
         [] ->
            ValEnv (venv, ExportAll)
       | [(ValEnv _) as result] ->
            result
       | [arg] ->
            let args = strings_of_value venv pos arg in
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
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (0, 1), List.length args)))

(*
 * Loop.
 * \begin{doc}
 * \hypertarget{while}{}
 * \fun{while}
 *
 * \begin{verbatim}
 *    while <test>
 *       <body>
 * \end{verbatim}
 *
 * --or--
 *
 * \begin{verbatim}
 *     while <test>
 *     case <test1>
 *        <body1>
 *     ...
 *     case <testn>
 *        <bodyn>
 *     default
 *        <bodyd>
 * \end{verbatim}
 *
 * The loop is executed while the test is true.
 * In the first form, the \verb+<body>+ is executed on every loop iteration.
 * In the second form, the body \verb+<bodyI>+ is selected, as the first
 * case where the test \verb+<testI>+ is true.  If none apply, the optional
 * default case is evaluated.  If no cases are true, the loop exits.
 * The environment is automatically exported.
 *
 * Examples.
 *
 * Iterate for \verb+i+ from \verb+0+ to \verb+9+.
 *
 * \begin{verbatim}
 *     i = 0
 *     while $(lt $i, 10)
 *        echo $i
 *        i = $(add $i, 1)
 * \end{verbatim}
 *
 * The following example is equivalent.
 *
 * \begin{verbatim}
 *    i = 0
 *    while true
 *    case $(lt $i, 10)
 *       echo $i
 *       i = $(add $i, 1)
 * \end{verbatim}
 *
 * The following example is similar, but some special cases are printed.
 * value is printed.
 *
 * \begin{verbatim}
 *     i = 0
 *     while $(lt $i, 10)
 *     case $(equal $i, 0)
 *        echo zero
 *     case $(equal $i, 1)
 *        echo one
 *     default
 *        echo $i
 * \end{verbatim}
 * \end{doc}
 *)
let rec eval_while_cases venv pos loc orig_cases arg cases =
   match cases with
      (v, pattern, e) :: cases ->
         if Lm_symbol.eq v case_sym && bool_of_value venv pos pattern || Lm_symbol.eq v default_sym then
            let venv, _ = eval_body_value_env venv pos e in
               while_loop venv pos loc orig_cases arg
         else
            eval_while_cases venv pos loc orig_cases arg cases
    | [] ->
         venv

and while_loop venv pos loc cases arg =
   if bool_of_value venv pos arg then
      eval_while_cases venv pos loc cases arg cases
   else
      venv

let while_fun venv pos loc args =
   let pos = string_pos "while" pos in
   let cases, arg =
      match args with
         [ValCases cases; arg] ->
            cases, arg
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))
   in
   let venv = while_loop venv pos loc cases arg in
      ValEnv (venv, ExportAll)

(************************************************************************
 * Register.
 *)
let () =
   let builtin_vars =
      let user =
         try Unix.getlogin () with
            Unix.Unix_error _
          | Not_found ->
               "nobody"
      in
         ["OS",             (fun _ -> ValData Sys.os_type);
          "OSTYPE",         (fun _ -> ValData Sys.os_type);
          "SYSNAME",        (fun _ -> ValData Lm_uname.sysname);
          "NODENAME",       (fun _ -> ValData Lm_uname.nodename);
          "OS_VERSION",     (fun _ -> ValData Lm_uname.version);
          "OS_RELEASE",     (fun _ -> ValData Lm_uname.release);
          "MACHINE",        (fun _ -> ValData Lm_uname.machine);
          "HOST",           (fun _ -> ValData Lm_uname.nodename);
          "OMAKE_VERSION",  (fun _ -> ValData Omake_magic.version);
          "USER",           (fun _ -> ValData user);
          "PID",            (fun _ -> ValInt (Unix.getpid ()));
          "HOME",           (fun venv -> ValDir (venv_intern_dir venv home_dir))]
   in
   let builtin_funs =
      [true,  "addprefix",             addprefix,           ArityExact 2;
       true,  "mapprefix",             mapprefix,           ArityExact 2;
       true,  "removeprefix",          removeprefix,        ArityExact 2;

       true,  "addsuffix",             addsuffix,           ArityExact 2;
       true,  "mapsuffix",             mapsuffix,           ArityExact 2;
       true,  "addsuffixes",           addsuffixes,         ArityExact 2;
       true,  "removesuffix",          removesuffix,        ArityRange (1, 2);
       true,  "replacesuffixes",       replacesuffixes,     ArityExact 3;

       (* String operations *)
       true,  "string",                string,              ArityExact 1;
       true,  "string-escaped",        string_escaped,      ArityExact 1;
       true,  "quote",                 quote,               ArityExact 1;
       true,  "quote-argv",            quote_argv,          ArityExact 1;
       true,  "html-string",           html_string,         ArityExact 1;
       true,  "add-wrapper",           add_wrapper,         ArityExact 3;
       true,  "capitalize",            capitalize,          ArityExact 1;
       true,  "uncapitalize",          uncapitalize,        ArityExact 1;
       true,  "lowercase",             lowercase,           ArityExact 1;
       true,  "uppercase",             uppercase,           ArityExact 1;

       (* System operations *)
       true,  "getenv",                getenv,              ArityRange (1, 2);
       true,  "defined-env",           defined_env,         ArityExact 1;
       true,  "setenv",                setenv,              ArityExact 2;
       true,  "exit",                  exit_fun,            ArityRange (0, 1);
       true,  "raise",                 raise_fun,           ArityExact 1;
       true,  "get-registry",          get_registry,        ArityRange (3, 4);

       (* Normal variables *)
       true,  "getvar",                getvar,              ArityExact 1;
       true,  "setvar",                setvar,              ArityExact 2;

       (* Logic *)
       true,  "not",                   not_fun,             ArityExact 1;
       false, "or",                    or_fun,              ArityAny;
       false, "and",                   and_fun,             ArityAny;
       true,  "equal",                 equal,               ArityExact 2;
       true,  "if",                    if_fun,              ArityRange (2, 3);
       false, "try",                   try_fun,             ArityExact 2;
       false, "switch",                switch_fun,          ArityAny;
       false, "match",                 match_fun,           ArityAny;
       true,  "defined",               defined,             ArityExact 1;

       (* List operations *)
       true,  "array",                 array_fun,           ArityAny;
       true,  "split",                 split_fun,           ArityRange (1, 2);
       true,  "concat",                concat_fun,          ArityExact 2;
       true,  "filter",                filter,              ArityExact 2;
       true,  "filter-out",            filter_out,          ArityExact 2;
       true,  "nth",                   nth_fun,             ArityExact 2;
       true,  "nth-hd",                nth_hd_fun,          ArityExact 2;
       true,  "nth-tl",                nth_tl_fun,          ArityExact 2;
       true,  "subrange",              subrange_fun,        ArityExact 3;
       true,  "length",                length_fun,          ArityExact 1;
       true,  "rev",                   rev_fun,             ArityExact 1;

       (* Set operations *)
       true,  "set",                   set,                 ArityExact 1;
       true,  "mem",                   mem,                 ArityExact 2;
       true,  "intersection",          intersection,        ArityExact 2;
       true,  "intersects",            intersects,          ArityExact 2;
       true,  "set-diff",              set_diff,            ArityExact 2;

       (* Shell command *)
       true,  "system",                system,              ArityExact 1;
       true,  "shell",                 shell,               ArityExact 1;
       true,  "shella",                shella,              ArityExact 1;
       true,  "shell-code",            shell_code,          ArityExact 1;

       true,  "export",                export,              ArityRange (0, 1);

       false, "while",                 while_fun,           ArityExact 2]
   in
   let builtin_info =
      { builtin_empty with builtin_vars = builtin_vars;
                           builtin_funs = builtin_funs
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
