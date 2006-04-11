(*
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

open Lm_location

open Omake_ast
open Omake_env

module Pos = MakePos (struct let name = "Omake_exn_print" end)
open Pos

(*
 * Other exception.
 *)
let pp_print_other_exn buf exn =
   match exn with
      Unix.Unix_error (errno, f, arg) ->
         fprintf buf "@[<v 3>%s(%s): %s@]" (**)
            f arg (Unix.error_message errno)
    | Sys_error s ->
         fprintf buf "@[<v 3>Sys_error: %s@]" s
    | Sys.Break ->
         fprintf buf "@[<v 3>Break@]"
    | Failure s ->
         fprintf buf "Failure: %s" s
    | Invalid_argument s ->
         fprintf buf "Invalid argument: %s" s
    | exn ->
         fprintf buf "@[<v 3>%s@]" (**)
            (Printexc.to_string exn)

(*
 * Exception printer.
 *)
let pp_print_exn buf exn =
   match exn with
      OmakeException (pos, exn) ->
         fprintf buf "@[<v 3>*** omake error:@ %a@ %a@]" (**)
            pp_print_pos pos
            pp_print_exn exn
    | UncaughtException (pos, exn) ->
         fprintf buf "@[<v 3>*** omake error:@ %a@ %a@]" (**)
            pp_print_pos pos
            pp_print_other_exn exn
    | RaiseException (pos, obj) ->
         fprintf buf "@[<v 3>*** omake error:@ %a@ %a@]" (**)
            pp_print_pos pos
            pp_print_value (ValObject obj)
    | OmakeFatal s ->
         fprintf buf "@[<v 3>*** omake fatal error:@ %s@]" s
    | ExitException (pos, code) ->
         fprintf buf "@[<v 3>*** omake %s:@ %a@ early exit(%i) requested by an omake file@]" (**)
            (if code = 0 then "warning" else "error")
            pp_print_pos pos
            code
    | Return (loc, _) ->
         fprintf buf "@[<v 3>*** omake internal error:@ %a@ uncaught return@]" (**)
            pp_print_location loc
    | exn ->
         fprintf buf "@[<v 3>*** omake error:@ %a@]" pp_print_other_exn exn

(*
 * If one of these exceptions occurs during process creation,
 * treat it as a command failure.
 *)
let is_shell_exn exn =
   match exn with
      OmakeException _
    | OmakeFatal _
    | UncaughtException _
    | RaiseException _
    | Unix.Unix_error _
    | Sys_error _
    | Failure _
    | Invalid_argument _
    | Return _ ->
         true
    | _ ->
         false

(*
 * Exception handler.
 *)
let catch f x =
   try f x with
      OmakeException _
    | OmakeFatal _
    | UncaughtException _
    | RaiseException _
    | Unix.Unix_error _
    | Sys_error _
    | Return _ as exn ->
         eprintf "%a@." pp_print_exn exn;
         exit Omake_state.exn_error_code
    | ExitException (_, code) as exn ->
         eprintf "%a@." pp_print_exn exn;
         exit code

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
