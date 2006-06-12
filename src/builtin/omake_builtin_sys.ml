(*
 * Miscellaneous system functions.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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

open Omake_ir
open Omake_env
open Omake_node
open Omake_value
open Omake_symbol
open Omake_builtin
open Omake_builtin_type
open Omake_builtin_util

module Pos = MakePos (struct let name = "Omake_builtin_sys" end);;
open Pos

(*
 * \begin{doc}
 * \fun{gettimeofday}
 *
 * \begin{verbatim}
 *    $(gettimeofday) : Float
 * \end{verbatim}
 *
 * The \verb+gettimeofday+ function returns the time of day in seconds
 * since January 1, 1970.
 *
 * \end{doc}
 *)
let gettimeofday venv pos loc args =
   let pos = string_pos "gettimeofday" pos in
      match args with
         [] ->
            ValFloat (Unix.gettimeofday ())
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 0, List.length args)))

(************************************************************************
 * Tables.
 *)

let () =
   let builtin_funs =
      [true, "gettimeofday",          gettimeofday,         ArityExact 0]
   in
   let builtin_info =
      { builtin_empty with builtin_funs = builtin_funs }
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
