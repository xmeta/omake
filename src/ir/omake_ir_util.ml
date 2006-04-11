(*
 * Utilities for IR expressions.
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
open Omake_ir

let loc_of_exp e =
   match e with
      LetVarExp (loc, _, _, _, _)
    | KeyExp (loc, _)
    | LetKeyExp (loc, _, _, _)
    | LetFunExp (loc, _, _, _, _)
    | LetObjectExp (loc, _, _, _)
    | LetThisExp (loc, _)
    | ShellExp (loc, _)
    | IfExp (loc, _)
    | SequenceExp (loc, _)
    | SectionExp (loc, _, _)
    | OpenExp (loc, _)
    | IncludeExp (loc, _, _)
    | ApplyExp (loc, _, _, _)
    | SuperApplyExp (loc, _, _, _, _)
    | MethodApplyExp (loc, _, _, _)
    | StaticExp (loc, _, _, _)
    | ExportExp (loc, _)
    | CancelExportExp loc
    | ReturnCatchExp (loc, _)
    | StringExp (loc, _)
    | ReturnExp (loc, _)
    | ReturnObjectExp (loc, _)
    | ReturnSaveExp loc ->
         loc

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
