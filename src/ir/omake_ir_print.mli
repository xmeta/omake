(*
 * Print the IR.
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
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified By: Aleksey Nogin @email{nogin@cs.caltech.edu}
 * @end[license]
 *)
open Lm_printf

open Omake_ir

val pp_print_scope_kind      : formatter -> scope_kind -> unit
val pp_print_arity           : formatter -> arity -> unit
val pp_print_string_exp      : formatter -> string_exp -> unit
val pp_print_string_exp_list : formatter -> string_exp list -> unit
val pp_print_exp             : formatter -> exp -> unit
val pp_print_exp_simple      : formatter -> exp -> unit
val pp_print_prog            : formatter -> prog -> unit

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
