(*
 * An identifier server.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Mojave Group, Caltech
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

open Lm_map

open Fmarshal

open Omake_marshal

(*
 * A job identifier is just an integer.
 *)
type id = int

(*
 * Table based on file descriptor.
 *)
module IdCompare =
struct
   type t = id

   let compare = (-)
end

module IdTable = Lm_map.LmMake (IdCompare)

(*
 * Print an id.
 *)
let pp_print_pid = pp_print_int

(*
 * Id allocation.
 *)
let index = ref 1

let create () =
   let id = !index in
      index := succ id;
      id

(*
 * Marshaling.
 *)
let marshal_id id =
   List [Magic IdMagic; Int id]

let unmarshal_id l =
   match l with
      List [Magic IdMagic; Int id] ->
         id
    | _ ->
         raise MarshalError

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
