(*
 * Indirection, to allow different implementations of symbols.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2006 Mojave Group, Caltech
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
open Lm_hash
open Lm_thread
open Lm_printf

module Standard =
struct
    type symbol = Lm_symbol.symbol

    let add = Lm_symbol.add
    let compare = Lm_symbol.compare
    let eq = Lm_symbol.eq
    let get v = v
    let is_numeric_symbol = Lm_symbol.is_numeric_symbol
    let make = Lm_symbol.make
    let new_name = Lm_symbol.new_name
    let new_symbol = Lm_symbol.new_symbol
    let pp_print_symbol = Lm_symbol.pp_print_symbol
    let string_of_symbol = Lm_symbol.string_of_symbol
    let to_string = Lm_symbol.to_string

    module SymbolSet = Lm_symbol.SymbolSet;;
    module SymbolTable = Lm_symbol.SymbolTable;;
end;;

module Hash =
struct
   (* %%MAGICBEGIN%% *)
   module Arg =
   struct
      type t = int * string

      let debug = "Om_symbol"

      let hash = Hashtbl.hash
(*
         let buf = HashCode.create () in
            HashCode.add_string buf s;
            HashCode.add_int buf i;
            HashCode.code buf
*)

      let compare (i1, s1) (i2, s2) =
         if i1 < i2 then
            -1
         else if i1 > i2 then
            1
         else
            Lm_string_util.string_compare s1 s2

      let reintern v = v
   end;;
   (* %%MAGICEND%% *)

   module SymbolHash = MakeHashMarshal (Arg);;

   type symbol = SymbolHash.t;;

   let get = SymbolHash.get

   let make i s = SymbolHash.create (i, s)
   let new_number, make =
      let count = ref 100 in
      let lock = Mutex.create "Om_symbol" in
         (fun () ->
               Mutex.lock lock;
               let i = !count in
                  count := succ i;
                  Mutex.unlock lock;
                  i),
         (fun s i ->
               if i >= !count then begin
                  Mutex.lock lock;
                  count := max (!count) (succ i);
                  Mutex.unlock lock
               end;
               SymbolHash.create (i, s))

   let to_string (v : symbol) : string =
      snd (get v)

   let new_symbol v =
      SymbolHash.create (new_number (), to_string v)

   let new_name v pred =
      let s = to_string v in
      let rec search i =
         let nv = make s i in
            if pred nv then
               search (succ i)
            else
               nv
      in
         search 0

   let string_of_symbol v =
      let i, s = get v in
         if i = 0 then
            s
         else
            s ^ string_of_int i

   let pp_print_symbol buf v =
      let i, s = get v in
         if i = 0 then
            pp_print_string buf s
         else
            fprintf buf "%s_%d" s i

   let add (s : string) : symbol = SymbolHash.create (0, s)

   let compare = SymbolHash.compare
   let eq = SymbolHash.equal

   let rec all_digits s i =
      i < 0 || (match s.[i] with
                   '0' .. '9' -> all_digits s (pred i)
                 | _ -> false)

   let is_numeric_symbol v =
      all_digits (snd (get v)) 0

   module SymbolSet = Lm_set.LmMake (SymbolHash);;
   module SymbolTable = Lm_map.LmMake (SymbolHash);;
end;;

include Hash;;

type var = symbol;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
