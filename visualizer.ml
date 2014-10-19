(*
  LambdaTactician - a cmd-line tactical lambda game.
  Copyright (C) 2014 Claes Worm 

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

open Batteries
open Core_rand00 
open Gametypes

module type S = sig 
  val board : gstate:Gstate.t -> unit
end

module Basic : S = struct 

  open Gstate
  open Player

  let str_of_sym = function 
    | X -> "x" | Y -> "y" | Z -> "z"

  let sep, fill = "|", "_" 

  let str_of_elem = function 
    | Lambda (s0, s1) -> [ str_of_sym s0; "."; str_of_sym s1 ]
    | Symbol s -> [ fill; str_of_sym s; fill ]
    | Empty -> List.make 3 fill

  let board ~gstate = 
    let elems = List.of_enum (Board.enum gstate.board) in
    let board = String.concat ""
        (List.concat 
           ((List.fold_right (fun elem acc -> 
                [sep] :: (str_of_elem elem.element) :: acc )
             ) elems [[sep]] )) in
    let full = ((*"\r" ^*) board)
    in print_string full

end

