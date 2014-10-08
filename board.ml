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

type t = (element_wrap option) array

let make n = 
  Array.make 
    (if (n mod 2) <> 0 
     then failwith "Board.make: N is not even." 
     else n) 
    None

let enum = Array.enum

let get_symbol = function
  | Symbol sym -> sym
  | _ -> failwith "Board.get_symbol: Can only match on Symbol"

let eval action board = 
  let board' = Array.copy board in
  let _ = match action with 
  | Kill (killer, killed) -> 
    let pos = Option.get killed.position 
    in board'.(pos) <- Some {killed with killed = true}
  | Application (lambda, value) -> (match lambda.element with 
      | Lambda (sym_in, sym_out) 
        when sym_in = (get_symbol value.element) -> 
        ( let pos = Option.get lambda.position 
          in board'.(pos) <- Some { lambda with element = Symbol sym_out };
          let pos = Option.get value.position
          in board'.(pos) <- None )
      | _ -> failwith "Board.eval: Application")
  in board'


let eval_to_consequence action board = 
  match action with 
  | Move_all_and_add (p_id, direction, elem) -> 
    let len = Array.length board in
    let board' = Array.make len None in
    let rec aux it_from it_to ~pos_old ~pos_moved ~out_of_bounds = 
      board'.(len-1) <- elem; (*goto goo*)
      let conseqs = [] in
      for i = len-2 to 0 do
        let pos_old = i+1 
        in match board.(pos_old) with 
        | {id = p_id} -> 
          let pos_moved = i-1 in
          if pos_moved < 0 then

            board'.(pos_moved) <- board.(pos_moved)
        | _ -> board'.(i) <- 
    ( match direction with 
      | `Left  -> 
        ( 
      | `Right -> 
  | Move_all (p_id, direction) -> assert false

let remove_killed = 
  Array.map (function 
      | Some {killed = true} -> None
      | other -> other) 
      
