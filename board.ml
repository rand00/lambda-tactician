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

let enum = Array.enum

let get_symbol = function
  | Symbol sym -> sym
  | _ -> failwith "Board.get_symbol: Can only match on Symbol"

let eval action board = 
  let _ = match action with 
  | Kill (killer, killed) -> 
    let pos = Option.get killed.position 
    in board.(pos) <- Some {killed with killed = true}
  | Application (lambda, value) -> (match lambda.element with 
      | Lambda (sym_in, sym_out) 
        when sym_in = (get_symbol value.element) -> 
        ( let pos = Option.get lambda.position 
          in board.(pos) <- Some { lambda with element = Symbol sym_out };
          let pos = Option.get value.position
          in board.(pos) <- None )
      | _ -> failwith "Board.eval: Application")
  in board


let eval_to_consequence action board = 
  match action with 
  | Move_all_and_add (direction, elem) -> assert false
  | Move_all direction -> assert false

let remove_killed = 
  Array.map (function 
      | Some {killed = true} -> None
      | other -> other) 
      
