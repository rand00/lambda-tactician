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

type t = element_wrap array

let make n = 
  Array.make 
    (if (n mod 2) <> 0 
     then failwith "Board.make: N is not even." 
     else n) 
    empty_wrap
  |> Array.mapi (fun i e -> { e with position = Some i })

let enum = Array.enum

let get_symbol = function
  | Symbol sym -> sym
  | _ -> failwith "Board.get_symbol: Can only match on Symbol"

let eval action board = 
  let board' = Array.copy board in
  let _ = match action with 
  | Kill (killer, killed) -> 
    let pos = Option.get killed.position 
    in board'.(pos) <- {killed with killed = true}
  | Application (lambda, value) -> (match lambda.element with 
      | Lambda (sym_in, sym_out) 
        when sym_in = (get_symbol value.element) -> 
        ( let pos = Option.get lambda.position 
          in board'.(pos) <- { lambda with element = Symbol sym_out };
          let pos = Option.get value.position
          in board'.(pos) <- empty_wrap )
      | _ -> failwith "Board.eval: Application")
  in board'


let move_all p_id direction board = 
  let len = Array.length board in
  let board' = make len in
  let conseqs = Array.fold_left (fun conseqs elem ->
      match elem with 
      | {owner} when owner = p_id-> ( match direction with

          | Left -> 
            let pos_to = (Option.get elem.position)-2 in
            let pos_over = pos_to+1 in
            let elem' = { elem with position = Some pos_to} in

            if pos_to < 0 then 
              (Out_of_bounds (Left, elem')) :: conseqs
            else 
              ( board'.(pos_to) <- elem';
                (Jumpover (elem', board.(pos_over))) :: conseqs )

          | Right -> 
            let pos_to = (Option.get elem.position)+2 in
            let pos_over = pos_to-1 in
            let elem' = {elem with position = Some pos_to} in

            if pos_to >= len then
              (Out_of_bounds (Right, elem')) :: conseqs
            else
              ( board'.(pos_to) <- elem';
                (Jumpover (elem', board.(pos_over))) :: conseqs ))

      | elem -> 
        let pos = Option.get elem.position in
        ( board'.(pos) <- elem;
          conseqs )
    ) [] board 
  in conseqs, board'

let eval_to_consequence action board = 
  match action with 
  | Move_all_and_add (p_id, direction, elem) -> 
    let conseqs, board' = move_all p_id direction board in
    let pos_add = match direction with 
      | Left -> (Array.length board')-1
      | Right -> 0 in
    let _ = board'.(pos_add) <- {elem with position = Some pos_add}
    in conseqs, board'
  | Move_all (p_id, direction) -> 
    move_all p_id direction board


let remove_killed = 
  Array.map (function 
      | {killed = true; position} -> { empty_wrap with position }
      | other -> other) 
      
