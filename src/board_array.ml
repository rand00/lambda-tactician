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

let eval_action board action = 
  let board' = Array.copy board in
  let _ = 
    ( match action with 
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
      | At_home elem | At_opponent elem -> () )
  in board'


let move_all p_id direction board = 
  let len = Array.length board in
  Array.fold_left (fun (conseqs, board') elem -> 

      let aux_move1 ~jmp_pos ~over_pos ~oobounds = 
        let elem' = { elem with position = Some jmp_pos} in
        if oobounds then 
          ((Out_of_bounds (Left, elem')) :: conseqs,
           board')
        else 
          ( if board'.(jmp_pos).element <> Empty then
              failwith "Board:move_all: overwriting an non-empty element."
            else
              ( board'.(jmp_pos) <- elem';
                ((Jumpover (elem', board.(over_pos))) :: conseqs,
                 board')))

      in match elem with
      | {owner} when owner = p_id-> ( match direction with
          | Left -> 
            let jmp_pos = (Option.get elem.position)-2 
            in aux_move1
              ~jmp_pos
              ~over_pos:(jmp_pos+1)
              ~oobounds:(jmp_pos < 0)
          | Right -> 
            let jmp_pos = (Option.get elem.position)+2 
            in aux_move1
              ~jmp_pos
              ~over_pos:(jmp_pos+1)
              ~oobounds:(jmp_pos >= len))

      | {element} as elem when element <> Empty -> 
        let pos = Option.get elem.position in
        if board'.(pos).element <> Empty then
          failwith "Board:move_all: overwriting an non-empty element."
        else
          ( board'.(pos) <- elem;
            conseqs, board' )

      | _ -> conseqs, board'

    ) ([], make len) board 

let move_all_and_add board elem ~elems_owned_by ~direction = 
  let conseqs, board' = move_all elems_owned_by direction board in
  let pos_add = match direction with 
    | Left -> (Array.length board')-1
    | Right -> 0 in
  let _ = board'.(pos_add) <- {elem with position = Some pos_add}
  in conseqs, board'


let remove_killed_elems = 
  Array.map (function 
      | {killed = true; position} -> { empty_wrap with position }
      | other -> other) 
      
