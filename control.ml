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
open Gstate

let gstep gstate = gstate

  let elem_chosen = Gstate.next_move gstate in 
  (*< goto wrap in rule for 
    . mana-cost
    . legality (enough mana?) !!!
      . other reason not legal?*)
  let conseqs, board = 
    match elem_chosen with (*match on element of wrapper*)
    | Empty -> 
      Board.eval_move_to_effect 
        (Move_all 
           (gstate.turn, 
            Gstate.(opposite_direction 
                      (player_position gstate))))
    | _ -> assert false
  in gstate 



(*goto determine legal turn action based on some new user-action type *)

(*goto: iterate throgh (map) and remove earlier killed elems (they are there for last-turn visuals)*)

(*moving the current players elems*)
(*goto 
  . only do this if owner-actions allows (a path in the user-action tree; make new type)
  . return actions + new-array*)
(*goto rewrite ; 
  match on input-actions (based on rules)
  . don't control direction from here - let rules-mod choose and pmatch!
  . let rules-mod return the action to be done on some*)
(*goto action allows for it; add new elem from curr player (if relevant) + add this to actions*)

(*goto calculate mana-costs/-income from 'actions', and later return mana+winner-gamestate*)

(*goto visualize*)

(* some game logic to rewrite::

    Array.fold_left (fun i {element; owner} -> 
        if owner = gstate.turn then
          match player_pos gstate owner with
          | `Left  -> "..."
          | `Right -> "...")
      (Array.(make (length gstate.board) Empty), [])
      gstate.board

  { gstate with 
    board = Board.eval action board; 
    turn = (player_opposite gstate.turn) }
*)


(* ~rules ~visualizer ~iinterp ; firstclass mod's*)
let gloop gstate_init = 
  let open Player in
  let rec aux = function
    | { winner = Some player; p0; p1 } -> 
      (match player with 
       | P0 -> print_endline ("And the winner is "^p0.name^"!")
       | P1 -> print_endline ("And the winner is "^p1.name^"!")
       | PNone -> failwith "Control: Ehm.. something wen't wrong.")
    | gstate -> aux (gstep gstate)
  in 
  aux gstate_init

