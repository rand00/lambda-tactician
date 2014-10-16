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

let rec gstep gstate = 

  let module Rules = (val gstate.rules) in

  Gstate.next_player_element ~gstate 
  |> Rules.apply_cost_to_element ~gstate
  |> Rules.is_element_legal ~gstate
  |> function
  | `Legal element -> 

    begin 

      let gstate = Rules.update_player_mana 
          (`From_element element) 
          ~gstate in

      let conseqs, board = 
        Board.move_all_and_add gstate.board element
          ~elems_owned_by:gstate.turn
          ~direction:(opposite_direction (player_position ~gstate)) in

      let actions = List.filter_map 
          Rules.conseq_to_action ~gstate
          conseqs in

      let board = List.fold_left 
          Board.eval_action 
          board 
          actions in

      let gstate = 
        ( Rules.update_player_mana
            (`From_actions actions) 
            { gstate with board } )
        |> Rules.determine_possible_winner

      in { gstate with turn = (Player.opposite gstate.turn) }

    end

  | `Illegal illegal_elem ->
    begin

      Rules.apply_punishment illegal_elem ~gstate
      |> Rules.determine_possible_winner
      |> function
      | { winner = Some _ } as gstate -> gstate
      | _ -> gstep gstate (*same players turn*)

    end



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



(*------------------------gloop ------------------------*)
(*goto
  . implement_visualization here
  . remove earlier killed elems
  . switch gstate.turn to other player
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

