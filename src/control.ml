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

let rec gameturn gstate ~rules ~visualizer ~synth = 

  let module Rules = (val rules : Rules.S) in
  let module Visualize = (val visualizer : Visualizer.S) in
  let module Synth = (val synth : Synth.S) in
  
  let _ = Synth.sinew0 [] in
  
  Gstate.next_player_element ~gstate (Rules.return_cost ~gstate)
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
          ~direction:(opposite_direction (current_player_position ~gstate)) in

      let actions = List.filter_map 
          (Rules.conseq_to_action ~gstate)
          conseqs in

      let board = List.fold_left 
          Board.eval_action 
          board 
          actions in

      let gstate = 
        Rules.update_player_mana ~gstate
          (`From_actions actions) 
        |> Rules.set_possible_winner in

      let _ = Visualize.run { gstate with board } in

      let board = Board.remove_killed_elems board

      in { gstate with 
           turn = (Player.opposite gstate.turn);
           board }

    end

  | `Illegal illegal_elem ->

    begin

      Rules.apply_punishment illegal_elem ~gstate
      |> Rules.set_possible_winner
      |> function
      | { winner = Some _ } as gstate -> gstate
      | _ -> gameturn gstate ~rules ~visualizer ~synth (*..same players turn*)

    end


(* ~iinterp ; fcmod*)
let gloop gstate_init ~rules ~visualizer ~synth = 
  let open Player in
  let rec loop_if_no_winner = function
    | { winner = Some player; p0; p1 } -> 
      (match player with 
       | P0 -> print_endline ("And the winner is "^p0.name^"!")
       | P1 -> print_endline ("And the winner is "^p1.name^"!")
       | PNone -> failwith "Control: Ehm.. something wen't wrong.")
    | gstate -> loop_if_no_winner (gameturn gstate ~rules ~visualizer ~synth)
  in 
  loop_if_no_winner gstate_init
