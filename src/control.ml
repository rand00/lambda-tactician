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
open BatExt_rand00
open Gametypes
open Gstate

let rec gameturn gstate ~rules ~visualizer ~synth = 

  let module Rules = (val rules : Rules.S) in
  (* > should contain two-way mailbox/signal comm (maybe both?)
     . out-mb to send new game state 
       (can be a signal_send function instead of mb: just keep "update func") 
     . in-mb to check for visualizer anim done
       (> is needed? .. maybe not yet just find a way to timeout signals so they stop
       and are GC'ed)    *)
  let module Visualize = (val visualizer : Visualizer.S) 
  in
  Gstate.next_player_element ~gstate (Rules.return_cost ~gstate)
  |> Rules.apply_cost_to_element ~gstate
  |> Rules.is_element_legal ~gstate
  |> function
  | `Legal element -> 

    begin 
 
      let _ = SC.Synth.synth synth "ratata" 
          [ (match gstate.turn with 
                | P0 -> ("panfrom", `I (-1)) 
                | P1 | PNone -> "panfrom", `I 1) ] in
      (*< gomaybe: should be okay for running inside Lwt*)

      let gstate = Rules.update_player_mana 
          (`From_element element) 
          ~gstate in

      let conseqs, board = 
        Board.move_all_and_add 
          (opposite_direction (current_player_position ~gstate))
          element
          gstate.board in
      (*gomaybe: too expensive an operation to run atomically in lwt?*)

      let actions = List.filter_map 
          (Rules.conseq_to_action ~gstate)
          conseqs in
      (*goto use Lwt_list.filter_map_p (is order important here?)*)

      let board = List.fold_left 
          Board.eval_action 
          board 
          actions in
      (*<goto 
        . rewrite internals of Board.eval_action to be inside Lwt 
        . use Lwt_list.fold_right_p (is order important?)
      *)

      let gstate = 
        Rules.update_player_mana ~gstate
          (`From_actions actions) 
        |> Rules.set_possible_winner 
        |> fun gstate -> { gstate with board } in
      (*<goto just wrap in lwt.return? (or use lwt_list inside)*)

      let _ = Visualize.update gstate in
      (*< goto make a lwt-call*)

      let board = 
        Board.remove_killed_elems board
        |> Board.increment_time 
      (*<goto just wrap in lwt.return? (or use lwt_list inside)*)

      in { gstate with turn = (Player.opposite gstate.turn); board }
      (*<goto just wrap in lwt.return*)

    end

  | `Illegal illegal_elem ->

    begin

      let _ = SC.Synth.synth synth "ratata" 
          [ (match gstate.turn with 
                | P0 -> ("panfrom", `I (-1)) 
                | P1 | PNone -> "panfrom", `I 1) ] in

      Rules.apply_punishment illegal_elem ~gstate
      |> Rules.set_possible_winner
      |> function
      | { winner = Some _ } as gstate -> 
        (*> goto from here Lwt*)
        let _ = Visualize.update gstate 
        in gstate
      | gstate -> 
        (*> goto from here Lwt*)
        let _ = Visualize.update gstate 
        in gameturn gstate ~rules ~visualizer ~synth (*..same players turn*)

    end


(* ~iinterp ; fcmod*)
let gloop gstate_init ~rules ~visualizer ~synth = 
  let open Player in
  let module Visualize = (val visualizer : Visualizer.S) in
  let _ = SC.Synth.synth synth "synth_ghost2" [] in
  (*>goto use Lwt from here*)
  let _ = print_endline "" in
  let _ = Visualize.update gstate_init
  in
  let rec loop_if_no_winner = function
    | { winner = Some player; p0; p1 } -> 
      (match player with 
       | P0 -> print_endline ("And the winner is "^p0.name^"!")
       | P1 -> print_endline ("And the winner is "^p1.name^"!")
       | PNone -> failwith "Control: Ehm.. something wen't wrong - PNone is no player.")
    | gstate -> loop_if_no_winner (gameturn gstate ~rules ~visualizer ~synth)
  in 
  loop_if_no_winner gstate_init

