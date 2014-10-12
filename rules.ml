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
  
  val apply_cost_to_element : gstate:Gstate.t -> element_wrap -> element_wrap

  val return_cost : element -> float

  val is_element_legal : gstate:Gstate.t -> element_wrap -> 
    [ `Legal of element_wrap 
    | `Illegal of element_wrap ]

  val update_player_mana : gstate:Gstate.t -> 
    [ `From_element of element_wrap 
    | `From_actions of element_action list ] 
    -> Gstate.t
                     
  val conseq_to_action : board_move_conseq -> element_action

  val apply_punishment : gstate:Gstate.t -> element_wrap -> Gstate.t

  val determine_possible_winner : gstate:Gstate.t -> Gstate.t

end

module Basic : S = struct 

  let apply_cost_to_element ~gstate = function
    | { element = Lambda _ } as e -> { e with mana_cost = gstate.element_cost.lambda } 
    | { element = Symbol _ } as e -> { e with mana_cost = gstate.element_cost.symbol }
    | { element = Empty } as e -> { e with mana_cost = gstate.element_cost.empty } 

  let return_cost ~gstate = function 
    | Lambda _ -> gstate.element_cost.lambda
    | Symbol _ -> gstate.element_cost.symbol
    | Empty -> gstate.element_cost.empty

  let is_element_legal ~gstate elem = 
    if elem.mana_cost <= (Gstate.current_player_mana ~gstate) 
    then `Legal elem
    else `Illegal elem

  let update_player_mana_from_element ~gstate =
    let open Player in 
    match gstate.turn with 
    | P0 -> { gstate with 
              p0 = { gstate.p0 with 
                     mana = gstate.p0.mana -. element.mana_cost }}
    | P1 -> { gstate with 
              p1 = { gstate.p1 with 
                     mana = gstate.p1.mana -. element.mana_cost }}
    | PNone -> failwith "Gstate: update_player_mana: PNone is no player"
                 
  let update_player_mana_from_actions ~gstate =
    let open Player in 
    match gstate.turn with 
    | P0 -> List.fold_left 
    | P1 -> 
    | PNone -> )
    
  let update_player_mana ~gstate = function
    | `From_element element -> update_player_mana_from_element ~gstate element
    | `From_actions actions -> update_player_mana_from_actions ~gstate actions


  let conseq_to_action conseq = assert false

  let apply_punishment ~gstate elem = assert false

  let determine_possible_winner ~gstate = assert false

end


