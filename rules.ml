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

module type S = sig

  type t = <
    return_cost : element -> float;

    apply_cost_to_element : gstate:Gstate.t -> element_wrap -> element_wrap;

    is_element_legal : gstate:Gstate.t -> element_wrap -> 
      [ `Legal of element_wrap 
      | `Illegal of element_wrap ];

    update_player_mana : gstate:Gstate.t -> 
      [ `From_element of element_wrap 
      | `From_actions of element_action list ] 
      -> Gstate.t;

    conseq_to_action : board_move_conseq -> element_action;

(*    apply_punishment : gstate:Gstate.t -> (*element_wrap ->*) Gstate.t;*)

    determine_possible_winner : gstate:Gstate.t -> Gstate.t;  
  >

  val make : ?values:rule_values -> unit -> t

end  

module Basic : S = struct 

  class rules (rule_values:rule_values) = object(self)
    
    method apply_cost_to_element ~(gstate:Gstate.t) = function
      | { element = Lambda _ } as e -> { e with mana_cost = gstate.rule_values.elements.lambda }
      | { element = Symbol _ } as e -> { e with mana_cost = gstate.rule_values.elements.symbol }
      | { element = Empty } as e -> { e with mana_cost = gstate.rule_values.elements.empty } 

    method return_cost ~(gstate:Gstate.t) = function
      | Lambda _ -> gstate.rule_values.elements.lambda
      | Symbol _ -> gstate.rule_values.elements.symbol
      | Empty -> gstate.rule_values.elements.empty

    method is_element_legal ~(gstate:Gstate.t) elem = 
      if elem.mana_cost <= (Gstate.current_player_mana ~gstate) 
      then `Legal elem
      else `Illegal elem

    method update_player_mana_from_element ~(gstate:Gstate.t) element =
      let open Player in 
      match gstate.turn with 
      | P0 -> { gstate with 
                p0 = { gstate.p0 with 
                       mana = gstate.p0.mana -. element.mana_cost }}
      | P1 -> { gstate with 
                p1 = { gstate.p1 with 
                       mana = gstate.p1.mana -. element.mana_cost }}
      | PNone -> failwith "Gstate: update_player_mana: PNone is no player"

    method update_player_mana_from_actions ~(gstate:Gstate.t) =
      let open Player in 
      List.fold_left (fun gstate -> function
          | Kill ( { owner = P0; element=killer } , 
                   { owner = P1; element=killed } ) -> 
            { gstate with 
              p0 = { gstate.p0 with 
                     mana = gstate.p0.mana +. 
                            (match killed with 
                             | Lambda _ -> rule_values.actions.kill_lambda 
                             | Symbol _ -> rule_values.actions.kill_symbol
                             | _ -> 0. ) }}

          | Kill ( { owner = P1; element=killer }, 
                   { owner = P0; element=killed } ) -> 
            { gstate with 
              p1 = { gstate.p1 with
                     mana = gstate.p1.mana +. 
                            (match killed with
                             | Lambda _ -> rule_values.actions.kill_lambda
                             | Symbol _ -> rule_values.actions.kill_symbol
                             | _ -> 0. ) }}

          | Application ({owner = P0; element=(Lambda _)}, 
                         {owner = P1; element=(Symbol _)}) -> 
            { gstate with 
              p0 = { gstate.p0 with 
                     mana = gstate.p0.mana +. rule_values.actions.application }}

          | Application ({owner = P1; element=(Lambda _)}, 
                         {owner = P0; element=(Symbol _)}) -> 
            { gstate with 
              p1 = { gstate.p1 with 
                     mana = gstate.p1.mana +. rule_values.actions.application }}

          | _ -> failwith "Rules:update_player_mana_from_actions: Wrong input"

        ) gstate 


    method update_player_mana ~(gstate:Gstate.t) = function
      | `From_element element -> self#update_player_mana_from_element ~gstate element
      | `From_actions actions -> self#update_player_mana_from_actions ~gstate actions


    method conseq_to_action (conseq:board_move_conseq):element_action = assert false

    method apply_punishment ~(gstate:Gstate.t) (illegal_elem:element_wrap) = gstate

    method determine_possible_winner ~(gstate):Gstate.t = gstate

  end

  let std_values = {
    elements = {
      lambda = 0.3;
      symbol = 0.3;
      empty = 0.0;
    };
    actions = {
      kill_lambda = 0.1;
      kill_symbol = 0.1;
      move_element = 0.0;
      application = 0.2;
    };
  }

  let make ?(values=std_values) = new rules values

end


