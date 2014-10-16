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
open Rulestypes

(**Standard values to be included in Gstate.t*)
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


(**Types of the subfunctors of a full ruleset
   -> these are actually not neccesary for the Basic rules module,
      but they serve to propose the structure for future (possibly)
      more complex rulesets. 
*)
module type R1_Cost = sig 
  val return_cost : gstate:Gstate.t -> element -> float
  val apply_cost_to_element : gstate:Gstate.t -> element_wrap -> element_wrap
end

module type R2_Legality = sig
  val is_element_legal : gstate:Gstate.t -> element_wrap -> 
    [ `Legal of element_wrap 
    | `Illegal of element_wrap ]
end
module type R2Fun_Legality = functor (RCost : R1_Cost) -> R2_Legality

module type R3_Mana = sig
    val update_mana_from_element : gstate:Gstate.t -> element_wrap -> Gstate.t
    val update_mana_from_actions : gstate:Gstate.t -> element_action list -> Gstate.t
end
module type R3Fun_Mana = functor (RCost : R1_Cost) -> R3_Mana

module type R4_DepMana = sig
  val update_player_mana : gstate:Gstate.t -> 
    [ `From_element of element_wrap 
    | `From_actions of element_action list ] -> Gstate.t
  val apply_punishment : gstate:Gstate.t -> element_wrap -> Gstate.t
end
module type R4Fun_DepMana = 
  functor (RCost : R1_Cost) -> 
  functor (RMana : R3Fun_Mana) -> R4_DepMana 

module type R5_Rest = sig
  val conseq_to_action : board_move_conseq -> element_action
  val determine_possible_winner : gstate:Gstate.t -> Gstate.t  
end


(**Signature for the collection of rules-functions put together as first-class module.*)
module type S = sig
  include R1_Cost
  include R2_Legality
  include R4_DepMana
  include R5_Rest
end  


(**Basic implementation of rules - these can be mixed with custom sub-modules,
   resulting in a customly composed ruleset.*)

module Basic1_cost : R1_Cost = struct 

  open Gstate

  let apply_cost_to_element ~gstate = function
    | { element = Lambda _ } as e -> { e with mana_cost = gstate.rvalues.elements.lambda }
    | { element = Symbol _ } as e -> { e with mana_cost = gstate.rvalues.elements.symbol }
    | { element = Empty } as e -> { e with mana_cost = gstate.rvalues.elements.empty } 

  let return_cost ~gstate = function
    | Lambda _ -> gstate.rvalues.elements.lambda
    | Symbol _ -> gstate.rvalues.elements.symbol
    | Empty -> gstate.rvalues.elements.empty

end

module Basic2_legality = 
  functor (RCost : R1_Cost) -> struct 

    let is_element_legal ~gstate elem = 
      if elem.mana_cost <= (Gstate.current_player_mana ~gstate) 
      then `Legal elem
      else `Illegal elem

  end

module Basic3_update_mana = 
  functor (RCost : R1_Cost) -> struct

    open Gstate

    let update_mana_from_element ~gstate elem =
      let open Player in 
      match gstate.turn with 
      | P0 -> { gstate with 
                p0 = { gstate.p0 with 
                       mana = gstate.p0.mana -. elem.mana_cost }}
      | P1 -> { gstate with 
                p1 = { gstate.p1 with 
                       mana = gstate.p1.mana -. elem.mana_cost }}
      | PNone -> failwith "Gstate: update_player_mana: PNone is no player"

    let update_mana_from_actions ~gstate =
      let open Player in 
      let open Gstate in
      List.fold_left (fun gstate -> function
          | Kill ( { owner = P0; element=killer } , 
                   { owner = P1; element=killed } ) -> 
            { gstate with 
              p0 = { gstate.p0 with 
                     mana = gstate.p0.mana +. 
                            (match killed with 
                             | Lambda _ -> gstate.rvalues.actions.kill_lambda 
                             | Symbol _ -> gstate.rvalues.actions.kill_symbol
                             | _ -> 0. ) }}

          | Kill ( { owner = P1; element=killer }, 
                   { owner = P0; element=killed } ) -> 
            { gstate with 
              p1 = { gstate.p1 with
                     mana = gstate.p1.mana +. 
                            (match killed with
                             | Lambda _ -> gstate.rvalues.actions.kill_lambda
                             | Symbol _ -> gstate.rvalues.actions.kill_symbol
                             | _ -> 0. ) }}

          | Application ({owner = P0; element=(Lambda _)}, 
                         {owner = P1; element=(Symbol _)}) -> 
            { gstate with 
              p0 = { gstate.p0 with 
                     mana = gstate.p0.mana +. gstate.rvalues.actions.application }}

          | Application ({owner = P1; element=(Lambda _)}, 
                         {owner = P0; element=(Symbol _)}) -> 
            { gstate with 
              p1 = { gstate.p1 with 
                     mana = gstate.p1.mana +. gstate.rvalues.actions.application }}

          | _ -> failwith "Rules:update_player_mana_from_actions: Wrong input"

        ) gstate 

  end

module Basic4_dep_mana = 
  functor (RCost : R1_Cost) -> 
  functor (RManaFun : R3Fun_Mana) -> struct

    module RMana = RManaFun(RCost)

    let update_player_mana ~gstate = function
      | `From_element element -> RMana.update_mana_from_element ~gstate element
      | `From_actions actions -> RMana.update_mana_from_actions ~gstate actions

    let apply_punishment ~gstate illegal_elem = assert false

  end


module Basic5_rest = struct 
  let conseq_to_action conseq = assert false
  let determine_possible_winner ~gstate = assert false
end


(**Final composed Basic module - this is the standard module to be used as first class module*)
module Basic : S = struct 

  include Basic1_cost
  include Basic2_legality(Basic1_cost)

  include Basic3_update_mana(Basic1_cost)
  include Basic4_dep_mana (Basic1_cost) (Basic3_update_mana)

  include Basic5_rest

end




