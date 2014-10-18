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

(*Mana*)
module type R_Mana = sig
    val update_mana_from_element : gstate:Gstate.t -> element_wrap -> Gstate.t
    val update_mana_from_actions : gstate:Gstate.t -> element_action list -> Gstate.t
end
module type RFun_Mana = functor (RCost : R1_Cost) -> R_Mana

module type R3_DepMana = sig
  val update_player_mana : gstate:Gstate.t -> 
    [ `From_element of element_wrap 
    | `From_actions of element_action list ] -> Gstate.t
  val apply_punishment : gstate:Gstate.t -> element_wrap -> Gstate.t
end
module type R3Fun_DepMana = 
  functor (RCost : R1_Cost) -> 
  functor (RMana : RFun_Mana) -> R3_DepMana 

(*Rest*)
module type R4_ActionsPlus = sig
  val conseq_to_action : gstate:Gstate.t -> board_move_conseq -> element_action option
  val determine_possible_winner : gstate:Gstate.t -> Gstate.t  
end


(**Signature for the collection of rules-functions put together as first-class module.*)
module type S = sig
  include R1_Cost
  include R2_Legality
  include R3_DepMana
  include R4_ActionsPlus
end  


(**Basic implementation of rules - these can be mixed with custom sub-modules,
   resulting in a customly composed ruleset.*)

module Basic1_cost : R1_Cost = struct 

  open Gstate

  let return_cost ~gstate = function
    | Lambda _ -> gstate.rvalues.elements.lambda
    | Symbol _ -> gstate.rvalues.elements.symbol
    | Empty -> gstate.rvalues.elements.empty

  let apply_cost_to_element ~gstate e = 
    { e with mana_cost = return_cost ~gstate e.element } 

end

module Basic2_legality = 
  functor (RCost : R1_Cost) -> struct 

    let is_element_legal ~gstate elem = 
      if elem.mana_cost <= (Gstate.current_player_mana ~gstate) 
      then `Legal elem
      else `Illegal elem

  end

module Basic_mana = 
  functor (RCost : R1_Cost) -> struct

    open Gstate

    let update_mana_from_element ~gstate elem =
      Gstate.add_player_mana ~gstate gstate.turn 
        (Float.neg elem.mana_cost)

    let update_mana_from_actions ~gstate =
      let open Player in 
      let open Gstate in
      List.fold_left (fun gstate -> function

          | Kill ( killer, killed ) -> 
            Gstate.add_player_mana ~gstate killer.owner
              (match killed.element with 
                   | Lambda _ -> gstate.rvalues.actions.kill_lambda 
                   | Symbol _ -> gstate.rvalues.actions.kill_symbol
                   | _ -> 0. )

          | Application (({ element=(Lambda _) } as lambda), 
                         ({ element=(Symbol _) } )) -> 
            Gstate.add_player_mana ~gstate lambda.owner
              gstate.rvalues.actions.application

          | Application _ -> failwith "Rules:update_player_mana_from_actions: Wrong input"

          | At_opponent elem -> 
            Gstate.add_player_mana ~gstate elem.owner
              elem.mana_cost

          | At_home elem -> 
            Gstate.add_player_mana ~gstate elem.owner
              elem.mana_cost

        ) gstate 

  end

module Basic3_dep_mana = 
  functor (RCost : R1_Cost) -> 
  functor (RManaFun : RFun_Mana) -> struct

    module RMana = RManaFun(RCost)
    open Gstate
    open Player

    let update_player_mana ~gstate = function
      | `From_element element -> RMana.update_mana_from_element ~gstate element
      | `From_actions actions -> RMana.update_mana_from_actions ~gstate actions

    let apply_punishment = RMana.update_mana_from_element

  end


module Basic4_actions_plus = struct 

  open Gstate
  open Player

  (*goto: think over rules *)
  let conseq_to_action ~gstate = function
    | Jumpover ( ({element = jumper} as jumpwrap), 
                 ({element = stander} as standwrap)) -> 
      ( match jumper, stander with 
        | Lambda (i,o), Symbol s when i = s -> Some (Application (jumpwrap, standwrap))
        | Lambda (_,_), Symbol _ -> Some (Kill (standwrap, jumpwrap))
        | Symbol X, Symbol Z
        | Symbol Y, Symbol X
        | Symbol Z, Symbol Y -> Some (Kill (jumpwrap, standwrap))
        | _ -> None )
    | Out_of_bounds (direction, elem) -> 
      if direction = (player_position ~gstate) 
      then Some (At_home elem)
      else Some (At_opponent elem)

  let determine_possible_winner ~gstate =
    let p0m, p1m = gstate.p0.mana, gstate.p1.mana 
    in
    if p0m <= p1m then 
      if p0m <= 0. then { gstate with winner = Some P1 } else gstate
    else if p1m < p0m then 
      if p1m <= 0. then { gstate with winner = Some P0 } else gstate
    else gstate

end


(**Final composed Basic module - this is the standard module to be used as first class module*)
module Basic : S = struct 

  include Basic1_cost
  include Basic2_legality(Basic1_cost)
  include Basic3_dep_mana (Basic1_cost) (Basic_mana)
  include Basic4_actions_plus

end




