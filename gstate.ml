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

type t = {
  p0 : Player.t;
  p1 : Player.t;
  turn : player_id;
  board : Board.t;
  rules : (module Rules.S);
  element_costs : element_costs;
  winner : player_id option;
}

let player_position gstate = 
  let open Player in
  match gstate.turn with
  | P0 -> gstate.p0.position
  | P1 -> gstate.p1.position
  | PNone -> failwith "Gstate: PNone is no player"

let opposite_direction = function
  | Left -> Right
  | Right -> Left

let next_player_element gstate = 
  let open Player in
  match gstate.turn with 
  | P0 -> 
    let next_elem = 
      gstate.p0.next_move 
        gstate.board 
        gstate.p0.mana
    in { owner = P0;
         element = next_elem;
         mana_cost = 0.;
         killed = false;
         position = None; }
  | P1 -> 
    let next_elem = 
      gstate.p1.next_move 
        gstate.board 
        gstate.p1.mana
    in { owner = P1;
         element = next_elem;
         mana_cost = 0.;
         killed = false;
         position = None; } 
  | PNone -> failwith "Gstate: PNone is no player"


