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
open Player 

type app_mode = [
    `Mode_loading
  | `Mode_game
]

type t = { (*goto where to put rules?*)
  p0 : Player.t;
  p1 : Player.t;
  turn : player_id;
  rvalues : Rulestypes.t;
  board : Board.t;
  winner : player_id option;
}

let player_name ~gstate = function
  | P0 -> gstate.p0.name
  | P1 -> gstate.p1.name
  | PNone -> failwith "Gstate:player_name: PNone no player."

let player_mana ~gstate = function
  | P0 -> gstate.p0.mana
  | P1 -> gstate.p1.mana
  | PNone -> failwith "Gstate:player_mana: PNone is no player"

let current_player_mana ~gstate = 
  player_mana ~gstate gstate.turn

let player_position ~gstate = function
  | P0 -> gstate.p0.position
  | P1 -> gstate.p1.position
  | PNone -> failwith "Gstate: player_position: PNone is no player"

let current_player_position ~gstate = 
  player_position ~gstate gstate.turn


let opposite_direction = function
  | Left -> Right
  | Right -> Left

let next_player_element ~gstate return_cost =
  let open Lwt in
  match gstate.turn with 
  | P0 -> 
    let next_elem = Lwt_main.run
        (gstate.p0.next_move
           gstate.board 
           gstate.p0.mana
           return_cost)
    in { (empty_wrap_init ()) with
         owner = P0;
         element = next_elem;
       }
  | P1 -> 
    let next_elem = Lwt_main.run
        (gstate.p1.next_move
           gstate.board 
           gstate.p1.mana
           return_cost)
    in { (empty_wrap_init ()) with
         owner = P1;
         element = next_elem;
       }
  | PNone -> failwith "Gstate: PNone is no player"

let add_player_mana ~gstate pid diff = 
  match pid with 
  | P0 ->
    { gstate with 
      p0 = { gstate.p0 with
             mana = gstate.p0.mana +. diff }}
  | P1 ->
    { gstate with 
      p1 = { gstate.p1 with
             mana = gstate.p1.mana +. diff }}
  | PNone -> failwith "Gstate:update_player_mana:PNone is no player."
