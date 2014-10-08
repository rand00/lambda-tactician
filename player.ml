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

type location = 
  | Local
  | Remote of string * int

type mana = float

type t = {
  id : Gametypes.player_id;
  name : string;
  location : location;
  position : Gametypes.direction;
  next_move : (Board.t -> mana -> Gametypes.element);
  mana : mana;
}

let opposite = function | P0 -> P1 | P1 -> P0 | PNone -> PNone

