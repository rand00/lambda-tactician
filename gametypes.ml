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

type symbol = X | Y | Z

type player_id = P0 | P1

type element = 
  | Lambda of symbol * symbol
  | Symbol of symbol
  | Empty

type element_wrap = {
  owner : player_id;
  element : element;
  mana_cost : float;
  killed : bool;
}

type game_board = element_wrap array
type temp_board = (element_wrap list) array

type location = 
  | Local
  | Remote of string * int

type mana = float

type player = {
  id : player_id;
  name : string;
  location : location;
  position : [ `Left | `Right ];
  next_move : (game_board -> mana -> element);
  mana : mana;
}

type game_state = {
  p0 : player;
  p1 : player;
  turn : player_id;
  board : game_board;
  winner : player_id option;
}

type player_action = 
  | Move_all_and_add of element
  | Move_all_backward

type game_action = 
  | Kill of element
  | Keep of element
  | Application of element * element
