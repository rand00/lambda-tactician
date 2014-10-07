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
  position : int option; (*for Board internal use?*)
}

type direction = | Left | Right

type element_wrap_active = element_wrap

type element_action = 
  | Kill of element_wrap_active * element_wrap
  | Application of element_wrap_active * element_wrap

type board_action =
  | Move_all_and_add of direction * element_wrap
  | Move_all of direction

type board_action_result = 
  | Jumpover of element_wrap_active * element_wrap
  | Out_of_bounds of direction element_wrap 
