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

type t

val make : int -> t

val length : t -> int

val enum : t -> Gametypes.element_wrap BatEnum.t

val eval_action : t -> Gametypes.element_action -> t

(*val move_to_effect : Gametypes.board_action -> t 
  -> (Gametypes.board_move_conseq list) * t*)

val move_all_and_add : 
  Gametypes.direction -> 
  Gametypes.element_wrap -> 
(*  elems_owned_by:Gametypes.player_id -> *)
  t -> 
  Gametypes.board_move_conseq list * t

val remove_killed_elems : t -> t


