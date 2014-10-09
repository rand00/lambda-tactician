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

val enum : t -> Gametypes.element_wrap BatEnum.t

val eval_action : Gametypes.element_action -> t -> t

val eval_move_to_effect : Gametypes.board_action -> t 
  -> (Gametypes.board_action_conseqs list) * t

val remove_killed : t -> t


