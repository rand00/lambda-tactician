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

(*goto rewrite these functions to modify board.t state and be practical to call from `Control`*)


let player_pos gstate = function
  | P0 -> gstate.p0.position
  | P1 -> gstate.p1.position

let player_opposite = function | P0 -> P1 | P1 -> P0

let make_move from_pos e0 to_pos e1 ~prev_board ~next_board = 
  let 
  in next_board.(pos) <- { elem with just_survived = true }

let next visualizer gstate = 
  let _ = begin

    Array.fold_left (fun i {element; owner} -> 
        if owner = gstate.turn then
          match player_pos gstate owner with
          | `Left  -> "..."
          | `Right -> "...")
      (Array.(make (length gstate.board) Empty), [])
      gstate.board
  end 
  in
  { gstate with 
    board = next_board; 
    turn = (player_opposite gstate.turn) }



