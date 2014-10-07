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

type t = element_wrap array

let enum = Array.enum

let eval action board = 
  | Kill (killer, killed) -> 
  | Application (lambda, value) -> 

;;
let eval_to_consequence action board = 
  match action with 
  | Move_all_and_add elem -> 
  | Move_all_backward -> 

;;
let remove_killed board = 
