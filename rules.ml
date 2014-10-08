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

module type S = sig 
  
  val apply_cost : element_wrap -> element_wrap

  val return_cost : element -> float

end

module Basic : S = struct 

  let cost_lambda = 0.30
  let cost_symbol = 0.30
  let cost_empty = 0.0

  let apply_cost = function
    | { element = Lambda _ } as e -> { e with mana_cost = cost_lambda } 
    | { element = Symbol _ } as e -> { e with mana_cost = cost_symbol }
    | { element = Empty } as e -> { e with mana_cost = cost_empty } 

  let return_cost = function 
    | Lambda _ -> cost_lambda
    | Symbol _ -> cost_symbol
    | Empty -> cost_empty

end


