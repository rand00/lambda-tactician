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

let _ = Random.self_init ()

let random_symbol () = 
  Random.int 3 |> function
  | 0 -> X
  | 1 -> Y
  | _ -> Z

module Random = struct 

  open Lwt
  
  let random_element (pct_lambda, pct_symbol) = 
    let rand = Random.float 1. in
    if rand < pct_lambda then
      Lambda (random_symbol (), random_symbol ())
    else if rand < (pct_lambda +. pct_symbol) then
      Symbol (random_symbol ())
    else Empty

  let next_move _ _ = 
    let pct_lambda, pct_symbol = (0.1, 0.5) in
    Lwt_unix.sleep ((Random.float 1.5) +. 0.1)
    >> return (random_element (pct_lambda, pct_symbol))

  
end
