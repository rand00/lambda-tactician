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

type t = element_wrap list

let make n = 
  List.make
    ( if (n mod 2) <> 0 
      then failwith "Board.make: N is not even." 
      else n ) 
    empty_wrap

let enum = List.enum

let set_killed pos = 
  List.modify_at pos (fun e -> { e with killed = true } ) 

let eval_action board = function
  | Kill (killer_wrap, killed_wrap) -> 
    let pos = killed_wrap.position |> Option.get in
    set_killed pos board

  | Application (lambda_wrap, value_wrap) ->
    (*not checking for equal symbols, as this is a job for rules*)
    let _, lamb_out = get_lambda lambda.element in
    let pos_value, pos_lambda = 
      Tuple.Tuple2.mapn Option.get
        (value_wrap.position, lambda_wrap.position) 
    in 
    set_killed pos_value board
    |> List.modify_at pos_lambda (fun e -> 
        { e with element = Symbol lamb_out }
      ) board
         
  | At_home _ | At_opponent _ -> board
  










