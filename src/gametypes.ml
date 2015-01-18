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

type player_id = P0 | P1 | PNone

type element = 
  | Lambda of symbol * symbol
  | Symbol of symbol
  | Empty

let get_symbol = function
  | Symbol s -> s
  | _ -> failwith "Board.get_symbol: Can only match on Symbol"

let get_lambda = function
  | Lambda (i,o) -> i,o
  | _ -> failwith "Board.get_lambda: Can only match on Lambda"

let symbol_to_str = function
  | X -> "x" | Y -> "y" | Z -> "z"

let element_to_str = function 
  | Lambda (s0, s1) -> "lambda:"^(symbol_to_str s0)^"."^(symbol_to_str s1)
  | Symbol sym -> "sym:"^(symbol_to_str sym)
  | Empty -> "empty"

type element_wrap = {
  owner : player_id;
  element : element;
  mana_cost : float;
  killed : bool;
  id : int; (*autoincremented by empty_wrap >*)
}

let element_i_init = ref 0
let incr_ret_id_init () = let i = !element_i_init in incr element_i_init; i
let element_i_succ = ref 100
let incr_ret_id () = let i = !element_i_succ in incr element_i_succ; 
  (*Printf.printf "succ id specified : %d\n" i; *)
  i

let empty_wrap = {
  owner = PNone; 
  element = Empty; 
  mana_cost = 0.; 
  killed = false;
  id = 0 (*change this manually*)
}

let empty_wrap_init () = { 
  empty_wrap with 
  id = (let i = !element_i_init in 
        incr element_i_init; 
        (*Printf.printf "succ id specified : %d\n" i; *)
        i)
}

type direction = Left | Right

type element_wrap_active = element_wrap

type element_action = 
  | Kill of element_wrap_active * element_wrap
  | Application of element_wrap_active * element_wrap
  | At_opponent of element_wrap
  | At_home of element_wrap

type board_action =
  | Move_all_and_add of player_id * direction * element_wrap
  | Move_all of player_id * direction

type board_move_conseq = 
  | Jumpover of element_wrap_active * element_wrap
  | Out_of_bounds of direction * element_wrap 

(*
type 'a tmp = 
  | Jumpover of 'a * 'a
  | Out_of_bounds of direction * 'a
*)
