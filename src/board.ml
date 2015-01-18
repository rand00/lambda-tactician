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
  |> List.map (fun e -> 
      { e with id = incr_ret_id_init () } )
(*< gomaybe is this map neccesary? - implemented when trying to correct bug
  - why would empty cells get modified..*)

let enum = List.enum

let find_pos e board = 
  let pos,_ = List.findi (fun i e' -> 
      e.id = e'.id ) board
  in pos

(*goto bug here? 
  - fold right should be correct - check now again
  - there is no visualization of this... hmm check - also other places where possible bug*)
let set_killed e board = 
(*  print_endline "\nCalling 'set_killed'\n";*)
  List.fold_right (fun e' acc -> 
(*      Printf.printf "e.id = '%d' & e'.id = '%d'\n" e.id e'.id;*)
      if e.id = e'.id then 
        { e' with killed = true } :: acc
      else e' :: acc ) 
    board
    []

(**not in use*)
let set_killed_at pos = 
  List.modify_at pos (fun e -> { e with killed = true } ) 

(*gomaybe gofix; better way of modifying elems in list than search+get pos?*)
let eval_action board = function
  | Kill (killer_wrap, killed_wrap) -> 
    (*print_endline "Calling 'eval_action' in 'Kill' match";*)
    set_killed killed_wrap board

  (*goto > some kind of bug here with replication of symbols all over? check again*)
  | Application (lambda_wrap, value_wrap) ->
(*    print_endline "\nCalling 'eval_action' in 'Application' match\n";*)
    (*not checking for equal symbols, as this is a job for rules*)
    let _, lamb_out = get_lambda lambda_wrap.element in
    set_killed value_wrap board
    |> List.fold_left (fun acc e -> 
        Printf.printf "e.id = '%d' & lambda_wrap.id = '%d'\n" e.id lambda_wrap.id;
        if e.id = lambda_wrap.id then 
          { e with element = Symbol lamb_out } :: acc 
        else e :: acc)
      []
         
  | At_home _ | At_opponent _ -> board

(**not in use*)
let rec zipswitch_a0 ?(append=[]) ?(remove_last=false) = function
  | [] -> append
  | e::[] -> if remove_last then append else e::append
  | e::e'::[] -> if remove_last then e'::append else e'::e::append
  | e::e'::tl -> e'::e::(zipswitch_a0 ~append ~remove_last tl)

(*hmm weird version - possible to make better? 
  - fx by writing a non-tailrec version? *)
let zipswitch ?(append=[]) direction l = 
  let rec aux (conseq_acc, board_acc) = function
    | [] -> (conseq_acc, (List.rev append) @ board_acc)
    | e::[] -> (conseq_acc, (List.rev append) @ (e::board_acc))
    | e::e'::tl -> ( match direction with 
        | Right -> aux ((Jumpover (e,e'))::conseq_acc, 
                        (e::e'::board_acc)) tl
        | Left  -> aux ((Jumpover (e',e))::conseq_acc, 
                        (e::e'::board_acc)) tl ) 
  in 
  let conseqs, board = aux ([],[]) l 
  in (conseqs, (List.rev board))

let pop_last l = 
  let rec aux acc = function
    | [] -> failwith "Board.pop_last: don't give me an empty list"
    | last::[] -> (List.rev acc), last
    | e::tl -> aux (e::acc) tl
  in aux [] l

let pop_first = function
  | [] -> failwith "Board.pop_first: don't give me an empty list"
  | fst :: tl -> fst, tl

let move_all_and_add direction elem board = 
  match direction with 
  | Right -> 
    let conseqs, board' = zipswitch Right board in
    let board',last = board' |> pop_last in 
    let board' = elem::board' in
    let conseqs = match last with 
      | { owner = P0 } | { owner = P1 } ->
        (Out_of_bounds (Right, last))::conseqs 
      | _ -> conseqs
    in conseqs, board'

  | Left  -> 
    let conseqs, board' = zipswitch Left board ~append:[elem] in
    let first, board' = board' |> pop_first in 
    let conseqs = match first with 
      | { owner = P0 } | { owner = P1 } ->
        (Out_of_bounds (Left, first))::conseqs
      | _ -> conseqs
    in conseqs, board'

let remove_killed_elems = 
(*  print_endline "\nCalling 'remove_killed_elems'\n";*)
  List.map (function 
      | { killed = true; id } -> { empty_wrap with id }
      | other -> other) 
  









