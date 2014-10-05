open Batteries
open Core_rand00 
open Gametypes

let player_pos gstate = function
  | P0 -> gstate.p0.position
  | P1 -> gstate.p1.position

let player_opposite = function | P0 -> P1 | P1 -> P0

let make_move from_pos e0 to_pos e1 ~prev_board ~next_board = 
  match  with 
  | `Attacking (elem, pos) -> next_board.(pos) <- { elem with just_survived = true }
  | `Defending (elem, pos) -> prev_board.(pos) <- { elem with just_survived = true }


(*goto save a first-class 'rules' module in gstate*)
let next visualizer gstate = 
  let open Array in
  let _ = begin

    (*goto: iterate throgh (map) and remove killed elems*)

    (*moving the current players elems*)
    (*goto return actions + new-array*)
    fold_left (fun i {element; owner} -> 
        if owner = gstate.turn then
          match player_pos gstate owner with
          | `Left  -> make_move i moving_elem (succ i) (gstate.board.(succ i)) 
                        ~prev_board:gstate.board ~next_board 
          | `Right -> make_move i moving_elem (succ i) (gstate.board.(pred i)) 
                        ~prev_board:gstate.board ~next_board )
      ((make (length gstate.board) Empty), [])
      gstate.board
  end 
  in

  (*goto add new elem from curr player + add this to actions*)
  
  (*goto calculate mana-costs/-income from 'actions', and later return mana+winner-gamestate*)

  (*goto visualize*)

  { gstate with 
    board = next_board; 
    turn = (player_opposite gstate.turn) }



