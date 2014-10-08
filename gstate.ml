open Batteries
open Core_rand00
open Gametypes

type t = {
  p0 : Player.t;
  p1 : Player.t;
  turn : player_id;
  board : Board.t;
  rules : (module Rules.S);
  winner : player_id option;
}

let player_position gstate = 
  let open Player in
  match gstate.turn with
  | P0 -> gstate.p0.position
  | P1 -> gstate.p1.position
  | PNone -> failwith "Gstate: PNone is no player"

let opposite_direction = function
  | Left -> Right
  | Right -> Left

let next_move gstate pid = 
  let open Player in
  let module R = (val gstate.rules)
  in match pid with 
  | P0 -> 
    let next_elem = 
      gstate.p0.next_move 
        gstate.board 
        gstate.p0.mana
    in { owner = P0;
         element = next_elem;
         mana_cost = R.return_cost next_elem;
         killed = false;
         position = None; }
  | P1 -> 
    let next_elem = 
      gstate.p1.next_move 
        gstate.board 
        gstate.p1.mana
    in { owner = P1;
         element = next_elem;
         mana_cost = R.return_cost next_elem;
         killed = false;
         position = None; } 
  | PNone -> failwith "Gstate: PNone is no player"


