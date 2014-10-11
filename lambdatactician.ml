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

let start () =
  
  let gstate = {
    winner = None;
    board = Board.make 16;

    element_costs = {
      lambda = 0.3;
      symbol = 0.3;
      empty = 0.0;
    };

    rules = (module Rules.Basic : Rules.S);

    p0 = {
      id = P0; name = "Hansi";
      location = Local;
      position = `Left;
      next_move = Ai.Random.next_move;
      mana = 1.;
    };

    p1 = {
      id = P1; name = "Finka";
      location = Local;
      position = `Right;
      next_move = Ai.Random.next_move;
      mana = 1.;
    };
  } 
  
  in Control.gloop gstate
  (*  set  ~visualizer:Visualizer.termprinter0 *)


let _ = start ()


    
    




