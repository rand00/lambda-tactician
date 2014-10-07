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

let _ = 

  let gstate = {
    winner = None;
    board = Array.make 16 Empty;

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
  } in 
  
  in 
  Control.gloop gstate
    ~next_fun:Board.next
    ~visualizer:Visualizer.termprinter0 
    
    



