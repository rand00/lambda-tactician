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
open Gstate
open Player

let run_game () =
  
  let gstate = {
    winner = None;
    turn = P0;
    board = Board.make 16;
    rvalues = Rules.std_values;

    p0 = { id = P0; name = "Hansi";
           location = Local;
           position = Left;
           next_move = Ai.NoSuicideAI.next_move;
           mana = 1.;
         };

    p1 = { id = P1; name = "Finka";
           location = Local;
           position = Right;
           next_move = Ai.NoSuicideAI.next_move;
           mana = 1.; };
  } in

  let _ = Synth.Server.run () in
  let _ = print_endline "" in

  let module C : Synth.CSig = struct
    let client = Synth.Client.make () end in

  let _ = at_exit (fun () -> Synth.quit_all C.client) in

  (*goto: make load-screen while waiting for server with lwt_unix.sleep*)
  let _ = Unix.sleep 4 in
  
  Control.gloop gstate
    ~rules:(module Rules.Basic)
    ~visualizer:(module Visualizer.Basic_oneline)
    ~synth:(module Synth.Make (C) : Synth.S)


let _ = run_game ()


    
    




