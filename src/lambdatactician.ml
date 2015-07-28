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
open BatExt_rand00
open Gametypes
open Gstate
open Player

let synth_client_while_loadscr () = 
  Lwt_main.run 
    ( let open Lwt in
      let open Lwt_main in 
      let client = 
        SC.Server.run_with_lwt () >>= ( function
            | false -> fail_with
              "Lambdatactian: SuperCollider server (scsynth) failed to start."
            | true -> 
              let%lwt client = return (SC.Client.make ()) in
              let () = at_exit (fun () -> return (SC.Client.quit_all client)) 
              in return client ) in
      let loadscr = Visualizer.Basic_oneline.loading client
      in 
      let%lwt () = loadscr <&> (client >>= fun _ -> return ())
      in client
    )

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
  } 
  in
  Control.gloop gstate
    ~rules:(module Rules.Basic)
    (*>goto change input to be a function instead (no need for more functions than one..?*)
    ~visualizer:(module Visualizer.Basic_oneline)
    ~synth:(synth_client_while_loadscr ())


let _ = run_game ()


    
    




