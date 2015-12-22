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


let run_with_loadscreen ~gstate ~visualizer = 
  let module V = (val visualizer: Visualizer.S) in
  let open Lwt in
  let client = 
    SC.Server.Lwt.run () >>= ( function
        (*>goto: should be a notification instead of a failure - (and also some debug info)*)
        | false -> fail_with
                     "Lambdatactian: SuperCollider server (scsynth) failed to start."
        | true -> SC.Client.Lwt.make ()) in
  let wait_for = client >>= fun _ -> Lwt.return_unit in
  let loadscr = V.loading ~gstate ~wait_for ()
  in 
  let%lwt () = loadscr <&> (client >>= fun _ -> return ())
  in client

