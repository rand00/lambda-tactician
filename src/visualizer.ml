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

module type S = sig 
  val update : Gstate.t -> unit
  val loading : gstate:Gstate.t -> wait_for:'a Lwt.t -> unit Lwt.t 
end

module Term = struct

  module Basic = struct 

    open Gstate
    open Player

    let sep, fill = "|", "_" 
    let mana_fill = "."
    let mana_bar = "|"
    let len_mana, len_name, len_elem = 9, 5, 3
    let len_bbuffer = 1

    let columns b = 
      let len_board = (Board.length b) in
      ((len_mana + 2) * 2) + (len_name * 2) + (len_bbuffer * 2) 
      + (len_elem * len_board) 
      + ((len_board + 1) * String.length sep)

    let strlst_of_elem = function 
      | Lambda (s0, s1) -> [ symbol_to_str s0; "."; symbol_to_str s1 ]
      | Symbol sym -> [ fill; symbol_to_str sym; fill ]
      | Empty -> List.make len_elem fill

    let str_of_pname ~gstate lim pid = 
      let name = Gstate.player_name ~gstate pid in
      if String.length name > lim then
        String.sub name 0 lim 
      else name

    let str_of_pmana ~gstate len_full pid = 
      let mana = Gstate.player_mana ~gstate pid in
      let nbars = Int.of_float (mana *. (Float.of_int len_full)) in
      let ppos = Gstate.player_position ~gstate pid 
      in
      "[" :: ( match ppos with
          | Left -> 
            List.fold_righti (fun i s acc ->
                if nbars > i then 
                  mana_bar :: acc
                else 
                  s :: acc
              ) (List.make len_full mana_fill) ["]"]
          | Right -> 
            List.fold_lefti (fun acc i s ->
                if nbars > i then 
                  mana_bar :: acc
                else 
                  s :: acc
              ) ["]"] (List.make len_full mana_fill) )
      |> String.concat ""

    let board gstate print = 
      let elems = List.of_enum (Board.enum gstate.board) in
      let board_str = String.concat ""
          (List.concat 
             ((List.fold_right (fun elem acc -> 
                  [sep] :: (match elem.killed with
                      | true  -> ( match elem.element with
                          | Lambda _ -> ["###"]
                          | _ -> ["_#_"] )
                      | false -> strlst_of_elem elem.element) :: acc )
               ) elems [[sep]] )) in
      let bbuffer = String.make len_bbuffer ' ' in
      let full = String.concat ""
          [ str_of_pmana ~gstate len_mana P0;
            str_of_pname ~gstate len_name P0;
            bbuffer; board_str; bbuffer;
            str_of_pname ~gstate len_name P1;
            str_of_pmana ~gstate len_mana P1 ]
      in print full

    let update gstate = board gstate print_endline

    let loading ~gstate ~wait_for = 
      let open Lwt in
      let open Lwt_mvar in
      let rec loop () =
        Lwt_unix.sleep 2. >>= fun () -> 
        if is_sleeping (wait_for) then loop ()
        else return ()
      in loop ()

    module Oneline = struct

      let update gstate = board gstate (fun s ->
          Sys.command "tput cuu1" |> ignore;
          print_endline s
        )

      let loading ~gstate ~wait_for = 
        let open Lwt in
        let open Lwt_mvar in
        let tmin_dur = Lwt_unix.sleep 4. in
        let cols = columns gstate.Gstate.board in
        let achars = [|"|"; "/";"-"; "\\"|] in
        let sleep_frame = 0.5 /. (float (Array.length achars)) in
        let output = 
          let s = String.make cols '-' in
          String.blit "Lambda" 0 s (cols/2 - 10) 6; 
          String.blit "Tactician" 0 s (cols/2 + 5) 9; 
          s
        in
        let rec loop n = 
          return (String.blit achars.(n) 0 output (cols / 2) 1) >>
          Lwt_io.printl output >>
          Lwt_unix.system "tput cuu1" >>= fun _ ->
          Lwt_unix.sleep sleep_frame >>= fun () -> 
          if is_sleeping wait_for || is_sleeping tmin_dur then 
            loop (succ n mod 4)
          else return ()
        in loop 0

    end

  end

end



