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

module type S = sig 
  val run : Gstate.t -> unit
end

module Basic = struct 

  open Gstate
  open Player

  let sep, fill = "|", "_" 
  let mana_fill = "."
  let mana_bar = "|"

  let strlst_of_elem = function 
    | Lambda (s0, s1) -> [ symbol_to_str s0; "."; symbol_to_str s1 ]
    | Symbol sym -> [ fill; symbol_to_str sym; fill ]
    | Empty -> List.make 3 fill

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
    let len_mana, len_name = 9, 5 in
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
    let full = String.concat ""
        [ str_of_pmana ~gstate len_mana P0;
          str_of_pname ~gstate len_name P0;
          " "; board_str; " ";
          str_of_pname ~gstate len_name P1;
          str_of_pmana ~gstate len_mana P1 ]
    in 
    print full

  let run gstate = board gstate print_endline

end

module Basic_oneline = struct
  include Basic
  
  let run gstate = board gstate (fun s ->
      Sys.command "tput cuu1" |> ignore;
      print_endline s
    )

end
