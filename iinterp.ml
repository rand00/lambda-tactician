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

let match_symbol = function
  | <:re< [" \t"]* ["xX"]{1} [" \t"]* >> -> Some X
  | <:re< [" \t"]* ["yY"]{1} [" \t"]* >> -> Some Y
  | <:re< [" \t"]* ["zZ"]{1} [" \t"]* >> -> Some Z
  | _ -> None

let rec match_element = function
  | <:re< [" \t"]* ( ["xXyYzZ"] as inp ) 
                   [" \t"]* "." [" \t"]*
                   ( ["xXyYzZ"] as out ) [" \t"]* >> -> 
    ( match (match_symbol inp, match_symbol out) with
      | Some sym, Some sym' -> Some (Lambda (sym, sym'))
      | _ -> None )
  | <:re< [" \t"]* ( ["xXyYzZ"] as sym ) >> -> 
    Some (Symbol (Option.get (match_symbol sym)))
  | _ -> None

