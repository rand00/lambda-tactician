open Batteries
open Core_rand00 
open Gametypes

let gloop gstate_init ~next_fun ~visualizer = 
  let next = next_function visualizer
  in
  let rec aux = function
    | { winner = Some player; p0; p1 } -> 
      (match player with 
       | P0 -> print_endline "And the winner is "^p0.name^"!"
       | P1 -> print_endline "And the winner is "^p1.name^"!")
    | gstate -> gameloop (next gstate)
  in 
  aux gstate_init

