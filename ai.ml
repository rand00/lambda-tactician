open Batteries 
open Core_rand00
open Gametypes


let random_symbol () = 
  Random.int 3 |> function
  | 0 -> X
  | 1 -> Y
  | 2 -> Z

module Random = struct 

  let random_element (pct_lambda, pct_symbol) = 
    let rand = Random.float 1. in
    if rand < pct_lambda then
      Lambda (random_symbol (), random_symbol ())
    else if rand < (pct_lambda +. pct_symbol) then
      Symbol (random_symbol ())
    else Empty

  (*goto: use lwt here?*)
  let next_move _ _ = 
    let pct_lambda, pct_symbol = (0.1, 0.5)
    in random_element (pct_lambda, pct_symbol)

end
