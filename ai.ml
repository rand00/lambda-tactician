open Batteries 
open Core_rand00
open Gametypes


let random_symbol () = 
  Random.int 3 |> function
  | 0 -> X
  | 1 -> Y
  | 2 -> Z

module Random = struct 

  let random_element player_id (pct_lambda, pct_symbol) = 
    let rand = Random.float 1. in
    if rand < pct_lambda then
      Lambda (player_id, [random_symbol ()], [random_symbol ()])
    else if rand < (pct_lambda +. pct_symbol) then
      Symbol (player_id, random_symbol ())
    else Empty

  let next_move player_id _ = 
    let pct_lambda, pct_symbol = (0.1, 0.5)
    in random_element player_id (pct_lambda, pct_symbol)

end
