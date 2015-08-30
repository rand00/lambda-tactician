open Batteries

(**Abstract type of animations*)

module T = struct

  type 'a rules = 'a -> 'a

  (*The 'a type parameter is for saving state about the animation-elements (Ae's)*)
  type 'a anim = 
    | Al of 'a anim list * 'a anim list rules option
    | Ae of 'a * 'a rules option

end

open T

(**Evaluation*)

let state_eq ~eq s s' = 
  let rec aux s s' = 
    match s, s' with
    | Al (le, _), Al (le', _) -> begin
        try List.fold_right2 (fun e e' acc_eq -> 
            acc_eq && aux e e'
          ) le le' true
        with LazyList.Different_list_size _ -> false
      end
    | Ae (state, _), Ae (state', _) -> eq state state'
    | _ -> false
  in aux s s'

let map_rules e = function 
  | Some r -> r e
  | None -> e

let rec incr_anim = function
  | Ae (e, rules) -> Ae (map_rules e rules, rules)
  | Al (el, rules) -> Al (map_rules (List.map incr_anim el) rules, rules)

let eval ~cat ~get = 
  let rec aux = function
    | Ae (e, _) -> get e
    | Al (e::el, _) -> List.fold_left (fun acc e -> 
        cat acc (aux e)
      ) (aux e) el
    | Al ([], _) -> failwith "Empty animation!"
  in aux 




