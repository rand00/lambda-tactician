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

(*For one-layer animations*)
let layer_eq ~eq s s' = 
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

(*For layered animations*)
let equal ~eq l l' = List.fold_right2 (fun e e' acc_eq -> 
    acc_eq && layer_eq ~eq e e'
  ) l l' true

let map_rules e = function 
  | Some r -> r e
  | None -> e

let rec incr_layer = function
  | Ae (e, rules) -> Ae (map_rules e rules, rules)
  | Al (el, rules) -> Al (map_rules (List.map incr_layer el) rules, rules)

let incr_anim l = List.map incr_layer l

let eval_layer ~cat ~get = 
  let rec aux = function
    | Ae (e, _) -> get e
    | Al (e::el, _) -> List.fold_left (fun acc e -> 
        cat acc (aux e)
      ) (aux e) el
    | Al ([], _) -> failwith "Empty animation!"
  in aux 

let eval ~cat ~get = List.map (eval_layer ~cat ~get)


