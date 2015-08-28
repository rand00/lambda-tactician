open Batteries

(**Abstract type of animations*)

type 'a rules = 'a -> 'a

type 'a anim = 
  | Al of 'a anim list * 'a anim list rules option
  | Ae of 'a * 'a rules option

(**Some internal representation of animation-elements*)

type 'a state = {
  c : 'a;
(*  prev : 'a option;*)
}

let std_state = {
  c = ""; 
}

let rec state_eq s s' = match s, s' with
  | Al (le, _), Al (le', _) -> begin
      try List.fold_right2 (fun e e' eq -> 
          eq && state_eq e e'
        ) le le' true
      with LazyList.Different_list_size _ -> false
    end
  | Ae (state, _), Ae (state', _) -> state = state'
  | _ -> false

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

(**FRP deps for anims*)

open Lwt
open Lwt_react

let (>|~) = (fun e f -> S.map f e)

let frames, send_frame = E.create ()
(*let print_frame = S.map (Lwt_io.printf "%d\n") frames*)

let frames_s = S.hold 0 frames

(*goto this is an interesting but weird definition where both a signal and event of frames is used... 
  - how to make more clean alternative? 
      => fix need a function that will give same value on second update step (with equal input as before)*)
let anim = 
  let anim_def = 
    Al ([ 
        Ae ({ std_state with c = "[" }, Some (function 
            | { c = "[" } -> { c = "-" }
            | _ -> { c = "[" }
          ));
        Ae ({ std_state with c = "" },
            Some (fun _ -> match S.value frames_s mod 4 with
                | 0 -> { c = "   " }
                | 1 -> { c = "|  " }
                | 2 -> { c = "|| " }
                | 3 -> { c = "|||" }
              ));
        Ae ({ std_state with c = "]" }, None);
      ], None)
    |> incr_anim
  in S.fold ~eq:state_eq (fun anim_acc _ -> incr_anim anim_acc) anim_def frames

(*
let anim = 
  let anim_def = 
    Al ([ 
        Ae ({ std_state with c = "[" }, Some (function 
            | { c = "[" } -> { c = "-" }
            | _ -> { c = "[" }
          ));
        Ae ({ std_state with c = "" },
            Some (fun _ -> match S.value frames mod 4 with
                | 0 -> { c = "   " }
                | 1 -> { c = "|  " }
                | 2 -> { c = "|| " }
                | 3 -> { c = "|||" }
              ));
        Ae ({ std_state with c = "]" }, None);
      ], None)
  in S.fix ~eq:state_eq (anim_def) (fun s -> 
      let s' = S.map ~eq:state_eq (fun anim -> incr_anim anim ) s
      in s', s'
    )
*)

let eval_anim = anim >|~ eval ~cat:(^) ~get:(fun {c} -> c) 

let print_anim = eval_anim >|~ (fun s -> Sys.command "tput cuu1" |> ignore; Lwt_io.printf "%s\n" s)

let () = 
  let frame_n () =
    let rec aux n = 
      let () = send_frame n in
      Lwt_unix.sleep 0.04 >> aux (if n = 1000 then 0 else succ n)
    in aux 0
  in Lwt.async (fun () -> (return (Lwt_main.run (frame_n ()))))

(**Tests*)

(*
let run () =
  (*goto parametrize animation, and lift it over frp dep's*)
  let anim = 
    Al ([ 
        Ae ({ std_state with c = "[" }, 
            Some ( function 
                | {c; prev = None;}   -> {c = "Q"; prev = Some c}
                | {c; prev = Some c'} -> {c = c'; prev = Some c} 
              ));
        Ae ({ std_state with c = "|||" }, None);
        Ae ({ std_state with c = "]" }, 
            Some ( function         
                | {prev = None; c} -> {prev = Some c; c = "Z"}
                | {prev = Some c'; c} -> {prev = Some c; c = c'}
              ));
      ], None)
  in 
  let eval_anim = eval ~cat:(^) ~get:(fun {c} -> c) in
  let rec loop anim = function
    | 0 -> ()
    | n -> begin
        print_endline (eval anim);
        loop (incr_anim anim) (pred n)
      end
  in loop anim 10 
*)



