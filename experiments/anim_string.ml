open Batteries
(*FRP deps for anims*)
open Lwt
open Lwt_react
open LTerm_style 

open Anim 

let (>|~) e f = S.map f e 

(**Tests*)

type state = {
  s : string;
  c_fg : color;
  c_bg : color
(*  prev : 'a option;*)
}

let std_state = {
  s = ""; 
  c_fg = default;
  c_bg = default 
}

let frames, send_frame = E.create ()
(*let print_frame = S.map (Lwt_io.printf "%d\n") frames*)

let frames_s = S.hold 0 frames

let term = Iinterp.Term.t

let anim = 
  let anim_def = 
    Al ([ 
        Ae ({ std_state with s = "[" }, Some (function 
            | { s = "[" } as st -> { st with s = "-" }
            | st -> { st with s = "[" }
          ));
        Ae ({ std_state with s = "" },
            Some (fun _ -> match S.value frames_s mod 4 with
                | 0 -> { std_state with s = "   "; c_fg = index 160 }
                | 1 -> { std_state with s = "|  "; c_fg = index 161 }
                | 2 -> { std_state with s = "|| "; c_fg = index 162 }
                | 3 -> { std_state with s = "|||"; c_fg = index 163 }
              ));
        Ae ({ std_state with s = "]" }, None);
      ], None
      ) |> Anim.incr_anim
  in S.fold ~eq:Anim.state_eq (fun anim_acc _ -> Anim.incr_anim anim_acc) anim_def frames

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


(*goto make eval LTerm instead*)
let eval_anim = LTerm_text.( Anim.( 
    anim >|~ Anim.eval 
      ~cat:(@) 
      ~get:(fun {s; c_fg; c_bg} -> [B_fg c_fg; B_bg c_bg; S s; E_fg; E_bg] ) 
  ))

let print_anim = eval_anim >|~ (fun txt -> 
    Sys.command "tput cuu1" |> ignore; 
    LTerm.printls (LTerm_text.eval txt)
  )

(* for simple string evaluation>>
let eval_anim = Anim.( anim >|~ Anim.eval ~cat:(^) ~get:(fun {s} -> s) )
let print_anim = eval_anim >|~ (fun s -> Sys.command "tput cuu1" |> ignore; Lwt_io.printf "%s\n" s)
*)

let () = 
  let frame_n () =
    let rec aux n = 
      let () = send_frame n in
      Lwt_unix.sleep 0.04 >> aux (if n = 1000 then 0 else succ n)
    in aux 0
  in Lwt.async (fun () -> return (Lwt_main.run (frame_n ())))

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
