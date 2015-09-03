open Batteries
open Lwt
open CamomileLibrary.UPervasives

let test1 () = 
  let m = LTerm_draw.make_matrix 
      LTerm_geom.({rows=1; cols=300}) in
  let m' = Array.map (fun a -> 
      Array.map (fun ({LTerm_draw.char} as r) -> 
          {r with LTerm_draw.char = uchar_of_int 113}
        ) a
    ) m in
  let term = 
    let open Lwt in
    Lwt_main.run (
      let%lwt term = Lazy.force LTerm.stdout in
      (return term) 
    ) in
  LTerm.render term m'

let test2 () = 
  let m = LTerm_draw.make_matrix 
      LTerm_geom.({rows=1; cols=300}) in
  let n = LTerm_draw.make_matrix 
      LTerm_geom.({rows=1; cols=600}) in
  let m' = Array.map (fun a -> 
      Array.map (fun ({LTerm_draw.char} as r) -> 
          {r with LTerm_draw.char = uchar_of_int 113}
        ) a
    ) m in
  let n' = Array.map (fun a -> 
      Array.map (fun ({LTerm_draw.char} as r) -> 
          {r with LTerm_draw.char = uchar_of_int 114}
        ) a
    ) n in
  let term = 
    let open Lwt in
    Lwt_main.run (
      let%lwt term = Lazy.force LTerm.stdout in
      (return term) 
    ) in
  LTerm.render term n' >>
  Lwt_unix.sleep 1. >>
  LTerm.render term m' >>
  Lwt_unix.sleep 1. >>
  LTerm.render_update term n' m' >>
  Lwt_unix.sleep 1. >>
  LTerm.render_update term m' n' >>
  Lwt_unix.sleep 1. >>
  LTerm.render_update term n' m' 


(*testing rendering contexts*)
let test3 () = 
  let open LTerm_geom in 
  let d = {rows=1; cols=300} in
  let m = LTerm_draw.make_matrix d in
  let c = LTerm_draw.context m d in
  let t = Iinterp.Term.t in
  for i=0 to 50 do
    let _ = LTerm_draw.clear c in
    let _ = LTerm_draw.draw_styled c 0 i 
        LTerm_text.( LTerm_style.(
            eval [B_fg (index i); S "woow"; E_fg ]
          )) in
    Lwt_main.run (
      LTerm.render t m
      >> Lwt_unix.sleep 1.)
  done


(*testing rendering 'outside' context*)
let test4 () = 
  let open LTerm_geom in 
  let d = {rows=1; cols=300} in
  let m = LTerm_draw.make_matrix d in
  let c = LTerm_draw.context m d in
  let t = Iinterp.Term.t in
  for i=6 downto -20 do
    let _ = LTerm_draw.clear c in
    let _ = LTerm_draw.draw_styled c 0 i 
        LTerm_text.( LTerm_style.(
            eval [B_fg (index (abs i)); S "woow"; E_fg ]
          )) in
    Lwt_main.run (
      LTerm.render t m
      >> Lwt_unix.sleep 1.)
  done

let _  = test4 ()
