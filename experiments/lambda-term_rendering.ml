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

let _  = Lwt_main.run (test2 ())
