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
open BatExt_rand00
open Gametypes

module type S = sig 
  val suggest_len : unit -> int
  val update : Gstate.t -> unit Lwt.t
  val loading : gstate:Gstate.t -> wait_for:'a Lwt.t -> unit Lwt.t 
end

module Term = struct

  (*goto see notebook for changes/todo*)
  module Basic = struct 

    open Gstate
    open Player

    let sep, fill = "|", "_" 
    let mana_fill = "."
    let mana_bar = "|"
    let len_mana, len_name, len_elem = 9, 5, 3
    let len_bbuffer = 1

    let columns_aux len_board = 
      ((len_mana + 2) * 2) + (len_name * 2) + (len_bbuffer * 2) 
      + (len_elem * len_board) 
      + ((len_board + 1) * String.length sep)
    
    let columns b = 
      let len_board = (Board.length b) in
      columns_aux len_board

    let suggest_len () = 
      let term_cols = Sys.term_ncolumns () in
      (*let _ = Printf.printf "term # cols: %d\n" term_cols in*)
      let rec iter_find len = 
        let nxt_len = columns_aux len in
        if nxt_len > term_cols then max (len-2) 4 else iter_find (len + 2)
      in
      iter_find 4

    let strlst_of_elem = function 
      | Lambda (s0, s1) -> [ symbol_to_str s0; "."; symbol_to_str s1 ]
      | Symbol sym -> [ fill; symbol_to_str sym; fill ]
      | Empty -> List.make len_elem fill

    let str_of_pname ~gstate lim pid = 
      let name = Gstate.player_name ~gstate pid in
      if String.length name > lim then
        String.sub name 0 lim 
      else name

    let str_of_pmana ~gstate len_full pid = 
      let mana = Gstate.player_mana ~gstate pid in
      let nbars = Int.of_float (mana *. (Float.of_int len_full)) in
      let ppos = Gstate.player_position ~gstate pid 
      in
      "[" :: ( match ppos with
          | Left -> 
            List.fold_righti (fun i s acc ->
                if nbars > i then 
                  mana_bar :: acc
                else 
                  s :: acc
              ) (List.make len_full mana_fill) ["]"]
          | Right -> 
            List.fold_lefti (fun acc i s ->
                if nbars > i then 
                  mana_bar :: acc
                else 
                  s :: acc
              ) ["]"] (List.make len_full mana_fill) )
      |> String.concat ""

    let board gstate print = 
      let elems = List.of_enum (Board.enum gstate.board) in
      let board_str = String.concat ""
          (List.concat 
             ((List.fold_right (fun elem acc -> 
                  [sep] :: (match elem.killed with
                      | true  -> ( match elem.element with
                          | Lambda _ -> ["###"]
                          | _ -> ["_#_"] )
                      | false -> strlst_of_elem elem.element) :: acc )
               ) elems [[sep]] )) in
      let bbuffer = String.make len_bbuffer ' ' in
      let full = String.concat ""
          [ str_of_pmana ~gstate len_mana P0;
            str_of_pname ~gstate len_name P0;
            bbuffer; board_str; bbuffer;
            str_of_pname ~gstate len_name P1;
            str_of_pmana ~gstate len_mana P1 ]
      in print full

    let update gstate = board gstate print_endline

    let loading ~gstate ~wait_for = 
      let open Lwt in
      let open Lwt_mvar in
      let rec loop () =
        Lwt_unix.sleep 2. >>= fun () -> 
        if is_sleeping (wait_for) then loop ()
        else return ()
      in loop ()

    module Oneline = struct

      let suggest_len = suggest_len

      let update gstate = board gstate (fun s ->
          Sys.command "tput cuu1" |> ignore;
          print_endline s
        )

      let loading ~gstate ~wait_for = 
        let open Lwt in
        let open Lwt_mvar in
        let tmin_dur = Lwt_unix.sleep 4. in
        let cols = columns gstate.Gstate.board in
        let achars = [|"|"; "/";"-"; "\\"|] in
        let sleep_frame = 0.5 /. (float (Array.length achars)) in
        let output = 
          let s = String.make cols '-' in
          String.blit "Lambda" 0 s (cols/2 - 10) 6; 
          String.blit "Tactician" 0 s (cols/2 + 5) 9; 
          s
        in
        let rec loop n = 
          return (String.blit achars.(n) 0 output (cols / 2) 1) >>
          Lwt_io.printl output >>
          Lwt_unix.system "tput cuu1" >>= fun _ ->
          Lwt_unix.sleep sleep_frame >>= fun () -> 
          if is_sleeping wait_for || is_sleeping tmin_dur then 
            loop (succ n mod 4)
          else return ()
        in loop 0

    end

  end

  module Fancy : S = struct

    open Lwt
    open Lwt_react

    let term = Iinterp.Term.t

    let gstate, send_gstate = E.create ()

    let frames, send_frame = E.create ()
    let frames_s = S.hold 0 frames
  (*
    let print_frame = 
      let s = S.map (Lwt_io.printf "frame: %d\n\n") frames_s in
      let _ = S.keep s in s
   *)

    (*howto; map gameboard to set of signals (animations) 
      > but where does the animations get their updates from? > a mapped 'update' over Gstate.t -> diff signals (of anims)
           , later collected together in a full signal (of a gameboard anim collection)
             > eval this to Lambdaterm printable type
        . how are new animations initiated?
          > are they just statically defined closures acting on board/gstate? 
            > or can you add new animations to the graph? (one would need a recursive
              animation definition? -> we already have! see constructor Al who's rule can 
              add new animations inside list; and rule can depend on some signal! (with S.value)
    *)

    let app_mode, send_app_mode = E.create ()

    let first_update = ref true

    let run_frames_on_first () =
      let rec aux n = 
        let () = send_frame n in
        Lwt_unix.sleep 0.04 
        (*>> Lwt_io.printf "\nframe from thread: %d \t first update: %b\n\n" n !first_update*)
        >> aux (if n = 1000 then 0 else succ n)
      in 
      if !first_update then (
        Lwt.async (fun () -> (aux 0));
        first_update := false; 
      )

    (**Input functions*)

    let loading ~gstate ~wait_for = 
      run_frames_on_first ();
      send_app_mode Gstate.(`Mode_loading);
      wait_for >>= fun _ -> Lwt_unix.sleep 3.

    let update gstate = 
      run_frames_on_first ();
      send_app_mode Gstate.(`Mode_game);
      Lwt.wrap1 send_gstate gstate

    (**Game state*)

    let (>|~) e f = S.map f e 
    let (>>~) f e = S.bind f e 

    let columns_get () = LTerm.get_size term >|= LTerm_geom.cols 

    let columns = 
      E.filter (fun frames -> frames mod 5 = 0) (*every n'th frame, update*)
        frames
      |> E.map_s (fun _ -> columns_get ()) 
      |> S.hold (Lwt_main.run (columns_get())) (*run is only run once (and returns quickly)*)

(*
    let print_columns = S.map (Lwt_io.printf "columns: %d\n\n") columns 
    let _ = S.keep print_columns
*)

    (*goto (later) make this depend on THIS modules visualization*)
    let suggest_len = Basic.suggest_len

    let p0_mana = Gstate.( Player.(
        let mana = E.map (fun {p0} -> p0.mana) gstate
        in S.hold 1. mana
      ))

    let p1_mana = Gstate.( Player.(
        let mana = E.map (fun {p1} -> p1.mana) gstate
        in S.hold 1. mana
      ))

    let winner = Gstate.( Player.(
        let winner = E.map (fun {winner} -> winner) gstate
        in S.hold None winner
      ))

    (**Animations*)

    open Anim.T
    open LTerm_style 

    (*gomaybe think if I want to lift ALL layers at the same time, or collect them later
      after they've become signals... *)
    (*goto possibly extend this function with a non-changing fold over (S.value of) messages
      > this way overlays can be rendered without becoming part of animation layers @ def time
      > still.. think of better solution?
    *)
    let lift_anim anim_def = S.fold 
        (fun anim_acc _ -> Anim.incr_anim anim_acc) 
        ~eq:(Anim.equal ~eq:(=))
        anim_def
        frames

    type state = {
      s : string;
      i : int; (*indent from left*)
      c_fg : color;
      c_bg : color
      (*  prev : 'a option;*)
    }

    (*goto rename to new_st?*)
    let std_st = {
      s = ""; 
      i = 0;
      c_fg = default;
      c_bg = default 
    }

    let c_i = LTerm_style.index

    (*goto make more helper funcs + move these modules somewhere else?*)

    module Ae = struct 
      let indent_incr inc each ({i} as st) = {
        st with i=(if S.value frames_s mod each = 0 then i + inc else i)
      } 
    end

    module Al = struct 
      let indent_head plus = function
        | [] -> [] 
        | (Ae ({i} as st, f))::tl -> 
          (Ae ({st with i = i + plus}, f))::tl 
    end

    module Adef = struct 

      let make_curtain dir str ~len ~indent ~cols = 
        Al (List.init len (fun iter -> 
            Ae ({ std_st with 
                  s = str; 
                  c_fg = c_i 8; 
                  i = indent
                }, None )) 
            |> Al.indent_head ( match dir with 
                | `Go_left -> cols - (len*(indent +1))
                | `Go_right -> 0 ), 
            (*<goto this case shows we want a rendering wrapper
               that supports 'overdraw' to the right (left is fine) .. *)
            Some (Al.indent_head (match dir with `Go_left -> -1 | _ -> 1)))

    end

    (*goto change loading(/splash) animation to one with layers
        1. transform "lambda tactician" letters + color swoosh over?
        2. add colors (+/color-animations) to curtains purple,redish,blueish
    *)
    (*>goo2*)
    let loading_anim = 
      let s0, s1 = "Lambda", "Tactician" 
      and cols = S.value columns 
      and space_between = 9 in
      let outer_space str = 
        ((float cols) /. 2.) -. ((float space_between) /. 2.) -.
        (float (String.length str))
        |> int_of_float in
      let bg = Ae (
          { std_st with s = String.make cols '-'; c_fg = c_i 3 }, None) 
      and title = Al ([ 
          Ae ({ std_st with s = s0; c_fg = c_i 3; i = outer_space s0 }, None);
          Ae ({ std_st with s = s1; c_fg = c_i 3; i = space_between }, None);
        ], None) 
      in lift_anim [
        bg; title; 
        Adef.make_curtain `Go_left "\\" ~len:30 ~indent:3 ~cols;
        Adef.make_curtain `Go_right "/" ~len:5 ~indent:4 ~cols
      ]

    let game_anim = 
      lift_anim [
        Ae ({ std_st with s = ">> game is running" }, 
            Some (fun st -> match (S.value frames_s / 4) mod 4 with
                | 0 -> { st with s = ">> game is running -" }
                | 1 -> { st with s = ">> game is running --" }
                | 2 -> { st with s = ">> game is running ---" } 
                | 3 -> { st with s = ">> game is running ----" }
                | _ -> { st with s = ">> game is running" }
              ))
      ]
    (*<goto make animation game-board 
      > composed by:
        . mana-bars
        . name
        . gameboard (how to map blocks to enduring animations? (id's rem in anim-state?))
    *)
    (*< let some 'layer-rule'-closure be mapped over the 'anim_layers' signal
          > this depends (with S.value or normal signal depend) on some signals with 
            state for creating messages e.g.
    *)


    (**Game-mode switching*)

    let visu_switcher = 
      let app_mode_s = (S.hold `Mode_loading app_mode) in
      S.map (function
            `Mode_loading -> loading_anim
          | `Mode_game -> game_anim
        ) app_mode_s
      |> S.switch 

    (**Rendering*)

    (*gomaybe Anim.eval should be done more efficiently with some kind of fold instead*)
    let anim_layers = LTerm_text.( 
        visu_switcher >|~ (fun l -> 
            Anim.eval l
              ~cat:(@) 
              ~get:(fun st -> [st])
          )
      )

    let visu_width = visu_switcher >|~ fun layers -> 
      Anim.eval layers
        ~cat:(+)
        ~get:(fun {s; i} -> i + (Zed_utf8.length s))
      |> fun lengths -> List.fold_right max lengths 0

    let render_width = S.l2 (fun visu_width columns -> 
        max visu_width columns
      ) visu_width columns

    let draw_matrix = render_width >|~ (fun render_width -> 
        LTerm_geom.( 
          LTerm_draw.make_matrix {rows=1; cols=(render_width)} )
      ) 

    let draw_context = S.l2 (fun render_width matrix-> 
        let dim = LTerm_geom.({rows=1; cols=render_width}) in
        LTerm_draw.context matrix dim
      ) render_width draw_matrix

    (*goto define wrapper for render*)
    (*<goo*)

    let visualize = 
      S.l3 (fun layers context matrix -> 
          LTerm_draw.clear context;
          List.iter (fun layer -> 
              List.fold_left (fun pos_acc {s; i; c_fg; c_bg} ->
                  LTerm_text.([B_fg c_fg; B_bg c_bg; S s; E_bg; E_fg])
                  |> LTerm_text.eval 
                  |> LTerm_draw.draw_styled context 0 (pos_acc + i);
                  (*<goto use (to be) new render wrapper*)
                  (pos_acc + i + (Zed_utf8.length s))
                ) 0 layer |> ignore; 
              ()
            ) layers;
          LTerm.render term matrix
        ) anim_layers draw_context draw_matrix


    let _ = S.keep visualize

  end


end



