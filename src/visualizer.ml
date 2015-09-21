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

    let board gstate = 
      let elems = Board.list gstate.board in
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
      String.concat "" [
        str_of_pmana ~gstate len_mana P0;
        str_of_pname ~gstate len_name P0;
        bbuffer; board_str; bbuffer;
        str_of_pname ~gstate len_name P1;
        str_of_pmana ~gstate len_mana P1;
      ]

    let update gstate = print_endline (board gstate)

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

      let update gstate = 
        Sys.command "tput cuu1" |> ignore;
        Lwt_io.printl (board gstate)

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

  module Fancy () : S = struct

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
      > but where does the animations get their updates from? 
         > a mapped 'update' over Gstate.t -> diff signals (of anims)
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
      wait_for >>= fun _ -> Lwt_unix.sleep 1. (*7.*)

    let update gstate = 
      run_frames_on_first ();
      send_app_mode Gstate.(`Mode_game);
      (*Lwt_io.printl "\n\nupdate was called!"*)
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

    module G_s = struct
      
      open Gstate
      open Player

      let p0_name = 
          let name = E.map (fun {p0} -> p0.name) gstate
          in S.hold "" name

      let p1_name = 
          let name = E.map (fun {p1} -> p1.name) gstate
          in S.hold "" name

      let p0_mana = 
          let mana = E.map (fun {p0} -> p0.mana) gstate
          in S.hold 1. mana

      let p1_mana = 
          let mana = E.map (fun {p1} -> p1.mana) gstate
          in S.hold 1. mana

      let p0_pos = 
          let pos = E.map (fun {p0} -> p0.position) gstate
          in S.hold Left pos

      let p1_pos = 
          let pos = E.map (fun {p1} -> p1.position) gstate
          in S.hold Right pos

      let winner = 
          let winner = E.map (fun {winner} -> winner) gstate
          in S.hold None winner

      let turn = 
          let turn = E.map (fun {turn} -> turn) gstate
          in S.hold P0 turn

      let board = 
          let board = E.map (fun {board} -> board) gstate
          in S.hold (Board.make 2) board


      (*goto do turn, board, position*)

    end

    (**Animations*)

    open Anim.T
    open LTerm_style 

    let eq a a' = Anim.equal ~eq:(=) a a'

    (*goto possibly extend this function with a non-changing fold over sign messages
      > this way overlays can be rendered without becoming part of animation layers @ def time
      > still.. think of better solution?
    *)
    let lift_anim anim_def = S.fold ~eq 
        (fun anim_acc _ -> Anim.incr_anim anim_acc) 
        anim_def frames

    type extra = {
      pos : float;
      age : int;
    }

    let std_ex = { pos = 0.; age = 0 }

    type state = {
      s : string;
      i : int; (*indent from left*)
      c_fg : color;
      c_bg : color;
      ex : extra;
      (*  prev : 'a option;*)
    }

    let std_st = {
      s = ""; 
      i = 0;
      c_fg = default;
      c_bg = default;
      ex = std_ex;
    }

    module Color = struct 

      let inverse (r,g,b) = (255-r, 255-g, 255-b)

      let rgb (r,g,b) = LTerm_style.rgb r g b

      let get_rgb_exc = function 
        | LTerm_style.RGB (r, g ,b) -> (r, g, b) 
        | _ -> failwith "Visualizer: Sorry - you tried to get RGB values from a non-RGB value."

      let lerp (r,g,b) (r',g',b') r1 r2 v = 
        let f,i = float, int_of_float in
        (* let _ = assert (v >= r1 && v <= r2) in*)
        let v_pct = (v -. r1) /. (r2 -. r1) in 
        let r'' = (((f r') -. (f r)) *. v_pct) +. (f r)
        and g'' = (((f g') -. (f g)) *. v_pct) +. (f g)
        and b'' = (((f b') -. (f b)) *. v_pct) +. (f b)
        in (
          max 0 (min (i r'') 255)
        , max 0 (min (i g'') 255)
        , max 0 (min (i b'') 255)
        )
        
      let i = LTerm_style.index

    end

    let id x = x

    (*goto (in general) move helper modules somewhere else?*)

    let each fr f x = match S.value frames_s mod fr with 0 -> f x | _ -> id x

    module Ae = struct 

      (*goto this is not used right now - can also be defined in terms of 'each'*)
      let indent_incr inc each ({i} as st) = {
        st with i=(if S.value frames_s mod each = 0 then i + inc else i)
      } 

    end

    module Al = struct 
      let indent_head plus = function
        | [] -> [] 
        | (Ae ({i} as st, f))::tl -> (Ae ({st with i = i + plus}, f))::tl 
        | (Al _)::tl as al -> al
    end

    module Adef = struct 

      let make_curtain dir str ~len ~indent ~cols = 
        Al (List.init len (fun iter -> 
            Ae ({ std_st with 
                  s = str; 
                  c_fg = Color.i 8; 
                  i = indent
                }, None )) 
            |> Al.indent_head ( match dir with 
                | `Go_left -> cols - (len*(indent +1))
                | `Go_right -> 0 ), 
            Some (Al.indent_head (match dir with `Go_left -> -1 | _ -> 1)))

      let anim_of_str str ~ae_init_mapi ~ae_succ_mapi ~all_map = 
        Al ((String.enum str) |> Enum.mapi 
              (fun i c -> 
                 Ae ( { std_st with s = (String.of_char c) } |> ae_init_mapi i
                    , Some (ae_succ_mapi i)))
            |> List.of_enum
           , all_map)

    end

    (*goto place in own module and structure funcs. like Game_anim*)
    let loading_anim = 
      let s0, s1 = "Lambda", "Tactician"
      and cols = S.value columns 
      and space_between = 9 in
      let outer_space str = 
        ((float cols) /. 2.) -. ((float space_between) /. 2.) -.
        (float (String.length str))
        |> int_of_float 
      in
      let bg = 
        let c1 = (160, 158, 90)
        and c2 = (88, 227, 171) 
(*
        let c1 = (0, 0, 200)
        and c2 = (255, 255, 255)
*)
        and n_sines = 1.
        and speed = (Float.pi /. (float cols)) *. 3.
        in
        Adef.anim_of_str (String.make cols '-')
          ~ae_init_mapi:(fun i st -> { st with ex = { st.ex with
              pos = (float i) *. ((Float.pi *. n_sines) /. (float cols))
            }})
          ~ae_succ_mapi:(fun i st -> 
              { st with 
                ex = { st.ex with pos = st.ex.pos +. speed };
                c_fg = 
                  let r,g,b = Color.lerp c1 c2 0. 1. (sin st.ex.pos) in
                  LTerm_style.rgb r g b;
              })
          ~all_map:None
      and title = 
        let each_n = 20 
        and max_age = 5
        in Al ([ 
            Adef.anim_of_str s0
              ~ae_init_mapi:(fun i st -> match i with 
                  | 0 -> { st with i = outer_space s0; c_fg = Color.i 3 }
                  | _ -> { st with c_fg = Color.i 3 } )
              ~ae_succ_mapi:(fun i -> (each each_n (fun st -> match i with 
                  | 0 -> { st with 
                           i = if st.ex.age < max_age then 
                               st.i - ((String.length s0) + 1) 
                             else st.i;
                           ex = { st.ex with age = succ st.ex.age }}
                  | _ -> { st with 
                           i = if st.ex.age < max_age then succ st.i else st.i;
                           ex = { st.ex with age = succ st.ex.age }
                         } )))
              ~all_map:None;
            Adef.anim_of_str s1
              ~ae_init_mapi:(fun i st -> match i with 
                  | 0 -> { st with i = space_between; c_fg = Color.i 3 }
                  | _ -> { st with c_fg = Color.i 3 } )
              ~ae_succ_mapi:(fun i -> (each each_n (fun st -> match i with 
                  | 0 -> st
                  | _ -> { st with 
                           i = if st.ex.age < max_age then succ st.i else st.i;
                           ex = { st.ex with age = succ st.ex.age } 
                         } )))
              ~all_map:None
          ], None) 
      in lift_anim [
        bg; title; 
        Adef.make_curtain `Go_left "\\" ~len:10 ~indent:7 ~cols;
        Adef.make_curtain `Go_right "/" ~len:10 ~indent:8 ~cols
      ]

    module Game_anim = struct

      (** Old fixpoint alternative version of lifting animations:
      let eq_af (a, f) (a', f') = f = f' && Anim.equal ~eq:(=) a a'

      let define_fixp_anim af = 
        let af' = S.l2 (fun (a, f) f' -> 
            if f = f' then (a, f) else (Anim.incr_anim a, f') 
          ) af frames_s
            ~eq:eq_af
        in af', S.map ~eq fst af' 

      let lift_anim' anim = S.fix (anim, S.value frames_s) define_fixp_anim ~eq:eq_af
      *)
      
      (*howto/what-to:
        . should length be modifiable on the fly? (complicates things / will reset animations in manabar)
          > no I think it should only be invisi.-buffers who's length dep. on columns
        . generate mana-animation as Al([Ae's]) -> match on ppos inside Al-rule and apply reverse 
          . (Ae's should both be empty and full mana-fields)
          . Ae's rule control if should be empty/full field
      *)
      (*goto >
        . make aestetic  
        . make just deleted mana be colored differently + blinking (> dep. on st.ex.age + st.s?)
      *)
      let make_manabar ~len ~mana_s ~pos_s ?(c_empty='.') ?(c_full='|') () = 
        let len_mana = len-2 in
        Al ([
            Ae( { std_st with s = "[" }, None );
            ( Adef.anim_of_str (String.make (len-2) c_empty)
                ~ae_init_mapi:(fun _ st -> { st with c_fg = Color.i 3} )
                ~ae_succ_mapi:(fun i st -> 
                    let nbars = int_of_float 
                        ((S.value mana_s) *. (float len_mana)) 
                    in { st with 
                         s = String.of_char 
                             (if i <= nbars then c_full else c_empty) 
                       }
                  )
                ~all_map:None
              |> fun al -> match (S.value pos_s), al with
              (*goto put in all_map as before, so players dynamically can change place, 
                but do by check diff between delayed pos_s and curr pos_s (look for delay comb.) *)
              | Right, Al (al, r) -> Al (List.rev al, r)
              | _ -> al );
            Ae ({ std_st with s = "]" }, None );
          ], None)

      let p0_mana_a = lift_anim [ 
          make_manabar ~len:11 
            ~mana_s:G_s.p0_mana 
            ~pos_s:G_s.p0_pos () ]

      let p1_mana_a = lift_anim [ 
          make_manabar ~len:11 
            ~mana_s:G_s.p1_mana 
            ~pos_s:G_s.p1_pos () ]

      (*goto visualize: make dep. on G_s.turn (make bold? / color? / char anim?)*)
      let make_name ~name_s ~color = lift_anim [
          Ae( { std_st with c_fg = color }
            , Some( fun st -> 
              { st with s = S.value name_s } 
                )
            )
        ]

      let p0_name_a = make_name ~name_s:G_s.p0_name ~color:(Color.i 0)

      let p1_name_a = make_name ~name_s:G_s.p1_name ~color:(Color.i 4)

      (*howto1 make animation game-board 
          > gameboard: how to map blocks to enduring animations? 
            > id's of blocks saved in anim-state? (yes)
              > so need some function that takes 
                . board as input
                . maps correct anim-rules to moved-around blocks ((Al, block-list) -> Al)
                  . (block-types are saved in animation state? or find better sol.?)
                  > so it's an Al-rule?
      *)
      (*howto2> 
        . make an Al-rule that links old blocks with some id with new blocks position
          > this way anims of blocks can follow a block 
            > make some system where rules gets reset/overrided at certain state events
      *)
      (*goto make depend on player position like mana etc.*)
      (*goo*)
      let gameboard_a = lift_anim [
          let board = S.value G_s.board in
          (** should define the initial state + register animation functions inside state, to be applied each frame*)
          let show_new_elem_state = function
            (*goto save animation closures in list in state? -> then they can get reset and extended dynamically
              > then we need a new equals function for state that doesn't compare functions *)
            (*goto define player color in gamestate? >+ make sign. over it (used here and in pX_name_a ) 
              > depend on this here
            *)
            | { killed = true } -> { std_st with s = "###"; c_fg = Color.rgb (94, 229, 229) } 
            | _ -> assert false (*goo*) 
          in 
          Al ( 
            List.map (fun e -> 
                Ae (show_new_elem_state e, None)
              ) (Board.list board)
            , None
          )
        ]

      (*<goo*)
      (*goto define rest of animation parts (plus messages) *)

      (*goto make order of anims depend on position of players*)
      let full = 
        S.merge ~eq (@) [] [p0_mana_a; p0_name_a; p1_name_a; p1_mana_a] 
        |> S.map ~eq (fun l -> [ Al( l, None ) ] ) 

      (*[ (inv_buff_anim?); gameboard_a  ]*)

      (*goto let some 'layer-rule'-closure be mapped over the 'anim_layers' signal
            > this depends (with S.value or normal signal depend) on some signals with 
              state for creating messages e.g.?
          > else always have a message animation (which is empty when not used) 
      *)

    end


    (**Game-mode switching*)

    let visu_switcher = 
      let app_mode_s = (S.hold `Mode_loading app_mode) in
      S.map 
        (function
            `Mode_loading -> loading_anim
          | `Mode_game -> Game_anim.full
        ) app_mode_s
      |> S.switch ~eq         
    (*< apparantly we need this for not failing on equality-check, 
        even though it's given at lift-time*)


    (**Rendering*)

    (*>gomaybe Anim.eval should be done more efficiently with some kind of fold instead*)
    (*in reality this signal is the flattened representation of the animation layers*)
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

    let render_width = columns

    let draw_matrix = render_width >|~ (fun render_width -> 
        LTerm_geom.( 
          LTerm_draw.make_matrix {rows=1; cols=(render_width)} )
      ) 

    let draw_context = S.l2 (fun render_width matrix-> 
        let dim = LTerm_geom.({rows=1; cols=render_width}) in
        LTerm_draw.context matrix dim
      ) render_width draw_matrix

(* We don't need this ... 
    let snoc l e = e :: l (*reverse args of 'cons'*)

    let cropped_anim_layers = S.l2 (fun max_width layers -> 
        List.fold_right (fun layer acc -> 
            List.fold_left (fun (pos_acc, res_acc) ({s; i} as st) ->
                let curr_len = pos_acc + i + (Zed_utf8.length s) in
                if curr_len > max_width then 
                  (curr_len, res_acc)
                else 
                  (curr_len, st::res_acc)
              ) (0, []) layer
            |> snd 
            |> List.rev 
            |> snoc acc
          ) layers []
      ) render_width anim_layers 
*)

    let visualize = 
      S.l3 (fun layers context matrix -> 
          LTerm_draw.clear context;
          List.iter (fun layer -> 
              List.fold_left (fun pos_acc {s; i; c_fg; c_bg} ->
                  LTerm_text.([B_fg c_fg; B_bg c_bg; S s; E_bg; E_fg])
                  |> LTerm_text.eval 
                  |> LTerm_draw.draw_styled context 0 (pos_acc + i);
                  (pos_acc + i + (Zed_utf8.length s))
                ) 0 layer |> ignore; 
              ()
            ) layers;
          LTerm.render term matrix
        ) anim_layers draw_context draw_matrix


    let _ = S.keep visualize

  end


end



