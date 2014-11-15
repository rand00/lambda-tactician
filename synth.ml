
open Batteries 
open Core_rand00
open Lwt


module Server = struct 
  open Osc_lwt.Udp

  let run () =
    Lwt_io.printl "Starting scsynth server - see server-log in ./log/scsynth.log"
    >> Lwt_unix.system "./run_scsynth.sh 2>&1 > ./log/scsynth.log"
    >>= (fun _ -> Lwt_io.printl "Scsynth exited.")

end

module Client = struct
  open Osc_lwt.Udp

  let make () =
    let localhost = Unix.inet_addr_of_string "127.0.0.1" 
    and port = 57110 in
    let addr = Lwt_unix.ADDR_INET (localhost, port)
    in Client.create(), addr

end


open Osc_lwt.Udp (*Client and Server modules rebound *)

let quit_all (client, addr) =
  Client.send client addr Osc.(Message {
      address = "/quit"; arguments = [] })
  >> Client.destroy client

(** Internal functions - hide by mli*)

let curr_node_id = ref 1000l
let next_node_id () =
  let open Int32 in
  let i = !curr_node_id in
  let _ = curr_node_id := succ i
  in i


(** Make functor for convenience*)
module Make
    (C : sig
       val client :
         (Osc_lwt.Udp.Client.t, Lwt_unix.sockaddr)
     end) = struct

  let client, addr = C.client
  
  (** Types of synths args*)

  type synth_args =
    | Dur of float
    | Freq of float
    | Mul of float

  let map_arg = function
    | Dur f  -> [ "dur"; Float f ]
    | Freq f -> [ "freq"; Int32 (Int32.of_float f) ]
    | Mul f -> [ "mul"; Float f ]

  let map_args args =
    List.concat (List.map map_arg args)

  let get_dur = function
    | Dur f -> f | _ -> assert false

  let wait_duration ?(dur = 0.5) args =
    Lwt_unix.sleep
      (try
         (List.find (function | Dur _ -> true | _ -> false) args)
         |> get_dur
       with NotFound -> dur)

  let make_osc_msgs synth args =
    let node_id = next_node_id () in
    let start = Osc.(Message {
        address = "/s_new"; 
        arguments = [
          String synth; 
          Int32 node_id; 
          Int32 0l; 
          Int32 0l; 
        ] @ (map_args args);
      }) in
    let stop = Osc.(Message {
        address = "/n_free";
        arguments = [ Int32 node_id ]
      })
    in start, stop

  let make_synth synth args =
    let start, stop = make_osc_msgs synth args
    in Client.send client addr start
    >> wait_duration args
    >> Client.send client addr stop


  (** Simple synth functions - for fx action/conseq sounds*)

  let sinew0 args = make_synth "sinew" args

end








