
open Batteries 
open Core_rand00
open Lwt


module Server = struct 
  open Osc_lwt.Udp

  let run () =
    Lwt_main.run
      (Lwt.async (fun () ->
           (if not (Sys.file_exists "log") then
              Lwt_unix.mkdir "log" 0o755
            else return ())
           >> Lwt_unix.system "./run_scsynth.sh 2>&1 > ./log/scsynth.log");
       return ())

end

module Client = struct
  open Osc_lwt.Udp

  let make () =
    let localhost = Unix.inet_addr_of_string "127.0.0.1" 
    and port = 57110 in
    let addr = Lwt_unix.ADDR_INET (localhost, port)
    in (Lwt_main.run (Client.create())), addr

end


open Osc_lwt.Udp (*Client and Server modules rebound *)

let quit_all (client, addr) =
  Lwt_main.run (
    Client.send client addr Osc.(Message {
        address = "/quit"; arguments = [] })
    >> Client.destroy client
  )

(** Internal functions - hide by mli*)

let curr_node_id = ref 1000l
let next_node_id () =
  let open Int32 in
  let i = !curr_node_id in
  let _ = curr_node_id := succ i
  in i


module type CSig = sig
  val client : Osc_lwt.Udp.Client.t * Lwt_unix.sockaddr
end

module type S = sig

  type synth_args = [
    | `Dur of float
    | `Freq of float
    | `Mul of float
  ]

  val make_synth : ?autofree:bool -> string -> synth_args list -> unit

  val sinew0 : synth_args list -> unit

  val ghostwind : synth_args list -> unit

end

(** Make functor for convenience*)
module Make (C : CSig) = struct

  open Osc
  
  let client, addr = C.client
  
  (** Types of synths args*)

  type synth_args = [
      `Dur of float
    | `Freq of float
    | `Mul of float
  ]

  let map_arg = function
    | `Dur f  -> [ String "dur"; Float32 f ]
    | `Freq f -> [ String "freq"; Int32 (Int32.of_float f) ]
    | `Mul f -> [ String "mul"; Float32 f ]

  let map_args args =
    List.concat (List.map map_arg args)

  let get_dur = function
    | `Dur f -> f | _ -> assert false

  let wait_duration ?(dur = 0.5) args =
    Lwt_unix.sleep
      (try
         (List.find (function | `Dur _ -> true | _ -> false) args)
         |> get_dur
       with Not_found -> dur)

  (*goto : rename this and pass synth_node_id separately 
    so other osc_msg_handlers can be used with this *)
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

  (*goto: rename this to 'simple_synth'? or just make 'make_synth_handler' next *)
  let make_synth ?(autofree=false) synth args =
    Lwt_main.run
      (Lwt.async 
         (fun () ->
            let start, stop = make_osc_msgs synth args
            in Client.send client addr start
            (*goto: shouldn't it be possible to set duration even if no autofree?*)
            >> (if autofree then 
                  wait_duration args >> Client.send client addr stop
                else return ()));
       return ())


  (** Simple synth functions - for fx action/conseq sounds*)

  let sinew0 args = make_synth ~autofree:true "sinew" args

  let ghostwind args = make_synth "atmos_ghostwind" args



  
end








