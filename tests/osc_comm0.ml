
open Batteries 
open Core_rand00
open Lwt
open Osc_lwt.Udp


let test () = 
  let localhost = Unix.inet_addr_of_string "127.0.0.1" 
  and port = 57110 in
  let addr = Lwt_unix.ADDR_INET (localhost, port) in
  let n_id = 1001l
  in
  let start_synth = Osc.(Message {
      address = "/s_new"; (*use of '/' instead of '\' from sc-lang definitions*)
      arguments = [
        String "sinew"; (*synthdef name*)
        Int32 n_id; (*node id*)
        Int32 0l; (*?*)
        Int32 0l; (*?*)
        String "vol"; (*synthdef parameter*)
        Float32 1.1; (*parameter value*)
      ];
    })
  and change_freq = Osc.(Message {
      address = "/n_set";
      arguments = [
        Int32 n_id;
        String "freq";
        Int32 431l;
      ]
    })
  and stop_synth = Osc.(Message {
      address = "/n_free";
      arguments = [ Int32 n_id ]
    })
  in 
  Client.create ()
  >>= fun client ->

  Lwt_io.printl "Starting synth"
  >> Client.send client addr start_synth
  >> Lwt_unix.sleep 1.

  >> Lwt_io.printl "Changing frequency"
  >> Client.send client addr change_freq
  >> Lwt_unix.sleep 1.

  >> Lwt_io.printl "Stopping synth"
  >> Client.send client addr stop_synth
  >> Lwt_unix.sleep 1.    
  >> Client.destroy client


let _ = Lwt_main.run (test ())





