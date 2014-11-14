
open Batteries 
open Core_rand00
open Lwt
open Osc_lwt.Udp


let test () = 
  let localhost = Unix.inet_addr_of_string "127.0.0.1" 
  and port = 57110 in
  let addr = Lwt_unix.ADDR_INET (localhost, port) in
  let packet = Osc.(Message {
      address = "/s_new"; (*goto change to some useful sig*)
      arguments = [
        String "sinew"; (*synthdef name*)
        Int32 1001l; (*node id*)
        Int32 0l; (*?*)
        Int32 0l; (*?*)
        String "vol"; (*synthdef parameter*)
        Float32 1.1; (*parameter value*)
      ];
    })
  in
  Client.create () >>= fun client ->
  Client.send client addr packet
  >> Client.destroy client
  

let _ = test ()





