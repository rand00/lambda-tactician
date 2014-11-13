
open Batteries 
open Core_rand00
open Lwt
open Osc_lwt.Udp


let test () = 
  let localhost = Unix.inet_addr_of_string "127.0.0.1" 
  and port = 57110 in
  let addr = Lwt_unix.ADDR_INET (localhost, port) in
  let packet = Osc.(Message {
      address = "/s_newargs"; (*goto change to some useful sig*)
      arguments = [];
    })
  in
  Client.create () >>= fun client ->
  Client.send client addr packet
  >> Client.destroy client
  

let _ = test ()





