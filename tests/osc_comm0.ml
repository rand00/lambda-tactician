
open Batteries 
open Core_rand00
open Lwt
open Osc_lwt.Udp


let start_SCsynth () =
  Lwt_io.printl "Starting scsynth server - see server-log in ./log/scsynth.log"
  >> Lwt_unix.system "./run_scsynth.sh 2>&1 > ./log/scsynth.log"
  >>= (fun _ -> Lwt_io.printl "Scsynth exited.")

let test_client () = 
  let localhost = Unix.inet_addr_of_string "127.0.0.1" 
  and port = 57110 in
  let addr = Lwt_unix.ADDR_INET (localhost, port) in
  let n_id = 1001l
  in
  let start_synthdef = Osc.(Message {
      (*use of '/' instead of '\' from sc-lang definitions
        see http://doc.sccode.org/Guides/OSC_commands.html *)
      address = "/s_new"; 
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
  and stop_synthdef = Osc.(Message {
      address = "/n_free";
      arguments = [ Int32 n_id ]
    })

  and stop_server = Osc.(Message {
      address = "/quit";
      arguments = []
    })
  in 
  Client.create ()
  >>= fun client ->

  (*when calling some synth from sclang (and running server from script), 
    there is a need to free the resources (with C-s in emacs) before the 
    server takes requests correctly - this might be neccesary here too.*)

  Lwt_io.printl "Starting synth"
  >> Client.send client addr start_synthdef
  >> Lwt_unix.sleep 1.

  >> Lwt_io.printl "Changing frequency"
  >> Client.send client addr change_freq
  >> Lwt_unix.sleep 1.

  >> Lwt_io.printl "Stopping synth"
  >> Client.send client addr stop_synthdef
  >> Lwt_unix.sleep 1.    

  >> Lwt_io.printl "Stopping server"
  >> Client.send client addr stop_server
  >> Client.destroy client


let _ = Lwt_main.run (
    Lwt.async start_SCsynth;
    Lwt_unix.sleep 5.
    >> test_client () )







