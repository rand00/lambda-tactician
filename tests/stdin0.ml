
open Batteries
open Core_rand00

let print_frevert s = 
  let s' = String.filter (function 
      | '\n'|'\r' -> false | _ -> true ) s in
  print_string s';
  print_string (s'^"\r")

let test1 () = 
  let str_again = "oqwkdpqkwdo kqwdokqwkd poqkwdo pqkwd okqwdop kqwodp kqwokpd\r" 
  in
  print_string "1";print_string "1";print_string "1";
  print_string str_again; flush stdout;
  IO.read_line stdin |> print_frevert;
  print_string str_again; flush stdout;
  IO.read_line stdin |> print_frevert

let test2 () = 
  print_string "oooooooooooooo"; flush stdout;
  IO.read_line stdin |> print_frevert;
  print_string "oooooooooooooo"; flush stdout;
  print_string "nnnnnnnnnnnnnnn"; flush stdout

let test3 () =
  print_string "foo";
  read_line () |> ignore;
  print_string "bar"

let test4 () =
  print_string "foo"; flush stdout;
  IO.read stdin |> ignore;
  print_string "bar" 

let test5 () =
  print_string "foo"; flush stdout;
  IO.read_line stdin |> print_string;
  Sys.command "tput cuu1" |> ignore;
  print_string "bar" 
  

let _ = test5 ()
