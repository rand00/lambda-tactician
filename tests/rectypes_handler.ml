open Rectypes0 

let testy = function
  | Something rec0t -> Some rec0t
  | Opened | Every | Where -> None

let _ = 
  match testy (Something (Rectypes1.Foo Rectypes1.Wrappy)) with
  | Some _ -> print_endline "Teletubbies"
  | None -> print_endline "Mormortubbies"
