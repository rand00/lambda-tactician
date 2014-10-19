open Batteries
open Core_rand00

type foo = {
  name : string;
  energy : float;
}

let stdfoo = { name = "none"; energy = 0.0 }

let arr = [| 
  {stdfoo with name = "foo0"};
  {stdfoo with name = "foo1"};
  {stdfoo with name = "foo2"};
  {stdfoo with name = "foo3"};
|]

let arr' = 
  Array.fold_lefti (fun arr' i foo -> 
      arr'.(i) <- {foo with energy = (Float.of_int i)};
      arr')
    Array.(make (length arr) stdfoo) 
    arr

let _ =
  Array.iter (fun foo -> print_endline (String.of_float foo.energy))
    arr'
