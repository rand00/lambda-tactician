(*open Rectypes0*) (* <- can't open a module that refers to self*)

type somewrap = 
  | Wrappy 

type t = 
  | Foo of somewrap
  | Bar

