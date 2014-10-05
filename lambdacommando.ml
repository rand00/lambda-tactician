open Batteries
open Core_rand00 
open Gametypes

let _ = 

  let gstate = {
    winner = None;
    board = Array.make 16 Empty;

    p0 = {
      id = P0; name = "Hansi";
      location = Local;
      position = `Left;
      next_move = Ai.Random.next_move;
      mana = 1.;
    };

    p1 = {
      id = P1; name = "Finka";
      location = Local;
      position = `Right;
      next_move = Ai.Random.next_move;
      mana = 1.;
    };
  } in 
  
  in 
  Control.gloop gstate
    ~next_fun:Board.next
    ~visualizer:Visualizer.termprinter0 
    
    




