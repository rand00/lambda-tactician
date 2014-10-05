
type symbol = X | Y | Z

type player_id = P0 | P1

type element = 
  | Lambda of symbol * symbol
  | Symbol of symbol
  | Empty

type element_wrap = {
  owner : player_id;
  element : element;
  mana_cost : float;
  killed : bool;
}

type game_board = element_wrap array
type temp_board = (element_wrap list) array

type location = 
  | Local
  | Remote of string * int

type mana = float

type player = {
  id : player_id;
  name : string;
  location : location;
  position : [ `Left | `Right ];
  next_move : (game_board -> mana -> element);
  mana : mana;
}

type game_state = {
  p0 : player;
  p1 : player;
  turn : player_id;
  board : game_board;
  winner : player_id option;
}

type game_action = 
  | Kill of element
  | Keep of element
  | Application of element * element
