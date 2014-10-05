
type symbol = X | Y | Z

type player_id = P0 | P1

type element = 
  | Lambda of player_id * symbol list * symbol list
  | Symbol of player_id * symbol
  | Empty

type game_board = element array

type player = {
  id : player_id;
  name : string;
  ip : string;
  port : int;

  position : [ `Left | `Right ];
  next_move : (player_id -> game_board -> element);
  mana : float;
}

type game_state = {
  player0 : player;
  player1 : player;
  board : game_board;
}
