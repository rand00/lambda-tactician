
type t

val make : int -> t

val enum : t -> Gametypes.element_wrap BatEnum.t

val eval_action : Gametypes.element_action -> t -> t

val eval_move_to_effect : Gametypes.board_action -> t 
  -> (Gametypes.board_action_conseqs list) * t

val remove_killed : t -> t


