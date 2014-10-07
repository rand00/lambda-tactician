
type t

val enum : t -> Gametypes.element_wrap option BatEnum.t

val eval : Gametypes.element_action -> t -> t

val eval_to_consequence : Gametypes.board_action -> t 
  -> (Gametypes.board_action_result) * t

val remove_killed : t -> t

