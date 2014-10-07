
type t

val enum : t -> Gametypes.element_wrap BatEnum.t

val eval : Gametypes.action -> t -> t

