
type element_value = {
  lambda : float;
  symbol : float;
  empty : float;
}

type action_value = {
  kill_lambda : float;
  kill_symbol : float;
  move_element : float; (*for harvesting more by having more on the field*)
  application : float;
}

type t = {
  elements : element_value;
  actions : action_value;
}
