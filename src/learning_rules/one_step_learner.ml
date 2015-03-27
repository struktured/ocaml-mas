(** This module is for any learner which uses one step 
    to compute updates to the value function *)
let default_gamma = 0.9
let default_alpha = 0.2

let update ~a ~s ~r ~s' ?(alpha=default_alpha) ?(gamma=default_gamma)
      ~a' ~q_s'_a' ~q_s_a =
  q_s_a +. alpha *. (r +. gamma *. q_s'_a' -. q_s_a)


