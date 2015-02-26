module type Action = Mas_core.Action

module Q_learner =
struct
  let default_gamma = 0.9
  let default_alpha = 0.2

  let update ~a ~s ~r ~s' ?(alpha=default_alpha) ?(gamma=default_gamma) ~a' ~q_s'_a' ~q_s_a =
    let q_s_a = q_s_a +. alpha *. (r +. gamma *. q_s'_a' -. q_s_a) in
    q_s_a

  module Make(Value_function : Value_function.S) =
  struct
    let update ~a ~s ~r ~s' ?alpha ?gamma ~value_fn ~actions =
      let a', q_s'_a' = Value_function.best_action value_fn s' actions in
      let q_s_a = Value_function.value value_fn ~action:a s in
      update ~a ~s ~r ~s' ?alpha ?gamma ~a' ~q_s'_a' ~q_s_a
  end
end

(** TODO NOT CORRECT ! *)
module Sarsa_learner =
struct
  let default_gamma = 0.9
  let default_alpha = 0.2

  let update ~a ~s ~r ~s' ?(alpha=default_alpha) ?(gamma=default_gamma) ~a' ~q_s'_a' ~q_s_a =
    let q_s_a = q_s_a +. alpha *. (r +. gamma *. q_s'_a' -. q_s_a) in
    q_s_a

  module Make(Value_function : Value_function.S) =
  struct
    let update ~a ~s ~r ~s' ?alpha ?gamma ~value_fn ~actions =
      let a', q_s'_a' = Value_function.best_action value_fn s' actions in
      let q_s_a = Value_function.value value_fn ~action:a s in
      update ~a ~s ~r ~s' ?alpha ?gamma ~a' ~q_s'_a' ~q_s_a
  end
end
