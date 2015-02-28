module type Action = Mas_core.Action


module One_step_learner = struct
  let default_gamma = 0.9
  let default_alpha = 0.2

  let update ~a ~s ~r ~s' ?(alpha=default_alpha) ?(gamma=default_gamma) ~a' ~q_s'_a' ~q_s_a =
    q_s_a +. alpha *. (r +. gamma *. q_s'_a' -. q_s_a)

end


module Q_learner =
struct
  include One_step_learner
  module Make(Value_function : Value_function.S) =
  struct
     
    let update ~a  ~a' ~q_s'_a' ~s ~r ~s' ?alpha ?gamma ~value_fn =
      (* Q learning estimates the value function with the best action *)
      let q_s_a = Value_function.value value_fn ~action:a s in
      update ~a ~s ~r ~s' ?alpha ?gamma ~a' ~q_s'_a' ~q_s_a

    let update_best_action ~a ~s ~r ~s' ?alpha ?gamma ~value_fn ~actions =
      let a', q_s'_a' = Value_function.best_action value_fn s' actions in
      update ~a  ~a' ~q_s'_a' ~s ~r ~s' ?alpha ?gamma ~value_fn 
  end
end

module Sarsa_learner =
struct
  include One_step_learner

  module Make(Value_function : Value_function.S) =
  struct
    let update ~a ~a' ~s ~r ~s' ?alpha ?gamma ?q_s'_a' ~value_fn ~actions =
      let q_s'_a' = CCOpt.get_lazy (fun () -> Value_function.value value_fn ~action:a' s') q_s'_a' in
      let q_s_a = Value_function.value value_fn ~action:a s in
      update ~a ~s ~r ~s' ?alpha ?gamma ~a' ~q_s'_a' ~q_s_a
  end
end
