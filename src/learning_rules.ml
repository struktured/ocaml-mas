open Mas_core

(** This module is for any learner which uses one step 
   to compute updates to the value function *)
module One_step_learner = struct
  let default_gamma = 0.9
  let default_alpha = 0.2

  let update ~a ~s ~r ~s' ?(alpha=default_alpha) ?(gamma=default_gamma)
    ~a' ~q_s'_a' ~q_s_a =
    q_s_a +. alpha *. (r +. gamma *. q_s'_a' -. q_s_a)
end

module Q_learner =
struct
  include One_step_learner
  module Make(Value_function : Value_function.S) =
  struct
    module State = Value_function.State
    module Action = Value_function.Action

    let update ~a  ~a' ~q_s'_a' ~s ~r ~s' ?alpha ?gamma ~value_fn =
      (* Q learning estimates the value function with the best action *)
      let q_s_a = Value_function.value value_fn ~action:a s in
      update ~a ~s ~r ~s' ?alpha ?gamma ~a' ~q_s'_a' ~q_s_a

    let update_best_action ~a ~s ~r ~s' ?alpha ?gamma ~value_fn ~actions =
      let a', q_s'_a' = Value_function.best_action value_fn s' actions in
      update ~a  ~a' ~q_s'_a' ~s ~r ~s' ?alpha ?gamma ~value_fn
    
    module RingBuffer = CCRingBuffer.Make
                          (struct type t = State.t * Action.t * Reward.t end)

    module Learning_rule = Value_function.Learning_rule

    let init ?alpha ?gamma action_fn : 
      (Value_function.t * Reward.t) Learning_rule.t = 
      let ring_buffer = RingBuffer.create ~bounded:true 2 in
      let update_rule ((value_fn:Value_function.t),
                       (orig:Reward.t)) (a:Action.t) (s:State.t) (r:Reward.t) =
        RingBuffer.push_back ring_buffer (s, a, r); 
        if RingBuffer.length ring_buffer <= 1 then (value_fn, orig) else
          let s', a', r' = RingBuffer.peek_front ring_buffer in
          let r'' = update_best_action
                      ~a ~s ~r ~s' ?alpha ?gamma ~value_fn ~actions:(action_fn s) in
          (value_fn, r'') in update_rule
  end
end

module Sarsa_learner =
struct
  include One_step_learner

  module Make_internal(Value_function : Value_function.S) =
  struct
    let update ~a ~a' ~s ~r ~s' ?alpha ?gamma ?q_s'_a' ~value_fn ~actions =
      let q_s'_a' = CCOpt.get_lazy (fun () -> Value_function.value value_fn ~action:a' s') q_s'_a' in
      let q_s_a = Value_function.value value_fn ~action:a s in
      update ~a ~s ~r ~s' ?alpha ?gamma ~a' ~q_s'_a' ~q_s_a
  end
end

