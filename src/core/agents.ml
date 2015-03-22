open Mas_system


(** Most reinforcement learning algorithms depend on a state
    abstraction. This strictly defines the transformation
    required for a particular state based agent antipicating
    states of type [`s] and emitting actions of type ['a]. 
  *)
module State_transform =
  struct
    type ('s, 'a, 'b) t = ('a, 'b) Observation.t -> 's [@@deriving show]
  end

(**
  A state base policy is a function which given states 
  of type ['s], it returns actions of type ['a] *)
module State_based_policy =
 struct
  type ('s, 'a) t = 's -> 'a [@@deriving show]
 end


(** Creates a state based agent with state and action
    types defined by the [Value_function] and 
    [Opp_action] parameters *)
module Make_state_based (Value_function : Value_function.S)
  (Opp_action:Action.S) =
  struct
    module State = Value_function.State
    module Action = Value_function.Action

    let init
      (policy: (State.t, Action.t) State_based_policy.t)
      (state_trans: (State.t, Action.t, Opp_action.t) State_transform.t)
      (value_fn: Value_function.t)
      (reward_fn: (Action.t, Opp_action.t) Reward_fn.t)
      ~(name:string) =
      let policy' obs =
        let s = state_trans obs in policy s in
      let reward_fn' obs = reward_fn obs in
      let value_fn' =
       let value ?action obs =
         let s = state_trans obs in (Value_function.value value_fn) ?action s in
       let count ?action obs =
         let s = state_trans obs in (Value_function.count value_fn) ?action s in
       let update t ~action obs r =
         let s = state_trans obs in 
         let _ = (Value_function.update value_fn) value_fn action s r in
         t in
      Mas_system.Value_fn.init ~count ~value ~update in
      Agent.init policy' reward_fn' value_fn' name
  end

(*
  module Make_Q_learner(Value_function : Value_function.S)
  (Opp_action : Action) =
    struct
      module State = Value_function.State
      module Action = Value_function.Action
      module Agent = Make_state_based(Value_function)(Opp_action)
      let default_gamma = 0.9
      let default_alpha = 0.2
      let init ?(alpha=default_alpha) ?(gamma=default_gamma)
        policy state_trans value_fn reward_fn ~name =
          let agent = Agent.init policy state_trans
            value_fn reward_fn ~name in agent

      let update2 a s r s' alpha gamma vf a' q_s'_a' =
        let q_s_a = Value_function.value vf ~action:a s in
        let q_s_a = q_s_a +. alpha *. (r +. gamma *. q_s'_a' -. q_s_a) in
        q_s_a
 
      let update a s r s' alpha gamma vf actions =
        let a', q_s'_a' = Value_function.best_action vf s' actions in
        update2 a s r s' alpha gamma vf a' q_s'_a'

(*        Value_function.set ~action:a s q_s_a vf  *)
    end
*)
