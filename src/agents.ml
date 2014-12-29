open Mas_core

module type STATE = 
sig
  type t [@@deriving show, ord] 
end

module State_transform =
  struct
    type ('s, 'a, 'b) t = ('a, 'b) Observation.t -> 's
  end

module State_based_value_fn =
  struct
    type ('s, 'a) t = ?action:'a -> 's -> Reward.t
  end

module State_based_policy = 
 struct 
  type ('s, 'a) t = 's -> 'a [@@deriving show] 
 end

module State_based_reward_fn = 
 struct 
  type 's t = 's -> Reward.t [@@deriving show] 
 end

module Make_state_based (State:STATE) (Action:Action) (Opp_action:Action) =
  struct
    let init 
      ~(policy: (State.t, Action.t) State_based_policy.t) 
      ~(state_trans: (State.t, Action.t, Opp_action.t) State_transform.t) 
      ~(value_fn: (State.t, Action.t) State_based_value_fn.t) 
      ~(reward_fn: State.t State_based_reward_fn.t)
      ~(name:string) =
      let policy' obs = 
        let state = state_trans obs in
        policy state in
      let reward_fn' obs = 
        let state = state_trans obs in 
        reward_fn state in
      Agent.init policy' reward_fn' name
  end
