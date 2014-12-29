open Mas_core

module type STATE = 
sig
  type t [@@deriving show, ord] 
end

module State_transform =
  struct
    type ('s, 'a, 'b) t = ('a, 'b) Observation.t -> 's
  end

module Value_fn =
  struct
    type ('s,'a) t = ?action:'a -> 's -> Reward.t
  end

module Make_agent(State:STATE) =
  struct
    let init 
      (policy: 's -> 'a) 
      (state_trans: ('s, 'a ,'b) State_transform.t) 
      (value_fn: ('s, 'a) Value_fn.t) 
      (reward_fn: 's -> Reward.t) 
      (name:string) =
      let policy' obs = 
        let state = state_trans obs in
        policy state in
      let reward_fn' obs = 
        let state = state_trans obs in 
        reward_fn state in
      Agent.init policy' reward_fn' name
  end
