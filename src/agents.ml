open Mas_core

module type STATE =
sig
  type t [@@deriving show, ord]
end

module State_transform =
  struct
    type ('s, 'a, 'b) t = ('a, 'b) Observation.t -> 's [@@deriving show]
  end

module State_based_policy =
 struct
  type ('s, 'a) t = 's -> 'a [@@deriving show]
 end

module Make_state_based (Value_function : Value_functions.S) (Opp_action:Action) =
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
    (* let s = state_trans obs in reward_fn (* s *) obs in *)
      let value_fn' =
       let value ?action obs =
         let s = state_trans obs in (Value_function.value value_fn) ?action s in
       let count ?action obs  =
         let s = state_trans obs in (Value_function.count value_fn) ?action s in
       let update ~action obs r =
         let s = state_trans obs in (Value_function.update value_fn) action s r in
      Value_fn.init ~count ~value ~update in
      Agent.init policy' reward_fn' value_fn' name
  end
