(**
   The simplest of reinforcement learning policies. The best
   action is chosen by the agent [epsilon] percent of the time,
   otherwise a random action is chosen (including the best action).
*)
open Mas_core
open Mas_system

open Prob_cache_containers

module Random = Sampling.Random
module Make(Value_function : Value_function.S) =
struct
  let default_eps = 0.90
  module State = Value_function.State
  module Action = Value_function.Action

  let init ?(eps=default_eps) value_fn
        (action_provider : State.t -> Action.t array) :
    (State.t, Action.t) Agents.State_based_policy.t =
    let rand_float = Random.float 1.0 in
    fun (state:State.t) ->
      let actions = action_provider state in
      if actions = CCArray.empty then failwith("no actions possible for state: " ^ State.show state) else
        let exploit = Random.run rand_float <= eps in
        if exploit then 
          let best_act = Value_function.best_action value_fn state actions in
          let (a,(_:float)) = best_act in
          a
        else
          let gen = CCArray.random_choose actions in
          Random.run gen
end

