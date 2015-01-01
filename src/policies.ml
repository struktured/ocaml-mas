open Mas_core
 
open Prob_cache_containers

module Random = CCRandom
module GreedyPolicy(State : Agents.STATE) (Action : Action) = 
struct
  let default_eps = 0.90

  module Value_fn = Value_functions.Make_discrete(State)(Action)

  let init ?(eps=default_eps) value_fn (action_provider : State.t -> Action.t array) : (State.t, Action.t) Agents.State_based_policy.t =
   let rand_float = CCRandom.float 1.0 in
   let rand_int = CCRandom.int Pervasives.max_int in
   fun (state:State.t) -> 
     let actions = action_provider state in
     let expectations = CCArray.map (fun action -> (action, Value_fn.value value_fn ~action state)) actions in
     let exploit = Random.run rand_float <= eps in
     if exploit then 
      let (a, exp) = CCArray.fold (fun ((best_action, best_exp) as best) ((cur_action, cur_exp) as cur) -> 
         if (cur_exp >= best_exp) then cur else best) 
         (CCArray.get actions 0, Reward.min_value)
         expectations
      in a 
     else 
       let gen = CCArray.random_choose actions in
       Random.run gen
end
