open Mas_core
 
open Prob_cache_containers

module Random = CCRandom

let sample ?rand weights =
  let thres = Random.run (CCOpt.get_lazy (fun () -> Random.float 1.0) rand) in
  let sum = CCArray.fold (+.) 0. weights in
  let normalized = CCArray.map (fun w -> w /. sum) weights in
  let (_:float), index = CCArray.fold_while (fun (acc, i) w ->
    let acc = acc +. w in
    if (i+1) < Array.length normalized && acc < thres
      then (acc, i+1), `Continue
      else (acc, i), `Stop) (0., 0) normalized
  in index


(**
  A policy that randomizes its behavior given a set of weights for each action.
  By default, backed by a uniform distribution.
 *)
module RandomPolicy =
  struct
    let init ?(weights:('s -> 'a array -> float array) option) (action_provider : 's -> 'a array) :
      ('s, 'a) Agents.State_based_policy.t =
      let rand = Random.float 1.0 in
      fun s ->
        let open CCArray in
        let actions = action_provider s in
        let num_actions = CCArray.length actions in
        if num_actions = 0 then failwith("no actions possible for state") else
        let weights = CCOpt.get_lazy (fun () -> fun (_:'s) (actions :'a array) -> actions >>|
          fun (_:'a) -> 1.0 /. CCFloat.of_int num_actions) weights in
        let index = sample ~rand (weights s actions) in
        actions.(index)
  end

(**
  The simplest of reinforcement learning policies. The best
  action is chosen by the agent [epsilon] percent of the time,
  otherwise a random action is chosen (including the best action).
 *)
module GreedyPolicy(State : Agents.STATE) (Action : Action) =
struct
  let default_eps = 0.90

  module Value_fn = Value_functions.Make(State)(Action)

  let init ?(eps=default_eps) value_fn (action_provider : State.t -> Action.t array) :
    (State.t, Action.t) Agents.State_based_policy.t =
   let rand_float = Random.float 1.0 in
   fun (state:State.t) ->
     let actions = action_provider state in
     if actions = CCArray.empty then failwith("no actions possible for state: " ^ State.show state) else
     let exploit = Random.run rand_float <= eps in
     if exploit then 
       let opt = Value_fn.best_action value_fn state actions in
       let (a,(_:float)) = CCOpt.get_exn opt in
       a
     else
       let gen = CCArray.random_choose actions in
       Random.run gen
end

module UCTPolicy(State: Agents.STATE) (Action : Action) =
struct

  module Value_fn = Value_functions.Make(State)(Action)

  let init ?(c=2.0) value_fn (action_provider : State.t -> Action.t array) :
    (State.t, Action.t) Agents.State_based_policy.t =
    fun (state:State.t) ->
      let n = Value_fn.count value_fn state in
      let actions = action_provider state in
      if actions = CCArray.empty then failwith("no actions possible for state: " ^ State.show state) else
        let expectations = CCArray.map (fun action -> (action, Value_fn.value value_fn ~action state)) actions in
        let (a, exp) = CCArray.fold (fun ((best_action, best_exp) as best) ((cur_action, cur_exp) as cur) ->
          let n_a = Value_fn.count value_fn ~action:cur_action state in
          let biased = if n_a = 0 then cur_exp +. c else
            cur_exp +. c *. sqrt (log (CCFloat.of_int n) /. (CCFloat.of_int n_a)) in
          if biased >= best_exp then cur else best)
          (CCArray.get actions 0, Reward.min_value)
          expectations in a
end
