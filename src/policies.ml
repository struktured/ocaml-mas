open Mas_core
 
open Prob_cache_containers

module Random = CCRandom

let sample ?rand weights =
  let thres = Random.run (CCOpt.get_lazy (fun () -> Random.float 1.0) rand) in
  let sum = CCArray.fold (+.) 0. weights in
  let normalized = CCArray.map (fun w -> w /. sum) weights in
  let prob, index = CCArray.fold_while (fun (acc, i) w ->
    let acc = acc +. w in
    if (i+1) < Array.length normalized && acc < thres
      then (acc, i+1), `Continue
      else (acc, i), `Stop) (0., 0) normalized
  in index

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

module GreedyPolicy(State : Agents.STATE) (Action : Action) =
struct
  let default_eps = 0.90

  module Value_fn = Value_functions.Make_discrete(State)(Action)

  let init ?(eps=default_eps) value_fn (action_provider : State.t -> Action.t array) :
    (State.t, Action.t) Agents.State_based_policy.t =
   let rand_float = Random.float 1.0 in
   fun (state:State.t) ->
     let actions = action_provider state in
     if actions = CCArray.empty then failwith("no actions possible for state: " ^ State.show state) else
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
