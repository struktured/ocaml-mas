open Mas_core
open Mas_system

open Prob_cache_containers

module Random = Sampling.Random

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
      let index = Sampling.from_weights ~rand (weights s actions) in
      actions.(index)
