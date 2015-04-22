open Mas_core
open Mas_system

open Prob_cache_containers

open Oml

let init ?seed ?(weights:float array option) (action_provider : 's -> 'a array) :
  ('s, 'a) State_based_policy.t =
  fun s ->
    let open CCArray in
    let actions = action_provider s in
    let num_actions = CCArray.length actions in
    if num_actions = 0 then failwith("no actions possible for state") else
      let weights = CCOpt.get_lazy (fun () ->
                                     Array.create num_actions (1.0 /.
                                                               CCFloat.of_int
                                                                 num_actions)) weights in
      Sampling.Poly.multinomial ?seed actions weights ()
