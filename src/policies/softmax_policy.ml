(**
  Softmax policy. Prefer actions of higher value by exponentially
  weighting them and sampling from the resulting the boltzman distribution.
 *)
open Oml
open Mas_core
open Mas_system
open Mas_value_functions
open Prob_cache_containers

module Random = Sampling_ext.Random
module Make(Value_function : Value_function.S) =
struct
  let default_temp = 1.0
  module State = Value_function.State
  module Action = Value_function.Action

  let init ?(temp=default_temp) value_fn
        (action_provider : State.t -> Action.t array) :
    (State.t, Action.t) State_based_policy.t =
    fun (state:State.t) ->
      let actions = action_provider state in
      if actions = CCArray.empty then failwith("no actions possible for state: " ^ State.show state) else
      let weights = CCArray.map (fun action -> Value_function.value value_fn ~action state) actions in
      Sampling.Poly.softmax ~temp actions weights ()
end

