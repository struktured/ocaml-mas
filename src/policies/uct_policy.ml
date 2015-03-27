open Mas_core
open Mas_system
open Mas_value_functions
open Prob_cache_containers

module Random = Sampling_ext.Random

module Make(Value_function: Value_function.S) =
struct
  module State = Value_function.State
  module Action = Value_function.Action

  let init ?(c=2.0) value_fn (action_provider : State.t -> Action.t array) :
    (State.t, Action.t) State_based_policy.t =
    fun (state:State.t) ->
      let n = Value_function.count value_fn state in
      let actions = action_provider state in
      if actions = CCArray.empty then failwith("no actions possible for state: " ^ State.show state) else
        let expectations = CCArray.map (fun action -> (action, Value_function.value value_fn ~action state)) actions in
        let (a, exp) = CCArray.fold (fun ((best_action, best_exp) as best) ((cur_action, cur_exp) as cur) ->
          let n_a = Value_function.count value_fn ~action:cur_action state in
          let biased = if n_a = 0 then cur_exp +. c else
              cur_exp +. c *. sqrt (log (CCFloat.of_int n) /. (CCFloat.of_int n_a)) in
          if biased >= best_exp then cur else best)
          (CCArray.get actions 0, Reward.min_value)
          expectations in a
end

