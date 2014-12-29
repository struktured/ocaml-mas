open Mas_core
 
open Prob_cache_containers

module Random = CCRandom
module GreedyPolicy(State: Action) (Action: Action) = 
struct
  open Observation
  let default_eps = 0.90

  module Event = struct type t = State of State.t | Action of Action.t [@@deriving show, ord] end
  open Event
  module Model = Set_model.Make(Event)

  let init ?(eps=default_eps) model (action_provider : State.t -> Action.t list) : (Action.t, State.t) Policy.t =
   let rand_float = CCRandom.float 1.0 in
   let rand_int = CCRandom.int Pervasives.max_int in
   fun obs -> 
     let actions = action_provider obs.action in
     let expectations = List.map (fun a -> (a, Model.exp (Model.Events.of_list [State obs.action; Action a]) model)) actions in
     let exploit = Random.run rand_float <= eps in
     if exploit then 
      let (a, exp) = List.fold_right (fun ((best_action, best_exp) as best) ((cur_action, cur_exp) as cur) -> 
         if (cur_exp >= best_exp) then cur else best) 
         expectations 
         (List.hd actions, Reward.min_value)
     in a 
     else 
       let index = (Random.run rand_int) mod (List.length actions) in
       List.nth actions index
end
