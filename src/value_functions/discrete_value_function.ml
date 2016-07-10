open Mas_system

open Prob_cache_common

(** Defines a discrete state value function- it maps discrete state and actions with
    reward estimates by caching them explicitly. These are also typically classified
    as tabular methods in the reinforcement learning literature. *)
module type S =
  sig
    include Value_function.S
    module StateAction : sig type t = private
      State of State.t | Action of Action.t [@@deriving show, ord] end

    type unary_learning_rule = (t * Reward.t) Learning_rule.t

    val init : ?prior_count:(?action:Action.t -> State.t -> int) ->
      ?prior_reward:(?action:Action.t -> State.t -> Reward.t) ->
      ?learning_rule:unary_learning_rule -> string -> t
  end


module StateAction = struct
  module type S =
    sig
      module State : State.S
      module Action : Action.S
      type t = State of State.t | Action of Action.t [@@deriving show, ord]
    end
  module Make(State:State.S)(Action:Action.S) : 
      S with module State = State and module Action = Action = 
    struct
      module State = State
      module Action = Action
      type t = State of State.t | Action of Action.t [@@deriving show, ord]
    end
end

module State_action_update_fn =
struct
  module type S = sig
    module Obs : StateAction.S
    include Prob_cache_common.Update_rules.S with module Obs := Obs
  end
end

module Make_with_cache (State_action_update_fn : State_action_update_fn.S) (Cache:
  Prob_cache_containers.Model_intf.S with module Event := State_action_update_fn.Obs) :
  S with
    module State = State_action_update_fn.Obs.State and
    module Action = State_action_update_fn.Obs.Action =
struct
  module Obs = State_action_update_fn.Obs
  include Value_function.Make(Obs.State)(Obs.Action)
  module Update_rules = Prob_cache_common.Update_rules
  module Update_rule = Update_rules.Rule_wrap(State_action_update_fn)
  module Mean_update_rule = Update_rules.Mean(Update_rule.Obs)
  type update_rule = Update_rules.UPDATE_FN(Obs).t

  let events_of ?action s = Cache.Events.of_list (
    match action with
    | Some a -> [Obs.State s; Obs.Action a]
    | None -> [Obs.State s])

  let state_of events : State.t option = let event_list = Cache.Events.to_list events in
    let rec f_rec = function [] -> None | (Obs.State s)::l -> Some s | (_::l) -> f_rec l in
    f_rec event_list

  let state_of_exn events = CCOpt.get_exn (state_of events)

  let action_of events : Action.t option = let event_list = Cache.Events.to_list events in
    let rec f_rec = function [] -> None | (Obs.Action a)::l -> Some a | (_::l) -> f_rec l in
    f_rec event_list

  let action_of_exn events = CCOpt.get_exn (action_of events)

  let with_s_a (events:Cache.Events.t) (f_s_a: ?action:Action.t -> State.t -> 'a) (default:'a) =
    match state_of events with
    | None -> default
    | Some s ->
      let action = action_of events in f_s_a ?action s

  let _update cache action s r =
    let s_a = events_of ~action s in
    Cache.observe ~exp:r s_a cache

  let _count cache ?action s =
    let s_a = events_of ?action s in
    Cache.count s_a cache

  type unary_learning_rule = (t * Reward.t) Learning_rule.t

  let init ?prior_count ?prior_reward ?(learning_rule : unary_learning_rule option) name =
    let prior_count = CCOpt.map (fun pc -> (fun events ->
      with_s_a events pc 0)) prior_count in
    let prior_exp = CCOpt.map (fun pr -> (fun events ->
      with_s_a events pr 0.0)) prior_reward in
    let vf = ref None in
    let update_rule : Cache.update_rule option = CCOpt.map (fun rule -> fun ?orig ~obs ~exp ~cnt ->
      (* Only specialize the update rule when it's a state action pair
         that is being updated *)
      if action_of obs = None || state_of obs = None
        then (*Mean_update_rule.apply ?orig ~obs ~exp ~size:cnt *) failwith("mean_update_rule.apply")
      else
          let t', r' = rule (CCOpt.get_exn !vf, CCOpt.get 0.0 orig)
            (action_of_exn obs) (state_of_exn obs) exp in r') learning_rule in
    let cache = ref (Cache.create ?prior_count ?prior_exp ?update_rule ~name) in
    let value ?action s =
      let events = events_of ?action s in Cache.exp events !cache
    in init
      ~value
      ~count:(fun ?action s -> _count !cache ?action s)
      ~update:(fun t action s r -> vf := Some t; cache := _update !cache action s r;t)
      ~name

end

(** Creates a discrete state value function- it maps discrete state and actions with
    reward estimates by caching them explicitly. These are also typically classified
    as tabular methods in the reinforcement learning literature. *)
module Make(State_action_update_fn:State_action_update_fn.S) = Make_with_cache 
 (State_action_update_fn)(Prob_cache_containers.Set_model.Make(StateAction))


