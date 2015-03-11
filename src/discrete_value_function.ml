open Mas_core

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

(** Creates a discrete state value function- it maps discrete state and actions with
    reward estimates by caching them explicitly. These are also typically classified
    as tabular methods in the reinforcement learning literature. *)
module Make(State:State.S) (Action:Action.S) :
  S with module State = State and module Action = Action =
struct
  include Value_function.Make(State)(Action)
  module StateAction = struct type t =
    State of State.t | Action of Action.t [@@deriving show, ord] end
  module Cache = Prob_cache_containers.Set_model.Make(StateAction)

  type update_rule = StateAction.t Prob_cache_common.Update_rules.Update_fn.t

  let events_of ?action s = Cache.Events.of_list (
    match action with
    | Some a -> [StateAction.State s; StateAction.Action a]
    | None -> [StateAction.State s])

  let state_of events : State.t option = let event_list = Cache.Events.to_list events in
    let rec f_rec = function [] -> None | (StateAction.State s)::l -> Some s | (_::l) -> f_rec l in
    f_rec event_list

  let state_of_exn events = CCOpt.get_exn (state_of events)

  let action_of events : Action.t option = let event_list = Cache.Events.to_list events in
    let rec f_rec = function [] -> None | (StateAction.Action a)::l -> Some a | (_::l) -> f_rec l in
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
    let update_rule = CCOpt.map (fun l -> fun ?(orig=0.0) ~obs ~exp ~cnt -> 
      let t', r' = try l (CCOpt.get_exn !vf, orig) (action_of_exn obs) (state_of_exn obs) exp 
(* ---> *) with Invalid_argument e -> print_endline e (* TODO FIX ME *); CCOpt.get_exn !vf, orig in r') learning_rule in
    let cache = ref (Cache.create ?prior_count ?prior_exp ?update_rule ~name) in
    let value ?action s =
      let events = events_of ?action s in Cache.exp events !cache
    in init
      ~value
      ~count:(fun ?action s -> _count !cache ?action s)
      ~update:(fun t action s r -> vf := Some t; cache := _update !cache action s r;t)
      ~name

end
