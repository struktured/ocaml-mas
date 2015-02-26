open Mas_core

module type STATE = sig type t [@@deriving show, ord] end

open Prob_cache_common
(** Creates a discrete state value function- it maps discrete state and actions with
    reward estimates by caching them explicitly *)
module Make(State:STATE) (Action:Action) =
struct
  module Value_function = Value_function.Make(State)(Action)
  type t = Value_function.t
  module Action = Action
  module State = State
  module StateAction = struct type t = State of State.t | Action of Action.t [@@deriving show, ord] end
  open StateAction
  module Cache = Prob_cache_containers.Set_model.Make(StateAction)

  let events_of ?action s = Cache.Events.of_list (
    match action with
    | Some a -> [State s; Action a]
    | None -> [State s])

  let state_of events : State.t option = let event_list = Cache.Events.to_list events in
    let rec f_rec = function [] -> None | (State s)::l -> Some s | (_::l) -> f_rec l in
    f_rec event_list

  let action_of events : Action.t option = let event_list = Cache.Events.to_list events in
    let rec f_rec = function [] -> None | (Action a)::l -> Some a | (_::l) -> f_rec l in
    f_rec event_list

  let with_s_a (events:Cache.Events.t) (f_s_a: ?action:Action.t -> State.t -> 'a) (default:'a) =
    match state_of events with
    | None -> default
    | Some s ->
      let action = action_of events in f_s_a ?action s

  let update cache action s r =
    let s_a = events_of ~action s in
    Cache.observe ~exp:r s_a cache

  let count cache ?action s =
    let s_a = events_of ?action s in
    Cache.count s_a cache

  let init ?prior_count ?prior_reward ?update_rule name =
    let prior_count = CCOpt.map (fun pc -> (fun events -> 
      with_s_a events pc 0)) prior_count in
    let prior_exp = CCOpt.map (fun pr -> (fun events ->
      with_s_a events pr 0.0)) prior_reward in
    let cache = ref (Cache.create ?prior_count ?prior_exp ?update_rule ~name) in
    let value ?action s =
      let events = events_of ?action s in Cache.exp events !cache
    in Value_function.init
      ~value
      ~count:(fun ?action s -> count !cache ?action s)
      ~update:(fun t action s r -> cache := update !cache action s r)
      ~name

  let name = Value_function.name
  let value = Value_function.value
end


module Make_Q_Learner (State:STATE) (Action:Action) =
  struct
    module Value_fn = Value_function.Make(State)(Action)
    module RingBuffer = CCRingBuffer.Make
      (struct type t = State.t * Action.t * Reward.t end)

    module Q_Learner = Learning_rules.Q_learner.Make(Value_fn) 

    let init ?alpha ?gamma name action_fn =
      let ring_buffer = RingBuffer.create ~bounded:true 2 in
      let update_rule (value_fn:Value_fn.t) (a:Action.t) (s:State.t) (r:Reward.t) =
        let s', a', r' = RingBuffer.peek_front ring_buffer in
        let r'' = Q_Learner.update ~a ~s ~r ~s' ?alpha ?gamma ~value_fn ~actions:(action_fn s) in
        RingBuffer.push_back ring_buffer (s, a, r); r'' in
      update_rule
end
