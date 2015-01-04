open Mas_core

module type STATE = sig type t [@@deriving show, ord] end

module State_based_value_fn =
struct
  type ('s, 'a) t = ?action:'a -> 's -> Reward.t [@deriving show]
end


open Prob_cache_common

(**
  The state based value function signature
 *)
module type S = 
sig
  module State : STATE 

  module Action : Action

  type t
  val init : 
    count : (?action:Action.t -> State.t -> int) -> 
    value : (?action:Action.t -> State.t -> Reward.t) -> 
    update : (Action.t -> State.t -> Reward.t -> unit) ->
    name : string -> t

  val value : t -> ?action:Action.t -> State.t -> Reward.t
  val count : t -> ?action:Action.t -> State.t -> int
  val update : t -> Action.t -> State.t -> Reward.t -> unit
  val name : t -> string

end


module Make (State:STATE) (Action : Action) : 
  S with module Action = Action and module State = State = 
struct
  module Action = Action
  module State = State

  type t = {
    count : (?action:Action.t -> State.t -> int); 
    value : (?action:Action.t -> State.t -> Reward.t);
    update : (Action.t -> State.t -> Reward.t -> unit);
    name : string} [@@deriving show]

  let init ~count ~value ~update ~name = {count;value;update;name}
  let value t = t.value
  let count t = t.count
  let update t = t.update
  let name t = t.name
end


(** Creates a discrete state value function- it maps discrete state and actions with
    reward estimates by caching them explicitly *)
module Make_discrete (State:STATE) (Action:Action) =
struct
  module Value_function = Make(State)(Action)
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
  
  let init ?prior_count ?prior_reward ?update_rule ~name = 
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
      ~update:(fun action s r -> cache := update !cache action s r)
      ~name
end

