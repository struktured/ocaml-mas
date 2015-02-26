open Mas_core

module type STATE = sig type t [@@deriving show, ord] end

module type S = 
sig
  module State : STATE
  module Action : Action

  type t
  val init :
    count : (?action:Action.t -> State.t -> int) ->
    value : (?action:Action.t -> State.t -> Reward.t) ->
    update : (t -> Action.t -> State.t -> Reward.t -> unit) ->
    name : string -> t

  val value : t -> ?action:Action.t -> State.t -> Reward.t
  val count : t -> ?action:Action.t -> State.t -> int
  val update : t -> Action.t -> State.t -> Reward.t -> unit
  val name : t -> string
  val best_action : t -> State.t -> Action.t array -> Action.t * Reward.t 
end

module Make (State:STATE) (Action : Action) :
  S with module Action = Action and module State = State =
struct
  module Action = Action
  module State = State

  type t = {
    count : (?action:Action.t -> State.t -> int);
    value : (?action:Action.t -> State.t -> Reward.t);
    update : (t -> Action.t -> State.t -> Reward.t ->  unit);
    name : string
  } [@@deriving show]

  let init ~count ~value ~update ~name = {count;value;update;name}
  let value t = t.value
  let count t = t.count
  let update t = t.update t
  let name t = t.name

  let best_action t state actions =
    let expectations = CCArray.map (fun action -> action, (value t) ~action state) actions in
    let folder best_opt ((cur_action, cur_exp) as cur) =
      match best_opt with
      | None -> Some cur
      | Some (best_act, best_exp) ->
        if (cur_exp >= best_exp) then Some cur else best_opt 
    in
    CCOpt.get_exn (CCArray.fold folder None expectations)
end


