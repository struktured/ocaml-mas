open Mas_core


module type S = 
sig
  module State : State.S
  module Action : Action

  module Learning_rule :
    Learning_rule.S with module State = State and module Action = Action

  type t [@@deriving show]
  
  val value : t -> ?action:Action.t -> State.t -> Reward.t
  val count : t -> ?action:Action.t -> State.t -> int
  val update : t -> t Learning_rule.t 
  val name : t -> string
  val best_action : t -> State.t -> Action.t array -> Action.t * Reward.t 
end

module type S_with_init =
sig
  include S

  val init :
    count : (?action:Action.t -> State.t -> int) ->
    value : (?action:Action.t -> State.t -> Reward.t) ->
    update : t Learning_rule.t ->
    name : string -> t
end

module Make (State:State.S) (Action : Action) :
  S_with_init with module State = State and module Action = Action =
struct
  module State = State
  module Action = Action
  module Learning_rule = Learning_rule.Make(State)(Action)

  type t = {
    count : (?action:Action.t -> State.t -> int);
    value : (?action:Action.t -> State.t -> Reward.t);
    update : t Learning_rule.t;
    name : string
  } [@@deriving show]

  let init ~count ~value ~update ~name = {count;value;update;name}
  let value t = t.value
  let count t = t.count
  let update t = t.update
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
