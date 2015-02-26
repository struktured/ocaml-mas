open Mas_core

module type STATE = sig type t [@@deriving show, ord] end

(**
  The state based value function signature.
 *)
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
  S with module Action = Action and module State = State
