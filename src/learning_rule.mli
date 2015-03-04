open Mas_core


module type S =
sig
  module State : State.S
  module Action : Action
  type 'a t = 'a -> Action.t -> State.t -> Reward.t -> 'a [@@deriving show]
end

module Make : functor (State:State.S) (Action:Action)
  -> S with module State = State and module Action = Action
