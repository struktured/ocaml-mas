open Mas_system

module type S =
sig
  module State : State.S
  module Action : Action.S
  type 'a t = 'a -> Action.t -> State.t -> Reward.t -> 'a [@@deriving show]
end


module Make (State:State.S) (Action:Action.S)
  : S with module State = State and module Action = Action =
struct
  module State = State
  module Action = Action
  type 'a t = 'a -> Action.t -> State.t -> Reward.t -> 'a [@@deriving show]
end

