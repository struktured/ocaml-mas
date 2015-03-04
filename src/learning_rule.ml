open Mas_core

module type STATE = sig type t [@@deriving show, ord] end

module type S =
sig
  module State : STATE
  module Action : Action
  type 'a t = 'a -> Action.t -> State.t -> Reward.t -> 'a [@@deriving show]
end

module Make (State:STATE) (Action:Action)
  : S with module State = State and module Action = Action =
struct
  module State = State
  module Action = Action
  type 'a t = 'a -> Action.t -> State.t -> Reward.t -> 'a [@@deriving show]
end
