open Core.Std
open Async.Std
open Mas_async_system

module type S =
sig
  module State : State.S
  module Action : Action.S
  type 'a t = 'a -> Action.t -> State.t -> Reward.t -> 'a Deferred.t [@@deriving show]
end

module Make : functor (State:State.S) (Action:Action.S)
  -> S with module State = State and module Action = Action
