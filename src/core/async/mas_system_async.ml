open Core.Std
open Async.Std
open Mas_core

module Make(Deferred:sig type 'a t end) =
struct
module Reward = Mas_intf.Reward
(** Defines an agent and operations to run the agent within the environment *)
module rec Agent :
sig
  (** An agent which emits actions of type ['a] given observed
      actions of type ['b] from other agents
  * /
  *)
  type ('a, 'b) t = private {
    policy: ('a, 'b) Policy.t;
    reward_function : 'a Reward_function.t;
    value_function : ('a, 'b) Value_function.t;
    name:string} [@@deriving show]

  val policy : ('a, 'b) t -> ('a, 'b) Policy.t
  (** The policy this agent uses to make actions given observations *)

  val reward_function : ('a, 'b) t -> 'a Reward_function.t
  (** The reward function associated with the given agent.
      Determines a reward signal given an observation *)

  val value_function : ('a, 'b) t -> ('a, 'b) Value_function.t
  (** The value function associated with the given agent.
      Determines an estimated reward signal given an observation *)

  val init : ('a, 'b) Policy.t -> 'a Reward_function.t ->
    ('a, 'b) Value_function.t -> name:string -> ('a, 'b) t
  (** Initializes an agent given a policy, reward function, value function, and [name] *)

  val name : ('a, 'b) t -> string
  (** Gets the [name] of the agent *)

end =
struct
  type ('a, 'b) t = {
    policy: ('a, 'b) Policy.t;
    reward_function : 'a Reward_function.t;
    value_function : ('a, 'b) Value_function.t;
    name:string} [@@deriving show]

  let init (policy:('a, 'b) Policy.t) (reward_function:'a Reward_function.t) value_function ~name =
    {policy;value_function;reward_function;name}
  let policy (t:('a, 'b) t) = t.policy
  let reward_function (t:('a, 'b) t) = t.reward_function
  let value_function t = t.value_function
  let name t = t.name
end

and
  (** A policy is how an agent decides to act in an environment. *)
  Policy : sig type ('a, 'b) t = 'a Observation.t -> 'b Action.t Deferred.t [@@deriving show] end =
struct
  (** A policy function - given an observation, get back an action *)
  type ('a, 'b) t = 'a Observation.t -> 'b Action.t Deferred.t [@@deriving show]
end

and
  (** A reward function transforms observations into reward signals for a particular agent. While the reward function
    is assigned to the agent, its inner workings are still opaque to it beyond being a feedback mechanism.
   *)
  Reward_function : sig type 'a t = 'a Observation.t -> Reward.t Deferred.t [@@deriving show] end =
struct
  (** The type of reward functions. Given an observation, determine a reward signal *)
  type 'a t = 'a Observation.t -> Reward.t Deferred.t [@@deriving show]
end

and
  (** The value function module. Defines an estimator of expected reward
      given an observation and (optionally) an action. Value functions
      are typically evolving, as environments will update the value function
      with each newly observed reward signal by the agent *)
  Value_function :
    sig
      (** The type of a value function, composing of a [value], [count],
          and [update] function. *)
      type ('a, 'b) t = private {
        value : ?action:'b Action.t -> 'a Observation.t -> Reward.t Deferred.t;
        count : ?action:'b Action.t -> 'a Observation.t -> int Deferred.t;
        update : ('a, 'b) t -> action:'b Action.t -> 'a Observation.t -> Reward.t -> ('a, 'b) t Deferred.t
      } [@@deriving show]
  
      val value : ('a, 'b) t -> ?action: 'b Action.t -> 'a Observation.t
        -> Reward.t Deferred.t
      (** Gets the value estimator for this value function instance.
          The returned function estimates the reward signal given
          the observation and an optional action. *)

      val count : ('a, 'b) t -> ?action: 'b Action.t -> 'a Observation.t -> int Deferred.t
      (** Gets the visitor count function for this value function instance.
          The returned function counts the number of the times the
          given observation and an optional [action] have been
          visited or attempted by the agent *)


      val update : ('a, 'b) t -> action : 'b Action.t -> 'a Observation.t
        -> Reward.t -> ('a, 'b) t Deferred.t
      (** Updates a value function instance. Given an action,
          new observation, and reward signal, update the underlying
          model of the value function *)

      val init : 
        value:(?action:'b Action.t -> 'a Observation.t -> Reward.t Deferred.t) -> 
        count:(?action:'b Action.t -> 'a Observation.t -> int Deferred.t) ->
        update:(('a, 'b) t -> action:'b Action.t -> 'a Observation.t -> Reward.t -> ('a, 'b) t Deferred.t) ->
        ('a, 'b) t
      (** Initializes a new value function given a [value] estimator,
          visitor [count] function, and an value [update] function *)
    end =
  struct
  type ('a, 'b) t = {
    value : ?action:'b Action.t -> 'a Observation.t -> Reward.t Deferred.t;
    count : ?action:'b Action.t -> 'a Observation.t -> int Deferred.t;
    update : ('a, 'b) t -> action:'b Action.t -> 'a Observation.t -> Reward.t -> ('a, 'b) t Deferred.t
  } [@@deriving show]
  
  let init ~value ~count ~update =
    {value;count;update}

  let value t = t.value
  let count t = t.count
  let update t ~action s r = t.update t ~action s r
  end
and 
  (** Observations are what other agents are given as input to react to (by applying their policies). 
    In this framework, observations are basically actions by other agents. Notably, this is where
    the library is multi-agent oriented. 
   
    Instead of an agent vs. the environment model, it is agent vs. agent. In the degenerate case,
    we simply have the traditional environment be an opponent agent whose actions are the states
    generated by the environment. *)
  Observation : sig type 'a t = 'a [@@deriving show] end =  
struct
  (** An observation is abstract but must at least 
      be printable according the [show] signtures *)
  type 'a t = 'a [@@deriving show]
end
and
  (** Actions are taken by agents, but also observed by other agents *)
  Action : sig type 'b t = 'b [@@deriving show] end =  
struct
  type 'b t = 'b [@@deriving show]
end
end
