module Reward = Mas_intf.Reward

module type Action = Mas_intf.Action

module type S = Mas_intf.S


module Make(Action:Action) (*: S with module Action = Action  *) = 
struct

  (** Defines an agent and operations to the run the agent within the environment *)
  module rec Agent : 
    sig 
      type 'a t = private {policy: 'a Policy.t} [@@deriving show] 
      val policy : 'a t -> 'a Policy.t val init : 'a Policy.t -> 'a t 
    end =
  struct
    type 'a t = {policy: 'a Policy.t} [@@deriving show]

    let policy t = t.policy

    let init policy = {policy}
  end

  and 
    (** A policy is how an agent decides to act in an environment. **)
    Policy : sig type 'a t = 'a Agent.t -> 'a Observation.t -> 'a Action.t [@@deriving show] end =
  struct
    (** A policy function - given an agent and observation for the agent, get back an action from the agent *)
    type 'a t = 'a Agent.t -> 'a Observation.t -> 'a Action.t [@@deriving show]
  end

  and 
    (** A single observation observed by a particular agent *)
    Observation : sig type 'a t = private {agent: 'a Agent.t; action: 'a Action.t; epoch: int} [@@deriving show] end = 
  struct
    (** An observation is modeled as an [action] and the [agent] that caused it at [epoch] **)
    type 'a t = {agent: 'a Agent.t; action: 'a Action.t; epoch: int} [@@deriving show]
  end

  module Environment = 
  struct
    type params = {trials:int} [@@deriving show]
    type 'a state = {params:params; agents:'a Agent.t list; obs:'a Observation.t}
    type 'a t = 'a state Gen.t   
    (** TODO start action / " for initial state? 
    let init params agents = {params;agents} 

    let run params t = ()

    let agents t = t.agents

    let reward a t = 0.0
  end
end
