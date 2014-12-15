

(** The type for all reward values, currently just float *)
module Reward = CCFloat

(** Defines actions possible within an environment.
    The only action omnipresent is the [`Reward] action *)
module type Action =
sig
  (** The action type, an open polymorphic variant with [`Reward] as the only known type. 
      Users need extend this type to support different actions in their own environments *)
  type 'a t = [> `Reward of Reward.t] as 'a [@@deriving show, ord]
end

module type S = sig
  module Action : Action

  (** Defines an agent and operations to the run the agent within the environment *)
  module rec Agent :
  sig
    module Action : Action

    (** The type of an agent. *)
    type t

    val policy : t -> 'a Policy.t
    (** The agent's policy function *)

    val init : unit -> t 
    (** Initializes an agent *)

  end

  and 
    (** A policy is how an agent decides to act in an environment. **)
    Policy : 
  sig
    (** A policy function - given an agent and observation for the agent, get back an action from the agent *)
    type 'a t = Agent.t -> 'a Observation.t -> 'a Action.t
  end

  and 
    (** A single observation observed by a particular agent *)
    Observation : 
  sig
    (** An observation is modeled as an [action] and the [agent] that caused it at [epoch] **)
    type 'a t = {agent: Agent.t; action: 'a Action.t; epoch: int} [@@deriving show, ord]
  end

  module type Environment =
  sig
    type params
    type t

    val init : Agent.t list -> t
    val run :  params -> t -> t 

    val agents : t -> Agent.t list

    val reward : Agent.t -> t -> Reward.t
  end
end     

