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


  (** The dealer environment has one agent serving as a dealer
   * and all other agents are the brokers. The environment begins
   * with the dealer issuing some initial state to all brokers.
   * The dealer then sends an acto*)
  module DealerEnvironment(Action:Action) = struct
    module Action = Action
    type params = {trials:int} [@@deriving show]
    type 'a, 'b state = {init_params:params; dealer: 'a Agent.t brokers:'b Agent.t list; obs:'a Observation.t}
    type 'a t = 'a state Gen.t  
    let init params agents = 
      let open Gen.Infix in     
      Gen.init ~limit:params.trials (fun epoch -> 
        {trials=params.trials;params;agents;epoch})

    let agents t = t.agents

    let reward a t = 0.0
  end
end

(* ^^Exchange model^^
 * trader agent -> exchange agent: BUY | SELL 
 * exchange agent -> trader agent: NEW | CANCEL | FILL 
 * 
 * Example sequence w/traders T1, T2, and exchange E
 * T1 -> E : BUY 
 * E -> T1 : NEW
 *   NEW -> T1 : (Not actionable)
 * E -> T2 : NEW
 *   NEW -> T2 : (Not actionable)
 * T1 -> (does nothing)
 * T2 ->
 * *)
