module Reward = Mas_intf.Reward

module type Action = Mas_intf.Action

module type S = Mas_intf.S


module type State = sig type t [@@deriving show] end

module Make(Action:Action) (*: S with module Action = Action  *) = 
struct

  (** Defines an agent and operations to the run the agent within the environment *)
  module rec Agent : 
    sig 
      type 'a t = private {policy: 'a Policy.t} [@@deriving show] 
      val policy : 'a t -> 'a Policy.t 
      val init : 'a Policy.t -> 'a t 
    end =
  struct
    type 'a t = {policy: 'a Policy.t} [@@deriving show]

    let policy t = t.policy

    let init policy = {policy}
  end

  and 
    (** A policy is how an agent decides to act in an environment. *)
    Policy : sig type 'a t = 'a Observation.t -> 'a Action.t [@@deriving show] end =
  struct
    (** A policy function - given an observation, get back an action *)
    type 'a t = a Observation.t -> 'a Action.t * Reward.t [@@deriving show]
  end

  and 
    Observation : sig type 'a t end =  
  struct
    (** An observation is modeled as an [action] and the [agent] that caused it at [epoch] *)
    type 'a t = {agent: 'a Agent.t; action: 'a Action.t; reward:Reward.t; epoch: int} [@@deriving show]
  end


  (** A simple two agent environment *)
  module Environment(Action:Action) = struct
    module Action = Action
    type params = {trials:int} [@@deriving show]
    type 'a state = {params:params; opp: 'a Agent.t; agent:'a Agent.t; obs:'a Observation.t}
    type 'a t = 'a state Gen.t  
    let init ~(params:params) ~(opp:'a Agent.t) ~(agent:'a Agent.t) = 
      let open Gen.Infix in     
      Gen.init ~limit:params.trials (fun epoch ->
        if epoch = 0 then {trials=params.trials;params;agent;opp;obs={agent=opp;action=`Init;epoch=0;reward=0.}} else
        let acting_agent = match (epoch % 2) with | 0 -> opp | 1 -> agent in
        let policy = acting_agent policy in
        let action, reward = policy obs in
        let obs = {agent=acting_agent;epoch;action;reward} in
        {params; agent; opp; obs})
  end
end
