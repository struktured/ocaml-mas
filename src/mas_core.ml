module Reward = Mas_intf.Reward

module Action = struct type 'a t = [>] as 'a [@@deriving show, ord] end

module Action_with_init = struct type 'a t = [> `Init] as 'a [@@deriving show, ord] end

  (** Defines an agent and operations to the run the agent within the environment *)
  module rec Agent : 
    sig
      type ('a, 'b) t = {policy: ('a, 'b) Policy.t;reward : ('a,'b) Reward_fn.t} 
        constraint 'a = 'a Action.t constraint 'b = 'b Action.t [@@deriving show] 
      val policy : ('a, 'b) t -> ('a, 'b) Policy.t 
      val reward : ('a, 'b) t -> ('a, 'b) Reward_fn.t
      val init : ('a, 'b) Policy.t -> ('a, 'b) Reward_fn.t -> ('a, 'b) t 
    end =
  struct
    type ('a, 'b) t = {policy: ('a, 'b) Policy.t; reward: ('a, 'b) Reward_fn.t} 
     constraint 'a = 'a Action.t constraint 'b = 'b Action.t [@@deriving show]
    let init (policy:('a, 'b) Policy.t) (reward:('a, 'b) Reward_fn.t) = {reward;policy}
    let policy (t:('a, 'b) t) = t.policy
    let reward (t:('a, 'b) t) = t.reward
 end

  and 
    (** A policy is how an agent decides to act in an environment. *)
    Policy : sig type ('a, 'b) t =
      ('a, 'b) Observation.t -> 'a Action.t constraint 'a = 'a Action.t constraint 'b = 'b Action.t [@@deriving show] end =
  struct
    (** A policy function - given an observation, get back an action *)
   type ('a,'b) t = ('a,'b) Observation.t -> 'a Action.t [@@deriving show]
  end

  and 
    Reward_fn : sig type ('a, 'b) t = ('a,' b) Observation.t -> Reward.t constraint 'a = 'a Action.t constraint 'b = 'b Action.t [@@deriving show] end =
  struct
    type ('a, 'b) t = ('a, 'b) Observation.t -> Reward.t [@@deriving show]
  end

  and 
    Observation : sig type ('a,'b) t constraint 'a = 'a Action.t constraint 'b = 'b Action.t end =  
  struct
    (** An observation is modeled as an [action] and the [agent] that caused it at [epoch] *)
    type ('a, 'b) t = {agent: ('b, 'a) Agent.t; action: 'b Action.t; epoch: int} [@@deriving show]
  end

(*
  (** A simple two agent environment *)
  module Environment = struct
    type params = {trials:int} [@@deriving show]
    type ('a,'b) obs = From_agent of ('a,'b) Observation.t  | From_opp of ('b,'a) Observation.t
    type ('a,'b) state = {params:params; opp: ('b,'a) Agent.t; agent:('a,'b) Agent.t; obs:('a,'b) obs}
    type ('a,'b) t = ('a,'b)state Gen.t  
    let init ~(params:params) ~(opp:('b,'a) Agent.t) ~(agent:('a,'b) Agent.t) =
      let open Gen.Infix in 
      Gen.init ~limit:params.trials (fun epoch -> let open Observation in 
        if epoch = 0 then {params;agent;opp;obs=From_agent {agent=opp;action=`Init;epoch}} else
        let acting_agent = match (epoch % 2) with | 0 -> opp | 1 -> agent in
        let policy = Agent.policy acting_agent in
        let action = policy obs in
        let obs = {agent=acting_agent;epoch;action} in
        {params; agent; opp; obs})

     let agent t = t.agent
     let opp t = t.opp
     let turn t = match (t.obs.epoch % 2) with 0 -> t.agent | 1 -> t.opp
     let reward t = let agent = turn t in 
       Agent.reward agent t.obs
  end

  *)
