module Reward = Mas_intf.Reward

module type Action = sig type t [@@deriving show, ord] end

(** Defines an agent and operations to run the agent within the environment *)
module rec Agent : 
sig
  type ('a, 'b) t = {policy: ('a, 'b) Policy.t;reward : ('a,'b) Reward_fn.t;name:string} [@@deriving show]
  val policy : ('a, 'b) t -> ('a, 'b) Policy.t 
  val reward : ('a, 'b) t -> ('a, 'b) Reward_fn.t
  val init : ('a, 'b) Policy.t -> ('a, 'b) Reward_fn.t -> string -> ('a, 'b) t 
  val name : ('a, 'b) t -> string
end =
struct
  type ('a, 'b) t = {policy: ('a, 'b) Policy.t; reward: ('a, 'b) Reward_fn.t; name:string} [@@deriving show]
  let init (policy:('a, 'b) Policy.t) (reward:('a, 'b) Reward_fn.t) name = {reward;policy;name}
  let policy (t:('a, 'b) t) = t.policy
  let reward (t:('a, 'b) t) = t.reward
  let name t = t.name
end

and 
  (** A policy is how an agent decides to act in an environment. *)
  Policy : sig type ('a, 'b) t = ('a, 'b) Observation.t -> 'a [@@deriving show] end =
struct
  (** A policy function - given an observation, get back an action *)
  type ('a,'b) t = ('a,'b) Observation.t -> 'a [@@deriving show]
end

and
  (** A reward function transforms observations into reward signals for a particular agent. While the reward function
    is assigned to the agent, its inner workings are still opaque to it beyond being a feedback mechanism.
   *)
  Reward_fn : sig type ('a, 'b) t = ('a, 'b) Observation.t -> Reward.t  [@@deriving show] end =
struct
  (** The type of reward functions. Given an observation, determine a reward signal *)
  type ('a, 'b) t = ('a, 'b) Observation.t -> Reward.t [@@deriving show]
end
and 
  (** Observations are what other agents are given as input to react to (by applying their policies). 
    In this framework, observations are basically actions by other agents. Notably, this is where
    the library is multi-agent oriented. 
   
    Instead of an agent vs. the environment model, it is agent vs. agent. In the degenerate case,
    we simply have the traditional environment be an opponent agent whose actions are the states
    generated by the environment. *)
  Observation : sig type ('a, 'b) t = {agent: ('b, 'a) Agent.t; action: 'b; epoch: int} [@@deriving show] end =  
struct
  (** An observation is modeled as an [action] and the [agent] that caused it at [epoch] *)
  type ('a, 'b) t = {agent: ('b, 'a) Agent.t; action: 'b; epoch: int}  [@@deriving show]
end


(** A simple two agent, turn based environment with a primary agent and an opponent agent. *)
module Environment_2_agents : sig 
  open Observation
  type ('a, 'b) turn = private Opponent of ('b, 'a) Agent.t | Agent of ('a, 'b) Agent.t [@@deriving show]
  type ('a, 'b) obs = private From_agent of ('b, 'a) Observation.t  | From_opponent of ('a,'b) Observation.t [@@deriving show]
  type ('a, 'b) params = {trials:int; init_obs: ('a,'b) obs} [@@deriving show] 
  type ('a, 'b) state = private {params:('a, 'b) params; opponent: ('b,'a) Agent.t; agent:('a,'b) Agent.t; obs:('a,'b) obs;reward:Reward.t} [@@deriving show]
  type ('a, 'b) t = ('a, 'b) state Gen.t

  val from_agent_obs : ?epoch:int -> 'a -> ('a, 'b) Agent.t -> ('a, 'b) obs
  (** Creates an observation from the agent to the opponent *)

  val from_opponent_obs : ?epoch:int -> 'b -> ('b, 'a) Agent.t -> ('a, 'b) obs
  (** Creates an observatoin from the opponent to the agent *)

  val init : params:('a, 'b) params -> agent:('a,'b) Agent.t -> opponent:('b, 'a) Agent.t -> ('a,'b) t 
  (** Initializes an environment generator given some initial parameters [params], an [agent], and an [opponent] *)

  val opponent : ('a, 'b) state -> ('b , 'a) Agent.t
  (** Gets the opponent given a state instance *)

  val agent : ('a, 'b) state -> ('a, 'b) Agent.t
  (** Gets the agent given a state instance *)

  val turn : ('a, 'b) state -> ('a, 'b) turn 
  (** Gets which agent's turn it is to act given a state instance *)

  val reward : ('a, 'b) state -> Reward.t
  (** Gets the last observed reward in the system for the agent to act given a state instance *)

  val params: ('a, 'b) state -> ('a, 'b) params
  (** Gets the initializing parameters of the environment generator *)

end = 
struct
  open Observation
  type ('a, 'b) turn = Opponent of ('b, 'a) Agent.t | Agent of ('a, 'b) Agent.t [@@deriving show]
  type ('a, 'b) obs = From_agent of ('b, 'a) Observation.t  | From_opponent of ('a,'b) Observation.t [@@deriving show]
  type ('a, 'b) params = {trials:int; init_obs: ('a, 'b) obs} [@@deriving show] 
  type ('a, 'b) state = {params:('a, 'b) params; opponent: ('b,'a) Agent.t; agent:('a,'b) Agent.t; obs:('a,'b) obs; reward : float} [@@deriving show]
  type ('a, 'b) t = ('a, 'b) state Gen.t

  let init ~(params:('a, 'b) params) ~(agent:('a, 'b) Agent.t) ~(opponent:('b, 'a) Agent.t) : ('a, 'b) t =
    let open Gen.Infix in 
    Gen.(0--(params.trials-1)) |> fun g -> Gen_ext.fold_map (fun state epoch -> 
      match state.obs with
      | From_opponent (obs:('a, 'b) Observation.t) ->
        let policy = Agent.policy agent in
        let action = policy obs in
        let obs = {agent;epoch=obs.epoch+1;action} in
        let reward = (Agent.reward opponent) obs in
        {params; agent; opponent; obs = From_agent obs;reward}
      | From_agent (obs:('b, 'a) Observation.t) ->
        let policy = Agent.policy opponent in
        let action = policy obs in
        let obs = {agent=opponent;epoch=obs.epoch+1;action} in
        let reward = (Agent.reward agent) obs in
        {params; agent; opponent; obs = From_opponent obs;reward}
    )
    {params;agent;opponent;obs=params.init_obs;reward=0.} g 

 
  let agent t = t.agent
  let opponent t = t.opponent
  let turn t = match t.obs with From_agent _ -> Opponent t.opponent | From_opponent _ -> Agent t.agent
  let from_agent_obs ?(epoch=0) action agent = From_agent {agent; action; epoch}
  let from_opponent_obs ?(epoch=0) action agent = From_opponent {agent; action; epoch}
  let params t = t.params
  let reward t = t.reward

end


