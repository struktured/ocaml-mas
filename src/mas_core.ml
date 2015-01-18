(** The type for reward signals, equal to float *)
module Reward = Mas_intf.Reward

(** Defines the minimum requirements for an action,
 * which that it is showable and comparable *)
module type Action = sig type t [@@deriving show, ord] end

(** Defines an agent and operations to run the agent within the environment *)
module rec Agent :
sig
  
  (** An agent which takes emits actions of type ['a] given observed
      actions of type ['b] from other agents
  *)
  type ('a, 'b) t = private {
    policy: ('a, 'b) Policy.t;
    reward_fn : ('a, 'b) Reward_fn.t;
    value_fn : ('a, 'b) Value_fn.t;
    name:string} [@@deriving show]
 
  val policy : ('a, 'b) t -> ('a, 'b) Policy.t
  (** The policy this agent uses to make actions given observations *)

  val reward_fn : ('a, 'b) t -> ('a, 'b) Reward_fn.t
  (** The reward function associated with the given agent.
      Determines a reward signal given an observation *)

  val value_fn : ('a, 'b) t -> ('a, 'b) Value_fn.t
  (** The value function associated with the given agent.
      Determines an estimated reward signal given an observation *)

  val init : ('a, 'b) Policy.t -> ('a, 'b) Reward_fn.t -> ('a, 'b) Value_fn.t -> name:string -> ('a, 'b) t
  (** Initializes an agent given a policy, reward function, value function, and [name] *)

  val name : ('a, 'b) t -> string
  (** Gets the [name] of the agent *)

end =
struct
  type ('a, 'b) t = {
    policy: ('a, 'b) Policy.t;
    reward_fn : ('a, 'b) Reward_fn.t;
    value_fn : ('a, 'b) Value_fn.t;
    name:string} [@@deriving show]

  let init (policy:('a, 'b) Policy.t) (reward_fn:('a, 'b) Reward_fn.t) value_fn ~name =
    {policy;value_fn;reward_fn;name}
  let policy (t:('a, 'b) t) = t.policy
  let reward_fn (t:('a, 'b) t) = t.reward_fn
  let value_fn t = t.value_fn
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
  (** The value function module. Defines an estimator of expected reward
      given an observation and (optionally) an action. Value functions
      are typically evolving, as environments will update the value function
      with each newly observed reward signal by the agent *)
  Value_fn :
    sig
      (** The type of a value function, composing of a [value], [count],
          and [update] function. *)
      type ('a, 'b) t = private {
        value : ?action:'a -> ('a, 'b) Observation.t -> Reward.t;
        count : ?action:'a -> ('a, 'b) Observation.t -> int;
        update : action:'a -> ('a, 'b) Observation.t -> Reward.t -> unit
      } [@@deriving show]
  
      val value : ('a, 'b) t -> ?action: 'a -> ('a, 'b) Observation.t
        -> Reward.t
      (** Gets the value estimator for this value function instance.
          The returned function estimates the reward signal given
          the observation and an optional action. *)

      val count : ('a, 'b) t -> ?action: 'a -> ('a, 'b) Observation.t -> int
      (** Gets the visitor count function for this value function instance.
          The returned function counts the number of the times the
          given observation and an optional [action] have been
          visited or attempted by the agent *)


      val update : ('a, 'b) t -> action : 'a -> ('a, 'b) Observation.t
        -> Reward.t -> unit
      (** Updates a value function instance. Given an action,
          new observation, and reward signal, update the underlying
          model of the value function *)

      val init : 
        value:(?action:'a -> ('a, 'b) Observation.t -> Reward.t) -> 
        count:(?action:'a -> ('a, 'b) Observation.t -> int) ->
        update:(action:'a -> ('a, 'b) Observation.t -> Reward.t -> unit) ->
        ('a, 'b) t
      (** Initializes a new value function given a [value] estimator,
          visitor [count] function, and an value [update] function *)
    end =
  struct
  type ('a, 'b) t = {
    value : ?action:'a -> ('a, 'b) Observation.t -> Reward.t;
    count : ?action:'a -> ('a, 'b) Observation.t -> int;
    update : action:'a -> ('a, 'b) Observation.t -> Reward.t -> unit
  } [@@deriving show]
  
  let init ~value ~count ~update =
    {value;count;update}

  let value t = t.value
  let count t = t.count
  let update t ~action s r = t.update ~action s r
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

  (** Which agent's turn it is to act *)
  type ('a, 'b) turn = private Opponent of ('b, 'a) Agent.t | Agent of ('a, 'b) Agent.t [@@deriving show]

  (** Wraps an observation depending on if it's from the agent or the opponent *)
  type ('a, 'b) obs = private From_agent of ('b, 'a) Observation.t  | From_opponent of ('a,'b) Observation.t [@@deriving show]
  
  (** The initializing parameters of the environment *)
  type ('a, 'b) params = {trials:int; init_obs: ('a,'b) obs} [@@deriving show] 

  (** The environment state, containing the parameters initializing the environment, the agent, opponent, and the last observation and reward *)
  type ('a, 'b) state = private {params:('a, 'b) params; opponent: ('b,'a) Agent.t; agent:('a,'b) Agent.t; obs:('a,'b) obs;reward:Reward.t} [@@deriving show]
  
  (** A generator of successive environment states *)
  type ('a, 'b) t = ('a, 'b) state Gen.t

  val from_agent_obs : ?epoch:int -> 'a -> ('a, 'b) Agent.t -> ('a, 'b) obs
  (** Creates an observation from the agent to the opponent *)

  val from_opponent_obs : ?epoch:int -> 'b -> ('b, 'a) Agent.t -> ('a, 'b) obs
  (** Creates an observation from the opponent to the agent *)

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

  val epoch: ('a, 'b) state -> int
  (** Gets the current epoch (trial) count *)

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
    Gen.(0--(params.trials-1)) |> fun g -> Gen.scan (fun state epoch ->
      match state.obs with
      | From_opponent (obs:('a, 'b) Observation.t) ->
        let policy = Agent.policy agent in
        let action = policy obs in
        let obs' = {agent;epoch=obs.epoch+1;action} in
        let reward = (Agent.reward_fn agent) obs in
        (Value_fn.update (Agent.value_fn agent)) ~action obs reward;
        {params; agent; opponent; obs = From_agent obs';reward}
      | From_agent (obs:('b, 'a) Observation.t) ->
        let policy = Agent.policy opponent in
        let action = policy obs in
        let obs' = {agent=opponent;epoch=obs.epoch+1;action} in
        let reward = (Agent.reward_fn opponent) obs in
        (Value_fn.update (Agent.value_fn opponent)) ~action obs reward;
        {params; agent; opponent; obs = From_opponent obs';reward}
    )
    {params;agent;opponent;obs=params.init_obs;reward=0.} g 

 
  let agent t = t.agent
  let opponent t = t.opponent
  let turn t = match t.obs with From_agent _ -> Opponent t.opponent | From_opponent _ -> Agent t.agent
  let from_agent_obs ?(epoch=0) action agent = From_agent {agent; action; epoch}
  let from_opponent_obs ?(epoch=0) action agent = From_opponent {agent; action; epoch}
  let params t = t.params
  let reward t = t.reward
  let epoch t = match t.obs with From_agent obs -> obs.epoch | From_opponent obs -> obs.epoch

end


