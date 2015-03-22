open Mas_system
  open Observation
  
  (** Which agent's turn it is to act *)
  type who = Agent | Opponent [@@deriving show]
  
  (** Wraps an observation depending on if it's from the agent or the opponent *)
  type ('a, 'b) obs = private From_agent of ('b, 'a) Observation.t  | From_opponent of ('a,'b) Observation.t [@@deriving show]
  
  (** The initializing parameters of the environment *)
  type ('a, 'b) params = {trials:int; init_obs: ('a,'b) obs} [@@deriving show] 

  (** The environment state, containing the parameters initializing the environment, the agent, opponent, and the last observation and reward *)
  type ('a, 'b) state = private {
    params:('a, 'b) params; 
    agent:('a,'b) Agent.t; 
    opponent: ('b,'a) Agent.t;
    obs:('a,'b) obs;
    agent_reward:Reward.t;
    opponent_reward:Reward.t} [@@deriving show]
  
  (** A generator of successive environment states (see [Gen.t] for details) *)
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

  val turn : ('a, 'b) state -> who
  (** Gets which agent's turn it is to act given a state instance *)

  val reward : ('a, 'b) state -> who -> Reward.t
  (** Gets the last observed reward in the system for the agent to act given a state instance and agent *)

  val params: ('a, 'b) state -> ('a, 'b) params
  (** Gets the initializing parameters of the environment generator *)

  val epoch: ('a, 'b) state -> int
  (** Gets the current epoch (trial) count *)
