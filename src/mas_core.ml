module Reward = Mas_intf.Reward

module Action = struct type 'a t = [>] as 'a end
module Action_with_init = struct type 'a t = [> `Init] as 'a end

(** Defines an agent and operations to the run the agent within the environment *)
module rec Agent : 
sig
  type ('a, 'b) t = {policy: ('a, 'b) Policy.t;reward : ('a,'b) Reward_fn.t;name:string} 
    constraint 'a = 'a Action.t constraint 'b = 'b Action.t 
  val policy : ('a, 'b) t -> ('a, 'b) Policy.t 
  val reward : ('a, 'b) t -> ('a, 'b) Reward_fn.t
  val init : ('a, 'b) Policy.t -> ('a, 'b) Reward_fn.t -> string -> ('a, 'b) t 
  val name : ('a, 'b) t -> string
end =
struct
  type ('a, 'b) t = {policy: ('a, 'b) Policy.t; reward: ('a, 'b) Reward_fn.t; name:string} 
    constraint 'a = 'a Action.t constraint 'b = 'b Action.t 
  let init (policy:('a, 'b) Policy.t) (reward:('a, 'b) Reward_fn.t) name = {reward;policy;name}
  let policy (t:('a, 'b) t) = t.policy
  let reward (t:('a, 'b) t) = t.reward
  let name t = t.name
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
  Observation : sig type ('a,'b) t = {agent: ('b, 'a) Agent.t; action: 'b Action.t; epoch: int} 
  constraint 'a = 'a Action.t constraint 'b = 'b Action.t end =  
struct
  (** An observation is modeled as an [action] and the [agent] that caused it at [epoch] *)
  type ('a, 'b) t = {agent: ('b, 'a) Agent.t; action: 'b Action.t; epoch: int} 
end


(** A simple two agent environment *)
module Environment : sig 
  open Observation
  type params = {trials:int} [@@deriving show] 
  type ('a,'b) turn = Opponent of ('b, 'a) Agent.t | Agent of ('a, 'b) Agent.t
  type ('a,'b) obs = From_agent of ('b, 'a) Observation.t  | From_opp of ('a,'b) Observation.t
  type ('a,'b) state = {params:params; opp: ('b,'a) Agent.t; agent:('a,'b) Agent.t; obs:('a,'b) obs}
  type ('a,'b) t = ('a,'b) state Gen.t
  val init : params:params -> opp:('b, [> `Init] as 'a) Agent.t -> agent:('a,'b) Agent.t -> ('a,'b) t 
  val opp : ('a, 'b) state -> ('b , 'a) Agent.t
  val agent : ('a, 'b) state -> ('a, 'b) Agent.t
  val turn : ('a,'b) state -> ('a,'b) turn
end = 
struct
  open Observation
  type params = {trials:int} [@@deriving show]
  type ('a,'b) turn = Opponent of ('b, 'a) Agent.t | Agent of ('a, 'b) Agent.t
  type ('a,'b) obs = From_agent of ('b, 'a) Observation.t  | From_opp of ('a,'b) Observation.t
  type ('a,'b) state = {params:params; opp: ('b,'a) Agent.t; agent:('a,'b) Agent.t; obs:('a,'b) obs}
  type ('a,'b) t = ('a,'b) state Gen.t
  let init ~(params:params) ~(opp:('b,'a) Agent.t) ~(agent:('a,'b) Agent.t) : ('a, 'b) t =
    let open Gen.Infix in 
    Gen.(0--(params.trials-1)) |> fun g -> Gen_ext.fold_map (fun state epoch -> 
      match state.obs with
      | From_opp (obs:('a, 'b) Observation.t) ->
        let policy = Agent.policy agent in
        let action = policy obs in
        let obs = {agent;epoch;action} in
        {params; agent; opp; obs = From_agent obs}
      | From_agent (obs:('b, 'a) Observation.t) ->
        let policy = Agent.policy opp in
        let action = policy obs in
        let obs = {agent=opp;epoch;action} in
        {params; agent; opp;obs = From_opp obs}
    ) 
      {params;agent;opp;obs=From_agent {agent;action=`Init;epoch=0}} g 

  let agent t = t.agent
  let opp t = t.opp
  let turn t = match t.obs with From_agent _ -> Opponent t.opp | From_opp _ -> Agent t.agent
  let reward t = let obs, turn = 
    match t.obs with From_agent obs -> obs, t.opp | From_opp obs -> obs, t.agent in
    Agent.reward turn obs
end


