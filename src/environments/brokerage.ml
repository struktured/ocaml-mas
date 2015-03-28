(** A multiagent environment for many to one agent interactions through a broker agent *)

open Mas_core
open Mas_system
open Observation
type who = Agent | Broker [@@deriving show]

module Broker_action = struct
  type ('priv, 'pub) t =
    | Private of 'priv
    | Public of 'pub [@@deriving show, ord]
end

type ('a, 'priv, 'pub) obs =
  | From_agent of ('a, ('priv, 'pub) Broker_action.t) Observation.t
  | From_broker of  (('priv, 'pub) Broker_action.t, 'a) Observation.t
    [@@deriving show]

type ('a, 'priv, 'pub) params = {trials:int; init_obs: ('a, 'priv, 'pub) obs} 
  [@@deriving show]

type ('a, 'priv, 'pub) state = {
  params:('a, 'priv, 'pub) params; 
  agents:('a, ('priv, 'pub) Broker_action.t) Agent.t array;     
  broker: (('priv, 'pub) Broker_action.t, 'a) Agent.t; 
  obs:('a, 'priv, 'pub) obs; 
  agent_reward : float; 
  broker_reward:float } [@@deriving show]
type ('a, 'priv, 'pub) t = ('a, 'priv, 'pub) state Gen.t

let init ~(params:('a, 'priv, 'pub) params) ~(agents:('a, ('priv, 'pub) Broker_action.t) Agent.t array) 
  ~(broker:(('priv, 'pub) Broker_action.t, 'a) Agent.t) : ('a, 'priv, 'pub) t =
  Gen.(0--(params.trials-1)) |> Gen.scan (fun state epoch ->
    match state.obs with
    | From_broker (obs:('a, ('priv, 'pub) Broker_action.t) Observation.t) ->
      List.fold_left (fun state agent -> 
        let policy = Agent.policy agent in
        let action = policy obs in
        let obs' = {agent;epoch=obs.epoch+1;action} in
        let agent_reward = (Agent.reward_fn agent) obs in
        ignore ((Value_fn.update (Agent.value_fn agent)) ~action obs agent_reward);
        {state with obs = From_agent obs';agent_reward})
    | From_agent (obs:(('priv, 'pub) Broker_action.t, 'a) Observation.t) ->
      let policy = Agent.policy broker in
      let action = policy obs in
      let obs' = {agent=broker;epoch=obs.epoch+1;action} in
      let broker_reward = (Agent.reward_fn broker) obs in
      ignore ((Value_fn.update (Agent.value_fn broker)) ~action obs broker_reward);
      {state with obs = From_broker obs';broker_reward}
  )
  {params;agents;broker;obs=params.init_obs;agent_reward=0.;broker_reward}

let agent t = t.agent
let broker t = t.broker
let turn t = match t.obs with From_agent _ -> Broker | From_broker _ -> Agent 
let from_agent_obs ?(epoch=0) action agent = From_agent {agent; action; epoch}
let from_broker_obs ?(epoch=0) action agent = From_broker {agent; action; epoch}
let params t = t.params
let reward t turn = match turn with Agent -> t.agent_reward | Broker -> t.broker_reward
let epoch t = match t.obs with From_agent obs -> obs.epoch | From_broker obs -> obs.epoch
