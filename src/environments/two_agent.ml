open Mas_system
  open Observation
  type who = Agent | Opponent [@@deriving show]
  type ('a, 'b) obs = From_agent of ('b, 'a) Observation.t 
    | From_opponent of ('a,'b) Observation.t [@@deriving show]
  type ('a, 'b) params = {trials:int; init_obs: ('a, 'b) obs} [@@deriving show] 
  type ('a, 'b) state = {
    params:('a, 'b) params; 
    agent:('a, 'b) Agent.t;     
    opponent: ('b, 'a) Agent.t; 
    obs:('a, 'b) obs; 
    agent_reward : float; 
    opponent_reward:float } [@@deriving show]
  type ('a, 'b) t = ('a, 'b) state Gen.t

  let init ~(params:('a, 'b) params) ~(agent:('a, 'b) Agent.t) ~(opponent:('b, 'a) Agent.t) : ('a, 'b) t =
    Gen.(0--(params.trials-1)) |> Gen.scan (fun state epoch ->
      match state.obs with
      | From_opponent (obs:('a, 'b) Observation.t) ->
        let policy = Agent.policy agent in
        let action = policy obs in
        let obs' = {agent;epoch=obs.epoch+1;action} in
        let agent_reward = (Agent.reward_fn agent) obs in
        ignore ((Value_fn.update (Agent.value_fn agent)) ~action obs agent_reward);
        {state with obs = From_agent obs';agent_reward}
      | From_agent (obs:('b, 'a) Observation.t) ->
        let policy = Agent.policy opponent in
        let action = policy obs in
        let obs' = {agent=opponent;epoch=obs.epoch+1;action} in
        let opponent_reward = (Agent.reward_fn opponent) obs in
        ignore ((Value_fn.update (Agent.value_fn opponent)) ~action obs opponent_reward);
        {state with obs = From_opponent obs';opponent_reward}
    )
    {params;agent;opponent;obs=params.init_obs;agent_reward=0.;opponent_reward=0.}

 
  let agent t = t.agent
  let opponent t = t.opponent
  let turn t = match t.obs with From_agent _ -> Opponent | From_opponent _ -> Agent 
  let from_agent_obs ?(epoch=0) action agent = From_agent {agent; action; epoch}
  let from_opponent_obs ?(epoch=0) action agent = From_opponent {agent; action; epoch}
  let params t = t.params
  let reward t turn = match turn with Agent -> t.agent_reward | Opponent -> t.opponent_reward
  let epoch t = match t.obs with From_agent obs -> obs.epoch | From_opponent obs -> obs.epoch

let noisy ?(min=0.) ?(max=1.) x =
  let max_noise = if x >= -1. && x <= 1. then x *. x else sqrt((sqrt (CCFloat.abs x))) in
  let noise = CCRandom.float_range (-.max_noise) max_noise in
  fun () ->
    let n = CCRandom.run noise in
    let sum = x +. n in
    if sum <= max && sum >= min then sum
    else let sum = x -. n in
    CCFloat.max min (CCFloat.min max sum)
