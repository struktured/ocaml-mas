open Mas_core

let noisy ?(min=0.) ?(max=1.) x = 
  let max_noise = if x >= -1. && x <= 1. then x *. x else sqrt (CCFloat.abs x) in
  let noise = CCRandom.float_range (-.max_noise) max_noise in
  fun () -> 
    let n = CCRandom.run noise in
    let sum = x +. n in
    if sum <= max && sum >= min then sum
    else let sum = x -. n in
    CCFloat.max min (CCFloat.min max sum)

module NArmedBandit =
struct
  open Observation
  module Env = Environment_2_agents
  open Environment_2_agents
  type arm = int [@@deriving show, ord]
  type reward = float [@@deriving show, ord]

  type opponent = (reward, arm) Agent.t [@@deriving show]
  type agent = (arm, reward) Agent.t [@@deriving show]
  type params = (arm, reward) Env.params
  let agent_reward obs = match (obs.action:reward) with r -> r

  let init_agent policy name =
    Agent.init policy agent_reward name

  let init_opponent ~(arms:int) : opponent =
    let rand = CCRandom.float 1.0 in
    let open Gen.Infix in
    let arm_rewards = Gen.to_array (Gen.(0--(arms-1)) >>| fun (_:int) -> noisy (CCRandom.run rand)) in
    let policy : (reward, arm) Policy.t = fun obs -> match obs.action with a -> (arm_rewards.(a) ()) in
    Agent.init policy (fun obs -> 0.0) ((string_of_int arms) ^ "-armed bandit")

  let init ?(arms=10) ~trials ~(agent:agent) : (arm, reward) Env.t =
    let opponent : opponent = init_opponent ~arms in
    let params : params = {trials;init_obs=Env.from_opponent_obs (0.0) opponent} in
    Env.init ~params ~agent ~opponent

  let init_with_policy ?arms ~trials ?(name="player") policy = 
    init ?arms ~trials ~agent:(init_agent policy name)
end 
