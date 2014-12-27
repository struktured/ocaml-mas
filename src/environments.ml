open Mas_core

let noisy ?(min=0.) ?(max=1.) x = 
  let max_noise = if x >= -1. && x <= 1. then x *. x else sqrt x in
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
  type 'a agent_action = [`Arm of int] as 'a [@@deriving show, ord]
  type 'b opponent_action = [`Reward of float] as 'b [@@deriving show, ord]
  type ('a, 'b) agent = ('a opponent_action, 'b agent_action) Agent.t [@@deriving show]
  type ('b, 'a) opponent = (['b agent_action | `Init], 'a opponent_action) Agent.t [@@deriving show]

  let agent_reward obs = match obs.action with `Reward r -> r | _ -> 0.0

  let init_agent policy name = 
    Agent.init policy agent_reward name

  let init_opponent ~(arms:int) =
    let rand = CCRandom.float 1.0 in
    let open Gen.Infix in
    let arm_rewards = Gen.to_array (Gen.(0--(arms-1)) >>| fun (_:int) -> noisy (CCRandom.run rand)) in  
    let policy obs = match obs.action with 
      | `Arm a -> `Reward (arm_rewards.(a) ())
      | `Init -> `Reward 0.0
      | a  -> failwith ("unknown action: " ^ (Action.show a a)) in
    Agent.init policy (fun obs -> 0.0) ((string_of_int arms) ^ "-armed bandit")
 
  let init ?(arms=10) ~params ~agent = 
    Environment_2_agents.init ~params ~agent:agent ~opponent:(init_opponent arms)

  let init_with_policy ?arms ?(name="player") ~params policy  = 
    init ~params ?arms ~agent:(init_agent policy name) 

end 
