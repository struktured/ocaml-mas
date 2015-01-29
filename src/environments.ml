open Mas_core

(** A simple two agent, turn based environment with a primary agent and an opponent agent. *)
module Environment_2_agents : sig 
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

  val turn : ('a, 'b) state -> who
  (** Gets which agent's turn it is to act given a state instance *)

  val reward : ('a, 'b) state -> who -> Reward.t
  (** Gets the last observed reward in the system for the agent to act given a state instance and agent *)

  val params: ('a, 'b) state -> ('a, 'b) params
  (** Gets the initializing parameters of the environment generator *)

  val epoch: ('a, 'b) state -> int
  (** Gets the current epoch (trial) count *)

end = 
struct 
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
        (Value_fn.update (Agent.value_fn agent)) ~action obs agent_reward;
        {state with obs = From_agent obs';agent_reward}
      | From_agent (obs:('b, 'a) Observation.t) ->
        let policy = Agent.policy opponent in
        let action = policy obs in
        let obs' = {agent=opponent;epoch=obs.epoch+1;action} in
        let opponent_reward = (Agent.reward_fn opponent) obs in
        (Value_fn.update (Agent.value_fn opponent)) ~action obs opponent_reward;
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
end


let noisy ?(min=0.) ?(max=1.) x =
  let max_noise = if x >= -1. && x <= 1. then x *. x else sqrt((sqrt (CCFloat.abs x))) in
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
  module Arm = struct type t = int [@@deriving show, ord] end
  module Reward = struct type t = float [@@deriving show, ord] end
  module State =
    struct
      let compare_unit () () = 0
      type t = unit [@opaque] [@@deriving show, ord]
    end

  type opponent = (Reward.t, Arm.t) Agent.t [@@deriving show]
  type agent = (Arm.t, Reward.t) Agent.t [@@deriving show]
  type params = (Arm.t, Reward.t) Env.params
  let agent_reward obs = match (obs.action:Reward.t) with r -> r

  module State_based_value_function = Value_functions.Make_discrete(State)(Arm)

  module BanditAgent = Agents.Make_state_based(State_based_value_function.Value_function)(Reward)

  module GreedyPolicy = Policies.GreedyPolicy(State)(Arm)

  let state_trans obs = ()

  let init_agent policy value_fn name =
    BanditAgent.init policy state_trans value_fn agent_reward name

  let default_arms = 5

  let init_opponent ?arm_rewards ?(num_arms=default_arms) () : opponent =
    let rand = CCRandom.float 1.0 in
    let open Gen.Infix in
    let arm_rewards = CCOpt.get_lazy (fun () -> Gen.to_array (Gen.(0--(num_arms-1)) >>| 
      fun (_:int) -> noisy (CCRandom.run rand))) arm_rewards in
    let policy : (Reward.t, Arm.t) Policy.t = fun obs -> match obs.action with a -> (arm_rewards.(a) ()) in
    Agent.init policy (fun obs -> 0.0) (Value_fn.init ~count:(fun ?action obs -> 0) 
      ~value:(fun ?action obs -> 0.0) ~update:(fun ~action obs r -> ())) 
      ~name:((string_of_int (CCArray.length arm_rewards)) ^ "-armed bandit")

  let init ?arm_rewards ?num_arms ~trials ~(agent:agent) : (Arm.t, Reward.t) Env.t =
    let opponent : opponent = init_opponent ?arm_rewards ?num_arms () in
    let params : params = {trials;init_obs=Env.from_opponent_obs 0.0 opponent} in
    Env.init ~params ~agent ~opponent

  let init_with_policy ?arm_rewards ?num_arms ~trials ?(name="player") policy value_fn = 
    init ?arm_rewards ?num_arms ~trials ~agent:(init_agent policy value_fn name)
end 
