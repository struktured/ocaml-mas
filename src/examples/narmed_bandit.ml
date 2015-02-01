open Mas
open Mas_core
open Environments

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
  module UCTPolicy = Policies.UCTPolicy(State)(Arm)

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

open NArmedBandit
module Env = Environment_2_agents
module R = Mas_plot.Running_average

let arm_weights = Array.of_list [0.1;0.5;1.0]
let eps = 0.85
let trials = 150
let c = 2.0

type policy = [`Random | `Greedy | `UCT]

let go ?(policy=`Greedy) ?(eps=eps) ?(c=c) ?weights
  ?(trials=trials) ?(arm_weights=arm_weights) ?(show_plot=true) () =
  let arm_rewards = CCArray.map (fun x -> fun () -> x) arm_weights in
  let max_weight, max_index = 
    CCArray.foldi (fun (w, k) i w' -> 
      if w' > w then (w', k+1) else (w, k)) (0.0, 0) arm_weights in
  let num_arms = CCArray.length arm_rewards in
  let action_provider _ = CCArray.(0--(num_arms-1)) in
  let value_function = State_based_value_function.init "discrete" in
  let policy = match policy with 
    | `Random -> Policies.RandomPolicy.init ?weights action_provider 
    | `Greedy -> GreedyPolicy.init ~eps value_function action_provider 
    | `UCT -> UCTPolicy.init ~c value_function action_provider in
  let env = NArmedBandit.init_with_policy
              ~arm_rewards ~trials policy value_function in
  let plot = if show_plot then Some (Archimedes_plot.init ()) else None in
  let player_turn = Env.Agent in 
  let env = CCOpt.maybe (fun p -> Archimedes_plot.running_avg p 
    ~turn:player_turn ~ub:(fun s -> max_weight) env) env plot in
  let print (s, avg) =
    let reward = Env.reward s Env.Agent in
    print_endline ("State: " ^ Environment_2_agents.show_state Arm.pp Reward.pp s);
    print_endline (
      "Epoch: " ^ string_of_int avg.R.agent_epoch ^ 
      ", Reward: " ^ Reward.show reward ^
      ", Avg Reward: " ^ Reward.show avg.R.agent_avg
    ) in 
  let env = R.decorate env in
  Gen.iter print env;plot

let close p =
  CCOpt.maybe (fun p -> Archimedes_plot.close p) () p

