open Mas
open Mas_core
open Environments
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

