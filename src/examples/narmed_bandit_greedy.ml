open Mas
open Mas_core
open Environments
open NArmedBandit

module Env = Environment_2_agents
module R = Mas_plot.Running_average

let arms = 3
let eps = 0.90
let trials = 2000

let action_provider _ = CCArray.(0--(arms-1))

let value_function = State_based_value_function.init "discrete"

let policy = GreedyPolicy.init
  ~eps value_function action_provider

let () =
  let env = NArmedBandit.init_with_policy
    ~arms ~trials policy value_function in
  let plot = Archimedes_plot.init () in
  let player_turn = Env.Agent in 
  let env = Archimedes_plot.running_avg plot env player_turn in
  let print (s, avg) =
    let reward = Env.reward s Env.Agent in
    print_endline ("State: " ^ Environment_2_agents.show_state Arm.pp Reward.pp s);
    print_endline (
      "Epoch: " ^ string_of_int avg.R.agent_epoch ^ 
      ", Reward: " ^ Reward.show reward ^
      ", Avg Reward: " ^ Reward.show avg.R.agent_avg
  ) in 
  let env = R.decorate env in
  Gen.iter print env
  (*Archimedes_plot.close plot *)

