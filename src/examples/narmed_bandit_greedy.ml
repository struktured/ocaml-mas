open Mas
open Mas_core
open Environments
open NArmedBandit

module Env = Environment_2_agents

let arms = 3
let eps = 0.85
let trials = 5000

let action_provider _ = CCArray.(0--(arms-1))

let value_function = State_based_value_function.init "discrete value function"

let policy = GreedyPolicy.init
  ~eps value_function action_provider

let () =
  let env = NArmedBandit.init_with_policy
    ~arms ~trials policy value_function in
  let init_state = CCOpt.get_exn (Gen.next env) in
  let plot = Archimedes_plot.init () in
  let player_turn = Env.Agent in 
  let env = Archimedes_plot.running_avg plot env player_turn in
  let print (s, (epoch, avg_reward)) = 
    print_endline ("Epoch: " ^ 
    string_of_int epoch ^ ", Avg Reward: " ^ 
    Reward.show avg_reward) in 
  let env = Mas_plot.running_avg env player_turn in
  Gen.iter print env;
  Archimedes_plot.close plot

