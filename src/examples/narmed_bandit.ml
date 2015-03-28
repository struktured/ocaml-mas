open Mas_core
open Mas_system
open Mas_environments
open Mas_plot
open Mas_policies
open Mas_value_functions
open Mas_learning_rules
open Mas_agents

module NArmedBandit =
struct
  let last_plot = ref None

  open Observation
  module Env = Two_agent
  open Two_agent
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
  module Value_function = Discrete_value_function.Make(State)(Arm)

  module BanditAgent = State_based_agent.Make(Value_function)(Reward)

  module Greedy_policy = Greedy_policy.Make(Value_function)
  module Uct_policy = Uct_policy.Make(Value_function)
  module Softmax_policy = Softmax_policy.Make(Value_function)

  let agent_reward (obs:(BanditAgent.Action.t, Reward.t) Observation.t) = match (obs.action:Reward.t) with r -> r

  let state_trans obs = ()

  let init_agent policy value_fn name =
    BanditAgent.init policy state_trans value_fn agent_reward ~name

  let default_arms = 5

  let init_opponent ?arm_rewards ?(num_arms=default_arms) () : opponent =
    let open Gen.Infix in
    let arm_rewards = CCOpt.get_lazy (fun () -> Gen.to_array (Gen.(0--(num_arms-1)) >>|
      fun (_:int) -> Oml.Sampling.normal ~mean:0.5 ~std:1.0 ())) arm_rewards in
    let policy : (Reward.t, Arm.t) Policy.t = fun obs -> match obs.action with a -> (arm_rewards.(a) ()) in
    Agent.init policy (fun obs -> 0.0) (Value_fn.init ~count:(fun ?action obs -> 0)
      ~value:(fun ?action obs -> 0.0) ~update:(fun t ~action obs r -> t))
      ~name:((string_of_int (CCArray.length arm_rewards)) ^ "-armed bandit")

  let init ?arm_rewards ?num_arms ~trials ~(agent:agent) : (Arm.t, Reward.t) Env.t =
    let opponent : opponent = init_opponent ?arm_rewards ?num_arms () in
    let params : params = {trials;init_obs=Env.from_opponent_obs 0.0 opponent} in
    Env.init ~params ~agent ~opponent

  let init_with_policy ?arm_rewards ?num_arms ~trials ?(name="player") policy value_fn =
    init ?arm_rewards ?num_arms ~trials ~agent:(init_agent policy value_fn name)
end

open NArmedBandit
module Env = Two_agent
module R = Decorators.Running_average

let arm_weights = Array.of_list [0.1;0.5;1.0]
let eps = 0.85
let trials = 150
let c = 2.0
let temp = 1.0
type policy = [`Random | `Greedy | `UCT | `Softmax]
type learning_rule = [`Mean_update | `Q_learner]
module Q_learner = Q_learner.Make(Value_function)
module Sarsa_learner = Sarsa_learner.Make(Value_function)

let go ?(policy=`Greedy) ?(learning_rule=`Mean_update) ?(eps=eps) ?(c=c) ?weights
  ?(trials=trials) ?(arm_weights=arm_weights) ?(show_plot=true) () =
  let arm_rewards = CCArray.map (fun x -> fun () -> x) arm_weights in
  let max_weight, max_index =
    CCArray.foldi (fun (w, k) i w' ->
      if w' > w then (w', k+1) else (w, k)) (0.0, 0) arm_weights in
  let num_arms = CCArray.length arm_rewards in
  let action_provider _ = CCArray.(0--(num_arms-1)) in
  let learning_rule = match learning_rule with
    | `Mean_update -> None
    | `Sarsa_learner -> Some (Sarsa_learner.init action_provider)
    | `Q_learner -> Some (Q_learner.init action_provider) in
  let value_function = Value_function.init "discrete" ?learning_rule in
  let policy = match policy with 
    | `Random -> Random_policy.init ?weights action_provider
    | `Greedy -> Greedy_policy.init ~eps value_function action_provider
    | `UCT -> Uct_policy.init ~c value_function action_provider 
    | `Softmax -> Softmax_policy.init ~temp value_function action_provider in
  let env = NArmedBandit.init_with_policy
              ~arm_rewards ~trials policy value_function in
  let plot = if show_plot then Some (Archimedes_plot.init ()) else None in
  last_plot := plot;
  let player_turn = Env.Agent in
  let env = CCOpt.maybe (fun p -> Archimedes_plot.running_avg p
    ~turn:player_turn ~ub:(fun s -> max_weight) env) env plot in
  let print (s, avg) =
    let reward = Env.reward s Env.Agent in
    print_endline ("State: " ^ Two_agent.show_state Arm.pp Reward.pp s);
    print_endline (
      "Epoch: " ^ string_of_int avg.R.agent_epoch ^
      ", Reward: " ^ Reward.show reward ^
      ", Avg Reward: " ^ Reward.show avg.R.agent_avg
    ) in
  let env = R.decorate env in
  Gen.iter print env;plot

let close p : unit =
  CCOpt.maybe (fun p -> Archimedes_plot.close p) () p

let close_last_plot () = close !last_plot; last_plot := None

