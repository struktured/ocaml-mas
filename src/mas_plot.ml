open Mas_core
open Environments

let mean = Prob_cache_common.Update_rules.mean

module type S =
sig
  type t
  val running_avg : t ->
    ('a, 'b) Environment_2_agents.t ->
    Environment_2_agents.turn ->
    ('a, 'b) Environment_2_agents.t
  val close : t -> unit
end

module Running_average =
struct
  type t = {
    who: Environment_2_agents.turn option; agent_epoch:int; opponent_epoch:int;
    agent_avg: Reward.t; opponent_avg: Reward.t}

  let decorate (g:('a, 'b) Environment_2_agents.t) =
    let folder t state =
      let reward = Environment_2_agents.reward state in
      match Environment_2_agents.turn state with
      | Environment_2_agents.Agent as w ->
        let agent_avg = mean ~cnt:t.agent_epoch ~orig:t.agent_avg ~obs:reward in
        {t with who = Some w; agent_epoch = t.agent_epoch+1; agent_avg}
      | Environment_2_agents.Opponent as w ->
        let opponent_avg = mean ~cnt:t.opponent_epoch ~orig:t.opponent_avg ~obs:reward in
        {t with who = Some w; opponent_epoch=t.opponent_epoch+1; opponent_avg}
    in
    Gen_ext.fold_tuple folder 
      {who=None;agent_epoch=0;opponent_epoch=0;agent_avg=0.;opponent_avg=0.} g
end 

