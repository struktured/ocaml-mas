open Mas_core
open Environments

let mean = Prob_cache_common.Update_rules.mean

module type S =
sig
  type t
  val running_avg : t ->
    ?turn:Environment_2_agents.who ->
    ?ub:(('a, 'b) Environment_2_agents.state -> float) ->
    ('a, 'b) Environment_2_agents.t ->
    ('a, 'b) Environment_2_agents.t
  val close : t -> unit
end

module type Environment_decorator = 
  sig
    type t
    val decorate : ('a, 'b) Environment_2_agents.t ->
      (('a, 'b) Environment_2_agents.state * t) Gen.t
  end

module Running_average (*: Environment_decorator *) = 
struct
type t = {
    who: Environment_2_agents.who option; agent_epoch:int; opponent_epoch:int;
    agent_avg: Reward.t; opponent_avg: Reward.t}

  let who t = t.who
  let agent_avg t = t.agent_avg
  let opponent_avg t = t.opponent_avg
  let agent_epoch t = t.agent_epoch
  let opponent_epoch t = t.opponent_epoch

  let decorate (g:('a, 'b) Environment_2_agents.t) =
    let folder t state =
      match Environment_2_agents.turn state with
      | Environment_2_agents.Agent as w ->
        let agent_reward = Environment_2_agents.reward state w in
        let agent_avg = mean ~cnt:t.agent_epoch ~obs:state ~orig:t.agent_avg ~exp:agent_reward in
        {t with who = Some w; agent_epoch = t.agent_epoch+1; agent_avg}
      | Environment_2_agents.Opponent as w ->
        let opponent_reward = Environment_2_agents.reward state w in
        let opponent_avg = mean ~cnt:t.opponent_epoch ~obs:state ~orig:t.opponent_avg ~exp:opponent_reward in
        {t with who = Some w; opponent_epoch=t.opponent_epoch+1; opponent_avg}
    in
    Gen_ext.fold_tuple folder 
      {who=None;agent_epoch=0;opponent_epoch=0;agent_avg=0.;opponent_avg=0.} g
end 

