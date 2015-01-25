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

let running_avg (g:('a,'b) Environment_2_agents.t) turn =
  let folder (orig_epoch, orig) state =
    if Environment_2_agents.turn state = turn 
    then
     let reward = Environment_2_agents.reward state in
      let avg = mean ~cnt:orig_epoch ~orig ~obs:reward in
       (orig_epoch+1, avg)
    else 
      (orig_epoch, orig)
  in
  Gen_ext.fold_tuple folder (0, 0.0) g
 

