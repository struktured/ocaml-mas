(** Abstraction over various plotting middlewares
  First we will use archimedes because its very easy to get going
  and works in utop.
  
  plplot and gnuplot are other possibilities, as well as js_of_ocaml + js plotting lib (cohttp server ?) *)

open Mas_core
let mean = Prob_cache_common.Update_rules.mean

module type S =
sig
  type t
  val running_avg : t -> 
    ('a, 'b) Environment_2_agents.t -> 
    ('a, 'b) Environment_2_agents.t
  val close : t -> unit
end

let running_avg (g:('a,'b) Environment_2_agents.t) = Gen_ext.fold_tuple
   (fun (orig_epoch, orig) state -> 
        match Environment_2_agents.turn state with 
        | Environment_2_agents.Opponent _ -> (orig_epoch, orig) 
        | Environment_2_agents.Agent _ ->       
          let reward = Environment_2_agents.reward state in
          let avg = mean ~cnt:orig_epoch ~orig ~obs:reward in
          (orig_epoch+1, avg)) (0, 0.0) g

