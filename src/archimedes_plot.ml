let reward_ub = 1.0
let reward_lb = 0.0
open Gen.Infix

module A = Archimedes

type t = {viewport: A.Viewport.t}

let init ?(viewport=A.init ["graphics"]) () =
  let p = { viewport } in p

  (*     let plot_iter = A.fx viewport f reward_lb reward_ub in p *)
(*    let reward_vs_time ?(update_int=10) t (env:('a, 'b) Environment_2_agents.t)  =  () *)
let close t = A.close t.viewport
let running_avg t g = 
  A.Viewport.title t.viewport "Average Reward Over Time";
  A.Axes.box ~grid:true t.viewport;
  let module Cache = CCHashtbl.Make(CCInt) in
  let cache = Cache.create 0 in
  Mas_plot.running_avg g |> 
  Gen_ext.side_effect_iter
    (fun (state, (cnt, avg_reward)) ->
       Cache.add cache cnt avg_reward;
       A.fx t.viewport (fun x -> 
         let i = CCFloat.to_int x in
         CCOpt.get 0.0 (Cache.get cache i)) 0.0 (CCFloat.of_int cnt);
       A.show t.viewport) |> 
  Gen_ext.tuple_drop_second



