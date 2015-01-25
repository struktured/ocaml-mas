let reward_ub = 1.0
let reward_lb = 0.0
open Gen.Infix

module A = Archimedes

type t = {viewport: A.Viewport.t}

let init ?(viewport=A.init ["graphics"]) () =
  let p = { viewport } in p

let close t = A.close t.viewport
let running_avg t g turn = 
  A.Viewport.title t.viewport "Average Reward Over Time";
  A.Viewport.xlabel t.viewport "Epoch";
  A.Viewport.ylabel t.viewport "R";
  A.Axes.box ~grid:true t.viewport;
  let p = A.Path.make() in
  A.Path.move_to p 0.0 0.0;
  Mas_plot.running_avg g turn |> 
  Gen_ext.consume_second
    (fun (_, (cnt, avg_reward)) ->
       let x = CCFloat.of_int cnt in
       let y = avg_reward in
       A.Path.line_to p x y;
       A.Viewport.stroke t.viewport `Data p;
       A.show t.viewport) 
