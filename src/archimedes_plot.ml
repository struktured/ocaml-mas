open Gen.Infix
open Environments
module A = Archimedes

type t = {viewport: A.Viewport.t}
module R = Mas_plot.Running_average

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
  Mas_plot.Running_average.decorate g |>
  Gen.filter (fun (_, t) -> match t.R.who with 
    | Some w -> w = turn 
    | None -> false)
  |>
  Gen_ext.consume_second
    (fun (_, avg) -> 
       let x = CCFloat.of_int avg.R.agent_epoch in
       let y = avg.R.agent_avg in
       A.Path.line_to p x y;
       A.Viewport.stroke t.viewport `Data p;
       A.show t.viewport) 
