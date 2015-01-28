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
  A.Viewport.ylabel t.viewport "Reward";
  A.Axes.box ~grid:true t.viewport;
  let p = A.Path.make() in
  A.Path.move_to p 0.0 0.0;
  Mas_plot.Running_average.decorate g |>
  Gen.filter (fun (_, t) -> match R.who t with 
    | Some w -> w = turn 
    | None -> false)
  |>
  Gen_ext.consume_second
    (fun (_, avg) -> 
       let x = CCFloat.of_int avg.R.agent_epoch in
       let y = avg.R.agent_avg in
       let orig_x, orig_y = A.Path.current_point p in
       A.Path.clear p;
       A.Path.move_to p orig_x orig_y;
       A.Path.line_to p x y;
       A.Viewport.stroke t.viewport `Data p;
       A.show t.viewport) 
