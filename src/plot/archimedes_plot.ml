open Mas_core
open Gen.Infix
open Mas_environments
module A = Archimedes

type t = {viewport: A.Viewport.t}
module R = Decorators.Running_average

let init ?(viewport=A.init ["graphics"]) () =
  let p = { viewport } in p

let close t = A.close t.viewport


let append p x y =
  if A.Path.is_empty p then
    A.Path.move_to p x y 
  else
    let orig_x, orig_y = A.Path.current_point p in
    A.Path.clear p;
    A.Path.move_to p orig_x orig_y;
    A.Path.line_to p x y
  
let running_avg t ?(turn=Two_agent.Agent) 
  ?(ub=fun s -> 1.0) g = 
  A.Viewport.title t.viewport "Average Reward Over Time";
  A.Viewport.xlabel t.viewport "Epoch";
  A.Viewport.ylabel t.viewport "Reward";
  A.Axes.box ~grid:true t.viewport;
  let p = A.Path.make() in
  let p_ub = A.Path.make() in
  R.decorate g |>
  Gen.filter (fun (_, t) -> match R.who t with 
    | Some w -> w = turn 
    | None -> false)
  |>
  Gen_ext.consume_second
    (fun ((s:('a, 'b) Two_agent.state), avg) -> 
       let x = CCFloat.of_int avg.R.agent_epoch in
       let y = avg.R.agent_avg in
       let y_ub = ub s in 
       append p x y; 
       append p_ub x y_ub;
       A.Viewport.set_color t.viewport A.Color.black;
       A.Viewport.stroke t.viewport `Data p;
       A.Viewport.set_color t.viewport A.Color.red;
       A.Viewport.stroke t.viewport `Data p_ub;
       A.show t.viewport) 
