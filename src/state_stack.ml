
open Mas_core
module type STATE = sig type t [@@deriving show, ord] end

module Make(State:STATE)(Action:Action) =
struct
  let default_max_size = 10
  type state_action_reward = {state: State.t; action: Action.t ; reward: Reward.t} 
  type t = {max_size:int; actual_size:int; stack: state_action_reward list}
  let init ?(max_size=default_max_size) () = 
    {max_size;actual_size=0; stack=[]}

  let append state action reward t = 
    if t.actual_size < t.max_size then 
      {t with actual_size=t.actual_size+1;stack={state;action;reward}::t.stack}
    else
      {t with stack = {state;action;reward}::CCList.range' 0 t.max_size stack}
  let last t = List.hd t.stack

  let nth = List.nth t.stack

  let take n t = CCList.take n t.stack

    

