open Mas_core
module State_based_value_fn =
  struct
    type ('s, 'a) t = ?action:'a -> 's -> Reward.t [@deriving show]
  end


module type STATE = sig type t [@@deriving show, ord] end

module Make(State:STATE)(Action:Action) = 
struct
  module StateAction = struct type t = State of State.t | Action of Action.t [@@deriving show, ord] end
  open StateAction
  module Cache = Prob_cache_containers.Set_model.Make(StateAction)

  let init () : (State.t, Action.t) State_based_value_fn.t = 
    let cache = Cache.create "value-function" in
  fun ?action s -> 
  let events = Cache.Events.of_list (
    match action with  
    | Some a -> [State s; Action a]
    | None -> [State s])
  in Cache.exp events cache 
end

