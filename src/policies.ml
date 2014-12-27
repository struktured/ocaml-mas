open Mas_core
 
open Prob_cache_containers

module GreedyPolicy(Action:Set_model.EVENT) = 
struct
 module Model = Prob_cache_containers.Set_model.Make(Action)
  let default_eps = 0.90
  let init ?(eps=default_eps) model action_provider =
   let rand_float = CCRandom.float 1.0 in
   let rand_int = CCRandom.int Pervasives.max_int in
   fun obs -> 
     let actions = action_provider obs in
     let expectations = List.map (fun a -> (a, Model.exp a model)) in
     let should_optimize = CCRandom.run rand_float <= eps in
     if should_optimize then 
      let (a, exp) = List.fold_right (fun ((best_action, best_exp) as best) ((cur_action, cur_exp) as cur) -> 
         if (cur_exp >= best_exp) then cur else best) 
         expectations 
         (List.hd actions, Reward.min_value)   
     in a 
     else 
       let index = (CCRandom.run rand_int) mod (List.length actions) in
       List.nth actions index
       
end


