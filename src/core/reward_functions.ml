open Mas_system

module State_based_reward_fn =
 struct
  type 's t = 's -> Reward.t [@@deriving show]
 end
