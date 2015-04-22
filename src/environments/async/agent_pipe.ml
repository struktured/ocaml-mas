open Core.Std
open Async.Std
open Mas_core_async
open Mas_async_system

module Bidirectional = struct
(*  type ('a, 'b) obs = From_agent of ('a, 'b) Observation.t | To_agent of ('b, 'a) Observation.t [@@deriving show]
  type ('a, 'b) state = {epoch:int; obs: ('a, 'b) obs}

  let init_state state = {epoch=0; obs=From_agent state}*)
  let create (from_agent:('a, 'b) Agent.t) (to_agent : ('b, 'a) Agent.t) init_obs =
    let reader1, writer1 = Pipe.create () in
    let reader2, writer2 = Pipe.create () in
    let open Deferred.Monad_infix in
(*     Pipe.write writer1 init_obs >>= *)
      let rec read agent r w = Pipe.read r >>|
        function
          | `Eof -> `Finished init_obs
          | `Ok obs -> let policy = Agent.policy agent in
         policy obs; `Repeat obs
      in () (* Deferred.repeat_until_finished init_obs (read from_agent reader1 writer2)*)
end

