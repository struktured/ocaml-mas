open Core.Std
open Async.Std
open Mas_core_async
open Mas_async_system

module Bidirectional = struct
type ('a, 'b) obs = 
  | From_agent of ('b, 'a) Observation.t 
  | To_agent of ('a,'b) Observation.t (** [@@deriving show] *)

let create (from_agent:('b, 'a) Agent.t)
    (to_agent : ('a, 'b) Agent.t) (init_obs:('a,'b) obs) 
     (*:('a, 'b) Agent.t * ('b, 'a) Agent.t Deferred.t *) =
    let reader1, writer1 = Pipe.create () in
    let reader2, writer2 = Pipe.create () in
    let open Deferred.Monad_infix in
    let read_write r w agent : [`Repeat of ('c,'d) Agent.t | `Finished of
                                  ('d,'c) Agent.t] Deferred.t
      = Pipe.read r >>|
        function
          | `Eof -> `Finished agent
          | `Ok obs -> `Repeat agent 
(*            (let policy = Agent.policy agent in policy obs >>=
              fun obs' -> Pipe.write w obs' >>| fun () -> `Repeat agent) *)
    in
    (match init_obs with
      | From_agent from_obs -> Pipe.write writer2 from_obs
      | To_agent to_obs -> Pipe.write writer1 to_obs)
    (*>>= (fun () -> Deferred.both
      (Deferred.repeat_until_finished from_agent (read_write reader1 writer2))
      (Deferred.repeat_until_finished to_agent (read_write reader2 writer1))) *)
end
