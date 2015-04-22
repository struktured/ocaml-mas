open Core.Std
open Async.Std
open Mas_core_async
open Mas_async_system

module type S = sig

type ('a, 'b) obs =
  | From_agent of ('b, 'a) Observation.t
  | From_opponent of ('a, 'b) Observation.t  (* [@@deriving show] *)
val create : ('a, 'b) Agent.t -> ('b, 'a) Agent.t -> ('a,'b) obs 
    -> ('a, 'b) Agent.t * ('b, 'a) Agent.t Deferred.t
 
end


module Bidirectional : S = struct

type ('a, 'b) obs =
  | From_agent of ('b, 'a) Observation.t
  | From_opponent of ('a, 'b) Observation.t  (* [@@deriving show] *)

let read_write r w agent = 
      let open Deferred.Monad_infix in 
      Pipe.read r >>=
        function
          | `Eof -> Deferred.return(`Finished agent)
          | `Ok obs ->
            (let policy = Agent.policy agent in policy obs >>=
              fun obs' -> Pipe.write w obs' >>| fun () -> `Repeat agent)

let create (from_agent:('a, 'b) Agent.t)
    (from_opp  : ('b, 'a) Agent.t) (init_obs:('a,'b) obs) =
    let (reader1 : ('b, 'a) Observation.t Pipe.Reader.t),
        (writer1 : ('b, 'a) Observation.t Pipe.Writer.t) = Pipe.create () in
    let (reader2 : ('a, 'b) Observation.t Pipe.Reader.t),
        (writer2 : ('a, 'b) Observation.t Pipe.Writer.t) = Pipe.create () in
    let open Deferred.Monad_infix in 
    (match init_obs with
      | From_agent from_obs -> Pipe.write writer1 from_obs
      | From_opponent to_obs -> Pipe.write writer2 to_obs)
    >>= fun () ->
    let f1 = read_write reader1 writer2 in
    let f2 = read_write reader2 writer1 in
    Deferred.both
      (Deferred.repeat_until_finished from_agent f1)
      (Deferred.repeat_until_finished from_opp f2)
end
