open Core.Std
open Async.Std
open Mas_core_async
open Mas_async_system

module type S = sig

type ('a, 'b) pipe = {
  reader1 : ('a, 'b) Observation.t Pipe.Reader.t;
  writer1 : ('a, 'b) Observation.t Pipe.Writer.t;
  reader2 : ('b, 'a) Observation.t Pipe.Reader.t;
  writer2 : ('b, 'a) Observation.t Pipe.Writer.t}

type ('a, 'b) obs =
  | From_agent of ('b, 'a) Observation.t
  | From_opponent of ('a, 'b) Observation.t  (* [@@deriving show] *)

type ('a, 'b) t = {pipe:('a, 'b) pipe; agent: ('a, 'b) Agent.t; opponent: ('b, 'a) Agent.t}

val create : ('a, 'b) Agent.t -> ('b, 'a) Agent.t
    -> ('a, 'b) t

val start : ('a, 'b) t -> ('a,'b) obs -> (('a, 'b) Agent.t * ('b, 'a) Agent.t) Deferred.t

end

module Bidirectional : S = struct

type ('a, 'b) pipe = {
  reader1 : ('a, 'b) Observation.t Pipe.Reader.t;
  writer1 : ('a, 'b) Observation.t Pipe.Writer.t;
  reader2 : ('b, 'a) Observation.t Pipe.Reader.t;
  writer2 : ('b, 'a) Observation.t Pipe.Writer.t}

type ('a, 'b) obs =
  | From_agent of ('b, 'a) Observation.t
  | From_opponent of ('a, 'b) Observation.t  (* [@@deriving show] *)

type ('a, 'b) t = {pipe:('a,'b) pipe; agent: ('a, 'b) Agent.t; opponent: ('b,'a) Agent.t}

let read_write r w agent =
      let open Deferred.Monad_infix in
      Pipe.read r >>=
        function
          | `Eof -> Deferred.return(`Finished agent)
          | `Ok obs ->
            (let policy = Agent.policy agent in policy obs >>=
             fun action -> Pipe.write w Observation.{agent;action;epoch=obs.epoch+1} >>|
             fun () -> `Repeat agent)

let create (from_agent:('a, 'b) Agent.t)
    (from_opp  : ('b, 'a) Agent.t) =
  let (reader1, writer1), (reader2,writer2) = Pipe.create(), Pipe.create() in
  let pipe = {reader1;writer1;reader2;writer2} in
  {pipe;agent=from_agent;opponent=from_opp}

let start t init_obs = 
    let open Deferred.Monad_infix in 
    (match init_obs with
      | From_agent from_agent -> Pipe.write t.pipe.writer2 from_agent
      | From_opponent from_opp -> Pipe.write t.pipe.writer1 from_opp)
    >>= fun () ->
    let f1 = read_write t.pipe.reader1 t.pipe.writer2 in
    let f2 = read_write t.pipe.reader2 t.pipe.writer1 in
    Deferred.both
      (Deferred.repeat_until_finished t.agent f1)
      (Deferred.repeat_until_finished t.opponent f2)
end
