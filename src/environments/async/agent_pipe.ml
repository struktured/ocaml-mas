open Core.Std
open Async.Std
open Mas_core_async
open Mas_system_async

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

module type Observation_monad = 
  sig
    type ('a, 'b) t =  ('a, 'b) Observation.t Deferred.t
    val map : ('a, 'b) t -> ('a, 'b) Agent.t -> ('b, 'a) t
    val bind : ('a, 'b) t -> ('a, 'b) Agent.t Deferred.t -> ('b, 'a) t
    val return : ('a, 'b) t -> ('a, 'b) Observation.t
  end

module type Observation_monad2 = 
  sig
    type ('a, 'b) t =  ('a, 'b) Observation.t Deferred.t
    val map : ('a, 'b) t -> (('a, 'b) Observation.t -> ('b, 'a) Observation.t) -> ('b, 'a) t
    val bind : ('a, 'b) t -> (('a, 'b) Observation.t -> ('b, 'a) t) -> ('b, 'a) t
    val return : ('a, 'b) t -> ('a, 'b) Observation.t
  end

module type Observation_monad3 = 
  sig
    type ('a, 'b) t =  ('a, 'b) Observation.t Deferred.t
    val map : ('a, 'b) t -> (('a, 'b) Observation.t -> ('b, 'a) Agent.t) -> ('b, 'a) t
    val bind : ('a, 'b) t -> (('a, 'b) Observation.t -> ('b, 'a) Agent.t Deferred.t) -> ('b, 'a) t
    val return : ('a, 'b) t -> ('a, 'b) Observation.t
  end


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

module Agent_monad =
struct
  module type S = 
  sig
    type ('a, 'b) t =  ('a, 'b) Agent.t Deferred.t
    val map : ('a, 'b) t -> (('a, 'b) Observation.t -> ('b, 'a) Agent.t) -> ('b, 'a) t
    val bind : ('a, 'b) t -> (('a, 'b) Observation.t -> ('b, 'a) t) -> ('b, 'a) t
    val return : ('a, 'b) t -> ('a, 'b) Agent.t
    val create : ('a, 'b) Observation.t -> ('a,'b) t
  end
module type S2 = 
sig
  type ('a, 'b) t =  ('a, 'b) Observation.t Deferred.t
  val map : ('a, 'b) t -> (('a, 'b) Observation.t -> ('a, 'b) Agent.t) -> ('b, 'a) t
  val bind : ('a, 'b) t -> (('a, 'b) Observation.t -> ('a, 'b) Agent.t Deferred.t) -> ('b, 'a) t
  val link : ('a, 'b) Agent.t Deferred.t -> (('a, 'b) Observation.t -> ('a,'b) Agent.t Deferred.t)
  val return : ('a, 'b) Observation.t -> ('a, 'b) t
  val init : ?epoch:int -> 'a -> ('a,'b) Agent.t -> ('b, 'a) t

  val (>>=) : ('a, 'b) t -> (('a, 'b) Observation.t ->
    ('a, 'b) Agent.t Deferred.t) -> ('b, 'a) t
  val (>>|) : ('a, 'b) t -> (('a, 'b) Observation.t -> ('a, 'b) Agent.t) -> ('b, 'a) t

  val forever : ?epoch:int -> 'a -> ('a, 'b) Agent.t ->
    (('b,'a) Observation.t -> ('b, 'a) t) -> unit

  val repeat_until_finished: ?epoch:int -> 'a ->
    ('a, 'b) Mas_core_async.Mas_system_async.Agent.t ->
    (('b, 'a) Mas_core_async.Mas_system_async.Observation.t ->
      [ `Finished of 'c
      | `Repeat of ('b, 'a) Observation.t]
      Deferred.t) -> 'c Deferred.t
  end

  module Std : S2 = struct
    type ('a, 'b) t =  ('a, 'b) Observation.t Deferred.t

    let return = Deferred.return

    let init ?(epoch=0) action agent = return Observation.{epoch;action;agent}

    let bind (t:('a, 'b) t) f = t >>=
      fun obs -> f obs >>= fun agent -> Agent.policy agent obs >>|
      fun action -> Observation.{agent;action;epoch=obs.epoch+1}

    let map (t:('a, 'b) t) f = t >>=
      fun obs -> let agent = f obs in Agent.policy agent obs >>|
      fun action -> Observation.{agent;action;epoch=obs.epoch+1}

     let (>>=) = bind
     let (>>|) = map

     let forever ?(epoch=0) (action:'a) agent (f:(('b,'a) Observation.t -> ('b,'a) t)) =
       Deferred.forever Observation.{action;agent;epoch} f

     let repeat_until_finished ?(epoch=0) action agent f =
       Deferred.repeat_until_finished Observation.{action;agent;epoch} f

     let choice = Deferred.choice
     let upon = Deferred.upon
     let any = Deferred.any
     let all = Deferred.all
     let both = Deferred.both
     let choose = Deferred.choose

     let link agent = fun _ -> agent
  end

  module Testing = struct
    open Std
    let test_3_agents_forever action agent1 agent2 agent3 =
       forever action agent1 @@ fun obs ->
       return obs >>=
       link agent2 >>=
       link agent3
    let test_3_agents_repeat_until_finished ?(last_epoch=1000) 
      action agent1 agent2 agent3 =
       repeat_until_finished action agent1 @@ fun obs ->
         if obs.Observation.epoch >= last_epoch then `Finished 0 else
       return obs >>=
       link agent2 >>=
       link agent3

  end

end
