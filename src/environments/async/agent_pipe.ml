open Core.Std
open Async.Std
open Mas_core_async
module Mas_system_async = Mas_system_async.Make(Deferred)
open Mas_system_async

module Pipe = struct
module type S = sig

type ('a, 'b) pipe = {
  reader1 : 'a Observation.t Pipe.Reader.t;
  writer1 : 'a Observation.t Pipe.Writer.t;
  reader2 : 'b Observation.t Pipe.Reader.t;
  writer2 : 'b Observation.t Pipe.Writer.t}

type ('a, 'b) obs =
  | From_agent of 'b Observation.t
  | From_opponent of 'a Observation.t  (* [@@deriving show] *)
type ('a, 'b) t = {pipe:('a, 'b) pipe; agent: ('a, 'b) Agent.t; opponent: ('b, 'a) Agent.t}

val create : ('a, 'b) Agent.t -> ('b, 'a) Agent.t ->
  ('a, 'b) t

val start : ('a, 'b) t -> ('a,'b) obs ->
    (('a, 'b) Agent.t * ('b, 'a) Agent.t) Deferred.t

end

module Bidirectional : S = struct

type ('a, 'b) pipe = {
  reader1 : 'a Observation.t Pipe.Reader.t;
  writer1 : 'a Observation.t Pipe.Writer.t;
  reader2 : 'b Observation.t Pipe.Reader.t;
  writer2 : 'b Observation.t Pipe.Writer.t}

type ('a, 'b) obs =
  | From_agent of 'b Observation.t
  | From_opponent of 'a Observation.t  (* [@@deriving show] *)

type ('a, 'b) t = {pipe:('a,'b) pipe; agent: ('a, 'b) Agent.t; opponent: ('b,'a) Agent.t}

let read_write r w agent =
      let open Deferred.Monad_infix in
      Pipe.read r >>=
        function
          | `Eof -> Deferred.return(`Finished agent)
          | `Ok obs ->
            (let policy = Agent.policy agent in policy obs >>=
             fun action -> Pipe.write w action >>|
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
      | From_opponent from_opp-> Pipe.write t.pipe.writer1 from_opp)
    >>= fun () ->
    let f1 = read_write t.pipe.reader1 t.pipe.writer2 in
    let f2 = read_write t.pipe.reader2 t.pipe.writer1 in
    Deferred.both
      (Deferred.repeat_until_finished t.agent f1)
      (Deferred.repeat_until_finished t.opponent f2)
end

end

module Monad =
  struct
module type S = 
sig
  
  type 'a t =  'a Observation.t Deferred.t
  val map : 'a t -> ('a Observation.t -> ('a, 'b) Agent.t) -> 'b t
  val bind : 'a t -> ('a Observation.t -> ('a, 'b) Agent.t Deferred.t) -> 'b t
  val bind_always : ('a, 'b) Agent.t Deferred.t -> ('a Observation.t -> ('a,'b) Agent.t Deferred.t)
  val map_always : ('a, 'b) Agent.t -> ('a Observation.t -> ('a, 'b) Agent.t Deferred.t)
  val return : 'a Observation.t -> 'a t
  
  val (>>=) : 'a t -> ('a Observation.t ->
    ('a, 'b) Agent.t Deferred.t) -> 'b t
  val (>>|) : 'a t -> ('a Observation.t -> ('a, 'b) Agent.t) -> 'b t
  val (>|=) : 'a t -> ('a Observation.t -> ('a, 'b) Agent.t) -> 'b t
  val (=>>) : 'a t -> ('a, 'b) Agent.t Deferred.t -> 'b t
  val (|>>) : 'a t -> ('a, 'b) Agent.t -> 'b t

  val forever : 'a Observation.t ->
    ('a Observation.t -> 'a t) -> unit

  val repeat_until_finished: 'a Observation.t ->
    ('a Observation.t ->
      [ `Finished of 'c
      | `Repeat of 'a Observation.t]
      Deferred.t) -> 'c Deferred.t

  val ignore : 'a t -> unit Deferred.t

  end

  module Deferred : S = struct
    type 'a t =  'a Observation.t Deferred.t

    let return = Deferred.return

    let bind t f = t >>=
      fun obs -> f obs >>= 
      fun agent -> Agent.policy agent obs >>=
      fun action -> Agent.reward_function agent obs >>=
      fun reward -> Agent.value_function agent |> 
      fun value_function -> Deferred.ignore @@ 
        Value_function.update value_function ~action obs reward >>|
        fun () -> action

    let map t f = bind t @@ fun obs -> return @@ f obs
    let bind_always agent = fun _ -> agent
    let map_always agent = fun _ -> Deferred.return agent

    let (>>=) = bind
    let (>>|) = map
    let (>|=) = map

    let (=>>) t agent = t >>= bind_always agent
    let (|>>) t agent = t >>= map_always agent

     let forever init_obs f =
       Deferred.forever init_obs f

     let repeat_until_finished init_obs f =
       Deferred.repeat_until_finished init_obs f
(*
    let fold ?(epoch=0) action agent init f =
       Deferred.repeat_until_finished Observation.{action;agent;epoch} f
*)
(*
    let scan ?(epoch=0) action agent init f =
       Deferred.repeat_until_finished Observation.{action;agent;epoch} f
*)
     let choice = Deferred.choice
     let upon = Deferred.upon
     let any = Deferred.any
     let all = Deferred.all
     let both = Deferred.both
     let choose = Deferred.choose
     let ignore = Deferred.ignore
  end

  module Testing = struct
    open Deferred
    let test_3_agents_forever action agent1 agent2 agent3 =
      forever Observation.{epoch=0;action;agent=agent1} @@ fun obs ->
       return obs |>> agent2 |>> agent3
    let test_3_agents_repeat_until_finished ?(last_epoch=1000) 
      action agent1 agent2 agent3 =
        repeat_until_finished Observation.{action;agent=agent1;epoch=0} @@ 
        fun obs -> Async.Std.Deferred.return @@
          if obs.Observation.epoch >= last_epoch then `Finished obs else
          (*return obs |>> agent2 |>> agent3 *) `Repeat obs

  end

end
