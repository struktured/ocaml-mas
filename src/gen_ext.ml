(** Various extensions to the [gen] package *)
open Gen.Infix

let fold_map = Gen.fold_map 

let side_effect_iter f = Gen.map (fun x -> let () = f x in x)
(** Calls [f x : unit] on each element, possibly with side effects. 
   Useful for hooking in IO operations and otherwise during a 
   generator's life cycle. *)

let fold_tuple (f:'a -> 'b -> 'a) (acc:'a) g = 
  let g = Gen.fold_map
  (fun ((e:'b option), (acc:'a)) (e':'b) -> 
    let acc = f acc e' in Some e',acc) (None, acc) g in
    Gen.drop 1 g >>| fun (opt, acc) -> CCOpt.get_exn opt, acc
(** Performs a fold [f acc x] on each element but 
    the [acc] value is computed purely as a side effect in a tuple *)

let ignore_fold_tuple f acc g = 
  fold_tuple f acc g >>| fun (e, acc) -> e
(** Ignores the accumulated value but retains the original generator *)

let drop_first g = Gen.map (fun (a, b) -> b) g
let drop_second g = Gen.map (fun (a, b) -> a) g

let consume_first (f:'a * 'b -> unit) g = Gen.map (fun (a, b) -> f (a,b); b) g
let consume_second (f:'a * 'b -> unit) g = Gen.map (fun (a, b) -> f (a,b); a) g

