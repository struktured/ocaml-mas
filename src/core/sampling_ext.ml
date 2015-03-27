module Random = CCRandom

let from_weights ?rand weights =
  let thres = Random.run (CCOpt.get_lazy (fun () -> Random.float 1.0) rand) in
  let sum = CCArray.fold (+.) 0. weights in
  let normalized = CCArray.map (fun w -> w /. sum) weights in
  let (_:float), index = CCArray.fold_while (fun (acc, i) w ->
    let acc = acc +. w in
    if (i+1) < Array.length normalized && acc < thres
      then (acc, i+1), `Continue
      else (acc, i), `Stop) (0., 0) normalized
  in index
