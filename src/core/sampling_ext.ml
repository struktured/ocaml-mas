module Random = CCRandom

let from_weights ?rand weights =
  let sum = CCArray.fold (+.) 0. weights in
  let normalized = CCArray.map (fun w -> w /. sum) weights in
  Oml.Statistics.Sampling.multinomial normalized
