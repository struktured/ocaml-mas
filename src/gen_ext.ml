let fold_map f s g =
  Gen.map (let state = ref s in fun x -> state := f (!state) x; !state) g
