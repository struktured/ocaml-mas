(**
  A state base policy is a function which given states 
  of type ['s], it returns actions of type ['a] *)
type ('s, 'a) t = 's -> 'a [@@deriving show]


