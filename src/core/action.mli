(** Defines the minimum requirements for an action,
 * which that it is showable and comparable *)
module type S = sig type t [@@deriving show, ord] end
