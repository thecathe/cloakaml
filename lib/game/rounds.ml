(** linked-list into previous rounds *)
type t =
  { this : Round.t
  ; prev : t option
  }
