(** @canonical Cloakaml.Game.Spec.Term *)

(** {1 unused} *)

exception TermTodo

module type S = sig
  type t [@@deriving show { with_path = false }, eq]
end

module type InputS = sig
  type t [@@deriving show { with_path = false }, eq]
end

module Make (X : InputS) : S with type t = X.t = struct
  include X
end
