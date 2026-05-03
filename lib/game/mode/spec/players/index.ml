(** @canonical Cloakaml.Game.Mode.Spec.Players.Index *)

module type S = sig
  type t

  include Set.OrderedType with type t := t
  include Hashtbl.HashedType with type t := t

  val initial : t
  val of_int : int -> t
  val next : t -> t
  val prev : t -> t
  val show : t -> string
end

module type InputS = sig
  type t

  include Set.OrderedType with type t := t
  include Hashtbl.HashedType with type t := t

  val initial : t
  val of_int : int -> t
  val next : t -> t
  val prev : t -> t
  val show : t -> string
end

module Make (X : InputS) : S with type t = X.t = struct
  include X
end
