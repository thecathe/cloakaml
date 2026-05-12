(** @canonical Cloakaml.Game.Spec.Id *)

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

module Default : S with type t = Int.t = Make (struct
    include Int

    let initial : t = 0
    let of_int (x : int) : t = x
    let next (x : t) : t = x + 1
    let prev (x : t) : t = x - 1
    let show (x : t) : string = Printf.sprintf "%i" x
  end)
