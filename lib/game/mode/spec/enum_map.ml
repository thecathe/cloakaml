(** @canonical Game.Mode.Spec.EnumMap *)

module type S = sig
include Enum_type.S

  val hash : t -> int
  val compare : t -> t -> int

  module Map : Hashtbl.S with type key = t
end

module type InputS = sig
  type t [@@deriving show { with_path = false }, eq, enum]
end

module Make (X : InputS) : S with type t = X.t = struct
  include X

  let hash (x : t) : int = Int.hash (to_enum x)
  let compare (a : t) (b : t) : int = Int.compare (to_enum a) (to_enum b)

  module Map : Hashtbl.S with type key = t = Hashtbl.Make (struct
      type t = X.t

      let hash (x : t) : int = hash x
      let equal (a : t) (b : t) : bool = equal a b
    end)
end
