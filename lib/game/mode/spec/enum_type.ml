(** @canonical Cloakaml.Game.Mode.Spec.EnumType *)

(** {1 unused} *)

exception EnumKindTodo

module type S = sig
  type t [@@deriving show { with_path = false }, eq, enum]

  exception EnumOutOfBounds of int

  val of_int : int -> t

  val collect : unit -> t list
end

module type InputS = sig
  type t [@@deriving show { with_path = false }, eq, enum]
end

module Make (X : InputS) : S with type t = X.t = struct
  include X

  exception EnumOutOfBounds of int

  let of_int (n : int) : t =
    match of_enum n with Some x -> x | None -> raise (EnumOutOfBounds n)
  ;;

  let collect () : t list = List.init (max + 1) of_int
end
