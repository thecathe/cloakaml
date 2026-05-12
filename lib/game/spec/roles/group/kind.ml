(** @canonical Cloakaml.Game.Spec.Role.Group.Kind *)

type t =
  | Role
  | Kind
  | Alignment
[@@deriving show { with_path = false }, eq, enum]

include (
  Enum_map.Make (struct
    type nonrec t = t

    let pp = pp
    let show = show
    let equal = equal
    let of_enum = of_enum
    let to_enum = to_enum
    let min = min
    let max = max
  end) :
    Enum_map.S with type t := t)
