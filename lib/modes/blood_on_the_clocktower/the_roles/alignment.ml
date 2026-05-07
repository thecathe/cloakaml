(** @canonical Modes.BloodOnTheClockTower.TheRoles.Alignment *)

  type t =
    | Good
    | Evil
    | Neutral
  [@@deriving show { with_path = false }, eq, enum]