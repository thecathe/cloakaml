(** @canonical Modes.BloodOnTheClockTower.Alignment *)

  type t =
    | Good
    | Evil
    | Neutral
  [@@deriving show { with_path = false }, eq, enum]