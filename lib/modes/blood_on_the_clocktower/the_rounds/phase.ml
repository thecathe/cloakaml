(** @canonical Modes.BloodOnTheClockTower.TheRounds.Phase *)

type t =
  | Day
  | Night
[@@deriving show { with_path = false }, enum, eq]

let initial : t = Day
