(** @canonical Cloakaml.Modes.BloodOnTheClockTower.ThePlayers.Status *)

type t =
  | Alive
  | Dead
  | Asleep
  | Poisoned
[@@deriving show { with_path = false }, eq, enum]

let initial : t = Alive
