(** @canonical Cloakaml.Modes.BloodOnTheClockTower.ThePlayers.Status *)

type t =
  | Alive
  | Dead
  | Alive_And of extra
[@@deriving show { with_path = false }, eq]

and extra =
  | Asleep
  | Nominated
  | Poisoned
  | Both of (extra * extra)
[@@deriving show { with_path = false }, eq]

let initial : t = Alive
