(** @canonical Modes.BloodOnTheClockTower.TheTriggers.Kind *)

type t =
  | Setup
  | StartOfGame
  | OnPhase
  | Passive
  | OnEvent
  | OneTimeUse
[@@deriving show { with_path = false }, eq, enum]
