(** @canonical Modes.BloodOnTheClockTower.TheTriggers.Trigger *)

type t =
  | Setup
  | PerceivedAs
  | StartOfGame
  | EachNight
  | EachDay
  | OnDeath
  | OnNomination
  | Action
  | AlwaysActive
  | EndDay
  | DemonDies
[@@deriving show { with_path = false }, eq, enum]