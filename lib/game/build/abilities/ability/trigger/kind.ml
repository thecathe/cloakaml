(** @canonical Game.Build.Abilities.Ability.Trigger.Kind *)

type t =
  | Setup
  | StartOfGame
  | PhaseDependant
  | Passive
  | OnEvent
  | OneTimeUse
  | Conditional
[@@deriving show { with_path = false }, eq]
