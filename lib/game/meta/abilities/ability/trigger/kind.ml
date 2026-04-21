(** @canonical Game.Meta.Abilities.Ability.Trigger.Kind *)

type t =
  | Setup
  | StartOfGame
  | PhaseDependant
  | Passive
  | OnEvent
  | OneTimeUse
  | Conditional
[@@deriving show { with_path = false }, eq]
