(** @canonical Modes.BloodOnTheClockTower.TheTriggers *)

module Kind = Spec.Abilities.Ability.Triggers.Trigger.Kind.Make (Kind)

module Trigger =
  Spec.Abilities.Ability.Triggers.Trigger.Make
    (Kind)
    (struct
      include Trigger

      type kind = Kind.t

      let kind : t -> kind = function
        | Setup -> Setup
        | StartOfGame -> StartOfGame
        | AlwaysActive -> Passive
        | EachNight | EachDay | EndDay -> OnPhase
        | PerceivedAs | OnDeath | OnNomination | DemonDies -> OnEvent
        | Action -> OneTimeUse
      ;;
    end)

module Build () =
  Spec.Abilities.Ability.Triggers.Make (The_roles.Build ()) (Trigger)
