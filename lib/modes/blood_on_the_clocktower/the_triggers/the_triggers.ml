(** @canonical Modes.BloodOnTheClockTower.TheTriggers *)

module Kind = Mode.Spec.Abilities.Triggers.Kind.Make (Kind)

module Trigger =
  Mode.Spec.Abilities.Triggers.Trigger.Make
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
  Mode.Spec.Abilities.Triggers.Make (The_roles.Build ()) (Trigger)
