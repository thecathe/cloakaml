(** @canonical Cloakaml.Modes.BloodOnTheClockTower *)

(** {1 Roles} *)

module TheRoles = The_roles
module Roles = TheRoles.Build ()

(** {1 Players} *)

module ThePlayers = The_players
module Players = ThePlayers.Build ()

(** {1 Rounds} *)

module TheRounds = The_rounds
module Rounds = TheRounds.Build ()

(** {1 Abilities} *)

(** {2 Triggers} *)

module TheTriggers = The_triggers

module Triggers =
  Mode.Spec.Abilities.Triggers.Make (Roles) (TheTriggers.Kind)
    (TheTriggers.Trigger)
    (struct
      open TheTriggers

      type role = Roles.role
      type role_kind = Roles.role_kind
      type role_alignment = Roles.role_alignment
      type trigger = Trigger.t
      type trigger_kind = Kind.t

      let kind_of_trigger : trigger -> trigger_kind = function
        | _ -> raise Enum_type.EnumKindTodo
      ;;
    end)

(** {1 Mode: Blood on the Clocktower} *)

module Make () = struct
  module Roles = Roles
  module Triggers = Triggers
end
