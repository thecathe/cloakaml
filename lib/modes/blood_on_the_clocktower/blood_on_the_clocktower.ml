(** @canonical Cloakaml.Modes.BloodOnTheClockTower *)

(** {1 Roles} *)

module TheRoles = The_roles

module Roles =
  Mode.Spec.Roles.Make (TheRoles.Alignment) (TheRoles.Kind) (TheRoles.Role)
    (struct
      open TheRoles

      type role = Role.t
      type kind = Kind.t
      type alignment = Alignment.t

      let alignment_of_kind : kind -> alignment = function
        | Townsfolk | Outsider -> Good
        | Minion | Demon -> Evil
        | _ -> Neutral
      ;;

      let kind_of_role : role -> kind = function
        (* townsfolk *)
        | Washerwoman -> Townsfolk
        | Librarian -> Townsfolk
        | Investigator -> Townsfolk
        | Chef -> Townsfolk
        | Empath -> Townsfolk
        | FortuneTeller -> Townsfolk
        | Undertaker -> Townsfolk
        | Monk -> Townsfolk
        | Ravenkeeper -> Townsfolk
        | Virgin -> Townsfolk
        | Slayer -> Townsfolk
        | Soldier -> Townsfolk
        | Mayor -> Townsfolk
        (* outsiders *)
        | Butler -> Outsider
        | Drunk -> Outsider
        | Recluse -> Outsider
        | Saint -> Outsider
        (* minions *)
        | Poisoner -> Minion
        | Spy -> Minion
        | ScarletWoman -> Minion
        | Baron -> Minion
        (* demons *)
        | Imp -> Demon
      ;;
    end)

module Group = Mode.Spec.Group.Make (Roles)

(** {1 Players} *)

module Index = Mode.Spec.Players.Index.Default

(** TODO: status *)
module Status = Mode.Spec.Players.Status.Make (struct
    type t = Status [@@deriving show { with_path = false }, eq, enum]

    let initial : t = Status
  end)

(** TODO: knowledge *)
module Knowledge = Mode.Spec.Players.Knowledge.Make (struct
    type t = Knowledge [@@deriving show { with_path = false }, eq, enum]

    let initial : t = Knowledge
  end)

module Player =
  Mode.Spec.Players.Player.Make (Index) (Roles) (Status) (Knowledge) (Group)

module ThePlayers = Mode.Spec.Players.Make (Player)

(** {1 Rounds} *)

module TheRounds = The_rounds

(** {1 Abilities} *)

(** {2 Triggers} *)

module TheTriggers = The_triggers

module Triggers =
  Mode.Spec.Abilities.Triggers.Make (Roles) (TheTriggers.Kind)
    (TheTriggers.Trigger)
    (struct
      open TheTriggers

      type role = Roles.Role.t
      type role_kind = Roles.Kind.t
      type role_alignment = Roles.Alignment.t
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
