(** @canonical Cloakaml.Modes.BloodOnTheClockTower *)

(** {1 Roles} *)

module TheRoles = The_roles

module Alignment :
  Mode.Spec.Role.Alignment.S with type t = TheRoles.Alignment.t =
  Mode.Spec.Role.Alignment.Make (TheRoles.Alignment)

module Kind :
  Mode.Spec.Role.Kind.S
  with type t = TheRoles.Kind.t
   and module Alignment = Alignment
   and type alignment = Alignment.t =
  Mode.Spec.Role.Kind.Make
    (Alignment)
    (struct
      include TheRoles.Kind

      type alignment = Alignment.t

      let alignment : t -> alignment = function
        | Townsfolk | Outsider -> Good
        | Minion | Demon -> Evil
        | _ -> Neutral
      ;;
    end)

module Role :
  Mode.Spec.Role.S
  with type t = TheRoles.Role.t
   and module Kind = Kind
   and type kind = Kind.t =
  Mode.Spec.Role.Make
    (Kind)
    (struct
      include TheRoles.Role

      type kind = Kind.t

      let kind : t -> kind = function
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

module Roles = Mode.Spec.Roles.Make (Role)
module Group = Roles.Group

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
  Mode.Spec.Players.Player.Make (Index) (Roles) (Status) (Knowledge)

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
