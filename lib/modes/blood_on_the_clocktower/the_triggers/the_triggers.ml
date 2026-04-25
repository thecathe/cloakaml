(** @canonical Modes.BloodOnTheClockTower.TheTriggers *)

module Kind = Kind
module Trigger = Trigger

module Make (R : Mode.Spec.Roles.S) :
  Mode.Spec.Abilities.Triggers.S
  with type role = R.Role.t
   and type role_kind = R.Kind.t
   and type role_alignment = R.Alignment.t
   and type trigger = Trigger.t
   and type trigger_kind = Kind.t =
  Mode.Spec.Abilities.Triggers.Make (R) (Kind) (Trigger) (struct

  type role = R.Role.t
  type role_kind = R.Kind.t
  type role_alignment = R.Alignment.t
  type trigger = Trigger.t
  type trigger_kind = Kind.t

  let kind_of_trigger : trigger -> trigger_kind = function
  | _ -> raise Enum_type.EnumKindTodo
  end)
