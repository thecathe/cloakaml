(** @canonical Modes.BloodOnTheClockTower.TheAbilities *)

module TheRounds = The_rounds
module TheRoles = The_roles
module TheTriggers = The_triggers

(* type f = (TheRounds.Round.t -> unit) *)
type f = (int -> unit)
type t = TheRoles.Role.t * (TheTriggers.Trigger.t * f) list