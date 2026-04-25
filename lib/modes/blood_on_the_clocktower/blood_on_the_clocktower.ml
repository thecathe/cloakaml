(** @canonical Modes.BloodOnTheClockTower *)

module TheRoles = The_roles
module TheTriggers = The_triggers

module Make () = struct
  module Roles = TheRoles.Make ()

  (** TODO: shouldn't take params, use {!Roles} directly (dune library) *)
  module Triggers = TheTriggers.Make (Roles)
end
