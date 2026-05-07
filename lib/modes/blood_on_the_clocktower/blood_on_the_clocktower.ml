(** @canonical Cloakaml.Modes.BloodOnTheClockTower *)

module TheRoles = The_roles
module ThePlayers = The_players
module TheRounds = The_rounds
module TheTriggers = The_triggers

(** {1 Mode: Blood on the Clocktower} *)

module Build () = struct
  module Roles = TheRoles.Build ()
  module Players = ThePlayers.Build ()
  module Rounds = TheRounds.Build ()
  module Triggers = TheTriggers.Build ()
end

(*******************)

(** {1 Sketch of Abilities} *)

include Build ()
