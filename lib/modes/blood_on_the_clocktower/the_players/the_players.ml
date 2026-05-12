(** @canonical Cloakaml.Modes.BloodOnTheClockTower.ThePlayers *)

module Id = Spec.Id.Default
module Status = Spec.Players.Player.Status.Make (Status)
module Player = Spec.Players.Player.Make (Id) (The_roles.Build ()) (Status)
module Build () = Spec.Players.Make (Player)
