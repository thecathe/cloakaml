(** @canonical Cloakaml.Modes.BloodOnTheClockTower.ThePlayers *)

module Id = Mode.Spec.Id.Default
module Status = Mode.Spec.Player.Status.Make (Status)
module Player = Mode.Spec.Player.Make (Id) (The_roles.Build ()) (Status)
module Build () = Mode.Spec.Players.Make (Player)
