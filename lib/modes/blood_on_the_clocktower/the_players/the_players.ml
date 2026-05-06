(** @canonical Cloakaml.Modes.BloodOnTheClockTower.ThePlayers *)

module Index = Mode.Spec.Index.Default
module Status = Mode.Spec.Player.Status.Make (Status)
module Player = Mode.Spec.Player.Make (Index) (The_roles.Build ()) (Status)
module Build () = Mode.Spec.Players.Make (Player)
