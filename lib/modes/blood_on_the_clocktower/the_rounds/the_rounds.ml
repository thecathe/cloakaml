(** @canonical Modes.BloodOnTheClockTower.TheRounds *)

module Phase = Mode.Spec.Round.Data.Phase.Make (Phase)
module Data = Mode.Spec.Round.Data.Make (Phase) (The_players.Build ())
module Round = Mode.Spec.Round.Make (Data)
module Build () = Mode.Spec.Rounds.Make (Round)
