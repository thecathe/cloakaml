(** @canonical Modes.BloodOnTheClockTower.TheRounds *)

module Phase = Spec.Rounds.Round.Data.Phase.Make (Phase)
module Data = Spec.Rounds.Round.Data.Make (Phase) (The_players.Build ())
module Round = Spec.Rounds.Round.Make (Data)
module Build () = Spec.Rounds.Make (Round)
