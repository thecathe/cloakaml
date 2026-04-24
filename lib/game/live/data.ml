(** @canonical Game.Live.Data *)

type t =
  { initial : Build.Rounds.initial
  ; current : Build.Rounds.Round.t
  ; abilities : Build.Abilities.t
  }

let create (initial : Build.Rounds.initial) : t =
  { initial
  ; current = Build.Rounds.Round.make initial.players
  ; abilities = Build.Abilities.map_players initial.players
  }
;;

let initial (x : t) : Build.Rounds.initial = x.initial
let current (x : t) : Build.Rounds.Round.t = x.current
let players (x : t) : Build.Players.t = (current x).players
let abilties (x : t) : Build.Abilities.t = x.abilities
