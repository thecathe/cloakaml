(** @canonical Game.Live.Data *)

type t =
  { initial : Meta.Rounds.initial
  ; current : Meta.Rounds.Round.t
  ; abilities : Meta.Abilities.t
  }

let initial (initial : Meta.Rounds.initial) : t =
  { initial
  ; current = Meta.Rounds.Round.make initial.players
  ; abilities = Meta.Abilities.map_players initial.players
  }
;;

let initial (x : t) : Meta.Rounds.initial = x.initial
let current (x : t) : Meta.Rounds.Round.t = x.current
let players (x : t) : Meta.Players.t = (current x).players
let abilties (x : t) : Meta.Abilities.t = x.abilities
