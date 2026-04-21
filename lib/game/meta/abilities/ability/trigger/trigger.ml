(** @canonical Game.Meta.Abilities.Ability.Trigger *)

type t =
  | Setup
  | PerceivedAs
  | StartOfGame
  | EachNight
  | EachDay
  | OnDeath
  | OnNomination
  | Action
  | AlwaysActive
  | EndDay
  | DemonDies
[@@deriving show { with_path = false }, eq, enum]

let hash x = Int.hash (to_enum x)
let compare (a : t) (b : t) : int = Int.compare (to_enum a) (to_enum b)

(** {1 Kind} *)

module Kind = Kind

let kind : t -> Kind.t = function
  | Setup -> Setup
  | StartOfGame -> StartOfGame
  | PerceivedAs | AlwaysActive -> Passive
  | EachNight | EachDay -> PhaseDependant
  | OnDeath | OnNomination -> OnEvent
  | Action -> OneTimeUse
  | EndDay | DemonDies -> Conditional
;;

let is_kind (a : Kind.t) (b : t) : bool = kind b |> Kind.equal a
