(** @canonical Game.Mode.Spec.Rounds.Data *)

type ('phase, 'players, 'rolemap) t =
  { starting_phase : 'phase
  ; players : 'players
  ; rolemap : 'rolemap
  }

module type S = sig
  type phase
  type players
  type rolemap
  type nonrec t = (phase, players, rolemap) t

  val create : phase -> players -> rolemap -> t
end

module type InputS = sig
  type phase
  type players
  type rolemap

  val starting_phase : phase
  val players : players
  val rolemap : rolemap
end

module Make
    (P : Phase.S)
    (R : Roles.S)
    (Ps :
       Players.S
       with type role = R.Role.t
        and type role_kind = R.Kind.t
        and type role_alignment = R.Alignment.t)
    (X :
       InputS
       with type phase = P.t
        and type players = Ps.t
        and type rolemap = bool R.Role.Map.t) :
  S
  with type phase = P.t
   and type players = Ps.t
   and type rolemap = bool R.Role.Map.t = struct
  type phase = P.t
  type players = Ps.t
  type rolemap = bool R.Role.Map.t
  type nonrec t = (phase, players, rolemap) t

  let create (starting_phase : phase) (players : players) (rolemap : rolemap)
    : t
    =
    { starting_phase; players; rolemap }
  ;;
end
