(** @canonical Game.Mode.Spec.Rounds.Data *)

type ('phase, 'players) t =
  { phase : 'phase
  ; players : 'players
  }

type ('phase, 'players, 'rolemap) initial =
  { starting_phase : 'phase
  ; players : 'players
  ; rolemap : 'rolemap
  }

module type S = sig
  type phase
  type players
  type rolemap
  type nonrec t = (phase, players) t
  type nonrec initial = (phase, players, rolemap) initial

  val initial
    :  ?starting_phase:phase
    -> players
    -> ?rolemap:rolemap
    -> unit
    -> initial

  val create : ?phase:phase -> players -> t
  val starting_phase : initial -> phase
  val rolemap : initial -> rolemap
  val phase : t -> phase
  val players : t -> players
end

module type InputS = sig
  (* type phase
     type players
     type rolemap

     val starting_phase : phase *)
end

module Make
    (P : Phase.S)
    (R : Roles.S)
    (Pl :
       Players.Player.S
       with type role = R.Role.t
        and type role_kind = R.Kind.t
        and type role_alignment = R.Alignment.t)
    (Ps :
       Players.S
       with type role = R.Role.t
        and type role_kind = R.Kind.t
        and type role_alignment = R.Alignment.t
       with type player = Pl.t
        and type elt = Pl.t)
    (X :
       InputS
       (* with type phase = P.t
          and type players = Ps.t
          and type rolemap = bool R.Role.Map.t *)) :
  S
  with type phase = P.t
   and type players = Ps.t
   and type rolemap = bool R.Role.Map.t = struct
  type phase = P.t
  type players = Ps.t
  type rolemap = bool R.Role.Map.t
  type nonrec t = (phase, players) t
  type nonrec initial = (phase, players, rolemap) initial

  let init_rolemap (players : players) : rolemap =
    let xs : Ps.elt list = Ps.to_list players in
    List.init (Ps.cardinal players) (fun n -> List.nth xs n |> Pl.role, false)
    |> List.to_seq
    |> R.Role.Map.of_seq
  ;;

  let initial
        ?(starting_phase : phase = P.initial)
        (players : players)
        ?(rolemap : rolemap = init_rolemap players)
        ()
    : initial
    =
    { starting_phase; players; rolemap }
  ;;

  let create ?(phase : phase = P.initial) (players : players) : t =
    { phase; players }
  ;;

  let starting_phase (x : initial) : phase = x.starting_phase
  let rolemap (x : initial) : rolemap = x.rolemap
  let phase (x : t) : phase = x.phase
  let players (x : t) : players = x.players
end
