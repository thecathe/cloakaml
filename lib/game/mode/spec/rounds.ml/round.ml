(** @canonical Game.Mode.Spec.Rounds.Round *)

type ('phase, 'players) t =
  { phase : 'phase
  ; players : 'players
  }

module type S = sig
  type phase
  type group
  type player
  type players
  type nonrec t = (phase, players) t

  val create : ?phase:phase -> players -> t
  val phase : t -> phase
  val players : t -> players
  val show : t -> string
  val random_player : ?f:(players -> players) -> t -> player
end

module type InputS = sig
  (* type phase *)
  (* type players *)
  (* type t *)

  (* include Enum_map.InputS with type t = (phase, players) t *)

  (* val starting : t *)
end

module Make
    (P : Phase.S)
    (R : Roles.S)
    (G :
       Group.S
       with type role = R.Role.t
        and type role_kind = R.Kind.t
        and type role_alignment = R.Alignment.t)
    (Ps :
       Players.S
       with type role = R.Role.t
        and type role_kind = R.Kind.t
        and type role_alignment = R.Alignment.t
        and type group = G.t)
    (X : InputS (* with type phase = P.t and type players = Ps.t *)) :
  S
  with type phase = P.t
   and type group = G.t
   and type player = Ps.elt
   and type players = Ps.t = struct
  (* include Enum_map.Make (X) *)

  type phase = P.t
  type group = G.t
  type player = Ps.elt
  type players = Ps.t
  type nonrec t = (phase, players) t

  let create ?(phase : phase = P.initial) (players : players) : t =
    { phase; players }
  ;;

  let phase (x : t) : phase = x.phase
  let players (x : t) : players = x.players
  let show (x : t) : string = ""

  let random_player ?(f : (Ps.t -> Ps.t) option) (x : t) : player =
    match f with
    | None -> Ps.random x.players
    | Some f -> Ps.random ~f x.players
  ;;
end
