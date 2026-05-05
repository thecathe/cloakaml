(** @canonical Game.Mode.Spec.Rounds.Round *)

type 'data t =
  { this : 'data
  ; prev : 'data t option
  }

module type S = sig
  type phase
  type data
  type group
  type player
  type players
  type nonrec t = data t

  val create : ?prev:t -> ?phase:phase -> players -> t
  val data : t -> data
  val next : t -> t
  val prev : t -> t
  val index : t -> int
  val phase : t -> phase
  val players : t -> players
  val show : t -> string
  val is_phase : phase -> t -> bool
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
    (D :
       Data.S
       with type phase = P.t
        and type players = Ps.t
        and type rolemap = bool R.Role.Map.t)
    (X : InputS (* with type phase = P.t and type players = Ps.t *)) :
  S
  with type phase = P.t
   and type data = D.t
   and type group = G.t
   and type player = Ps.elt
   and type players = Ps.t
   and type t = D.t t = struct
  (* include Enum_map.Make (X) *)

  type phase = P.t
  type data = D.t
  type group = G.t
  type player = Ps.elt
  type players = Ps.t
  type nonrec t = data t

  let create
        ?(prev : t option)
        ?(phase : phase =
          match prev with None -> P.initial | Some x -> P.next x.this.phase)
        (players : players)
    : t
    =
    { this = D.create ~phase players; prev }
  ;;

  let data (x : t) : data = x.this
  let next (x : t) : t = { this = data x |> D.next ; prev = Some x }

  exception NoPrevRound

  let prev : t -> t = function
    | { prev = None; _ } -> raise NoPrevRound
    | { prev = Some x; _ } -> x
  ;;

  let rec index : t -> int = function
    | { prev = None; _ } -> 1
    | { prev = Some x; _ } -> 1 + index x
  ;;

  let phase (x : t) : phase = index x |> P.of_int
  let players (x : t) : players = x.this.players
  let show (x : t) : string = "TODO: show"
  let is_phase (a : phase) (x : t) : bool = phase x |> P.equal a

  let random_player ?(f : (Ps.t -> Ps.t) option) (x : t) : player =
    match f with
    | None -> Ps.random x.this.players
    | Some f -> Ps.random ~f x.this.players
  ;;
end
