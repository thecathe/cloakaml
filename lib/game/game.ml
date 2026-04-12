module Roles = struct
  type t =
    (* townsfolk *)
    | Washerwoman
    | Librarian
    | Investigator
    | Chef
    | Empath
    | FortuneTeller
    | Undertaker
    | Monk
    | Ravenkeeper
    | Virgin
    | Slayer
    | Soldier
    | Mayor
    (* outsiders *)
    | Butler
    | Drunk
    | Recluse
    | Saint
    (* minions *)
    | Poisoner
    | Spy
    | ScarletWoman
    | Baron
    (* demons *)
    | Imp
  [@@deriving show { with_path = false }]

  type kind =
    | Townsfolk
    | Outsider
    | Minion
    | Demon
  [@@deriving show { with_path = false }]

  let kind : t -> kind = function
    (* townsfolk *)
    | Washerwoman -> Townsfolk
    | Librarian -> Townsfolk
    | Investigator -> Townsfolk
    | Chef -> Townsfolk
    | Empath -> Townsfolk
    | FortuneTeller -> Townsfolk
    | Undertaker -> Townsfolk
    | Monk -> Townsfolk
    | Ravenkeeper -> Townsfolk
    | Virgin -> Townsfolk
    | Slayer -> Townsfolk
    | Soldier -> Townsfolk
    | Mayor -> Townsfolk
    (* outsiders *)
    | Butler -> Outsider
    | Drunk -> Outsider
    | Recluse -> Outsider
    | Saint -> Outsider
    (* minions *)
    | Poisoner -> Minion
    | Spy -> Minion
    | ScarletWoman -> Minion
    | Baron -> Minion
    (* demons *)
    | Imp -> Demon
  ;;

  let is_townfolk (x : t) : bool =
    match kind x with Townsfolk -> true | _ -> false
  ;;

  let is_outsider (x : t) : bool =
    match kind x with Outsider -> true | _ -> false
  ;;

  let is_minion (x : t) : bool = match kind x with Minion -> true | _ -> false
  let is_demon (x : t) : bool = match kind x with Demon -> true | _ -> false

  (* *)
  let is_good (x : t) : bool = is_townfolk x || is_outsider x
  let is_evil (x : t) : bool = is_minion x || is_demon x

  type alignment =
    | Good
    | Evil
  [@@deriving show { with_path = false }]

  exception CannotDetermineAlignment of t

  let alignment (x : t) : alignment =
    if is_good x
    then Good
    else if is_evil x
    then Evil
    else raise (CannotDetermineAlignment x)
  ;;

  exception CantDetermineAllies of (t * t)

  let allied (a : t) (b : t) : bool =
    match alignment a, alignment b with
    | Good, Good -> true
    | Evil, Evil -> true
    | _, _ -> false
  ;;

  let opposed (a : t) (b : t) : bool = allied a b |> Bool.not
end

module type Player_Type = sig
  type t =
    { index : int
    ; role : Roles.t
    ; mutable alive : bool
    }
  [@@deriving show]

  val create : int -> Roles.t -> t
  val alive : t -> bool
  val dead : t -> bool
  val allied : t -> t -> bool
  val opposed : t -> t -> bool
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module Player : Player_Type = struct
  type t =
    { index : int
    ; role : Roles.t
    ; mutable alive : bool
    }
  [@@deriving show { with_path = false }]

  let create (index : int) (role : Roles.t) : t = { index; role; alive = true }
  let alive (x : t) : bool = x.alive
  let dead (x : t) : bool = Bool.not (alive x)
  let allied (a : t) (b : t) : bool = Roles.allied a.role b.role
  let opposed (a : t) (b : t) : bool = Roles.opposed a.role b.role
  let compare (a : t) (b : t) : int = Int.compare a.index b.index
  let equal (a : t) (b : t) : bool = Int.equal a.index b.index
end

module type Players_Type = sig
  type t = Set.Make(Player).t
  type elt = Player.t

  val add : elt -> t -> t
  val min : t -> elt

  exception TooFewPlayers

  val create : elt list -> t
  val alive : t -> t
  val dead : t -> t
  val allied : elt -> t -> t
  val opposed : elt -> t -> t

  type neighbours =
    { left : Player.t
    ; right : Player.t
    }
  [@@deriving show]

  (** [neighbours x ys] returns the neighbours of [x] in [ys], i.e., those indexed either side of [x]. {b Note:} requires that [x] be in [ys]. {b Note:} if there is only one valid neighbour, then both {!left} and {!right} will refer to the same {!Player.t}. {b Note:} raises [NoNeighbour] in the event that [x] would be it's own neighbours.
  *)
  val neighbours : elt -> t -> neighbours

  val allied_neighbours : elt -> t -> neighbours
  val opposed_neighbours : elt -> t -> neighbours
  val alive_neighbours : elt -> t -> neighbours
  val dead_neighbours : elt -> t -> neighbours
end

module MakePlayers (Args : sig
    val minimum_num_players : int
  end) : Players_Type = struct
  (**  *)

  module Players = Set.Make (Player)

  let add = Players.add
  let min = Players.min_elt

  exception TooFewPlayers

  let create (xs : Player.t list) : Players.t =
    if List.length xs < Args.minimum_num_players
    then raise TooFewPlayers
    else Players.of_list xs
  ;;

  let alive : Players.t -> Players.t = Players.filter Player.alive
  let dead : Players.t -> Players.t = Players.filter Player.dead

  let allied (x : Player.t) : Players.t -> Players.t =
    Players.filter (Player.allied x)
  ;;

  let opposed (x : Player.t) : Players.t -> Players.t =
    Players.filter (Player.opposed x)
  ;;

  type neighbours =
    { left : Player.t
    ; right : Player.t
    }
  [@@deriving show { with_path = false }]

  exception NoNeighbour
  exception PlayerNotIncluded
  exception NegativeIndex

  let neighbours (x : Player.t) (ys : Players.t) : neighbours =
    let max : int = Players.cardinal ys in
    let zs : Player.t list = Players.to_list ys in
    let get : int -> Player.t =
      try List.nth zs with Invalid_argument _ -> raise NegativeIndex
    in
    match List.find_index (Player.equal x) zs with
    | None -> raise PlayerNotIncluded
    | Some offset ->
      let index (n : int) : int = if n < 0 then max - 1 else n in
      let seek (dir : int) : Player.t = get (index ((offset + dir) mod max)) in
      let left : Player.t = seek (-1) in
      let right : Player.t = seek 1 in
      (* if the left and right are [x] then there are no valid neighbours. *)
      (* note: this still allows them to be the same non-self player. *)
      let self : Player.t -> bool = Player.equal x in
      if self left && self right then raise NoNeighbour;
      { left; right }
  ;;

  let filter_neighbours
        (f : Players.t -> Players.t)
        (x : Player.t)
        (ys : Players.t)
    : neighbours
    =
    f ys |> add x |> neighbours x
  ;;

  let allied_neighbours (x : Player.t) : Players.t -> neighbours =
    filter_neighbours (allied x) x
  ;;

  let opposed_neighbours (x : Player.t) : Players.t -> neighbours =
    filter_neighbours (opposed x) x
  ;;

  let alive_neighbours : Player.t -> Players.t -> neighbours =
    filter_neighbours alive
  ;;

  let dead_neighbours : Player.t -> Players.t -> neighbours =
    filter_neighbours dead
  ;;

  type t = Players.t
  type elt = Player.t
end

module Players : Players_Type = MakePlayers (struct
    let minimum_num_players = 4
  end)

(** make game *)
module Make = struct
  module Round = struct
    (** *)

    (** tracks turn counter and players *)
    type t =
      { num : int
      ; players : Players.t
      }

    let initial (players : Players.t) : t = { num = 0; players }

    module type Args = sig
      val data : t
    end

    module Make (Args : Args) : Round.S with type data = t = Round.Make (struct
        type data = t

        let initial = Args.data
      end)
  end

  let players : Players.t =
    Players.create
      [ Player.create 0 Washerwoman
      ; Player.create 1 Librarian
      ; Player.create 2 Investigator
      ; Player.create 3 Chef (*********)
      ; Player.create 4 Poisoner
      ; Player.create 5 Imp
      ]
  ;;

  let round : Round.t = Round.initial players

  (** create fresh round-env wrapper around active round *)
  module RoundEnv = Round.Make (struct
      let data = round
    end)

  (** linked-list into previous rounds *)
  type rounds =
    { this : Round.t
    ; prev : rounds option
    }

  type t = { round : rounds }
end
