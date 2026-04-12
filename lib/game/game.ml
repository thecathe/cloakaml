module Roles = Roles

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
  include Set.S with type elt = Player.t

  val add_role : Roles.t -> t -> t
  val create : elt list -> t
  val alive : t -> t
  val dead : t -> t
  val allied : elt -> t -> t
  val opposed : elt -> t -> t

  module Neighbours : sig
    type t =
      { left : Player.t
      ; right : Player.t
      }
    [@@deriving show]

    exception NoNeighboursFound
    exception InvalidPlayerIndex
    exception RootPlayerNotFound of Player.t

    val find : Player.t -> Player.t list -> t
    val find_opt : ?rooted:bool -> Player.t -> Player.t list -> t option
  end

  (** [neighbours x ys] returns the neighbours of [x] in [ys], i.e., those indexed either side of [x]. {b Note:} requires that [x] be in [ys]. {b Note:} if there is only one valid neighbour, then both {!Neighbours.left} and {!Neighbours.right} will refer to the same {!Player.t}. {b Note:} raises [NoNeighbour] in the event that [x] would be it's own neighbours.
  *)
  val neighbours : elt -> t -> Neighbours.t

  val allied_neighbours : elt -> t -> Neighbours.t
  val opposed_neighbours : elt -> t -> Neighbours.t
  val alive_neighbours : elt -> t -> Neighbours.t
  val dead_neighbours : elt -> t -> Neighbours.t
end

module MakePlayers (Args : sig
    val minimum_num_players : int
  end) : Players_Type = struct
  (**  *)

  module Players : Set.S with type elt = Player.t = Set.Make (Player)
  include Players

  let add_role (x : Roles.t) (ys : t) : t =
    add (Player.create (cardinal ys) x) ys
  ;;

  let create (xs : elt list) : t = of_list xs
  let alive : t -> t = filter Player.alive
  let dead : t -> t = filter Player.dead
  let allied (x : elt) : t -> t = filter (Player.allied x)
  let opposed (x : elt) : t -> t = filter (Player.opposed x)

  module Neighbours = struct
    type t =
      { left : Player.t
      ; right : Player.t
      }
    [@@deriving show]

    exception NoNeighboursFound
    exception InvalidPlayerIndex
    exception RootPlayerNotFound of Player.t

    let find (x : Player.t) (ys : Player.t list) : t =
      match List.find_index (Player.equal x) ys with
      | None -> raise (RootPlayerNotFound x)
      | Some offset ->
        let get : int -> Player.t =
          try List.nth ys with Invalid_argument _ -> raise InvalidPlayerIndex
        in
        let max : int = List.length ys in
        let wraparound (n : int) : int =
          let n' : int = n mod max in
          if n' < 0 then max - 1 else n'
        in
        let seek (dir : int) : Player.t = get (wraparound (offset + dir)) in
        let left : Player.t = seek (-1) in
        let right : Player.t = seek 1 in
        let nobody : Player.t -> bool = Player.equal x in
        if nobody left && nobody right then raise NoNeighboursFound;
        { left; right }
    ;;

    (** ... if [rooted] is [true] then any {!RootPlayerNotFound} exceptions are propagated.
    *)
    let find_opt ?(rooted : bool = true) (x : Player.t) (ys : Player.t list)
      : t option
      =
      try Some (find x ys) with
      | NoNeighboursFound -> None
      | RootPlayerNotFound x ->
        if rooted then raise (RootPlayerNotFound x) else None
    ;;

    let num_aligned (a : Roles.alignment) (x : Player.t) (ys : Player.t list)
      : int
      =
      match find_opt x ys with
      | None -> 0
      | Some { left; right } ->
        let f (z : Player.t) : int =
          if Roles.alignment z.role |> Roles.equal_alignment a then 1 else 0
        in
        f left + f right
    ;;
  end

  let neighbours (x : elt) (ys : t) : Neighbours.t =
    to_list ys |> Neighbours.find x
  ;;

  
     let filter_neighbours (f : t -> t) (x : elt) (ys : t) : Neighbours.t =
     f ys |> add x |> neighbours x
     ;;

     let allied_neighbours (x : elt) : t -> Neighbours.t =
     filter_neighbours (allied x) x
     ;;

     let opposed_neighbours (x : elt) : t -> Neighbours.t =
     filter_neighbours (opposed x) x
     ;;

     let alive_neighbours : elt -> t -> Neighbours.t = filter_neighbours alive
     let dead_neighbours : elt -> t -> Neighbours.t = filter_neighbours dead
  
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
