module type GameState_ = sig
  type t

  val is_night : t -> bool
  val is_day : t -> bool
  val check_winner : t -> bool option
end

module Abilties = struct
  type trigger =
    | AtNight
    | OnDeath
    | StartOfGame

  type ('game_state, 'result) t =
    { triggers : trigger list
    ; f : 'game_state -> 'result
    }

  module type S = sig
    module G : GameState_
    type result
    type t' = (G.t, result) t
    val data : t'
    val check : t' -> bool
    val run : t' -> result 
    val run_opt : t' -> result option
  end

  (* module Make (X : sig
      type result
      val data : ('a, result) t
    end) : S with type result = X.result = struct
    include X
    type t' = (game_state, result) t

    end *)

  (* module RavenKeeper (X : sig
      type game_state
    end) : S = struct
    type game_state = X.game_state

    (** TODO: ideally this would be more concrete information rather than a string referencing their role
    *)
    type result = { role : string }

    type t' = (game_state, result) t

    let data : t' = {
      triggers = [AtNight;OnDeath];
      f = (

      )
    }


  end *)
end

module type S = sig
  type ability

  type t =
    { name : string
    ; index : int
    ; alive : bool
    ; ability : ability
    ; alignment : Alignment.t
    }

  val is_alive : t -> bool
  val is_dead : t -> bool
  val allied : t -> t -> bool
end

(* module Make (Ability : Abilties.S) : S with type ability = Ability.t = struct
  type ability = Ability.t

  type t =
    { name : string
    ; index : int
    ; alive : bool
    ; ability : Ability.t
    ; alignment : Alignment.t
    }

  let is_alive (x : t) : bool = x.alive
  let is_dead (x : t) : bool = Bool.not x.alive
  let allied (a : t) (b : t) : bool = Alignment.allied a.alignment b.alignment
end *)

module MakePlayers (Player : S) = struct
  module Set : Set.S with type elt = Player.t = Set.Make (struct
      type t = Player.t

      let compare (a : t) (b : t) : int = Int.compare a.index b.index
    end)

  let minimum_num_players : int = 4

  exception LessThanMinimumNumPlayers

  let of_list (xs : Player.t list) : Set.t =
    if List.length xs >= minimum_num_players
    then Set.of_list xs
    else raise LessThanMinimumNumPlayers
  ;;

  exception IndexOutOfBounds of int

  let get_indexed (x : int) (ys : Set.t) : Player.t =
    try List.nth (Set.elements ys) x with
    | Failure _ -> raise (IndexOutOfBounds x)
  ;;

  let get_alive : Set.t -> Set.t = Set.filter Player.is_alive
  let get_dead : Set.t -> Set.t = Set.filter Player.is_dead

  type neighbours =
    { lhs : Player.t
    ; rhs : Player.t
    }

  let neighbours ?(living : bool option = None) (x : Player.t) (ys : Set.t)
    : neighbours
    =
    let g (dir : int) : Player.t = get_indexed (x.index + dir) ys in
    let f (dir : int) : Player.t =
      match living with
      | None -> g dir
      | Some b ->
        let rec f (offset : int) : Player.t =
          let y : Player.t = g offset in
          if Bool.equal b y.alive then y else f (offset + dir)
        in
        f dir
    in
    { lhs = f (-1); rhs = f 1 }
  ;;

  (** put this so low so it doesn't interfere with {!Player.t} *)
  type t = Set.t
end

(* module GameState = struct
  module Players = MakePlayers ()

  type t = { players : Players.t }
end *)
