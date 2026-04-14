module AbilityMap = struct
  module Map : Hashtbl.S with type key = Player.t = Hashtbl.Make (struct
      type t = Player.t

      let hash (x : Player.t) = Roles.to_enum x.role

      let equal (a : Player.t) (b : Player.t) : bool =
        Int.equal (Roles.to_enum a.role) (Roles.to_enum b.role)
      ;;
    end)

  include Map

  type t' = Abilities.t t
end

(** tracks turn counter and players *)
type t =
  { num : int
  ; players : Players.t
  ; phase : Phase.data
  ; abilities : AbilityMap.t'
  }

exception ToDo

let initial ?(starting : Phase.t = Phase.Day) (players : Players.t) : t =
  let abilities = AbilityMap.create (Players.cardinal players) in
  Players.iter
    (fun x ->
      try
        let ability = Abilities.make x.role in
        AbilityMap.add abilities x ability
      with
      | effect Abilities.Abilities.NeedRolesToTarget (), k ->
        Effect.Deep.continue k (Players.random players).index
      | effect Abilities.Abilities.AddExtraOutsider (), k ->
        (* TODO: *)
        raise ToDo
      (* Effect.Deep.continue k () *))
    players;
  { num = 0; phase = Phase.make starting; players; abilities }
;;

let is_phase (x : t) : Phase.t -> bool = Phase.equal x.phase.current

(** [is_tonight n x] is [true] if [x.phase.current] is {{!Phase.Night}Night} and [x.num] is equal to [n], i.e., [n] is the turn number collected in the {{!Phase.Day}Day} for an ability to be used at {{!Phase.Night}Night}.
*)
let is_tonight (n : int) (x : t) : bool = Int.equal n x.num && is_phase x Night

(** [is_today n x] is {i dual} respective to {!is_tonight} but for {{!Phase.Day}Day}.
*)
let is_today (n : int) (x : t) : bool = Int.equal n x.num && is_phase x Day

(** [is_tomorrow n x] returns [is_today (n + 1) x]. *)
let is_tomorrow (n : int) (x : t) : bool = is_today (n + 1) x

let players_with_active_abilities (x : t) : Players.t =
  Players.with_active_abilities x.players
;;

let players_with_phase_abilities (x : t) : Players.t =
  Players.with_phase_abilities x.phase.current x.players
;;
