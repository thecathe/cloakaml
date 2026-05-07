(** @canonical Game.Mode.Spec.Round.Data *)

module Phase = Phase

type ('phase, 'players) t =
  { phase : 'phase
  ; players : 'players
  }

type ('phase, 'rolemap) initial =
  { starting_phase : 'phase
  ; rolemap : 'rolemap
  }

module type S = sig
  module Phase : Phase.S

  type phase = Phase.t

  module Players : Players.S

  type players = Players.t
  type roles = Players.Player.Roles.Set.t
  type rolemap = bool Players.Player.Roles.Role.Map.t
  type nonrec t = (phase, players) t
  type nonrec initial = (phase, rolemap) initial

  val initial : ?starting_phase:phase -> roles -> initial

  exception PlayerRolesDisjoint of Players.t
  exception PlayerRolesInvalid of Players.t

  val create : initial -> players -> t
  val next : t -> t
  val starting_phase : initial -> phase
  val rolemap : initial -> rolemap
  val phase : t -> phase
  val players : t -> players
end

module Make (P : Phase.S) (Ps : Players.S) :
  S
  with module Phase = P
   and module Players = Ps
   and type t = (P.t, Ps.t) t
   and type initial = (P.t, bool Ps.Player.Roles.Role.Map.t) initial = struct
  module Phase = P

  type phase = Phase.t

  module Players = Ps

  type players = Players.t
  type roles = Players.Player.Roles.Set.t
  type rolemap = bool Players.Player.Roles.Role.Map.t
  type nonrec t = (phase, players) t
  type nonrec initial = (phase, rolemap) initial

  let init_rolemap (roles : roles) : rolemap =
    let module Roles = Players.Player.Roles in
    let module RoleMap = Players.Player.Roles.Role.Map in
    let xs : Roles.role list = Roles.Set.to_list roles in
    let rolemap : rolemap = RoleMap.create (List.length xs) in
    List.iter (fun x -> RoleMap.add rolemap x false) xs;
    rolemap
  ;;

  let initial ?(starting_phase : phase = Phase.initial) (roles : roles)
    : initial
    =
    { starting_phase; rolemap = init_rolemap roles }
  ;;

  exception PlayerRolesDisjoint of Players.t
  exception PlayerRolesInvalid of Players.t

  let assert_player_roles (rolemap : rolemap) (players : players) : unit =
    let module RoleMap = Players.Player.Roles.Role.Map in
    let players_with_roles =
      Players.filter (fun x -> RoleMap.mem rolemap x.role) players
    in
    if Players.disjoint players players_with_roles
    then raise (PlayerRolesDisjoint (Players.diff players players_with_roles));
    let invalid_roles =
      Players.filter (fun x -> RoleMap.find rolemap x.role |> Bool.not) players
    in
    if Players.is_empty invalid_roles |> Bool.not
    then raise (PlayerRolesInvalid invalid_roles)
  ;;

  let create (initial : initial) (players : players) : t =
    assert_player_roles initial.rolemap players;
    { phase = initial.starting_phase; players }
  ;;

  let next (x : t) : t = { x with phase = Phase.step ~starting:x.phase 1 }
  let starting_phase (x : initial) : phase = x.starting_phase
  let rolemap (x : initial) : rolemap = x.rolemap
  let phase (x : t) : phase = x.phase
  let players (x : t) : players = x.players
end
