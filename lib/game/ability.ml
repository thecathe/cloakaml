type kind =
  | Setup
  | StartOfGame
  | Active
[@@deriving show { with_path = false }, eq]

module Trigger = struct
  type t =
    | Setup
    | Passive
    | StartOfGame
  (* | EachNight *)
  (* | EachDay *)
  (* | Conditional *)
  [@@deriving show { with_path = false }, eq, enum]

  let kind : t -> kind = function
    | Setup -> Setup
    | StartOfGame -> StartOfGame
    | Passive -> Active
  ;;

  let is_kind a b : bool = kind b |> equal_kind a
end

exception ToDo

let todo (s : string) : unit = Printf.printf "TODO: %s\n" s

module RoleAbility = struct
  type f = Round.t -> unit
  type map = Trigger.t * f

  module type S = sig
    val x : Roles.t
    val get : unit -> map list
    val triggers : unit -> Trigger.t list
    val has_kind : kind -> bool
  end

  type t = (module S)

  let has_kind (x : kind) (module X : S) : bool = X.has_kind x
  let filter_by_kind (x : kind) : t list -> t list = List.filter (has_kind x)
  let role (module X : S) : Roles.t = X.x

  module Make (X : sig
      val x : Roles.t
      val abilities : map list
    end) : S = struct
    (** ... *)
    let x : Roles.t = X.x

    (** ... *)
    let get () : map list = X.abilities

    (** ... *)
    let triggers () : Trigger.t list = get () |> List.map fst

    (** ... *)
    let has_kind (x : kind) : bool =
      triggers () |> List.exists (Trigger.is_kind x)
    ;;
  end

  let setup_baron : map =
    let f =
      fun ({ players; rolemap; _ } : Round.t) ->
      Players.replace_n_player_role_kinds 2 Townsfolk Outsider rolemap players
    in
    Setup, f
  ;;

  exception RoleAbilityNotFound of (Roles.t * kind)

  module Kinds = struct
    module type S = sig
      val trigger : Trigger.t
      val of_role : Roles.t -> f
      val get : Roles.t -> map
    end

    module Make (X : sig
        val trigger : Trigger.t
        val roles : (Roles.t * f) list
      end) : S = struct
      (** *)
      let trigger : Trigger.t = X.trigger

      (** *)
      let of_role (x : Roles.t) : f =
        try List.find (fun (y, z) -> Roles.equal x y) X.roles |> snd with
        | Not_found -> raise (RoleAbilityNotFound (x, Trigger.kind trigger))
      ;;

      (** *)
      let get (x : Roles.t) : map = trigger, of_role x
    end

    module Setup : S = Make (struct
        let trigger : Trigger.t = Setup

        let baron : Round.t -> unit =
          fun ({ players; rolemap; _ } : Round.t) ->
          Players.replace_n_player_role_kinds
            2
            Townsfolk
            Outsider
            rolemap
            players
        ;;

        let roles : (Roles.t * f) list = [ Baron, baron ]
      end)

    module StartOfGame : S = Make (struct
        let trigger : Trigger.t = StartOfGame

        (** {1 Start of Game Role Abilities} *)

        let washerwoman = fun x -> todo __FUNCTION__
        let librarian = fun x -> todo __FUNCTION__
        let investigator = fun x -> todo __FUNCTION__
        let chef = fun x -> todo __FUNCTION__

        let roles : (Roles.t * f) list =
          [ Washerwoman, washerwoman
          ; Librarian, librarian
          ; Investigator, investigator
          ; Chef, chef
          ]
        ;;
      end)

    let get (x : Roles.t) : map list =
      match x with
      (* *)
      | Washerwoman | Librarian | Investigator | Chef -> [ StartOfGame.get x ]
      (* *)
      | Baron -> [ Setup.get x ]
      (* *)
      | _ -> []
    ;;
  end

  let make (x : Roles.t) : t =
    (module Make (struct
         let x = x
         let abilities = Kinds.get x
       end) : S)
  ;;

  let of_player (x : Player.t) : t = make x.role

  exception NoneRoles

  let of_role_enum (x : int) : t =
    match Roles.of_enum x with None -> raise NoneRoles | Some x -> make x
  ;;

  let role_has_kind (x : kind) (y : Roles.t) : bool = make y |> has_kind x
end

type role_ability = RoleAbility.t

module RoleAbilities = struct
  type t = RoleAbility.t list

  let get ?(of_players : Players.t option = None) () : t =
    match of_players with
    | None -> List.init Roles.max RoleAbility.of_role_enum
    | Some players -> Players.to_list players |> List.map RoleAbility.of_player
  ;;

  let have_kind ?(of_players : Players.t option) (x : kind) : Roles.t list =
    get ~of_players ()
    |> RoleAbility.filter_by_kind x
    |> List.map RoleAbility.role
  ;;
end

let player_has_kind (y : kind) (x : Player.t) : bool =
  RoleAbility.role_has_kind y x.role
;;

let player_has_any_kinds (ys : kind list) (x : Player.t) : bool =
  List.exists (fun y -> player_has_kind y x) ys
;;

let players_have_kind (y : kind) : Player.t list -> Player.t list =
  List.filter (player_has_kind y)
;;

let players_have_kind (y : kind) (xs : Players.t) : Players.t =
  Players.to_list xs |> players_have_kind y |> Players.of_list
;;

let players_have_any_kinds (ys : kind list) (xs : Players.t) : Players.t =
  Players.to_list xs |> List.filter (player_has_any_kinds ys) |> Players.of_list
;;

let players_with_kinds_of_ability (ys : kind list) (x : Round.t) : Players.t =
  players_have_any_kinds ys x.players
;;

module Map = struct
  module Map : Hashtbl.S with type key = Trigger.t = Hashtbl.Make (struct
      type t = Trigger.t [@@deriving show { with_path = false }]

      let hash x = Int.hash (Trigger.to_enum x)
      let equal a b = Int.equal (Trigger.to_enum a) (Trigger.to_enum b)
    end)

  include Map (** @closed *)

  type t' = RoleAbility.f t

  let add_all (map : t') (xs : RoleAbility.map list) : t' =
    xs |> List.to_seq |> Map.add_seq map;
    map
  ;;

  let of_role ?(map : t' = create 0) (x : Roles.t) : t' =
    let module R : RoleAbility.S = (val RoleAbility.make x) in
    R.get () |> add_all map
  ;;

  let make ?(map : t' = create 0) (x : Player.t) : t' = of_role ~map x.role

  let has_kind (map : t') (x : kind) : bool =
    Map.to_seq map
    |> List.of_seq
    |> List.exists (fun (k, _) -> Trigger.is_kind x k)
  ;;

  let kind_opt (x : kind) : key -> RoleAbility.f -> RoleAbility.f option =
    fun k v -> if Trigger.is_kind x k then Some v else None
  ;;

  let reduce_by_kind (map : t') (x : kind) : unit =
    Map.filter_map_inplace (kind_opt x) map
  ;;
end

type t = Map.t' Player.Map.t

let map_player ?(map : t = Player.Map.create 0) (x : Player.t) : t =
  Player.Map.replace map x (Map.make x);
  map
;;

let map_players (xs : Players.t) : t =
  Players.cardinal xs
  |> Player.Map.create
  |> Players.fold (fun x map -> map_player ~map x) xs
;;

let kind_opt (x : kind) : Player.t -> Map.t' -> Map.t' option =
  fun k v ->
  let map = Map.copy v in
  Map.reduce_by_kind map x;
  if Map.length map <= 0 then None else Some map
;;

let of_kind (y : kind) (x : Round.t) : t =
  let map = map_players x.players in
  Player.Map.filter_map_inplace (kind_opt y) map;
  map
;;

let get_setup (x : Round.t) : t = of_kind Setup x
let players (map : t) = Player.Map.to_seq_keys map |> Players.of_seq
