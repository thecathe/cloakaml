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

(* module Triggers = struct
  module Set : Set.S with type elt = Trigger.t = Set.Make (struct
      type t = Trigger.t

      let compare a b : int =
        Int.compare (Trigger.to_enum a) (Trigger.to_enum b)
      ;;
    end)

  include Set [@@deriving show { with_path = false }] (** @closed *)
end *)

type f = Round.t -> unit
type map = Trigger.t * f

module type S = sig
  val x : Roles.t
  val get : unit -> map list
  val triggers : unit -> Trigger.t list
  val has_kind : kind -> bool
end

exception ToDo

module BlankRole : S = struct
  let x : Roles.t = Washerwoman
  let get () : map list = []
  let triggers () : Trigger.t list = get () |> List.map fst

  let has_kind (x : kind) : bool =
    triggers () |> List.exists (Trigger.is_kind x)
  ;;
end

module Baron : S = struct
  let x : Roles.t = Baron

  let setup () : f =
    fun x ->
    Players.replace_n_player_role_kinds 2 Townsfolk Outsider x.rolemap x.players
  ;;

  let get () : map list = [ Setup, setup () ]
  let triggers () : Trigger.t list = get () |> List.map fst

  let has_kind (x : kind) : bool =
    triggers () |> List.exists (Trigger.is_kind x)
  ;;
end

type role_ability = (module S)
type role_abilities = role_ability list

let has_kind (x : kind) (module Y : S) : bool = Y.has_kind x

let filter_by_kind (x : kind) : (module S) list -> (module S) list =
  List.filter (has_kind x)
;;

let of_role : Roles.t -> (module S) = function
  | Baron -> (module Baron : S)
  | _ -> (module BlankRole : S)
;;

let of_player (x : Player.t) : (module S) = of_role x.role
let to_role (module X : S) : Roles.t = X.x

exception NoneRoles

let enum_role_abilities (x : int) : (module S) =
  match Roles.of_enum x with None -> raise NoneRoles | Some x -> of_role x
;;

let role_abilities ?(of_players : Players.t option = None) () : role_abilities =
  match of_players with
  | None -> List.init Roles.max enum_role_abilities
  | Some players -> Players.to_list players |> List.map of_player
;;

let role_ability_has_kind (x : kind) (y : Roles.t) : bool =
  of_role y |> has_kind x
;;

let role_abilities_have_kind ?(of_players : Players.t option) (x : kind)
  : Roles.t list
  =
  role_abilities ~of_players () |> filter_by_kind x |> List.map to_role
;;

let player_has_kind (y : kind) (x : Player.t) : bool =
  role_ability_has_kind y x.role
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

  type t' = f t

  let add_all (map : t') (xs : map list) : t' =
    xs |> List.to_seq |> Map.add_seq map;
    map
  ;;

  let of_role ?(map : t' = create 0) : Roles.t -> t' = function
    | Baron -> Baron.get () |> add_all map
    | _ -> map
  ;;

  let make ?(map : t' = create 0) (x : Player.t) : t' = of_role ~map x.role

  let has_kind (map : t') (x : kind) : bool =
    Map.to_seq map
    |> List.of_seq
    |> List.exists (fun (k, _) -> Trigger.is_kind x k)
  ;;

  let kind_opt (x : kind) : key -> f -> f option =
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

(* let of_kinds (ys:kind list) (x:Round.t) : Map.t' =
   map_players x.players |> Map.filter_map_inplace (fun k v ->

   ) *)

let get_setup (x : Round.t) : t = of_kind Setup x
let players (map : t) = Player.Map.to_seq_keys map |> Players.of_seq
