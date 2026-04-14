module AbilityMap = struct
  module Map : Hashtbl.S with type key = Player.t = Hashtbl.Make (struct
      type t = Player.t

      let hash (x : Player.t) = Int.hash x.index
      let equal (a : Player.t) (b : Player.t) : bool = Int.equal a.index b.index
    end)

  include Map

  type t' = Abilities.t t
end

(** tracks turn counter and players *)
type t =
  { num : int
  ; phase : Phase.data
  ; players : Players.t
  ; abilities : AbilityMap.t'
  }

exception ToDo

let get_random_player (x : Roles.alignment) : Players.t -> Player.t =
  Players.random ~f:(Players.aligned x)
;;

let get_ability (x : Player.t) : Abilities.t = Abilities.make x.role
let map_player_ability x acc : unit = get_ability x |> AbilityMap.replace acc x

let rec map_players_ability acc : Player.t list -> unit = function
  | [] ->
    print_endline "exit map_players_ability";
    ()
  | h :: tl ->
    Printf.printf "mapping: %s\n" (Player.show h);
    map_player_ability h acc;
    Printf.printf "cont. mapping: %s\n" (Players.show (Players.of_list tl));
    map_players_ability acc tl
;;

let rec populate_ability_map
          (xs : Players.t)
          ?(acc : AbilityMap.t' = AbilityMap.create (Players.cardinal xs))
          (rm : bool Roles.Map.t)
  : AbilityMap.t'
  =
  (fun () -> Players.to_list xs |> map_players_ability acc)
  |> handle_populate_ability_map xs acc rm;
  acc

and handle_populate_ability_map xs acc rm (f : unit -> unit) : unit =
  match f () with
  | () -> ()
  (* | effect e, _ ->
    Printf.printf
      "Unhandled effect: %s\n"
      (Obj.Extension_constructor.name (Obj.Extension_constructor.of_val e));
    raise (Effect.Unhandled e) *)
    (* TODO: effects... *)
  | effect Abilities.Abilities.GetTargetPlayer, k ->
    let x = get_random_player Roles.Good xs in
    Printf.printf "target player: %s\n" (Player.show x);
    handle_populate_ability_map xs acc rm (fun () ->
      Effect.Deep.continue k x.index)
  | effect Abilities.Abilities.AddExtraOutsiders (n : int), k ->
    let replaced = Players.replace_n_kinds n Townsfolk Outsider rm xs in
    Printf.printf "replaced: %s\n" (Players.show replaced);
    let need_updating = AbilityMap.to_seq_keys acc |> Players.of_seq in
    Printf.printf "need updating: %s\n" (Players.show need_updating);
    let ys = Players.inter replaced need_updating in
    handle_populate_ability_map ys acc rm (fun () ->
      Players.to_list ys |> map_players_ability acc);
    handle_populate_ability_map xs acc rm (fun () -> Effect.Deep.continue k ())
;;

let initial
      ?(starting : Phase.t = Phase.Day)
      (map : bool Roles.Map.t)
      (players : Players.t)
  : t
  =
  let abilities = populate_ability_map players map in
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
