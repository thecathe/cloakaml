module Map = Map

(** {1 Set of Players} *)

include Set.Make (Player) [@@deriving show { with_path = false }] (** @closed *)

let show (xs : t) : string =
  String.cat
    (fold (fun x acc -> Printf.sprintf "%s\n  %s" acc (Player.show x)) xs "[")
    "\n]"
;;

(** [add_role x ys] takes a {!Roles.Role.t} [x] and makes a fresh {!Player.t} with [index] equal to {!cardinal}.
*)
let add_role (x : Roles.Role.t) (ys : t) : t =
  add (Player.create (cardinal ys) x) ys
;;

let create (xs : Player.t list) : t = of_list xs

(** {2 Filter Functions} *)

exception NoPlayersWithRole

let kinds (kind : Roles.Kind.t) (xs : t) : t =
  to_list xs
  |> List.filter (fun (z : Player.t) ->
    Roles.Kind.equal kind (Roles.Role.kind z.role))
  |> of_list
;;

let aligned (alignment : Roles.Alignment.t) (xs : t) : t =
  to_list xs
  |> List.filter (fun (z : Player.t) ->
    Roles.Alignment.equal alignment (Roles.Role.alignment z.role))
  |> of_list
;;

let random ?(f : (t -> t) option) (xs : t) : Player.t =
  let xs : Player.t list =
    Option.fold ~none:xs ~some:(fun f -> f xs) f |> to_list
  in
  Random.self_init ();
  try List.nth xs (Random.int (List.length xs)) with
  | Failure _ -> raise NoPlayersWithRole
;;

(** {3 Player Fields} *)

(** {4 Status} *)

let alive : t -> t = filter Player.alive
let dead : t -> t = filter Player.dead
let poisoned : t -> t = filter Player.poisoned
let status (x : Player.Status.t) : t -> t = filter (Player.status x)

(** {4 Groups} *)

let allied (x : Player.t) : t -> t = filter (Player.allied x)
let opposed (x : Player.t) : t -> t = filter (Player.opposed x)

exception NoPlayerWithRole of Roles.Role.t

let group : Roles.Group.t -> t -> t = function
  | Role x ->
    fun ys ->
      (try filter (fun y -> Roles.Role.equal x y.role) ys with
       | Not_found -> raise (NoPlayerWithRole x))
  | Kind x -> kinds x
  | Alignment x -> aligned x
;;

(** helper function for applying filters that require target to be in resulting set.
*)
let incl_self (f : Player.t -> t -> t) (x : Player.t) : t -> t =
  fun xs -> f x xs |> add x
;;

(** {3 Player Neighbours} *)

module Neighbours = Neighbours

(** [neighbours x ys] returns the neighbours of [x] in [ys], i.e., those indexed either side of [x]. {b Note:} requires that [x] be in [ys]. {b Note:} if there is only one valid neighbour, then both {!Neighbours.left} and {!Neighbours.right} will refer to the same {!Player.t}. {b Note:} raises [NoNeighbour] in the event that [x] would be it's own neighbours.
*)
let neighbours ?(f : t -> t = fun x -> x) (x : Player.t) (ys : t) : Neighbours.t
  =
  f ys |> to_list |> Neighbours.find x
;;

let allied_neighbours (x : Player.t) : t -> Neighbours.t =
  neighbours ~f:(incl_self allied x) x
;;

let opposed_neighbours (x : Player.t) : t -> Neighbours.t =
  neighbours ~f:(incl_self opposed x) x
;;

let alive_neighbours : Player.t -> t -> Neighbours.t = neighbours ~f:alive
let dead_neighbours : Player.t -> t -> Neighbours.t = neighbours ~f:dead

(* let with_active_abilities : t -> t =
   filter (fun y ->
   Abilities.make y.role |> Abilities.active |> List.is_empty |> Bool.not)
   ;; *)

(* let with_phase_abilities (x : Phase.t) : t -> t =
   filter (fun y ->
   Abilities.make y.role |> Abilities.phase x |> List.is_empty |> Bool.not)
   ;; *)

(* *)

let exists_role_kind (x : Roles.Kind.t) (map : bool Roles.Map.t) : bool =
  Roles.Map.to_seq map
  |> List.of_seq
  |> List.filter (fun ((k, v) : Roles.Role.t * bool) ->
    Roles.Kind.equal x (Roles.Role.kind k) && Bool.not v)
  |> List.is_empty
  |> Bool.not
;;

(** {3 Setup} *)

exception NoNewRole
exception NoOldRole

let assert_can_change (x : Roles.Kind.t) (map : bool Roles.Map.t) (e : exn)
  : unit
  =
  if exists_role_kind x map
  then (
    Printf.printf "no players with role: %s\n" (Roles.Kind.show x);
    raise e)
;;

let log_replace_kind (target : Player.t) new_role : unit =
  Printf.printf
    "replacing player %i role: %s -> %s\n"
    target.index
    (Roles.Role.show target.role)
    (Roles.Role.show new_role)
;;

(** [replace_player_role_kind old new map players exclude] replaces the {{!Player.t.role}role} of one {{!Player.t}player} in [players] that is not in [exclude] with {{!type:Roles.Kind.t}kind} [old] is replaced with a new {{!Player.t.role}role} with {{!type:Roles.Kind.t}kind} [new]. We use [map] to ensure that we don't introduce duplicate {{!Roles.Role.t}roles} into the game, {i and} to that we can't re-add a {{!Roles.Role.t}role} that has been removed.
*)
let replace_player_role_kind
      (a : Roles.Kind.t)
      (b : Roles.Kind.t)
      (rolemap : bool Roles.Map.t)
      (xs : t)
      (ys : t)
  : Player.t
  =
  assert_can_change b rolemap NoNewRole;
  try
    let target : Player.t = random ~f:(kinds a) (diff xs ys) in
    Roles.random_kind b |> Player.replace_role target
  with
  | NoPlayersWithRole ->
    Printf.printf "no players with role: %s\n" (Roles.Kind.show a);
    raise NoOldRole
;;

let rec replace_n_player_role_kinds
          ?(acc : t = empty)
          (n : int)
          (a : Roles.Kind.t)
          (b : Roles.Kind.t)
          (rolemap : bool Roles.Map.t)
          (xs : t)
  : unit
  =
  if n <= 0
  then Printf.printf "stop replacing kinds: %s" (show acc)
  else (
    let replaced = replace_player_role_kind a b rolemap xs acc in
    replace_n_player_role_kinds ~acc:(add replaced acc) (n - 1) a b rolemap xs)
;;
