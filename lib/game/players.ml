module Players : Set.S with type elt = Player.t = Set.Make (Player)
include Players

let show (xs : t) : string =
  String.cat
    (fold (fun x acc -> Printf.sprintf "%s\n  %s" acc (Player.show x)) xs "[")
    "\n]"
;;

let add_role (x : Roles.t) (ys : t) : t = add (Player.create (cardinal ys) x) ys
let create (xs : Player.t list) : t = of_list xs

exception NoPlayersWithRole

let kinds (kind : Roles.kind) (xs : t) : t =
  to_list xs
  |> List.filter (fun (z : Player.t) ->
    Roles.equal_kind kind (Roles.kind z.role))
  |> of_list
;;

let aligned (alignment : Roles.alignment) (xs : t) : t =
  to_list xs
  |> List.filter (fun (z : Player.t) ->
    Roles.equal_alignment alignment (Roles.alignment z.role))
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

let alive : t -> t = filter Player.alive
let dead : t -> t = filter Player.dead
let poisoned : t -> t = filter Player.poisoned
let status (x : Player.Status.t) : t -> t = filter (Player.status x)
let allied (x : Player.t) : t -> t = filter (Player.allied x)
let opposed (x : Player.t) : t -> t = filter (Player.opposed x)

exception NoPlayerWithRole of Roles.t

let group : Roles.group -> t -> t = function
  | Roles.Role x ->
    fun ys ->
      (try filter (fun y -> Roles.equal x y.role) ys with
       | Not_found -> raise (NoPlayerWithRole x))
  | Roles.Kind x -> kinds x
  | Roles.Alignment x -> aligned x
;;

(** helper function for applying filters that require target to be in resulting set.
*)
let incl_self (f : Player.t -> t -> t) (x : Player.t) : t -> t =
  fun xs -> f x xs |> add x
;;

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

let with_active_abilities : t -> t =
  filter (fun y ->
    Abilities.make y.role |> Abilities.active |> List.is_empty |> Bool.not)
;;

let with_phase_abilities (x : Phase.t) : t -> t =
  filter (fun y ->
    Abilities.make y.role |> Abilities.phase x |> List.is_empty |> Bool.not)
;;

(* *)

let exists_role_kind (x : Roles.kind) (map : bool Roles.Map.t) : bool =
  Roles.Map.to_seq map
  |> List.of_seq
  |> List.filter (fun ((k, v) : Roles.t * bool) ->
    Roles.equal_kind x (Roles.kind k) && Bool.not v)
  |> List.is_empty
  |> Bool.not
;;

exception NoNewRole
exception NoOldRole

let assert_can_change (x : Roles.kind) (map : bool Roles.Map.t) (e : exn) : unit
  =
  if exists_role_kind x map
  then (
    Printf.printf "no players with role: %s\n" (Roles.show_kind x);
    raise e)
;;

let replace_kind
      (a : Roles.kind)
      (b : Roles.kind)
      (map : bool Roles.Map.t)
      (xs : t)
      (ys : t)
  : Player.t
  =
  assert_can_change b map NoNewRole;
  try
    let target : Player.t = random ~f:(kinds a) (Players.diff xs ys) in
    let new_role = Roles.random_kind b in
    Printf.printf
      "replacing player %i role: %s -> %s\n"
      target.index
      (Roles.show target.role)
      (Roles.show new_role);
    target.role <- new_role;
    target
  with
  | NoPlayersWithRole ->
    Printf.printf "no players with role: %s\n" (Roles.show_kind a);
    raise NoOldRole
;;

let rec replace_n_kinds
          ?(acc : t = empty)
          (n : int)
          (a : Roles.kind)
          (b : Roles.kind)
          (map : bool Roles.Map.t)
          (xs : t)
  : t
  =
  if n <= 0
  then (
    Printf.printf "stop replacing kinds";
    acc)
  else (
    let replaced = replace_kind a b map xs acc in
    replace_n_kinds ~acc:(add replaced acc) (n - 1) a b map xs)
;;
