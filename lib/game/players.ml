module Players : Set.S with type elt = Player.t = Set.Make (Player)
include Players

let show (xs : t) : string =
  String.cat
    (fold
       (fun x acc -> String.cat (Player.show x) "\n" |> String.cat acc)
       xs
       "[\n")
    "]"
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
let allied (x : Player.t) : t -> t = filter (Player.allied x)
let opposed (x : Player.t) : t -> t = filter (Player.opposed x)

let neighbours (x : Player.t) (ys : t) : Neighbours.t =
  to_list ys |> Neighbours.find x
;;

let filter_neighbours (f : t -> t) (x : Player.t) (ys : t) : Neighbours.t =
  f ys |> add x |> neighbours x
;;

let allied_neighbours (x : Player.t) : t -> Neighbours.t =
  filter_neighbours (allied x) x
;;

let opposed_neighbours (x : Player.t) : t -> Neighbours.t =
  filter_neighbours (opposed x) x
;;

let alive_neighbours : Player.t -> t -> Neighbours.t = filter_neighbours alive
let dead_neighbours : Player.t -> t -> Neighbours.t = filter_neighbours dead

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
  if exists_role_kind x map then raise e
;;

let replace_kind
      (a : Roles.kind)
      (b : Roles.kind)
      (map : bool Roles.Map.t)
      (xs : t)
  : unit
  =
  assert_can_change b map NoNewRole;
  try
    let target : Player.t = random ~f:(kinds a) xs in
    let new_role = Roles.random_kind b in
    target.role <- new_role
  with
  | NoPlayersWithRole -> raise NoOldRole
;;
