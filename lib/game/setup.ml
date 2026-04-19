exception TooFewPlayers of int

module Distribution = struct
  type t =
    { mutable townsfolk : int
    ; mutable outsiders : int
    ; mutable minions : int
    ; mutable demons : int
    }
  [@@deriving show { with_path = false }, make]

  let make townsfolk outsiders minions demons =
    make ~townsfolk ~outsiders ~minions ~demons
  ;;

  (** see {{:https://i.redd.it/cunojgi5omnd1.jpeg}here} for the distributions for {b Blood on the Clocktower}.
  *)
  let get : int -> t = function
    | 5 -> make 3 0 1 1
    | 6 -> make 3 1 1 1
    | 7 -> make 5 0 1 1
    | 8 -> make 5 1 1 1
    | 9 -> make 5 2 1 1
    | 10 -> make 7 0 2 1
    | 11 -> make 7 1 2 1
    | 12 -> make 7 2 2 1
    | 13 -> make 9 0 3 1
    | 14 -> make 9 1 3 1
    | n -> if n < 5 then raise (TooFewPlayers n) else make 9 2 3 1
  ;;

  let sum (x : t) : int = x.townsfolk + x.outsiders + x.minions + x.demons

  let show_role_status_map (x : bool Roles.Map.t) : string =
    String.cat
      (Roles.Map.fold
         (fun k v acc -> Printf.sprintf "%s%b: %s\n" acc v (Roles.Role.show k))
         x
         "[\n")
      "]"
  ;;
 
  let fresh_role_status_map () : bool Roles.Map.t =
    let tbl : bool Roles.Map.t = Roles.Map.create Roles.Role.max in
    List.iter (fun x -> Roles.Map.add tbl x false) Roles.collect;
    tbl 
  ;;

  type pull_role =
    { role : Roles.Kind.t 
    ; map : bool Roles.Map.t
    }

  let available_roles (x : pull_role) : Roles.Role.t list =
    Roles.Map.fold
      (fun k v acc ->
        if Roles.Kind.equal x.role (Roles.Role.kind k) && Bool.not v
        then k :: acc
        else acc)
      x.map
      []
  ;;

  let show_available_roles (x : Roles.Kind.t) (xs : Roles.Role.t list) : string =
    Printf.sprintf
      "available %s:%s\n"
      (Roles.Kind.show x)
      (List.fold_left
         (fun acc x -> Printf.sprintf "%s %s" acc (Roles.Role.show x))
         ""
         xs)
  ;;

  let pull_role (x : pull_role) : Roles.Role.t =
    let xs = available_roles x in
    let y = List.nth xs (Random.int (List.length xs)) in
    Roles.Map.replace x.map y true;
    y
  ;;

  let rec fill_role (x : pull_role) (n : int) (acc : Players.t) : Players.t =
    if n <= 0
    then acc
    else Players.add_role (pull_role x) acc |> fill_role x (n - 1)
  ;;

  let fill_roles (map : bool Roles.Map.t) (x : t) : Players.t =
    Printf.printf "dist: %s\n" (show x);
    Players.empty
    |> fill_role { map; role = Townsfolk } x.townsfolk
    |> fill_role { map; role = Outsider } x.outsiders
    |> fill_role { map; role = Minion } x.minions
    |> fill_role { map; role = Demon } x.demons
  ;;

  let players (map : bool Roles.Map.t) (n : int) : Players.t =
    get n |> fill_roles map
  ;;
end

let players
      ?(map : bool Roles.Map.t = Distribution.fresh_role_status_map ())
  : int -> Players.t
  =
  Distribution.players map
;;

let round (starting : Phase.t) (n : int) : Round.t =
  let map : bool Roles.Map.t = Distribution.fresh_role_status_map () in
  players ~map n |> Round.initial ~starting map
;;
