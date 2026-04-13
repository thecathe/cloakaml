type t =
  (* townsfolk *)
  | Washerwoman
  | Librarian
  | Investigator
  | Chef
  | Empath
  | FortuneTeller
  | Undertaker
  | Monk
  | Ravenkeeper
  | Virgin
  | Slayer
  | Soldier
  | Mayor
  (* outsiders *)
  | Butler
  | Drunk
  | Recluse
  | Saint
  (* minions *)
  | Poisoner
  | Spy
  | ScarletWoman
  | Baron
  (* demons *)
  | Imp
[@@deriving show { with_path = false }, enum, eq]

type kind =
  | Townsfolk
  | Outsider
  | Minion
  | Demon
  | Traveller
  | Fabled
  | Loric
[@@deriving show { with_path = false }, eq]

let kind : t -> kind = function
  (* townsfolk *)
  | Washerwoman -> Townsfolk
  | Librarian -> Townsfolk
  | Investigator -> Townsfolk
  | Chef -> Townsfolk
  | Empath -> Townsfolk
  | FortuneTeller -> Townsfolk
  | Undertaker -> Townsfolk
  | Monk -> Townsfolk
  | Ravenkeeper -> Townsfolk
  | Virgin -> Townsfolk
  | Slayer -> Townsfolk
  | Soldier -> Townsfolk
  | Mayor -> Townsfolk
  (* outsiders *)
  | Butler -> Outsider
  | Drunk -> Outsider
  | Recluse -> Outsider
  | Saint -> Outsider
  (* minions *)
  | Poisoner -> Minion
  | Spy -> Minion
  | ScarletWoman -> Minion
  | Baron -> Minion
  (* demons *)
  | Imp -> Demon
;;

let is_townfolk (x : t) : bool =
  match kind x with Townsfolk -> true | _ -> false
;;

let is_outsider (x : t) : bool =
  match kind x with Outsider -> true | _ -> false
;;

let is_minion (x : t) : bool = match kind x with Minion -> true | _ -> false
let is_demon (x : t) : bool = match kind x with Demon -> true | _ -> false

(* *)

exception RoleEnumOutOfBounds

(** [roles] ... (* TODO: can we cache this? some reference wrapper where [let |() = ...] programmatically sets it when module first loaded? *) *)
let roles : t list =
  List.init (max + 1) (fun n ->
    match of_enum n with Some x -> x | None -> raise RoleEnumOutOfBounds)
;;

let townsfolk : t list = List.filter is_townfolk roles
let outsiders : t list = List.filter is_outsider roles
let minions : t list = List.filter is_minion roles
let demons : t list = List.filter is_demon roles

(* *)
let is_good (x : t) : bool = is_townfolk x || is_outsider x
let is_evil (x : t) : bool = is_minion x || is_demon x

type alignment =
  | Good
  | Evil
[@@deriving show { with_path = false }, eq]

exception CannotDetermineAlignment of t

let alignment (x : t) : alignment =
  if is_good x
  then Good
  else if is_evil x
  then Evil
  else raise (CannotDetermineAlignment x)
;;


let allied (a : t) (b : t) : bool =
  match alignment a, alignment b with
  | Good, Good -> true
  | Evil, Evil -> true
  | _, _ -> false
;;

let opposed (a : t) (b : t) : bool = allied a b |> Bool.not
