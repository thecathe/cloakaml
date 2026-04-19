module Kind = Kind
module Alignment = Alignment
module Role = Role
module Group = Group
module Map = Map

(** {1 {!Role.t} lists} *)

(**  collect [roles] ... (* TODO: can we cache this? some reference wrapper where [let |() = ...] programmatically sets it when module first loaded? *)
*)
let collect : Role.t list =
  List.init (Role.max + 1) (fun n ->
    match Role.of_enum n with Some x -> x | None -> raise Role.EnumOutOfBounds)
;;

let townsfolk : Role.t list = List.filter Role.is_townfolk collect
let outsiders : Role.t list = List.filter Role.is_outsider collect
let minions : Role.t list = List.filter Role.is_minion collect
let demons : Role.t list = List.filter Role.is_demon collect

exception KindNotImplemented of Kind.t

let of_kind : Kind.t -> Role.t list = function
  | Townsfolk -> townsfolk
  | Outsider -> outsiders
  | Minion -> minions
  | Demon -> demons
  | x -> raise (KindNotImplemented x)
;;

let random_kind (x : Kind.t) : Role.t =
  Random.self_init ();
  let xs = of_kind x in
  List.nth xs (Random.int (List.length xs))
;;

(** {1 {!Role.t} Sets} *)

include Set.Make (Role) (** @closed *)

(** wrapper for {!collect} *)
let get () : t = collect |> of_list

let of_kind (x : Kind.t) : t = of_kind x |> of_list
