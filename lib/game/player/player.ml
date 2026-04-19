module Roles = Roles
module Status = Status
module Knowledge = Knowledge

(** {1 Player Type} *)


type t =
  { index : int
  ; mutable role : Roles.Role.t
  ; mutable status : Status.t
  ; knowledge : Knowledge.t list (* ; hooks : HookMap.t *)
  }
(* [@@deriving show { with_path = false }] *)

let show (x : t) : string =
  Printf.sprintf
    "%s (%i, %s)"
    (Roles.Role.show x.role)
    x.index
    (Status.show x.status)
;;

let index (x : t) : int = x.index
let role (x : t) : Roles.Role.t = x.role
let status (x : t) : Status.t = x.status

(** [create index role] ... *)
let create (index : int) (role : Roles.Role.t) : t =
  { index; role; status = Status.initial; knowledge = [] }
;;

let replace_role (x : t) (y : Roles.Role.t) : t =
  x.role <- y;
  x
;;

(** {2 Equality & Comparison} *)

let compare (a : t) (b : t) : int = Int.compare a.index b.index
let equal (a : t) (b : t) : bool = Int.equal a.index b.index
let hash (x : t) = Int.hash x.index

(** {2 Query Functions} *)

(** {3 Status} *)

let alive (x : t) : bool = Status.alive x.status
let dead (x : t) : bool = Status.dead x.status
let poisoned (x : t) : bool = Status.poisoned x.status
let status (x : Status.t) (y : t) : bool = Status.equal x y.status

(** {3 Allegiance} *)

let allied (a : t) (b : t) : bool = Roles.Role.allied a.role b.role
let opposed (a : t) (b : t) : bool = Roles.Role.opposed a.role b.role

(* let ability_triggers (x : t) : Abilities.Triggers.t =
   Abilities.Triggers.get x.role
   ;; *)

exception CannotPoisonDeadPerson

let poison (x : t) : unit =
  if dead x then raise CannotPoisonDeadPerson;
  x.status <- Poisoned
;;
