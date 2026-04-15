(** {1 Player Type} *)

module Status = struct
  type t =
    | Alive
    | Dead
    | Poisoned
  [@@deriving show { with_path = false }, eq]

  let initial : t = Alive
  let poisoned : t -> bool = function Poisoned -> true | _ -> false
  let alive : t -> bool = function Alive -> true | x -> poisoned x
  let dead : t -> bool = function Dead -> true | _ -> false
end

(* NOTE: maybe useful for automatically attaching events to happen to players themselves. e.g., poisoned for one day would mean that "NextDay" their status would be reverted? *)
(* module HookMap = Hashtbl.Make (struct
  type t = Abilities.Ability.Trigger.t
 
  exception ToDoHookMap

  (* TODO: hash *)
  let hash x : int = raise ToDoHookMap

  let equal a b : bool = Abilities.Ability.Trigger.equal a b

end) *)

type t =
  { index : int
  ; mutable role : Roles.t
  ; mutable status : Status.t (* ; hooks : HookMap.t *)
  }
[@@deriving show { with_path = false }]

let index (x:t) : int = x.index
let role (x:t) : Roles.t = x.role
let status (x:t) : Status.t = x.status

let show (x : t) : string =
  Printf.sprintf
    "%s (%i, %s)"
    (Roles.show x.role)
    x.index
    (Status.show x.status)
;;

(** [create index role] ... *)
let create (index : int) (role : Roles.t) : t =
  { index; role; status = Status.initial }
;;

let replace_role (x:t) (y:Roles.t) : t = 
  x.role <- y; x
;;

(** {2 Equality & Comparison} *)

let compare (a : t) (b : t) : int = Int.compare a.index b.index
let equal (a : t) (b : t) : bool = Int.equal a.index b.index

(** {2 Query Functions} *)

(** {3 Status} *)

let alive (x : t) : bool = Status.alive x.status
let dead (x : t) : bool = Status.dead x.status
let poisoned (x : t) : bool = Status.poisoned x.status
let status (x : Status.t) (y : t) : bool = Status.equal x y.status

(** {3 Allegiance} *)

let allied (a : t) (b : t) : bool = Roles.allied a.role b.role
let opposed (a : t) (b : t) : bool = Roles.opposed a.role b.role


(* let ability_triggers (x : t) : Abilities.Triggers.t =
  Abilities.Triggers.get x.role
;; *)

module Map : Hashtbl.S with type key = t = Hashtbl.Make (struct
    type k = t
    type t = k

    let hash (x : t) = Int.hash x.index
    let equal (a : t) (b : t) : bool = Int.equal a.index b.index
  end)

exception CannotPoisonDeadPerson

let poison (x : t) : unit =
  if dead x then raise CannotPoisonDeadPerson;
  x.status <- Poisoned
;;
