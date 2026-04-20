(** @canonical Game.Abilities.Ability *)

(** Types *)
include Types
(** @closed *)

(** Sig *)
include Sig

(** Trigger *)
module Trigger = Trigger

let has_kind (x : Trigger.Kind.t) (module X : S) : bool = X.has_kind x

let filter_by_kind (x : Trigger.Kind.t) : t list -> t list =
  List.filter (has_kind x)
;;

let role (module X : S) : Roles.Role.t = X.x

(** {1 Maker} *)

module Make (X : sig
    val x : Roles.Role.t
    val abilities : map list
  end) : S = struct
  (** ... *)
  let x : Roles.Role.t = X.x

  (** ... *)
  let get () : map list = X.abilities

  (** ... *)
  let triggers () : Trigger.t list = get () |> List.map fst

  (** ... *)
  let has_kind (x : Trigger.Kind.t) : bool =
    triggers () |> List.exists (Trigger.is_kind x)
  ;;
end

let make (x : Roles.Role.t) : t =
  (module Make (struct
       let x = x
       let abilities = Of_role.get x
     end) : S)
;;

let get = make
let of_player (x : Player.t) : t = make x.role

exception NoneRoles

let of_role_enum (x : int) : t =
  match Roles.Role.of_enum x with None -> raise NoneRoles | Some x -> make x
;;

let role_has_kind (x : Trigger.Kind.t) (y : Roles.Role.t) : bool =
  make y |> has_kind x
;;
