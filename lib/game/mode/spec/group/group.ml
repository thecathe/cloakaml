(** @canonical Cloakaml.Game.Mode.Spec.Group *)

module type S = sig
  module Kind = Kind

  type kind = Kind.t
  type role
  type role_kind
  type role_alignment

  type t =
    | Role of role
    | Kind of role_kind
    | Alignment of role_alignment
  [@@deriving eq]

  val kind : t -> Kind.t
  val is_role_of_group : t -> role -> bool
end

module Make (R : Roles.S) :
  S
  with type role = R.Role.t
   and type role_kind = R.Kind.t
   and type role_alignment = R.Alignment.t = struct
  module Kind = Kind

  type kind = Kind.t
  type role = R.Role.t
  type role_kind = R.Kind.t
  type role_alignment = R.Alignment.t

  let equal_role = R.Role.equal
  let equal_role_kind = R.Kind.equal
  let equal_role_alignment = R.Alignment.equal

  type t =
    | Role of role
    | Kind of role_kind
    | Alignment of role_alignment
  [@@deriving eq]

  let kind : t -> kind = function
    | Role _ -> Role
    | Kind _ -> Kind
    | Alignment _ -> Alignment
  ;;

  let is_role_of_group (a : t) (x : role) : bool =
    match a with
    | Role b -> R.Role.equal b x
    | Kind b -> R.Kind.equal b (R.Role.kind x)
    | Alignment b -> R.Alignment.equal b (R.Role.alignment x)
  ;;

  (* let role (a:Kind.t)  : (role -> t -> bool) =
     match a with
     | Role -> fun x -> x
     | Kind ->  fun x -> fun y -> K.equal b (R.kind x)
     | Alignment -> R.alignment *)
end
