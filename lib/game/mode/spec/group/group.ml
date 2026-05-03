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

module Make
    (A : Roles.Alignment.S)
    (K : Roles.Kind.S with type alignment = A.t)
    (R : Roles.Role.S with type kind = K.t and type alignment = A.t) :
  S with type role = R.t and type role_kind = K.t and type role_alignment = A.t =
struct
  module Kind = Kind

  type kind = Kind.t
  type role = R.t
  type role_kind = K.t
  type role_alignment = A.t

  let equal_role = R.equal
  let equal_role_kind = K.equal
  let equal_role_alignment = A.equal

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
    | Role b -> R.equal b x
    | Kind b -> K.equal b (R.kind x)
    | Alignment b -> A.equal b (R.alignment x)
  ;;

  (* let role (a:Kind.t)  : (role -> t -> bool) =
     match a with
     | Role -> fun x -> x
     | Kind ->  fun x -> fun y -> K.equal b (R.kind x)
     | Alignment -> R.alignment *)
end
