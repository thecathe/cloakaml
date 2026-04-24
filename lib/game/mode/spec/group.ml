(** @canonical Game.Mode.Spec.Group *)

module type S = sig
  type role
  type kind
  type alignment

  type t =
    | Role of role
    | Kind of kind
    | Alignment of alignment
  [@@deriving eq]
end

module Make
    (A : Roles.Alignment.S)
    (K : Roles.Kind.S with type alignment = A.t)
    (R : Roles.Role.S with type kind = K.t and type alignment = A.t) :
  S with type role = R.t and type kind = K.t and type alignment = A.t = struct
  type role = R.t
  type kind = K.t
  type alignment = A.t

  let equal_alignment = A.equal
  let equal_kind = K.equal
  let equal_role = R.equal

  type t =
    | Role of role
    | Kind of kind
    | Alignment of alignment
  [@@deriving eq]
end
