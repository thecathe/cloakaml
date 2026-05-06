(** @canonical Cloakaml.Game.Mode.Spec.Role.Group *)

module Kind = Kind

module type S = sig
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

module Make (R : Role.S) :
  S
  with type role = R.t
   and type role_kind = R.kind
   and type role_alignment = R.alignment = struct
  type kind = Kind.t
  type role = R.t
  type role_kind = R.kind
  type role_alignment = R.alignment

  let equal_role = R.equal
  let equal_role_kind = R.equal_kind
  let equal_role_alignment = R.equal_alignment

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
    | Role b -> equal_role b x
    | Kind b -> equal_role_kind b (R.kind x)
    | Alignment b -> equal_role_alignment b (R.alignment x)
  ;;

  (* let role (a:Kind.t)  : (role -> t -> bool) =
     match a with
     | Role -> fun x -> x
     | Kind ->  fun x -> fun y -> K.equal b (R.kind x)
     | Alignment -> R.alignment *)
end

