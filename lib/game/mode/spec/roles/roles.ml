(** @canonical Cloakaml.Game.Mode.Spec.Roles *)

(** {1 Role} *)

(** {1 Attributes} *)

(** {1 Functor} *)

module type S = sig
  (* module Alignment : Role.Alignment.S *)
  (* module Kind : Role.Kind.S with type alignment = Alignment.t *)
  module Role : Role.S
  (* with type kind = Kind.t and type alignment = Alignment.t *)
  (* module Group : Group.S with type role = Role.t and type kind = Kind.t and type alignment = Alignment.t *)

  type role = Role.t
  type role_kind = Role.kind
  type role_alignment = Role.alignment
  type roles = role list

  val collect : unit -> roles
  val random : ?roles:roles -> unit -> role
  val of_kind : ?roles:roles -> role_kind -> roles
  val of_alignment : ?roles:roles -> role_alignment -> roles

  module Set : sig
    include Set.S with type elt = role

    val get : unit -> t
    val of_kind : ?roles:roles -> role_kind -> t
  end

  module Group :
    Group.S
    with type role = Role.t
     and type role_kind = Role.kind
     and type role_alignment = Role.alignment

  type group = Group.t
end

module type InputS = sig
  (* include Role.S *)
  (* module Alignment : Role.Alignment.S *)
  module Kind : Role.Kind.S
(* with type alignment = Alignment.t *)
  module Role : Role.InputS with type kind = Kind.t
  (* module Role : sig
     type t

     val kind_of_role : t -> Kind.t
     val alignment_of_kind : t -> Alignment.t
     end *)
end

module Make
    (* (A : Enum_type.InputS) *)
    (* (K : Enum_type.InputS) *)
    (* (R : Enum_type.InputS) *)
    (R : Role.S )
     (* (X :
       InputS
       (* with type role = R.t *)
       (* and type kind = K.t *)
       (* and type alignment = A.t *)) *)
        :

  S
  with
  module Role = R
  and type role = R.t


  (* type role = R.t  *)
  (* module Alignment = Role.Alignment.Make (A) *)
  (* and module Kind = Kind.Make (K) *)
  (* and module Role = Role. *)
  (* and type role_alignment = R.alignment *)
  (* and type role_kind = R.kind *) = struct
  (* module Alignment = Role.Alignment.Make (A) *)

  (* module Kind =
     Role.Kind.Make
     (Alignment)
     (struct
     include K

     type alignment = A.t

     let alignment : t -> alignment = X.alignment_of_kind
     end) *)

  module Role = R
  module Group = Group.Make (Role)

  type group = Group.t
  type role = Role.t
  type role_kind = Role.kind
  type role_alignment = Role.alignment

  (* (struct
     include R

     type kind = K.t
     type alignment = A.t

     let kind : t -> kind = X.kind_of_role
     end) *)

  (* module Group = Group.Make (Alignment) (Kind) (Role) *)

  (** {2 Roles} *)

  type roles = role list

  let collect = Role.collect

  let random ?(roles : roles = collect ()) () : role =
    Random.self_init ();
    List.nth roles (Random.int (List.length roles))
  ;;

  (** {3 Roles of Kind} *)

  let of_kind ?(roles : roles = collect ()) (x : role_kind) : roles =
    List.filter (Role.is_kind x) roles
  ;;

  let of_alignment ?(roles : roles = collect ()) (x : role_alignment) : roles =
    List.filter (Role.is_alignment x) roles
  ;;

  (** {3 Set of Roles} *)
  module Set = struct
    include Set.Make (Role)

    let get () : t = collect () |> of_list

    let of_kind ?(roles : roles = collect ()) (x : role_kind) : t =
      of_kind ~roles x |> of_list
    ;;
  end
end
