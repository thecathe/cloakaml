(** @canonical Cloakaml.Game.Spec.Roles *)

(** {1 Role} *)

module Role = Role
module Group = Group

(** {1 Attributes} *)

(** {1 Functor} *)

module type S = sig
  module Role : Role.S

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

module Make (R : Role.S) : S with module Role = R and type role = R.t = struct
  module Role = R
  type role = Role.t
  type role_kind = Role.kind
  type role_alignment = Role.alignment

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

  (** {3 Group of Roles} *)

  module Group = Group.Make (Role)

  type group = Group.t
end
