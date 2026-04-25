(** @canonical Game.Mode.Spec.Roles *)

(** {1 Role} *)

(** {1 Attributes} *)

module Alignment = Alignment
module Kind = Kind
module Role = Role

(** {1 Functor} *)

module type S = sig
  module Alignment : Alignment.S
  module Kind : Kind.S with type alignment = Alignment.t
  module Role : Role.S with type kind = Kind.t and type alignment = Alignment.t

  type roles = Role.t list

  val collect : unit -> roles
  val random : ?roles:roles -> unit -> Role.t
  val of_kind : ?roles:roles -> Kind.t -> roles

  module Set : sig
    include Set.S with type elt = Role.t

    val get : unit -> t
    val of_kind : ?roles:roles -> Kind.t -> t
  end
end

module Make
    (A : Enum_type.InputS)
    (K : Enum_type.InputS)
    (R : Enum_type.InputS)
    (X : sig
           type role
           type kind
           type alignment

           val alignment_of_kind : kind -> alignment
           val kind_of_role : role -> kind
         end
         with type role = R.t
          and type kind = K.t
          and type alignment = A.t) : S = struct
  module Alignment = Alignment.Make (A)

  module Kind =
    Kind.Make
      (Alignment)
      (struct
        include K

        type alignment = A.t

        let alignment : t -> alignment = X.alignment_of_kind
      end)

  module Role =
    Role.Make (Alignment) (Kind)
      (struct
        include R

        type kind = K.t
        type alignment = A.t

        let kind : t -> kind = X.kind_of_role
      end)

  (** {2 Roles} *)

  type roles = Role.t list

  let collect = Role.collect

  let random ?(roles : roles = collect ()) () : Role.t =
    Random.self_init ();
    List.nth roles (Random.int (List.length roles))
  ;;

  (** {3 Roles of Kind} *)

  let of_kind ?(roles : roles = collect ()) (x : Kind.t) : roles =
    List.filter (Role.is_kind x) roles
  ;;

  (** {3 Set of Roles} *)
  module Set = struct
    include Set.Make (Role)

    let get () : t = collect () |> of_list

    let of_kind ?(roles : roles = collect ()) (x : Kind.t) : t =
      of_kind ~roles x |> of_list
    ;;
  end
end
