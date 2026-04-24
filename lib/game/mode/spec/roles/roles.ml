(** @canonical Game.Mode.Spec.Roles *)

(** {1 Role} *)

(** {1 Attributes} *)

module Alignment = Alignment
module Kind = Kind
module Role = Role

(** {1 Functor} *)

module type S = sig
  module A : Alignment.S
  module K : Kind.S with type alignment = A.t
  module R : Role.S with type kind = K.t and type alignment = A.t
end

module Make
    (A : Enum_type.S)
    (K : Enum_type.S)
    (R : Enum_type.S)
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
  module A = Alignment.Make (A)

  module K =
    Kind.Make
      (A)
      (struct
        include K

        type alignment = A.t

        let alignment : t -> alignment = X.alignment_of_kind
      end)

  module R =
    Role.Make (A) (K)
      (struct
        include R

        type kind = K.t
        type alignment = A.t

        let kind : t -> kind = X.kind_of_role
      end)
end
