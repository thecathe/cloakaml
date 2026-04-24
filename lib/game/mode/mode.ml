(** @canonical Game.Mode *)

(** {1 Spec} *)

module Spec = Spec

(** {1 A Mode} *)

module type S = sig
  (* type role
  type kind
  type alignment *)
end

module Make (X : sig
    module Role : Spec.Roles.Role.S
  end) : S = struct
  (* open X

  type role = Role.t
  type kind = Role.kind
  type alignment = Role.alignment *)
end
