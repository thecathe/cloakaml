(** @canonical Game.Mode.Spec.Role.Alignment *)

module type S = sig
  include Enum_map.S
end

module type InputS = Enum_map.InputS

module Make (X : Enum_map.InputS) : S with type t = X.t = struct
  include Enum_map.Make (X)
end
