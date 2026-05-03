(** @canonical Cloakaml.Game.Mode.Spec.Roles.Alignment *)

module type S = sig
  include Enum_map.S
end

module type InputS = sig
  include Enum_map.InputS
end

module Make (X : InputS) : S with type t = X.t = struct
  include Enum_map.Make (X)
end
