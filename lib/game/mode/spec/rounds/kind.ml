(** @canonical Game.Mode.Spec.Rounds.Kind *)

type ('data, 'round) t =
  | Initial of 'data
  | Round of 'round

module type S = sig
  type data
  type round
  type nonrec t = (data, round) t
end

module Make (R : Round.S) (D : Data.S) :
  S with type data = D.t and type round = R.t = struct
  type data = D.t
  type round = R.t
  type nonrec t = (data, round) t


end
