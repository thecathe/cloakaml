open Ability

module P = struct
  let has_kind (y : Trigger.Kind.t) (x : Player.t) : bool =
    role_has_kind y x.role
  ;;

  let has_any_kinds (ys : Trigger.Kind.t list) (x : Player.t) : bool =
    List.exists (fun y -> has_kind y x) ys
  ;;
end

module Ps = struct
  (** shadowed *)
  let have_kind (y : Trigger.Kind.t) : Player.t list -> Player.t list =
    List.filter (P.has_kind y)
  ;;

  (** shadowing *)
  let have_kind (y : Trigger.Kind.t) (xs : Players.t) : Players.t =
    Players.to_list xs |> have_kind y |> Players.of_list
  ;;

  let have_any_kinds (ys : Trigger.Kind.t list) (xs : Players.t) : Players.t =
    Players.to_list xs |> List.filter (P.has_any_kinds ys) |> Players.of_list
  ;;

  let with_kinds_of_ability (ys : Trigger.Kind.t list) (x : Round.t) : Players.t
    =
    have_any_kinds ys x.players
  ;;
end

module Player = P
module Players = Ps
