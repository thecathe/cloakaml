(** @canonical Game.Meta.Abilities.Query *)

open Ability

module Player = struct
  let has_kind (y : Trigger.Kind.t) (x : Players.Player.t) : bool =
    role_has_kind y x.role
  ;;

  let has_any_kinds (ys : Trigger.Kind.t list) (x : Players.Player.t) : bool =
    List.exists (fun y -> has_kind y x) ys
  ;;
end

module Players = struct
  (** shadowed *)
  let have_kind (y : Trigger.Kind.t)
    : Players.Player.t list -> Players.Player.t list
    =
    List.filter (Player.has_kind y)
  ;;

  (** shadowing *)
  let have_kind (y : Trigger.Kind.t) (xs : Players.t) : Players.t =
    Players.to_list xs |> have_kind y |> Players.of_list
  ;;

  let have_any_kinds (ys : Trigger.Kind.t list) (xs : Players.t) : Players.t =
    Players.to_list xs
    |> List.filter (Player.has_any_kinds ys)
    |> Players.of_list
  ;;

  let with_kinds_of_ability (ys : Trigger.Kind.t list) (x : Round.t) : Players.t
    =
    have_any_kinds ys x.players
  ;;
end
