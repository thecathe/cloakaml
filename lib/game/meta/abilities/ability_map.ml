(** @canonical Game.Meta.Abilities.AbilityMap *)

open Ability

include Hashtbl.Make (Trigger) (** @closed *)

type t' = f t

let add_all (map : t') (xs : map list) : t' =
  xs |> List.to_seq |> add_seq map;
  map
;;

let of_role ?(map : t' = create 0) (x : Roles.Role.t) : t' =
  let module R : S = (val make x) in
  R.get () |> add_all map
;;

let make ?(map : t' = create 0) (x : Player.t) : t' = of_role ~map x.role

let has_kind (map : t') (x : Trigger.Kind.t) : bool =
  to_seq map |> List.of_seq |> List.exists (fun (k, _) -> Trigger.is_kind x k)
;;

let kind_opt (x : Trigger.Kind.t) : key -> f -> f option =
  fun k v -> if Trigger.is_kind x k then Some v else None
;;

let reduce_by_kind (map : t') (x : Trigger.Kind.t) : unit =
  filter_map_inplace (kind_opt x) map
;;
