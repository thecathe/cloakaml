(** @canonical Cloakaml.Game.Mode.Spec.Players *)

module type S = sig
  (* include Enum_map.S *)

  type role
  type role_kind
  type role_alignment
  type group
  type player
  type player_status
  type player_knowledge

  module Map : Hashtbl.S with type key = player

  include Set.S with type elt = player [@@deriving show { with_path = false }]
  (** @closed *)

  val create : player list -> t
  val add_role : role -> t -> t

  exception CouldNotFindValidPlayer

  val random : ?f:(t -> t) -> t -> player
  val filterf : (player -> bool) -> t -> t
  val have_status : player_status -> t -> t
  val have_knowledge : player_knowledge -> t -> t
  val have_group : group -> t -> t
  val kind : role_kind -> t -> t
  val aligned : role_alignment -> t -> t
end

module type InputS = sig
  include Enum_map.InputS
end

module Make
    (I : Index.S)
    (R : Roles.S)
    (G :
       Group.S
       with type role = R.Role.t
        and type role_kind = R.Kind.t
        and type role_alignment = R.Alignment.t)
    (P :
       Player.S
       with type index = I.t
        and type role = R.Role.t
        and type role_kind = R.Kind.t
        and type role_alignment = R.Alignment.t
       (* and type status = S.t *)
       (* and type knowledge = K.t *)
        and type group = G.t)
    (X : InputS) :
  S
  with type role = R.Role.t
   and type role_kind = R.Kind.t
   and type role_alignment = R.Alignment.t
   and type group = G.t
   and type player = P.t
   and type player_status = P.status
   and type player_knowledge = P.knowledge = struct
  (* include Enum_map.Make (Xand
     ) *)

  type role = R.Role.t
  type role_kind = R.Kind.t
  type role_alignment = R.Alignment.t
  type group = G.t
  type player = P.t
  type player_status = P.status
  type player_knowledge = P.knowledge

  (** {1 Map of Players} *)

  module Map : Hashtbl.S with type key = P.t = Hashtbl.Make (P)

  (** {1 Set of Players} *)

  include (Set.Make (P) : Set.S with type elt = P.t) [@@deriving
                                                       show
                                                         { with_path = false }]
  (** @closed *)

  (** alias for [Set.of_list] *)
  let create : player list -> t = of_list

  (** [add_role x ys] takes a {!Roles.Role.t} [x] and makes a fresh {!Player.t} with [index] equal to {!cardinal}.
  *)
  let add_role (x : role) (ys : t) : t =
    add (P.create (cardinal ys |> I.of_int) x) ys
  ;;

  exception CouldNotFindValidPlayer

  let random ?(f : (t -> t) option) (xs : t) : player =
    let xs : P.t list =
      Option.fold ~none:xs ~some:(fun f -> f xs) f |> to_list
    in
    Random.self_init ();
    try List.nth xs (Random.int (List.length xs)) with
    | Failure _ -> raise CouldNotFindValidPlayer
  ;;

  (** {2 Filter Functions} *)

  let filterf (f : P.t -> bool) (xs : t) : t =
    to_list xs |> List.filter f |> of_list
  ;;

  let have_status (a : P.status) : t -> t = filterf (P.has_status a)
  let have_knowledge (a : P.knowledge) : t -> t = filterf (P.has_knowledge a)
  let have_group (group : G.t) : t -> t = filterf (P.has_group group)

  let kind (kind : R.Kind.t) (xs : t) : t =
    (* filterf (fun (z : P.t) -> R.Kind.equal kind (R.Role.kind z.role)) xs *)
    have_group (Kind kind) xs
  ;;

  let aligned (alignment : R.Alignment.t) (xs : t) : t =
    (* filterf
       (fun (z : P.t) -> R.Alignment.equal alignment (R.Role.alignment z.role))
       xs *)
    have_group (Alignment alignment) xs
  ;;
end
