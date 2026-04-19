type t =
  { left : Player.t
  ; right : Player.t
  }
(* [@@deriving show { with_path = false }] *)

let show (x : t) : string =
  Printf.sprintf
    "{ left: %s\n; right: %s\n}\n"
    (Player.show x.left)
    (Player.show x.right)
;;

exception NoNeighboursFound
exception InvalidPlayerIndex
exception RootPlayerNotFound of Player.t

let find (x : Player.t) (ys : Player.t list) : t =
  match List.find_index (Player.equal x) ys with
  | None -> raise (RootPlayerNotFound x)
  | Some offset ->
    let get : int -> Player.t =
      try List.nth ys with Invalid_argument _ -> raise InvalidPlayerIndex
    in
    let max : int = List.length ys in
    let wraparound (n : int) : int =
      let n' : int = n mod max in
      if n' < 0 then max - 1 else n'
    in
    let seek (dir : int) : Player.t = get (wraparound (offset + dir)) in
    let left : Player.t = seek (-1) in
    let right : Player.t = seek 1 in
    let nobody : Player.t -> bool = Player.equal x in
    if nobody left && nobody right then raise NoNeighboursFound;
    { left; right }
;;

(** ... if [rooted] is [true] then any {!RootPlayerNotFound} exceptions are propagated.
*)
let find_opt ?(rooted : bool = true) (x : Player.t) (ys : Player.t list)
  : t option
  =
  try Some (find x ys) with
  | NoNeighboursFound -> None
  | RootPlayerNotFound (x : Player.t) ->
    if rooted then raise (RootPlayerNotFound x) else None
;;

let num_aligned (a : Roles.Alignment.t) (x : Player.t) (ys : Player.t list)
  : int
  =
  match find_opt x ys with
  | None -> 0
  | Some { left; right } ->
    let f (z : Player.t) : int =
      if Roles.Role.alignment z.role |> Roles.Alignment.equal a then 1 else 0
    in
    f left + f right
;;
