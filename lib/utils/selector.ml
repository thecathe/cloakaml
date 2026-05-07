type 'a t =
  | All
  | None
  | Filter of ('a -> bool)
  | Union of 'a t * 'a t
  | Intersect of 'a t * 'a t

(* let equal (type a) (f:a -> a -> bool) (x:a t) (y:a t) : bool =
  match x, y with
  | All, All | None, None -> true
  | Filter f, Filter g ->  *)