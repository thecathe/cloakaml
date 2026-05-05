type 'a t =
  | All
  | None
  | Filter of ('a -> bool)
  | Union of 'a t * 'a t
  | Intersect of 'a t * 'a t
