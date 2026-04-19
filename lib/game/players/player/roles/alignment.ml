type t =
  | Good
  | Evil
[@@deriving show { with_path = false }, eq]
