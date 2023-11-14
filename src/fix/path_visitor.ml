module Base = Path

type path_t = Base.t =
  | Pident of Ident_visitor.ident_t
  | Pdot of path_t * (string[@opaque])
  | Papply of path_t * path_t
[@@deriving
  visitors { variety = "iter"; ancestors = [ "Ident_visitor.iter" ]; nude = true }]
