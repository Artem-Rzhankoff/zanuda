module Base = Longident

type longident_t = Base.t =
  | Lident of (string[@opaque])
  | Ldot of longident_t * (string[@opaque])
  | Lapply of longident_t * longident_t
[@@deriving visitors { variety = "iter"; nude = true }]
