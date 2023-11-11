module Base = Ident 

type ident_t = Base.t [@opaque]

[@@deriving visitors {variety = "iter"; nude = true}]