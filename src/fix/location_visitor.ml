module Base = Location

type location_t = Base.t = {
  loc_start: Lexing_visitor.position;
  loc_end: Lexing_visitor.position;
  loc_ghost: bool;
}
and 'a loc = 'a Base.loc = {
  txt : 'a;
  loc : location_t;
} (* добавил nude, потому что без него, как я понял, базовые методы (пр. visit_int) наследовались от VisitorsRuntime по умолчанию, переопределяя при этом такие же методы в Lexing_visitor.iter (странно, но ок)*)
[@@deriving visitors {variety = "iter"; polymorphic = true; monomorphic = ["'env"]; ancestors = [ "Lexing_visitor.iter"]; nude = true}]

(* какая-то трабла с visit_position *)