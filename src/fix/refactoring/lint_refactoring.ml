open Base
open Lexing
open Visitors

class ['self] result_iter =
  object
    inherit ['self] Typedtree_visitor.iter
    method! visit_array _f _env _xs = ()
    (* тут наверное определить visit_expression, где будут находить выражение по локе*)
  end

let a = if true then true else false

module type REFACTORING = sig
  val visitor
    : < visit_Closed : Location.t -> _
      ; visit_tt_case :
          'a.
          (Location.t -> 'a -> unit) -> Location.t -> 'a Typedtree_visitor.tt_case -> unit
      ; .. >
        result_iter
end