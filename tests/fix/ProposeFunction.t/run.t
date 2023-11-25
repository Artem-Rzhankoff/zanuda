  $ dune build
  File "ProposeFunction.ml", line 6, characters 7-8:
  6 | let _f x y = 
             ^
  Error (warning 27 [unused-var-strict]): unused variable x.
  [1]
  $ zanuda -fix .
  File "ProposeFunction.ml", lines 7-8, characters 10-4:
  7 | ..........with
  8 |   | .........
  Alert zanuda-linter: (Fix `Propose_function` lint)
  This pattern matching will be replaced by an equivalent and more concise construction `function`
  
  File "ProposeFunction.ml", line 7, characters 2-10:
  7 |   match y with
        ^^^^^^^^
  Alert zanuda-linter: (Fix `Propose_function` lint)
  This pattern matching will be replaced by an equivalent and more concise construction `function`
  
  File "ProposeFunction.ml", line 6, characters 9-10:
  6 | let _f x y = 
               ^
  Alert zanuda-linter: (Fix `Propose_function` lint)
  Removing extra argument of an expression with replacing `match .. with ` to `function`
  
  Fatal error: exception Sys_error("ProposeFunction.ml: Permission denied")
  Raised by primitive operation at Stdlib.open_out_gen in file "stdlib.ml", line 331, characters 29-55
  Called from Stdlib.open_out in file "stdlib.ml" (inlined), line 336, characters 2-74
  Called from Replacement__Repl.apply_all.(fun) in file "src/fix/replacement/repl.ml", line 144, characters 15-29
  Called from Stdlib__List.iter in file "list.ml", line 110, characters 12-15
  Called from Dune__exe__Main in file "src/main.ml", line 234, characters 6-143
  [2]
