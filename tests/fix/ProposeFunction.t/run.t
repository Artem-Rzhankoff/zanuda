  $ dune build
  File "ProposeFunction.ml", line 6, characters 7-8:
  6 | let _f x y = 
             ^
  Error (warning 27 [unused-var-strict]): unused variable x.
  [1]
  $ zanuda -dir .
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
  
