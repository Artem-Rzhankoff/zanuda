  $ dune build
  File "ProposeFunction.ml", line 6, characters 7-8:
  6 | let _f x y = 
             ^
  Error (warning 27 [unused-var-strict]): unused variable x.
  [1]
  $ zanuda -dir .
  File "ProposeFunction.ml", lines 2-3, characters 10-4:
  2 | ..........with
  3 |   | .........
  Alert zanuda-linter: (Fix `Propose_function` lint)
  This pattern matching will be replaced by an equivalent and more concise construction `function`
  
  File "ProposeFunction.ml", line 2, characters 2-10:
  2 |   match x with
        ^^^^^^^^
  Alert zanuda-linter: (Fix `Propose_function` lint)
  This pattern matching will be replaced by an equivalent and more concise construction `function`
  
  File "ProposeFunction.ml", line 1, characters 9-11:
  1 | let _f x _y = 
               ^^
  Alert zanuda-linter: (Fix `Propose_function` lint)
  Removing extra argument of an expression with replacing `match .. with ` to `function`
  
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
  
  File '_build/default/ProposeFunction.ml' doesn't have corresponding .mli interface
  
  File "ProposeFunction.ml", lines 6-9, characters 9-13:
  6 | .........y = 
  7 |   match y with
  8 |   | true -> 1
  9 |   |false -> 0
  Alert zanuda-linter: Using `function` is recommended
  File "ProposeFunction.ml", lines 1-4, characters 0-14:
  1 | let _f x _y = 
  2 |   match x with
  3 |   | true -> 1
  4 |   | false -> 0
  Alert zanuda-linter:  First item in file should be a documentation comment with copyright information. For example:
              (** Copyright 2021-2022, Winnie Pooh et al. *)
  File "ProposeFunction.ml", lines 6-9, characters 0-13:
  6 | let _f x y = 
  7 |   match y with
  8 |   | true -> 1
  9 |   |false -> 0
  Alert zanuda-linter: Second item in file should be a documentation comment with correct license information. For example:
             (** SPDX-License-Identifier: LGPL-3.0-or-later *)
  File "ProposeFunction.ml", lines 6-9, characters 9-13:
  6 | .........y = 
  7 |   match y with
  8 |   | true -> 1
  9 |   |false -> 0
  Alert zanuda-linter: Using `function` is recommended
  1,9d0
  < let _f x _y = 
  <   match x with
  <   | true -> 1
  <   | false -> 0
  < 
  < let _f x y = 
  <   match y with
  <   | true -> 1
  <   |false -> 0
  \ No newline at end of file
  damn
