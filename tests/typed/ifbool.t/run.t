  $ dune build
  $ zanuda  -no-check-filesystem -no-top_file_license -dir .  -ordjsonl /dev/null
  File "IfBool.ml", line 2, characters 2-15:
  2 |   if true then 1 else 2
        ^^^^^^^^^^^^^
  Alert zanuda-linter: (Fix `If_bool` lint):
  This boolean expression will be replaced by an equivalent with removing unwise`if_then_else`
  
  File "IfBool.ml", line 2, characters 16-23:
  2 |   if true then 1 else 2
                      ^^^^^^^
  Alert zanuda-linter: (Fix `If_bool` lint):
  This boolean expression will be replaced by an equivalent with removing unwise`if_then_else`
  
  File "IfBool.ml", line 8, characters 20-28:
  8 |   | _::xs -> foo1 (r && true) xs
                          ^^^^^^^^
  Alert zanuda-linter: (Fix `If_bool` lint)
  This boolean expression will be replaced by an equivalent with removing unwise conjunction
  
  File "IfBool.ml", line 9, characters 11-16:
  9 |   | _ ->  (r && false)
                 ^^^^^
  Alert zanuda-linter: (Fix `If_bool` lint)
  This boolean expression will be replaced by an equivalent with removing unwise conjunction
  
  File "IfBool.ml", line 2, characters 2-23:
  2 |   if true then 1 else 2
        ^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Executing 'if true' smells bad
  
  File "IfBool.ml", line 4, characters 14-39:
  4 | let __ f x  = if f x then true else f x
                    ^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Executing 'if ... then true' smells bad
  
  File "IfBool.ml", line 5, characters 14-40:
  5 | let __ f x  = if f x then f x else false
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Executing 'if ... then .. else false' smells bad
  
  File "IfBool.ml", line 8, characters 18-29:
  8 |   | _::xs -> foo1 (r && true) xs
                        ^^^^^^^^^^^
  Alert zanuda-linter: Conjunction with boolean smells bad
  
  File "IfBool.ml", line 9, characters 10-22:
  9 |   | _ ->  (r && false)
                ^^^^^^^^^^^^
  Alert zanuda-linter: Conjunction with boolean smells bad
  
  1,15d0
  < let __ () =
  <   if true then 1 else 2
  < 
  < let __ f x  = if f x then true else f x
  < let __ f x  = if f x then f x else false
  < 
  < let rec foo1 r = function
  <   | _::xs -> foo1 (r && true) xs
  <   | _ ->  (r && false)
  < 
  < 
  < type substring =
  <   { name : int
  <   }
  < [@@deriving fields]
  damn
