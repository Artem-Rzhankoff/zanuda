  $ dune build
  $ zanuda -fix .
  File "IfBool.ml", line 2, characters 11-14:
  2 | let _f x = if x then true else false
                 ^^^
  Alert zanuda-linter: (Fix `If_bool` lint):
  This boolean expression will be replaced by an equivalent with removing unwise`if_then_else`
  
  File "IfBool.ml", line 2, characters 15-36:
  2 | let _f x = if x then true else false
                     ^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: (Fix `If_bool` lint):
  This boolean expression will be replaced by an equivalent with removing unwise`if_then_else`
  
  File "IfBool.ml", line 6, characters 9-22:
  6 | let _f = if true then false else true
               ^^^^^^^^^^^^^
  Alert zanuda-linter: (Fix `If_bool` lint):
  This boolean expression will be replaced by an equivalent with removing unwise`if_then_else`
  
  File "IfBool.ml", line 6, characters 27-37:
  6 | let _f = if true then false else true
                                 ^^^^^^^^^^
  Alert zanuda-linter: (Fix `If_bool` lint):
  This boolean expression will be replaced by an equivalent with removing unwise`if_then_else`
  
  File "IfBool.ml", line 8, characters 26-34:
  8 | let _unwise_bool_exp x = x && true
                                ^^^^^^^^
  Alert zanuda-linter: (Fix `If_bool` lint)
  This boolean expression will be replaced by an equivalent with removing unwise conjunction
  
