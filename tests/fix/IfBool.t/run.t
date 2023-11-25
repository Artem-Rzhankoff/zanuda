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
  
  Fatal error: exception Sys_error("IfBool.ml: Permission denied")
  Raised by primitive operation at Stdlib.open_out_gen in file "stdlib.ml", line 331, characters 29-55
  Called from Stdlib.open_out in file "stdlib.ml" (inlined), line 336, characters 2-74
  Called from Replacement__Repl.apply_all.(fun) in file "src/fix/replacement/repl.ml", line 144, characters 15-29
  Called from Stdlib__List.iter in file "list.ml", line 110, characters 12-15
  Called from Dune__exe__Main in file "src/main.ml", line 234, characters 6-143
  [2]
