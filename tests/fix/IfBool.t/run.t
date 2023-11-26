  $ dune build
  $ zanuda -dir .
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
  
  File '_build/default/IfBool.ml' doesn't have corresponding .mli interface
  
  File "IfBool.ml", line 2, characters 0-36:
  2 | let _f x = if x then true else false
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter:  First item in file should be a documentation comment with copyright information. For example:
              (** Copyright 2021-2022, Winnie Pooh et al. *)
  File "IfBool.ml", line 4, characters 0-36:
  4 | let _f x = if x then false else true
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Second item in file should be a documentation comment with correct license information. For example:
             (** SPDX-License-Identifier: LGPL-3.0-or-later *)
  File "IfBool.ml", line 2, characters 11-36:
  2 | let _f x = if x then true else false
                 ^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Executing 'if ... then true' smells bad
  
  File "IfBool.ml", line 4, characters 11-36:
  4 | let _f x = if x then false else true
                 ^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Executing 'if ... then false' smells bad
  
  File "IfBool.ml", line 6, characters 9-37:
  6 | let _f = if true then false else true
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Executing 'if true' smells bad
  
  File "IfBool.ml", line 8, characters 25-34:
  8 | let _unwise_bool_exp x = x && true
                               ^^^^^^^^^
  Alert zanuda-linter: Conjunction with boolean smells bad
  
  1,9d0
  < 
  < let _f x = if x then true else false
  < 
  < let _f x = if x then false else true
  < 
  < let _f = if true then false else true
  < 
  < let _unwise_bool_exp x = x && true
  < 
  damn
