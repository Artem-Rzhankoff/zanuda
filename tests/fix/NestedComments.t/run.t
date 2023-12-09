  $ dune build
  $ zanuda -dir . > /dev/null 
  $ cat fix_gen/diffs.log
  Diffs for file BugDemo.ml
  3c3
  < let f x = match (* lvl1 (* lvl2*)*) x with true -> 0 | false -> 1
  ---
  > let f  = function   (* lvl1 (* lvl2*)          true -> 0 | false -> 1

