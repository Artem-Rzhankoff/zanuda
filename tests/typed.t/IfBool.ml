let __ () =
  if true then 1 else 2

let __ f x  = if f x then true else f x
let __ f x  = if f x then f x else false
