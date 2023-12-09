
let f x = match (* lvl1 (* lvl2*)*) x with true -> 0 | false -> 1

let f x = if x (* (*nested_comment*) *) && true then x else false
