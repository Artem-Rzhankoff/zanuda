let a x =
  match x with
  | true -> 1
  | false -> 0
;;

let a x y =
  match x with
  | 1 -> y
  | _ -> y && false
;;
