open Location
open Typedtree

type pos =
  | Start
  | End

let printt fname sl sc el ec =
  print_string
  @@ Printf.sprintf
       "if_bool file: %s  start line: %d col: %d  end line: %d end: %d\n"
       fname
       sl
       sc
       el
       ec
;;

let get_loc e = function
  | Start -> e.loc_start.pos_lnum, e.loc_start.pos_cnum - e.loc_start.pos_bol
  | End -> e.loc_end.pos_lnum, e.loc_end.pos_cnum - e.loc_end.pos_bol
;;

let get_payload fname point1 point2 pos1 pos2 =
  let (sl1, sc1), (el1, ec1) = get_loc point1 pos1, get_loc point2 pos2 in
  printt fname sl1 sc1 el1 ec1
;;

let get_payload2 fname point1 point2 =
  List.iter (fun (e1, e2, p) -> get_payload fname e1 e2 p p) [ point1, point2, Start; point2, point1, End ]
;;

(* ------------------------ *)

let printt_pf fname sl sc el ec =
  print_string
  @@ Printf.sprintf
       "propose_function file: %s  start line: %d col: %d  end line: %d end: %d\n"
       fname
       sl
       sc
       el
       ec
;;

let get_payload_pf fname point1 point2 pos1 pos2 =
  let (sl1, sc1), (el1, ec1) = get_loc point1 pos1, get_loc point2 pos2 in
  printt_pf fname sl1 sc1 el1 ec1
;;
