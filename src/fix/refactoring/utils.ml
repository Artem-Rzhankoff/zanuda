open Location
open Typedtree
open Replacement.Repl.OrderedType
open Replacement.Repl 

type pos =
  | Start
  | End

let gen_loc spoint epoint = function
  | Start, Start ->
    { loc_start = spoint.loc_start; loc_end = epoint.loc_start; loc_ghost = false }
  | Start, End ->
    { loc_start = spoint.loc_start; loc_end = epoint.loc_end; loc_ghost = false }
  | End, Start ->
    { loc_start = spoint.loc_end; loc_end = epoint.loc_start; loc_ghost = false }
  | End, End ->
    { loc_start = spoint.loc_end; loc_end = epoint.loc_end; loc_ghost = false }
;;

let gen_constr_loc point offset = 
  let open Lexing in
  let loc_start = point.loc_start in
  let loc_end = {pos_fname = loc_start.pos_fname; pos_lnum = loc_start.pos_lnum; pos_bol = loc_start.pos_bol; pos_cnum = loc_start.pos_cnum + offset} in
  {point with loc_end = loc_end }
  

let print_pf {location; payload} label = 
  let fname = location.loc_start.pos_fname in
  let sl, sc = location.loc_start.pos_lnum, (location.loc_start.pos_cnum - location.loc_start.pos_bol) in
  let el, ec = location.loc_end.pos_lnum, (location.loc_end.pos_cnum - location.loc_end.pos_bol) in
  let padding = match payload with Default -> "" | Padding s -> s in
  print_string @@ Printf.sprintf "%s file: %s  start: (%d, %d)   end: (%d, %d) payload: |%s| \n" label fname sl sc el ec padding;;

let set_payload_pf ({location; payload} as r) = 
  print_pf {location; payload} "propose_function";
  Replacement.Repl.add location.loc_start.pos_fname r
;;

let set_payload_bool ({location; payload} as r) = 
  print_pf {location; payload} "if_bool";
    Replacement.Repl.add location.loc_start.pos_fname r
;;

