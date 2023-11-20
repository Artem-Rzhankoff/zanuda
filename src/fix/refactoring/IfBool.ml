open Visitors
open Typedtree

type pos =
  | Start
  | End

type ite =
  | If
  | Then
  | Else

let get_loc (e : Location.t) = function
  | Start -> e.loc_start.pos_lnum, e.loc_start.pos_cnum - e.loc_start.pos_bol
  | End -> e.loc_end.pos_lnum, e.loc_end.pos_cnum - e.loc_end.pos_bol
;;

let bool_value e =
  let open Tast_pattern in
  parse ebool e.exp_loc ~on_error:(fun _ () -> None) e (fun b () -> Some b) ()
;;

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

let get_payload fname e1 e2 p =
  let (sl1, sc1), (el1, ec1) = get_loc e1.exp_loc p, get_loc e2.exp_loc p in
  printt fname sl1 sc1 el1 ec1
;;

let get_payload2 fname e1 e2 =
  List.iter (fun (e1, e2, p) -> get_payload fname e1 e2 p) [ e1, e2, Start; e2, e1, End ]
;;

let check_bool e args vbool =
  let fname = e.exp_loc.loc_start.pos_fname in
  let _, val1 = List.nth args 0 in
  let _, val2 = List.nth args 1 in
  let open Tast_pattern in
  match val1, val2 with
  | Some e1, Some e2 ->
    parse
      ebool
      e1.exp_loc
      ~on_error:(fun _ () ->
        match vbool with
        | true -> get_payload fname e1 e2 End
        | false -> get_payload fname e1 e2 Start)
      e1
      (fun _ () ->
        match vbool with
        | true -> get_payload fname e1 e2 Start
        | false -> get_payload fname e1 e2 End)
      ()
  | _ -> failwith "invalid_arg"
;;

let get_ite_loc e ie te ee (pbool_site, ebool) =
  let fname = e.exp_loc.loc_start.pos_fname in
  let match_ite = function
    | If, true, _ ->
      (* if true then x else y --> x *)
      get_payload2 fname e te
    | If, false, _ (* if false then x else y --> y *)
    | Then, true, Some true (* if val then true else true --> true *)
    | Then, false, Some false ->
      (* if val then false else false --> false*)
      get_payload2 fname e ee
    | Then, true, Some false (* if val then true else false --> val *)
    | Then, false, Some true ->
      (* if val then false else true --> not val *)
      get_payload2 fname e ie
    | _ ->
      (* previous p-m covers cases when ebool was parsed in then-e*)
      ()
  in
  match_ite (pbool_site, ebool, bool_value ee)
;;

module rec Lint : sig
  include module type of struct
    let visitor
      : < visit_Closed : Location.t -> _
        ; visit_tt_case :
            'a.
            (Location.t -> 'a -> unit)
            -> Location.t
            -> 'a Typedtree_visitor.tt_case
            -> unit
        ; .. >
      =
      object (_self)
        inherit [_] Lint_refactoring.result_iter
      end
    ;;
  end
end = struct
  let visitor =
    object (_self)
      inherit [_] Lint_refactoring.result_iter as super

      method! visit_expression
        env
        ({ exp_env = _exp_env; exp_loc = _exp_loc; exp_desc; exp_extra = _exp_extra; _ }
         as this) =
        (match exp_desc with
         | Texp_apply (f, args) ->
           let pat =
             let open Tast_pattern in
             texp_apply2 (texp_ident (path [ "Stdlib"; "&&" ])) ebool drop
             ||| texp_apply2 (texp_ident (path [ "Stdlib"; "&&" ])) drop ebool
             |> map1 ~f:(fun b -> check_bool this args b)
           in
           Tast_pattern.parse
             pat
             this.exp_loc
             ~on_error:(fun _desc () -> ())
             this
             (fun s () -> ())
             ()
         | Texp_ifthenelse (ie, te, ee) ->
           let pat =
             let open Tast_pattern in
             let ite =
               texp_ite ebool drop drop
               |> map1 ~f:(fun b ->
                 get_ite_loc this ie te (Option.get ee) (If, b);
                 b)
               ||| (texp_ite drop ebool drop
                    |> map1 ~f:(fun b ->
                      get_ite_loc this ie te (Option.get ee) (Then, b);
                      b))
               ||| (texp_ite drop drop (some ebool)
                    |> map1 ~f:(fun b ->
                      get_ite_loc this ie te (Option.get ee) (Else, b);
                      b))
             in
             ite
           in
           Tast_pattern.parse
             pat
             this.exp_loc
             ~on_error:(fun _desc () -> ())
             this
             (fun _ () -> ())
             ()
         | _ -> ());
        super#visit_expression env this
    end
  ;;
end

(* еще надо понимать, что replacements могут в теории перекрывать друг друга; расставлять что-то типа приоритета или хз*)

let a x y = if true then x else y
(*
   let a x y =              x       |
*)

let a x y = if false then x else y
(*
   let a x y =                      y|
*)

let a x = if x then true else false
(*
   let a x =    x                     |
*)

let a x = if x then false else true
(*
   let a x =not x                     |
*)

let a x = if x then false else false
(*
   let a x =                      false|
*)

let a x = if x then true else true
(*
   let a x =                     true|
*)

let a x = if x && true then 1 else 2
(*
   let a x = if x         then 1 else 2 *)

let a x = if x && false then 1 else 2
(*
   let a x = if      false then 1 else 2
*)

let a x = if true && x then 1 else 2
(*
   let a x = if         x then 1 else 2
*)

let a x = if false && x then 1 else 2
(*
   let a x = if false      then 1 else 2
*)
