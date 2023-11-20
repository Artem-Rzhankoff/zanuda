open Visitors
open Tast_pattern
open Lint_refactoring
open Base

let first_case_loc (cs : case_comp list) = 
  let c = List.nth cs 0 in 
  match c with
  | Some s -> (s.c_lhs.pat_loc.loc_start.pos_lnum, (s.c_lhs.pat_loc.loc_start.pos_cnum - s.c_lhs.pat_loc.loc_start.pos_bol))
  | _ -> failwith "invalid_arg"

let no_ident ident c =
  let exception Found in
  let open Typedtree in
  let visitor
    : < visit_Closed : Ident.t -> _
      ; visit_tt_case :
          'a. (Ident.t -> 'a -> unit) -> Ident.t -> 'a Typedtree_visitor.tt_case -> unit
      ; .. >
    =
    object (_self)
      inherit [_] result_iter as super

      method! visit_expression
        ident
        ({ exp_env = _exp_env; exp_loc = _exp_loc; exp_desc; exp_extra = _exp_extra; _ }
         as this) =
        match exp_desc with
        | Texp_ident (Path.Pident id, _, _) when Ident.equal id ident -> raise Found
        | Texp_function { param } when Ident.equal ident param -> ()
        | _ -> super#visit_expression ident this

      method! visit_tt_case
        : 'a. (Ident.t -> 'a -> unit) -> Ident.t -> 'a Typedtree_visitor.tt_case -> unit =
        fun visit_'a ident this ->
          let _id_name = Ident.name ident in
          (* the purpose of this is that otherwise the method type becomes monomorphic *)
          match Ident.name ident with
          | _id_name ->
            (match c.c_lhs.pat_desc with
             | Tpat_value v ->
               (match (v :> pattern) with
                | { pat_desc = Tpat_var (id, _) } ->
                  if Ident.equal ident id
                  then ()
                  else super#visit_tt_case visit_'a ident this
                | _ -> super#visit_tt_case visit_'a ident this)
             | _ -> super#visit_tt_case visit_'a ident this)
    end
  in
  try
    visitor#visit_tt_case (fun _ _ -> ()) ident c;
    true
  with
  | Found -> false
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
        inherit [_] result_iter
      end
    ;;
  end
end = struct
  let visitor =
    object (_self)
      inherit [_] result_iter as super

      method! visit_expression
        env
        ({ exp_env = _exp_env; exp_loc; exp_desc; exp_extra = _exp_extra; _ } as this) =
        match exp_desc with
        | Texp_function
            { arg_label = _arg_label; param = _param; cases = _cases; partial = _partial }
          ->
          let pat = (* тут надо match находить или шо*)
            let open Tast_pattern in
            texp_function (case (tpat_var __) none (texp_match (texp_ident __) __) ^:: nil)
          in
          Tast_pattern.parse
            pat
            exp_loc
            ~on_error:(fun _desc () -> ())
            this
            (fun argname ident cases () ->
              begin match ident with
              | Path.Pident id ->
                if String.equal argname (Ident.name id)
                   && List.for_all cases ~f:(fun c -> no_ident id c)
                then
                  let a, b = first_case_loc cases in 
                  print_string
                  @@ Printf.sprintf
                       "propose_function start file: %s line: %d col: %d  end line: %d col: %d \n"
                       exp_loc.loc_start.pos_fname
                       exp_loc.loc_start.pos_lnum
                       (exp_loc.loc_start.pos_cnum - exp_loc.loc_start.pos_bol)
                       a b
              | _ -> () end)
            ();
          super#visit_expression env this (* without this call it doesn't traverse nested function construction. Why? *)
        | _ ->
          ();
          super#visit_expression env this
    end
  ;;
end

let a x y =
  match y with
  | 1 -> true
  | _ -> x
;;

(*  
let a x = 
  function    |
  | 1 -> true
  | _ -> x
*)

let a = fun x y -> match y with 1 -> true | _ -> x
(*
let a = fun x |*)
(* нужна лока параметра и первого варианта
   let a x = match x with 1 -> true | _ -> false
   let a   =     function 1 -> true | _ -> false
*)
(* нужна лока
   let a = fun x -> match x with 1 -> true | _ -> false
   let a =              function 1 -> true | _ -> false
*)
