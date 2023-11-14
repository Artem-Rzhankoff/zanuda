open Base
open Lexing
open Warnings

class ['self] result_iter =
  object
    inherit ['self] Typedtree_visitor.iter
    method! visit_array _f _env _xs = 
      ()
    (* тут наверное определить visit_expression, где будут находить выражение по локе*)
  end

let a = if true then true else false

module rec IfBool : sig
  include module type of struct
    let visitor : < visit_Closed : Location.t -> _ ; visit_tt_case: 'a. (Location.t -> 'a -> unit) -> Location.t -> 'a Typedtree_visitor.tt_case -> unit ; .. > =
      object (_self)
        inherit [_] result_iter (* все внимание на resul_iter*)
        (* то есть получается, что где-то тут неизвествен тип visit_tt_case, и метод становится полиморфным*)
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
        (match exp_desc with
         | Texp_apply (f, args) ->
           let pat =
             let ops =
               let open Tast_pattern in
               texp_apply2 (texp_ident (path [ "Stdlib"; "&&" ])) ebool drop
               ||| texp_apply2 (texp_ident (path [ "Stdlib"; "&&" ])) drop ebool
               |> map1 ~f:(fun _ -> Format.asprintf "Conjunction with boolean smells bad")
             in
             ops
           in
           Tast_pattern.parse
             pat
             this.exp_loc
             ~on_error:(fun _desc () -> ())
             this
             (fun s () ->
               print_string
               @@ Printf.sprintf
                    "apply file: %s line: %d col: %d\n" (* ну вместо печати надо будет вызывать replace модуль с соответствующим методом *)
                    this.exp_loc.loc_start.pos_fname
                    this.exp_loc.loc_start.pos_lnum
                    (this.exp_loc.loc_start.pos_cnum - this.exp_loc.loc_start.pos_bol))
             ()
         | Texp_ifthenelse (_ie, _te, _ee) ->
           let pat =
             let open Tast_pattern in
             let ite =
               texp_ite ebool drop drop
               |> map1 ~f:(Format.asprintf "Executing 'if %b' smells bad")
               ||| (texp_ite drop ebool drop
                    |> map1 ~f:(Format.asprintf "Executing 'if ... then %b' smells bad"))
               ||| (texp_ite drop drop (some ebool)
                    |> map1
                         ~f:
                           (Format.asprintf
                              "Executing 'if ... then .. else %b' smells bad"))
             in
             ite
           in
           Tast_pattern.parse
             pat
             this.exp_loc
             ~on_error:(fun _desc () -> ())
             this
             (fun s () ->
               print_string
               @@ Printf.sprintf
                    "if_bool file: %s line: %d col: %d\n"
                    exp_loc.loc_start.pos_fname
                    exp_loc.loc_start.pos_lnum
                    (exp_loc.loc_start.pos_cnum - exp_loc.loc_start.pos_bol))
             ()
         | _ -> ());
        super#visit_expression env this
    end
  ;;
end

module rec ProposeFunction : sig
  include module type of struct
  let visitor : < visit_Closed : Location.t -> _ ; visit_tt_case : 'a. (Location.t -> 'a -> unit) -> Location.t -> 'a Typedtree_visitor.tt_case -> unit; .. > =
    object (_self)
      inherit [_] result_iter
    end
  ;;
  end
end = struct
  let visitor = 
  object (_self)
    inherit [_] result_iter as _super

  end
end
