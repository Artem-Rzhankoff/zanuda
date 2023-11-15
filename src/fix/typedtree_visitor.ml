open Asttypes (* tt_case *)
open Types
module Base = Typedtree
open Base

type partial = Base.partial =
  | Partial
  | Total

and attribute = (Parsetree.attribute[@opaque])
and attributes = attribute list
(*
   and (Base.value [@opaque]) = Base.(Base.value [@opaque]) = Value_pattern
   and (Base.computation [@opaque])  = Base.(Base.computation [@opaque]) = Computation_pattern
*)

(*and _ pattern_category =
  | Value : (Base.value [@opaque]) pattern_category
  | Computation : (Base.computation [@opaque]) pattern_category*)
and value = Base.value = Value_pattern

and computation = Base.computation = Computation_pattern

and 'k general_pattern = ('k Base.pattern_desc [@opaque]) pattern_data [@opaque]


and 'a pattern_data = 'a Base.pattern_data =
  { pat_desc : 'a 
  ; pat_loc : Location_visitor.location_t
  ; pat_extra : (pat_extra * Location_visitor.location_t * attribute list) list
  ; pat_type : Types_visitor.ty_type_expr [@opaque]
  ; pat_env : Env_visitor.env_t
  ; pat_attributes : attribute list
  }

and pat_extra = Base.pat_extra =
  | Tpat_constraint of core_type
  | Tpat_type of Path_visitor.path_t * Longident_visitor.longident_t loc
  | Tpat_open of
      Path_visitor.path_t * Longident_visitor.longident_t loc * Env_visitor.env_t
  | Tpat_unpack

(* and 'k pattern_desc =
   | Tpat_any
   | Tpat_var of Ident_visitor.ident_t * string loc
   | Tpat_alias of pattern * Ident_visitor.ident_t * string loc
   | Tpat_constant of constant
   | Tpat_tuple  of pattern list
   | Tpat_construct of Longident_visitor.longident_t loc * Types_visitor.ty_constructor_description * pattern list * (Ident_visitor.ident_t loc list * core_type) option
   | Tpat_variant of label * pattern option * Types_visitor.ty_row_desc ref
   | Tpat_array of pattern list
   | Tpat_lazy of pattern
   | Tpat_value of tpat_value_argument
   | Tpat_exception of pattern
   | Tpat_or of pattern * pattern *  Types_visitor.ty_row_desc option
*)
(* and tpat_value_argument = (Base.value [@opaque]) general_pattern *)
and expression = Base.expression =
  { exp_desc : expression_desc
  ; exp_loc : Location_visitor.location_t
  ; exp_extra : (exp_extra * Location_visitor.location_t * attribute list) list
  ; exp_type : Types_visitor.ty_type_expr
  ; exp_env : Env_visitor.env_t
  ; exp_attributes : attribute list
  }

and exp_extra = Base.exp_extra =
  | Texp_constraint of core_type
  | Texp_coerce of core_type option * core_type
  | Texp_poly of core_type option
  | Texp_newtype of string

and expression_desc = Base.expression_desc =
  | Texp_ident of
      Path_visitor.path_t
      * Longident_visitor.longident_t loc
      * Types_visitor.ty_value_description
  | Texp_constant of constant
  | Texp_let of rec_flag * value_binding list * expression
  | Texp_function of
      { arg_label : Asttype_visitor.arg_label
      ; param : Ident_visitor.ident_t
      ; cases : (value tt_case) list
      ; partial : partial
      }
  | Texp_apply of expression * (Asttype_visitor.arg_label * expression option) list
  | Texp_match of
      expression
      * (computation tt_case) list
      * partial (* ((Base.computation [@opaque]) [@opaque])*)
  | Texp_try of
      expression
      * (value tt_case) list (* ((Base.value [@opaque]) [@opaque])*)
  | Texp_tuple of expression list
  | Texp_construct of
      Longident_visitor.longident_t loc
      * Types_visitor.ty_constructor_description
      * expression list
  | Texp_variant of label * expression option
  | Texp_record of
      { fields : (Types_visitor.ty_label_description * record_label_definition) array
      ; representation : Types_visitor.ty_record_representation
      ; extended_expression : expression option
      }
  | Texp_field of
      expression * Longident_visitor.longident_t loc * Types_visitor.ty_label_description
  | Texp_setfield of
      expression
      * Longident_visitor.longident_t loc
      * Types_visitor.ty_label_description
      * expression
  | Texp_array of expression list
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_while of expression * expression
  | Texp_for of
      Ident_visitor.ident_t
      * (Parsetree.pattern[@opaque])
      * expression
      * expression
      * direction_flag
      * expression
  | Texp_send of expression * meth
  | Texp_new of
      Path_visitor.path_t
      * Longident_visitor.longident_t loc
      * Types_visitor.ty_class_declaration
  | Texp_instvar of Path_visitor.path_t * Path_visitor.path_t * string loc
  | Texp_setinstvar of Path_visitor.path_t * Path_visitor.path_t * string loc * expression
  | Texp_override of
      Path_visitor.path_t * (Ident_visitor.ident_t * string loc * expression) list
  | Texp_letmodule of
      Ident_visitor.ident_t option
      * string option loc
      * Types_visitor.ty_module_presence
      * module_expr
      * expression
  | Texp_letexception of extension_constructor * expression
  | Texp_assert of expression
  | Texp_lazy of expression
  | Texp_object of class_structure * string list
  | Texp_pack of module_expr
  | Texp_letop of
      { let_ : binding_op
      ; ands : binding_op list
      ; param : Ident_visitor.ident_t
      ; body : (value tt_case[@opaque] (* ((Base.value [@opaque])[@opaque]) *))
      ; partial : partial
      }
  | Texp_unreachable
  | Texp_extension_constructor of Longident_visitor.longident_t loc * Path_visitor.path_t
  | Texp_open of open_declaration * expression

and meth = Base.meth =
  | Tmeth_name of string
  | Tmeth_val of Ident_visitor.ident_t
  | Tmeth_ancestor of Ident_visitor.ident_t * Path_visitor.path_t

 and 'k tt_case = 'k Base.case = {
  c_lhs: 'k general_pattern;
  c_guard: expression option;
  c_rhs: expression;
}
 

and function_param =
  { fp_arg_label : Asttype_visitor.arg_label
  ; fp_param : Ident_visitor.ident_t
  ; fp_partial : partial
  ; fp_kind : function_param_kind
  ; fp_newtypes : string loc list
  ; fp_loc : Location_visitor.location_t
  }

and function_param_kind =
  | Tparam_pat of (Base.pattern[@opaque])
  | Tparam_optional_default of (Base.pattern[@opaque]) * expression

and function_body =
  | Tfunction_body of expression
  | Tfunction_cases of
      { cases : (value tt_case) list
          (* ((Base.value [@opaque]) [@opaque]) *)
      ; partial : partial
      ; param : Ident_visitor.ident_t
      ; loc : Location_visitor.location_t
      ; exp_extra : exp_extra option
      ; attributes : attributes
      }

and record_label_definition = Base.record_label_definition =
  | Kept of Types_visitor.ty_type_expr
  | Overridden of Longident_visitor.longident_t loc * expression

and binding_op = Base.binding_op =
  { bop_op_path : Path_visitor.path_t
  ; bop_op_name : string loc
  ; bop_op_val : Types_visitor.ty_value_description
  ; bop_op_type : Types_visitor.ty_type_expr
  ; bop_exp : expression
  ; bop_loc : Location_visitor.location_t
  }

(* Value expressions for the class language *)
and class_expr = Base.class_expr =
  { cl_desc : class_expr_desc
  ; cl_loc : Location_visitor.location_t
  ; cl_type : Types_visitor.ty_class_type
  ; cl_env : Env_visitor.env_t
  ; cl_attributes : attribute list
  }

and class_expr_desc = Base.class_expr_desc =
  | Tcl_ident of Path_visitor.path_t * Longident_visitor.longident_t loc * core_type list
  | Tcl_structure of class_structure
  | Tcl_fun of
      Asttype_visitor.arg_label
      * (Base.pattern[@opaque])
      * (Ident_visitor.ident_t * expression) list
      * class_expr
      * partial
  | Tcl_apply of class_expr * (Asttype_visitor.arg_label * expression option) list
  | Tcl_let of
      rec_flag
      * value_binding list
      * (Ident_visitor.ident_t * expression) list
      * class_expr
  | Tcl_constraint of
      class_expr * class_type option * string list * string list * (MethSet.t[@opaque])
    (* Visible instance variables, methods and concrete methods *)
  | Tcl_open of open_description * class_expr

and class_structure = Base.class_structure =
  { cstr_self : (Base.pattern[@opaque])
  ; cstr_fields : class_field list
  ; cstr_type : Types_visitor.ty_class_signature
  ; cstr_meths : (Ident_visitor.ident_t Types.Meths.t[@opaque])
  }

and class_field = Base.class_field =
  { cf_desc : class_field_desc
  ; cf_loc : Location_visitor.location_t
  ; cf_attributes : attribute list
  }

and class_field_kind = Base.class_field_kind =
  | Tcfk_virtual of core_type 
  | Tcfk_concrete of override_flag * expression

and class_field_desc = Base.class_field_desc =
  | Tcf_inherit of
      override_flag
      * class_expr
      * string option
      * (string * Ident_visitor.ident_t) list
      * (string * Ident_visitor.ident_t) list
    (* Inherited instance variables and concrete methods *)
  | Tcf_val of string loc * mutable_flag * Ident_visitor.ident_t * class_field_kind * bool
  | Tcf_method of string loc * private_flag * class_field_kind
  | Tcf_constraint of core_type * core_type
  | Tcf_initializer of expression
  | Tcf_attribute of attribute

(* Value expressions for the module language *)
and module_expr = Base.module_expr =
  { mod_desc : module_expr_desc
  ; mod_loc : Location_visitor.location_t
  ; mod_type : Types_visitor.ty_module_type
  ; mod_env : Env_visitor.env_t
  ; mod_attributes : attribute list
  }

and module_type_constraint = Base.module_type_constraint =
  | Tmodtype_implicit
  | Tmodtype_explicit of module_type

and functor_parameter = Base.functor_parameter =
  | Unit [@name "tt_Unit"]
  | Named of Ident_visitor.ident_t option * string option loc * module_type
  [@name "tt_Named"]

and module_expr_desc = Base.module_expr_desc =
  | Tmod_ident of Path_visitor.path_t * Longident_visitor.longident_t loc
  | Tmod_structure of structure
  | Tmod_functor of functor_parameter * module_expr
  | Tmod_apply of module_expr * module_expr * module_coercion
  | Tmod_constraint of
      module_expr
      * Types_visitor.ty_module_type
      * module_type_constraint
      * module_coercion
  | Tmod_unpack of expression * Types_visitor.ty_module_type

and structure = Base.structure =
  { str_items : structure_item list
  ; str_type : Types_visitor.ty_signature
  ; str_final_env : Env_visitor.env_t
  }

and structure_item = Base.structure_item =
  { str_desc : structure_item_desc
  ; str_loc : Location_visitor.location_t
  ; str_env : Env_visitor.env_t
  }

and structure_item_desc = Base.structure_item_desc =
  | Tstr_eval of expression * attributes
  | Tstr_value of rec_flag * value_binding list
  | Tstr_primitive of value_description
  | Tstr_type of rec_flag * type_declaration list
  | Tstr_typext of type_extension
  | Tstr_exception of type_exception
  | Tstr_module of module_binding
  | Tstr_recmodule of module_binding list
  | Tstr_modtype of module_type_declaration
  | Tstr_open of open_declaration
  | Tstr_class of (class_declaration * string list) list
  | Tstr_class_type of (Ident_visitor.ident_t * string loc * class_type_declaration) list
  | Tstr_include of include_declaration
  | Tstr_attribute of attribute

and module_binding = Base.module_binding =
  { mb_id : Ident_visitor.ident_t option
  ; mb_name : string option loc
  ; mb_presence : Types_visitor.ty_module_presence
  ; mb_expr : module_expr
  ; mb_attributes : attribute list
  ; mb_loc : Location_visitor.location_t
  }

and value_binding = Base.value_binding =
  { vb_pat : (Base.pattern[@opaque])
  ; vb_expr : expression
  ; vb_attributes : attributes
  ; vb_loc : Location_visitor.location_t
  }

and module_coercion = Base.module_coercion =
  | Tcoerce_none
  | Tcoerce_structure of
      (int * module_coercion) list * (Ident_visitor.ident_t * int * module_coercion) list
  | Tcoerce_functor of module_coercion * module_coercion
  | Tcoerce_primitive of primitive_coercion
  | Tcoerce_alias of Env_visitor.env_t * Path_visitor.path_t * module_coercion

and module_type = Base.module_type =
  { mty_desc : module_type_desc
  ; mty_type : Types_visitor.ty_module_type
  ; mty_env : Env_visitor.env_t
  ; mty_loc : Location_visitor.location_t
  ; mty_attributes : attribute list
  }

and module_type_desc = Base.module_type_desc =
  | Tmty_ident of Path_visitor.path_t * Longident_visitor.longident_t loc
  | Tmty_signature of signature
  | Tmty_functor of functor_parameter * module_type
  | Tmty_with of
      module_type
      * (Path_visitor.path_t * Longident_visitor.longident_t loc * with_constraint) list
  | Tmty_typeof of module_expr
  | Tmty_alias of Path_visitor.path_t * Longident_visitor.longident_t loc

(* Keep primitive and information for and-based lambda-code specialization *)
and primitive_coercion = Base.primitive_coercion =
  { pc_desc : (Primitive.description[@opaque] (*??*))
  ; pc_type : Types_visitor.ty_type_expr
  ; pc_env : Env_visitor.env_t
  ; pc_loc : Location_visitor.location_t
  }

and signature = Base.signature =
  { sig_items : signature_item list
  ; sig_type : Types_visitor.ty_signature
  ; sig_final_env : Env_visitor.env_t
  }

and signature_item = Base.signature_item =
  { sig_desc : signature_item_desc
  ; sig_env : Env_visitor.env_t (* BINANNOT ADDED *)
  ; sig_loc : Location_visitor.location_t
  }

and signature_item_desc = Base.signature_item_desc =
  | Tsig_value of value_description
  | Tsig_type of rec_flag * type_declaration list
  | Tsig_typesubst of type_declaration list
  | Tsig_typext of type_extension
  | Tsig_exception of type_exception
  | Tsig_module of module_declaration
  | Tsig_modsubst of module_substitution
  | Tsig_recmodule of module_declaration list
  | Tsig_modtype of module_type_declaration
  | Tsig_modtypesubst of module_type_declaration
  | Tsig_open of open_description
  | Tsig_include of include_description
  | Tsig_class of class_description list
  | Tsig_class_type of class_type_declaration list
  | Tsig_attribute of attribute

and module_declaration = Base.module_declaration =
  { md_id : Ident_visitor.ident_t option
  ; md_name : string option loc
  ; md_presence : Types_visitor.ty_module_presence
  ; md_type : module_type
  ; md_attributes : attribute list
  ; md_loc : Location_visitor.location_t
  }

and module_substitution = Base.module_substitution =
  { ms_id : Ident_visitor.ident_t
  ; ms_name : string loc
  ; ms_manifest : Path_visitor.path_t
  ; ms_txt : Longident_visitor.longident_t loc
  ; ms_attributes : attributes
  ; ms_loc : Location_visitor.location_t
  }

and module_type_declaration = Base.module_type_declaration =
  { mtd_id : Ident_visitor.ident_t
  ; mtd_name : string loc
  ; mtd_type : module_type option
  ; mtd_attributes : attribute list
  ; mtd_loc : Location_visitor.location_t
  }

and 'a open_infos = 'a Base.open_infos =
  { open_expr : 'a
  ; open_bound_items : Types_visitor.ty_signature
  ; open_override : override_flag
  ; open_env : Env_visitor.env_t
  ; open_loc : Location_visitor.location_t
  ; open_attributes : attribute list
  }

and open_description = (Base.open_description[@opaque])

(*= (Path_visitor.path_t * Longident_visitor.longident_t loc) open_infos *)
and open_declaration = (Base.open_declaration[@opaque])
(* = module_expr open_infos *)

and 'a include_infos = 'a Base.include_infos =
  { incl_mod : 'a
  ; incl_type : Types_visitor.ty_signature
  ; incl_loc : Location_visitor.location_t
  ; incl_attributes : attribute list
  }

and include_description = (Base.include_description[@opaque])

(* = module_type include_infos *)
and include_declaration = (Base.include_declaration[@opaque])
(* = module_expr include_infos*)

and with_constraint = Base.with_constraint =
  | Twith_type of type_declaration
  | Twith_module of Path_visitor.path_t * Longident_visitor.longident_t loc
  | Twith_modtype of module_type
  | Twith_typesubst of type_declaration
  | Twith_modsubst of Path_visitor.path_t * Longident_visitor.longident_t loc
  | Twith_modtypesubst of module_type

and core_type = Base.core_type =
  { (* mutable because of [Typeclass.declare_method] *)
    mutable ctyp_desc : core_type_desc
  ; mutable ctyp_type : Types_visitor.ty_type_expr
  ; ctyp_env : Env_visitor.env_t (* BINANNOT ADDED *)
  ; ctyp_loc : Location_visitor.location_t
  ; ctyp_attributes : attribute list
  }

and core_type_desc = Base.core_type_desc =
  | Ttyp_any
  | Ttyp_var of string
  | Ttyp_arrow of Asttype_visitor.arg_label * core_type * core_type
  | Ttyp_tuple of core_type list
  | Ttyp_constr of
      Path_visitor.path_t * Longident_visitor.longident_t loc * core_type list
  | Ttyp_object of object_field list * closed_flag
  | Ttyp_class of Path_visitor.path_t * Longident_visitor.longident_t loc * core_type list
  | Ttyp_alias of core_type * string
  | Ttyp_variant of row_field list * closed_flag * label list option
  | Ttyp_poly of string list * core_type
  | Ttyp_package of package_type

and package_type = Base.package_type =
  { pack_path : Path_visitor.path_t
  ; pack_fields : (Longident_visitor.longident_t loc * core_type) list
  ; pack_type : Types_visitor.ty_module_type
  ; pack_txt : Longident_visitor.longident_t loc
  }

and row_field = Base.row_field =
  { rf_desc : row_field_desc
  ; rf_loc : Location_visitor.location_t
  ; rf_attributes : attributes
  }

and row_field_desc = Base.row_field_desc =
  | Ttag of string loc * bool * core_type list
  | Tinherit of core_type

and object_field = Base.object_field =
  { of_desc : object_field_desc
  ; of_loc : Location_visitor.location_t
  ; of_attributes : attributes
  }

and object_field_desc = Base.object_field_desc =
  | OTtag of string loc * core_type
  | OTinherit of core_type

and value_description = Base.value_description =
  { val_id : Ident_visitor.ident_t
  ; val_name : string loc
  ; val_desc : core_type
  ; val_val : Types_visitor.ty_value_description
  ; val_prim : string list
  ; val_loc : Location_visitor.location_t
  ; val_attributes : attribute list
  }

and type_declaration = Base.type_declaration =
  { typ_id : Ident_visitor.ident_t
  ; typ_name : string loc
  ; typ_params : (core_type * (variance * injectivity)) list
  ; typ_type : Types_visitor.ty_type_declaration
  ; typ_cstrs : (core_type * core_type * Location_visitor.location_t) list
  ; typ_kind : type_kind
  ; typ_private : private_flag
  ; typ_manifest : core_type option
  ; typ_loc : Location_visitor.location_t
  ; typ_attributes : attribute list
  }

and type_kind = Base.type_kind =
  | Ttype_abstract
  | Ttype_variant of constructor_declaration list
  | Ttype_record of label_declaration list
  | Ttype_open

and label_declaration = Base.label_declaration =
  { ld_id : Ident_visitor.ident_t
  ; ld_name : string loc
  ; ld_mutable : mutable_flag
  ; ld_type : core_type
  ; ld_loc : Location_visitor.location_t
  ; ld_attributes : attribute list
  }

and constructor_declaration = Base.constructor_declaration =
  { cd_id : Ident_visitor.ident_t
  ; cd_name : string loc
  ; cd_vars : string loc list
  ; cd_args : constructor_arguments
  ; cd_res : core_type option
  ; cd_loc : Location_visitor.location_t
  ; cd_attributes : attribute list
  }

and constructor_arguments = Base.constructor_arguments =
  | Cstr_tuple of core_type list
  | Cstr_record of label_declaration list

and type_extension = Base.type_extension =
  { tyext_path : Path_visitor.path_t
  ; tyext_txt : Longident_visitor.longident_t loc
  ; tyext_params : (core_type * (variance * injectivity)) list
  ; tyext_constructors : extension_constructor list
  ; tyext_private : private_flag
  ; tyext_loc : Location_visitor.location_t
  ; tyext_attributes : attribute list
  }

and type_exception = Base.type_exception =
  { tyexn_constructor : extension_constructor
  ; tyexn_loc : Location_visitor.location_t
  ; tyexn_attributes : attribute list
  }

and extension_constructor = Base.extension_constructor =
  { ext_id : Ident_visitor.ident_t
  ; ext_name : string loc
  ; ext_type : Types_visitor.ty_extension_constructor
  ; ext_kind : extension_constructor_kind
  ; ext_loc : Location_visitor.location_t
  ; ext_attributes : attribute list
  }

and extension_constructor_kind = Base.extension_constructor_kind =
  | Text_decl of string loc list * constructor_arguments * core_type option
  | Text_rebind of Path_visitor.path_t * Longident_visitor.longident_t loc

and class_type = Base.class_type =
  { cltyp_desc : class_type_desc
  ; cltyp_type : Types_visitor.ty_class_type
  ; cltyp_env : Env_visitor.env_t
  ; cltyp_loc : Location_visitor.location_t
  ; cltyp_attributes : attribute list
  }

and class_type_desc = Base.class_type_desc =
  | Tcty_constr of
      Path_visitor.path_t * Longident_visitor.longident_t loc * core_type list
  | Tcty_signature of class_signature
  | Tcty_arrow of Asttype_visitor.arg_label * core_type * class_type
  | Tcty_open of open_description * class_type

and class_signature = Base.class_signature =
  { csig_self : core_type
  ; csig_fields : class_type_field list
  ; csig_type : Types_visitor.ty_class_signature
  }

and class_type_field = Base.class_type_field =
  { ctf_desc : class_type_field_desc
  ; ctf_loc : Location_visitor.location_t
  ; ctf_attributes : attribute list
  }

and class_type_field_desc = Base.class_type_field_desc =
  | Tctf_inherit of class_type
  | Tctf_val of (string * mutable_flag * virtual_flag * core_type)
  | Tctf_method of (string * private_flag * virtual_flag * core_type)
  | Tctf_constraint of (core_type * core_type)
  | Tctf_attribute of attribute

and class_declaration = (Base.class_declaration[@opaque])
(* class_expr class_infos *)

and class_description = (Base.class_description[@opaque])
(* class_type class_infos *)

and class_type_declaration = (Base.class_type_declaration[@opaque])
(* class_type class_infos *)

and 'a class_infos = 'a Base.class_infos =
  { ci_virt : virtual_flag
  ; ci_params : (core_type * (variance * injectivity)) list
  ; ci_id_name : string loc
  ; ci_id_class : Ident_visitor.ident_t
  ; ci_id_class_type : Ident_visitor.ident_t
  ; ci_id_object : Ident_visitor.ident_t
  ; ci_id_typehash : Ident_visitor.ident_t
  ; ci_expr : 'a
  ; ci_decl : Types_visitor.ty_class_declaration
  ; ci_type_decl : Types_visitor.ty_class_type_declaration
  ; ci_loc : Location_visitor.location_t
  ; ci_attributes : attribute list
  }

and implementation =
  { structure : structure
  ; coercion : module_coercion
  ; signature : Types_visitor.ty_signature
  ; shape : (Shape.t[@opaque])
  }
[@@deriving
  visitors
    { variety = "iter";
    polymorphic = true;
    monomorphic = [ "'env"]
    ; ancestors = [ "Env_visitor.iter"; "Types_visitor.iter" ]
    ; nude = true
    }]
