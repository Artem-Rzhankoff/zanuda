open Asttypes
module Base = Types

type ty_type_expr = (Base.type_expr[@opaque]) [@opaque]

and ty_type_desc = Base.type_desc =
  | Tvar of string option
  | Tarrow of arg_label * ty_type_expr * ty_type_expr * ty_commutable
  | Ttuple of ty_type_expr list
  | Tconstr of Path_visitor.path_t * ty_type_expr list * ty_abbrev_memo ref
  | Tobject of ty_type_expr * (Path_visitor.path_t * ty_type_expr list) option ref
  | Tfield of string * ty_field_kind * ty_type_expr * ty_type_expr
  | Tnil
  | Tlink of ty_type_expr
  | Tsubst of ty_type_expr * ty_type_expr option
  | Tvariant of ty_row_desc
  | Tunivar of string option
  | Tpoly of ty_type_expr * ty_type_expr list
  | Tpackage of Path_visitor.path_t * (Longident_visitor.longident_t * ty_type_expr) list

and ty_row_desc = (Base.row_desc[@opaque]) [@opaque] (* hope this is temporary*)
(* and ty_row_field = Base.row_field*)

and ty_abbrev_memo = Base.abbrev_memo =
  | Mnil
  | Mcons of
      private_flag * Path_visitor.path_t * ty_type_expr * ty_type_expr * ty_abbrev_memo
  | Mlink of ty_abbrev_memo ref

and ty_field_kind = (Base.field_kind[@opaque]) [@opaque]
and ty_commutable = (Base.commutable[@opaque]) [@opaque]

and ty_value_description = Base.value_description =
  { val_type : ty_type_expr
  ; val_kind : ty_value_kind
  ; val_loc : Location_visitor.location_t
  ; val_attributes : (Parsetree.attributes[@opaque])
  ; val_uid : (Shape.Uid.t[@opaque])
  }

and ty_value_kind = Base.value_kind =
  | Val_reg
  | Val_prim of (Primitive.description[@opaque])
  | Val_ivar of mutable_flag * string
  | Val_self of
      ty_class_signature
      * ty_self_meths
      * (Ident_visitor.ident_t Base.Vars.t[@opaque])
      * string
  | Val_anc of ty_class_signature * (Ident_visitor.ident_t Base.Meths.t[@opaque]) * string

and ty_self_meths = Base.self_meths =
  | Self_concrete of (Ident_visitor.ident_t Base.Meths.t[@opaque])
  | Self_virtual of (Ident_visitor.ident_t Base.Meths.t[@opaque]) ref

and variance_t = (Base.Variance.t[@opaque]) [@opaque]

(* and variance_f = Base.Variance.f =
   May_pos | May_neg | May_weak | Inj | Pos | Neg | Inv *)
(* This type is not referred to by anything else. *)
(* and foo = Base.Separability.t [@opaque] *)

and ty_type_declaration = Base.type_declaration =
  { type_params : ty_type_expr list
  ; type_arity : int
  ; type_kind : ty_type_decl_kind
  ; type_private : private_flag
  ; type_manifest : ty_type_expr option
  ; type_variance : variance_t list
  ; type_separability : (Base.Separability.t [@opaque]) list
  ; type_is_newtype : bool
  ; type_expansion_scope : int
  ; type_loc : Location_visitor.location_t
  ; type_attributes : (Parsetree.attributes[@opaque])
  ; type_immediate :
      (Type_immediacy.t[@opaque])
  ; type_unboxed_default : bool
  ; type_uid : (Shape.Uid.t[@opaque])
  }

and ty_type_decl_kind = (Base.type_decl_kind[@opaque])

and ('a, 'b) ty_type_kind = ('a, 'b) Base.type_kind =
  | Type_abstract
  | Type_record of 'a list * ty_record_representation
  | Type_variant of 'b list * ty_variant_representation
  | Type_open

and ty_record_representation = Base.record_representation =
  | Record_regular
  | Record_float
  | Record_unboxed of bool
  | Record_inlined of int
  | Record_extension of Path_visitor.path_t

and ty_variant_representation = Base.variant_representation =
  | Variant_regular
  | Variant_unboxed

and ty_label_declaration = Base.label_declaration =
  { ld_id : Ident_visitor.ident_t
  ; ld_mutable : mutable_flag
  ; ld_type : ty_type_expr
  ; ld_loc : Location_visitor.location_t
  ; ld_attributes : (Parsetree.attributes[@opaque])
  ; ld_uid : (Shape.Uid.t[@opaque])
  }

and ty_constructor_declaration = Base.constructor_declaration =
  { cd_id : Ident_visitor.ident_t
  ; cd_args : ty_constructor_arguments
  ; cd_res : ty_type_expr option
  ; cd_loc : Location_visitor.location_t
  ; cd_attributes : (Parsetree.attributes[@opaque])
  ; cd_uid : (Shape.Uid.t[@opaque])
  }

and ty_constructor_arguments = Base.constructor_arguments =
  | Cstr_tuple of ty_type_expr list [@name "ty_Cstr_tuple"]
  | Cstr_record of ty_label_declaration list [@name "ty_Cstr_record"]
(* Visitors for these constructors are renamed to avoid clashes with Typedtree *)

(* and ty_unboxed_status = Base.unboxed_status =
      private {
        unboxed: bool;
        default: bool; (* True for unannotated unboxable types. *)
      } [@@build
          fun unboxed default ->
            match unboxed, default with
            | false, false -> Base.unboxed_false_default_false
            | false, true  -> Base.unboxed_false_default_true
            | true, false  -> Base.unboxed_true_default_false
            | true, true   -> Base.unboxed_true_default_true
        ]
*)

and ty_extension_constructor = Base.extension_constructor =
  { ext_type_path : Path_visitor.path_t
  ; ext_type_params : ty_type_expr list
  ; ext_args : ty_constructor_arguments
  ; ext_ret_type : ty_type_expr option
  ; ext_private : private_flag
  ; ext_loc : Location_visitor.location_t
  ; ext_attributes : (Parsetree.attributes[@opaque])
  ; ext_uid : (Shape.Uid.t[@opaque])
  }

and ty_class_type = Base.class_type =
  | Cty_constr of Path_visitor.path_t * ty_type_expr list * ty_class_type
  | Cty_signature of ty_class_signature
  | Cty_arrow of arg_label * ty_type_expr * ty_class_type

and ty_class_signature = Base.class_signature =
  { csig_self : ty_type_expr
  ; mutable csig_self_row : ty_type_expr
  ; mutable csig_vars :
      ((mutable_flag * virtual_flag * ty_type_expr) Base.Vars.t[@opaque])
  ; mutable csig_meths :
      ((ty_method_privacy * virtual_flag * ty_type_expr) Base.Meths.t[@opaque])
  }

and ty_method_privacy = Base.method_privacy =
  | Mpublic
  | Mprivate of ty_field_kind

and ty_class_declaration = Base.class_declaration =
  { cty_params : ty_type_expr list
  ; mutable cty_type : ty_class_type
  ; cty_path : Path_visitor.path_t
  ; cty_new : ty_type_expr option
  ; cty_variance : variance_t list
  ; cty_loc : Location_visitor.location_t
  ; cty_attributes : (Parsetree.attributes[@opaque])
  ; cty_uid : (Shape.Uid.t[@opaque])
  }

and ty_class_type_declaration = Base.class_type_declaration =
  { clty_params : ty_type_expr list
  ; clty_type : ty_class_type
  ; clty_path : Path_visitor.path_t
  ; clty_variance : variance_t list
  ; clty_loc : Location_visitor.location_t
  ; clty_attributes : (Parsetree.attributes[@opaque])
  ; clty_uid : (Shape.Uid.t[@opaque])
  }

and ty_visibility = Base.visibility =
  | Exported
  | Hidden

and ty_module_type = Base.module_type =
  | Mty_ident of Path_visitor.path_t
  | Mty_signature of ty_signature
  | Mty_functor of ty_functor_parameter * ty_module_type
  | Mty_alias of Path_visitor.path_t

and ty_functor_parameter = Base.functor_parameter =
  | Unit
  | Named of Ident_visitor.ident_t option * ty_module_type

and ty_module_presence = Base.module_presence =
  | Mp_present
  | Mp_absent

and ty_signature = ty_signature_item list

and ty_signature_item = Base.signature_item =
  | Sig_value of Ident_visitor.ident_t * ty_value_description * ty_visibility
  | Sig_type of
      Ident_visitor.ident_t * ty_type_declaration * ty_rec_status * ty_visibility
  | Sig_typext of
      Ident_visitor.ident_t * ty_extension_constructor * ty_ext_status * ty_visibility
  | Sig_module of
      Ident_visitor.ident_t
      * ty_module_presence
      * ty_module_declaration
      * ty_rec_status
      * ty_visibility
  | Sig_modtype of Ident_visitor.ident_t * ty_modtype_declaration * ty_visibility
  | Sig_class of
      Ident_visitor.ident_t * ty_class_declaration * ty_rec_status * ty_visibility
  | Sig_class_type of
      Ident_visitor.ident_t * ty_class_type_declaration * ty_rec_status * ty_visibility

and ty_module_declaration = Base.module_declaration =
  { md_type : ty_module_type
  ; md_attributes : (Parsetree.attributes[@opaque])
  ; md_loc : Location_visitor.location_t
  ; md_uid : (Shape.Uid.t[@opaque])
  }

and ty_modtype_declaration = Base.modtype_declaration =
  { mtd_type : ty_module_type option (* None: abstract *)
  ; mtd_attributes : (Parsetree.attributes[@opaque])
  ; mtd_loc : Location_visitor.location_t
  ; mtd_uid : (Shape.Uid.t[@opaque])
  }

and ty_rec_status = Base.rec_status =
  | Trec_not
  | Trec_first
  | Trec_next

and ty_ext_status = Base.ext_status =
  | Text_first
  | Text_next
  | Text_exception

and ty_constructor_description = Base.constructor_description =
  { cstr_name : string
  ; cstr_res : ty_type_expr
  ; cstr_existentials : ty_type_expr list
  ; cstr_args : ty_type_expr list
  ; cstr_arity : int
  ; cstr_tag : ty_constructor_tag
  ; cstr_consts : int
  ; cstr_nonconsts : int
  ; cstr_generalized : bool
  ; cstr_private : private_flag
  ; cstr_loc : Location_visitor.location_t
  ; cstr_attributes : (Parsetree.attributes[@opaque])
  ; cstr_inlined : ty_type_declaration option
  ; cstr_uid : (Shape.Uid.t[@opaque])
  }

and ty_constructor_tag = Base.constructor_tag =
  | Cstr_constant of int
  | Cstr_block of int
  | Cstr_unboxed
  | Cstr_extension of Path_visitor.path_t * bool
(* Extension constructor
   true if a constant false if a block*)

and ty_label_description = Base.label_description =
  { lbl_name : string
  ; lbl_res : ty_type_expr
  ; lbl_arg : ty_type_expr
  ; lbl_mut : mutable_flag
  ; lbl_pos : int
  ; lbl_all : ty_label_description array
  ; lbl_repres : ty_record_representation
  ; lbl_private : private_flag
  ; lbl_loc : Location_visitor.location_t
  ; lbl_attributes : (Parsetree.attributes[@opaque])
  ; lbl_uid : (Shape.Uid.t[@opaque])
  }
[@@deriving
  visitors
    { variety = "iter"
    ; polymorphic = true
    ; monomorphic = [ "'env" ]
    ; ancestors =
        [ "Asttype_visitor.iter"; "Path_visitor.iter"; "Longident_visitor.iter" ]
    ; nude = true
    }]
