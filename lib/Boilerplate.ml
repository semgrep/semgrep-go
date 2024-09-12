(**
   Boilerplate to be used as a template when mapping the go CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_int_literal (env : env) (tok : CST.int_literal) =
  (* int_literal *) token env tok

let map_float_literal (env : env) (tok : CST.float_literal) =
  (* float_literal *) token env tok

let map_imm_tok_dquot (env : env) (tok : CST.imm_tok_dquot) =
  (* "\"" *) token env tok

let map_rune_literal (env : env) (tok : CST.rune_literal) =
  (* rune_literal *) token env tok

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* identifier *) token env tok

let map_interpreted_string_literal_basic_content (env : env) (tok : CST.interpreted_string_literal_basic_content) =
  (* pattern "[^\"\\n\\\\]+" *) token env tok

let map_semgrep_ellipsis_metavar (env : env) (tok : CST.semgrep_ellipsis_metavar) =
  (* pattern \$\.\.\.[a-zA-Z_][a-zA-Z_0-9]* *) token env tok

let map_pat_1d78758 (env : env) (tok : CST.pat_1d78758) =
  (* pattern \n *) token env tok

let map_anon_choice_EQ_4ccabd6 (env : env) (x : CST.anon_choice_EQ_4ccabd6) =
  (match x with
  | `EQ tok -> R.Case ("EQ",
      (* "=" *) token env tok
    )
  | `COLONEQ tok -> R.Case ("COLONEQ",
      (* ":=" *) token env tok
    )
  )

let map_raw_string_literal (env : env) (tok : CST.raw_string_literal) =
  (* raw_string_literal *) token env tok

let map_anon_choice_new_0342769 (env : env) (x : CST.anon_choice_new_0342769) =
  (match x with
  | `New tok -> R.Case ("New",
      (* "new" *) token env tok
    )
  | `Make tok -> R.Case ("Make",
      (* "make" *) token env tok
    )
  )

let map_imaginary_literal (env : env) (tok : CST.imaginary_literal) =
  (* imaginary_literal *) token env tok

let map_anon_choice_pat_1d78758_89f618f (env : env) (x : CST.anon_choice_pat_1d78758_89f618f) =
  (match x with
  | `Pat_1d78758 x -> R.Case ("Pat_1d78758",
      map_pat_1d78758 env x
    )
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  | `NUL tok -> R.Case ("NUL",
      (* "\000" *) token env tok
    )
  )

let map_goto_statement (env : env) ((v1, v2) : CST.goto_statement) =
  let v1 = (* "goto" *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  R.Tuple [v1; v2]

let map_field_name_list (env : env) ((v1, v2) : CST.field_name_list) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_empty_labeled_statement (env : env) ((v1, v2) : CST.empty_labeled_statement) =
  let v1 = (* identifier *) token env v1 in
  let v2 = (* ":" *) token env v2 in
  R.Tuple [v1; v2]

let map_qualified_type (env : env) ((v1, v2, v3) : CST.qualified_type) =
  let v1 = (* identifier *) token env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_break_statement (env : env) ((v1, v2) : CST.break_statement) =
  let v1 = (* "break" *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* identifier *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_continue_statement (env : env) ((v1, v2) : CST.continue_statement) =
  let v1 = (* "continue" *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* identifier *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_string_literal (env : env) (x : CST.string_literal) =
  (match x with
  | `Raw_str_lit tok -> R.Case ("Raw_str_lit",
      (* raw_string_literal *) token env tok
    )
  | `Inte_str_lit (v1, v2, v3) -> R.Case ("Inte_str_lit",
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Inte_str_lit_basic_content tok -> R.Case ("Inte_str_lit_basic_content",
              (* pattern "[^\"\\n\\\\]+" *) token env tok
            )
          | `Esc_seq tok -> R.Case ("Esc_seq",
              (* escape_sequence *) token env tok
            )
          )
        ) v2)
      in
      let v3 = map_imm_tok_dquot env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let rec map_anon_choice_exp_stmt_047b57a (env : env) (x : CST.anon_choice_exp_stmt_047b57a) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression_statement env x
    )
  | `Vari_arg (v1, v2) -> R.Case ("Vari_arg",
      let v1 = map_expression_statement env v1 in
      let v2 = (* "..." *) token env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_anon_choice_lit_elem_0952f3f (env : env) (x : CST.anon_choice_lit_elem_0952f3f) =
  (match x with
  | `Lit_elem x -> R.Case ("Lit_elem",
      map_literal_element env x
    )
  | `Keyed_elem (v1, v2, v3) -> R.Case ("Keyed_elem",
      let v1 = map_literal_element env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_literal_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_param_decl_18823e5 (env : env) (x : CST.anon_choice_param_decl_18823e5) =
  (match x with
  | `Param_decl x -> R.Case ("Param_decl",
      map_parameter_declaration env x
    )
  | `Vari_param_decl (v1, v2, v3) -> R.Case ("Vari_param_decl",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* identifier *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "..." *) token env v2 in
      let v3 = map_type_ env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_param_list_29faba4 (env : env) (x : CST.anon_choice_param_list_29faba4) =
  (match x with
  | `Param_list x -> R.Case ("Param_list",
      map_parameter_list env x
    )
  | `Simple_type x -> R.Case ("Simple_type",
      map_simple_type env x
    )
  )

and map_argument_list (env : env) ((v1, v2, v3) : CST.argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_anon_choice_exp_stmt_047b57a env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_exp_stmt_047b57a env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_array_type (env : env) ((v1, v2, v3, v4) : CST.array_type) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_expression_statement env v2 in
  let v3 = (* "]" *) token env v3 in
  let v4 = map_type_ env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_choice_STAR_exp (v1, v2, v3) -> R.Case ("Exp_choice_STAR_exp",
      let v1 = map_expression_statement env v1 in
      let v2 =
        (match v2 with
        | `STAR tok -> R.Case ("STAR",
            (* "*" *) token env tok
          )
        | `SLASH tok -> R.Case ("SLASH",
            (* "/" *) token env tok
          )
        | `PERC tok -> R.Case ("PERC",
            (* "%" *) token env tok
          )
        | `LTLT tok -> R.Case ("LTLT",
            (* "<<" *) token env tok
          )
        | `GTGT tok -> R.Case ("GTGT",
            (* ">>" *) token env tok
          )
        | `AMP tok -> R.Case ("AMP",
            (* "&" *) token env tok
          )
        | `AMPHAT tok -> R.Case ("AMPHAT",
            (* "&^" *) token env tok
          )
        )
      in
      let v3 = map_expression_statement env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_choice_PLUS_exp (v1, v2, v3) -> R.Case ("Exp_choice_PLUS_exp",
      let v1 = map_expression_statement env v1 in
      let v2 =
        (match v2 with
        | `PLUS tok -> R.Case ("PLUS",
            (* "+" *) token env tok
          )
        | `DASH tok -> R.Case ("DASH",
            (* "-" *) token env tok
          )
        | `BAR tok -> R.Case ("BAR",
            (* "|" *) token env tok
          )
        | `HAT tok -> R.Case ("HAT",
            (* "^" *) token env tok
          )
        )
      in
      let v3 = map_expression_statement env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_choice_EQEQ_exp (v1, v2, v3) -> R.Case ("Exp_choice_EQEQ_exp",
      let v1 = map_expression_statement env v1 in
      let v2 =
        (match v2 with
        | `EQEQ tok -> R.Case ("EQEQ",
            (* "==" *) token env tok
          )
        | `BANGEQ tok -> R.Case ("BANGEQ",
            (* "!=" *) token env tok
          )
        | `LT tok -> R.Case ("LT",
            (* "<" *) token env tok
          )
        | `LTEQ tok -> R.Case ("LTEQ",
            (* "<=" *) token env tok
          )
        | `GT tok -> R.Case ("GT",
            (* ">" *) token env tok
          )
        | `GTEQ tok -> R.Case ("GTEQ",
            (* ">=" *) token env tok
          )
        )
      in
      let v3 = map_expression_statement env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMPAMP_exp (v1, v2, v3) -> R.Case ("Exp_AMPAMP_exp",
      let v1 = map_expression_statement env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression_statement env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BARBAR_exp (v1, v2, v3) -> R.Case ("Exp_BARBAR_exp",
      let v1 = map_expression_statement env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression_statement env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_statement_list env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_call_expression (env : env) (x : CST.call_expression) =
  (match x with
  | `Choice_new_spec_arg_list (v1, v2) -> R.Case ("Choice_new_spec_arg_list",
      let v1 = map_anon_choice_new_0342769 env v1 in
      let v2 = map_special_argument_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Exp_opt_type_args_arg_list (v1, v2, v3) -> R.Case ("Exp_opt_type_args_arg_list",
      let v1 = map_expression_statement env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_arguments env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_argument_list env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_channel_type (env : env) (x : CST.channel_type) =
  (match x with
  | `Chan_choice_simple_type (v1, v2) -> R.Case ("Chan_choice_simple_type",
      let v1 = (* "chan" *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    )
  | `Chan_LTDASH_choice_simple_type (v1, v2, v3) -> R.Case ("Chan_LTDASH_choice_simple_type",
      let v1 = (* "chan" *) token env v1 in
      let v2 = (* "<-" *) token env v2 in
      let v3 = map_type_ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `LTDASH_chan_choice_simple_type (v1, v2, v3) -> R.Case ("LTDASH_chan_choice_simple_type",
      let v1 = (* "<-" *) token env v1 in
      let v2 = (* "chan" *) token env v2 in
      let v3 = map_type_ env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_communication_case (env : env) ((v1, v2, v3, v4) : CST.communication_case) =
  let v1 = (* "case" *) token env v1 in
  let v2 =
    (match v2 with
    | `Send_stmt x -> R.Case ("Send_stmt",
        map_send_statement env x
      )
    | `Rece_stmt x -> R.Case ("Rece_stmt",
        map_receive_statement env x
      )
    )
  in
  let v3 = (* ":" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_statement_list env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_composite_literal (env : env) ((v1, v2) : CST.composite_literal) =
  let v1 =
    (match v1 with
    | `Map_type x -> R.Case ("Map_type",
        map_map_type env x
      )
    | `Slice_type x -> R.Case ("Slice_type",
        map_slice_type env x
      )
    | `Array_type x -> R.Case ("Array_type",
        map_array_type env x
      )
    | `Impl_len_array_type x -> R.Case ("Impl_len_array_type",
        map_implicit_length_array_type env x
      )
    | `Struct_type x -> R.Case ("Struct_type",
        map_struct_type env x
      )
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Gene_type x -> R.Case ("Gene_type",
        map_generic_type env x
      )
    | `Qual_type x -> R.Case ("Qual_type",
        map_qualified_type env x
      )
    )
  in
  let v2 = map_literal_value env v2 in
  R.Tuple [v1; v2]

and map_const_spec (env : env) ((v1, v2, v3) : CST.const_spec) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_type_ env x
            ))
          | None -> R.Option None)
        in
        let v2 = (* "=" *) token env v2 in
        let v3 = map_expression_list env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Const_decl (v1, v2) -> R.Case ("Const_decl",
      let v1 = (* "const" *) token env v1 in
      let v2 =
        (match v2 with
        | `Const_spec x -> R.Case ("Const_spec",
            map_const_spec env x
          )
        | `LPAR_rep_const_spec_choice_pat_1d78758_RPAR (v1, v2, v3) -> R.Case ("LPAR_rep_const_spec_choice_pat_1d78758_RPAR",
            let v1 = (* "(" *) token env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = map_const_spec env v1 in
                let v2 = map_anon_choice_pat_1d78758_89f618f env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            let v3 = (* ")" *) token env v3 in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Type_decl (v1, v2) -> R.Case ("Type_decl",
      let v1 = (* "type" *) token env v1 in
      let v2 =
        (match v2 with
        | `Type_spec x -> R.Case ("Type_spec",
            map_type_spec env x
          )
        | `Type_alias x -> R.Case ("Type_alias",
            map_type_alias env x
          )
        | `LPAR_rep_choice_type_spec_choice_pat_1d78758_RPAR (v1, v2, v3) -> R.Case ("LPAR_rep_choice_type_spec_choice_pat_1d78758_RPAR",
            let v1 = (* "(" *) token env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 =
                  (match v1 with
                  | `Type_spec x -> R.Case ("Type_spec",
                      map_type_spec env x
                    )
                  | `Type_alias x -> R.Case ("Type_alias",
                      map_type_alias env x
                    )
                  )
                in
                let v2 = map_anon_choice_pat_1d78758_89f618f env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            let v3 = (* ")" *) token env v3 in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Var_decl (v1, v2) -> R.Case ("Var_decl",
      let v1 = (* "var" *) token env v1 in
      let v2 =
        (match v2 with
        | `Var_spec x -> R.Case ("Var_spec",
            map_var_spec env x
          )
        | `Var_spec_list x -> R.Case ("Var_spec_list",
            map_var_spec_list env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  )

and map_default_case (env : env) ((v1, v2, v3) : CST.default_case) =
  let v1 = (* "default" *) token env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_statement_list env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_defer_statement (env : env) ((v1, v2) : CST.defer_statement) =
  let v1 = (* "defer" *) token env v1 in
  let v2 = map_expression_statement env v2 in
  R.Tuple [v1; v2]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Choice_un_exp x -> R.Case ("Choice_un_exp",
      (match x with
      | `Un_exp x -> R.Case ("Un_exp",
          map_unary_expression env x
        )
      | `Bin_exp x -> R.Case ("Bin_exp",
          map_binary_expression env x
        )
      | `Sele_exp x -> R.Case ("Sele_exp",
          map_selector_expression env x
        )
      | `Index_exp x -> R.Case ("Index_exp",
          map_index_expression env x
        )
      | `Slice_exp x -> R.Case ("Slice_exp",
          map_slice_expression env x
        )
      | `Call_exp x -> R.Case ("Call_exp",
          map_call_expression env x
        )
      | `Type_asse_exp x -> R.Case ("Type_asse_exp",
          map_type_assertion_expression env x
        )
      | `Type_conv_exp x -> R.Case ("Type_conv_exp",
          map_type_conversion_expression env x
        )
      | `Type_inst_exp x -> R.Case ("Type_inst_exp",
          map_type_instantiation_expression env x
        )
      | `Id tok -> R.Case ("Id",
          (* identifier *) token env tok
        )
      | `Choice_new x -> R.Case ("Choice_new",
          map_anon_choice_new_0342769 env x
        )
      | `Comp_lit x -> R.Case ("Comp_lit",
          map_composite_literal env x
        )
      | `Func_lit x -> R.Case ("Func_lit",
          map_func_literal env x
        )
      | `Choice_raw_str_lit x -> R.Case ("Choice_raw_str_lit",
          map_string_literal env x
        )
      | `Int_lit tok -> R.Case ("Int_lit",
          (* int_literal *) token env tok
        )
      | `Float_lit tok -> R.Case ("Float_lit",
          (* float_literal *) token env tok
        )
      | `Imag_lit tok -> R.Case ("Imag_lit",
          (* imaginary_literal *) token env tok
        )
      | `Rune_lit tok -> R.Case ("Rune_lit",
          (* rune_literal *) token env tok
        )
      | `Nil tok -> R.Case ("Nil",
          (* "nil" *) token env tok
        )
      | `True tok -> R.Case ("True",
          (* "true" *) token env tok
        )
      | `False tok -> R.Case ("False",
          (* "false" *) token env tok
        )
      | `Iota tok -> R.Case ("Iota",
          (* "iota" *) token env tok
        )
      | `Paren_exp x -> R.Case ("Paren_exp",
          map_parenthesized_expression env x
        )
      )
    )
  | `Semg_ellips_meta tok -> R.Case ("Semg_ellips_meta",
      (* pattern \$\.\.\.[a-zA-Z_][a-zA-Z_0-9]* *) token env tok
    )
  | `Semg_deep_ellips x -> R.Case ("Semg_deep_ellips",
      map_semgrep_deep_ellipsis env x
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Typed_meta (v1, v2, v3, v4, v5) -> R.Case ("Typed_meta",
      let v1 = (* "(" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_type_ env v4 in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_expression_case (env : env) ((v1, v2, v3, v4) : CST.expression_case) =
  let v1 = (* "case" *) token env v1 in
  let v2 = map_expression_list env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_statement_list env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_expression_list (env : env) ((v1, v2) : CST.expression_list) =
  let v1 = map_expression_statement env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression_statement env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_expression_statement (env : env) (x : CST.expression_statement) =
  map_expression env x

and map_expression_switch_statement (env : env) ((v1, v2, v3, v4, v5, v6) : CST.expression_switch_statement) =
  let v1 = (* "switch" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_simple_statement env v1 in
        let v2 = (* ";" *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_expression_statement env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "{" *) token env v4 in
  let v5 =
    R.List (List.map (fun x ->
      (match x with
      | `Exp_case x -> R.Case ("Exp_case",
          map_expression_case env x
        )
      | `Defa_case x -> R.Case ("Defa_case",
          map_default_case env x
        )
      )
    ) v5)
  in
  let v6 = (* "}" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_field_declaration (env : env) ((v1, v2) : CST.field_declaration) =
  let v1 =
    (match v1 with
    | `Id_rep_COMMA_id_choice_simple_type (v1, v2, v3) -> R.Case ("Id_rep_COMMA_id_choice_simple_type",
        let v1 = (* identifier *) token env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = (* identifier *) token env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 = map_type_ env v3 in
        R.Tuple [v1; v2; v3]
      )
    | `Opt_STAR_choice_id (v1, v2) -> R.Case ("Opt_STAR_choice_id",
        let v1 =
          (match v1 with
          | Some tok -> R.Option (Some (
              (* "*" *) token env tok
            ))
          | None -> R.Option None)
        in
        let v2 =
          (match v2 with
          | `Id tok -> R.Case ("Id",
              (* identifier *) token env tok
            )
          | `Qual_type x -> R.Case ("Qual_type",
              map_qualified_type env x
            )
          | `Gene_type x -> R.Case ("Gene_type",
              map_generic_type env x
            )
          )
        in
        R.Tuple [v1; v2]
      )
    )
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_string_literal env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_field_declaration_list (env : env) ((v1, v2, v3) : CST.field_declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_field_declaration env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = map_anon_choice_pat_1d78758_89f618f env v1 in
            let v2 = map_field_declaration env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_anon_choice_pat_1d78758_89f618f env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_for_clause (env : env) ((v1, v2, v3, v4, v5) : CST.for_clause) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_simple_statement env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* ";" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_expression_statement env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* ";" *) token env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_simple_statement env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_for_statement (env : env) ((v1, v2, v3) : CST.for_statement) =
  let v1 = (* "for" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Exp x -> R.Case ("Exp",
            map_expression_statement env x
          )
        | `For_clause x -> R.Case ("For_clause",
            map_for_clause env x
          )
        | `Range_clause x -> R.Case ("Range_clause",
            map_range_clause env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 = map_block env v3 in
  R.Tuple [v1; v2; v3]

and map_func_literal (env : env) ((v1, v2, v3, v4) : CST.func_literal) =
  let v1 = (* "func" *) token env v1 in
  let v2 = map_parameter_list env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_choice_param_list_29faba4 env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_block env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_generic_type (env : env) ((v1, v2) : CST.generic_type) =
  let v1 =
    (match v1 with
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Qual_type x -> R.Case ("Qual_type",
        map_qualified_type env x
      )
    | `Nega_type x -> R.Case ("Nega_type",
        map_negated_type env x
      )
    )
  in
  let v2 = map_type_arguments env v2 in
  R.Tuple [v1; v2]

and map_go_statement (env : env) ((v1, v2) : CST.go_statement) =
  let v1 = (* "go" *) token env v1 in
  let v2 = map_expression_statement env v2 in
  R.Tuple [v1; v2]

and map_if_statement (env : env) ((v1, v2, v3, v4, v5) : CST.if_statement) =
  let v1 = (* "if" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_simple_statement env v1 in
        let v2 = (* ";" *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = map_expression_statement env v3 in
  let v4 = map_block env v4 in
  let v5 =
    (match v5 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "else" *) token env v1 in
        let v2 =
          (match v2 with
          | `Blk x -> R.Case ("Blk",
              map_block env x
            )
          | `If_stmt x -> R.Case ("If_stmt",
              map_if_statement env x
            )
          )
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_implicit_length_array_type (env : env) ((v1, v2, v3, v4) : CST.implicit_length_array_type) =
  let v1 = (* "[" *) token env v1 in
  let v2 = (* "..." *) token env v2 in
  let v3 = (* "]" *) token env v3 in
  let v4 = map_type_ env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_index_expression (env : env) ((v1, v2, v3, v4) : CST.index_expression) =
  let v1 = map_expression_statement env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 = map_expression_statement env v3 in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_interface_elem (env : env) (x : CST.interface_elem) =
  (match x with
  | `Meth_elem (v1, v2, v3) -> R.Case ("Meth_elem",
      let v1 = (* identifier *) token env v1 in
      let v2 = map_parameter_list env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_anon_choice_param_list_29faba4 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Type_elem x -> R.Case ("Type_elem",
      map_type_elem env x
    )
  )

and map_labeled_statement (env : env) ((v1, v2, v3) : CST.labeled_statement) =
  let v1 = (* identifier *) token env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_literal_element (env : env) (x : CST.literal_element) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression_statement env x
    )
  | `Lit_value x -> R.Case ("Lit_value",
      map_literal_value env x
    )
  )

and map_literal_value (env : env) ((v1, v2, v3) : CST.literal_value) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = map_anon_choice_lit_elem_0952f3f env v1 in
              let v2 =
                R.List (List.map (fun (v1, v2) ->
                  let v1 = (* "," *) token env v1 in
                  let v2 = map_anon_choice_lit_elem_0952f3f env v2 in
                  R.Tuple [v1; v2]
                ) v2)
              in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        let v2 =
          (match v2 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_map_type (env : env) ((v1, v2, v3, v4, v5) : CST.map_type) =
  let v1 = (* "map" *) token env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 = map_type_ env v3 in
  let v4 = (* "]" *) token env v4 in
  let v5 = map_type_ env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_negated_type (env : env) ((v1, v2) : CST.negated_type) =
  let v1 = (* "~" *) token env v1 in
  let v2 = map_type_ env v2 in
  R.Tuple [v1; v2]

and map_parameter_declaration (env : env) (x : CST.parameter_declaration) =
  (match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Semg_ellips_meta tok -> R.Case ("Semg_ellips_meta",
      (* pattern \$\.\.\.[a-zA-Z_][a-zA-Z_0-9]* *) token env tok
    )
  | `Opt_id_rep_COMMA_id_choice_simple_type (v1, v2) -> R.Case ("Opt_id_rep_COMMA_id_choice_simple_type",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_field_name_list env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = map_anon_choice_param_decl_18823e5 env v1 in
              let v2 =
                R.List (List.map (fun (v1, v2) ->
                  let v1 = (* "," *) token env v1 in
                  let v2 = map_anon_choice_param_decl_18823e5 env v2 in
                  R.Tuple [v1; v2]
                ) v2)
              in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        let v2 =
          (match v2 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expression_statement env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_range_clause (env : env) ((v1, v2, v3) : CST.range_clause) =
  let v1 =
    (match v1 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_expression_list env v1 in
        let v2 = map_anon_choice_EQ_4ccabd6 env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v2 = (* "range" *) token env v2 in
  let v3 = map_expression_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_receive_statement (env : env) ((v1, v2) : CST.receive_statement) =
  let v1 =
    (match v1 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_expression_list env v1 in
        let v2 = map_anon_choice_EQ_4ccabd6 env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v2 = map_expression_statement env v2 in
  R.Tuple [v1; v2]

and map_return_statement (env : env) ((v1, v2) : CST.return_statement) =
  let v1 = (* "return" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_expression_list env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_select_statement (env : env) ((v1, v2, v3, v4) : CST.select_statement) =
  let v1 = (* "select" *) token env v1 in
  let v2 = (* "{" *) token env v2 in
  let v3 =
    R.List (List.map (fun x ->
      (match x with
      | `Comm_case x -> R.Case ("Comm_case",
          map_communication_case env x
        )
      | `Defa_case x -> R.Case ("Defa_case",
          map_default_case env x
        )
      )
    ) v3)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_selector_expression (env : env) ((v1, v2, v3) : CST.selector_expression) =
  let v1 = map_expression_statement env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_semgrep_deep_ellipsis (env : env) ((v1, v2, v3) : CST.semgrep_deep_ellipsis) =
  let v1 = (* "<..." *) token env v1 in
  let v2 = map_expression_statement env v2 in
  let v3 = (* "...>" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_send_statement (env : env) ((v1, v2, v3) : CST.send_statement) =
  let v1 = map_expression_statement env v1 in
  let v2 = (* "<-" *) token env v2 in
  let v3 = map_expression_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_simple_statement (env : env) (x : CST.simple_statement) =
  (match x with
  | `Exp_stmt x -> R.Case ("Exp_stmt",
      map_expression_statement env x
    )
  | `Send_stmt x -> R.Case ("Send_stmt",
      map_send_statement env x
    )
  | `Inc_stmt (v1, v2) -> R.Case ("Inc_stmt",
      let v1 = map_expression_statement env v1 in
      let v2 = (* "++" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Dec_stmt (v1, v2) -> R.Case ("Dec_stmt",
      let v1 = map_expression_statement env v1 in
      let v2 = (* "--" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Assign_stmt (v1, v2, v3) -> R.Case ("Assign_stmt",
      let v1 = map_expression_list env v1 in
      let v2 =
        (match v2 with
        | `STAREQ tok -> R.Case ("STAREQ",
            (* "*=" *) token env tok
          )
        | `SLASHEQ tok -> R.Case ("SLASHEQ",
            (* "/=" *) token env tok
          )
        | `PERCEQ tok -> R.Case ("PERCEQ",
            (* "%=" *) token env tok
          )
        | `LTLTEQ tok -> R.Case ("LTLTEQ",
            (* "<<=" *) token env tok
          )
        | `GTGTEQ tok -> R.Case ("GTGTEQ",
            (* ">>=" *) token env tok
          )
        | `AMPEQ tok -> R.Case ("AMPEQ",
            (* "&=" *) token env tok
          )
        | `AMPHATEQ tok -> R.Case ("AMPHATEQ",
            (* "&^=" *) token env tok
          )
        | `PLUSEQ tok -> R.Case ("PLUSEQ",
            (* "+=" *) token env tok
          )
        | `DASHEQ tok -> R.Case ("DASHEQ",
            (* "-=" *) token env tok
          )
        | `BAREQ tok -> R.Case ("BAREQ",
            (* "|=" *) token env tok
          )
        | `HATEQ tok -> R.Case ("HATEQ",
            (* "^=" *) token env tok
          )
        | `EQ tok -> R.Case ("EQ",
            (* "=" *) token env tok
          )
        )
      in
      let v3 = map_expression_list env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Short_var_decl (v1, v2, v3) -> R.Case ("Short_var_decl",
      let v1 = map_expression_list env v1 in
      let v2 = (* ":=" *) token env v2 in
      let v3 = map_expression_list env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_simple_type (env : env) (x : CST.simple_type) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Gene_type x -> R.Case ("Gene_type",
      map_generic_type env x
    )
  | `Qual_type x -> R.Case ("Qual_type",
      map_qualified_type env x
    )
  | `Poin_type (v1, v2) -> R.Case ("Poin_type",
      let v1 = (* "*" *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    )
  | `Struct_type x -> R.Case ("Struct_type",
      map_struct_type env x
    )
  | `Inte_type (v1, v2, v3, v4) -> R.Case ("Inte_type",
      let v1 = (* "interface" *) token env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = map_interface_elem env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = map_anon_choice_pat_1d78758_89f618f env v1 in
                let v2 = map_interface_elem env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            let v3 =
              (match v3 with
              | Some x -> R.Option (Some (
                  map_anon_choice_pat_1d78758_89f618f env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      let v4 = (* "}" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Array_type x -> R.Case ("Array_type",
      map_array_type env x
    )
  | `Slice_type x -> R.Case ("Slice_type",
      map_slice_type env x
    )
  | `Map_type x -> R.Case ("Map_type",
      map_map_type env x
    )
  | `Chan_type x -> R.Case ("Chan_type",
      map_channel_type env x
    )
  | `Func_type (v1, v2, v3) -> R.Case ("Func_type",
      let v1 = (* "func" *) token env v1 in
      let v2 = map_parameter_list env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_anon_choice_param_list_29faba4 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Nega_type x -> R.Case ("Nega_type",
      map_negated_type env x
    )
  )

and map_slice_expression (env : env) ((v1, v2, v3, v4) : CST.slice_expression) =
  let v1 = map_expression_statement env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 =
    (match v3 with
    | `Opt_exp_COLON_opt_exp (v1, v2, v3) -> R.Case ("Opt_exp_COLON_opt_exp",
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_expression_statement env x
            ))
          | None -> R.Option None)
        in
        let v2 = (* ":" *) token env v2 in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_expression_statement env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      )
    | `Opt_exp_COLON_exp_COLON_exp (v1, v2, v3, v4, v5) -> R.Case ("Opt_exp_COLON_exp_COLON_exp",
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_expression_statement env x
            ))
          | None -> R.Option None)
        in
        let v2 = (* ":" *) token env v2 in
        let v3 = map_expression_statement env v3 in
        let v4 = (* ":" *) token env v4 in
        let v5 = map_expression_statement env v5 in
        R.Tuple [v1; v2; v3; v4; v5]
      )
    )
  in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_slice_type (env : env) ((v1, v2, v3) : CST.slice_type) =
  let v1 = (* "[" *) token env v1 in
  let v2 = (* "]" *) token env v2 in
  let v3 = map_type_ env v3 in
  R.Tuple [v1; v2; v3]

and map_special_argument_list (env : env) ((v1, v2, v3) : CST.special_argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_type_ env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_expression_statement env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Choice_decl x -> R.Case ("Choice_decl",
      (match x with
      | `Decl x -> R.Case ("Decl",
          map_declaration env x
        )
      | `Simple_stmt x -> R.Case ("Simple_stmt",
          map_simple_statement env x
        )
      | `Ret_stmt x -> R.Case ("Ret_stmt",
          map_return_statement env x
        )
      | `Go_stmt x -> R.Case ("Go_stmt",
          map_go_statement env x
        )
      | `Defer_stmt x -> R.Case ("Defer_stmt",
          map_defer_statement env x
        )
      | `If_stmt x -> R.Case ("If_stmt",
          map_if_statement env x
        )
      | `For_stmt x -> R.Case ("For_stmt",
          map_for_statement env x
        )
      | `Exp_switch_stmt x -> R.Case ("Exp_switch_stmt",
          map_expression_switch_statement env x
        )
      | `Type_switch_stmt x -> R.Case ("Type_switch_stmt",
          map_type_switch_statement env x
        )
      | `Select_stmt x -> R.Case ("Select_stmt",
          map_select_statement env x
        )
      | `Labe_stmt x -> R.Case ("Labe_stmt",
          map_labeled_statement env x
        )
      | `Fall_stmt tok -> R.Case ("Fall_stmt",
          (* "fallthrough" *) token env tok
        )
      | `Brk_stmt x -> R.Case ("Brk_stmt",
          map_break_statement env x
        )
      | `Cont_stmt x -> R.Case ("Cont_stmt",
          map_continue_statement env x
        )
      | `Goto_stmt x -> R.Case ("Goto_stmt",
          map_goto_statement env x
        )
      | `Blk x -> R.Case ("Blk",
          map_block env x
        )
      | `Empty_stmt tok -> R.Case ("Empty_stmt",
          (* ";" *) token env tok
        )
      )
    )
  | `Semg_ellips_meta tok -> R.Case ("Semg_ellips_meta",
      (* pattern \$\.\.\.[a-zA-Z_][a-zA-Z_0-9]* *) token env tok
    )
  | `Semg_deep_ellips x -> R.Case ("Semg_deep_ellips",
      map_semgrep_deep_ellipsis env x
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

and map_statement_list (env : env) (x : CST.statement_list) =
  (match x with
  | `Stmt_rep_choice_pat_1d78758_stmt_opt_choice_pat_1d78758_opt_empty_labe_stmt (v1, v2, v3) -> R.Case ("Stmt_rep_choice_pat_1d78758_stmt_opt_choice_pat_1d78758_opt_empty_labe_stmt",
      let v1 = map_statement env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_anon_choice_pat_1d78758_89f618f env v1 in
          let v2 = map_statement env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_anon_choice_pat_1d78758_89f618f env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_empty_labeled_statement env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Empty_labe_stmt x -> R.Case ("Empty_labe_stmt",
      map_empty_labeled_statement env x
    )
  )

and map_struct_type (env : env) ((v1, v2) : CST.struct_type) =
  let v1 = (* "struct" *) token env v1 in
  let v2 = map_field_declaration_list env v2 in
  R.Tuple [v1; v2]

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Simple_type x -> R.Case ("Simple_type",
      map_simple_type env x
    )
  | `Paren_type (v1, v2, v3) -> R.Case ("Paren_type",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_type_ env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_type_alias (env : env) ((v1, v2, v3) : CST.type_alias) =
  let v1 = (* identifier *) token env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_type_ env v3 in
  R.Tuple [v1; v2; v3]

and map_type_arguments (env : env) ((v1, v2, v3, v4, v5) : CST.type_arguments) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_type_elem env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_elem env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* "]" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_type_assertion_expression (env : env) ((v1, v2, v3, v4, v5) : CST.type_assertion_expression) =
  let v1 = map_expression_statement env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = (* "(" *) token env v3 in
  let v4 = map_type_ env v4 in
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_type_case (env : env) ((v1, v2, v3, v4, v5) : CST.type_case) =
  let v1 = (* "case" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ":" *) token env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_statement_list env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_type_conversion_expression (env : env) ((v1, v2, v3, v4, v5) : CST.type_conversion_expression) =
  let v1 = map_type_ env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_expression_statement env v3 in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_type_elem (env : env) ((v1, v2) : CST.type_elem) =
  let v1 = map_type_ env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "|" *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_type_instantiation_expression (env : env) ((v1, v2, v3, v4, v5, v6) : CST.type_instantiation_expression) =
  let v1 = map_type_ env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 = map_type_ env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  let v5 =
    (match v5 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v6 = (* "]" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_type_parameter_declaration (env : env) ((v1, v2, v3) : CST.type_parameter_declaration) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 = map_type_elem env v3 in
  R.Tuple [v1; v2; v3]

and map_type_parameter_list (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameter_list) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_type_parameter_declaration env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_parameter_declaration env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* "]" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_type_spec (env : env) ((v1, v2, v3) : CST.type_spec) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_parameter_list env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_type_ env v3 in
  R.Tuple [v1; v2; v3]

and map_type_switch_header (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.type_switch_header) =
  let v1 =
    (match v1 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_simple_statement env v1 in
        let v2 = (* ";" *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_expression_list env v1 in
        let v2 = (* ":=" *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = map_expression_statement env v3 in
  let v4 = (* "." *) token env v4 in
  let v5 = (* "(" *) token env v5 in
  let v6 = (* "type" *) token env v6 in
  let v7 = (* ")" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_type_switch_statement (env : env) ((v1, v2, v3, v4, v5) : CST.type_switch_statement) =
  let v1 = (* "switch" *) token env v1 in
  let v2 = map_type_switch_header env v2 in
  let v3 = (* "{" *) token env v3 in
  let v4 =
    R.List (List.map (fun x ->
      (match x with
      | `Type_case x -> R.Case ("Type_case",
          map_type_case env x
        )
      | `Defa_case x -> R.Case ("Defa_case",
          map_default_case env x
        )
      )
    ) v4)
  in
  let v5 = (* "}" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_unary_expression (env : env) ((v1, v2) : CST.unary_expression) =
  let v1 =
    (match v1 with
    | `PLUS tok -> R.Case ("PLUS",
        (* "+" *) token env tok
      )
    | `DASH tok -> R.Case ("DASH",
        (* "-" *) token env tok
      )
    | `BANG tok -> R.Case ("BANG",
        (* "!" *) token env tok
      )
    | `HAT tok -> R.Case ("HAT",
        (* "^" *) token env tok
      )
    | `STAR tok -> R.Case ("STAR",
        (* "*" *) token env tok
      )
    | `AMP tok -> R.Case ("AMP",
        (* "&" *) token env tok
      )
    | `LTDASH tok -> R.Case ("LTDASH",
        (* "<-" *) token env tok
      )
    )
  in
  let v2 = map_expression_statement env v2 in
  R.Tuple [v1; v2]

and map_var_spec (env : env) ((v1, v2, v3) : CST.var_spec) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | `Choice_simple_type_opt_EQ_exp_list (v1, v2) -> R.Case ("Choice_simple_type_opt_EQ_exp_list",
        let v1 = map_type_ env v1 in
        let v2 =
          (match v2 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = (* "=" *) token env v1 in
              let v2 = map_expression_list env v2 in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    | `EQ_exp_list (v1, v2) -> R.Case ("EQ_exp_list",
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression_list env v2 in
        R.Tuple [v1; v2]
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_var_spec_list (env : env) ((v1, v2, v3) : CST.var_spec_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_var_spec env v1 in
      let v2 = map_anon_choice_pat_1d78758_89f618f env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_import_spec (env : env) ((v1, v2) : CST.import_spec) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        (match x with
        | `Dot tok -> R.Case ("Dot",
            (* "." *) token env tok
          )
        | `Blank_id tok -> R.Case ("Blank_id",
            (* "_" *) token env tok
          )
        | `Id tok -> R.Case ("Id",
            (* identifier *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  let v2 = map_string_literal env v2 in
  R.Tuple [v1; v2]

let map_import_spec_list (env : env) ((v1, v2, v3) : CST.import_spec_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_import_spec env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = map_anon_choice_pat_1d78758_89f618f env v1 in
            let v2 = map_import_spec env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_anon_choice_pat_1d78758_89f618f env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_top_level_declaration (env : env) (x : CST.top_level_declaration) =
  (match x with
  | `Pack_clause (v1, v2) -> R.Case ("Pack_clause",
      let v1 = (* "package" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Func_decl (v1, v2, v3, v4, v5, v6) -> R.Case ("Func_decl",
      let v1 = (* "func" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_parameter_list env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_parameter_list env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_param_list_29faba4 env x
          ))
        | None -> R.Option None)
      in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_block env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Meth_decl (v1, v2, v3, v4, v5, v6) -> R.Case ("Meth_decl",
      let v1 = (* "func" *) token env v1 in
      let v2 = map_parameter_list env v2 in
      let v3 = (* identifier *) token env v3 in
      let v4 = map_parameter_list env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_param_list_29faba4 env x
          ))
        | None -> R.Option None)
      in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_block env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Import_decl (v1, v2) -> R.Case ("Import_decl",
      let v1 = (* "import" *) token env v1 in
      let v2 =
        (match v2 with
        | `Import_spec x -> R.Case ("Import_spec",
            map_import_spec env x
          )
        | `Import_spec_list x -> R.Case ("Import_spec_list",
            map_import_spec_list env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  )

let map_source_file (env : env) ((v1, v2) : CST.source_file) =
  let v1 =
    R.List (List.map (fun x ->
      (match x with
      | `Stmt_choice_pat_1d78758 (v1, v2) -> R.Case ("Stmt_choice_pat_1d78758",
          let v1 = map_statement env v1 in
          let v2 = map_anon_choice_pat_1d78758_89f618f env v2 in
          R.Tuple [v1; v2]
        )
      | `Choice_pack_clause_choice_pat_1d78758 (v1, v2) -> R.Case ("Choice_pack_clause_choice_pat_1d78758",
          let v1 = map_top_level_declaration env v1 in
          let v2 = map_anon_choice_pat_1d78758_89f618f env v2 in
          R.Tuple [v1; v2]
        )
      )
    ) v1)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_top_level_declaration env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let dump_tree root =
  map_source_file () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | Comment (_loc, x) -> ("comment", "comment", map_comment env x)

let dump_extras (extras : CST.extras) =
  List.iter (fun extra ->
    let ts_rule_name, ocaml_type_name, raw_tree = map_extra () extra in
    let details =
      if ocaml_type_name <> ts_rule_name then
        Printf.sprintf " (OCaml type '%s')" ocaml_type_name
      else
        ""
    in
    Printf.printf "%s%s:\n" ts_rule_name details;
    Tree_sitter_run.Raw_tree.to_channel stdout raw_tree
  ) extras
