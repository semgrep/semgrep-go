(**
   Boilerplate to be used as a template when mapping the go CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (_tok : Tree_sitter_run.Token.t) =
  failwith "not implemented"

let blank (env : env) () =
  failwith "not implemented"

let todo (env : env) _ =
   failwith "not implemented"

let map_imaginary_literal (env : env) (tok : CST.imaginary_literal) =
  (* imaginary_literal *) token env tok

let map_float_literal (env : env) (tok : CST.float_literal) =
  (* float_literal *) token env tok

let map_anon_choice_LF_249c99f (env : env) (x : CST.anon_choice_LF_249c99f) =
  (match x with
  | `LF tok -> (* "\n" *) token env tok
  | `SEMI tok -> (* ";" *) token env tok
  )

let map_anon_choice_new_0342769 (env : env) (x : CST.anon_choice_new_0342769) =
  (match x with
  | `New tok -> (* "new" *) token env tok
  | `Make tok -> (* "make" *) token env tok
  )

let map_identifier (env : env) (tok : CST.identifier) =
  (* identifier *) token env tok

let map_rune_literal (env : env) (tok : CST.rune_literal) =
  (* rune_literal *) token env tok

let map_raw_string_literal (env : env) (tok : CST.raw_string_literal) =
  (* raw_string_literal *) token env tok

let map_int_literal (env : env) (tok : CST.int_literal) =
  (* int_literal *) token env tok

let map_anon_choice_EQ_4ccabd6 (env : env) (x : CST.anon_choice_EQ_4ccabd6) =
  (match x with
  | `EQ tok -> (* "=" *) token env tok
  | `COLONEQ tok -> (* ":=" *) token env tok
  )

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_interpreted_string_literal_basic_content (env : env) (tok : CST.interpreted_string_literal_basic_content) =
  (* pattern "[^\"\\n\\\\]+" *) token env tok

let map_field_name_list (env : env) ((v1, v2) : CST.field_name_list) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

let map_empty_labeled_statement (env : env) ((v1, v2) : CST.empty_labeled_statement) =
  let v1 = (* identifier *) token env v1 in
  let v2 = (* ":" *) token env v2 in
  todo env (v1, v2)

let map_constraint_term (env : env) ((v1, v2) : CST.constraint_term) =
  let v1 =
    (match v1 with
    | Some tok -> (* "~" *) token env tok
    | None -> todo env ())
  in
  let v2 = (* identifier *) token env v2 in
  todo env (v1, v2)

let map_qualified_type (env : env) ((v1, v2, v3) : CST.qualified_type) =
  let v1 = (* identifier *) token env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  todo env (v1, v2, v3)

let map_string_literal (env : env) (x : CST.string_literal) =
  (match x with
  | `Raw_str_lit tok -> (* raw_string_literal *) token env tok
  | `Inte_str_lit (v1, v2, v3) ->
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        List.map (fun x ->
          (match x with
          | `Inte_str_lit_basic_content tok ->
              (* pattern "[^\"\\n\\\\]+" *) token env tok
          | `Esc_seq tok -> (* escape_sequence *) token env tok
          )
        ) v2
      in
      let v3 = (* "\"" *) token env v3 in
      todo env (v1, v2, v3)
  )

let map_interface_type_name (env : env) (x : CST.interface_type_name) =
  (match x with
  | `Id tok -> (* identifier *) token env tok
  | `Qual_type x -> map_qualified_type env x
  )

let map_import_spec (env : env) ((v1, v2) : CST.import_spec) =
  let v1 =
    (match v1 with
    | Some x ->
        (match x with
        | `Dot tok -> (* "." *) token env tok
        | `Blank_id tok -> (* "_" *) token env tok
        | `Id tok -> (* identifier *) token env tok
        )
    | None -> todo env ())
  in
  let v2 = map_string_literal env v2 in
  todo env (v1, v2)

let rec map_anon_choice_elem_c42cd9b (env : env) (x : CST.anon_choice_elem_c42cd9b) =
  (match x with
  | `Elem x -> map_element env x
  | `Keyed_elem (v1, v2) ->
      let v1 =
        (match v1 with
        | `Exp_COLON (v1, v2) ->
            let v1 = map_expression env v1 in
            let v2 = (* ":" *) token env v2 in
            todo env (v1, v2)
        | `Lit_value_COLON (v1, v2) ->
            let v1 = map_literal_value env v1 in
            let v2 = (* ":" *) token env v2 in
            todo env (v1, v2)
        | `Id_COLON x -> map_empty_labeled_statement env x
        )
      in
      let v2 = map_element env v2 in
      todo env (v1, v2)
  )

and map_anon_choice_exp_047b57a (env : env) (x : CST.anon_choice_exp_047b57a) =
  (match x with
  | `Exp x -> map_expression env x
  | `Vari_arg (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = (* "..." *) token env v2 in
      todo env (v1, v2)
  )

and map_anon_choice_param_decl_18823e5 (env : env) (x : CST.anon_choice_param_decl_18823e5) =
  (match x with
  | `Param_decl x -> map_parameter_declaration env x
  | `Vari_param_decl (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some tok -> (* identifier *) token env tok
        | None -> todo env ())
      in
      let v2 = (* "..." *) token env v2 in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  )

and map_anon_choice_param_list_29faba4 (env : env) (x : CST.anon_choice_param_list_29faba4) =
  (match x with
  | `Param_list x -> map_parameter_list env x
  | `Simple_type x -> map_simple_type env x
  )

and map_argument_list (env : env) ((v1, v2, v3) : CST.argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = map_anon_choice_exp_047b57a env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_exp_047b57a env v2 in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | Some tok -> (* "," *) token env tok
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_array_type (env : env) ((v1, v2, v3, v4) : CST.array_type) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "]" *) token env v3 in
  let v4 = map_type_ env v4 in
  todo env (v1, v2, v3, v4)

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_choice_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `STAR tok -> (* "*" *) token env tok
        | `SLASH tok -> (* "/" *) token env tok
        | `PERC tok -> (* "%" *) token env tok
        | `LTLT tok -> (* "<<" *) token env tok
        | `GTGT tok -> (* ">>" *) token env tok
        | `AMP tok -> (* "&" *) token env tok
        | `AMPHAT tok -> (* "&^" *) token env tok
        )
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `PLUS tok -> (* "+" *) token env tok
        | `DASH tok -> (* "-" *) token env tok
        | `BAR tok -> (* "|" *) token env tok
        | `HAT tok -> (* "^" *) token env tok
        )
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `EQEQ tok -> (* "==" *) token env tok
        | `BANGEQ tok -> (* "!=" *) token env tok
        | `LT tok -> (* "<" *) token env tok
        | `LTEQ tok -> (* "<=" *) token env tok
        | `GT tok -> (* ">" *) token env tok
        | `GTEQ tok -> (* ">=" *) token env tok
        )
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  )

and map_block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> map_statement_list env x
    | None -> todo env ())
  in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_call_expression (env : env) (x : CST.call_expression) =
  (match x with
  | `Choice_new_spec_arg_list (v1, v2) ->
      let v1 = map_anon_choice_new_0342769 env v1 in
      let v2 = map_special_argument_list env v2 in
      todo env (v1, v2)
  | `Exp_opt_type_args_arg_list (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_type_arguments env x
        | None -> todo env ())
      in
      let v3 = map_argument_list env v3 in
      todo env (v1, v2, v3)
  )

and map_channel_type (env : env) (x : CST.channel_type) =
  (match x with
  | `Chan_choice_simple_type (v1, v2) ->
      let v1 = (* "chan" *) token env v1 in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
  | `Chan_LTDASH_choice_simple_type (v1, v2, v3) ->
      let v1 = (* "chan" *) token env v1 in
      let v2 = (* "<-" *) token env v2 in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  | `LTDASH_chan_choice_simple_type (v1, v2, v3) ->
      let v1 = (* "<-" *) token env v1 in
      let v2 = (* "chan" *) token env v2 in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  )

and map_communication_case (env : env) ((v1, v2, v3, v4) : CST.communication_case) =
  let v1 = (* "case" *) token env v1 in
  let v2 =
    (match v2 with
    | `Send_stmt x -> map_send_statement env x
    | `Rece_stmt x -> map_receive_statement env x
    )
  in
  let v3 = (* ":" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> map_statement_list env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and map_const_spec (env : env) ((v1, v2, v3) : CST.const_spec) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      todo env (v1, v2)
    ) v2
  in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3) ->
        let v1 =
          (match v1 with
          | Some x -> map_type_ env x
          | None -> todo env ())
        in
        let v2 = (* "=" *) token env v2 in
        let v3 = map_expression_list env v3 in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Const_decl (v1, v2) ->
      let v1 = (* "const" *) token env v1 in
      let v2 =
        (match v2 with
        | `Const_spec x -> map_const_spec env x
        | `LPAR_rep_const_spec_choice_LF_RPAR (v1, v2, v3) ->
            let v1 = (* "(" *) token env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = map_const_spec env v1 in
                let v2 = map_anon_choice_LF_249c99f env v2 in
                todo env (v1, v2)
              ) v2
            in
            let v3 = (* ")" *) token env v3 in
            todo env (v1, v2, v3)
        )
      in
      todo env (v1, v2)
  | `Type_decl (v1, v2) ->
      let v1 = (* "type" *) token env v1 in
      let v2 =
        (match v2 with
        | `Type_spec x -> map_type_spec env x
        | `Type_alias x -> map_type_alias env x
        | `LPAR_rep_choice_type_spec_choice_LF_RPAR (v1, v2, v3) ->
            let v1 = (* "(" *) token env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 =
                  (match v1 with
                  | `Type_spec x -> map_type_spec env x
                  | `Type_alias x -> map_type_alias env x
                  )
                in
                let v2 = map_anon_choice_LF_249c99f env v2 in
                todo env (v1, v2)
              ) v2
            in
            let v3 = (* ")" *) token env v3 in
            todo env (v1, v2, v3)
        )
      in
      todo env (v1, v2)
  | `Var_decl (v1, v2) ->
      let v1 = (* "var" *) token env v1 in
      let v2 =
        (match v2 with
        | `Var_spec x -> map_var_spec env x
        | `LPAR_rep_var_spec_choice_LF_RPAR (v1, v2, v3) ->
            let v1 = (* "(" *) token env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = map_var_spec env v1 in
                let v2 = map_anon_choice_LF_249c99f env v2 in
                todo env (v1, v2)
              ) v2
            in
            let v3 = (* ")" *) token env v3 in
            todo env (v1, v2, v3)
        )
      in
      todo env (v1, v2)
  )

and map_default_case (env : env) ((v1, v2, v3) : CST.default_case) =
  let v1 = (* "default" *) token env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> map_statement_list env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_element (env : env) (x : CST.element) =
  (match x with
  | `Exp x -> map_expression env x
  | `Lit_value x -> map_literal_value env x
  )

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Un_exp (v1, v2) ->
      let v1 =
        (match v1 with
        | `PLUS tok -> (* "+" *) token env tok
        | `DASH tok -> (* "-" *) token env tok
        | `BANG tok -> (* "!" *) token env tok
        | `HAT tok -> (* "^" *) token env tok
        | `STAR tok -> (* "*" *) token env tok
        | `AMP tok -> (* "&" *) token env tok
        | `LTDASH tok -> (* "<-" *) token env tok
        )
      in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Bin_exp x -> map_binary_expression env x
  | `Sele_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      todo env (v1, v2, v3)
  | `Index_exp (v1, v2, v3, v4) ->
      let v1 = map_expression env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Slice_exp (v1, v2, v3, v4) ->
      let v1 = map_expression env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 =
        (match v3 with
        | `Opt_exp_COLON_opt_exp (v1, v2, v3) ->
            let v1 =
              (match v1 with
              | Some x -> map_expression env x
              | None -> todo env ())
            in
            let v2 = (* ":" *) token env v2 in
            let v3 =
              (match v3 with
              | Some x -> map_expression env x
              | None -> todo env ())
            in
            todo env (v1, v2, v3)
        | `Opt_exp_COLON_exp_COLON_exp (v1, v2, v3, v4, v5) ->
            let v1 =
              (match v1 with
              | Some x -> map_expression env x
              | None -> todo env ())
            in
            let v2 = (* ":" *) token env v2 in
            let v3 = map_expression env v3 in
            let v4 = (* ":" *) token env v4 in
            let v5 = map_expression env v5 in
            todo env (v1, v2, v3, v4, v5)
        )
      in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Call_exp x -> map_call_expression env x
  | `Type_asse_exp (v1, v2, v3, v4, v5) ->
      let v1 = map_expression env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* "(" *) token env v3 in
      let v4 = map_type_ env v4 in
      let v5 = (* ")" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Type_conv_exp (v1, v2, v3, v4, v5) ->
      let v1 = map_type_ env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 =
        (match v4 with
        | Some tok -> (* "," *) token env tok
        | None -> todo env ())
      in
      let v5 = (* ")" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Id tok -> (* identifier *) token env tok
  | `Choice_new x -> map_anon_choice_new_0342769 env x
  | `Comp_lit (v1, v2) ->
      let v1 =
        (match v1 with
        | `Map_type x -> map_map_type env x
        | `Slice_type x -> map_slice_type env x
        | `Array_type x -> map_array_type env x
        | `Impl_len_array_type x ->
            map_implicit_length_array_type env x
        | `Struct_type x -> map_struct_type env x
        | `Id tok -> (* identifier *) token env tok
        | `Gene_type x -> map_generic_type env x
        | `Qual_type x -> map_qualified_type env x
        )
      in
      let v2 = map_literal_value env v2 in
      todo env (v1, v2)
  | `Func_lit (v1, v2, v3, v4) ->
      let v1 = (* "func" *) token env v1 in
      let v2 = map_parameter_list env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_anon_choice_param_list_29faba4 env x
        | None -> todo env ())
      in
      let v4 = map_block env v4 in
      todo env (v1, v2, v3, v4)
  | `Choice_raw_str_lit x -> map_string_literal env x
  | `Int_lit tok -> (* int_literal *) token env tok
  | `Float_lit tok -> (* float_literal *) token env tok
  | `Imag_lit tok -> (* imaginary_literal *) token env tok
  | `Rune_lit tok -> (* rune_literal *) token env tok
  | `Nil tok -> (* "nil" *) token env tok
  | `True tok -> (* "true" *) token env tok
  | `False tok -> (* "false" *) token env tok
  | `Iota tok -> (* "iota" *) token env tok
  | `Paren_exp (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* ")" *) token env v3 in
      todo env (v1, v2, v3)
  )

and map_expression_case (env : env) ((v1, v2, v3, v4) : CST.expression_case) =
  let v1 = (* "case" *) token env v1 in
  let v2 = map_expression_list env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> map_statement_list env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and map_expression_list (env : env) ((v1, v2) : CST.expression_list) =
  let v1 = map_expression env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and map_field_declaration (env : env) ((v1, v2) : CST.field_declaration) =
  let v1 =
    (match v1 with
    | `Id_rep_COMMA_id_choice_simple_type (v1, v2, v3) ->
        let v1 = (* identifier *) token env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = (* identifier *) token env v2 in
            todo env (v1, v2)
          ) v2
        in
        let v3 = map_type_ env v3 in
        todo env (v1, v2, v3)
    | `Opt_STAR_choice_id (v1, v2) ->
        let v1 =
          (match v1 with
          | Some tok -> (* "*" *) token env tok
          | None -> todo env ())
        in
        let v2 = map_interface_type_name env v2 in
        todo env (v1, v2)
    )
  in
  let v2 =
    (match v2 with
    | Some x -> map_string_literal env x
    | None -> todo env ())
  in
  todo env (v1, v2)

and map_field_declaration_list (env : env) ((v1, v2, v3) : CST.field_declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = map_field_declaration env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = map_anon_choice_LF_249c99f env v1 in
            let v2 = map_field_declaration env v2 in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | Some x -> map_anon_choice_LF_249c99f env x
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_for_clause (env : env) ((v1, v2, v3, v4, v5) : CST.for_clause) =
  let v1 =
    (match v1 with
    | Some x -> map_simple_statement env x
    | None -> todo env ())
  in
  let v2 = (* ";" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> map_expression env x
    | None -> todo env ())
  in
  let v4 = (* ";" *) token env v4 in
  let v5 =
    (match v5 with
    | Some x -> map_simple_statement env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5)

and map_generic_type (env : env) ((v1, v2) : CST.generic_type) =
  let v1 = (* identifier *) token env v1 in
  let v2 = map_type_arguments env v2 in
  todo env (v1, v2)

and map_if_statement (env : env) ((v1, v2, v3, v4, v5) : CST.if_statement) =
  let v1 = (* "if" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_simple_statement env v1 in
        let v2 = (* ";" *) token env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = map_expression env v3 in
  let v4 = map_block env v4 in
  let v5 =
    (match v5 with
    | Some (v1, v2) ->
        let v1 = (* "else" *) token env v1 in
        let v2 =
          (match v2 with
          | `Blk x -> map_block env x
          | `If_stmt x -> map_if_statement env x
          )
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5)

and map_implicit_length_array_type (env : env) ((v1, v2, v3, v4) : CST.implicit_length_array_type) =
  let v1 = (* "[" *) token env v1 in
  let v2 = (* "..." *) token env v2 in
  let v3 = (* "]" *) token env v3 in
  let v4 = map_type_ env v4 in
  todo env (v1, v2, v3, v4)

and map_interface_body (env : env) (x : CST.interface_body) =
  (match x with
  | `Meth_spec (v1, v2, v3) ->
      let v1 = (* identifier *) token env v1 in
      let v2 = map_parameter_list env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_anon_choice_param_list_29faba4 env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  | `Inte_type_name x -> map_interface_type_name env x
  | `Cons_elem (v1, v2) ->
      let v1 = map_constraint_term env v1 in
      let v2 =
        List.map (fun (v1, v2) ->
          let v1 = (* "|" *) token env v1 in
          let v2 = map_constraint_term env v2 in
          todo env (v1, v2)
        ) v2
      in
      todo env (v1, v2)
  )

and map_literal_value (env : env) ((v1, v2, v3) : CST.literal_value) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = map_anon_choice_elem_c42cd9b env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_elem_c42cd9b env v2 in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | Some tok -> (* "," *) token env tok
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_map_type (env : env) ((v1, v2, v3, v4, v5) : CST.map_type) =
  let v1 = (* "map" *) token env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 = map_type_ env v3 in
  let v4 = (* "]" *) token env v4 in
  let v5 = map_type_ env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_parameter_declaration (env : env) ((v1, v2) : CST.parameter_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_field_name_list env x
    | None -> todo env ())
  in
  let v2 = map_type_ env v2 in
  todo env (v1, v2)

and map_parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 =
          (match v1 with
          | Some (v1, v2) ->
              let v1 = map_anon_choice_param_decl_18823e5 env v1 in
              let v2 =
                List.map (fun (v1, v2) ->
                  let v1 = (* "," *) token env v1 in
                  let v2 = map_anon_choice_param_decl_18823e5 env v2 in
                  todo env (v1, v2)
                ) v2
              in
              todo env (v1, v2)
          | None -> todo env ())
        in
        let v2 =
          (match v2 with
          | Some tok -> (* "," *) token env tok
          | None -> todo env ())
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_range_clause (env : env) ((v1, v2, v3) : CST.range_clause) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 = map_expression_list env v1 in
        let v2 = map_anon_choice_EQ_4ccabd6 env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v2 = (* "range" *) token env v2 in
  let v3 = map_expression env v3 in
  todo env (v1, v2, v3)

and map_receive_statement (env : env) ((v1, v2) : CST.receive_statement) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 = map_expression_list env v1 in
        let v2 = map_anon_choice_EQ_4ccabd6 env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_send_statement (env : env) ((v1, v2, v3) : CST.send_statement) =
  let v1 = map_expression env v1 in
  let v2 = (* "<-" *) token env v2 in
  let v3 = map_expression env v3 in
  todo env (v1, v2, v3)

and map_simple_statement (env : env) (x : CST.simple_statement) =
  (match x with
  | `Exp x -> map_expression env x
  | `Send_stmt x -> map_send_statement env x
  | `Inc_stmt (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = (* "++" *) token env v2 in
      todo env (v1, v2)
  | `Dec_stmt (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = (* "--" *) token env v2 in
      todo env (v1, v2)
  | `Assign_stmt (v1, v2, v3) ->
      let v1 = map_expression_list env v1 in
      let v2 =
        (match v2 with
        | `STAREQ tok -> (* "*=" *) token env tok
        | `SLASHEQ tok -> (* "/=" *) token env tok
        | `PERCEQ tok -> (* "%=" *) token env tok
        | `LTLTEQ tok -> (* "<<=" *) token env tok
        | `GTGTEQ tok -> (* ">>=" *) token env tok
        | `AMPEQ tok -> (* "&=" *) token env tok
        | `AMPHATEQ tok -> (* "&^=" *) token env tok
        | `PLUSEQ tok -> (* "+=" *) token env tok
        | `DASHEQ tok -> (* "-=" *) token env tok
        | `BAREQ tok -> (* "|=" *) token env tok
        | `HATEQ tok -> (* "^=" *) token env tok
        | `EQ tok -> (* "=" *) token env tok
        )
      in
      let v3 = map_expression_list env v3 in
      todo env (v1, v2, v3)
  | `Short_var_decl (v1, v2, v3) ->
      let v1 = map_expression_list env v1 in
      let v2 = (* ":=" *) token env v2 in
      let v3 = map_expression_list env v3 in
      todo env (v1, v2, v3)
  )

and map_simple_type (env : env) (x : CST.simple_type) =
  (match x with
  | `Id tok -> (* identifier *) token env tok
  | `Gene_type x -> map_generic_type env x
  | `Qual_type x -> map_qualified_type env x
  | `Poin_type (v1, v2) ->
      let v1 = (* "*" *) token env v1 in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
  | `Struct_type x -> map_struct_type env x
  | `Inte_type (v1, v2, v3, v4) ->
      let v1 = (* "interface" *) token env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2, v3) ->
            let v1 = map_interface_body env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = map_anon_choice_LF_249c99f env v1 in
                let v2 = map_interface_body env v2 in
                todo env (v1, v2)
              ) v2
            in
            let v3 =
              (match v3 with
              | Some x -> map_anon_choice_LF_249c99f env x
              | None -> todo env ())
            in
            todo env (v1, v2, v3)
        | None -> todo env ())
      in
      let v4 = (* "}" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Array_type x -> map_array_type env x
  | `Slice_type x -> map_slice_type env x
  | `Map_type x -> map_map_type env x
  | `Chan_type x -> map_channel_type env x
  | `Func_type (v1, v2, v3) ->
      let v1 = (* "func" *) token env v1 in
      let v2 = map_parameter_list env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_anon_choice_param_list_29faba4 env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  )

and map_slice_type (env : env) ((v1, v2, v3) : CST.slice_type) =
  let v1 = (* "[" *) token env v1 in
  let v2 = (* "]" *) token env v2 in
  let v3 = map_type_ env v3 in
  todo env (v1, v2, v3)

and map_special_argument_list (env : env) ((v1, v2, v3, v4, v5) : CST.special_argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v5 = (* ")" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Decl x -> map_declaration env x
  | `Simple_stmt x -> map_simple_statement env x
  | `Ret_stmt (v1, v2) ->
      let v1 = (* "return" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_expression_list env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Go_stmt (v1, v2) ->
      let v1 = (* "go" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Defer_stmt (v1, v2) ->
      let v1 = (* "defer" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `If_stmt x -> map_if_statement env x
  | `For_stmt (v1, v2, v3) ->
      let v1 = (* "for" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x ->
            (match x with
            | `Exp x -> map_expression env x
            | `For_clause x -> map_for_clause env x
            | `Range_clause x -> map_range_clause env x
            )
        | None -> todo env ())
      in
      let v3 = map_block env v3 in
      todo env (v1, v2, v3)
  | `Exp_switch_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "switch" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) ->
            let v1 = map_simple_statement env v1 in
            let v2 = (* ";" *) token env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v4 = (* "{" *) token env v4 in
      let v5 =
        List.map (fun x ->
          (match x with
          | `Exp_case x -> map_expression_case env x
          | `Defa_case x -> map_default_case env x
          )
        ) v5
      in
      let v6 = (* "}" *) token env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Type_switch_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "switch" *) token env v1 in
      let v2 = map_type_switch_header env v2 in
      let v3 = (* "{" *) token env v3 in
      let v4 =
        List.map (fun x ->
          (match x with
          | `Type_case x -> map_type_case env x
          | `Defa_case x -> map_default_case env x
          )
        ) v4
      in
      let v5 = (* "}" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Select_stmt (v1, v2, v3, v4) ->
      let v1 = (* "select" *) token env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 =
        List.map (fun x ->
          (match x with
          | `Comm_case x -> map_communication_case env x
          | `Defa_case x -> map_default_case env x
          )
        ) v3
      in
      let v4 = (* "}" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Labe_stmt (v1, v2, v3) ->
      let v1 = (* identifier *) token env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_statement env v3 in
      todo env (v1, v2, v3)
  | `Fall_stmt tok -> (* "fallthrough" *) token env tok
  | `Brk_stmt (v1, v2) ->
      let v1 = (* "break" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> (* identifier *) token env tok
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Cont_stmt (v1, v2) ->
      let v1 = (* "continue" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> (* identifier *) token env tok
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Goto_stmt (v1, v2) ->
      let v1 = (* "goto" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      todo env (v1, v2)
  | `Blk x -> map_block env x
  | `Empty_stmt tok -> (* ";" *) token env tok
  )

and map_statement_list (env : env) (x : CST.statement_list) =
  (match x with
  | `Stmt_rep_choice_LF_stmt_opt_choice_LF_opt_empty_labe_stmt (v1, v2, v3) ->
      let v1 = map_statement env v1 in
      let v2 =
        List.map (fun (v1, v2) ->
          let v1 = map_anon_choice_LF_249c99f env v1 in
          let v2 = map_statement env v2 in
          todo env (v1, v2)
        ) v2
      in
      let v3 =
        (match v3 with
        | Some (v1, v2) ->
            let v1 = map_anon_choice_LF_249c99f env v1 in
            let v2 =
              (match v2 with
              | Some x -> map_empty_labeled_statement env x
              | None -> todo env ())
            in
            todo env (v1, v2)
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  | `Empty_labe_stmt x -> map_empty_labeled_statement env x
  )

and map_struct_type (env : env) ((v1, v2) : CST.struct_type) =
  let v1 = (* "struct" *) token env v1 in
  let v2 = map_field_declaration_list env v2 in
  todo env (v1, v2)

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Simple_type x -> map_simple_type env x
  | `Paren_type (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_type_ env v2 in
      let v3 = (* ")" *) token env v3 in
      todo env (v1, v2, v3)
  )

and map_type_alias (env : env) ((v1, v2, v3) : CST.type_alias) =
  let v1 = (* identifier *) token env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_type_ env v3 in
  todo env (v1, v2, v3)

and map_type_arguments (env : env) ((v1, v2, v3, v4, v5) : CST.type_arguments) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v5 = (* "]" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_type_case (env : env) ((v1, v2, v3, v4, v5) : CST.type_case) =
  let v1 = (* "case" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = (* ":" *) token env v4 in
  let v5 =
    (match v5 with
    | Some x -> map_statement_list env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5)

and map_type_parameter_list (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameter_list) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_parameter_declaration env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_parameter_declaration env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v5 = (* "]" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_type_spec (env : env) ((v1, v2, v3) : CST.type_spec) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> map_type_parameter_list env x
    | None -> todo env ())
  in
  let v3 = map_type_ env v3 in
  todo env (v1, v2, v3)

and map_type_switch_header (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.type_switch_header) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 = map_simple_statement env v1 in
        let v2 = (* ";" *) token env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_expression_list env v1 in
        let v2 = (* ":=" *) token env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = map_expression env v3 in
  let v4 = (* "." *) token env v4 in
  let v5 = (* "(" *) token env v5 in
  let v6 = (* "type" *) token env v6 in
  let v7 = (* ")" *) token env v7 in
  todo env (v1, v2, v3, v4, v5, v6, v7)

and map_var_spec (env : env) ((v1, v2, v3) : CST.var_spec) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      todo env (v1, v2)
    ) v2
  in
  let v3 =
    (match v3 with
    | `Choice_simple_type_opt_EQ_exp_list (v1, v2) ->
        let v1 = map_type_ env v1 in
        let v2 =
          (match v2 with
          | Some (v1, v2) ->
              let v1 = (* "=" *) token env v1 in
              let v2 = map_expression_list env v2 in
              todo env (v1, v2)
          | None -> todo env ())
        in
        todo env (v1, v2)
    | `EQ_exp_list (v1, v2) ->
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression_list env v2 in
        todo env (v1, v2)
    )
  in
  todo env (v1, v2, v3)

let map_import_spec_list (env : env) ((v1, v2, v3) : CST.import_spec_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = map_import_spec env v1 in
      let v2 = map_anon_choice_LF_249c99f env v2 in
      todo env (v1, v2)
    ) v2
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

let map_top_level_declaration (env : env) (x : CST.top_level_declaration) =
  (match x with
  | `Pack_clause (v1, v2) ->
      let v1 = (* "package" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      todo env (v1, v2)
  | `Func_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "func" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_type_parameter_list env x
        | None -> todo env ())
      in
      let v4 = map_parameter_list env v4 in
      let v5 =
        (match v5 with
        | Some x -> map_anon_choice_param_list_29faba4 env x
        | None -> todo env ())
      in
      let v6 =
        (match v6 with
        | Some x -> map_block env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Meth_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "func" *) token env v1 in
      let v2 = map_parameter_list env v2 in
      let v3 = (* identifier *) token env v3 in
      let v4 = map_parameter_list env v4 in
      let v5 =
        (match v5 with
        | Some x -> map_anon_choice_param_list_29faba4 env x
        | None -> todo env ())
      in
      let v6 =
        (match v6 with
        | Some x -> map_block env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Import_decl (v1, v2) ->
      let v1 = (* "import" *) token env v1 in
      let v2 =
        (match v2 with
        | `Import_spec x -> map_import_spec env x
        | `Import_spec_list x -> map_import_spec_list env x
        )
      in
      todo env (v1, v2)
  )

let map_source_file (env : env) (xs : CST.source_file) =
  List.map (fun x ->
    (match x with
    | `Stmt_choice_LF (v1, v2) ->
        let v1 = map_statement env v1 in
        let v2 = map_anon_choice_LF_249c99f env v2 in
        todo env (v1, v2)
    | `Choice_pack_clause_opt_choice_LF (v1, v2) ->
        let v1 = map_top_level_declaration env v1 in
        let v2 =
          (match v2 with
          | Some x -> map_anon_choice_LF_249c99f env x
          | None -> todo env ())
        in
        todo env (v1, v2)
    )
  ) xs
