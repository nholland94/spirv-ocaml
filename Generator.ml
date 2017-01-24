open Camlp4.PreCast
open Camlp4.PreCast.Ast
open Yojson.Basic.Util

let _loc = Loc.ghost

(* Types *)

type spirv_instruction_operand_quantifier = Optional | Variadic
type spirv_instruction_operand = string * string option * spirv_instruction_operand_quantifier option
type spirv_instruction =
  { op_name: string;
    op_code: int;
    operands: spirv_instruction_operand list;
    capabilities: string list}

type spirv_enumerant =
  { name: string;
    value: string;
    capabilities: string list;
    parameters: (string * string option) list }

type spirv_kind =
  | Enum of string * spirv_enumerant list
  | Type of string * string list

type spirv_info =
  { instructions: spirv_instruction list;
    kinds: spirv_kind list }

(* Type Helpers *)

let operand_is_optional (_, _, q) = (q = (Some Optional))

let cast_instruction_quantifier = function
  | "?" -> Optional
  | "*" -> Variadic
  | _   -> failwith "unexpected instruction quantifier"

let rec filter_kinds = function
  | Enum (n, e) :: t ->
      let (enums, types) = filter_kinds t in
      ((n, e) :: enums, types)
  | Type (n, k) :: t ->
      let (enums, types) = filter_kinds t in
      (enums, (n, k) :: types)
  | [] -> ([], [])

(* Generic Helpers *)

let (<=<) f g x = f (g x)

let rec last_el = function
  | [el]   -> el
  | _ :: t -> last_el t
  | []     -> failwith "cannot get last el of empty list"

let count_tail fn ls =
  let rec loop = function
    | h :: t ->
        let (cond, count) = loop t in
        if cond && fn h then (true, count + 1) else (false, count)
    | []     -> (true, 0)
  in
  let (_, count) = loop ls in
  count

let rm_tail amount ls =
  let rec loop = function
    | h :: t ->
        let (count, ls) = loop t in
        if count < amount then (count + 1, ls) else (count, h :: ls)
    | []     -> (0, [])
  in
  let (_, result) = loop ls in
  result

let slice_tail amount ls =
  let rec loop = function
    | h :: t ->
        let (count, ls) = loop t in
        if count < amount then (count + 1, h :: ls) else (count, ls)
    | []     -> (0, [])
  in
  let (_, result) = loop ls in
  result

let reduce fn = function
  | h1 :: h2 :: t -> List.fold_left fn h1 (h2 :: t)
  | [scalar]      -> failwith "cannot reduce a list of length 1"
  | []            -> failwith "cannot reduce an empty list"

let snake_case_of_camel_case str =
  let str_len = String.length str in

  let is_uppercase c =
    let v = Char.code c in
    65 <= v && v <= 90
  in

  let string_of_char = String.make 1 in

  let rec loop i =
    if i = str_len then "" else
      let c = String.get str i in
      let replacement =
        if is_uppercase c then
          (if i = 0 then "" else "_") ^ (string_of_char @@ Char.lowercase c)
        else string_of_char c
      in
      replacement ^ loop (i + 1)
  in

  loop 0

let generate_names n =
  let iota n =
    let rec loop i = if i < n then i :: (loop (i + 1)) else [] in
    loop 0
  in
  let string_of_char = String.make 1 in
  let lower_alpha_of_int n = string_of_char @@ Char.chr (n + 97) in
  List.map lower_alpha_of_int (iota n)

(* Ast Templates *)

module Templates = struct
  (* Simple Wrappers *)
  let id_lid n = IdLid (_loc, n)
  let id_uid n = IdUid (_loc, n)

  let ty_id id = TyId (_loc, id)
  let pa_id id = PaId (_loc, id)
  let ex_id id = ExId (_loc, id)

  let ty_id_lid = ty_id <=< id_lid
  let ty_id_uid = ty_id <=< id_uid
  let pa_id_lid = pa_id <=< id_lid
  let pa_id_uid = pa_id <=< id_uid
  let ex_id_lid = ex_id <=< id_lid
  let ex_id_uid = ex_id <=< id_uid

  let ty_or a b = TyOr (_loc, a, b)
  let ty_and a b = TyAnd (_loc, a, b)
  let ty_sta a b = TySta (_loc, a, b)
  let ty_app a b = TyApp (_loc, a, b)
  let mc_or a b = McOr (_loc, a, b)
  let pa_app a b = PaApp (_loc, a, b)
  let ex_int n = ExInt (_loc, n)

  let typedef name body = StTyp (_loc, TyDcl (_loc, name, [], body, []))

  (* Ast Concatenation *)
  let rec join_ast join = function
    | [i]    -> i
    | i :: t -> join i (join_ast join t)
    | []     -> failwith "cannot call join_ast on empty list"
  let join_types = join_ast ty_or
  let apply_types = join_ast ty_app
  let cons_tuple = join_ast ty_and
  let cons_sta = join_ast ty_sta
  let join_match_cases = join_ast mc_or

  let rec apply_pattern base = function
    | h :: t ->
        apply_pattern (PaApp (_loc, base, h)) t
    | []     -> base

  (* Variants *)
  type ocaml_type_variant = string * ctyp option

  let cons_variant cons = function
    | (name, None)         -> cons name
    | (name, Some of_type) -> TyOf (_loc, cons name, of_type)

  let variant = cons_variant ty_id_uid
  let polymorphic_variant = cons_variant (fun n -> TyVrn (_loc, n))

  let variants = join_types <=< List.map variant

  let polymorphic_variants vars =
    let types = List.map polymorphic_variant vars in
    TyVrnEq (_loc, join_types types)

  (* Matching *)
  (* currently only capable of representing variant match cases *)
  type ocaml_pattern = string * string list
  type ocaml_match_case = ocaml_pattern * expr

  let match_case (pattern, exp) =
    let pattern_ast = match pattern with
      | (variant, []) -> PaVrn (_loc, variant)
      | (variant, ls) ->
          apply_pattern (PaVrn (_loc, variant)) (List.map pa_id_lid ls)
    in
    McArr (_loc, pattern_ast, ExNil _loc, exp)

  let match_cases = function
    | []       -> failwith "cannot call match_patterns on empty list"
    | [scalar] -> match_case scalar
    | vec      -> join_match_cases (List.map match_case vec)

  (* Expressions *)
  let fn_call name args =
    let rec apply_args prev = function
      | []     -> failwith "cannot call fn_call on empty args list"
      | [e]    -> ExApp (_loc, prev, e)
      | e :: t -> apply_args (ExApp (_loc, prev, e)) t
    in
    apply_args (ex_id_uid name) args

  let monadic_fn_call name arg = fn_call name [arg]

  let rec ls_exp = function
    | h :: t ->
        ExApp (_loc,
          ExApp (_loc,
            ExId (_loc, IdUid (_loc, "::")),
            h),
          ls_exp t)
    | []     ->
        ExId (_loc, IdUid (_loc, "[]"))

  (* Scalable List Constructors *)

  (* constructs a scalar or type application from a list of strings *)
  let type_app_of_ls = function
    | []       -> failwith "cannot call type_app_of_ls on empty list"
    | [scalar] -> ty_id_lid scalar
    | vec      -> apply_types (List.map ty_id_lid vec)

  (* constructs a scalar or tuple type from a list of type asts *)
  let type_tuple_of_ls = function
    | []       -> failwith "cannot call type_tuple_of_ls on empty list"
    | [scalar] -> scalar
    | vec      -> cons_tuple vec

  (* constructs a scalar or static tuple type from a list of type asts *)
  let type_sta_tuple_of_ls = function
    | []       -> failwith "cannot call type_sta_tuple_of_ls on empty list"
    | [scalar] -> scalar
    | vec      -> TyTup (_loc, cons_sta vec)
end

(* JSON Parsing *)

let destruct_operand json =
  let json_keys = keys json in
  let kind = snake_case_of_camel_case @@ to_string @@ member "kind" json in
  let name =
    if List.mem "name" json_keys then
      Some (to_string @@ member "name" json)
    else None
  in
  let quantifier =
    if List.mem "quantifier" json_keys then
      Some (cast_instruction_quantifier @@ to_string @@ member "quantifier" json)
    else None
  in
  (kind, name, quantifier)

let destruct_instruction json =
  let json_keys = keys json in
  let operands =
    if List.mem "operands" json_keys then
      List.map destruct_operand @@ to_list @@ member "operands" json
    else []
  in
  let capabilities =
    if List.mem "capabilities" json_keys then
      List.map to_string @@ to_list @@ member "capabilities" json
    else []
  in
  { op_name = to_string @@ member "opname" json;
    op_code = to_int @@ member "opcode" json;
    operands = operands;
    capabilities = capabilities }

let destruct_parameter json =
  let kind = snake_case_of_camel_case @@ to_string @@ member "kind" json in
  let name =
    if List.mem "name" @@ keys json then
      Some (to_string @@ member "name" json)
    else None
  in
  (kind, name)

let destruct_enumerant parent_name value_fn json =
  let json_keys = keys json in
  let raw_name = to_string @@ member "enumerant" json in
  let name =
    if Str.string_match (Str.regexp "^[0-9]") raw_name 0 then
      parent_name ^ raw_name
    else if Str.string_match (Str.regexp "^[a-z]") raw_name 0 then
      String.capitalize raw_name
    else raw_name
  in
  let capabilities =
    if List.mem "capabilities" json_keys then
      List.map to_string @@ to_list @@ member "capabilities" json
    else []
  in
  let parameters =
    if List.mem "parameters" json_keys then
      List.map destruct_parameter @@ to_list @@ member "parameters" json
    else []
  in
  { name = name;
    value = value_fn @@ member "value" json;
    capabilities = capabilities;
    parameters = parameters }

(* TODO fix some of this *)
let type_string_of_literal_kind = function
  | "LiteralInteger" -> ["int"]
  | "LiteralString" -> ["string"]
  | "LiteralContextDependentNumber" -> ["int"]
  | "LiteralExtInstInteger" -> ["int"]
  | "LiteralSpecConstantOpInteger" -> ["int"]
  | _ -> failwith "unhandled literal kind"

let destruct_kind json =
  let kind_name = to_string @@ member "kind" json in
  let formatted_kind_name = snake_case_of_camel_case kind_name in
  match to_string @@ member "category" json with
    | "BitEnum"   -> Enum (formatted_kind_name, List.map (destruct_enumerant kind_name to_string) @@ to_list @@ member "enumerants" json)
    | "ValueEnum" -> Enum (formatted_kind_name, List.map (destruct_enumerant kind_name (string_of_int <=< to_int)) @@ to_list @@ member "enumerants" json)
    | "Id"        -> Type (formatted_kind_name, ["id"])
    | "Literal"   -> Type (formatted_kind_name, type_string_of_literal_kind kind_name)
    | "Composite" -> Type (formatted_kind_name, List.map snake_case_of_camel_case @@ List.map to_string @@ to_list @@ member "bases" json) (* TODO *)
    | _           -> failwith "shouldn't happen"

let parse_spirv_info spirv_json =
  { instructions = List.map destruct_instruction @@ to_list @@ member "instructions" spirv_json;
    kinds = List.map destruct_kind @@ to_list @@ member "operand_kinds" spirv_json }

(* Code Generation *)

let instruction_words_exp instruction operand_names =
  let code = Templates.ex_int @@ string_of_int instruction.op_code in
    let concat_ls_exps a b = Templates.fn_call "@" [a; b] in
    let op_refs = List.map Templates.ex_id_lid operand_names in
    let final_quantifier =
      if List.length ops = 0 then
        None
      else
        let (_, _, q) = last_el ops in q
    in
    match final_quantifier with
      | Some Optional ->
          let opt_operand_count = count_tail operand_is_optional ops in
          let static_op_refs = rm_tail opt_operand_count op_refs in
          let opt_op_refs = slice_tail opt_operand_count op_refs in

          let static_ls_exp = Templates.ls_exp (code :: static_op_refs) in
          let opt_ls_exps = List.map (Templates.monadic_fn_call "list_of_list_opt") opt_op_refs in
          reduce concat_exps ([static_ls_exp] @ opt_ls_exps)
      | Some Variadic ->
          let static_op_refs = rm_tail 1 op_refs in
          let var_op_ref = last_el op_refs in
          let static_ls_exp = Templates.ls_exp (code :: static_op_refs) in
          concat_exps static_ls_exp var_op_ref
      | None          ->
          Templates.ls_exp (code :: op_refs)

let build_words_of_op_fn instructions =
  let operand_words_exp code operand_names ops =
    let concat_exps a b = Templates.fn_call "@" [a; b] in
    let op_refs = List.map Templates.ex_id_lid operand_names in
    let final_quantifier =
      if List.length ops = 0 then
        None
      else
        let (_, _, q) = last_el ops in q
    in
    match final_quantifier with
      | Some Optional ->
          let opt_operand_count = count_tail operand_is_optional ops in
          let static_op_refs = rm_tail opt_operand_count op_refs in
          let opt_op_refs = slice_tail opt_operand_count op_refs in

          let static_ls_exp = Templates.ls_exp (code :: static_op_refs) in
          let opt_ls_exps = List.map (Templates.monadic_fn_call "list_of_list_opt") opt_op_refs in
          reduce concat_exps ([static_ls_exp] @ opt_ls_exps)
      | Some Variadic ->
          let static_op_refs = rm_tail 1 op_refs in
          let var_op_ref = last_el op_refs in
          let static_ls_exp = Templates.ls_exp (code :: static_op_refs) in
          concat_exps static_ls_exp var_op_ref
      | None          ->
          Templates.ls_exp (code :: op_refs)
  in

  let match_case_of_instruction i =
    let operand_names = generate_names (List.length i.operands) in
    let code = string_of_int i.op_code in
    let exp = operand_words_exp (Templates.ex_int code) operand_names i.operands in
    ((i.op_name, operand_names), exp)
  in

  let patterns = Templates.match_cases @@ List.map match_case_of_instruction instructions in

  <:str_item<
    let words_of_op = fun op ->
      let list_of_list_opt = fun ls_opt ->
        match ls_opt with
          | Some ls -> ls
          | None    -> []
      in
      match op with $patterns$
  >>

let build_kind_typedef (name, t) =
  let tuple = Templates.type_sta_tuple_of_ls @@ List.map Templates.ty_id_lid t in
  Templates.typedef name tuple

let build_enum_typedef (name, enumerants) =
  let convert_variant enum = (enum.name, None) in
  let variants = Templates.variants @@ List.map convert_variant enumerants in
  Templates.typedef name variants

let build_op_typedef instructions =
  let operands_ast operands =
    let list_of_operand = function
      | (kind, _, Some Optional) -> ["option"; kind]
      | (kind, _, Some Variadic) -> ["list"; kind]
      | (kind, _, None)          -> [kind]
    in

    let operand_lists = List.map list_of_operand operands in
    let operand_asts = List.map Templates.type_app_of_ls operand_lists in
    Templates.type_tuple_of_ls operand_asts
  in

  let convert_variant = function
    | { operands = []; op_name = name } -> (name, None)
    | { operands = ls; op_name = name } -> (name, Some (operands_ast ls))
  in

  let variants = Templates.polymorphic_variants @@ List.map convert_variant instructions in
  Templates.typedef "op" variants

let generate_code info =
  let output = Printers.OCaml.print_implem in
  let (enums, types) = filter_kinds info.kinds in

  output @@ Templates.typedef "id" (Templates.ty_id_lid "int");
  List.iter (output <=< build_kind_typedef) types;
  List.iter (output <=< build_enum_typedef) enums;
  output @@ build_op_typedef info.instructions;
  output @@ build_words_of_op_fn info.instructions

let () =
  let in_ch = open_in "spirv.core.grammar.json" in
  let json = Yojson.Basic.from_channel in_ch in
  generate_code @@ parse_spirv_info json;
  close_in in_ch
