open Camlp4.PreCast
open Camlp4.PreCast.Ast

let _loc = Loc.ghost

module StringMap = Map.Make(String)

let (<=<) f g x = f (g x)

type spv_type =
  | Id
  | LiteralInteger
  | LiteralContextDependentNumber
  | LiteralExtInstInteger
  | LiteralSpecConstantOpInteger
  | LiteralString
  | Composite of spv_type list

type spv_enumerant =
  { enumerant_name: string;
    enumerant_value: string;
    enumerant_capabilities: string list;
    enumerant_parameters: (string * string option) list }

type spv_kind = 
  | Type of string * string * spv_type
  | Enum of string * string * spv_enumerant list

type spv_operand_quantifier = Optional | Variadic

type spv_operand =
  { operand_kind: spv_kind;
    operand_name: string option;
    quantifier: spv_operand_quantifier option }

type spv_instruction =
  { instruction_name: string;
    instruction_code: int;
    instruction_operands: spv_operand list;
    instruction_capabilities: string list }

module Util = struct
  let mutate_head fn = function
    | h :: t -> (fn h) :: t
    | []     -> failwith "cannot mutate head of empty list"

  let reduce fn = function
    | h1 :: h2 :: t -> List.fold_left fn h1 (h2 :: t)
    | [scalar]      -> failwith "cannot reduce a list of length 1"
    | []            -> failwith "cannot reduce an empty list"

  let generate_names n =
    let iota n =
      let rec loop i = if i < n then i :: (loop (i + 1)) else [] in
      loop 0
    in
    let string_of_char = String.make 1 in
    let lower_alpha_of_int n = string_of_char @@ Char.chr (n + 97) in
    List.map lower_alpha_of_int (iota n)

  let ocaml_name_of_kind = function
    | Enum (_, n, _) -> n
    | Type (_, n, _) -> n

  let rec filter_kinds = function
    | Enum (_, n, e) :: t ->
        let (enums, types) = filter_kinds t in
        ((n, e) :: enums, types)
    | Type (_, n, k) :: t ->
        let (enums, types) = filter_kinds t in
        (enums, (n, k) :: types)
    | [] -> ([], [])

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
end

module Parsing = struct
  open Yojson.Basic.Util

  let cast_spv_kind = function
    | "Id" | "IdRef"                  -> Id
    | "LiteralInteger"                -> LiteralInteger
    | "LiteralContextDependentNumber" -> LiteralContextDependentNumber
    | "LiteralExtInstInteger"         -> LiteralExtInstInteger
    | "LiteralString"                 -> LiteralString
    | "LiteralSpecConstantOpInteger"  -> LiteralSpecConstantOpInteger
    | _ as kind                       -> failwith ("unhandled spv kind: " ^ kind)

  let opt_member fn key obj =
    if List.mem key (keys obj) then
      Some (fn @@ member key obj)
    else
      None

  let opt_ls_member key obj =
    if List.mem key (keys obj) then
      to_list @@ member key obj
    else
      []

  (* TODO: add escapes *)
  let format_and_escape_kind_name name = Util.snake_case_of_camel_case name
  let fix_invalid_enumerant_name prefix name =
    if Str.string_match (Str.regexp "^[0-9]") name 0 then
      prefix ^ name
    else if Str.string_match (Str.regexp "^[a-z]") name 0 then
      String.capitalize name
    else
      name

  let rec lookup_kind_by_raw_name raw_name = function
    | (Enum (name, _, _) as h) :: t
    | (Type (name, _, _) as h) :: t ->
        if name = raw_name then
          h
        else
          lookup_kind_by_raw_name raw_name t
    | [] ->
        failwith "kind not found"

  let cast_operand_quantifier = function
    | "?" -> Optional
    | "*" -> Variadic
    | _   -> failwith "unhandled instruction quantifier"

  let destruct_operand kinds obj =
    let raw_kind_name = to_string @@ member "kind" obj in
    let kind = lookup_kind_by_raw_name raw_kind_name kinds in
    let name = opt_member to_string "name" obj in
    let quantifier = opt_member (cast_operand_quantifier <=< to_string) "quantifier" obj in
    { operand_kind = kind;
      operand_name = name;
      quantifier = quantifier }

  let destruct_instruction kinds obj =
    let name = to_string @@ member "opname" obj in
    let code = to_int @@ member "opcode" obj in
    let operands = List.map (destruct_operand kinds) @@ opt_ls_member "operands" obj in
    let capabilities = List.map to_string @@ opt_ls_member "capabilities" obj in
    { instruction_name = name;
      instruction_code = code;
      instruction_operands = operands;
      instruction_capabilities = capabilities }

  let destruct_enumerant_parameter obj =
    let raw_kind_name = to_string @@ member "kind" obj in
    let kind_name = format_and_escape_kind_name raw_kind_name in
    let parameter_name = opt_member to_string "name" obj in
    (kind_name, parameter_name)

  let destruct_enumerant enum_type_name value_fn obj =
    let raw_name = to_string @@ member "enumerant" obj in
    let name = fix_invalid_enumerant_name enum_type_name raw_name in
    let value = value_fn @@ member "value" obj in
    let capabilities = List.map to_string @@ opt_ls_member "capabilities" obj in
    let parameters = List.map destruct_enumerant_parameter @@ opt_ls_member "parameters" obj in
    { enumerant_name = name;
      enumerant_value = value;
      enumerant_capabilities = capabilities;
      enumerant_parameters = parameters }

  let destruct_kind obj =
    let raw_kind_name = to_string @@ member "kind" obj in
    let kind_name = format_and_escape_kind_name raw_kind_name in

    let process_enumerants value_conv_fn =
      let raw_enumerants = to_list @@ member "enumerants" obj in
      let destruct = destruct_enumerant raw_kind_name value_conv_fn in
      List.map destruct raw_enumerants
    in

    let process_bases () =
      let kind_names = List.map to_string @@ to_list @@ member "bases" obj in
      List.map cast_spv_kind kind_names
    in

    match to_string @@ member "category" obj with
      | "BitEnum"   -> Enum (raw_kind_name, kind_name, process_enumerants to_string)
      | "ValueEnum" -> Enum (raw_kind_name, kind_name, process_enumerants (string_of_int <=< to_int))
      | "Id"        -> Type (raw_kind_name, kind_name, Id)
      | "Literal"   -> Type (raw_kind_name, kind_name, cast_spv_kind raw_kind_name)
      | "Composite" -> Type (raw_kind_name, kind_name, Composite (process_bases ()))
      | _           -> failwith ("unhandled kind category: " ^ raw_kind_name)

  let parse json =
    let kind_objs = to_list @@ member "operand_kinds" json in
    let instruction_objs = to_list @@ member "instructions" json in
    let spv_kinds = List.map destruct_kind kind_objs in
    let spv_instructions = List.map (destruct_instruction spv_kinds) instruction_objs in
    (spv_kinds, spv_instructions)
end



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
  type ocaml_variant_type = Variant | LabelVariant
  type ocaml_pattern = string * string list
  type ocaml_match_case = ocaml_pattern * expr

  let match_case variant_type (pattern, exp) =
    let conv_variant = match variant_type with
      | Variant      -> pa_id_uid
      | LabelVariant -> fun v -> PaVrn (_loc, v)
    in
    let pattern_ast = match pattern with
      | (variant, []) -> conv_variant variant
      | (variant, ls) -> apply_pattern (conv_variant variant) (List.map pa_id_lid ls)
    in
    McArr (_loc, pattern_ast, ExNil _loc, exp)

  let match_cases variant_type = function
    | []       -> failwith "cannot call match_patterns on empty list"
    | [scalar] -> match_case variant_type scalar
    | vec      -> join_match_cases (List.map (match_case variant_type) vec)

  let pattern_match exp variant_type cases =
    let patterns = match_cases variant_type cases in
    <:expr< match $exp$ with $patterns$ >>

  (* Expressions *)
  let fn_call name args =
    let rec apply_args prev = function
      | []     -> failwith "cannot call fn_call on empty args list"
      | [e]    -> ExApp (_loc, prev, e)
      | e :: t -> apply_args (ExApp (_loc, prev, e)) t
    in
    apply_args (ex_id_uid name) args

  let monadic_fn_call name arg = fn_call name [arg]

  let cons head tail = <:expr< $head$ :: $tail$ >>
  let concat_ls_exps a b = <:expr< $a$ @ $b$ >>

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

let rec ocaml_types_of_spv_type = function
  | Id                            -> ["id"]
  | LiteralInteger                -> ["int32"]
  | LiteralContextDependentNumber -> ["big_int"]
  | LiteralExtInstInteger
  | LiteralSpecConstantOpInteger  -> ["int"]
  | LiteralString                 -> ["string"]
  | Composite ls                  -> List.concat @@ List.map ocaml_types_of_spv_type ls

let build_kind_typedef (name, t) =
  let type_name = ocaml_types_of_spv_type t in
  let tuple = Templates.type_sta_tuple_of_ls @@ List.map Templates.ty_id_lid type_name in
  Templates.typedef name tuple

let build_enum_typedef (name, enumerants) =
  let convert_variant enum = (enum.enumerant_name, None) in
  let variants = Templates.variants @@ List.map convert_variant enumerants in
  Templates.typedef name variants

let build_op_typedef instructions =
  let operands_ast operands =
    let list_of_operand = function
      | { operand_kind = kind; quantifier = Some Optional; _ } -> ["option"; Util.ocaml_name_of_kind kind]
      | { operand_kind = kind; quantifier = Some Variadic; _ } -> ["list"; Util.ocaml_name_of_kind kind]
      | { operand_kind = kind; quantifier = None; _ }          -> [Util.ocaml_name_of_kind kind]
    in

    let operand_lists = List.map list_of_operand operands in
    let operand_asts = List.map Templates.type_app_of_ls operand_lists in
    Templates.type_tuple_of_ls operand_asts
  in

  let convert_variant = function
    | { instruction_operands = []; instruction_name = name } -> (name, None)
    | { instruction_operands = ls; instruction_name = name } -> (name, Some (operands_ast ls))
  in

  let variants = Templates.polymorphic_variants @@ List.map convert_variant instructions in
  Templates.typedef "op" variants

type list_or_sca = List | Scalar
type words_exp = list_or_sca * expr

(* TODO: add missing cases *)
let exp_type_of_operand_kind = function
  | Type (_, _, LiteralContextDependentNumber)
  | Type (_, _, LiteralExtInstInteger)
  | Type (_, _, LiteralSpecConstantOpInteger)
  | Type (_, _, LiteralString)
  | Type (_, _, Composite _)                   -> List
  | _                                          -> Scalar

(* This function currently makes some assumptions about how the one-off kinds are used:
 *   assumption 1: operands are named 'a' - 'z' (currently true in the compiler)
 *   assumption 2: LiteralContextDependentNumber is only in instructions which have a result type as the first operand
 *)
let conversion_fn_of_operand_kind = function
  | Type (_, _, Id)                            -> <:expr< word_of_id >>
  | Type (_, _, LiteralInteger)                -> <:expr< word_of_int >>
  | Type (_, _, LiteralContextDependentNumber) -> <:expr< words_of_sized_int (lookup_size a) >>
  | Type (_, _, LiteralExtInstInteger)         -> <:expr< todo >>
  | Type (_, _, LiteralSpecConstantOpInteger)  -> <:expr< todo >>
  | Type (_, _, LiteralString)                 -> <:expr< words_of_string >>
  | Type (_, name, Composite _)                -> Templates.ex_id_lid @@ "words_of_" ^ name
  | Enum (_, name, _)                          -> Templates.ex_id_lid @@ "value_of_" ^ name

let words_exp_of_operand (op, binding) =
  let kind = op.operand_kind in
  let conversion_fn = conversion_fn_of_operand_kind kind in
  match op.quantifier with
    | Some Optional ->
        (List, <:expr< list_of_option (apply_option $conversion_fn$ $binding$) >>)
    | Some Variadic ->
        (List, <:expr< List.map $conversion_fn$ $binding$ >>)
    | None          ->
        (exp_type_of_operand_kind kind, <:expr< $conversion_fn$ $binding$ >>)

let concat_words_exps words_exps =
  let empty_ls_exp = Templates.ex_id_uid "[]" in
  let rec join_words_exps = function
    | (Scalar, e) :: t ->
        let (was_scalar, ls) = join_words_exps t in
        let new_ls = if was_scalar then
          Util.mutate_head (Templates.cons e) ls
        else
          (Templates.cons e empty_ls_exp) :: ls
        in
        (true, new_ls)
    | (List, e) :: t   ->
        let (_, ls) = join_words_exps t in
        (false, e :: ls)
    | []               ->
        (false, [])
  in
  let (_, ls) = join_words_exps words_exps in
  match ls with
    | [e] -> e
    | _   -> Util.reduce Templates.concat_ls_exps ls

let build_words_exp_of_instruction instruction operand_names =
  let code_exp = Templates.ex_int @@ (Printf.sprintf "0x%08xl" instruction.instruction_code) in
  let operand_bindings = List.map Templates.ex_id_lid operand_names in
  let operand_words_exps = List.map words_exp_of_operand @@ List.combine instruction.instruction_operands operand_bindings in
  let words_exps = (Scalar, code_exp) :: operand_words_exps in
  concat_words_exps words_exps

let build_words_of_op_fn instructions =
  let match_case_of_instruction instruction =
    let operand_names = Util.generate_names (List.length instruction.instruction_operands) in
    let exp = build_words_exp_of_instruction instruction operand_names in
    ((instruction.instruction_name, operand_names), exp)
  in

  let patterns = Templates.match_cases Templates.LabelVariant @@ List.map match_case_of_instruction instructions in

  <:str_item<
    let words_of_op = fun (size_map : int IdMap.t) (op : op) ->
      let list_of_option = fun (opt : 'a option) ->
        match opt with
          | Some v  -> [v]
          | None    -> []
      in
      let apply_option = fun (fn : 'a -> 'b) (opt : 'a option) ->
        match opt with
          | Some v -> Some (fn v)
          | None   -> None
      in
      let lookup_size = fun (id : id) ->
        if IdMap.mem id size_map then
          IdMap.find id size_map
        else
          raise (Id_not_found id)
      in
      match op with $patterns$
  >>

let build_enum_value_fns (name, enumerants) =
  let match_case_of_enumerant { enumerant_name = name; enumerant_value = value } =
    ((name, []), Templates.ex_int (value ^ "l"))
  in

  let fn_name = "value_of_" ^ name in
  let name_id = Templates.ty_id_lid name in
  let fn_name_id = Templates.pa_id_lid fn_name in
  let patterns = Templates.match_cases Templates.Variant @@ List.map match_case_of_enumerant enumerants in

  <:str_item<
    let $fn_name_id$ = fun (v : $name_id$) ->
      match v with $patterns$
   >>

module StaticElements = struct
  let open_definitions = [
    <:str_item< open Big_int >>
  ]

  let module_definitions = [
    <:str_item< module IdMap = Map.Make(Int32) >>
  ]

  let exception_definitions = [
    <:str_item< exception Id_not_found of Int32.t >>
  ]

  let type_definitions = [
    <:str_item< type id = int32 >>;
    <:str_item< type word = int32 >>
  ]

  (* TODO: generate these *)
  let lazy_definitions = [
    <:str_item<
      let words_of_pair_literal_integer_id_ref = fun (n, i) -> [word_of_int n; word_of_id i]
    >>;
    <:str_item<
      let words_of_pair_id_ref__literal_integer = fun (i, n) -> [word_of_id i; word_of_int n]
    >>;
    <:str_item<
      let words_of_pair_id_ref_id_ref = fun (a, b) -> [word_of_id a; word_of_id b]
    >>
  ]

  let conversion_functions = [
    (* TODO: remove unecessary conversion functions *)
    <:str_item<
      let word_of_int = fun (i : int32) -> i
    >>;
    <:str_item<
      let word_of_id = fun (id : id) -> 
        if id < 0l then
          failwith "spirv ids must be positive"
        else
          id
    >>;
    <:str_item<
      let words_of_string = fun (str : string) ->
        let len = String.length str in
        let word_count = len / 4 in
        let buffer = Array.make word_count 0l in
        let add_char_to_word ch offset word =
          Int32.logor word (Int32.shift_left (Int32.of_int @@ Char.code ch) (offset * 4))
        in
        let rec add_char_to_buffer i =
          if i = len then
            ()
          else begin
            buffer.(i / 4) <- add_char_to_word (String.get str i) (i mod 4) buffer.(i / 4);
            add_char_to_buffer (i + 1)
          end
        in
        add_char_to_buffer 0;
        Array.to_list buffer
    >>
  ]
end

let generate_code (spv_kinds, spv_instructions) =
  let output = Printers.OCaml.print_implem in
  (* let type_map = collect_type_map spv_kinds in *)
  let (enums, types) = Util.filter_kinds spv_kinds in

  List.iter output StaticElements.open_definitions;
  List.iter output StaticElements.module_definitions;
  List.iter output StaticElements.exception_definitions;

  (* typedefs *)
  List.iter output StaticElements.type_definitions;
  List.iter (output <=< build_kind_typedef) types;
  List.iter (output <=< build_enum_typedef) enums;
  output @@ build_op_typedef spv_instructions;

  (* functions *)
  List.iter output @@ List.map build_enum_value_fns enums;
  List.iter output StaticElements.conversion_functions;
  List.iter output StaticElements.lazy_definitions;
  output @@ build_words_of_op_fn spv_instructions

let () =
  let in_ch = open_in "spirv.core.grammar.json" in
  let json = Yojson.Basic.from_channel in_ch in
  generate_code @@ Parsing.parse json;
  close_in in_ch
