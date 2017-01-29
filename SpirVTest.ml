open OUnit2
open Batteries
open SpirV

let id n = "%" ^ (string_of_int @@ Int32.to_int n)

let cons_big_int ls =
  let rec loop i = function
    | h :: t ->
        let shifted_value = Big_int.shift_left_big_int (Big_int.of_int h) (i * 32) in
        Big_int.and_big_int shifted_value (loop (i - 1) t)
    | []     -> Big_int.of_int 0
  in
  loop (List.length ls - 1) ls

let binary_comparison_set_creators : (string * (unit -> op list * string)) list = [
  (* TODO test unsigned constants *)
  ("signed integer values", fun () ->
    let func = 1l in
    let t_void = 2l in
    let t_func = 3l in
    let t_int_32 = 4l in
    let t_int_40 = 5l in
    let t_int_64 = 6l in
    let c_int_32_1 = 9l in
    let c_int_40_1 = 10l in
    let c_int_40_2 = 11l in
    let c_int_64_1 = 12l in

    [
      `OpCapability CapabilityShader;
      `OpMemoryModel (AddressingModelLogical, MemoryModelSimple);
      `OpEntryPoint (ExecutionModelGLCompute, func, "f", []);
      `OpExecutionMode (func, ExecutionModeLocalSize (1l, 1l, 1l));

      `OpTypeVoid t_void;
      `OpTypeFunction (t_func, t_void, []);

      `OpTypeInt (t_int_32, 32l, 1l);
      `OpTypeInt (t_int_40, 40l, 1l);
      `OpTypeInt (t_int_64, 64l, 1l);

      `OpConstant (t_int_32, c_int_32_1, BigInt (Big_int.big_int_of_int 200));
      `OpConstant (t_int_40, c_int_40_1, BigInt (Big_int.big_int_of_int 400));
      `OpConstant (t_int_40, c_int_40_2, BigInt (cons_big_int [0x0000000f; 0xffff00ff]));
      `OpConstant (t_int_64, c_int_64_1, BigInt (cons_big_int [0x00ff00ff; 0xffff007f]))
    ], "
                      OpCapability Shader
                      OpMemoryModel Logical Simple
                      OpEntryPoint GLCompute "^id func^" \"f\"
                      OpExecutionMode "^id func^" LocalSize 1 1 1

"^id t_void^"       = OpTypeVoid
"^id t_func^"       = OpTypeFunction "^id t_void^"

"^id t_int_32^"     = OpTypeInt 32 1
"^id t_int_40^"     = OpTypeInt 40 1
"^id t_int_64^"     = OpTypeInt 64 1

"^id c_int_32_1^"   = OpConstant "^id t_int_32^" 200
"^id c_int_40_1^"   = OpConstant "^id t_int_40^" 400
"^id c_int_40_2^"   = OpConstant "^id t_int_40^" 68719411455
"^id c_int_64_1^"   = OpConstant "^id t_int_64^" 71793711247196287
    "
  );
  (*
  ("signed integer values", fun () ->
  );
  ("floating point values", fun () ->
  );
  *)
  ("string values", fun () ->
    let func = 1l in
    let t_void = 2l in
    let t_func = 3l in
    let s_1 = 4l in
    let s_2 = 5l in
    let s_3 = 6l in
    let s_4 = 7l in
    let s_5 = 8l in
    let s_6 = 9l in
    let s_7 = 10l in
    let s_8 = 11l in
    let s_9 = 12l in

    [
      `OpCapability CapabilityShader;
      `OpMemoryModel (AddressingModelLogical, MemoryModelSimple);
      `OpEntryPoint (ExecutionModelGLCompute, func, "f", []);
      `OpExecutionMode (func, ExecutionModeLocalSize (1l, 1l, 1l));

      `OpTypeVoid t_void;
      `OpTypeFunction (t_func, t_void, []);
    ], "
                      OpCapability Shader
                      OpMemoryModel Logical Simple
                      OpEntryPoint GLCompute "^id func^" \"f\"
                      OpExecutionMode "^id func^" LocalSize 1 1 1

"^id s_1^"          = OpString \"a\"
"^id s_2^"          = OpString \"ab\"
"^id s_3^"          = OpString \"abc\"
"^id s_4^"          = OpString \"abcd\"
"^id s_5^"          = OpString \"abcde\"
"^id s_6^"          = OpString \"abcdef\"
"^id s_7^"          = OpString \"abcdefg\"
"^id s_8^"          = OpString \"abcdefgh\"
"^id s_9^"          = OpString \"this is a really long string\"

"^id t_void^"       = OpTypeVoid
"^id t_func^"       = OpTypeFunction "^id t_void^"
    "
  );
  ("specialization operations", fun () ->
    let func = 1l in
    let t_void = 2l in
    let t_func = 3l in
    let t_int = 4l in
    let c_a = 5l in
    let c_b = 6l in
    let c_c = 7l in

    [
      `OpCapability CapabilityShader;
      `OpMemoryModel (AddressingModelLogical, MemoryModelSimple);
      `OpEntryPoint (ExecutionModelGLCompute, func, "f", []);
      `OpExecutionMode (func, ExecutionModeLocalSize (1l, 1l, 1l));

      `OpTypeVoid t_void;
      `OpTypeFunction (t_func, t_void, []);

      `OpTypeInt (t_int, 32l, 1l);

      `OpConstant (t_int, c_a, BigInt (Big_int.of_int 100));
      `OpConstant (t_int, c_b, BigInt (Big_int.of_int 200));

      `OpSpecConstantOp (t_int, c_c, `IAdd (c_a, c_b))
    ], "
                      OpCapability Shader
                      OpMemoryModel Logical Simple
                      OpEntryPoint GLCompute "^id func^" \"f\"
                      OpExecutionMode "^id func^" LocalSize 1 1 1

"^id t_void^"       = OpTypeVoid
"^id t_func^"       = OpTypeFunction "^id t_void^"

"^id t_int^"        = OpTypeInt 32 1

"^id c_a^"          = OpConstant "^id t_int^" 100
"^id c_b^"          = OpConstant "^id t_int^" 200

"^id c_c^"          = OpSpecConstantOp "^id t_int^" IAdd "^id c_a^" "^id c_b^"
    "
  );
  ("extended instructions", fun () ->
    let glsl = 1l in
    let func = 2l in
    let t_void = 3l in
    let t_func = 4l in
    let t_int = 5l in
    let c_9 = 6l in
    let label = 7l in
    let r = 8l in

    [
      `OpCapability CapabilityShader;
      `OpExtInstImport (glsl, "GLSL.std.450");
      `OpMemoryModel (AddressingModelLogical, MemoryModelSimple);
      `OpEntryPoint (ExecutionModelGLCompute, func, "f", []);
      `OpExecutionMode (func, ExecutionModeLocalSize (1l, 1l, 1l));

      `OpTypeVoid t_void;
      `OpTypeFunction (t_func, t_void, []);

      `OpTypeInt (t_int, 32l, 1l);

      `OpConstant (t_int, c_9, BigInt (Big_int.of_int 9));

      `OpFunction (t_void, func, FunctionControlNone, t_func);
      `OpLabel label;
      `OpExtInst (t_int, r, glsl, fun () -> [0x0001l; c_9]);
    ], "
                      OpCapability Shader
"^id glsl^"         = OpExtInstImport \"GLSL.std.450\"
                      OpMemoryModel Logical Simple
                      OpEntryPoint GLCompute "^id func^" \"f\"
                      OpExecutionMode "^id func^" LocalSize 1 1 1

"^id t_void^"       = OpTypeVoid
"^id t_func^"       = OpTypeFunction "^id t_void^"

"^id t_int^"        = OpTypeInt 32 1

"^id c_9^"          = OpConstant "^id t_int^" 9

"^id func^"         = OpFunction "^id t_void^" None "^id t_func^"
"^id label^"        = OpLabel
"^id r^"            = OpExtInst "^id t_int^" "^id glsl^" Sqrt "^id c_9^"
    "
  );
  ("very large program", fun () ->
    let func = 1l in
    let t_void = 2l in
    let t_func = 3l in
    let t_int = 4l in

    let statement_base_id = 5l in

    let statement_count = 5000l in

    let base_ops = [
      `OpCapability CapabilityShader;
      `OpMemoryModel (AddressingModelLogical, MemoryModelSimple);
      `OpEntryPoint (ExecutionModelGLCompute, func, "f", []);
      `OpExecutionMode (func, ExecutionModeLocalSize (1l, 1l, 1l));

      `OpTypeVoid t_void;
      `OpTypeFunction (t_func, t_void, []);

      `OpTypeInt (t_int, 32l, 1l)
    ] in

    let build_op_statement identifier =
      `OpConstant (t_int, identifier, BigInt (Big_int.big_int_of_int 256))
    in

    let base_asm_source = "
                      OpCapability Shader
                      OpMemoryModel Logical Simple
                      OpEntryPoint GLCompute "^id func^" \"f\"
                      OpExecutionMode "^id func^" LocalSize 1 1 1

"^id t_void^"       = OpTypeVoid
"^id t_func^"       = OpTypeFunction "^id t_void^"

"^id t_int^"        = OpTypeInt 32 1"
    in

    let build_asm_statement identifier =
      id identifier^" = OpConstant "^id t_int^" 256"
    in

    let build_statements fn max =
      let rec loop i =
        if i > max then [] else fn (Int32.add statement_base_id i) :: loop (Int32.add i 1l)
      in
      loop 0l
    in

    let ops = base_ops @ build_statements build_op_statement statement_count in
    let asm_source = base_asm_source ^ "\n" ^ (String.concat "\n" @@ build_statements build_asm_statement statement_count) in

    (ops, asm_source)
  );
  ("copy.spv", fun () ->
    let func = 1l in
    let v_in = 2l in
    let v_out = 3l in
    let v_g_index = 4l in
    let t_struct = 5l in
    let t_in_arr = 6l in
    let t_void = 7l in
    let t_func = 8l in
    let t_int = 9l in
    let c_zero = 10l in
    let c_in_sz = 11l in
    let t_vec = 12l in
    let t_u_struct_p = 13l in
    let t_u_int_p = 14l in
    let t_in_vec_p = 15l in
    let t_in_int_p = 16l in
    let label = 17l in
    let g_index_p = 18l in
    let g_index = 19l in
    let in_p = 20l in
    let out_p = 21l in
    let input = 22l in

    [
      `OpCapability CapabilityShader;
      `OpMemoryModel (AddressingModelLogical, MemoryModelSimple);
      `OpEntryPoint (ExecutionModelGLCompute, func, "f", [v_in; v_out; v_g_index]);
      `OpExecutionMode (func, ExecutionModeLocalSize (1l, 1l, 1l));

      `OpDecorate (t_struct, DecorationBufferBlock);
      `OpDecorate (v_g_index, DecorationBuiltIn BuiltInGlobalInvocationId);
      `OpDecorate (v_in, DecorationDescriptorSet 0l);
      `OpDecorate (v_in, DecorationBinding 0l);
      `OpDecorate (v_out, DecorationDescriptorSet 0l);
      `OpDecorate (v_out, DecorationBinding 1l);
      `OpDecorate (t_in_arr, DecorationArrayStride 4l);
      `OpMemberDecorate (t_struct, 0l, DecorationOffset 0l);

      `OpTypeVoid t_void;
      `OpTypeFunction (t_func, t_void, []);
      `OpTypeInt (t_int, 32l, 1l);

      `OpConstant (t_int, c_zero, BigInt (Big_int.big_int_of_int 0));
      `OpConstant (t_int, c_in_sz, BigInt (Big_int.big_int_of_int 2048));

      `OpTypeArray (t_in_arr, t_int, c_in_sz);
      `OpTypeStruct (t_struct, [t_in_arr]);
      `OpTypeVector (t_vec, t_int, 3l);
      `OpTypePointer (t_u_struct_p, StorageClassUniform, t_struct);
      `OpTypePointer (t_u_int_p, StorageClassUniform, t_int);
      `OpTypePointer (t_in_vec_p, StorageClassInput, t_vec);
      `OpTypePointer (t_in_int_p, StorageClassInput, t_int);

      `OpVariable (t_u_struct_p, v_in, StorageClassUniform, None);
      `OpVariable (t_u_struct_p, v_out, StorageClassUniform, None);
      `OpVariable (t_u_struct_p, v_g_index, StorageClassInput, None);

      `OpFunction (t_void, func, FunctionControlNone, t_func);
      `OpLabel label;
      `OpAccessChain (t_in_int_p, g_index_p, v_g_index, [c_zero]);
      `OpLoad (t_int, g_index, g_index_p, None);
      `OpAccessChain (t_u_int_p, in_p, v_in, [c_zero; g_index]);
      `OpAccessChain (t_u_int_p, out_p, v_out, [c_zero; g_index]);
      `OpLoad (t_int, input, in_p, None);
      `OpStore (out_p, input, None);
      `OpReturn;
      `OpFunctionEnd
    ], "
                      OpCapability Shader
                      OpMemoryModel Logical Simple
                      OpEntryPoint GLCompute "^id func^" \"f\" "^id v_in^" "^id v_out^" "^id v_g_index^"
                      OpExecutionMode "^id func^" LocalSize 1 1 1

                      OpDecorate "^id t_struct^" BufferBlock
                      OpDecorate "^id v_g_index^" BuiltIn GlobalInvocationId
                      OpDecorate "^id v_in^" DescriptorSet 0
                      OpDecorate "^id v_in^" Binding 0
                      OpDecorate "^id v_out^" DescriptorSet 0
                      OpDecorate "^id v_out^" Binding 1
                      OpDecorate "^id t_in_arr^" ArrayStride 4
                      OpMemberDecorate "^id t_struct^" 0 Offset 0

"^id t_void^"       = OpTypeVoid
"^id t_func^"       = OpTypeFunction "^id t_void^"
"^id t_int^"        = OpTypeInt 32 1

"^id c_zero^"       = OpConstant "^id t_int^" 0
"^id c_in_sz^"      = OpConstant "^id t_int^" 2048

"^id t_in_arr^"     = OpTypeArray "^id t_int^" "^id c_in_sz^"
"^id t_struct^"     = OpTypeStruct "^id t_in_arr^"
"^id t_vec^"        = OpTypeVector "^id t_int^" 3
"^id t_u_struct_p^" = OpTypePointer Uniform "^id t_struct^"
"^id t_u_int_p^"    = OpTypePointer Uniform "^id t_int^"
"^id t_in_vec_p^"   = OpTypePointer Input "^id t_vec^"
"^id t_in_int_p^"   = OpTypePointer Input "^id t_int^"

"^id v_in^"         = OpVariable "^id t_u_struct_p^" Uniform
"^id v_out^"        = OpVariable "^id t_u_struct_p^" Uniform
"^id v_g_index^"    = OpVariable "^id t_u_struct_p^" Input

"^id func^"         = OpFunction "^id t_void^" None "^id t_func^"
"^id label^"        = OpLabel
"^id g_index_p^"    = OpAccessChain "^id t_in_int_p^" "^id v_g_index^" "^id c_zero^"
"^id g_index^"      = OpLoad "^id t_int^" "^id g_index_p^"
"^id in_p^"         = OpAccessChain "^id t_u_int_p^" "^id v_in^" "^id c_zero^" "^id g_index^"
"^id out_p^"        = OpAccessChain "^id t_u_int_p^" "^id v_out^" "^id c_zero^" "^id g_index^"
"^id input^"        = OpLoad "^id t_int^" "^id in_p^"
                      OpStore "^id out_p^" "^id input^"
                      OpReturn
                      OpFunctionEnd
    "
  );
]

(*
let validation_exception_set_creators = [
  ("result types must be defined", fun () ->
    [
    ], Id_not_defined (t_int)
  )
];
*)

let string_of_word = Printf.sprintf "0x%08lx"

let pp_diff_words f (expected, actual) =
  let open Format in
  let mark a b = if a = b then "O" else "X" in
  let rec loop = function
    | (ah :: at, bh :: bt) ->
      pp_print_string f ("| " ^ mark ah bh ^ " | " ^ string_of_word ah ^ " | " ^ string_of_word bh ^ " |");
      pp_force_newline f ();
      loop (at, bt)
    | (ah :: at, [])       ->
      pp_print_string f ("| X | " ^ string_of_word ah ^ " |            |");
      pp_force_newline f ();
      loop (at, [])
    | ([], bh :: bt)       ->
      pp_print_string f ("| X |            | " ^ string_of_word bh ^ " |");
      pp_force_newline f ();
      loop ([], bt)
    | ([], [])             -> ()
  in
  let cap = "===============================" in
  pp_force_newline f ();
  pp_print_string f cap;
  pp_force_newline f ();
  pp_print_string f "|   |  Expected  |   Actual   |";
  pp_force_newline f ();
  pp_print_string f cap;
  pp_force_newline f ();
  loop (expected, actual);
  pp_print_string f cap;
  pp_force_newline f ()

(*
let disassemble_words words =
  let rec write_words ch = function
    | h :: t -> (IO.write_real_i32 ch h; write_words ch t)
    | []     -> ()
  in

  let (in_ch, out_ch) = Unix.open_process "spirv-dis --raw-id -" in
  write_words out_ch words;
  let str = IO.read_all in_ch in
  if Unix.close_process (in_ch, out_ch) = Unix.WEXITED 0 then
    str
  else
    "Disassembly error: " ^ str
*)

let build_binary_comparison_test (name, fn) =
  let (ops, asm_source) = fn () in
  let rec read_all_with fn ch =
    try
      let value = fn ch in
      value :: read_all_with fn ch
    with
      | IO.No_more_input -> []
  in
  let check_status = function
    | Unix.WEXITED 0   -> ()
    | Unix.WEXITED n   -> assert_failure (Printf.sprintf "spirv-as exited with %d" n)
    | Unix.WSIGNALED n -> assert_failure (Printf.sprintf "spirv-as was killed by signal with exit code %d" n)
    | Unix.WSTOPPED n  -> assert_failure (Printf.sprintf "spirv-as was stopped by signal with exit code %d" n)
  in
  let fix_generator_code = function
    | (ma :: (va :: (ga :: ta)), mb :: (vb :: (gb :: tb))) ->
        (ma :: (va :: (ga :: ta)), mb :: (vb :: (ga :: tb)))
    | _ -> failwith "trim_gen_code called on invalid list"
  in
  name >:: fun _ -> begin
    let op_words = compile_to_words ops in
    (* let (in_ch, out_ch) = Unix.open_process (Printf.sprintf "echo '%s'spirv-as -o - -" asm_source) in *)
    let in_ch = Unix.open_process_in (Printf.sprintf "echo '%s' | spirv-as -o - -" asm_source) in
    (* IO.write_string out_ch asm_source; *)
    let asm_words = read_all_with IO.read_real_i32 in_ch in
    check_status @@ Unix.close_process_in in_ch;
    let (op_words, asm_words) = fix_generator_code (op_words, asm_words) in
    assert_equal ~pp_diff:pp_diff_words asm_words op_words
  end

(*
let build_validation_exception_test (name, fn) =
  let (ops, expected_error) = fn () in
  name >:: fun _ -> assert_raises expected_error (fun () -> compile_to_words ops)
*)

let suite = "SpirV" >::: [
  "binary comparisons" >::: List.map build_binary_comparison_test binary_comparison_set_creators
  (* "validation exceptions" >::: List.map build_validation_exception_test validation_exception_set_creators *)
]

let _ = run_test_tt_main suite
