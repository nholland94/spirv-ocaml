open OUnit2
open Batteries
open SpirV

let binary_comparison_set_creators =[
  ("copy.spv", fun () ->
    let func = 1 in
    let v_in = 2 in
    let v_out = 3 in
    let v_g_index = 4 in
    let t_struct = 5 in
    let t_in_arr = 6 in
    let t_void = 7 in
    let t_func = 8 in
    let t_int = 9 in
    let c_zero = 10 in
    let c_in_sz = 11 in
    let t_vec = 12 in
    let t_u_struct_p = 13 in
    let t_u_int_p = 14 in
    let t_in_vec_p = 15 in
    let t_in_int_p = 16 in
    let label = 17 in
    let g_index_p = 18 in
    let g_index = 19 in
    let in_p = 20 in
    let out_p = 21 in
    let input = 22 in

    let id n = "%" ^ string_of_int n in

    [
      `OpCapability Shader;
      `OpMemoryModel (Logical, Simple);
      `OpEntryPoint (GLCompute, func, "f", v_in, v_out, v_g_index);
      `OpExecutionMode (func, LocalSize (1, 1, 1));

      `OpDecorate (t_struct, BufferBlock);
      `OpDecorate (v_g_index, BuiltIn GlobalInvocationId);
      `OpDecorate (v_in, DescriptorSet 0);
      `OpDecorate (v_in, Binding 0);
      `OpDecorate (v_out, DescriptorSet 0);
      `OpDecorate (v_out, Binding 1);
      `OpDecorate (t_in_arr, ArrayStride 4);
      `OpMemberDecorate (t_struct, 0, Offset 0);

      `OpTypeVoid t_void;
      `OpTypeFunction (t_func, t_void, []);
      `OpTypeInt (t_int, 32, 1);

      `OpConstant (c_zero, t_int, 0);
      `OpConstant (c_in_sz, t_int, 2048);

      `OpTypeArray (t_in_arr, t_int, c_in_sz);
      `OpTypeStruct (t_struct, t_in_arr);
      `OpTypeVector (t_vec, t_int, 3);
      `OpTypePointer (t_u_struct_p, Uniform, t_struct);
      `OpTypePointer (t_u_int_p, Uniform, t_int);
      `OpTypePointer (t_in_vec_p, Input, t_vec);
      `OpTypePointer (t_in_int_p, Input, t_int);

      `OpVariable (v_in, t_u_struct_p, Uniform);
      `OpVariable (v_out, t_u_struct_p, Uniform);
      `OpVariable (v_g_index, t_in_vec_p, Input);

      `OpFunction (func, t_void, None, t_func);
      `OpLabel label;
      `OpAccessChain (g_index_p, t_in_int_p, v_g_index, [c_zero]);
      `OpLoad (g_index, t_int, g_index_p);
      `OpAccessChain (in_p, t_u_int_p, v_in, [c_zero; g_index]);
      `OpAccessChain (out_p, t_u_int_p, v_out, [c_zero; g_index]);
      `OpLoad (input, t_int, in_p);
      `OpStore (out_p, input);
      `OpReturn;
      `OpFunctionEnd
    ], "
                      OpCapability Shader
                      OpMemoryModel Logical Simple
                      OpEntryPoint GLCompute "^id func^" \"f\" "^id v_in^" "^id v_out^" "^id v_g_index^"
                      OpExecutionMode "^id func^" LocalSize 1 1 1

                      OpDecorate "^id t_struct^" BufferBlock
                      OpDecorate "^id v_g_index^" BuiltInt GlobalInvocationId
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
"^id v_g_index^"    = OpVariable "^id t_in_vec_p^" Input

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

let build_binary_comparison_test (name, fn) =
  let (ops, asm_source) = fn () in
  let rec read_all_with fn ch =
    try
      let value = fn ch in
      value :: read_all_with fn ch
    with
      | IO.No_more_input -> []
  in
  name >:: fun _ -> begin
    let op_words = compile_to_words ops in
    let in_ch = Unix.open_process_in (Printf.sprintf "spirv-as -o - %s" asm_source) in
    let asm_words = read_all_with IO.read_real_i32 in_ch in
    close_in in_ch;
    assert_equal op_words asm_words
  end

let suite = "SpirV" >::: [
  "binary_comparisons" >::: List.map build_binary_comparison_test binary_comparison_set_creators
]

let _ = run_test_tt_main suite
