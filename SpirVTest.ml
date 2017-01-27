open OUnit2
open SpirV

let binary_comparison_set_creators =[
  (fun () ->
    let func = 1 in
    let v_in = 2 in
    let v_out = 3 in
    let v_g_index = 4 in

    let s = string_of_int in

    [
      `OpCapability Shader;
      `OpMemoryModel (Logical, Simple);
      `OpEntryPoint (GLCompute, func, "f", v_in, v_out, v_g_index);

      `OpExecutionMode (func, LocalSize, 1, 1, 1)
    ], "
      OpCapability Shader
      OpMemoryModel Logical Simple
      OpEntryPoint GLCompute %"^s func^" \"f\" %"^s v_in^" %"^s v_out^" %"^s v_g_index^"
    "
  );
]

let suite = "SpirV" >::: [
  "binary_comparisons" >::: [
    (
    )
  ]
]

let _ = run_test_tt_main suite
