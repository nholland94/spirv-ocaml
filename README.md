# spirv-ocaml

A code-generated spirv compiler for ocaml.

[![Build Status](https://travis-ci.org/nholland94/spirv-ocaml.svg?branch=master)](https://travis-ci.org/nholland94/spirv-ocaml)

## Status

The library is ready for use and is available on [opam](https://opam.ocaml.org/packages/spirv/).

Things to do:
  - add 64 bit float literal support
  - add validations

## Warnings

This library currently does not perform any validation on the code being generated, so invalid programs will easily compile. It is recommended to check the output of this with the `spirv-val` tool from the Khronos SpirV tools repository.

## Usage

SpirV operations are specified using label variants (the number of spirv operations exceeds the ocaml type tag size, so a normal type variant won't work). SpirV enumerants are represented as OCaml type variants. For the case of flag types, enumerants are represented as a list of flags. Enumerants that normally specify extended operands wrap the extended operands as a tuple of arguments to the OCaml type constructor. Literal integers are represented with the `int32` type (for `int32` literals, add an `l` at the end; e.g. `10l`). Literal strings are represented with regular OCaml strings.

When specifying literal values to the `OpConstant` instruction, the values are wrapped with the `big_int_or_float` type (there is no check against the return type of `OpConstant` right now). The default op for a `OpSpecConstantOp` instruction is specified using the type `spec_op`. The values of `spec_op` are the valid `OpSpecConstantOp` instructions, except that the result type and result are removed and the `Op` prefix is removed (e.g. ```OpSpecConstantOp (type_id, result_id, `IAdd (constant_a, constant_b))``).

Extended instructions are currently implemented through a function abstraction. The extended instruction and it's operands are specified with a function in `OpExtInst` which has the type signature `unit -> int32 list`. The idea behind this is that extended instructions can be provided through other OCaml libraries which would return the compiled instruction and operands as a list of SpirV words.

## Example

```ocaml
let _ =
  let open SpirV in
  let copy_shader_instructions = [
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

    `OpFunction (t_void, func, [FunctionControlNone], t_func);
    `OpLabel label;
    `OpAccessChain (t_in_int_p, g_index_p, v_g_index, [c_zero]);
    `OpLoad (t_int, g_index, g_index_p, None);
    `OpAccessChain (t_u_int_p, in_p, v_in, [c_zero; g_index]);
    `OpAccessChain (t_u_int_p, out_p, v_out, [c_zero; g_index]);
    `OpLoad (t_int, input, in_p, None);
    `OpStore (out_p, input, None);
    `OpReturn;
    `OpFunctionEnd
  ] in

  let copy_shader = compile_to_words copy_shader_instructions in
  ... (* do some work with compiled shader module *)
```
