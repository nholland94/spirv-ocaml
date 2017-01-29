# spirv-ocaml

A code-generated spirv compiler for ocaml.

[![Build Status](https://travis-ci.org/nholland94/spirv-ocaml.svg?branch=master)](https://travis-ci.org/nholland94/spirv-ocaml)

## Status

Still under development, but close to ready for the first opam publish.

Things to do:
  - fix binary encoding of literal values
  - flag folding
  - write more tests
  - add validations

## Warnings

This library currently does not perform any validation on the code being generated, so invalid programs will easily compile. It is recommended to check the output of this with the `spirv-val` tool from the Khronos SpirV tools repository.

## Usage

SpirV operations are specified using label variants (the number of spirv operations exceeds the ocaml type tag size, so a normal type variant won't work). SpirV enumerants are represented as OCaml type variants. For the case of flag types, enumerants are represented as a list of flags. Enumerants that normally specify extended operands wrap the extended operands as a tuple of arguments to the OCaml type constructor. Literal integers are represented with the `int32` type (for `int32` literals, add an `l` at the end; e.g. `10l`). Literal strings are represented with regular OCaml strings.

When specifying literal values to the `OpConstant` instruction, the values are wrapped with the `big_int_or_float` type (there is no check against the return type of `OpConstant` right now). The default op for a `OpSpecConstantOp` instruction is specified using the type `spec_op`. The values of `spec_op` are the valid `OpSpecConstantOp` instructions, except that the result type and result are removed and the `Op` prefix is removed (e.g. `\`OpSpecConstantOp (type\_id, result\_id, \`IAdd (constant\_a, constant\_b))`).

Extended instructions are currently implemented through a function abstraction. The extended instruction and it's operands are specified with a function in `OpExtInst` which has the type signature `unit -> int32 list`. The idea behind this is that extended instructions can be provided through other OCaml libraries which would return the compiled instruction and operands as a list of SpirV words.
