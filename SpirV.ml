open Big_int;;
module IdMap = Map.Make(Int32);;
exception Id_not_found of Int32.t;;
type id = int32;;
type word = int32;;
type id_result_type = id;;
type id_result = id;;
type id_memory_semantics = id;;
type id_scope = id;;
type id_ref = id;;
type literal_integer = int32;;
type literal_string = string;;
type literal_context_dependent_number = big_int;;
type literal_ext_inst_integer = int;;
type literal_spec_constant_op_integer = int;;
type pair_literal_integer_id_ref = (int32 * id);;
type pair_id_ref_literal_integer = (id * int32);;
type pair_id_ref_id_ref = (id * id);;
type image_operands =
  None
  | Bias
  | Lod
  | Grad
  | ConstOffset
  | Offset
  | ConstOffsets
  | Sample
  | MinLod;;
type f_p_fast_math_mode = None | NotNaN | NotInf | NSZ | AllowRecip | Fast;;
type selection_control = None | Flatten | DontFlatten;;
type loop_control =
  None
  | Unroll
  | DontUnroll
  | DependencyInfinite
  | DependencyLength;;
type function_control = None | Inline | DontInline | Pure | Const;;
type memory_semantics =
  Relaxed
  | None
  | Acquire
  | Release
  | AcquireRelease
  | SequentiallyConsistent
  | UniformMemory
  | SubgroupMemory
  | WorkgroupMemory
  | CrossWorkgroupMemory
  | AtomicCounterMemory
  | ImageMemory;;
type memory_access = None | Volatile | Aligned | Nontemporal;;
type kernel_profiling_info = None | CmdExecTime;;
type source_language = Unknown | ESSL | GLSL | OpenCL_C | OpenCL_CPP;;
type execution_model =
  Vertex
  | TessellationControl
  | TessellationEvaluation
  | Geometry
  | Fragment
  | GLCompute
  | Kernel;;
type addressing_model = Logical | Physical32 | Physical64;;
type memory_model = Simple | GLSL450 | OpenCL;;
type execution_mode =
  Invocations
  | SpacingEqual
  | SpacingFractionalEven
  | SpacingFractionalOdd
  | VertexOrderCw
  | VertexOrderCcw
  | PixelCenterInteger
  | OriginUpperLeft
  | OriginLowerLeft
  | EarlyFragmentTests
  | PointMode
  | Xfb
  | DepthReplacing
  | DepthGreater
  | DepthLess
  | DepthUnchanged
  | LocalSize
  | LocalSizeHint
  | InputPoints
  | InputLines
  | InputLinesAdjacency
  | Triangles
  | InputTrianglesAdjacency
  | Quads
  | Isolines
  | OutputVertices
  | OutputPoints
  | OutputLineStrip
  | OutputTriangleStrip
  | VecTypeHint
  | ContractionOff
  | Initializer
  | Finalizer
  | SubgroupSize
  | SubgroupsPerWorkgroup;;
type storage_class =
  UniformConstant
  | Input
  | Uniform
  | Output
  | Workgroup
  | CrossWorkgroup
  | Private
  | Function
  | Generic
  | PushConstant
  | AtomicCounter
  | Image;;
type dim = Dim1D | Dim2D | Dim3D | Cube | Rect | Buffer | SubpassData;;
type sampler_addressing_mode =
  None
  | ClampToEdge
  | Clamp
  | Repeat
  | RepeatMirrored;;
type sampler_filter_mode = Nearest | Linear;;
type image_format =
  Unknown
  | Rgba32f
  | Rgba16f
  | R32f
  | Rgba8
  | Rgba8Snorm
  | Rg32f
  | Rg16f
  | R11fG11fB10f
  | R16f
  | Rgba16
  | Rgb10A2
  | Rg16
  | Rg8
  | R16
  | R8
  | Rgba16Snorm
  | Rg16Snorm
  | Rg8Snorm
  | R16Snorm
  | R8Snorm
  | Rgba32i
  | Rgba16i
  | Rgba8i
  | R32i
  | Rg32i
  | Rg16i
  | Rg8i
  | R16i
  | R8i
  | Rgba32ui
  | Rgba16ui
  | Rgba8ui
  | R32ui
  | Rgb10a2ui
  | Rg32ui
  | Rg16ui
  | Rg8ui
  | R16ui
  | R8ui;;
type image_channel_order =
  R
  | A
  | RG
  | RA
  | RGB
  | RGBA
  | BGRA
  | ARGB
  | Intensity
  | Luminance
  | Rx
  | RGx
  | RGBx
  | Depth
  | DepthStencil
  | SRGB
  | SRGBx
  | SRGBA
  | SBGRA
  | ABGR;;
type image_channel_data_type =
  SnormInt8
  | SnormInt16
  | UnormInt8
  | UnormInt16
  | UnormShort565
  | UnormShort555
  | UnormInt101010
  | SignedInt8
  | SignedInt16
  | SignedInt32
  | UnsignedInt8
  | UnsignedInt16
  | UnsignedInt32
  | HalfFloat
  | Float
  | UnormInt24
  | UnormInt101010_2;;
type f_p_rounding_mode = RTE | RTZ | RTP | RTN;;
type linkage_type = Export | Import;;
type access_qualifier = ReadOnly | WriteOnly | ReadWrite;;
type function_parameter_attribute =
  Zext
  | Sext
  | ByVal
  | Sret
  | NoAlias
  | NoCapture
  | NoWrite
  | NoReadWrite;;
type decoration =
  RelaxedPrecision
  | SpecId
  | Block
  | BufferBlock
  | RowMajor
  | ColMajor
  | ArrayStride
  | MatrixStride
  | GLSLShared
  | GLSLPacked
  | CPacked
  | BuiltIn
  | NoPerspective
  | Flat
  | Patch
  | Centroid
  | Sample
  | Invariant
  | Restrict
  | Aliased
  | Volatile
  | Constant
  | Coherent
  | NonWritable
  | NonReadable
  | Uniform
  | SaturatedConversion
  | Stream
  | Location
  | Component
  | Index
  | Binding
  | DescriptorSet
  | Offset
  | XfbBuffer
  | XfbStride
  | FuncParamAttr
  | FPRoundingMode
  | FPFastMathMode
  | LinkageAttributes
  | NoContraction
  | InputAttachmentIndex
  | Alignment
  | MaxByteOffset;;
type built_in =
  Position
  | PointSize
  | ClipDistance
  | CullDistance
  | VertexId
  | InstanceId
  | PrimitiveId
  | InvocationId
  | Layer
  | ViewportIndex
  | TessLevelOuter
  | TessLevelInner
  | TessCoord
  | PatchVertices
  | FragCoord
  | PointCoord
  | FrontFacing
  | SampleId
  | SamplePosition
  | SampleMask
  | FragDepth
  | HelperInvocation
  | NumWorkgroups
  | WorkgroupSize
  | WorkgroupId
  | LocalInvocationId
  | GlobalInvocationId
  | LocalInvocationIndex
  | WorkDim
  | GlobalSize
  | EnqueuedWorkgroupSize
  | GlobalOffset
  | GlobalLinearId
  | SubgroupSize
  | SubgroupMaxSize
  | NumSubgroups
  | NumEnqueuedSubgroups
  | SubgroupId
  | SubgroupLocalInvocationId
  | VertexIndex
  | InstanceIndex
  | SubgroupEqMaskKHR
  | SubgroupGeMaskKHR
  | SubgroupGtMaskKHR
  | SubgroupLeMaskKHR
  | SubgroupLtMaskKHR
  | BaseVertex
  | BaseInstance
  | DrawIndex;;
type scope = CrossDevice | Device | Workgroup | Subgroup | Invocation;;
type group_operation = Reduce | InclusiveScan | ExclusiveScan;;
type kernel_enqueue_flags = NoWait | WaitKernel | WaitWorkGroup;;
type capability =
  Matrix
  | Shader
  | Geometry
  | Tessellation
  | Addresses
  | Linkage
  | Kernel
  | Vector16
  | Float16Buffer
  | Float16
  | Float64
  | Int64
  | Int64Atomics
  | ImageBasic
  | ImageReadWrite
  | ImageMipmap
  | Pipes
  | Groups
  | DeviceEnqueue
  | LiteralSampler
  | AtomicStorage
  | Int16
  | TessellationPointSize
  | GeometryPointSize
  | ImageGatherExtended
  | StorageImageMultisample
  | UniformBufferArrayDynamicIndexing
  | SampledImageArrayDynamicIndexing
  | StorageBufferArrayDynamicIndexing
  | StorageImageArrayDynamicIndexing
  | ClipDistance
  | CullDistance
  | ImageCubeArray
  | SampleRateShading
  | ImageRect
  | SampledRect
  | GenericPointer
  | Int8
  | InputAttachment
  | SparseResidency
  | MinLod
  | Sampled1D
  | Image1D
  | SampledCubeArray
  | SampledBuffer
  | ImageBuffer
  | ImageMSArray
  | StorageImageExtendedFormats
  | ImageQuery
  | DerivativeControl
  | InterpolationFunction
  | TransformFeedback
  | GeometryStreams
  | StorageImageReadWithoutFormat
  | StorageImageWriteWithoutFormat
  | MultiViewport
  | SubgroupDispatch
  | NamedBarrier
  | PipeStorage
  | SubgroupBallotKHR
  | DrawParameters;;
type op =
  [
    | `OpNop
    | `OpUndef of id_result_type * id_result
    | `OpSourceContinued of literal_string
    | `OpSource of source_language * literal_integer * id_ref option
                   * literal_string option
    | `OpSourceExtension of literal_string
    | `OpName of id_ref * literal_string
    | `OpMemberName of id_ref * literal_integer * literal_string
    | `OpString of id_result * literal_string
    | `OpLine of id_ref * literal_integer * literal_integer
    | `OpExtension of literal_string
    | `OpExtInstImport of id_result * literal_string
    | `OpExtInst of id_result_type * id_result * id_ref
                    * literal_ext_inst_integer * id_ref list
    | `OpMemoryModel of addressing_model * memory_model
    | `OpEntryPoint of execution_model * id_ref * literal_string
                       * id_ref list
    | `OpExecutionMode of id_ref * execution_mode
    | `OpCapability of capability
    | `OpTypeVoid of id_result
    | `OpTypeBool of id_result
    | `OpTypeInt of id_result * literal_integer * literal_integer
    | `OpTypeFloat of id_result * literal_integer
    | `OpTypeVector of id_result * id_ref * literal_integer
    | `OpTypeMatrix of id_result * id_ref * literal_integer
    | `OpTypeImage of id_result * id_ref * dim * literal_integer
                      * literal_integer * literal_integer * literal_integer
                      * image_format * access_qualifier option
    | `OpTypeSampler of id_result
    | `OpTypeSampledImage of id_result * id_ref
    | `OpTypeArray of id_result * id_ref * id_ref
    | `OpTypeRuntimeArray of id_result * id_ref
    | `OpTypeStruct of id_result * id_ref list
    | `OpTypeOpaque of id_result * literal_string
    | `OpTypePointer of id_result * storage_class * id_ref
    | `OpTypeFunction of id_result * id_ref * id_ref list
    | `OpTypeEvent of id_result
    | `OpTypeDeviceEvent of id_result
    | `OpTypeReserveId of id_result
    | `OpTypeQueue of id_result
    | `OpTypePipe of id_result * access_qualifier
    | `OpTypeForwardPointer of id_ref * storage_class
    | `OpConstantTrue of id_result_type * id_result
    | `OpConstantFalse of id_result_type * id_result
    | `OpConstant of id_result_type * id_result
                     * literal_context_dependent_number
    | `OpConstantComposite of id_result_type * id_result * id_ref list
    | `OpConstantSampler of id_result_type * id_result
                            * sampler_addressing_mode * literal_integer
                            * sampler_filter_mode
    | `OpConstantNull of id_result_type * id_result
    | `OpSpecConstantTrue of id_result_type * id_result
    | `OpSpecConstantFalse of id_result_type * id_result
    | `OpSpecConstant of id_result_type * id_result
                         * literal_context_dependent_number
    | `OpSpecConstantComposite of id_result_type * id_result * id_ref list
    | `OpSpecConstantOp of id_result_type * id_result
                           * literal_spec_constant_op_integer
    | `OpFunction of id_result_type * id_result * function_control * id_ref
    | `OpFunctionParameter of id_result_type * id_result
    | `OpFunctionEnd
    | `OpFunctionCall of id_result_type * id_result * id_ref * id_ref list
    | `OpVariable of id_result_type * id_result * storage_class
                     * id_ref option
    | `OpImageTexelPointer of id_result_type * id_result * id_ref * id_ref
                              * id_ref
    | `OpLoad of id_result_type * id_result * id_ref * memory_access option
    | `OpStore of id_ref * id_ref * memory_access option
    | `OpCopyMemory of id_ref * id_ref * memory_access option
    | `OpCopyMemorySized of id_ref * id_ref * id_ref * memory_access option
    | `OpAccessChain of id_result_type * id_result * id_ref * id_ref list
    | `OpInBoundsAccessChain of id_result_type * id_result * id_ref
                                * id_ref list
    | `OpPtrAccessChain of id_result_type * id_result * id_ref * id_ref
                           * id_ref list
    | `OpArrayLength of id_result_type * id_result * id_ref * literal_integer
    | `OpGenericPtrMemSemantics of id_result_type * id_result * id_ref
    | `OpInBoundsPtrAccessChain of id_result_type * id_result * id_ref
                                   * id_ref * id_ref list
    | `OpDecorate of id_ref * decoration
    | `OpMemberDecorate of id_ref * literal_integer * decoration
    | `OpDecorationGroup of id_result
    | `OpGroupDecorate of id_ref * id_ref list
    | `OpGroupMemberDecorate of id_ref * pair_id_ref_literal_integer list
    | `OpVectorExtractDynamic of id_result_type * id_result * id_ref * id_ref
    | `OpVectorInsertDynamic of id_result_type * id_result * id_ref * id_ref
                                * id_ref
    | `OpVectorShuffle of id_result_type * id_result * id_ref * id_ref
                          * literal_integer list
    | `OpCompositeConstruct of id_result_type * id_result * id_ref list
    | `OpCompositeExtract of id_result_type * id_result * id_ref
                             * literal_integer list
    | `OpCompositeInsert of id_result_type * id_result * id_ref * id_ref
                            * literal_integer list
    | `OpCopyObject of id_result_type * id_result * id_ref
    | `OpTranspose of id_result_type * id_result * id_ref
    | `OpSampledImage of id_result_type * id_result * id_ref * id_ref
    | `OpImageSampleImplicitLod of id_result_type * id_result * id_ref
                                   * id_ref * image_operands option
    | `OpImageSampleExplicitLod of id_result_type * id_result * id_ref
                                   * id_ref * image_operands
    | `OpImageSampleDrefImplicitLod of id_result_type * id_result * id_ref
                                       * id_ref * id_ref
                                       * image_operands option
    | `OpImageSampleDrefExplicitLod of id_result_type * id_result * id_ref
                                       * id_ref * id_ref * image_operands
    | `OpImageSampleProjImplicitLod of id_result_type * id_result * id_ref
                                       * id_ref * image_operands option
    | `OpImageSampleProjExplicitLod of id_result_type * id_result * id_ref
                                       * id_ref * image_operands
    | `OpImageSampleProjDrefImplicitLod of id_result_type * id_result
                                           * id_ref * id_ref * id_ref
                                           * image_operands option
    | `OpImageSampleProjDrefExplicitLod of id_result_type * id_result
                                           * id_ref * id_ref * id_ref
                                           * image_operands
    | `OpImageFetch of id_result_type * id_result * id_ref * id_ref
                       * image_operands option
    | `OpImageGather of id_result_type * id_result * id_ref * id_ref * id_ref
                        * image_operands option
    | `OpImageDrefGather of id_result_type * id_result * id_ref * id_ref
                            * id_ref * image_operands option
    | `OpImageRead of id_result_type * id_result * id_ref * id_ref
                      * image_operands option
    | `OpImageWrite of id_ref * id_ref * id_ref * image_operands option
    | `OpImage of id_result_type * id_result * id_ref
    | `OpImageQueryFormat of id_result_type * id_result * id_ref
    | `OpImageQueryOrder of id_result_type * id_result * id_ref
    | `OpImageQuerySizeLod of id_result_type * id_result * id_ref * id_ref
    | `OpImageQuerySize of id_result_type * id_result * id_ref
    | `OpImageQueryLod of id_result_type * id_result * id_ref * id_ref
    | `OpImageQueryLevels of id_result_type * id_result * id_ref
    | `OpImageQuerySamples of id_result_type * id_result * id_ref
    | `OpConvertFToU of id_result_type * id_result * id_ref
    | `OpConvertFToS of id_result_type * id_result * id_ref
    | `OpConvertSToF of id_result_type * id_result * id_ref
    | `OpConvertUToF of id_result_type * id_result * id_ref
    | `OpUConvert of id_result_type * id_result * id_ref
    | `OpSConvert of id_result_type * id_result * id_ref
    | `OpFConvert of id_result_type * id_result * id_ref
    | `OpQuantizeToF16 of id_result_type * id_result * id_ref
    | `OpConvertPtrToU of id_result_type * id_result * id_ref
    | `OpSatConvertSToU of id_result_type * id_result * id_ref
    | `OpSatConvertUToS of id_result_type * id_result * id_ref
    | `OpConvertUToPtr of id_result_type * id_result * id_ref
    | `OpPtrCastToGeneric of id_result_type * id_result * id_ref
    | `OpGenericCastToPtr of id_result_type * id_result * id_ref
    | `OpGenericCastToPtrExplicit of id_result_type * id_result * id_ref
                                     * storage_class
    | `OpBitcast of id_result_type * id_result * id_ref
    | `OpSNegate of id_result_type * id_result * id_ref
    | `OpFNegate of id_result_type * id_result * id_ref
    | `OpIAdd of id_result_type * id_result * id_ref * id_ref
    | `OpFAdd of id_result_type * id_result * id_ref * id_ref
    | `OpISub of id_result_type * id_result * id_ref * id_ref
    | `OpFSub of id_result_type * id_result * id_ref * id_ref
    | `OpIMul of id_result_type * id_result * id_ref * id_ref
    | `OpFMul of id_result_type * id_result * id_ref * id_ref
    | `OpUDiv of id_result_type * id_result * id_ref * id_ref
    | `OpSDiv of id_result_type * id_result * id_ref * id_ref
    | `OpFDiv of id_result_type * id_result * id_ref * id_ref
    | `OpUMod of id_result_type * id_result * id_ref * id_ref
    | `OpSRem of id_result_type * id_result * id_ref * id_ref
    | `OpSMod of id_result_type * id_result * id_ref * id_ref
    | `OpFRem of id_result_type * id_result * id_ref * id_ref
    | `OpFMod of id_result_type * id_result * id_ref * id_ref
    | `OpVectorTimesScalar of id_result_type * id_result * id_ref * id_ref
    | `OpMatrixTimesScalar of id_result_type * id_result * id_ref * id_ref
    | `OpVectorTimesMatrix of id_result_type * id_result * id_ref * id_ref
    | `OpMatrixTimesVector of id_result_type * id_result * id_ref * id_ref
    | `OpMatrixTimesMatrix of id_result_type * id_result * id_ref * id_ref
    | `OpOuterProduct of id_result_type * id_result * id_ref * id_ref
    | `OpDot of id_result_type * id_result * id_ref * id_ref
    | `OpIAddCarry of id_result_type * id_result * id_ref * id_ref
    | `OpISubBorrow of id_result_type * id_result * id_ref * id_ref
    | `OpUMulExtended of id_result_type * id_result * id_ref * id_ref
    | `OpSMulExtended of id_result_type * id_result * id_ref * id_ref
    | `OpAny of id_result_type * id_result * id_ref
    | `OpAll of id_result_type * id_result * id_ref
    | `OpIsNan of id_result_type * id_result * id_ref
    | `OpIsInf of id_result_type * id_result * id_ref
    | `OpIsFinite of id_result_type * id_result * id_ref
    | `OpIsNormal of id_result_type * id_result * id_ref
    | `OpSignBitSet of id_result_type * id_result * id_ref
    | `OpLessOrGreater of id_result_type * id_result * id_ref * id_ref
    | `OpOrdered of id_result_type * id_result * id_ref * id_ref
    | `OpUnordered of id_result_type * id_result * id_ref * id_ref
    | `OpLogicalEqual of id_result_type * id_result * id_ref * id_ref
    | `OpLogicalNotEqual of id_result_type * id_result * id_ref * id_ref
    | `OpLogicalOr of id_result_type * id_result * id_ref * id_ref
    | `OpLogicalAnd of id_result_type * id_result * id_ref * id_ref
    | `OpLogicalNot of id_result_type * id_result * id_ref
    | `OpSelect of id_result_type * id_result * id_ref * id_ref * id_ref
    | `OpIEqual of id_result_type * id_result * id_ref * id_ref
    | `OpINotEqual of id_result_type * id_result * id_ref * id_ref
    | `OpUGreaterThan of id_result_type * id_result * id_ref * id_ref
    | `OpSGreaterThan of id_result_type * id_result * id_ref * id_ref
    | `OpUGreaterThanEqual of id_result_type * id_result * id_ref * id_ref
    | `OpSGreaterThanEqual of id_result_type * id_result * id_ref * id_ref
    | `OpULessThan of id_result_type * id_result * id_ref * id_ref
    | `OpSLessThan of id_result_type * id_result * id_ref * id_ref
    | `OpULessThanEqual of id_result_type * id_result * id_ref * id_ref
    | `OpSLessThanEqual of id_result_type * id_result * id_ref * id_ref
    | `OpFOrdEqual of id_result_type * id_result * id_ref * id_ref
    | `OpFUnordEqual of id_result_type * id_result * id_ref * id_ref
    | `OpFOrdNotEqual of id_result_type * id_result * id_ref * id_ref
    | `OpFUnordNotEqual of id_result_type * id_result * id_ref * id_ref
    | `OpFOrdLessThan of id_result_type * id_result * id_ref * id_ref
    | `OpFUnordLessThan of id_result_type * id_result * id_ref * id_ref
    | `OpFOrdGreaterThan of id_result_type * id_result * id_ref * id_ref
    | `OpFUnordGreaterThan of id_result_type * id_result * id_ref * id_ref
    | `OpFOrdLessThanEqual of id_result_type * id_result * id_ref * id_ref
    | `OpFUnordLessThanEqual of id_result_type * id_result * id_ref * id_ref
    | `OpFOrdGreaterThanEqual of id_result_type * id_result * id_ref * id_ref
    | `OpFUnordGreaterThanEqual of id_result_type * id_result * id_ref
                                   * id_ref
    | `OpShiftRightLogical of id_result_type * id_result * id_ref * id_ref
    | `OpShiftRightArithmetic of id_result_type * id_result * id_ref * id_ref
    | `OpShiftLeftLogical of id_result_type * id_result * id_ref * id_ref
    | `OpBitwiseOr of id_result_type * id_result * id_ref * id_ref
    | `OpBitwiseXor of id_result_type * id_result * id_ref * id_ref
    | `OpBitwiseAnd of id_result_type * id_result * id_ref * id_ref
    | `OpNot of id_result_type * id_result * id_ref
    | `OpBitFieldInsert of id_result_type * id_result * id_ref * id_ref
                           * id_ref * id_ref
    | `OpBitFieldSExtract of id_result_type * id_result * id_ref * id_ref
                             * id_ref
    | `OpBitFieldUExtract of id_result_type * id_result * id_ref * id_ref
                             * id_ref
    | `OpBitReverse of id_result_type * id_result * id_ref
    | `OpBitCount of id_result_type * id_result * id_ref
    | `OpDPdx of id_result_type * id_result * id_ref
    | `OpDPdy of id_result_type * id_result * id_ref
    | `OpFwidth of id_result_type * id_result * id_ref
    | `OpDPdxFine of id_result_type * id_result * id_ref
    | `OpDPdyFine of id_result_type * id_result * id_ref
    | `OpFwidthFine of id_result_type * id_result * id_ref
    | `OpDPdxCoarse of id_result_type * id_result * id_ref
    | `OpDPdyCoarse of id_result_type * id_result * id_ref
    | `OpFwidthCoarse of id_result_type * id_result * id_ref
    | `OpEmitVertex
    | `OpEndPrimitive
    | `OpEmitStreamVertex of id_ref
    | `OpEndStreamPrimitive of id_ref
    | `OpControlBarrier of id_scope * id_scope * id_memory_semantics
    | `OpMemoryBarrier of id_scope * id_memory_semantics
    | `OpAtomicLoad of id_result_type * id_result * id_ref * id_scope
                       * id_memory_semantics
    | `OpAtomicStore of id_ref * id_scope * id_memory_semantics * id_ref
    | `OpAtomicExchange of id_result_type * id_result * id_ref * id_scope
                           * id_memory_semantics * id_ref
    | `OpAtomicCompareExchange of id_result_type * id_result * id_ref
                                  * id_scope * id_memory_semantics
                                  * id_memory_semantics * id_ref * id_ref
    | `OpAtomicCompareExchangeWeak of id_result_type * id_result * id_ref
                                      * id_scope * id_memory_semantics
                                      * id_memory_semantics * id_ref * id_ref
    | `OpAtomicIIncrement of id_result_type * id_result * id_ref * id_scope
                             * id_memory_semantics
    | `OpAtomicIDecrement of id_result_type * id_result * id_ref * id_scope
                             * id_memory_semantics
    | `OpAtomicIAdd of id_result_type * id_result * id_ref * id_scope
                       * id_memory_semantics * id_ref
    | `OpAtomicISub of id_result_type * id_result * id_ref * id_scope
                       * id_memory_semantics * id_ref
    | `OpAtomicSMin of id_result_type * id_result * id_ref * id_scope
                       * id_memory_semantics * id_ref
    | `OpAtomicUMin of id_result_type * id_result * id_ref * id_scope
                       * id_memory_semantics * id_ref
    | `OpAtomicSMax of id_result_type * id_result * id_ref * id_scope
                       * id_memory_semantics * id_ref
    | `OpAtomicUMax of id_result_type * id_result * id_ref * id_scope
                       * id_memory_semantics * id_ref
    | `OpAtomicAnd of id_result_type * id_result * id_ref * id_scope
                      * id_memory_semantics * id_ref
    | `OpAtomicOr of id_result_type * id_result * id_ref * id_scope
                     * id_memory_semantics * id_ref
    | `OpAtomicXor of id_result_type * id_result * id_ref * id_scope
                      * id_memory_semantics * id_ref
    | `OpPhi of id_result_type * id_result * pair_id_ref_id_ref list
    | `OpLoopMerge of id_ref * id_ref * loop_control
    | `OpSelectionMerge of id_ref * selection_control
    | `OpLabel of id_result
    | `OpBranch of id_ref
    | `OpBranchConditional of id_ref * id_ref * id_ref * literal_integer list
    | `OpSwitch of id_ref * id_ref * pair_literal_integer_id_ref list
    | `OpKill
    | `OpReturn
    | `OpReturnValue of id_ref
    | `OpUnreachable
    | `OpLifetimeStart of id_ref * literal_integer
    | `OpLifetimeStop of id_ref * literal_integer
    | `OpGroupAsyncCopy of id_result_type * id_result * id_scope * id_ref
                           * id_ref * id_ref * id_ref * id_ref
    | `OpGroupWaitEvents of id_scope * id_ref * id_ref
    | `OpGroupAll of id_result_type * id_result * id_scope * id_ref
    | `OpGroupAny of id_result_type * id_result * id_scope * id_ref
    | `OpGroupBroadcast of id_result_type * id_result * id_scope * id_ref
                           * id_ref
    | `OpGroupIAdd of id_result_type * id_result * id_scope * group_operation
                      * id_ref
    | `OpGroupFAdd of id_result_type * id_result * id_scope * group_operation
                      * id_ref
    | `OpGroupFMin of id_result_type * id_result * id_scope * group_operation
                      * id_ref
    | `OpGroupUMin of id_result_type * id_result * id_scope * group_operation
                      * id_ref
    | `OpGroupSMin of id_result_type * id_result * id_scope * group_operation
                      * id_ref
    | `OpGroupFMax of id_result_type * id_result * id_scope * group_operation
                      * id_ref
    | `OpGroupUMax of id_result_type * id_result * id_scope * group_operation
                      * id_ref
    | `OpGroupSMax of id_result_type * id_result * id_scope * group_operation
                      * id_ref
    | `OpReadPipe of id_result_type * id_result * id_ref * id_ref * id_ref
                     * id_ref
    | `OpWritePipe of id_result_type * id_result * id_ref * id_ref * id_ref
                      * id_ref
    | `OpReservedReadPipe of id_result_type * id_result * id_ref * id_ref
                             * id_ref * id_ref * id_ref * id_ref
    | `OpReservedWritePipe of id_result_type * id_result * id_ref * id_ref
                              * id_ref * id_ref * id_ref * id_ref
    | `OpReserveReadPipePackets of id_result_type * id_result * id_ref
                                   * id_ref * id_ref * id_ref
    | `OpReserveWritePipePackets of id_result_type * id_result * id_ref
                                    * id_ref * id_ref * id_ref
    | `OpCommitReadPipe of id_ref * id_ref * id_ref * id_ref
    | `OpCommitWritePipe of id_ref * id_ref * id_ref * id_ref
    | `OpIsValidReserveId of id_result_type * id_result * id_ref
    | `OpGetNumPipePackets of id_result_type * id_result * id_ref * id_ref
                              * id_ref
    | `OpGetMaxPipePackets of id_result_type * id_result * id_ref * id_ref
                              * id_ref
    | `OpGroupReserveReadPipePackets of id_result_type * id_result * id_scope
                                        * id_ref * id_ref * id_ref * id_ref
    | `OpGroupReserveWritePipePackets of id_result_type * id_result
                                         * id_scope * id_ref * id_ref
                                         * id_ref * id_ref
    | `OpGroupCommitReadPipe of id_scope * id_ref * id_ref * id_ref * id_ref
    | `OpGroupCommitWritePipe of id_scope * id_ref * id_ref * id_ref * id_ref
    | `OpEnqueueMarker of id_result_type * id_result * id_ref * id_ref
                          * id_ref * id_ref
    | `OpEnqueueKernel of id_result_type * id_result * id_ref * id_ref
                          * id_ref * id_ref * id_ref * id_ref * id_ref
                          * id_ref * id_ref * id_ref * id_ref list
    | `OpGetKernelNDrangeSubGroupCount of id_result_type * id_result * id_ref
                                          * id_ref * id_ref * id_ref * id_ref
    | `OpGetKernelNDrangeMaxSubGroupSize of id_result_type * id_result
                                            * id_ref * id_ref * id_ref
                                            * id_ref * id_ref
    | `OpGetKernelWorkGroupSize of id_result_type * id_result * id_ref
                                   * id_ref * id_ref * id_ref
    | `OpGetKernelPreferredWorkGroupSizeMultiple of id_result_type
                                                    * id_result * id_ref
                                                    * id_ref * id_ref
                                                    * id_ref
    | `OpRetainEvent of id_ref
    | `OpReleaseEvent of id_ref
    | `OpCreateUserEvent of id_result_type * id_result
    | `OpIsValidEvent of id_result_type * id_result * id_ref
    | `OpSetUserEventStatus of id_ref * id_ref
    | `OpCaptureEventProfilingInfo of id_ref * id_ref * id_ref
    | `OpGetDefaultQueue of id_result_type * id_result
    | `OpBuildNDRange of id_result_type * id_result * id_ref * id_ref
                         * id_ref
    | `OpImageSparseSampleImplicitLod of id_result_type * id_result * id_ref
                                         * id_ref * image_operands option
    | `OpImageSparseSampleExplicitLod of id_result_type * id_result * id_ref
                                         * id_ref * image_operands
    | `OpImageSparseSampleDrefImplicitLod of id_result_type * id_result
                                             * id_ref * id_ref * id_ref
                                             * image_operands option
    | `OpImageSparseSampleDrefExplicitLod of id_result_type * id_result
                                             * id_ref * id_ref * id_ref
                                             * image_operands
    | `OpImageSparseSampleProjImplicitLod of id_result_type * id_result
                                             * id_ref * id_ref
                                             * image_operands option
    | `OpImageSparseSampleProjExplicitLod of id_result_type * id_result
                                             * id_ref * id_ref
                                             * image_operands
    | `OpImageSparseSampleProjDrefImplicitLod of id_result_type * id_result
                                                 * id_ref * id_ref * id_ref
                                                 * image_operands option
    | `OpImageSparseSampleProjDrefExplicitLod of id_result_type * id_result
                                                 * id_ref * id_ref * id_ref
                                                 * image_operands
    | `OpImageSparseFetch of id_result_type * id_result * id_ref * id_ref
                             * image_operands option
    | `OpImageSparseGather of id_result_type * id_result * id_ref * id_ref
                              * id_ref * image_operands option
    | `OpImageSparseDrefGather of id_result_type * id_result * id_ref
                                  * id_ref * id_ref * image_operands option
    | `OpImageSparseTexelsResident of id_result_type * id_result * id_ref
    | `OpNoLine
    | `OpAtomicFlagTestAndSet of id_result_type * id_result * id_ref
                                 * id_scope * id_memory_semantics
    | `OpAtomicFlagClear of id_ref * id_scope * id_memory_semantics
    | `OpImageSparseRead of id_result_type * id_result * id_ref * id_ref
                            * image_operands option
    | `OpSizeOf of id_result_type * id_result * id_ref
    | `OpTypePipeStorage of id_result
    | `OpConstantPipeStorage of id_result_type * id_result * literal_integer
                                * literal_integer * literal_integer
    | `OpCreatePipeFromPipeStorage of id_result_type * id_result * id_ref
    | `OpGetKernelLocalSizeForSubgroupCount of id_result_type * id_result
                                               * id_ref * id_ref * id_ref
                                               * id_ref * id_ref
    | `OpGetKernelMaxNumSubgroups of id_result_type * id_result * id_ref
                                     * id_ref * id_ref * id_ref
    | `OpTypeNamedBarrier of id_result
    | `OpNamedBarrierInitialize of id_result_type * id_result * id_ref
    | `OpMemoryNamedBarrier of id_ref * id_scope * id_memory_semantics
    | `OpModuleProcessed of literal_string
    | `OpSubgroupBallotKHR of id_result_type * id_result * id_ref
    | `OpSubgroupFirstInvocationKHR of id_result_type * id_result * id_ref
  ];;
let value_of_image_operands (v : image_operands) =
  match v with
  | None -> 0x0000l
  | Bias -> 0x0001l
  | Lod -> 0x0002l
  | Grad -> 0x0004l
  | ConstOffset -> 0x0008l
  | Offset -> 0x0010l
  | ConstOffsets -> 0x0020l
  | Sample -> 0x0040l
  | MinLod -> 0x0080l;;
let value_of_f_p_fast_math_mode (v : f_p_fast_math_mode) =
  match v with
  | None -> 0x0000l
  | NotNaN -> 0x0001l
  | NotInf -> 0x0002l
  | NSZ -> 0x0004l
  | AllowRecip -> 0x0008l
  | Fast -> 0x0010l;;
let value_of_selection_control (v : selection_control) =
  match v with
  | None -> 0x0000l
  | Flatten -> 0x0001l
  | DontFlatten -> 0x0002l;;
let value_of_loop_control (v : loop_control) =
  match v with
  | None -> 0x0000l
  | Unroll -> 0x0001l
  | DontUnroll -> 0x0002l
  | DependencyInfinite -> 0x0004l
  | DependencyLength -> 0x0008l;;
let value_of_function_control (v : function_control) =
  match v with
  | None -> 0x0000l
  | Inline -> 0x0001l
  | DontInline -> 0x0002l
  | Pure -> 0x0004l
  | Const -> 0x0008l;;
let value_of_memory_semantics (v : memory_semantics) =
  match v with
  | Relaxed -> 0x0000l
  | None -> 0x0000l
  | Acquire -> 0x0002l
  | Release -> 0x0004l
  | AcquireRelease -> 0x0008l
  | SequentiallyConsistent -> 0x0010l
  | UniformMemory -> 0x0040l
  | SubgroupMemory -> 0x0080l
  | WorkgroupMemory -> 0x0100l
  | CrossWorkgroupMemory -> 0x0200l
  | AtomicCounterMemory -> 0x0400l
  | ImageMemory -> 0x0800l;;
let value_of_memory_access (v : memory_access) =
  match v with
  | None -> 0x0000l
  | Volatile -> 0x0001l
  | Aligned -> 0x0002l
  | Nontemporal -> 0x0004l;;
let value_of_kernel_profiling_info (v : kernel_profiling_info) =
  match v with | None -> 0x0000l | CmdExecTime -> 0x0001l;;
let value_of_source_language (v : source_language) =
  match v with
  | Unknown -> 0l
  | ESSL -> 1l
  | GLSL -> 2l
  | OpenCL_C -> 3l
  | OpenCL_CPP -> 4l;;
let value_of_execution_model (v : execution_model) =
  match v with
  | Vertex -> 0l
  | TessellationControl -> 1l
  | TessellationEvaluation -> 2l
  | Geometry -> 3l
  | Fragment -> 4l
  | GLCompute -> 5l
  | Kernel -> 6l;;
let value_of_addressing_model (v : addressing_model) =
  match v with | Logical -> 0l | Physical32 -> 1l | Physical64 -> 2l;;
let value_of_memory_model (v : memory_model) =
  match v with | Simple -> 0l | GLSL450 -> 1l | OpenCL -> 2l;;
let value_of_execution_mode (v : execution_mode) =
  match v with
  | Invocations -> 0l
  | SpacingEqual -> 1l
  | SpacingFractionalEven -> 2l
  | SpacingFractionalOdd -> 3l
  | VertexOrderCw -> 4l
  | VertexOrderCcw -> 5l
  | PixelCenterInteger -> 6l
  | OriginUpperLeft -> 7l
  | OriginLowerLeft -> 8l
  | EarlyFragmentTests -> 9l
  | PointMode -> 10l
  | Xfb -> 11l
  | DepthReplacing -> 12l
  | DepthGreater -> 14l
  | DepthLess -> 15l
  | DepthUnchanged -> 16l
  | LocalSize -> 17l
  | LocalSizeHint -> 18l
  | InputPoints -> 19l
  | InputLines -> 20l
  | InputLinesAdjacency -> 21l
  | Triangles -> 22l
  | InputTrianglesAdjacency -> 23l
  | Quads -> 24l
  | Isolines -> 25l
  | OutputVertices -> 26l
  | OutputPoints -> 27l
  | OutputLineStrip -> 28l
  | OutputTriangleStrip -> 29l
  | VecTypeHint -> 30l
  | ContractionOff -> 31l
  | Initializer -> 33l
  | Finalizer -> 34l
  | SubgroupSize -> 35l
  | SubgroupsPerWorkgroup -> 36l;;
let value_of_storage_class (v : storage_class) =
  match v with
  | UniformConstant -> 0l
  | Input -> 1l
  | Uniform -> 2l
  | Output -> 3l
  | Workgroup -> 4l
  | CrossWorkgroup -> 5l
  | Private -> 6l
  | Function -> 7l
  | Generic -> 8l
  | PushConstant -> 9l
  | AtomicCounter -> 10l
  | Image -> 11l;;
let value_of_dim (v : dim) =
  match v with
  | Dim1D -> 0l
  | Dim2D -> 1l
  | Dim3D -> 2l
  | Cube -> 3l
  | Rect -> 4l
  | Buffer -> 5l
  | SubpassData -> 6l;;
let value_of_sampler_addressing_mode (v : sampler_addressing_mode) =
  match v with
  | None -> 0l
  | ClampToEdge -> 1l
  | Clamp -> 2l
  | Repeat -> 3l
  | RepeatMirrored -> 4l;;
let value_of_sampler_filter_mode (v : sampler_filter_mode) =
  match v with | Nearest -> 0l | Linear -> 1l;;
let value_of_image_format (v : image_format) =
  match v with
  | Unknown -> 0l
  | Rgba32f -> 1l
  | Rgba16f -> 2l
  | R32f -> 3l
  | Rgba8 -> 4l
  | Rgba8Snorm -> 5l
  | Rg32f -> 6l
  | Rg16f -> 7l
  | R11fG11fB10f -> 8l
  | R16f -> 9l
  | Rgba16 -> 10l
  | Rgb10A2 -> 11l
  | Rg16 -> 12l
  | Rg8 -> 13l
  | R16 -> 14l
  | R8 -> 15l
  | Rgba16Snorm -> 16l
  | Rg16Snorm -> 17l
  | Rg8Snorm -> 18l
  | R16Snorm -> 19l
  | R8Snorm -> 20l
  | Rgba32i -> 21l
  | Rgba16i -> 22l
  | Rgba8i -> 23l
  | R32i -> 24l
  | Rg32i -> 25l
  | Rg16i -> 26l
  | Rg8i -> 27l
  | R16i -> 28l
  | R8i -> 29l
  | Rgba32ui -> 30l
  | Rgba16ui -> 31l
  | Rgba8ui -> 32l
  | R32ui -> 33l
  | Rgb10a2ui -> 34l
  | Rg32ui -> 35l
  | Rg16ui -> 36l
  | Rg8ui -> 37l
  | R16ui -> 38l
  | R8ui -> 39l;;
let value_of_image_channel_order (v : image_channel_order) =
  match v with
  | R -> 0l
  | A -> 1l
  | RG -> 2l
  | RA -> 3l
  | RGB -> 4l
  | RGBA -> 5l
  | BGRA -> 6l
  | ARGB -> 7l
  | Intensity -> 8l
  | Luminance -> 9l
  | Rx -> 10l
  | RGx -> 11l
  | RGBx -> 12l
  | Depth -> 13l
  | DepthStencil -> 14l
  | SRGB -> 15l
  | SRGBx -> 16l
  | SRGBA -> 17l
  | SBGRA -> 18l
  | ABGR -> 19l;;
let value_of_image_channel_data_type (v : image_channel_data_type) =
  match v with
  | SnormInt8 -> 0l
  | SnormInt16 -> 1l
  | UnormInt8 -> 2l
  | UnormInt16 -> 3l
  | UnormShort565 -> 4l
  | UnormShort555 -> 5l
  | UnormInt101010 -> 6l
  | SignedInt8 -> 7l
  | SignedInt16 -> 8l
  | SignedInt32 -> 9l
  | UnsignedInt8 -> 10l
  | UnsignedInt16 -> 11l
  | UnsignedInt32 -> 12l
  | HalfFloat -> 13l
  | Float -> 14l
  | UnormInt24 -> 15l
  | UnormInt101010_2 -> 16l;;
let value_of_f_p_rounding_mode (v : f_p_rounding_mode) =
  match v with | RTE -> 0l | RTZ -> 1l | RTP -> 2l | RTN -> 3l;;
let value_of_linkage_type (v : linkage_type) =
  match v with | Export -> 0l | Import -> 1l;;
let value_of_access_qualifier (v : access_qualifier) =
  match v with | ReadOnly -> 0l | WriteOnly -> 1l | ReadWrite -> 2l;;
let value_of_function_parameter_attribute (v : function_parameter_attribute)
                                          =
  match v with
  | Zext -> 0l
  | Sext -> 1l
  | ByVal -> 2l
  | Sret -> 3l
  | NoAlias -> 4l
  | NoCapture -> 5l
  | NoWrite -> 6l
  | NoReadWrite -> 7l;;
let value_of_decoration (v : decoration) =
  match v with
  | RelaxedPrecision -> 0l
  | SpecId -> 1l
  | Block -> 2l
  | BufferBlock -> 3l
  | RowMajor -> 4l
  | ColMajor -> 5l
  | ArrayStride -> 6l
  | MatrixStride -> 7l
  | GLSLShared -> 8l
  | GLSLPacked -> 9l
  | CPacked -> 10l
  | BuiltIn -> 11l
  | NoPerspective -> 13l
  | Flat -> 14l
  | Patch -> 15l
  | Centroid -> 16l
  | Sample -> 17l
  | Invariant -> 18l
  | Restrict -> 19l
  | Aliased -> 20l
  | Volatile -> 21l
  | Constant -> 22l
  | Coherent -> 23l
  | NonWritable -> 24l
  | NonReadable -> 25l
  | Uniform -> 26l
  | SaturatedConversion -> 28l
  | Stream -> 29l
  | Location -> 30l
  | Component -> 31l
  | Index -> 32l
  | Binding -> 33l
  | DescriptorSet -> 34l
  | Offset -> 35l
  | XfbBuffer -> 36l
  | XfbStride -> 37l
  | FuncParamAttr -> 38l
  | FPRoundingMode -> 39l
  | FPFastMathMode -> 40l
  | LinkageAttributes -> 41l
  | NoContraction -> 42l
  | InputAttachmentIndex -> 43l
  | Alignment -> 44l
  | MaxByteOffset -> 45l;;
let value_of_built_in (v : built_in) =
  match v with
  | Position -> 0l
  | PointSize -> 1l
  | ClipDistance -> 3l
  | CullDistance -> 4l
  | VertexId -> 5l
  | InstanceId -> 6l
  | PrimitiveId -> 7l
  | InvocationId -> 8l
  | Layer -> 9l
  | ViewportIndex -> 10l
  | TessLevelOuter -> 11l
  | TessLevelInner -> 12l
  | TessCoord -> 13l
  | PatchVertices -> 14l
  | FragCoord -> 15l
  | PointCoord -> 16l
  | FrontFacing -> 17l
  | SampleId -> 18l
  | SamplePosition -> 19l
  | SampleMask -> 20l
  | FragDepth -> 22l
  | HelperInvocation -> 23l
  | NumWorkgroups -> 24l
  | WorkgroupSize -> 25l
  | WorkgroupId -> 26l
  | LocalInvocationId -> 27l
  | GlobalInvocationId -> 28l
  | LocalInvocationIndex -> 29l
  | WorkDim -> 30l
  | GlobalSize -> 31l
  | EnqueuedWorkgroupSize -> 32l
  | GlobalOffset -> 33l
  | GlobalLinearId -> 34l
  | SubgroupSize -> 36l
  | SubgroupMaxSize -> 37l
  | NumSubgroups -> 38l
  | NumEnqueuedSubgroups -> 39l
  | SubgroupId -> 40l
  | SubgroupLocalInvocationId -> 41l
  | VertexIndex -> 42l
  | InstanceIndex -> 43l
  | SubgroupEqMaskKHR -> 4416l
  | SubgroupGeMaskKHR -> 4417l
  | SubgroupGtMaskKHR -> 4418l
  | SubgroupLeMaskKHR -> 4419l
  | SubgroupLtMaskKHR -> 4420l
  | BaseVertex -> 4424l
  | BaseInstance -> 4425l
  | DrawIndex -> 4426l;;
let value_of_scope (v : scope) =
  match v with
  | CrossDevice -> 0l
  | Device -> 1l
  | Workgroup -> 2l
  | Subgroup -> 3l
  | Invocation -> 4l;;
let value_of_group_operation (v : group_operation) =
  match v with | Reduce -> 0l | InclusiveScan -> 1l | ExclusiveScan -> 2l;;
let value_of_kernel_enqueue_flags (v : kernel_enqueue_flags) =
  match v with | NoWait -> 0l | WaitKernel -> 1l | WaitWorkGroup -> 2l;;
let value_of_capability (v : capability) =
  match v with
  | Matrix -> 0l
  | Shader -> 1l
  | Geometry -> 2l
  | Tessellation -> 3l
  | Addresses -> 4l
  | Linkage -> 5l
  | Kernel -> 6l
  | Vector16 -> 7l
  | Float16Buffer -> 8l
  | Float16 -> 9l
  | Float64 -> 10l
  | Int64 -> 11l
  | Int64Atomics -> 12l
  | ImageBasic -> 13l
  | ImageReadWrite -> 14l
  | ImageMipmap -> 15l
  | Pipes -> 17l
  | Groups -> 18l
  | DeviceEnqueue -> 19l
  | LiteralSampler -> 20l
  | AtomicStorage -> 21l
  | Int16 -> 22l
  | TessellationPointSize -> 23l
  | GeometryPointSize -> 24l
  | ImageGatherExtended -> 25l
  | StorageImageMultisample -> 27l
  | UniformBufferArrayDynamicIndexing -> 28l
  | SampledImageArrayDynamicIndexing -> 29l
  | StorageBufferArrayDynamicIndexing -> 30l
  | StorageImageArrayDynamicIndexing -> 31l
  | ClipDistance -> 32l
  | CullDistance -> 33l
  | ImageCubeArray -> 34l
  | SampleRateShading -> 35l
  | ImageRect -> 36l
  | SampledRect -> 37l
  | GenericPointer -> 38l
  | Int8 -> 39l
  | InputAttachment -> 40l
  | SparseResidency -> 41l
  | MinLod -> 42l
  | Sampled1D -> 43l
  | Image1D -> 44l
  | SampledCubeArray -> 45l
  | SampledBuffer -> 46l
  | ImageBuffer -> 47l
  | ImageMSArray -> 48l
  | StorageImageExtendedFormats -> 49l
  | ImageQuery -> 50l
  | DerivativeControl -> 51l
  | InterpolationFunction -> 52l
  | TransformFeedback -> 53l
  | GeometryStreams -> 54l
  | StorageImageReadWithoutFormat -> 55l
  | StorageImageWriteWithoutFormat -> 56l
  | MultiViewport -> 57l
  | SubgroupDispatch -> 58l
  | NamedBarrier -> 59l
  | PipeStorage -> 60l
  | SubgroupBallotKHR -> 4423l
  | DrawParameters -> 4427l;;
let word_of_int (i : int32) = i;;
let word_of_id (id : id) =
  if id < 0l then failwith "spirv ids must be positive" else id;;
let words_of_string (str : string) =
  let len = String.length str in
  let word_count = len / 4 in
  let buffer = Array.make word_count 0l in
  let add_char_to_word ch offset word =
    Int32.logor word
      (Int32.shift_left (Int32.of_int @@ (Char.code ch)) (offset * 4)) in
  let rec add_char_to_buffer i =
    if i = len
    then ()
    else
      (buffer.(i / 4) <-
         add_char_to_word (String.get str i) (i mod 4) buffer.(i / 4);
       add_char_to_buffer (i + 1))
  in (add_char_to_buffer 0; Array.to_list buffer);;
let words_of_pair_literal_integer_id_ref (n, i) =
  [ word_of_int n; word_of_id i ];;
let words_of_pair_id_ref__literal_integer (i, n) =
  [ word_of_id i; word_of_int n ];;
let words_of_pair_id_ref_id_ref (a, b) = [ word_of_id a; word_of_id b ];;
let words_of_op (size_map : int IdMap.t) (op : op) =
  let list_of_option (opt : 'a option) =
    match opt with | Some v -> [ v ] | None -> [] in
  let apply_option (fn : 'a -> 'b) (opt : 'a option) =
    match opt with | Some v -> Some (fn v) | None -> None in
  let lookup_size (id : id) =
    if IdMap.mem id size_map
    then IdMap.find id size_map
    else raise (Id_not_found id)
  in
    match op with
    | `OpNop -> [ 0x0000l ]
    | `OpUndef (a, b) -> [ 0x0001l; word_of_id a; word_of_id b ]
    | `OpSourceContinued a -> [ 0x0002l ] @ (words_of_string a)
    | `OpSource (a, b, c, d) ->
        ([ 0x0003l; value_of_source_language a; word_of_int b ] @
           (list_of_option (apply_option word_of_id c)))
          @ (list_of_option (apply_option words_of_string d))
    | `OpSourceExtension a -> [ 0x0004l ] @ (words_of_string a)
    | `OpName (a, b) -> [ 0x0005l; word_of_id a ] @ (words_of_string b)
    | `OpMemberName (a, b, c) ->
        [ 0x0006l; word_of_id a; word_of_int b ] @ (words_of_string c)
    | `OpString (a, b) -> [ 0x0007l; word_of_id a ] @ (words_of_string b)
    | `OpLine (a, b, c) ->
        [ 0x0008l; word_of_id a; word_of_int b; word_of_int c ]
    | `OpExtension a -> [ 0x000al ] @ (words_of_string a)
    | `OpExtInstImport (a, b) ->
        [ 0x000bl; word_of_id a ] @ (words_of_string b)
    | `OpExtInst (a, b, c, d, e) ->
        ([ 0x000cl; word_of_id a; word_of_id b; word_of_id c ] @ (todo d)) @
          (List.map word_of_id e)
    | `OpMemoryModel (a, b) ->
        [ 0x000el; value_of_addressing_model a; value_of_memory_model b ]
    | `OpEntryPoint (a, b, c, d) ->
        ([ 0x000fl; value_of_execution_model a; word_of_id b ] @
           (words_of_string c))
          @ (List.map word_of_id d)
    | `OpExecutionMode (a, b) ->
        [ 0x0010l; word_of_id a; value_of_execution_mode b ]
    | `OpCapability a -> [ 0x0011l; value_of_capability a ]
    | `OpTypeVoid a -> [ 0x0013l; word_of_id a ]
    | `OpTypeBool a -> [ 0x0014l; word_of_id a ]
    | `OpTypeInt (a, b, c) ->
        [ 0x0015l; word_of_id a; word_of_int b; word_of_int c ]
    | `OpTypeFloat (a, b) -> [ 0x0016l; word_of_id a; word_of_int b ]
    | `OpTypeVector (a, b, c) ->
        [ 0x0017l; word_of_id a; word_of_id b; word_of_int c ]
    | `OpTypeMatrix (a, b, c) ->
        [ 0x0018l; word_of_id a; word_of_id b; word_of_int c ]
    | `OpTypeImage (a, b, c, d, e, f, g, h, i) ->
        [ 0x0019l; word_of_id a; word_of_id b; value_of_dim c; word_of_int d;
          word_of_int e; word_of_int f; word_of_int g;
          value_of_image_format h ] @
          (list_of_option (apply_option value_of_access_qualifier i))
    | `OpTypeSampler a -> [ 0x001al; word_of_id a ]
    | `OpTypeSampledImage (a, b) -> [ 0x001bl; word_of_id a; word_of_id b ]
    | `OpTypeArray (a, b, c) ->
        [ 0x001cl; word_of_id a; word_of_id b; word_of_id c ]
    | `OpTypeRuntimeArray (a, b) -> [ 0x001dl; word_of_id a; word_of_id b ]
    | `OpTypeStruct (a, b) ->
        [ 0x001el; word_of_id a ] @ (List.map word_of_id b)
    | `OpTypeOpaque (a, b) -> [ 0x001fl; word_of_id a ] @ (words_of_string b)
    | `OpTypePointer (a, b, c) ->
        [ 0x0020l; word_of_id a; value_of_storage_class b; word_of_id c ]
    | `OpTypeFunction (a, b, c) ->
        [ 0x0021l; word_of_id a; word_of_id b ] @ (List.map word_of_id c)
    | `OpTypeEvent a -> [ 0x0022l; word_of_id a ]
    | `OpTypeDeviceEvent a -> [ 0x0023l; word_of_id a ]
    | `OpTypeReserveId a -> [ 0x0024l; word_of_id a ]
    | `OpTypeQueue a -> [ 0x0025l; word_of_id a ]
    | `OpTypePipe (a, b) ->
        [ 0x0026l; word_of_id a; value_of_access_qualifier b ]
    | `OpTypeForwardPointer (a, b) ->
        [ 0x0027l; word_of_id a; value_of_storage_class b ]
    | `OpConstantTrue (a, b) -> [ 0x0029l; word_of_id a; word_of_id b ]
    | `OpConstantFalse (a, b) -> [ 0x002al; word_of_id a; word_of_id b ]
    | `OpConstant (a, b, c) ->
        [ 0x002bl; word_of_id a; word_of_id b ] @
          (words_of_sized_int (lookup_size a) c)
    | `OpConstantComposite (a, b, c) ->
        [ 0x002cl; word_of_id a; word_of_id b ] @ (List.map word_of_id c)
    | `OpConstantSampler (a, b, c, d, e) ->
        [ 0x002dl; word_of_id a; word_of_id b;
          value_of_sampler_addressing_mode c; word_of_int d;
          value_of_sampler_filter_mode e ]
    | `OpConstantNull (a, b) -> [ 0x002el; word_of_id a; word_of_id b ]
    | `OpSpecConstantTrue (a, b) -> [ 0x0030l; word_of_id a; word_of_id b ]
    | `OpSpecConstantFalse (a, b) -> [ 0x0031l; word_of_id a; word_of_id b ]
    | `OpSpecConstant (a, b, c) ->
        [ 0x0032l; word_of_id a; word_of_id b ] @
          (words_of_sized_int (lookup_size a) c)
    | `OpSpecConstantComposite (a, b, c) ->
        [ 0x0033l; word_of_id a; word_of_id b ] @ (List.map word_of_id c)
    | `OpSpecConstantOp (a, b, c) ->
        [ 0x0034l; word_of_id a; word_of_id b ] @ (todo c)
    | `OpFunction (a, b, c, d) ->
        [ 0x0036l; word_of_id a; word_of_id b; value_of_function_control c;
          word_of_id d ]
    | `OpFunctionParameter (a, b) -> [ 0x0037l; word_of_id a; word_of_id b ]
    | `OpFunctionEnd -> [ 0x0038l ]
    | `OpFunctionCall (a, b, c, d) ->
        [ 0x0039l; word_of_id a; word_of_id b; word_of_id c ] @
          (List.map word_of_id d)
    | `OpVariable (a, b, c, d) ->
        [ 0x003bl; word_of_id a; word_of_id b; value_of_storage_class c ] @
          (list_of_option (apply_option word_of_id d))
    | `OpImageTexelPointer (a, b, c, d, e) ->
        [ 0x003cl; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ]
    | `OpLoad (a, b, c, d) ->
        [ 0x003dl; word_of_id a; word_of_id b; word_of_id c ] @
          (list_of_option (apply_option value_of_memory_access d))
    | `OpStore (a, b, c) ->
        [ 0x003el; word_of_id a; word_of_id b ] @
          (list_of_option (apply_option value_of_memory_access c))
    | `OpCopyMemory (a, b, c) ->
        [ 0x003fl; word_of_id a; word_of_id b ] @
          (list_of_option (apply_option value_of_memory_access c))
    | `OpCopyMemorySized (a, b, c, d) ->
        [ 0x0040l; word_of_id a; word_of_id b; word_of_id c ] @
          (list_of_option (apply_option value_of_memory_access d))
    | `OpAccessChain (a, b, c, d) ->
        [ 0x0041l; word_of_id a; word_of_id b; word_of_id c ] @
          (List.map word_of_id d)
    | `OpInBoundsAccessChain (a, b, c, d) ->
        [ 0x0042l; word_of_id a; word_of_id b; word_of_id c ] @
          (List.map word_of_id d)
    | `OpPtrAccessChain (a, b, c, d, e) ->
        [ 0x0043l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
          (List.map word_of_id e)
    | `OpArrayLength (a, b, c, d) ->
        [ 0x0044l; word_of_id a; word_of_id b; word_of_id c; word_of_int d ]
    | `OpGenericPtrMemSemantics (a, b, c) ->
        [ 0x0045l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpInBoundsPtrAccessChain (a, b, c, d, e) ->
        [ 0x0046l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
          (List.map word_of_id e)
    | `OpDecorate (a, b) -> [ 0x0047l; word_of_id a; value_of_decoration b ]
    | `OpMemberDecorate (a, b, c) ->
        [ 0x0048l; word_of_id a; word_of_int b; value_of_decoration c ]
    | `OpDecorationGroup a -> [ 0x0049l; word_of_id a ]
    | `OpGroupDecorate (a, b) ->
        [ 0x004al; word_of_id a ] @ (List.map word_of_id b)
    | `OpGroupMemberDecorate (a, b) ->
        [ 0x004bl; word_of_id a ] @
          (List.map words_of_pair_id_ref_literal_integer b)
    | `OpVectorExtractDynamic (a, b, c, d) ->
        [ 0x004dl; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpVectorInsertDynamic (a, b, c, d, e) ->
        [ 0x004el; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ]
    | `OpVectorShuffle (a, b, c, d, e) ->
        [ 0x004fl; word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
          (List.map word_of_int e)
    | `OpCompositeConstruct (a, b, c) ->
        [ 0x0050l; word_of_id a; word_of_id b ] @ (List.map word_of_id c)
    | `OpCompositeExtract (a, b, c, d) ->
        [ 0x0051l; word_of_id a; word_of_id b; word_of_id c ] @
          (List.map word_of_int d)
    | `OpCompositeInsert (a, b, c, d, e) ->
        [ 0x0052l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
          (List.map word_of_int e)
    | `OpCopyObject (a, b, c) ->
        [ 0x0053l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpTranspose (a, b, c) ->
        [ 0x0054l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpSampledImage (a, b, c, d) ->
        [ 0x0056l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpImageSampleImplicitLod (a, b, c, d, e) ->
        [ 0x0057l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
          (list_of_option (apply_option value_of_image_operands e))
    | `OpImageSampleExplicitLod (a, b, c, d, e) ->
        [ 0x0058l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          value_of_image_operands e ]
    | `OpImageSampleDrefImplicitLod (a, b, c, d, e, f) ->
        [ 0x0059l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ] @
          (list_of_option (apply_option value_of_image_operands f))
    | `OpImageSampleDrefExplicitLod (a, b, c, d, e, f) ->
        [ 0x005al; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; value_of_image_operands f ]
    | `OpImageSampleProjImplicitLod (a, b, c, d, e) ->
        [ 0x005bl; word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
          (list_of_option (apply_option value_of_image_operands e))
    | `OpImageSampleProjExplicitLod (a, b, c, d, e) ->
        [ 0x005cl; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          value_of_image_operands e ]
    | `OpImageSampleProjDrefImplicitLod (a, b, c, d, e, f) ->
        [ 0x005dl; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ] @
          (list_of_option (apply_option value_of_image_operands f))
    | `OpImageSampleProjDrefExplicitLod (a, b, c, d, e, f) ->
        [ 0x005el; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; value_of_image_operands f ]
    | `OpImageFetch (a, b, c, d, e) ->
        [ 0x005fl; word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
          (list_of_option (apply_option value_of_image_operands e))
    | `OpImageGather (a, b, c, d, e, f) ->
        [ 0x0060l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ] @
          (list_of_option (apply_option value_of_image_operands f))
    | `OpImageDrefGather (a, b, c, d, e, f) ->
        [ 0x0061l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ] @
          (list_of_option (apply_option value_of_image_operands f))
    | `OpImageRead (a, b, c, d, e) ->
        [ 0x0062l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
          (list_of_option (apply_option value_of_image_operands e))
    | `OpImageWrite (a, b, c, d) ->
        [ 0x0063l; word_of_id a; word_of_id b; word_of_id c ] @
          (list_of_option (apply_option value_of_image_operands d))
    | `OpImage (a, b, c) ->
        [ 0x0064l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpImageQueryFormat (a, b, c) ->
        [ 0x0065l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpImageQueryOrder (a, b, c) ->
        [ 0x0066l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpImageQuerySizeLod (a, b, c, d) ->
        [ 0x0067l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpImageQuerySize (a, b, c) ->
        [ 0x0068l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpImageQueryLod (a, b, c, d) ->
        [ 0x0069l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpImageQueryLevels (a, b, c) ->
        [ 0x006al; word_of_id a; word_of_id b; word_of_id c ]
    | `OpImageQuerySamples (a, b, c) ->
        [ 0x006bl; word_of_id a; word_of_id b; word_of_id c ]
    | `OpConvertFToU (a, b, c) ->
        [ 0x006dl; word_of_id a; word_of_id b; word_of_id c ]
    | `OpConvertFToS (a, b, c) ->
        [ 0x006el; word_of_id a; word_of_id b; word_of_id c ]
    | `OpConvertSToF (a, b, c) ->
        [ 0x006fl; word_of_id a; word_of_id b; word_of_id c ]
    | `OpConvertUToF (a, b, c) ->
        [ 0x0070l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpUConvert (a, b, c) ->
        [ 0x0071l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpSConvert (a, b, c) ->
        [ 0x0072l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpFConvert (a, b, c) ->
        [ 0x0073l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpQuantizeToF16 (a, b, c) ->
        [ 0x0074l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpConvertPtrToU (a, b, c) ->
        [ 0x0075l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpSatConvertSToU (a, b, c) ->
        [ 0x0076l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpSatConvertUToS (a, b, c) ->
        [ 0x0077l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpConvertUToPtr (a, b, c) ->
        [ 0x0078l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpPtrCastToGeneric (a, b, c) ->
        [ 0x0079l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpGenericCastToPtr (a, b, c) ->
        [ 0x007al; word_of_id a; word_of_id b; word_of_id c ]
    | `OpGenericCastToPtrExplicit (a, b, c, d) ->
        [ 0x007bl; word_of_id a; word_of_id b; word_of_id c;
          value_of_storage_class d ]
    | `OpBitcast (a, b, c) ->
        [ 0x007cl; word_of_id a; word_of_id b; word_of_id c ]
    | `OpSNegate (a, b, c) ->
        [ 0x007el; word_of_id a; word_of_id b; word_of_id c ]
    | `OpFNegate (a, b, c) ->
        [ 0x007fl; word_of_id a; word_of_id b; word_of_id c ]
    | `OpIAdd (a, b, c, d) ->
        [ 0x0080l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpFAdd (a, b, c, d) ->
        [ 0x0081l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpISub (a, b, c, d) ->
        [ 0x0082l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpFSub (a, b, c, d) ->
        [ 0x0083l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpIMul (a, b, c, d) ->
        [ 0x0084l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpFMul (a, b, c, d) ->
        [ 0x0085l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpUDiv (a, b, c, d) ->
        [ 0x0086l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpSDiv (a, b, c, d) ->
        [ 0x0087l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpFDiv (a, b, c, d) ->
        [ 0x0088l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpUMod (a, b, c, d) ->
        [ 0x0089l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpSRem (a, b, c, d) ->
        [ 0x008al; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpSMod (a, b, c, d) ->
        [ 0x008bl; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpFRem (a, b, c, d) ->
        [ 0x008cl; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpFMod (a, b, c, d) ->
        [ 0x008dl; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpVectorTimesScalar (a, b, c, d) ->
        [ 0x008el; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpMatrixTimesScalar (a, b, c, d) ->
        [ 0x008fl; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpVectorTimesMatrix (a, b, c, d) ->
        [ 0x0090l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpMatrixTimesVector (a, b, c, d) ->
        [ 0x0091l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpMatrixTimesMatrix (a, b, c, d) ->
        [ 0x0092l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpOuterProduct (a, b, c, d) ->
        [ 0x0093l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpDot (a, b, c, d) ->
        [ 0x0094l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpIAddCarry (a, b, c, d) ->
        [ 0x0095l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpISubBorrow (a, b, c, d) ->
        [ 0x0096l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpUMulExtended (a, b, c, d) ->
        [ 0x0097l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpSMulExtended (a, b, c, d) ->
        [ 0x0098l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpAny (a, b, c) ->
        [ 0x009al; word_of_id a; word_of_id b; word_of_id c ]
    | `OpAll (a, b, c) ->
        [ 0x009bl; word_of_id a; word_of_id b; word_of_id c ]
    | `OpIsNan (a, b, c) ->
        [ 0x009cl; word_of_id a; word_of_id b; word_of_id c ]
    | `OpIsInf (a, b, c) ->
        [ 0x009dl; word_of_id a; word_of_id b; word_of_id c ]
    | `OpIsFinite (a, b, c) ->
        [ 0x009el; word_of_id a; word_of_id b; word_of_id c ]
    | `OpIsNormal (a, b, c) ->
        [ 0x009fl; word_of_id a; word_of_id b; word_of_id c ]
    | `OpSignBitSet (a, b, c) ->
        [ 0x00a0l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpLessOrGreater (a, b, c, d) ->
        [ 0x00a1l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpOrdered (a, b, c, d) ->
        [ 0x00a2l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpUnordered (a, b, c, d) ->
        [ 0x00a3l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpLogicalEqual (a, b, c, d) ->
        [ 0x00a4l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpLogicalNotEqual (a, b, c, d) ->
        [ 0x00a5l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpLogicalOr (a, b, c, d) ->
        [ 0x00a6l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpLogicalAnd (a, b, c, d) ->
        [ 0x00a7l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpLogicalNot (a, b, c) ->
        [ 0x00a8l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpSelect (a, b, c, d, e) ->
        [ 0x00a9l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ]
    | `OpIEqual (a, b, c, d) ->
        [ 0x00aal; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpINotEqual (a, b, c, d) ->
        [ 0x00abl; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpUGreaterThan (a, b, c, d) ->
        [ 0x00acl; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpSGreaterThan (a, b, c, d) ->
        [ 0x00adl; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpUGreaterThanEqual (a, b, c, d) ->
        [ 0x00ael; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpSGreaterThanEqual (a, b, c, d) ->
        [ 0x00afl; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpULessThan (a, b, c, d) ->
        [ 0x00b0l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpSLessThan (a, b, c, d) ->
        [ 0x00b1l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpULessThanEqual (a, b, c, d) ->
        [ 0x00b2l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpSLessThanEqual (a, b, c, d) ->
        [ 0x00b3l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpFOrdEqual (a, b, c, d) ->
        [ 0x00b4l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpFUnordEqual (a, b, c, d) ->
        [ 0x00b5l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpFOrdNotEqual (a, b, c, d) ->
        [ 0x00b6l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpFUnordNotEqual (a, b, c, d) ->
        [ 0x00b7l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpFOrdLessThan (a, b, c, d) ->
        [ 0x00b8l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpFUnordLessThan (a, b, c, d) ->
        [ 0x00b9l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpFOrdGreaterThan (a, b, c, d) ->
        [ 0x00bal; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpFUnordGreaterThan (a, b, c, d) ->
        [ 0x00bbl; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpFOrdLessThanEqual (a, b, c, d) ->
        [ 0x00bcl; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpFUnordLessThanEqual (a, b, c, d) ->
        [ 0x00bdl; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpFOrdGreaterThanEqual (a, b, c, d) ->
        [ 0x00bel; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpFUnordGreaterThanEqual (a, b, c, d) ->
        [ 0x00bfl; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpShiftRightLogical (a, b, c, d) ->
        [ 0x00c2l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpShiftRightArithmetic (a, b, c, d) ->
        [ 0x00c3l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpShiftLeftLogical (a, b, c, d) ->
        [ 0x00c4l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpBitwiseOr (a, b, c, d) ->
        [ 0x00c5l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpBitwiseXor (a, b, c, d) ->
        [ 0x00c6l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpBitwiseAnd (a, b, c, d) ->
        [ 0x00c7l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpNot (a, b, c) ->
        [ 0x00c8l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpBitFieldInsert (a, b, c, d, e, f) ->
        [ 0x00c9l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f ]
    | `OpBitFieldSExtract (a, b, c, d, e) ->
        [ 0x00cal; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ]
    | `OpBitFieldUExtract (a, b, c, d, e) ->
        [ 0x00cbl; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ]
    | `OpBitReverse (a, b, c) ->
        [ 0x00ccl; word_of_id a; word_of_id b; word_of_id c ]
    | `OpBitCount (a, b, c) ->
        [ 0x00cdl; word_of_id a; word_of_id b; word_of_id c ]
    | `OpDPdx (a, b, c) ->
        [ 0x00cfl; word_of_id a; word_of_id b; word_of_id c ]
    | `OpDPdy (a, b, c) ->
        [ 0x00d0l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpFwidth (a, b, c) ->
        [ 0x00d1l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpDPdxFine (a, b, c) ->
        [ 0x00d2l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpDPdyFine (a, b, c) ->
        [ 0x00d3l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpFwidthFine (a, b, c) ->
        [ 0x00d4l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpDPdxCoarse (a, b, c) ->
        [ 0x00d5l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpDPdyCoarse (a, b, c) ->
        [ 0x00d6l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpFwidthCoarse (a, b, c) ->
        [ 0x00d7l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpEmitVertex -> [ 0x00dal ]
    | `OpEndPrimitive -> [ 0x00dbl ]
    | `OpEmitStreamVertex a -> [ 0x00dcl; word_of_id a ]
    | `OpEndStreamPrimitive a -> [ 0x00ddl; word_of_id a ]
    | `OpControlBarrier (a, b, c) ->
        [ 0x00e0l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpMemoryBarrier (a, b) -> [ 0x00e1l; word_of_id a; word_of_id b ]
    | `OpAtomicLoad (a, b, c, d, e) ->
        [ 0x00e3l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ]
    | `OpAtomicStore (a, b, c, d) ->
        [ 0x00e4l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpAtomicExchange (a, b, c, d, e, f) ->
        [ 0x00e5l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f ]
    | `OpAtomicCompareExchange (a, b, c, d, e, f, g, h) ->
        [ 0x00e6l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f; word_of_id g; word_of_id h ]
    | `OpAtomicCompareExchangeWeak (a, b, c, d, e, f, g, h) ->
        [ 0x00e7l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f; word_of_id g; word_of_id h ]
    | `OpAtomicIIncrement (a, b, c, d, e) ->
        [ 0x00e8l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ]
    | `OpAtomicIDecrement (a, b, c, d, e) ->
        [ 0x00e9l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ]
    | `OpAtomicIAdd (a, b, c, d, e, f) ->
        [ 0x00eal; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f ]
    | `OpAtomicISub (a, b, c, d, e, f) ->
        [ 0x00ebl; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f ]
    | `OpAtomicSMin (a, b, c, d, e, f) ->
        [ 0x00ecl; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f ]
    | `OpAtomicUMin (a, b, c, d, e, f) ->
        [ 0x00edl; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f ]
    | `OpAtomicSMax (a, b, c, d, e, f) ->
        [ 0x00eel; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f ]
    | `OpAtomicUMax (a, b, c, d, e, f) ->
        [ 0x00efl; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f ]
    | `OpAtomicAnd (a, b, c, d, e, f) ->
        [ 0x00f0l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f ]
    | `OpAtomicOr (a, b, c, d, e, f) ->
        [ 0x00f1l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f ]
    | `OpAtomicXor (a, b, c, d, e, f) ->
        [ 0x00f2l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f ]
    | `OpPhi (a, b, c) ->
        [ 0x00f5l; word_of_id a; word_of_id b ] @
          (List.map words_of_pair_id_ref_id_ref c)
    | `OpLoopMerge (a, b, c) ->
        [ 0x00f6l; word_of_id a; word_of_id b; value_of_loop_control c ]
    | `OpSelectionMerge (a, b) ->
        [ 0x00f7l; word_of_id a; value_of_selection_control b ]
    | `OpLabel a -> [ 0x00f8l; word_of_id a ]
    | `OpBranch a -> [ 0x00f9l; word_of_id a ]
    | `OpBranchConditional (a, b, c, d) ->
        [ 0x00fal; word_of_id a; word_of_id b; word_of_id c ] @
          (List.map word_of_int d)
    | `OpSwitch (a, b, c) ->
        [ 0x00fbl; word_of_id a; word_of_id b ] @
          (List.map words_of_pair_literal_integer_id_ref c)
    | `OpKill -> [ 0x00fcl ]
    | `OpReturn -> [ 0x00fdl ]
    | `OpReturnValue a -> [ 0x00fel; word_of_id a ]
    | `OpUnreachable -> [ 0x00ffl ]
    | `OpLifetimeStart (a, b) -> [ 0x0100l; word_of_id a; word_of_int b ]
    | `OpLifetimeStop (a, b) -> [ 0x0101l; word_of_id a; word_of_int b ]
    | `OpGroupAsyncCopy (a, b, c, d, e, f, g, h) ->
        [ 0x0103l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f; word_of_id g; word_of_id h ]
    | `OpGroupWaitEvents (a, b, c) ->
        [ 0x0104l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpGroupAll (a, b, c, d) ->
        [ 0x0105l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpGroupAny (a, b, c, d) ->
        [ 0x0106l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpGroupBroadcast (a, b, c, d, e) ->
        [ 0x0107l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ]
    | `OpGroupIAdd (a, b, c, d, e) ->
        [ 0x0108l; word_of_id a; word_of_id b; word_of_id c;
          value_of_group_operation d; word_of_id e ]
    | `OpGroupFAdd (a, b, c, d, e) ->
        [ 0x0109l; word_of_id a; word_of_id b; word_of_id c;
          value_of_group_operation d; word_of_id e ]
    | `OpGroupFMin (a, b, c, d, e) ->
        [ 0x010al; word_of_id a; word_of_id b; word_of_id c;
          value_of_group_operation d; word_of_id e ]
    | `OpGroupUMin (a, b, c, d, e) ->
        [ 0x010bl; word_of_id a; word_of_id b; word_of_id c;
          value_of_group_operation d; word_of_id e ]
    | `OpGroupSMin (a, b, c, d, e) ->
        [ 0x010cl; word_of_id a; word_of_id b; word_of_id c;
          value_of_group_operation d; word_of_id e ]
    | `OpGroupFMax (a, b, c, d, e) ->
        [ 0x010dl; word_of_id a; word_of_id b; word_of_id c;
          value_of_group_operation d; word_of_id e ]
    | `OpGroupUMax (a, b, c, d, e) ->
        [ 0x010el; word_of_id a; word_of_id b; word_of_id c;
          value_of_group_operation d; word_of_id e ]
    | `OpGroupSMax (a, b, c, d, e) ->
        [ 0x010fl; word_of_id a; word_of_id b; word_of_id c;
          value_of_group_operation d; word_of_id e ]
    | `OpReadPipe (a, b, c, d, e, f) ->
        [ 0x0112l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f ]
    | `OpWritePipe (a, b, c, d, e, f) ->
        [ 0x0113l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f ]
    | `OpReservedReadPipe (a, b, c, d, e, f, g, h) ->
        [ 0x0114l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f; word_of_id g; word_of_id h ]
    | `OpReservedWritePipe (a, b, c, d, e, f, g, h) ->
        [ 0x0115l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f; word_of_id g; word_of_id h ]
    | `OpReserveReadPipePackets (a, b, c, d, e, f) ->
        [ 0x0116l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f ]
    | `OpReserveWritePipePackets (a, b, c, d, e, f) ->
        [ 0x0117l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f ]
    | `OpCommitReadPipe (a, b, c, d) ->
        [ 0x0118l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpCommitWritePipe (a, b, c, d) ->
        [ 0x0119l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ]
    | `OpIsValidReserveId (a, b, c) ->
        [ 0x011al; word_of_id a; word_of_id b; word_of_id c ]
    | `OpGetNumPipePackets (a, b, c, d, e) ->
        [ 0x011bl; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ]
    | `OpGetMaxPipePackets (a, b, c, d, e) ->
        [ 0x011cl; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ]
    | `OpGroupReserveReadPipePackets (a, b, c, d, e, f, g) ->
        [ 0x011dl; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f; word_of_id g ]
    | `OpGroupReserveWritePipePackets (a, b, c, d, e, f, g) ->
        [ 0x011el; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f; word_of_id g ]
    | `OpGroupCommitReadPipe (a, b, c, d, e) ->
        [ 0x011fl; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ]
    | `OpGroupCommitWritePipe (a, b, c, d, e) ->
        [ 0x0120l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ]
    | `OpEnqueueMarker (a, b, c, d, e, f) ->
        [ 0x0123l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f ]
    | `OpEnqueueKernel (a, b, c, d, e, f, g, h, i, j, k, l, m) ->
        [ 0x0124l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f; word_of_id g; word_of_id h;
          word_of_id i; word_of_id j; word_of_id k; word_of_id l ] @
          (List.map word_of_id m)
    | `OpGetKernelNDrangeSubGroupCount (a, b, c, d, e, f, g) ->
        [ 0x0125l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f; word_of_id g ]
    | `OpGetKernelNDrangeMaxSubGroupSize (a, b, c, d, e, f, g) ->
        [ 0x0126l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f; word_of_id g ]
    | `OpGetKernelWorkGroupSize (a, b, c, d, e, f) ->
        [ 0x0127l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f ]
    | `OpGetKernelPreferredWorkGroupSizeMultiple (a, b, c, d, e, f) ->
        [ 0x0128l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f ]
    | `OpRetainEvent a -> [ 0x0129l; word_of_id a ]
    | `OpReleaseEvent a -> [ 0x012al; word_of_id a ]
    | `OpCreateUserEvent (a, b) -> [ 0x012bl; word_of_id a; word_of_id b ]
    | `OpIsValidEvent (a, b, c) ->
        [ 0x012cl; word_of_id a; word_of_id b; word_of_id c ]
    | `OpSetUserEventStatus (a, b) -> [ 0x012dl; word_of_id a; word_of_id b ]
    | `OpCaptureEventProfilingInfo (a, b, c) ->
        [ 0x012el; word_of_id a; word_of_id b; word_of_id c ]
    | `OpGetDefaultQueue (a, b) -> [ 0x012fl; word_of_id a; word_of_id b ]
    | `OpBuildNDRange (a, b, c, d, e) ->
        [ 0x0130l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ]
    | `OpImageSparseSampleImplicitLod (a, b, c, d, e) ->
        [ 0x0131l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
          (list_of_option (apply_option value_of_image_operands e))
    | `OpImageSparseSampleExplicitLod (a, b, c, d, e) ->
        [ 0x0132l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          value_of_image_operands e ]
    | `OpImageSparseSampleDrefImplicitLod (a, b, c, d, e, f) ->
        [ 0x0133l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ] @
          (list_of_option (apply_option value_of_image_operands f))
    | `OpImageSparseSampleDrefExplicitLod (a, b, c, d, e, f) ->
        [ 0x0134l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; value_of_image_operands f ]
    | `OpImageSparseSampleProjImplicitLod (a, b, c, d, e) ->
        [ 0x0135l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
          (list_of_option (apply_option value_of_image_operands e))
    | `OpImageSparseSampleProjExplicitLod (a, b, c, d, e) ->
        [ 0x0136l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          value_of_image_operands e ]
    | `OpImageSparseSampleProjDrefImplicitLod (a, b, c, d, e, f) ->
        [ 0x0137l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ] @
          (list_of_option (apply_option value_of_image_operands f))
    | `OpImageSparseSampleProjDrefExplicitLod (a, b, c, d, e, f) ->
        [ 0x0138l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; value_of_image_operands f ]
    | `OpImageSparseFetch (a, b, c, d, e) ->
        [ 0x0139l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
          (list_of_option (apply_option value_of_image_operands e))
    | `OpImageSparseGather (a, b, c, d, e, f) ->
        [ 0x013al; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ] @
          (list_of_option (apply_option value_of_image_operands f))
    | `OpImageSparseDrefGather (a, b, c, d, e, f) ->
        [ 0x013bl; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ] @
          (list_of_option (apply_option value_of_image_operands f))
    | `OpImageSparseTexelsResident (a, b, c) ->
        [ 0x013cl; word_of_id a; word_of_id b; word_of_id c ]
    | `OpNoLine -> [ 0x013dl ]
    | `OpAtomicFlagTestAndSet (a, b, c, d, e) ->
        [ 0x013el; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e ]
    | `OpAtomicFlagClear (a, b, c) ->
        [ 0x013fl; word_of_id a; word_of_id b; word_of_id c ]
    | `OpImageSparseRead (a, b, c, d, e) ->
        [ 0x0140l; word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
          (list_of_option (apply_option value_of_image_operands e))
    | `OpSizeOf (a, b, c) ->
        [ 0x0141l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpTypePipeStorage a -> [ 0x0142l; word_of_id a ]
    | `OpConstantPipeStorage (a, b, c, d, e) ->
        [ 0x0143l; word_of_id a; word_of_id b; word_of_int c; word_of_int d;
          word_of_int e ]
    | `OpCreatePipeFromPipeStorage (a, b, c) ->
        [ 0x0144l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpGetKernelLocalSizeForSubgroupCount (a, b, c, d, e, f, g) ->
        [ 0x0145l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f; word_of_id g ]
    | `OpGetKernelMaxNumSubgroups (a, b, c, d, e, f) ->
        [ 0x0146l; word_of_id a; word_of_id b; word_of_id c; word_of_id d;
          word_of_id e; word_of_id f ]
    | `OpTypeNamedBarrier a -> [ 0x0147l; word_of_id a ]
    | `OpNamedBarrierInitialize (a, b, c) ->
        [ 0x0148l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpMemoryNamedBarrier (a, b, c) ->
        [ 0x0149l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpModuleProcessed a -> [ 0x014al ] @ (words_of_string a)
    | `OpSubgroupBallotKHR (a, b, c) ->
        [ 0x1145l; word_of_id a; word_of_id b; word_of_id c ]
    | `OpSubgroupFirstInvocationKHR (a, b, c) ->
        [ 0x1146l; word_of_id a; word_of_id b; word_of_id c ];;
