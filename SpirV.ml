type id = int;;
type id_result_type = id;;
type id_result = id;;
type id_memory_semantics = id;;
type id_scope = id;;
type id_ref = id;;
type literal_integer = int;;
type literal_string = string;;
type literal_context_dependent_number = int;;
type literal_ext_inst_integer = int;;
type literal_spec_constant_op_integer = int;;
type pair_literal_integer_id_ref = (literal_integer * id_ref);;
type pair_id_ref_literal_integer = (id_ref * literal_integer);;
type pair_id_ref_id_ref = (id_ref * id_ref);;
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
let words_of_op op =
  let list_of_list_opt ls_opt =
    match ls_opt with | Some ls -> ls | None -> []
  in
    match op with
    | `OpNop -> [ 0 ]
    | `OpUndef (a, b) -> [ 1; a; b ]
    | `OpSourceContinued a -> [ 2; a ]
    | `OpSource (a, b, c, d) ->
        ( @ ) ((( @ ) ([ 3; a; b ], (list_of_list_opt c))),
          (list_of_list_opt d))
    | `OpSourceExtension a -> [ 4; a ]
    | `OpName (a, b) -> [ 5; a; b ]
    | `OpMemberName (a, b, c) -> [ 6; a; b; c ]
    | `OpString (a, b) -> [ 7; a; b ]
    | `OpLine (a, b, c) -> [ 8; a; b; c ]
    | `OpExtension a -> [ 10; a ]
    | `OpExtInstImport (a, b) -> [ 11; a; b ]
    | `OpExtInst (a, b, c, d, e) -> ( @ ) ([ 12; a; b; c; d ], e)
    | `OpMemoryModel (a, b) -> [ 14; a; b ]
    | `OpEntryPoint (a, b, c, d) -> ( @ ) ([ 15; a; b; c ], d)
    | `OpExecutionMode (a, b) -> [ 16; a; b ]
    | `OpCapability a -> [ 17; a ]
    | `OpTypeVoid a -> [ 19; a ]
    | `OpTypeBool a -> [ 20; a ]
    | `OpTypeInt (a, b, c) -> [ 21; a; b; c ]
    | `OpTypeFloat (a, b) -> [ 22; a; b ]
    | `OpTypeVector (a, b, c) -> [ 23; a; b; c ]
    | `OpTypeMatrix (a, b, c) -> [ 24; a; b; c ]
    | `OpTypeImage (a, b, c, d, e, f, g, h, i) ->
        ( @ ) ([ 25; a; b; c; d; e; f; g; h ], (list_of_list_opt i))
    | `OpTypeSampler a -> [ 26; a ]
    | `OpTypeSampledImage (a, b) -> [ 27; a; b ]
    | `OpTypeArray (a, b, c) -> [ 28; a; b; c ]
    | `OpTypeRuntimeArray (a, b) -> [ 29; a; b ]
    | `OpTypeStruct (a, b) -> ( @ ) ([ 30; a ], b)
    | `OpTypeOpaque (a, b) -> [ 31; a; b ]
    | `OpTypePointer (a, b, c) -> [ 32; a; b; c ]
    | `OpTypeFunction (a, b, c) -> ( @ ) ([ 33; a; b ], c)
    | `OpTypeEvent a -> [ 34; a ]
    | `OpTypeDeviceEvent a -> [ 35; a ]
    | `OpTypeReserveId a -> [ 36; a ]
    | `OpTypeQueue a -> [ 37; a ]
    | `OpTypePipe (a, b) -> [ 38; a; b ]
    | `OpTypeForwardPointer (a, b) -> [ 39; a; b ]
    | `OpConstantTrue (a, b) -> [ 41; a; b ]
    | `OpConstantFalse (a, b) -> [ 42; a; b ]
    | `OpConstant (a, b, c) -> [ 43; a; b; c ]
    | `OpConstantComposite (a, b, c) -> ( @ ) ([ 44; a; b ], c)
    | `OpConstantSampler (a, b, c, d, e) -> [ 45; a; b; c; d; e ]
    | `OpConstantNull (a, b) -> [ 46; a; b ]
    | `OpSpecConstantTrue (a, b) -> [ 48; a; b ]
    | `OpSpecConstantFalse (a, b) -> [ 49; a; b ]
    | `OpSpecConstant (a, b, c) -> [ 50; a; b; c ]
    | `OpSpecConstantComposite (a, b, c) -> ( @ ) ([ 51; a; b ], c)
    | `OpSpecConstantOp (a, b, c) -> [ 52; a; b; c ]
    | `OpFunction (a, b, c, d) -> [ 54; a; b; c; d ]
    | `OpFunctionParameter (a, b) -> [ 55; a; b ]
    | `OpFunctionEnd -> [ 56 ]
    | `OpFunctionCall (a, b, c, d) -> ( @ ) ([ 57; a; b; c ], d)
    | `OpVariable (a, b, c, d) ->
        ( @ ) ([ 59; a; b; c ], (list_of_list_opt d))
    | `OpImageTexelPointer (a, b, c, d, e) -> [ 60; a; b; c; d; e ]
    | `OpLoad (a, b, c, d) -> ( @ ) ([ 61; a; b; c ], (list_of_list_opt d))
    | `OpStore (a, b, c) -> ( @ ) ([ 62; a; b ], (list_of_list_opt c))
    | `OpCopyMemory (a, b, c) -> ( @ ) ([ 63; a; b ], (list_of_list_opt c))
    | `OpCopyMemorySized (a, b, c, d) ->
        ( @ ) ([ 64; a; b; c ], (list_of_list_opt d))
    | `OpAccessChain (a, b, c, d) -> ( @ ) ([ 65; a; b; c ], d)
    | `OpInBoundsAccessChain (a, b, c, d) -> ( @ ) ([ 66; a; b; c ], d)
    | `OpPtrAccessChain (a, b, c, d, e) -> ( @ ) ([ 67; a; b; c; d ], e)
    | `OpArrayLength (a, b, c, d) -> [ 68; a; b; c; d ]
    | `OpGenericPtrMemSemantics (a, b, c) -> [ 69; a; b; c ]
    | `OpInBoundsPtrAccessChain (a, b, c, d, e) ->
        ( @ ) ([ 70; a; b; c; d ], e)
    | `OpDecorate (a, b) -> [ 71; a; b ]
    | `OpMemberDecorate (a, b, c) -> [ 72; a; b; c ]
    | `OpDecorationGroup a -> [ 73; a ]
    | `OpGroupDecorate (a, b) -> ( @ ) ([ 74; a ], b)
    | `OpGroupMemberDecorate (a, b) -> ( @ ) ([ 75; a ], b)
    | `OpVectorExtractDynamic (a, b, c, d) -> [ 77; a; b; c; d ]
    | `OpVectorInsertDynamic (a, b, c, d, e) -> [ 78; a; b; c; d; e ]
    | `OpVectorShuffle (a, b, c, d, e) -> ( @ ) ([ 79; a; b; c; d ], e)
    | `OpCompositeConstruct (a, b, c) -> ( @ ) ([ 80; a; b ], c)
    | `OpCompositeExtract (a, b, c, d) -> ( @ ) ([ 81; a; b; c ], d)
    | `OpCompositeInsert (a, b, c, d, e) -> ( @ ) ([ 82; a; b; c; d ], e)
    | `OpCopyObject (a, b, c) -> [ 83; a; b; c ]
    | `OpTranspose (a, b, c) -> [ 84; a; b; c ]
    | `OpSampledImage (a, b, c, d) -> [ 86; a; b; c; d ]
    | `OpImageSampleImplicitLod (a, b, c, d, e) ->
        ( @ ) ([ 87; a; b; c; d ], (list_of_list_opt e))
    | `OpImageSampleExplicitLod (a, b, c, d, e) -> [ 88; a; b; c; d; e ]
    | `OpImageSampleDrefImplicitLod (a, b, c, d, e, f) ->
        ( @ ) ([ 89; a; b; c; d; e ], (list_of_list_opt f))
    | `OpImageSampleDrefExplicitLod (a, b, c, d, e, f) ->
        [ 90; a; b; c; d; e; f ]
    | `OpImageSampleProjImplicitLod (a, b, c, d, e) ->
        ( @ ) ([ 91; a; b; c; d ], (list_of_list_opt e))
    | `OpImageSampleProjExplicitLod (a, b, c, d, e) -> [ 92; a; b; c; d; e ]
    | `OpImageSampleProjDrefImplicitLod (a, b, c, d, e, f) ->
        ( @ ) ([ 93; a; b; c; d; e ], (list_of_list_opt f))
    | `OpImageSampleProjDrefExplicitLod (a, b, c, d, e, f) ->
        [ 94; a; b; c; d; e; f ]
    | `OpImageFetch (a, b, c, d, e) ->
        ( @ ) ([ 95; a; b; c; d ], (list_of_list_opt e))
    | `OpImageGather (a, b, c, d, e, f) ->
        ( @ ) ([ 96; a; b; c; d; e ], (list_of_list_opt f))
    | `OpImageDrefGather (a, b, c, d, e, f) ->
        ( @ ) ([ 97; a; b; c; d; e ], (list_of_list_opt f))
    | `OpImageRead (a, b, c, d, e) ->
        ( @ ) ([ 98; a; b; c; d ], (list_of_list_opt e))
    | `OpImageWrite (a, b, c, d) ->
        ( @ ) ([ 99; a; b; c ], (list_of_list_opt d))
    | `OpImage (a, b, c) -> [ 100; a; b; c ]
    | `OpImageQueryFormat (a, b, c) -> [ 101; a; b; c ]
    | `OpImageQueryOrder (a, b, c) -> [ 102; a; b; c ]
    | `OpImageQuerySizeLod (a, b, c, d) -> [ 103; a; b; c; d ]
    | `OpImageQuerySize (a, b, c) -> [ 104; a; b; c ]
    | `OpImageQueryLod (a, b, c, d) -> [ 105; a; b; c; d ]
    | `OpImageQueryLevels (a, b, c) -> [ 106; a; b; c ]
    | `OpImageQuerySamples (a, b, c) -> [ 107; a; b; c ]
    | `OpConvertFToU (a, b, c) -> [ 109; a; b; c ]
    | `OpConvertFToS (a, b, c) -> [ 110; a; b; c ]
    | `OpConvertSToF (a, b, c) -> [ 111; a; b; c ]
    | `OpConvertUToF (a, b, c) -> [ 112; a; b; c ]
    | `OpUConvert (a, b, c) -> [ 113; a; b; c ]
    | `OpSConvert (a, b, c) -> [ 114; a; b; c ]
    | `OpFConvert (a, b, c) -> [ 115; a; b; c ]
    | `OpQuantizeToF16 (a, b, c) -> [ 116; a; b; c ]
    | `OpConvertPtrToU (a, b, c) -> [ 117; a; b; c ]
    | `OpSatConvertSToU (a, b, c) -> [ 118; a; b; c ]
    | `OpSatConvertUToS (a, b, c) -> [ 119; a; b; c ]
    | `OpConvertUToPtr (a, b, c) -> [ 120; a; b; c ]
    | `OpPtrCastToGeneric (a, b, c) -> [ 121; a; b; c ]
    | `OpGenericCastToPtr (a, b, c) -> [ 122; a; b; c ]
    | `OpGenericCastToPtrExplicit (a, b, c, d) -> [ 123; a; b; c; d ]
    | `OpBitcast (a, b, c) -> [ 124; a; b; c ]
    | `OpSNegate (a, b, c) -> [ 126; a; b; c ]
    | `OpFNegate (a, b, c) -> [ 127; a; b; c ]
    | `OpIAdd (a, b, c, d) -> [ 128; a; b; c; d ]
    | `OpFAdd (a, b, c, d) -> [ 129; a; b; c; d ]
    | `OpISub (a, b, c, d) -> [ 130; a; b; c; d ]
    | `OpFSub (a, b, c, d) -> [ 131; a; b; c; d ]
    | `OpIMul (a, b, c, d) -> [ 132; a; b; c; d ]
    | `OpFMul (a, b, c, d) -> [ 133; a; b; c; d ]
    | `OpUDiv (a, b, c, d) -> [ 134; a; b; c; d ]
    | `OpSDiv (a, b, c, d) -> [ 135; a; b; c; d ]
    | `OpFDiv (a, b, c, d) -> [ 136; a; b; c; d ]
    | `OpUMod (a, b, c, d) -> [ 137; a; b; c; d ]
    | `OpSRem (a, b, c, d) -> [ 138; a; b; c; d ]
    | `OpSMod (a, b, c, d) -> [ 139; a; b; c; d ]
    | `OpFRem (a, b, c, d) -> [ 140; a; b; c; d ]
    | `OpFMod (a, b, c, d) -> [ 141; a; b; c; d ]
    | `OpVectorTimesScalar (a, b, c, d) -> [ 142; a; b; c; d ]
    | `OpMatrixTimesScalar (a, b, c, d) -> [ 143; a; b; c; d ]
    | `OpVectorTimesMatrix (a, b, c, d) -> [ 144; a; b; c; d ]
    | `OpMatrixTimesVector (a, b, c, d) -> [ 145; a; b; c; d ]
    | `OpMatrixTimesMatrix (a, b, c, d) -> [ 146; a; b; c; d ]
    | `OpOuterProduct (a, b, c, d) -> [ 147; a; b; c; d ]
    | `OpDot (a, b, c, d) -> [ 148; a; b; c; d ]
    | `OpIAddCarry (a, b, c, d) -> [ 149; a; b; c; d ]
    | `OpISubBorrow (a, b, c, d) -> [ 150; a; b; c; d ]
    | `OpUMulExtended (a, b, c, d) -> [ 151; a; b; c; d ]
    | `OpSMulExtended (a, b, c, d) -> [ 152; a; b; c; d ]
    | `OpAny (a, b, c) -> [ 154; a; b; c ]
    | `OpAll (a, b, c) -> [ 155; a; b; c ]
    | `OpIsNan (a, b, c) -> [ 156; a; b; c ]
    | `OpIsInf (a, b, c) -> [ 157; a; b; c ]
    | `OpIsFinite (a, b, c) -> [ 158; a; b; c ]
    | `OpIsNormal (a, b, c) -> [ 159; a; b; c ]
    | `OpSignBitSet (a, b, c) -> [ 160; a; b; c ]
    | `OpLessOrGreater (a, b, c, d) -> [ 161; a; b; c; d ]
    | `OpOrdered (a, b, c, d) -> [ 162; a; b; c; d ]
    | `OpUnordered (a, b, c, d) -> [ 163; a; b; c; d ]
    | `OpLogicalEqual (a, b, c, d) -> [ 164; a; b; c; d ]
    | `OpLogicalNotEqual (a, b, c, d) -> [ 165; a; b; c; d ]
    | `OpLogicalOr (a, b, c, d) -> [ 166; a; b; c; d ]
    | `OpLogicalAnd (a, b, c, d) -> [ 167; a; b; c; d ]
    | `OpLogicalNot (a, b, c) -> [ 168; a; b; c ]
    | `OpSelect (a, b, c, d, e) -> [ 169; a; b; c; d; e ]
    | `OpIEqual (a, b, c, d) -> [ 170; a; b; c; d ]
    | `OpINotEqual (a, b, c, d) -> [ 171; a; b; c; d ]
    | `OpUGreaterThan (a, b, c, d) -> [ 172; a; b; c; d ]
    | `OpSGreaterThan (a, b, c, d) -> [ 173; a; b; c; d ]
    | `OpUGreaterThanEqual (a, b, c, d) -> [ 174; a; b; c; d ]
    | `OpSGreaterThanEqual (a, b, c, d) -> [ 175; a; b; c; d ]
    | `OpULessThan (a, b, c, d) -> [ 176; a; b; c; d ]
    | `OpSLessThan (a, b, c, d) -> [ 177; a; b; c; d ]
    | `OpULessThanEqual (a, b, c, d) -> [ 178; a; b; c; d ]
    | `OpSLessThanEqual (a, b, c, d) -> [ 179; a; b; c; d ]
    | `OpFOrdEqual (a, b, c, d) -> [ 180; a; b; c; d ]
    | `OpFUnordEqual (a, b, c, d) -> [ 181; a; b; c; d ]
    | `OpFOrdNotEqual (a, b, c, d) -> [ 182; a; b; c; d ]
    | `OpFUnordNotEqual (a, b, c, d) -> [ 183; a; b; c; d ]
    | `OpFOrdLessThan (a, b, c, d) -> [ 184; a; b; c; d ]
    | `OpFUnordLessThan (a, b, c, d) -> [ 185; a; b; c; d ]
    | `OpFOrdGreaterThan (a, b, c, d) -> [ 186; a; b; c; d ]
    | `OpFUnordGreaterThan (a, b, c, d) -> [ 187; a; b; c; d ]
    | `OpFOrdLessThanEqual (a, b, c, d) -> [ 188; a; b; c; d ]
    | `OpFUnordLessThanEqual (a, b, c, d) -> [ 189; a; b; c; d ]
    | `OpFOrdGreaterThanEqual (a, b, c, d) -> [ 190; a; b; c; d ]
    | `OpFUnordGreaterThanEqual (a, b, c, d) -> [ 191; a; b; c; d ]
    | `OpShiftRightLogical (a, b, c, d) -> [ 194; a; b; c; d ]
    | `OpShiftRightArithmetic (a, b, c, d) -> [ 195; a; b; c; d ]
    | `OpShiftLeftLogical (a, b, c, d) -> [ 196; a; b; c; d ]
    | `OpBitwiseOr (a, b, c, d) -> [ 197; a; b; c; d ]
    | `OpBitwiseXor (a, b, c, d) -> [ 198; a; b; c; d ]
    | `OpBitwiseAnd (a, b, c, d) -> [ 199; a; b; c; d ]
    | `OpNot (a, b, c) -> [ 200; a; b; c ]
    | `OpBitFieldInsert (a, b, c, d, e, f) -> [ 201; a; b; c; d; e; f ]
    | `OpBitFieldSExtract (a, b, c, d, e) -> [ 202; a; b; c; d; e ]
    | `OpBitFieldUExtract (a, b, c, d, e) -> [ 203; a; b; c; d; e ]
    | `OpBitReverse (a, b, c) -> [ 204; a; b; c ]
    | `OpBitCount (a, b, c) -> [ 205; a; b; c ]
    | `OpDPdx (a, b, c) -> [ 207; a; b; c ]
    | `OpDPdy (a, b, c) -> [ 208; a; b; c ]
    | `OpFwidth (a, b, c) -> [ 209; a; b; c ]
    | `OpDPdxFine (a, b, c) -> [ 210; a; b; c ]
    | `OpDPdyFine (a, b, c) -> [ 211; a; b; c ]
    | `OpFwidthFine (a, b, c) -> [ 212; a; b; c ]
    | `OpDPdxCoarse (a, b, c) -> [ 213; a; b; c ]
    | `OpDPdyCoarse (a, b, c) -> [ 214; a; b; c ]
    | `OpFwidthCoarse (a, b, c) -> [ 215; a; b; c ]
    | `OpEmitVertex -> [ 218 ]
    | `OpEndPrimitive -> [ 219 ]
    | `OpEmitStreamVertex a -> [ 220; a ]
    | `OpEndStreamPrimitive a -> [ 221; a ]
    | `OpControlBarrier (a, b, c) -> [ 224; a; b; c ]
    | `OpMemoryBarrier (a, b) -> [ 225; a; b ]
    | `OpAtomicLoad (a, b, c, d, e) -> [ 227; a; b; c; d; e ]
    | `OpAtomicStore (a, b, c, d) -> [ 228; a; b; c; d ]
    | `OpAtomicExchange (a, b, c, d, e, f) -> [ 229; a; b; c; d; e; f ]
    | `OpAtomicCompareExchange (a, b, c, d, e, f, g, h) ->
        [ 230; a; b; c; d; e; f; g; h ]
    | `OpAtomicCompareExchangeWeak (a, b, c, d, e, f, g, h) ->
        [ 231; a; b; c; d; e; f; g; h ]
    | `OpAtomicIIncrement (a, b, c, d, e) -> [ 232; a; b; c; d; e ]
    | `OpAtomicIDecrement (a, b, c, d, e) -> [ 233; a; b; c; d; e ]
    | `OpAtomicIAdd (a, b, c, d, e, f) -> [ 234; a; b; c; d; e; f ]
    | `OpAtomicISub (a, b, c, d, e, f) -> [ 235; a; b; c; d; e; f ]
    | `OpAtomicSMin (a, b, c, d, e, f) -> [ 236; a; b; c; d; e; f ]
    | `OpAtomicUMin (a, b, c, d, e, f) -> [ 237; a; b; c; d; e; f ]
    | `OpAtomicSMax (a, b, c, d, e, f) -> [ 238; a; b; c; d; e; f ]
    | `OpAtomicUMax (a, b, c, d, e, f) -> [ 239; a; b; c; d; e; f ]
    | `OpAtomicAnd (a, b, c, d, e, f) -> [ 240; a; b; c; d; e; f ]
    | `OpAtomicOr (a, b, c, d, e, f) -> [ 241; a; b; c; d; e; f ]
    | `OpAtomicXor (a, b, c, d, e, f) -> [ 242; a; b; c; d; e; f ]
    | `OpPhi (a, b, c) -> ( @ ) ([ 245; a; b ], c)
    | `OpLoopMerge (a, b, c) -> [ 246; a; b; c ]
    | `OpSelectionMerge (a, b) -> [ 247; a; b ]
    | `OpLabel a -> [ 248; a ]
    | `OpBranch a -> [ 249; a ]
    | `OpBranchConditional (a, b, c, d) -> ( @ ) ([ 250; a; b; c ], d)
    | `OpSwitch (a, b, c) -> ( @ ) ([ 251; a; b ], c)
    | `OpKill -> [ 252 ]
    | `OpReturn -> [ 253 ]
    | `OpReturnValue a -> [ 254; a ]
    | `OpUnreachable -> [ 255 ]
    | `OpLifetimeStart (a, b) -> [ 256; a; b ]
    | `OpLifetimeStop (a, b) -> [ 257; a; b ]
    | `OpGroupAsyncCopy (a, b, c, d, e, f, g, h) ->
        [ 259; a; b; c; d; e; f; g; h ]
    | `OpGroupWaitEvents (a, b, c) -> [ 260; a; b; c ]
    | `OpGroupAll (a, b, c, d) -> [ 261; a; b; c; d ]
    | `OpGroupAny (a, b, c, d) -> [ 262; a; b; c; d ]
    | `OpGroupBroadcast (a, b, c, d, e) -> [ 263; a; b; c; d; e ]
    | `OpGroupIAdd (a, b, c, d, e) -> [ 264; a; b; c; d; e ]
    | `OpGroupFAdd (a, b, c, d, e) -> [ 265; a; b; c; d; e ]
    | `OpGroupFMin (a, b, c, d, e) -> [ 266; a; b; c; d; e ]
    | `OpGroupUMin (a, b, c, d, e) -> [ 267; a; b; c; d; e ]
    | `OpGroupSMin (a, b, c, d, e) -> [ 268; a; b; c; d; e ]
    | `OpGroupFMax (a, b, c, d, e) -> [ 269; a; b; c; d; e ]
    | `OpGroupUMax (a, b, c, d, e) -> [ 270; a; b; c; d; e ]
    | `OpGroupSMax (a, b, c, d, e) -> [ 271; a; b; c; d; e ]
    | `OpReadPipe (a, b, c, d, e, f) -> [ 274; a; b; c; d; e; f ]
    | `OpWritePipe (a, b, c, d, e, f) -> [ 275; a; b; c; d; e; f ]
    | `OpReservedReadPipe (a, b, c, d, e, f, g, h) ->
        [ 276; a; b; c; d; e; f; g; h ]
    | `OpReservedWritePipe (a, b, c, d, e, f, g, h) ->
        [ 277; a; b; c; d; e; f; g; h ]
    | `OpReserveReadPipePackets (a, b, c, d, e, f) ->
        [ 278; a; b; c; d; e; f ]
    | `OpReserveWritePipePackets (a, b, c, d, e, f) ->
        [ 279; a; b; c; d; e; f ]
    | `OpCommitReadPipe (a, b, c, d) -> [ 280; a; b; c; d ]
    | `OpCommitWritePipe (a, b, c, d) -> [ 281; a; b; c; d ]
    | `OpIsValidReserveId (a, b, c) -> [ 282; a; b; c ]
    | `OpGetNumPipePackets (a, b, c, d, e) -> [ 283; a; b; c; d; e ]
    | `OpGetMaxPipePackets (a, b, c, d, e) -> [ 284; a; b; c; d; e ]
    | `OpGroupReserveReadPipePackets (a, b, c, d, e, f, g) ->
        [ 285; a; b; c; d; e; f; g ]
    | `OpGroupReserveWritePipePackets (a, b, c, d, e, f, g) ->
        [ 286; a; b; c; d; e; f; g ]
    | `OpGroupCommitReadPipe (a, b, c, d, e) -> [ 287; a; b; c; d; e ]
    | `OpGroupCommitWritePipe (a, b, c, d, e) -> [ 288; a; b; c; d; e ]
    | `OpEnqueueMarker (a, b, c, d, e, f) -> [ 291; a; b; c; d; e; f ]
    | `OpEnqueueKernel (a, b, c, d, e, f, g, h, i, j, k, l, m) ->
        ( @ ) ([ 292; a; b; c; d; e; f; g; h; i; j; k; l ], m)
    | `OpGetKernelNDrangeSubGroupCount (a, b, c, d, e, f, g) ->
        [ 293; a; b; c; d; e; f; g ]
    | `OpGetKernelNDrangeMaxSubGroupSize (a, b, c, d, e, f, g) ->
        [ 294; a; b; c; d; e; f; g ]
    | `OpGetKernelWorkGroupSize (a, b, c, d, e, f) ->
        [ 295; a; b; c; d; e; f ]
    | `OpGetKernelPreferredWorkGroupSizeMultiple (a, b, c, d, e, f) ->
        [ 296; a; b; c; d; e; f ]
    | `OpRetainEvent a -> [ 297; a ]
    | `OpReleaseEvent a -> [ 298; a ]
    | `OpCreateUserEvent (a, b) -> [ 299; a; b ]
    | `OpIsValidEvent (a, b, c) -> [ 300; a; b; c ]
    | `OpSetUserEventStatus (a, b) -> [ 301; a; b ]
    | `OpCaptureEventProfilingInfo (a, b, c) -> [ 302; a; b; c ]
    | `OpGetDefaultQueue (a, b) -> [ 303; a; b ]
    | `OpBuildNDRange (a, b, c, d, e) -> [ 304; a; b; c; d; e ]
    | `OpImageSparseSampleImplicitLod (a, b, c, d, e) ->
        ( @ ) ([ 305; a; b; c; d ], (list_of_list_opt e))
    | `OpImageSparseSampleExplicitLod (a, b, c, d, e) ->
        [ 306; a; b; c; d; e ]
    | `OpImageSparseSampleDrefImplicitLod (a, b, c, d, e, f) ->
        ( @ ) ([ 307; a; b; c; d; e ], (list_of_list_opt f))
    | `OpImageSparseSampleDrefExplicitLod (a, b, c, d, e, f) ->
        [ 308; a; b; c; d; e; f ]
    | `OpImageSparseSampleProjImplicitLod (a, b, c, d, e) ->
        ( @ ) ([ 309; a; b; c; d ], (list_of_list_opt e))
    | `OpImageSparseSampleProjExplicitLod (a, b, c, d, e) ->
        [ 310; a; b; c; d; e ]
    | `OpImageSparseSampleProjDrefImplicitLod (a, b, c, d, e, f) ->
        ( @ ) ([ 311; a; b; c; d; e ], (list_of_list_opt f))
    | `OpImageSparseSampleProjDrefExplicitLod (a, b, c, d, e, f) ->
        [ 312; a; b; c; d; e; f ]
    | `OpImageSparseFetch (a, b, c, d, e) ->
        ( @ ) ([ 313; a; b; c; d ], (list_of_list_opt e))
    | `OpImageSparseGather (a, b, c, d, e, f) ->
        ( @ ) ([ 314; a; b; c; d; e ], (list_of_list_opt f))
    | `OpImageSparseDrefGather (a, b, c, d, e, f) ->
        ( @ ) ([ 315; a; b; c; d; e ], (list_of_list_opt f))
    | `OpImageSparseTexelsResident (a, b, c) -> [ 316; a; b; c ]
    | `OpNoLine -> [ 317 ]
    | `OpAtomicFlagTestAndSet (a, b, c, d, e) -> [ 318; a; b; c; d; e ]
    | `OpAtomicFlagClear (a, b, c) -> [ 319; a; b; c ]
    | `OpImageSparseRead (a, b, c, d, e) ->
        ( @ ) ([ 320; a; b; c; d ], (list_of_list_opt e))
    | `OpSizeOf (a, b, c) -> [ 321; a; b; c ]
    | `OpTypePipeStorage a -> [ 322; a ]
    | `OpConstantPipeStorage (a, b, c, d, e) -> [ 323; a; b; c; d; e ]
    | `OpCreatePipeFromPipeStorage (a, b, c) -> [ 324; a; b; c ]
    | `OpGetKernelLocalSizeForSubgroupCount (a, b, c, d, e, f, g) ->
        [ 325; a; b; c; d; e; f; g ]
    | `OpGetKernelMaxNumSubgroups (a, b, c, d, e, f) ->
        [ 326; a; b; c; d; e; f ]
    | `OpTypeNamedBarrier a -> [ 327; a ]
    | `OpNamedBarrierInitialize (a, b, c) -> [ 328; a; b; c ]
    | `OpMemoryNamedBarrier (a, b, c) -> [ 329; a; b; c ]
    | `OpModuleProcessed a -> [ 330; a ]
    | `OpSubgroupBallotKHR (a, b, c) -> [ 4421; a; b; c ]
    | `OpSubgroupFirstInvocationKHR (a, b, c) -> [ 4422; a; b; c ];;
