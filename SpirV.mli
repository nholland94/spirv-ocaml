open Batteries;;
module IdMap : Map.S with type key = Int32.t;;
exception Id_not_found of Int32.t;;
type id = int32;;
type word = int32;;
type big_int = Big_int.big_int;;
type big_int_or_float = | BigInt of big_int | Float of float;;
type id_result_type = id;;
type id_result = id;;
type id_memory_semantics = id;;
type id_scope = id;;
type id_ref = id;;
type literal_integer = int32;;
type literal_string = string;;
type literal_context_dependent_number = big_int_or_float;;
type literal_ext_inst_integer = int;;
type literal_spec_constant_op_integer = int;;
type pair_literal_integer_id_ref = (int32 * id);;
type pair_id_ref_literal_integer = (id * int32);;
type pair_id_ref_id_ref = (id * id);;
type image_operands =
  ImageOperandsNone
  | ImageOperandsBias of id_ref
  | ImageOperandsLod of id_ref
  | ImageOperandsGrad of id_ref * id_ref
  | ImageOperandsConstOffset of id_ref
  | ImageOperandsOffset of id_ref
  | ImageOperandsConstOffsets of id_ref
  | ImageOperandsSample of id_ref
  | ImageOperandsMinLod of id_ref;;
type f_p_fast_math_mode =
  FPFastMathModeNone
  | FPFastMathModeNotNaN
  | FPFastMathModeNotInf
  | FPFastMathModeNSZ
  | FPFastMathModeAllowRecip
  | FPFastMathModeFast;;
type selection_control =
  SelectionControlNone
  | SelectionControlFlatten
  | SelectionControlDontFlatten;;
type loop_control =
  LoopControlNone
  | LoopControlUnroll
  | LoopControlDontUnroll
  | LoopControlDependencyInfinite
  | LoopControlDependencyLength of literal_integer;;
type function_control =
  FunctionControlNone
  | FunctionControlInline
  | FunctionControlDontInline
  | FunctionControlPure
  | FunctionControlConst;;
type memory_semantics =
  MemorySemanticsRelaxed
  | MemorySemanticsNone
  | MemorySemanticsAcquire
  | MemorySemanticsRelease
  | MemorySemanticsAcquireRelease
  | MemorySemanticsSequentiallyConsistent
  | MemorySemanticsUniformMemory
  | MemorySemanticsSubgroupMemory
  | MemorySemanticsWorkgroupMemory
  | MemorySemanticsCrossWorkgroupMemory
  | MemorySemanticsAtomicCounterMemory
  | MemorySemanticsImageMemory;;
type memory_access =
  MemoryAccessNone
  | MemoryAccessVolatile
  | MemoryAccessAligned of literal_integer
  | MemoryAccessNontemporal;;
type kernel_profiling_info =
  KernelProfilingInfoNone
  | KernelProfilingInfoCmdExecTime;;
type source_language =
  SourceLanguageUnknown
  | SourceLanguageESSL
  | SourceLanguageGLSL
  | SourceLanguageOpenCL_C
  | SourceLanguageOpenCL_CPP;;
type execution_model =
  ExecutionModelVertex
  | ExecutionModelTessellationControl
  | ExecutionModelTessellationEvaluation
  | ExecutionModelGeometry
  | ExecutionModelFragment
  | ExecutionModelGLCompute
  | ExecutionModelKernel;;
type addressing_model =
  AddressingModelLogical
  | AddressingModelPhysical32
  | AddressingModelPhysical64;;
type memory_model =
  MemoryModelSimple
  | MemoryModelGLSL450
  | MemoryModelOpenCL;;
type execution_mode =
  ExecutionModeInvocations of literal_integer
  | ExecutionModeSpacingEqual
  | ExecutionModeSpacingFractionalEven
  | ExecutionModeSpacingFractionalOdd
  | ExecutionModeVertexOrderCw
  | ExecutionModeVertexOrderCcw
  | ExecutionModePixelCenterInteger
  | ExecutionModeOriginUpperLeft
  | ExecutionModeOriginLowerLeft
  | ExecutionModeEarlyFragmentTests
  | ExecutionModePointMode
  | ExecutionModeXfb
  | ExecutionModeDepthReplacing
  | ExecutionModeDepthGreater
  | ExecutionModeDepthLess
  | ExecutionModeDepthUnchanged
  | ExecutionModeLocalSize of literal_integer * literal_integer
                              * literal_integer
  | ExecutionModeLocalSizeHint of literal_integer * literal_integer
                                  * literal_integer
  | ExecutionModeInputPoints
  | ExecutionModeInputLines
  | ExecutionModeInputLinesAdjacency
  | ExecutionModeTriangles
  | ExecutionModeInputTrianglesAdjacency
  | ExecutionModeQuads
  | ExecutionModeIsolines
  | ExecutionModeOutputVertices of literal_integer
  | ExecutionModeOutputPoints
  | ExecutionModeOutputLineStrip
  | ExecutionModeOutputTriangleStrip
  | ExecutionModeVecTypeHint of literal_integer
  | ExecutionModeContractionOff
  | ExecutionModeInitializer
  | ExecutionModeFinalizer
  | ExecutionModeSubgroupSize of literal_integer
  | ExecutionModeSubgroupsPerWorkgroup of literal_integer;;
type storage_class =
  StorageClassUniformConstant
  | StorageClassInput
  | StorageClassUniform
  | StorageClassOutput
  | StorageClassWorkgroup
  | StorageClassCrossWorkgroup
  | StorageClassPrivate
  | StorageClassFunction
  | StorageClassGeneric
  | StorageClassPushConstant
  | StorageClassAtomicCounter
  | StorageClassImage;;
type dim =
  Dim1D
  | Dim2D
  | Dim3D
  | DimCube
  | DimRect
  | DimBuffer
  | DimSubpassData;;
type sampler_addressing_mode =
  SamplerAddressingModeNone
  | SamplerAddressingModeClampToEdge
  | SamplerAddressingModeClamp
  | SamplerAddressingModeRepeat
  | SamplerAddressingModeRepeatMirrored;;
type sampler_filter_mode = SamplerFilterModeNearest | SamplerFilterModeLinear;;
type image_format =
  ImageFormatUnknown
  | ImageFormatRgba32f
  | ImageFormatRgba16f
  | ImageFormatR32f
  | ImageFormatRgba8
  | ImageFormatRgba8Snorm
  | ImageFormatRg32f
  | ImageFormatRg16f
  | ImageFormatR11fG11fB10f
  | ImageFormatR16f
  | ImageFormatRgba16
  | ImageFormatRgb10A2
  | ImageFormatRg16
  | ImageFormatRg8
  | ImageFormatR16
  | ImageFormatR8
  | ImageFormatRgba16Snorm
  | ImageFormatRg16Snorm
  | ImageFormatRg8Snorm
  | ImageFormatR16Snorm
  | ImageFormatR8Snorm
  | ImageFormatRgba32i
  | ImageFormatRgba16i
  | ImageFormatRgba8i
  | ImageFormatR32i
  | ImageFormatRg32i
  | ImageFormatRg16i
  | ImageFormatRg8i
  | ImageFormatR16i
  | ImageFormatR8i
  | ImageFormatRgba32ui
  | ImageFormatRgba16ui
  | ImageFormatRgba8ui
  | ImageFormatR32ui
  | ImageFormatRgb10a2ui
  | ImageFormatRg32ui
  | ImageFormatRg16ui
  | ImageFormatRg8ui
  | ImageFormatR16ui
  | ImageFormatR8ui;;
type image_channel_order =
  ImageChannelOrderR
  | ImageChannelOrderA
  | ImageChannelOrderRG
  | ImageChannelOrderRA
  | ImageChannelOrderRGB
  | ImageChannelOrderRGBA
  | ImageChannelOrderBGRA
  | ImageChannelOrderARGB
  | ImageChannelOrderIntensity
  | ImageChannelOrderLuminance
  | ImageChannelOrderRx
  | ImageChannelOrderRGx
  | ImageChannelOrderRGBx
  | ImageChannelOrderDepth
  | ImageChannelOrderDepthStencil
  | ImageChannelOrderSRGB
  | ImageChannelOrderSRGBx
  | ImageChannelOrderSRGBA
  | ImageChannelOrderSBGRA
  | ImageChannelOrderABGR;;
type image_channel_data_type =
  ImageChannelDataTypeSnormInt8
  | ImageChannelDataTypeSnormInt16
  | ImageChannelDataTypeUnormInt8
  | ImageChannelDataTypeUnormInt16
  | ImageChannelDataTypeUnormShort565
  | ImageChannelDataTypeUnormShort555
  | ImageChannelDataTypeUnormInt101010
  | ImageChannelDataTypeSignedInt8
  | ImageChannelDataTypeSignedInt16
  | ImageChannelDataTypeSignedInt32
  | ImageChannelDataTypeUnsignedInt8
  | ImageChannelDataTypeUnsignedInt16
  | ImageChannelDataTypeUnsignedInt32
  | ImageChannelDataTypeHalfFloat
  | ImageChannelDataTypeFloat
  | ImageChannelDataTypeUnormInt24
  | ImageChannelDataTypeUnormInt101010_2;;
type f_p_rounding_mode =
  FPRoundingModeRTE
  | FPRoundingModeRTZ
  | FPRoundingModeRTP
  | FPRoundingModeRTN;;
type linkage_type = LinkageTypeExport | LinkageTypeImport;;
type access_qualifier =
  AccessQualifierReadOnly
  | AccessQualifierWriteOnly
  | AccessQualifierReadWrite;;
type function_parameter_attribute =
  FunctionParameterAttributeZext
  | FunctionParameterAttributeSext
  | FunctionParameterAttributeByVal
  | FunctionParameterAttributeSret
  | FunctionParameterAttributeNoAlias
  | FunctionParameterAttributeNoCapture
  | FunctionParameterAttributeNoWrite
  | FunctionParameterAttributeNoReadWrite;;
type built_in =
  BuiltInPosition
  | BuiltInPointSize
  | BuiltInClipDistance
  | BuiltInCullDistance
  | BuiltInVertexId
  | BuiltInInstanceId
  | BuiltInPrimitiveId
  | BuiltInInvocationId
  | BuiltInLayer
  | BuiltInViewportIndex
  | BuiltInTessLevelOuter
  | BuiltInTessLevelInner
  | BuiltInTessCoord
  | BuiltInPatchVertices
  | BuiltInFragCoord
  | BuiltInPointCoord
  | BuiltInFrontFacing
  | BuiltInSampleId
  | BuiltInSamplePosition
  | BuiltInSampleMask
  | BuiltInFragDepth
  | BuiltInHelperInvocation
  | BuiltInNumWorkgroups
  | BuiltInWorkgroupSize
  | BuiltInWorkgroupId
  | BuiltInLocalInvocationId
  | BuiltInGlobalInvocationId
  | BuiltInLocalInvocationIndex
  | BuiltInWorkDim
  | BuiltInGlobalSize
  | BuiltInEnqueuedWorkgroupSize
  | BuiltInGlobalOffset
  | BuiltInGlobalLinearId
  | BuiltInSubgroupSize
  | BuiltInSubgroupMaxSize
  | BuiltInNumSubgroups
  | BuiltInNumEnqueuedSubgroups
  | BuiltInSubgroupId
  | BuiltInSubgroupLocalInvocationId
  | BuiltInVertexIndex
  | BuiltInInstanceIndex
  | BuiltInSubgroupEqMaskKHR
  | BuiltInSubgroupGeMaskKHR
  | BuiltInSubgroupGtMaskKHR
  | BuiltInSubgroupLeMaskKHR
  | BuiltInSubgroupLtMaskKHR
  | BuiltInBaseVertex
  | BuiltInBaseInstance
  | BuiltInDrawIndex;;
type scope =
  ScopeCrossDevice
  | ScopeDevice
  | ScopeWorkgroup
  | ScopeSubgroup
  | ScopeInvocation;;
type group_operation =
  GroupOperationReduce
  | GroupOperationInclusiveScan
  | GroupOperationExclusiveScan;;
type kernel_enqueue_flags =
  KernelEnqueueFlagsNoWait
  | KernelEnqueueFlagsWaitKernel
  | KernelEnqueueFlagsWaitWorkGroup;;
type capability =
  CapabilityMatrix
  | CapabilityShader
  | CapabilityGeometry
  | CapabilityTessellation
  | CapabilityAddresses
  | CapabilityLinkage
  | CapabilityKernel
  | CapabilityVector16
  | CapabilityFloat16Buffer
  | CapabilityFloat16
  | CapabilityFloat64
  | CapabilityInt64
  | CapabilityInt64Atomics
  | CapabilityImageBasic
  | CapabilityImageReadWrite
  | CapabilityImageMipmap
  | CapabilityPipes
  | CapabilityGroups
  | CapabilityDeviceEnqueue
  | CapabilityLiteralSampler
  | CapabilityAtomicStorage
  | CapabilityInt16
  | CapabilityTessellationPointSize
  | CapabilityGeometryPointSize
  | CapabilityImageGatherExtended
  | CapabilityStorageImageMultisample
  | CapabilityUniformBufferArrayDynamicIndexing
  | CapabilitySampledImageArrayDynamicIndexing
  | CapabilityStorageBufferArrayDynamicIndexing
  | CapabilityStorageImageArrayDynamicIndexing
  | CapabilityClipDistance
  | CapabilityCullDistance
  | CapabilityImageCubeArray
  | CapabilitySampleRateShading
  | CapabilityImageRect
  | CapabilitySampledRect
  | CapabilityGenericPointer
  | CapabilityInt8
  | CapabilityInputAttachment
  | CapabilitySparseResidency
  | CapabilityMinLod
  | CapabilitySampled1D
  | CapabilityImage1D
  | CapabilitySampledCubeArray
  | CapabilitySampledBuffer
  | CapabilityImageBuffer
  | CapabilityImageMSArray
  | CapabilityStorageImageExtendedFormats
  | CapabilityImageQuery
  | CapabilityDerivativeControl
  | CapabilityInterpolationFunction
  | CapabilityTransformFeedback
  | CapabilityGeometryStreams
  | CapabilityStorageImageReadWithoutFormat
  | CapabilityStorageImageWriteWithoutFormat
  | CapabilityMultiViewport
  | CapabilitySubgroupDispatch
  | CapabilityNamedBarrier
  | CapabilityPipeStorage
  | CapabilitySubgroupBallotKHR
  | CapabilityDrawParameters;;
type decoration =
  DecorationRelaxedPrecision
  | DecorationSpecId of literal_integer
  | DecorationBlock
  | DecorationBufferBlock
  | DecorationRowMajor
  | DecorationColMajor
  | DecorationArrayStride of literal_integer
  | DecorationMatrixStride of literal_integer
  | DecorationGLSLShared
  | DecorationGLSLPacked
  | DecorationCPacked
  | DecorationBuiltIn of built_in
  | DecorationNoPerspective
  | DecorationFlat
  | DecorationPatch
  | DecorationCentroid
  | DecorationSample
  | DecorationInvariant
  | DecorationRestrict
  | DecorationAliased
  | DecorationVolatile
  | DecorationConstant
  | DecorationCoherent
  | DecorationNonWritable
  | DecorationNonReadable
  | DecorationUniform
  | DecorationSaturatedConversion
  | DecorationStream of literal_integer
  | DecorationLocation of literal_integer
  | DecorationComponent of literal_integer
  | DecorationIndex of literal_integer
  | DecorationBinding of literal_integer
  | DecorationDescriptorSet of literal_integer
  | DecorationOffset of literal_integer
  | DecorationXfbBuffer of literal_integer
  | DecorationXfbStride of literal_integer
  | DecorationFuncParamAttr of function_parameter_attribute
  | DecorationFPRoundingMode of f_p_rounding_mode
  | DecorationFPFastMathMode of f_p_fast_math_mode
  | DecorationLinkageAttributes of literal_string * linkage_type
  | DecorationNoContraction
  | DecorationInputAttachmentIndex of literal_integer
  | DecorationAlignment of literal_integer
  | DecorationMaxByteOffset of literal_integer;;
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
val version : (int * int);;
val compile_to_words : op list -> word list;;
