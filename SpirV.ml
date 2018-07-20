open Batteries;;
module IdMap = Map.Make(Int32);;
exception Id_not_found of Int32.t;;
type id =
  int32
  and word =
  int32
  and big_int =
  Big_int.
  big_int
  and big_int_or_float =
  | BigInt of big_int | Float of float
  and ext_inst_fn =
  unit -> word list
  and id_result_type =
  id
  and id_result =
  id
  and id_memory_semantics =
  id
  and id_scope =
  id
  and id_ref =
  id
  and literal_integer =
  int32
  and literal_string =
  string
  and literal_context_dependent_number =
  big_int_or_float
  and literal_ext_inst_integer =
  ext_inst_fn
  and literal_spec_constant_op_integer =
  spec_op
  and pair_literal_integer_id_ref =
  (int32 * id)
  and pair_id_ref_literal_integer =
  (id * int32)
  and pair_id_ref_id_ref =
  (id * id)
  and image_operands =
  ImageOperandsNone
  | ImageOperandsBias of id_ref
  | ImageOperandsLod of id_ref
  | ImageOperandsGrad of id_ref * id_ref
  | ImageOperandsConstOffset of id_ref
  | ImageOperandsOffset of id_ref
  | ImageOperandsConstOffsets of id_ref
  | ImageOperandsSample of id_ref
  | ImageOperandsMinLod of id_ref
  and f_p_fast_math_mode =
  FPFastMathModeNone
  | FPFastMathModeNotNaN
  | FPFastMathModeNotInf
  | FPFastMathModeNSZ
  | FPFastMathModeAllowRecip
  | FPFastMathModeFast
  and selection_control =
  SelectionControlNone
  | SelectionControlFlatten
  | SelectionControlDontFlatten
  and loop_control =
  LoopControlNone
  | LoopControlUnroll
  | LoopControlDontUnroll
  | LoopControlDependencyInfinite
  | LoopControlDependencyLength of literal_integer
  and function_control =
  FunctionControlNone
  | FunctionControlInline
  | FunctionControlDontInline
  | FunctionControlPure
  | FunctionControlConst
  and memory_semantics =
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
  | MemorySemanticsImageMemory
  and memory_access =
  MemoryAccessNone
  | MemoryAccessVolatile
  | MemoryAccessAligned of literal_integer
  | MemoryAccessNontemporal
  and kernel_profiling_info =
  KernelProfilingInfoNone
  | KernelProfilingInfoCmdExecTime
  and source_language =
  SourceLanguageUnknown
  | SourceLanguageESSL
  | SourceLanguageGLSL
  | SourceLanguageOpenCL_C
  | SourceLanguageOpenCL_CPP
  and execution_model =
  ExecutionModelVertex
  | ExecutionModelTessellationControl
  | ExecutionModelTessellationEvaluation
  | ExecutionModelGeometry
  | ExecutionModelFragment
  | ExecutionModelGLCompute
  | ExecutionModelKernel
  and addressing_model =
  AddressingModelLogical
  | AddressingModelPhysical32
  | AddressingModelPhysical64
  and memory_model =
  MemoryModelSimple
  | MemoryModelGLSL450
  | MemoryModelOpenCL
  and execution_mode =
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
  | ExecutionModeSubgroupsPerWorkgroup of literal_integer
  and storage_class =
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
  | StorageClassImage
  and dim =
  Dim1D
  | Dim2D
  | Dim3D
  | DimCube
  | DimRect
  | DimBuffer
  | DimSubpassData
  and sampler_addressing_mode =
  SamplerAddressingModeNone
  | SamplerAddressingModeClampToEdge
  | SamplerAddressingModeClamp
  | SamplerAddressingModeRepeat
  | SamplerAddressingModeRepeatMirrored
  and sampler_filter_mode =
  SamplerFilterModeNearest
  | SamplerFilterModeLinear
  and image_format =
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
  | ImageFormatR8ui
  and image_channel_order =
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
  | ImageChannelOrderABGR
  and image_channel_data_type =
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
  | ImageChannelDataTypeUnormInt101010_2
  and f_p_rounding_mode =
  FPRoundingModeRTE
  | FPRoundingModeRTZ
  | FPRoundingModeRTP
  | FPRoundingModeRTN
  and linkage_type =
  LinkageTypeExport
  | LinkageTypeImport
  and access_qualifier =
  AccessQualifierReadOnly
  | AccessQualifierWriteOnly
  | AccessQualifierReadWrite
  and function_parameter_attribute =
  FunctionParameterAttributeZext
  | FunctionParameterAttributeSext
  | FunctionParameterAttributeByVal
  | FunctionParameterAttributeSret
  | FunctionParameterAttributeNoAlias
  | FunctionParameterAttributeNoCapture
  | FunctionParameterAttributeNoWrite
  | FunctionParameterAttributeNoReadWrite
  and built_in =
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
  | BuiltInDrawIndex
  and scope =
  ScopeCrossDevice
  | ScopeDevice
  | ScopeWorkgroup
  | ScopeSubgroup
  | ScopeInvocation
  and group_operation =
  GroupOperationReduce
  | GroupOperationInclusiveScan
  | GroupOperationExclusiveScan
  and kernel_enqueue_flags =
  KernelEnqueueFlagsNoWait
  | KernelEnqueueFlagsWaitKernel
  | KernelEnqueueFlagsWaitWorkGroup
  and capability =
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
  | CapabilityDrawParameters
  and decoration =
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
  | DecorationFPFastMathMode of f_p_fast_math_mode list
  | DecorationLinkageAttributes of literal_string * linkage_type
  | DecorationNoContraction
  | DecorationInputAttachmentIndex of literal_integer
  | DecorationAlignment of literal_integer
  | DecorationMaxByteOffset of literal_integer
  and op =
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
                    * literal_ext_inst_integer
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
    | `OpFunction of id_result_type * id_result * function_control list
                     * id_ref
    | `OpFunctionParameter of id_result_type * id_result
    | `OpFunctionEnd
    | `OpFunctionCall of id_result_type * id_result * id_ref * id_ref list
    | `OpVariable of id_result_type * id_result * storage_class
                     * id_ref option
    | `OpImageTexelPointer of id_result_type * id_result * id_ref * id_ref
                              * id_ref
    | `OpLoad of id_result_type * id_result * id_ref
                 * (memory_access list) option
    | `OpStore of id_ref * id_ref * (memory_access list) option
    | `OpCopyMemory of id_ref * id_ref * (memory_access list) option
    | `OpCopyMemorySized of id_ref * id_ref * id_ref
                            * (memory_access list) option
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
                                   * id_ref * (image_operands list) option
    | `OpImageSampleExplicitLod of id_result_type * id_result * id_ref
                                   * id_ref * image_operands list
    | `OpImageSampleDrefImplicitLod of id_result_type * id_result * id_ref
                                       * id_ref * id_ref
                                       * (image_operands list) option
    | `OpImageSampleDrefExplicitLod of id_result_type * id_result * id_ref
                                       * id_ref * id_ref
                                       * image_operands list
    | `OpImageSampleProjImplicitLod of id_result_type * id_result * id_ref
                                       * id_ref
                                       * (image_operands list) option
    | `OpImageSampleProjExplicitLod of id_result_type * id_result * id_ref
                                       * id_ref * image_operands list
    | `OpImageSampleProjDrefImplicitLod of id_result_type * id_result
                                           * id_ref * id_ref * id_ref
                                           * (image_operands list) option
    | `OpImageSampleProjDrefExplicitLod of id_result_type * id_result
                                           * id_ref * id_ref * id_ref
                                           * image_operands list
    | `OpImageFetch of id_result_type * id_result * id_ref * id_ref
                       * (image_operands list) option
    | `OpImageGather of id_result_type * id_result * id_ref * id_ref * id_ref
                        * (image_operands list) option
    | `OpImageDrefGather of id_result_type * id_result * id_ref * id_ref
                            * id_ref * (image_operands list) option
    | `OpImageRead of id_result_type * id_result * id_ref * id_ref
                      * (image_operands list) option
    | `OpImageWrite of id_ref * id_ref * id_ref
                       * (image_operands list) option
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
    | `OpLoopMerge of id_ref * id_ref * loop_control list
    | `OpSelectionMerge of id_ref * selection_control list
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
                                         * id_ref
                                         * (image_operands list) option
    | `OpImageSparseSampleExplicitLod of id_result_type * id_result * id_ref
                                         * id_ref * image_operands list
    | `OpImageSparseSampleDrefImplicitLod of id_result_type * id_result
                                             * id_ref * id_ref * id_ref
                                             * (image_operands list) option
    | `OpImageSparseSampleDrefExplicitLod of id_result_type * id_result
                                             * id_ref * id_ref * id_ref
                                             * image_operands list
    | `OpImageSparseSampleProjImplicitLod of id_result_type * id_result
                                             * id_ref * id_ref
                                             * (image_operands list) option
    | `OpImageSparseSampleProjExplicitLod of id_result_type * id_result
                                             * id_ref * id_ref
                                             * image_operands list
    | `OpImageSparseSampleProjDrefImplicitLod of id_result_type * id_result
                                                 * id_ref * id_ref * id_ref
                                                 * (image_operands list)
                                                     option
    | `OpImageSparseSampleProjDrefExplicitLod of id_result_type * id_result
                                                 * id_ref * id_ref * id_ref
                                                 * image_operands list
    | `OpImageSparseFetch of id_result_type * id_result * id_ref * id_ref
                             * (image_operands list) option
    | `OpImageSparseGather of id_result_type * id_result * id_ref * id_ref
                              * id_ref * (image_operands list) option
    | `OpImageSparseDrefGather of id_result_type * id_result * id_ref
                                  * id_ref * id_ref
                                  * (image_operands list) option
    | `OpImageSparseTexelsResident of id_result_type * id_result * id_ref
    | `OpNoLine
    | `OpAtomicFlagTestAndSet of id_result_type * id_result * id_ref
                                 * id_scope * id_memory_semantics
    | `OpAtomicFlagClear of id_ref * id_scope * id_memory_semantics
    | `OpImageSparseRead of id_result_type * id_result * id_ref * id_ref
                            * (image_operands list) option
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
  ]
  and spec_op =
  [
    | `SConvert of id_ref
    | `FConvert of id_ref
    | `SNegate of id_ref
    | `Not of id_ref
    | `IAdd of id_ref * id_ref
    | `ISub of id_ref * id_ref
    | `IMul of id_ref * id_ref
    | `UDiv of id_ref * id_ref
    | `SDiv of id_ref * id_ref
    | `UMod of id_ref * id_ref
    | `SRem of id_ref * id_ref
    | `SMod of id_ref * id_ref
    | `ShiftRightLogical of id_ref * id_ref
    | `ShiftRightArithmetic of id_ref * id_ref
    | `ShiftLeftLogical of id_ref * id_ref
    | `BitwiseOr of id_ref * id_ref
    | `BitwiseXor of id_ref * id_ref
    | `BitwiseAnd of id_ref * id_ref
    | `VectorShuffle of id_ref * id_ref * literal_integer list
    | `CompositeExtract of id_ref * literal_integer list
    | `CompositeInsert of id_ref * id_ref * literal_integer list
    | `LogicalOr of id_ref * id_ref
    | `LogicalAnd of id_ref * id_ref
    | `LogicalNot of id_ref
    | `LogicalEqual of id_ref * id_ref
    | `LogicalNotEqual of id_ref * id_ref
    | `Select of id_ref * id_ref * id_ref
    | `IEqual of id_ref * id_ref
    | `INotEqual of id_ref * id_ref
    | `ULessThan of id_ref * id_ref
    | `SLessThan of id_ref * id_ref
    | `UGreaterThan of id_ref * id_ref
    | `SGreaterThan of id_ref * id_ref
    | `ULessThanEqual of id_ref * id_ref
    | `SLessThanEqual of id_ref * id_ref
    | `UGreaterThanEqual of id_ref * id_ref
    | `SGreaterThanEqual of id_ref * id_ref
    | `QuantizeToF16 of id_ref
    | `ConvertFToS of id_ref
    | `ConvertSToF of id_ref
    | `ConvertFToU of id_ref
    | `ConvertUToF of id_ref
    | `UConvert of id_ref
    | `ConvertPtrToU of id_ref
    | `ConvertUToPtr of id_ref
    | `GenericCastToPtr of id_ref
    | `PtrCastToGeneric of id_ref
    | `Bitcast of id_ref
    | `FNegate of id_ref
    | `FAdd of id_ref * id_ref
    | `FSub of id_ref * id_ref
    | `FMul of id_ref * id_ref
    | `FDiv of id_ref * id_ref
    | `FRem of id_ref * id_ref
    | `FMod of id_ref * id_ref
    | `AccessChain of id_ref * id_ref list
    | `InBoundsAccessChain of id_ref * id_ref list
    | `PtrAccessChain of id_ref * id_ref * id_ref list
    | `InBoundsPtrAccessChain of id_ref * id_ref * id_ref list
  ];;
let magic_number = 0x07230203l;;
let version = (1, 1);;
let version_word = 0x00010100l;;
let generator_number = 0xfadel;;
let word_of_int (i : int32) = i;;
let word_of_id (id : id) =
  if id < 0l then failwith "spirv ids must be positive" else id;;
let word_of_float (f : float) = let open IO
  in
    let buf = output_string ()
    in
      (write_float buf f;
       let str = close_out buf in
       let rec write_to_int32 n acc ls =
         match ls with
         | h :: t ->
             let value = Int32.of_int @@ (Char.code h) in
             let acc = Int32.logor acc (Int32.shift_left value (8 * n))
             in write_to_int32 (n + 1) acc t
         | [] -> acc
       in write_to_int32 0 0l (String.to_list str));;

let words_of_context_dependent_number (size : int) (value : big_int_or_float)
                                      =
  let word_size = 32 in
  let words_of_sized_big_int n =
    let round_up_divisible divisor n =
      (n / divisor) + (if (n mod divisor) > 0 then 1 else 0) in
    let word_count = round_up_divisible word_size size in
    let extract_word i =
      Int32.of_int @@
        (Big_int.int_of_big_int @@ (Big_int.extract_big_int n (32 * i) 32)) in
    let rec extract_words i =
      if i < word_count
      then (extract_word i) :: (extract_words (i + 1))
      else []
    in extract_words 0
  in
    match value with
    | BigInt n -> words_of_sized_big_int n
    | Float f ->
        if size = 32
        then [ word_of_float f ]
        else
          raise
            (Invalid_argument
               "cannot write float literals of sizes other than 32 bits");;
let words_of_string (str : string) =
  let len = String.length str in
  let word_count = (len / 4) + (if (len mod 4) >= 0 then 1 else 0) in
  let buffer = Array.make word_count 0l in
  let add_char_to_word ch offset word =
    Int32.logor word
      (Int32.shift_left (Int32.of_int @@ (Char.code ch)) (offset * 8)) in
  let rec add_char_to_buffer i =
    if i = len
    then ()
    else
      (buffer.(i / 4) <-
         add_char_to_word (String.get str i) (abs @@ ((i mod 4) - 3))
           buffer.(i / 4);
       add_char_to_buffer (i + 1))
  in (add_char_to_buffer 0; Array.to_list buffer);;
let list_of_option (opt : 'a option) =
  match opt with | Some v -> [ v ] | None -> [];;
let list_of_list_option (opt : ('a list) option) =
  match opt with | Some v -> v | None -> [];;
let apply_option (fn : 'a -> 'b) (opt : 'a option) =
  match opt with | Some v -> Some (fn v) | None -> None;;
let words_of_pair_literal_integer_id_ref (n, i) =
  [ word_of_int n; word_of_id i ];;
let words_of_pair_id_ref_literal_integer (i, n) =
  [ word_of_id i; word_of_int n ];;
let words_of_pair_id_ref_id_ref (a, b) = [ word_of_id a; word_of_id b ];;
let words_of_image_operands (flags : image_operands list) =
  let split flag =
    match flag with
    | ImageOperandsNone -> (0x0000l, [])
    | ImageOperandsBias a -> (0x0001l, [ word_of_id a ])
    | ImageOperandsLod a -> (0x0002l, [ word_of_id a ])
    | ImageOperandsGrad (a, b) -> (0x0004l, [ word_of_id a; word_of_id b ])
    | ImageOperandsConstOffset a -> (0x0008l, [ word_of_id a ])
    | ImageOperandsOffset a -> (0x0010l, [ word_of_id a ])
    | ImageOperandsConstOffsets a -> (0x0020l, [ word_of_id a ])
    | ImageOperandsSample a -> (0x0040l, [ word_of_id a ])
    | ImageOperandsMinLod a -> (0x0080l, [ word_of_id a ]) in
  let combine_split_flags (a_flag, a_ops) (b_flag, b_ops) =
    ((Int32.logor a_flag b_flag), (a_ops @ b_ops)) in
  let (flag, ops) =
    List.fold_left combine_split_flags (0l, []) (List.map split flags)
  in flag :: ops;;
let word_of_f_p_fast_math_mode (flags : f_p_fast_math_mode list) =
  let flag_value flag =
    match flag with
    | FPFastMathModeNone -> 0x0000l
    | FPFastMathModeNotNaN -> 0x0001l
    | FPFastMathModeNotInf -> 0x0002l
    | FPFastMathModeNSZ -> 0x0004l
    | FPFastMathModeAllowRecip -> 0x0008l
    | FPFastMathModeFast -> 0x0010l in
  let fold_flag base flag = Int32.logor base (flag_value flag)
  in List.fold_left fold_flag 0l flags;;
let word_of_selection_control (flags : selection_control list) =
  let flag_value flag =
    match flag with
    | SelectionControlNone -> 0x0000l
    | SelectionControlFlatten -> 0x0001l
    | SelectionControlDontFlatten -> 0x0002l in
  let fold_flag base flag = Int32.logor base (flag_value flag)
  in List.fold_left fold_flag 0l flags;;
let words_of_loop_control (flags : loop_control list) =
  let split flag =
    match flag with
    | LoopControlNone -> (0x0000l, [])
    | LoopControlUnroll -> (0x0001l, [])
    | LoopControlDontUnroll -> (0x0002l, [])
    | LoopControlDependencyInfinite -> (0x0004l, [])
    | LoopControlDependencyLength a -> (0x0008l, [ word_of_int a ]) in
  let combine_split_flags (a_flag, a_ops) (b_flag, b_ops) =
    ((Int32.logor a_flag b_flag), (a_ops @ b_ops)) in
  let (flag, ops) =
    List.fold_left combine_split_flags (0l, []) (List.map split flags)
  in flag :: ops;;
let word_of_function_control (flags : function_control list) =
  let flag_value flag =
    match flag with
    | FunctionControlNone -> 0x0000l
    | FunctionControlInline -> 0x0001l
    | FunctionControlDontInline -> 0x0002l
    | FunctionControlPure -> 0x0004l
    | FunctionControlConst -> 0x0008l in
  let fold_flag base flag = Int32.logor base (flag_value flag)
  in List.fold_left fold_flag 0l flags;;
let word_of_memory_semantics (flags : memory_semantics list) =
  let flag_value flag =
    match flag with
    | MemorySemanticsRelaxed -> 0x0000l
    | MemorySemanticsNone -> 0x0000l
    | MemorySemanticsAcquire -> 0x0002l
    | MemorySemanticsRelease -> 0x0004l
    | MemorySemanticsAcquireRelease -> 0x0008l
    | MemorySemanticsSequentiallyConsistent -> 0x0010l
    | MemorySemanticsUniformMemory -> 0x0040l
    | MemorySemanticsSubgroupMemory -> 0x0080l
    | MemorySemanticsWorkgroupMemory -> 0x0100l
    | MemorySemanticsCrossWorkgroupMemory -> 0x0200l
    | MemorySemanticsAtomicCounterMemory -> 0x0400l
    | MemorySemanticsImageMemory -> 0x0800l in
  let fold_flag base flag = Int32.logor base (flag_value flag)
  in List.fold_left fold_flag 0l flags;;
let words_of_memory_access (flags : memory_access list) =
  let split flag =
    match flag with
    | MemoryAccessNone -> (0x0000l, [])
    | MemoryAccessVolatile -> (0x0001l, [])
    | MemoryAccessAligned a -> (0x0002l, [ word_of_int a ])
    | MemoryAccessNontemporal -> (0x0004l, []) in
  let combine_split_flags (a_flag, a_ops) (b_flag, b_ops) =
    ((Int32.logor a_flag b_flag), (a_ops @ b_ops)) in
  let (flag, ops) =
    List.fold_left combine_split_flags (0l, []) (List.map split flags)
  in flag :: ops;;
let word_of_kernel_profiling_info (flags : kernel_profiling_info list) =
  let flag_value flag =
    match flag with
    | KernelProfilingInfoNone -> 0x0000l
    | KernelProfilingInfoCmdExecTime -> 0x0001l in
  let fold_flag base flag = Int32.logor base (flag_value flag)
  in List.fold_left fold_flag 0l flags;;
let word_of_source_language (enum : source_language) =
  match enum with
  | SourceLanguageUnknown -> 0l
  | SourceLanguageESSL -> 1l
  | SourceLanguageGLSL -> 2l
  | SourceLanguageOpenCL_C -> 3l
  | SourceLanguageOpenCL_CPP -> 4l;;
let word_of_execution_model (enum : execution_model) =
  match enum with
  | ExecutionModelVertex -> 0l
  | ExecutionModelTessellationControl -> 1l
  | ExecutionModelTessellationEvaluation -> 2l
  | ExecutionModelGeometry -> 3l
  | ExecutionModelFragment -> 4l
  | ExecutionModelGLCompute -> 5l
  | ExecutionModelKernel -> 6l;;
let word_of_addressing_model (enum : addressing_model) =
  match enum with
  | AddressingModelLogical -> 0l
  | AddressingModelPhysical32 -> 1l
  | AddressingModelPhysical64 -> 2l;;
let word_of_memory_model (enum : memory_model) =
  match enum with
  | MemoryModelSimple -> 0l
  | MemoryModelGLSL450 -> 1l
  | MemoryModelOpenCL -> 2l;;
let words_of_execution_mode (enum : execution_mode) =
  match enum with
  | ExecutionModeInvocations a -> [ 0l; word_of_int a ]
  | ExecutionModeSpacingEqual -> [ 1l ]
  | ExecutionModeSpacingFractionalEven -> [ 2l ]
  | ExecutionModeSpacingFractionalOdd -> [ 3l ]
  | ExecutionModeVertexOrderCw -> [ 4l ]
  | ExecutionModeVertexOrderCcw -> [ 5l ]
  | ExecutionModePixelCenterInteger -> [ 6l ]
  | ExecutionModeOriginUpperLeft -> [ 7l ]
  | ExecutionModeOriginLowerLeft -> [ 8l ]
  | ExecutionModeEarlyFragmentTests -> [ 9l ]
  | ExecutionModePointMode -> [ 10l ]
  | ExecutionModeXfb -> [ 11l ]
  | ExecutionModeDepthReplacing -> [ 12l ]
  | ExecutionModeDepthGreater -> [ 14l ]
  | ExecutionModeDepthLess -> [ 15l ]
  | ExecutionModeDepthUnchanged -> [ 16l ]
  | ExecutionModeLocalSize (a, b, c) ->
      [ 17l; word_of_int a; word_of_int b; word_of_int c ]
  | ExecutionModeLocalSizeHint (a, b, c) ->
      [ 18l; word_of_int a; word_of_int b; word_of_int c ]
  | ExecutionModeInputPoints -> [ 19l ]
  | ExecutionModeInputLines -> [ 20l ]
  | ExecutionModeInputLinesAdjacency -> [ 21l ]
  | ExecutionModeTriangles -> [ 22l ]
  | ExecutionModeInputTrianglesAdjacency -> [ 23l ]
  | ExecutionModeQuads -> [ 24l ]
  | ExecutionModeIsolines -> [ 25l ]
  | ExecutionModeOutputVertices a -> [ 26l; word_of_int a ]
  | ExecutionModeOutputPoints -> [ 27l ]
  | ExecutionModeOutputLineStrip -> [ 28l ]
  | ExecutionModeOutputTriangleStrip -> [ 29l ]
  | ExecutionModeVecTypeHint a -> [ 30l; word_of_int a ]
  | ExecutionModeContractionOff -> [ 31l ]
  | ExecutionModeInitializer -> [ 33l ]
  | ExecutionModeFinalizer -> [ 34l ]
  | ExecutionModeSubgroupSize a -> [ 35l; word_of_int a ]
  | ExecutionModeSubgroupsPerWorkgroup a -> [ 36l; word_of_int a ];;
let word_of_storage_class (enum : storage_class) =
  match enum with
  | StorageClassUniformConstant -> 0l
  | StorageClassInput -> 1l
  | StorageClassUniform -> 2l
  | StorageClassOutput -> 3l
  | StorageClassWorkgroup -> 4l
  | StorageClassCrossWorkgroup -> 5l
  | StorageClassPrivate -> 6l
  | StorageClassFunction -> 7l
  | StorageClassGeneric -> 8l
  | StorageClassPushConstant -> 9l
  | StorageClassAtomicCounter -> 10l
  | StorageClassImage -> 11l;;
let word_of_dim (enum : dim) =
  match enum with
  | Dim1D -> 0l
  | Dim2D -> 1l
  | Dim3D -> 2l
  | DimCube -> 3l
  | DimRect -> 4l
  | DimBuffer -> 5l
  | DimSubpassData -> 6l;;
let word_of_sampler_addressing_mode (enum : sampler_addressing_mode) =
  match enum with
  | SamplerAddressingModeNone -> 0l
  | SamplerAddressingModeClampToEdge -> 1l
  | SamplerAddressingModeClamp -> 2l
  | SamplerAddressingModeRepeat -> 3l
  | SamplerAddressingModeRepeatMirrored -> 4l;;
let word_of_sampler_filter_mode (enum : sampler_filter_mode) =
  match enum with
  | SamplerFilterModeNearest -> 0l
  | SamplerFilterModeLinear -> 1l;;
let word_of_image_format (enum : image_format) =
  match enum with
  | ImageFormatUnknown -> 0l
  | ImageFormatRgba32f -> 1l
  | ImageFormatRgba16f -> 2l
  | ImageFormatR32f -> 3l
  | ImageFormatRgba8 -> 4l
  | ImageFormatRgba8Snorm -> 5l
  | ImageFormatRg32f -> 6l
  | ImageFormatRg16f -> 7l
  | ImageFormatR11fG11fB10f -> 8l
  | ImageFormatR16f -> 9l
  | ImageFormatRgba16 -> 10l
  | ImageFormatRgb10A2 -> 11l
  | ImageFormatRg16 -> 12l
  | ImageFormatRg8 -> 13l
  | ImageFormatR16 -> 14l
  | ImageFormatR8 -> 15l
  | ImageFormatRgba16Snorm -> 16l
  | ImageFormatRg16Snorm -> 17l
  | ImageFormatRg8Snorm -> 18l
  | ImageFormatR16Snorm -> 19l
  | ImageFormatR8Snorm -> 20l
  | ImageFormatRgba32i -> 21l
  | ImageFormatRgba16i -> 22l
  | ImageFormatRgba8i -> 23l
  | ImageFormatR32i -> 24l
  | ImageFormatRg32i -> 25l
  | ImageFormatRg16i -> 26l
  | ImageFormatRg8i -> 27l
  | ImageFormatR16i -> 28l
  | ImageFormatR8i -> 29l
  | ImageFormatRgba32ui -> 30l
  | ImageFormatRgba16ui -> 31l
  | ImageFormatRgba8ui -> 32l
  | ImageFormatR32ui -> 33l
  | ImageFormatRgb10a2ui -> 34l
  | ImageFormatRg32ui -> 35l
  | ImageFormatRg16ui -> 36l
  | ImageFormatRg8ui -> 37l
  | ImageFormatR16ui -> 38l
  | ImageFormatR8ui -> 39l;;
let word_of_image_channel_order (enum : image_channel_order) =
  match enum with
  | ImageChannelOrderR -> 0l
  | ImageChannelOrderA -> 1l
  | ImageChannelOrderRG -> 2l
  | ImageChannelOrderRA -> 3l
  | ImageChannelOrderRGB -> 4l
  | ImageChannelOrderRGBA -> 5l
  | ImageChannelOrderBGRA -> 6l
  | ImageChannelOrderARGB -> 7l
  | ImageChannelOrderIntensity -> 8l
  | ImageChannelOrderLuminance -> 9l
  | ImageChannelOrderRx -> 10l
  | ImageChannelOrderRGx -> 11l
  | ImageChannelOrderRGBx -> 12l
  | ImageChannelOrderDepth -> 13l
  | ImageChannelOrderDepthStencil -> 14l
  | ImageChannelOrderSRGB -> 15l
  | ImageChannelOrderSRGBx -> 16l
  | ImageChannelOrderSRGBA -> 17l
  | ImageChannelOrderSBGRA -> 18l
  | ImageChannelOrderABGR -> 19l;;
let word_of_image_channel_data_type (enum : image_channel_data_type) =
  match enum with
  | ImageChannelDataTypeSnormInt8 -> 0l
  | ImageChannelDataTypeSnormInt16 -> 1l
  | ImageChannelDataTypeUnormInt8 -> 2l
  | ImageChannelDataTypeUnormInt16 -> 3l
  | ImageChannelDataTypeUnormShort565 -> 4l
  | ImageChannelDataTypeUnormShort555 -> 5l
  | ImageChannelDataTypeUnormInt101010 -> 6l
  | ImageChannelDataTypeSignedInt8 -> 7l
  | ImageChannelDataTypeSignedInt16 -> 8l
  | ImageChannelDataTypeSignedInt32 -> 9l
  | ImageChannelDataTypeUnsignedInt8 -> 10l
  | ImageChannelDataTypeUnsignedInt16 -> 11l
  | ImageChannelDataTypeUnsignedInt32 -> 12l
  | ImageChannelDataTypeHalfFloat -> 13l
  | ImageChannelDataTypeFloat -> 14l
  | ImageChannelDataTypeUnormInt24 -> 15l
  | ImageChannelDataTypeUnormInt101010_2 -> 16l;;
let word_of_f_p_rounding_mode (enum : f_p_rounding_mode) =
  match enum with
  | FPRoundingModeRTE -> 0l
  | FPRoundingModeRTZ -> 1l
  | FPRoundingModeRTP -> 2l
  | FPRoundingModeRTN -> 3l;;
let word_of_linkage_type (enum : linkage_type) =
  match enum with | LinkageTypeExport -> 0l | LinkageTypeImport -> 1l;;
let word_of_access_qualifier (enum : access_qualifier) =
  match enum with
  | AccessQualifierReadOnly -> 0l
  | AccessQualifierWriteOnly -> 1l
  | AccessQualifierReadWrite -> 2l;;
let word_of_function_parameter_attribute (enum :
                                          function_parameter_attribute)
                                         =
  match enum with
  | FunctionParameterAttributeZext -> 0l
  | FunctionParameterAttributeSext -> 1l
  | FunctionParameterAttributeByVal -> 2l
  | FunctionParameterAttributeSret -> 3l
  | FunctionParameterAttributeNoAlias -> 4l
  | FunctionParameterAttributeNoCapture -> 5l
  | FunctionParameterAttributeNoWrite -> 6l
  | FunctionParameterAttributeNoReadWrite -> 7l;;
let word_of_built_in (enum : built_in) =
  match enum with
  | BuiltInPosition -> 0l
  | BuiltInPointSize -> 1l
  | BuiltInClipDistance -> 3l
  | BuiltInCullDistance -> 4l
  | BuiltInVertexId -> 5l
  | BuiltInInstanceId -> 6l
  | BuiltInPrimitiveId -> 7l
  | BuiltInInvocationId -> 8l
  | BuiltInLayer -> 9l
  | BuiltInViewportIndex -> 10l
  | BuiltInTessLevelOuter -> 11l
  | BuiltInTessLevelInner -> 12l
  | BuiltInTessCoord -> 13l
  | BuiltInPatchVertices -> 14l
  | BuiltInFragCoord -> 15l
  | BuiltInPointCoord -> 16l
  | BuiltInFrontFacing -> 17l
  | BuiltInSampleId -> 18l
  | BuiltInSamplePosition -> 19l
  | BuiltInSampleMask -> 20l
  | BuiltInFragDepth -> 22l
  | BuiltInHelperInvocation -> 23l
  | BuiltInNumWorkgroups -> 24l
  | BuiltInWorkgroupSize -> 25l
  | BuiltInWorkgroupId -> 26l
  | BuiltInLocalInvocationId -> 27l
  | BuiltInGlobalInvocationId -> 28l
  | BuiltInLocalInvocationIndex -> 29l
  | BuiltInWorkDim -> 30l
  | BuiltInGlobalSize -> 31l
  | BuiltInEnqueuedWorkgroupSize -> 32l
  | BuiltInGlobalOffset -> 33l
  | BuiltInGlobalLinearId -> 34l
  | BuiltInSubgroupSize -> 36l
  | BuiltInSubgroupMaxSize -> 37l
  | BuiltInNumSubgroups -> 38l
  | BuiltInNumEnqueuedSubgroups -> 39l
  | BuiltInSubgroupId -> 40l
  | BuiltInSubgroupLocalInvocationId -> 41l
  | BuiltInVertexIndex -> 42l
  | BuiltInInstanceIndex -> 43l
  | BuiltInSubgroupEqMaskKHR -> 4416l
  | BuiltInSubgroupGeMaskKHR -> 4417l
  | BuiltInSubgroupGtMaskKHR -> 4418l
  | BuiltInSubgroupLeMaskKHR -> 4419l
  | BuiltInSubgroupLtMaskKHR -> 4420l
  | BuiltInBaseVertex -> 4424l
  | BuiltInBaseInstance -> 4425l
  | BuiltInDrawIndex -> 4426l;;
let word_of_scope (enum : scope) =
  match enum with
  | ScopeCrossDevice -> 0l
  | ScopeDevice -> 1l
  | ScopeWorkgroup -> 2l
  | ScopeSubgroup -> 3l
  | ScopeInvocation -> 4l;;
let word_of_group_operation (enum : group_operation) =
  match enum with
  | GroupOperationReduce -> 0l
  | GroupOperationInclusiveScan -> 1l
  | GroupOperationExclusiveScan -> 2l;;
let word_of_kernel_enqueue_flags (enum : kernel_enqueue_flags) =
  match enum with
  | KernelEnqueueFlagsNoWait -> 0l
  | KernelEnqueueFlagsWaitKernel -> 1l
  | KernelEnqueueFlagsWaitWorkGroup -> 2l;;
let word_of_capability (enum : capability) =
  match enum with
  | CapabilityMatrix -> 0l
  | CapabilityShader -> 1l
  | CapabilityGeometry -> 2l
  | CapabilityTessellation -> 3l
  | CapabilityAddresses -> 4l
  | CapabilityLinkage -> 5l
  | CapabilityKernel -> 6l
  | CapabilityVector16 -> 7l
  | CapabilityFloat16Buffer -> 8l
  | CapabilityFloat16 -> 9l
  | CapabilityFloat64 -> 10l
  | CapabilityInt64 -> 11l
  | CapabilityInt64Atomics -> 12l
  | CapabilityImageBasic -> 13l
  | CapabilityImageReadWrite -> 14l
  | CapabilityImageMipmap -> 15l
  | CapabilityPipes -> 17l
  | CapabilityGroups -> 18l
  | CapabilityDeviceEnqueue -> 19l
  | CapabilityLiteralSampler -> 20l
  | CapabilityAtomicStorage -> 21l
  | CapabilityInt16 -> 22l
  | CapabilityTessellationPointSize -> 23l
  | CapabilityGeometryPointSize -> 24l
  | CapabilityImageGatherExtended -> 25l
  | CapabilityStorageImageMultisample -> 27l
  | CapabilityUniformBufferArrayDynamicIndexing -> 28l
  | CapabilitySampledImageArrayDynamicIndexing -> 29l
  | CapabilityStorageBufferArrayDynamicIndexing -> 30l
  | CapabilityStorageImageArrayDynamicIndexing -> 31l
  | CapabilityClipDistance -> 32l
  | CapabilityCullDistance -> 33l
  | CapabilityImageCubeArray -> 34l
  | CapabilitySampleRateShading -> 35l
  | CapabilityImageRect -> 36l
  | CapabilitySampledRect -> 37l
  | CapabilityGenericPointer -> 38l
  | CapabilityInt8 -> 39l
  | CapabilityInputAttachment -> 40l
  | CapabilitySparseResidency -> 41l
  | CapabilityMinLod -> 42l
  | CapabilitySampled1D -> 43l
  | CapabilityImage1D -> 44l
  | CapabilitySampledCubeArray -> 45l
  | CapabilitySampledBuffer -> 46l
  | CapabilityImageBuffer -> 47l
  | CapabilityImageMSArray -> 48l
  | CapabilityStorageImageExtendedFormats -> 49l
  | CapabilityImageQuery -> 50l
  | CapabilityDerivativeControl -> 51l
  | CapabilityInterpolationFunction -> 52l
  | CapabilityTransformFeedback -> 53l
  | CapabilityGeometryStreams -> 54l
  | CapabilityStorageImageReadWithoutFormat -> 55l
  | CapabilityStorageImageWriteWithoutFormat -> 56l
  | CapabilityMultiViewport -> 57l
  | CapabilitySubgroupDispatch -> 58l
  | CapabilityNamedBarrier -> 59l
  | CapabilityPipeStorage -> 60l
  | CapabilitySubgroupBallotKHR -> 4423l
  | CapabilityDrawParameters -> 4427l;;
let words_of_decoration (enum : decoration) =
  match enum with
  | DecorationRelaxedPrecision -> [ 0l ]
  | DecorationSpecId a -> [ 1l; word_of_int a ]
  | DecorationBlock -> [ 2l ]
  | DecorationBufferBlock -> [ 3l ]
  | DecorationRowMajor -> [ 4l ]
  | DecorationColMajor -> [ 5l ]
  | DecorationArrayStride a -> [ 6l; word_of_int a ]
  | DecorationMatrixStride a -> [ 7l; word_of_int a ]
  | DecorationGLSLShared -> [ 8l ]
  | DecorationGLSLPacked -> [ 9l ]
  | DecorationCPacked -> [ 10l ]
  | DecorationBuiltIn a -> [ 11l; word_of_built_in a ]
  | DecorationNoPerspective -> [ 13l ]
  | DecorationFlat -> [ 14l ]
  | DecorationPatch -> [ 15l ]
  | DecorationCentroid -> [ 16l ]
  | DecorationSample -> [ 17l ]
  | DecorationInvariant -> [ 18l ]
  | DecorationRestrict -> [ 19l ]
  | DecorationAliased -> [ 20l ]
  | DecorationVolatile -> [ 21l ]
  | DecorationConstant -> [ 22l ]
  | DecorationCoherent -> [ 23l ]
  | DecorationNonWritable -> [ 24l ]
  | DecorationNonReadable -> [ 25l ]
  | DecorationUniform -> [ 26l ]
  | DecorationSaturatedConversion -> [ 28l ]
  | DecorationStream a -> [ 29l; word_of_int a ]
  | DecorationLocation a -> [ 30l; word_of_int a ]
  | DecorationComponent a -> [ 31l; word_of_int a ]
  | DecorationIndex a -> [ 32l; word_of_int a ]
  | DecorationBinding a -> [ 33l; word_of_int a ]
  | DecorationDescriptorSet a -> [ 34l; word_of_int a ]
  | DecorationOffset a -> [ 35l; word_of_int a ]
  | DecorationXfbBuffer a -> [ 36l; word_of_int a ]
  | DecorationXfbStride a -> [ 37l; word_of_int a ]
  | DecorationFuncParamAttr a ->
      [ 38l; word_of_function_parameter_attribute a ]
  | DecorationFPRoundingMode a -> [ 39l; word_of_f_p_rounding_mode a ]
  | DecorationFPFastMathMode a -> [ 40l; word_of_f_p_fast_math_mode a ]
  | DecorationLinkageAttributes (a, b) ->
      41l :: ((words_of_string a) @ [ word_of_linkage_type b ])
  | DecorationNoContraction -> [ 42l ]
  | DecorationInputAttachmentIndex a -> [ 43l; word_of_int a ]
  | DecorationAlignment a -> [ 44l; word_of_int a ]
  | DecorationMaxByteOffset a -> [ 45l; word_of_int a ];;
let rec words_of_spec_op : spec_op -> word list =
  fun spec_op ->
    match spec_op with
    | `SConvert a -> [ 0x0072l; word_of_id a ]
    | `FConvert a -> [ 0x0073l; word_of_id a ]
    | `SNegate a -> [ 0x007el; word_of_id a ]
    | `Not a -> [ 0x00c8l; word_of_id a ]
    | `IAdd (a, b) -> [ 0x0080l; word_of_id a; word_of_id b ]
    | `ISub (a, b) -> [ 0x0082l; word_of_id a; word_of_id b ]
    | `IMul (a, b) -> [ 0x0084l; word_of_id a; word_of_id b ]
    | `UDiv (a, b) -> [ 0x0086l; word_of_id a; word_of_id b ]
    | `SDiv (a, b) -> [ 0x0087l; word_of_id a; word_of_id b ]
    | `UMod (a, b) -> [ 0x0089l; word_of_id a; word_of_id b ]
    | `SRem (a, b) -> [ 0x008al; word_of_id a; word_of_id b ]
    | `SMod (a, b) -> [ 0x008bl; word_of_id a; word_of_id b ]
    | `ShiftRightLogical (a, b) -> [ 0x00c2l; word_of_id a; word_of_id b ]
    | `ShiftRightArithmetic (a, b) -> [ 0x00c3l; word_of_id a; word_of_id b ]
    | `ShiftLeftLogical (a, b) -> [ 0x00c4l; word_of_id a; word_of_id b ]
    | `BitwiseOr (a, b) -> [ 0x00c5l; word_of_id a; word_of_id b ]
    | `BitwiseXor (a, b) -> [ 0x00c6l; word_of_id a; word_of_id b ]
    | `BitwiseAnd (a, b) -> [ 0x00c7l; word_of_id a; word_of_id b ]
    | `VectorShuffle (a, b, c) ->
        0x004fl ::
          ([ word_of_id a; word_of_id b ] @ (List.map word_of_int c))
    | `CompositeExtract (a, b) ->
        0x0051l :: ([ word_of_id a ] @ (List.map word_of_int b))
    | `CompositeInsert (a, b, c) ->
        0x0052l ::
          ([ word_of_id a; word_of_id b ] @ (List.map word_of_int c))
    | `LogicalOr (a, b) -> [ 0x00a6l; word_of_id a; word_of_id b ]
    | `LogicalAnd (a, b) -> [ 0x00a7l; word_of_id a; word_of_id b ]
    | `LogicalNot a -> [ 0x00a8l; word_of_id a ]
    | `LogicalEqual (a, b) -> [ 0x00a4l; word_of_id a; word_of_id b ]
    | `LogicalNotEqual (a, b) -> [ 0x00a5l; word_of_id a; word_of_id b ]
    | `Select (a, b, c) ->
        [ 0x00a9l; word_of_id a; word_of_id b; word_of_id c ]
    | `IEqual (a, b) -> [ 0x00aal; word_of_id a; word_of_id b ]
    | `INotEqual (a, b) -> [ 0x00abl; word_of_id a; word_of_id b ]
    | `ULessThan (a, b) -> [ 0x00b0l; word_of_id a; word_of_id b ]
    | `SLessThan (a, b) -> [ 0x00b1l; word_of_id a; word_of_id b ]
    | `UGreaterThan (a, b) -> [ 0x00acl; word_of_id a; word_of_id b ]
    | `SGreaterThan (a, b) -> [ 0x00adl; word_of_id a; word_of_id b ]
    | `ULessThanEqual (a, b) -> [ 0x00b2l; word_of_id a; word_of_id b ]
    | `SLessThanEqual (a, b) -> [ 0x00b3l; word_of_id a; word_of_id b ]
    | `UGreaterThanEqual (a, b) -> [ 0x00ael; word_of_id a; word_of_id b ]
    | `SGreaterThanEqual (a, b) -> [ 0x00afl; word_of_id a; word_of_id b ]
    | `QuantizeToF16 a -> [ 0x0074l; word_of_id a ]
    | `ConvertFToS a -> [ 0x006el; word_of_id a ]
    | `ConvertSToF a -> [ 0x006fl; word_of_id a ]
    | `ConvertFToU a -> [ 0x006dl; word_of_id a ]
    | `ConvertUToF a -> [ 0x0070l; word_of_id a ]
    | `UConvert a -> [ 0x0071l; word_of_id a ]
    | `ConvertPtrToU a -> [ 0x0075l; word_of_id a ]
    | `ConvertUToPtr a -> [ 0x0078l; word_of_id a ]
    | `GenericCastToPtr a -> [ 0x007al; word_of_id a ]
    | `PtrCastToGeneric a -> [ 0x0079l; word_of_id a ]
    | `Bitcast a -> [ 0x007cl; word_of_id a ]
    | `FNegate a -> [ 0x007fl; word_of_id a ]
    | `FAdd (a, b) -> [ 0x0081l; word_of_id a; word_of_id b ]
    | `FSub (a, b) -> [ 0x0083l; word_of_id a; word_of_id b ]
    | `FMul (a, b) -> [ 0x0085l; word_of_id a; word_of_id b ]
    | `FDiv (a, b) -> [ 0x0088l; word_of_id a; word_of_id b ]
    | `FRem (a, b) -> [ 0x008cl; word_of_id a; word_of_id b ]
    | `FMod (a, b) -> [ 0x008dl; word_of_id a; word_of_id b ]
    | `AccessChain (a, b) ->
        0x0041l :: ([ word_of_id a ] @ (List.map word_of_id b))
    | `InBoundsAccessChain (a, b) ->
        0x0042l :: ([ word_of_id a ] @ (List.map word_of_id b))
    | `PtrAccessChain (a, b, c) ->
        0x0043l :: ([ word_of_id a; word_of_id b ] @ (List.map word_of_id c))
    | `InBoundsPtrAccessChain (a, b, c) ->
        0x0046l :: ([ word_of_id a; word_of_id b ] @ (List.map word_of_id c));;
let rec words_and_id_of_op :
  int IdMap.t -> op -> ((int IdMap.t) * (id option) * (word list)) =
  fun size_map op ->
    let lookup_size (id : id) =
      if IdMap.mem id size_map
      then IdMap.find id size_map
      else
        (let print_ids k _ = print_endline @@ (Int32.to_string k)
         in (IdMap.iter print_ids size_map; raise (Id_not_found id))) in
    let build_op_words code operand_words =
      let shifted_word_count = ((List.length operand_words) + 1) lsl 16
      in
        (Int32.logor code (Int32.of_int shifted_word_count)) :: operand_words
    in
      match op with
      | `OpNop -> (size_map, None, (build_op_words 0x0000l []))
      | `OpUndef (a, b) ->
          (size_map, (Some b),
           (build_op_words 0x0001l [ word_of_id a; word_of_id b ]))
      | `OpSourceContinued a ->
          (size_map, None, (build_op_words 0x0002l (words_of_string a)))
      | `OpSource (a, b, c, d) ->
          (size_map, None,
           (build_op_words 0x0003l
              (([ word_of_source_language a; word_of_int b ] @
                  (list_of_option (apply_option word_of_id c)))
                 @ (list_of_list_option (apply_option words_of_string d)))))
      | `OpSourceExtension a ->
          (size_map, None, (build_op_words 0x0004l (words_of_string a)))
      | `OpName (a, b) ->
          (size_map, None,
           (build_op_words 0x0005l ([ word_of_id a ] @ (words_of_string b))))
      | `OpMemberName (a, b, c) ->
          (size_map, None,
           (build_op_words 0x0006l
              ([ word_of_id a; word_of_int b ] @ (words_of_string c))))
      | `OpString (a, b) ->
          (size_map, None,
           (build_op_words 0x0007l ([ word_of_id a ] @ (words_of_string b))))
      | `OpLine (a, b, c) ->
          (size_map, None,
           (build_op_words 0x0008l
              [ word_of_id a; word_of_int b; word_of_int c ]))
      | `OpExtension a ->
          (size_map, None, (build_op_words 0x000al (words_of_string a)))
      | `OpExtInstImport (a, b) ->
          (size_map, None,
           (build_op_words 0x000bl ([ word_of_id a ] @ (words_of_string b))))
      | `OpExtInst (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x000cl
              ([ word_of_id a; word_of_id b; word_of_id c ] @
                 ((fun f -> f ()) d))))
      | `OpMemoryModel (a, b) ->
          (size_map, None,
           (build_op_words 0x000el
              [ word_of_addressing_model a; word_of_memory_model b ]))
      | `OpEntryPoint (a, b, c, d) ->
          (size_map, None,
           (build_op_words 0x000fl
              (([ word_of_execution_model a; word_of_id b ] @
                  (words_of_string c))
                 @ (List.map word_of_id d))))
      | `OpExecutionMode (a, b) ->
          (size_map, None,
           (build_op_words 0x0010l
              ([ word_of_id a ] @ (words_of_execution_mode b))))
      | `OpCapability a ->
          (size_map, None, (build_op_words 0x0011l [ word_of_capability a ]))
      | `OpTypeVoid a ->
          (size_map, None, (build_op_words 0x0013l [ word_of_id a ]))
      | `OpTypeBool a ->
          (size_map, None, (build_op_words 0x0014l [ word_of_id a ]))
      | `OpTypeInt (a, b, c) ->
          ((IdMap.add a (Int32.to_int b) size_map), None,
           (build_op_words 0x0015l
              [ word_of_id a; word_of_int b; word_of_int c ]))
      | `OpTypeFloat (a, b) ->
          ((IdMap.add a (Int32.to_int b) size_map), None,
           (build_op_words 0x0016l [ word_of_id a; word_of_int b ]))
      | `OpTypeVector (a, b, c) ->
          (size_map, None,
           (build_op_words 0x0017l
              [ word_of_id a; word_of_id b; word_of_int c ]))
      | `OpTypeMatrix (a, b, c) ->
          (size_map, None,
           (build_op_words 0x0018l
              [ word_of_id a; word_of_id b; word_of_int c ]))
      | `OpTypeImage (a, b, c, d, e, f, g, h, i) ->
          (size_map, None,
           (build_op_words 0x0019l
              ([ word_of_id a; word_of_id b; word_of_dim c; word_of_int d;
                 word_of_int e; word_of_int f; word_of_int g;
                 word_of_image_format h ] @
                 (list_of_option (apply_option word_of_access_qualifier i)))))
      | `OpTypeSampler a ->
          (size_map, None, (build_op_words 0x001al [ word_of_id a ]))
      | `OpTypeSampledImage (a, b) ->
          (size_map, None,
           (build_op_words 0x001bl [ word_of_id a; word_of_id b ]))
      | `OpTypeArray (a, b, c) ->
          (size_map, None,
           (build_op_words 0x001cl
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpTypeRuntimeArray (a, b) ->
          (size_map, None,
           (build_op_words 0x001dl [ word_of_id a; word_of_id b ]))
      | `OpTypeStruct (a, b) ->
          (size_map, None,
           (build_op_words 0x001el
              ([ word_of_id a ] @ (List.map word_of_id b))))
      | `OpTypeOpaque (a, b) ->
          (size_map, None,
           (build_op_words 0x001fl ([ word_of_id a ] @ (words_of_string b))))
      | `OpTypePointer (a, b, c) ->
          (size_map, None,
           (build_op_words 0x0020l
              [ word_of_id a; word_of_storage_class b; word_of_id c ]))
      | `OpTypeFunction (a, b, c) ->
          (size_map, None,
           (build_op_words 0x0021l
              ([ word_of_id a; word_of_id b ] @ (List.map word_of_id c))))
      | `OpTypeEvent a ->
          (size_map, None, (build_op_words 0x0022l [ word_of_id a ]))
      | `OpTypeDeviceEvent a ->
          (size_map, None, (build_op_words 0x0023l [ word_of_id a ]))
      | `OpTypeReserveId a ->
          (size_map, None, (build_op_words 0x0024l [ word_of_id a ]))
      | `OpTypeQueue a ->
          (size_map, None, (build_op_words 0x0025l [ word_of_id a ]))
      | `OpTypePipe (a, b) ->
          (size_map, None,
           (build_op_words 0x0026l
              [ word_of_id a; word_of_access_qualifier b ]))
      | `OpTypeForwardPointer (a, b) ->
          (size_map, None,
           (build_op_words 0x0027l [ word_of_id a; word_of_storage_class b ]))
      | `OpConstantTrue (a, b) ->
          (size_map, (Some b),
           (build_op_words 0x0029l [ word_of_id a; word_of_id b ]))
      | `OpConstantFalse (a, b) ->
          (size_map, (Some b),
           (build_op_words 0x002al [ word_of_id a; word_of_id b ]))
      | `OpConstant (a, b, c) ->
          ((IdMap.add b (IdMap.find a size_map) size_map), (Some b),
           (build_op_words 0x002bl
              ([ word_of_id a; word_of_id b ] @
                 (words_of_context_dependent_number (lookup_size a) c))))
      | `OpConstantComposite (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x002cl
              ([ word_of_id a; word_of_id b ] @ (List.map word_of_id c))))
      | `OpConstantSampler (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x002dl
              [ word_of_id a; word_of_id b;
                word_of_sampler_addressing_mode c; word_of_int d;
                word_of_sampler_filter_mode e ]))
      | `OpConstantNull (a, b) ->
          (size_map, (Some b),
           (build_op_words 0x002el [ word_of_id a; word_of_id b ]))
      | `OpSpecConstantTrue (a, b) ->
          (size_map, (Some b),
           (build_op_words 0x0030l [ word_of_id a; word_of_id b ]))
      | `OpSpecConstantFalse (a, b) ->
          (size_map, (Some b),
           (build_op_words 0x0031l [ word_of_id a; word_of_id b ]))
      | `OpSpecConstant (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0032l
              ([ word_of_id a; word_of_id b ] @
                 (words_of_context_dependent_number (lookup_size a) c))))
      | `OpSpecConstantComposite (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0033l
              ([ word_of_id a; word_of_id b ] @ (List.map word_of_id c))))
      | `OpSpecConstantOp (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0034l
              ([ word_of_id a; word_of_id b ] @ (words_of_spec_op c))))
      | `OpFunction (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0036l
              [ word_of_id a; word_of_id b; word_of_function_control c;
                word_of_id d ]))
      | `OpFunctionParameter (a, b) ->
          (size_map, (Some b),
           (build_op_words 0x0037l [ word_of_id a; word_of_id b ]))
      | `OpFunctionEnd -> (size_map, None, (build_op_words 0x0038l []))
      | `OpFunctionCall (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0039l
              ([ word_of_id a; word_of_id b; word_of_id c ] @
                 (List.map word_of_id d))))
      | `OpVariable (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x003bl
              ([ word_of_id a; word_of_id b; word_of_storage_class c ] @
                 (list_of_option (apply_option word_of_id d)))))
      | `OpImageTexelPointer (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x003cl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e ]))
      | `OpLoad (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x003dl
              ([ word_of_id a; word_of_id b; word_of_id c ] @
                 (list_of_list_option (apply_option words_of_memory_access d)))))
      | `OpStore (a, b, c) ->
          (size_map, None,
           (build_op_words 0x003el
              ([ word_of_id a; word_of_id b ] @
                 (list_of_list_option (apply_option words_of_memory_access c)))))
      | `OpCopyMemory (a, b, c) ->
          (size_map, None,
           (build_op_words 0x003fl
              ([ word_of_id a; word_of_id b ] @
                 (list_of_list_option (apply_option words_of_memory_access c)))))
      | `OpCopyMemorySized (a, b, c, d) ->
          (size_map, None,
           (build_op_words 0x0040l
              ([ word_of_id a; word_of_id b; word_of_id c ] @
                 (list_of_list_option (apply_option words_of_memory_access d)))))
      | `OpAccessChain (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0041l
              ([ word_of_id a; word_of_id b; word_of_id c ] @
                 (List.map word_of_id d))))
      | `OpInBoundsAccessChain (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0042l
              ([ word_of_id a; word_of_id b; word_of_id c ] @
                 (List.map word_of_id d))))
      | `OpPtrAccessChain (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x0043l
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
                 (List.map word_of_id e))))
      | `OpArrayLength (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0044l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_int d ]))
      | `OpGenericPtrMemSemantics (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0045l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpInBoundsPtrAccessChain (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x0046l
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
                 (List.map word_of_id e))))
      | `OpDecorate (a, b) ->
          (size_map, None,
           (build_op_words 0x0047l
              ([ word_of_id a ] @ (words_of_decoration b))))
      | `OpMemberDecorate (a, b, c) ->
          (size_map, None,
           (build_op_words 0x0048l
              ([ word_of_id a; word_of_int b ] @ (words_of_decoration c))))
      | `OpDecorationGroup a ->
          (size_map, None, (build_op_words 0x0049l [ word_of_id a ]))
      | `OpGroupDecorate (a, b) ->
          (size_map, None,
           (build_op_words 0x004al
              ([ word_of_id a ] @ (List.map word_of_id b))))
      | `OpGroupMemberDecorate (a, b) ->
          (size_map, None,
           (build_op_words 0x004bl
              ([ word_of_id a ] @
                 (List.concat
                    (List.map words_of_pair_id_ref_literal_integer b)))))
      | `OpVectorExtractDynamic (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x004dl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpVectorInsertDynamic (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x004el
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e ]))
      | `OpVectorShuffle (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x004fl
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
                 (List.map word_of_int e))))
      | `OpCompositeConstruct (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0050l
              ([ word_of_id a; word_of_id b ] @ (List.map word_of_id c))))
      | `OpCompositeExtract (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0051l
              ([ word_of_id a; word_of_id b; word_of_id c ] @
                 (List.map word_of_int d))))
      | `OpCompositeInsert (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x0052l
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
                 (List.map word_of_int e))))
      | `OpCopyObject (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0053l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpTranspose (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0054l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpSampledImage (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0056l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpImageSampleImplicitLod (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x0057l
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
                 (list_of_list_option
                    (apply_option words_of_image_operands e)))))
      | `OpImageSampleExplicitLod (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x0058l
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
                 (words_of_image_operands e))))
      | `OpImageSampleDrefImplicitLod (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x0059l
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                 word_of_id e ] @
                 (list_of_list_option
                    (apply_option words_of_image_operands f)))))
      | `OpImageSampleDrefExplicitLod (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x005al
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                 word_of_id e ] @ (words_of_image_operands f))))
      | `OpImageSampleProjImplicitLod (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x005bl
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
                 (list_of_list_option
                    (apply_option words_of_image_operands e)))))
      | `OpImageSampleProjExplicitLod (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x005cl
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
                 (words_of_image_operands e))))
      | `OpImageSampleProjDrefImplicitLod (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x005dl
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                 word_of_id e ] @
                 (list_of_list_option
                    (apply_option words_of_image_operands f)))))
      | `OpImageSampleProjDrefExplicitLod (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x005el
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                 word_of_id e ] @ (words_of_image_operands f))))
      | `OpImageFetch (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x005fl
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
                 (list_of_list_option
                    (apply_option words_of_image_operands e)))))
      | `OpImageGather (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x0060l
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                 word_of_id e ] @
                 (list_of_list_option
                    (apply_option words_of_image_operands f)))))
      | `OpImageDrefGather (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x0061l
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                 word_of_id e ] @
                 (list_of_list_option
                    (apply_option words_of_image_operands f)))))
      | `OpImageRead (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x0062l
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
                 (list_of_list_option
                    (apply_option words_of_image_operands e)))))
      | `OpImageWrite (a, b, c, d) ->
          (size_map, None,
           (build_op_words 0x0063l
              ([ word_of_id a; word_of_id b; word_of_id c ] @
                 (list_of_list_option
                    (apply_option words_of_image_operands d)))))
      | `OpImage (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0064l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpImageQueryFormat (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0065l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpImageQueryOrder (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0066l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpImageQuerySizeLod (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0067l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpImageQuerySize (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0068l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpImageQueryLod (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0069l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpImageQueryLevels (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x006al
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpImageQuerySamples (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x006bl
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpConvertFToU (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x006dl
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpConvertFToS (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x006el
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpConvertSToF (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x006fl
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpConvertUToF (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0070l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpUConvert (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0071l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpSConvert (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0072l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpFConvert (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0073l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpQuantizeToF16 (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0074l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpConvertPtrToU (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0075l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpSatConvertSToU (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0076l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpSatConvertUToS (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0077l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpConvertUToPtr (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0078l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpPtrCastToGeneric (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0079l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpGenericCastToPtr (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x007al
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpGenericCastToPtrExplicit (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x007bl
              [ word_of_id a; word_of_id b; word_of_id c;
                word_of_storage_class d ]))
      | `OpBitcast (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x007cl
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpSNegate (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x007el
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpFNegate (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x007fl
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpIAdd (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0080l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpFAdd (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0081l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpISub (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0082l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpFSub (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0083l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpIMul (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0084l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpFMul (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0085l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpUDiv (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0086l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpSDiv (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0087l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpFDiv (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0088l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpUMod (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0089l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpSRem (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x008al
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpSMod (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x008bl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpFRem (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x008cl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpFMod (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x008dl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpVectorTimesScalar (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x008el
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpMatrixTimesScalar (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x008fl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpVectorTimesMatrix (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0090l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpMatrixTimesVector (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0091l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpMatrixTimesMatrix (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0092l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpOuterProduct (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0093l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpDot (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0094l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpIAddCarry (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0095l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpISubBorrow (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0096l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpUMulExtended (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0097l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpSMulExtended (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0098l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpAny (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x009al
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpAll (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x009bl
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpIsNan (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x009cl
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpIsInf (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x009dl
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpIsFinite (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x009el
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpIsNormal (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x009fl
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpSignBitSet (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x00a0l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpLessOrGreater (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00a1l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpOrdered (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00a2l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpUnordered (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00a3l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpLogicalEqual (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00a4l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpLogicalNotEqual (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00a5l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpLogicalOr (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00a6l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpLogicalAnd (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00a7l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpLogicalNot (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x00a8l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpSelect (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x00a9l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e ]))
      | `OpIEqual (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00aal
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpINotEqual (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00abl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpUGreaterThan (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00acl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpSGreaterThan (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00adl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpUGreaterThanEqual (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00ael
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpSGreaterThanEqual (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00afl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpULessThan (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00b0l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpSLessThan (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00b1l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpULessThanEqual (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00b2l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpSLessThanEqual (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00b3l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpFOrdEqual (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00b4l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpFUnordEqual (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00b5l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpFOrdNotEqual (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00b6l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpFUnordNotEqual (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00b7l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpFOrdLessThan (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00b8l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpFUnordLessThan (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00b9l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpFOrdGreaterThan (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00bal
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpFUnordGreaterThan (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00bbl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpFOrdLessThanEqual (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00bcl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpFUnordLessThanEqual (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00bdl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpFOrdGreaterThanEqual (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00bel
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpFUnordGreaterThanEqual (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00bfl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpShiftRightLogical (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00c2l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpShiftRightArithmetic (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00c3l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpShiftLeftLogical (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00c4l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpBitwiseOr (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00c5l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpBitwiseXor (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00c6l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpBitwiseAnd (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x00c7l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpNot (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x00c8l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpBitFieldInsert (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x00c9l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f ]))
      | `OpBitFieldSExtract (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x00cal
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e ]))
      | `OpBitFieldUExtract (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x00cbl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e ]))
      | `OpBitReverse (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x00ccl
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpBitCount (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x00cdl
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpDPdx (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x00cfl
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpDPdy (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x00d0l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpFwidth (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x00d1l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpDPdxFine (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x00d2l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpDPdyFine (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x00d3l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpFwidthFine (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x00d4l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpDPdxCoarse (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x00d5l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpDPdyCoarse (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x00d6l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpFwidthCoarse (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x00d7l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpEmitVertex -> (size_map, None, (build_op_words 0x00dal []))
      | `OpEndPrimitive -> (size_map, None, (build_op_words 0x00dbl []))
      | `OpEmitStreamVertex a ->
          (size_map, None, (build_op_words 0x00dcl [ word_of_id a ]))
      | `OpEndStreamPrimitive a ->
          (size_map, None, (build_op_words 0x00ddl [ word_of_id a ]))
      | `OpControlBarrier (a, b, c) ->
          (size_map, None,
           (build_op_words 0x00e0l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpMemoryBarrier (a, b) ->
          (size_map, None,
           (build_op_words 0x00e1l [ word_of_id a; word_of_id b ]))
      | `OpAtomicLoad (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x00e3l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e ]))
      | `OpAtomicStore (a, b, c, d) ->
          (size_map, None,
           (build_op_words 0x00e4l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpAtomicExchange (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x00e5l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f ]))
      | `OpAtomicCompareExchange (a, b, c, d, e, f, g, h) ->
          (size_map, (Some b),
           (build_op_words 0x00e6l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f; word_of_id g; word_of_id h ]))
      | `OpAtomicCompareExchangeWeak (a, b, c, d, e, f, g, h) ->
          (size_map, (Some b),
           (build_op_words 0x00e7l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f; word_of_id g; word_of_id h ]))
      | `OpAtomicIIncrement (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x00e8l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e ]))
      | `OpAtomicIDecrement (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x00e9l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e ]))
      | `OpAtomicIAdd (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x00eal
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f ]))
      | `OpAtomicISub (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x00ebl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f ]))
      | `OpAtomicSMin (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x00ecl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f ]))
      | `OpAtomicUMin (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x00edl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f ]))
      | `OpAtomicSMax (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x00eel
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f ]))
      | `OpAtomicUMax (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x00efl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f ]))
      | `OpAtomicAnd (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x00f0l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f ]))
      | `OpAtomicOr (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x00f1l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f ]))
      | `OpAtomicXor (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x00f2l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f ]))
      | `OpPhi (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x00f5l
              ([ word_of_id a; word_of_id b ] @
                 (List.concat (List.map words_of_pair_id_ref_id_ref c)))))
      | `OpLoopMerge (a, b, c) ->
          (size_map, None,
           (build_op_words 0x00f6l
              ([ word_of_id a; word_of_id b ] @ (words_of_loop_control c))))
      | `OpSelectionMerge (a, b) ->
          (size_map, None,
           (build_op_words 0x00f7l
              [ word_of_id a; word_of_selection_control b ]))
      | `OpLabel a ->
          (size_map, None, (build_op_words 0x00f8l [ word_of_id a ]))
      | `OpBranch a ->
          (size_map, None, (build_op_words 0x00f9l [ word_of_id a ]))
      | `OpBranchConditional (a, b, c, d) ->
          (size_map, None,
           (build_op_words 0x00fal
              ([ word_of_id a; word_of_id b; word_of_id c ] @
                 (List.map word_of_int d))))
      | `OpSwitch (a, b, c) ->
          (size_map, None,
           (build_op_words 0x00fbl
              ([ word_of_id a; word_of_id b ] @
                 (List.concat
                    (List.map words_of_pair_literal_integer_id_ref c)))))
      | `OpKill -> (size_map, None, (build_op_words 0x00fcl []))
      | `OpReturn -> (size_map, None, (build_op_words 0x00fdl []))
      | `OpReturnValue a ->
          (size_map, None, (build_op_words 0x00fel [ word_of_id a ]))
      | `OpUnreachable -> (size_map, None, (build_op_words 0x00ffl []))
      | `OpLifetimeStart (a, b) ->
          (size_map, None,
           (build_op_words 0x0100l [ word_of_id a; word_of_int b ]))
      | `OpLifetimeStop (a, b) ->
          (size_map, None,
           (build_op_words 0x0101l [ word_of_id a; word_of_int b ]))
      | `OpGroupAsyncCopy (a, b, c, d, e, f, g, h) ->
          (size_map, (Some b),
           (build_op_words 0x0103l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f; word_of_id g; word_of_id h ]))
      | `OpGroupWaitEvents (a, b, c) ->
          (size_map, None,
           (build_op_words 0x0104l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpGroupAll (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0105l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpGroupAny (a, b, c, d) ->
          (size_map, (Some b),
           (build_op_words 0x0106l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpGroupBroadcast (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x0107l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e ]))
      | `OpGroupIAdd (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x0108l
              [ word_of_id a; word_of_id b; word_of_id c;
                word_of_group_operation d; word_of_id e ]))
      | `OpGroupFAdd (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x0109l
              [ word_of_id a; word_of_id b; word_of_id c;
                word_of_group_operation d; word_of_id e ]))
      | `OpGroupFMin (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x010al
              [ word_of_id a; word_of_id b; word_of_id c;
                word_of_group_operation d; word_of_id e ]))
      | `OpGroupUMin (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x010bl
              [ word_of_id a; word_of_id b; word_of_id c;
                word_of_group_operation d; word_of_id e ]))
      | `OpGroupSMin (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x010cl
              [ word_of_id a; word_of_id b; word_of_id c;
                word_of_group_operation d; word_of_id e ]))
      | `OpGroupFMax (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x010dl
              [ word_of_id a; word_of_id b; word_of_id c;
                word_of_group_operation d; word_of_id e ]))
      | `OpGroupUMax (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x010el
              [ word_of_id a; word_of_id b; word_of_id c;
                word_of_group_operation d; word_of_id e ]))
      | `OpGroupSMax (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x010fl
              [ word_of_id a; word_of_id b; word_of_id c;
                word_of_group_operation d; word_of_id e ]))
      | `OpReadPipe (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x0112l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f ]))
      | `OpWritePipe (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x0113l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f ]))
      | `OpReservedReadPipe (a, b, c, d, e, f, g, h) ->
          (size_map, (Some b),
           (build_op_words 0x0114l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f; word_of_id g; word_of_id h ]))
      | `OpReservedWritePipe (a, b, c, d, e, f, g, h) ->
          (size_map, (Some b),
           (build_op_words 0x0115l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f; word_of_id g; word_of_id h ]))
      | `OpReserveReadPipePackets (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x0116l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f ]))
      | `OpReserveWritePipePackets (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x0117l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f ]))
      | `OpCommitReadPipe (a, b, c, d) ->
          (size_map, None,
           (build_op_words 0x0118l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpCommitWritePipe (a, b, c, d) ->
          (size_map, None,
           (build_op_words 0x0119l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d ]))
      | `OpIsValidReserveId (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x011al
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpGetNumPipePackets (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x011bl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e ]))
      | `OpGetMaxPipePackets (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x011cl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e ]))
      | `OpGroupReserveReadPipePackets (a, b, c, d, e, f, g) ->
          (size_map, (Some b),
           (build_op_words 0x011dl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f; word_of_id g ]))
      | `OpGroupReserveWritePipePackets (a, b, c, d, e, f, g) ->
          (size_map, (Some b),
           (build_op_words 0x011el
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f; word_of_id g ]))
      | `OpGroupCommitReadPipe (a, b, c, d, e) ->
          (size_map, None,
           (build_op_words 0x011fl
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e ]))
      | `OpGroupCommitWritePipe (a, b, c, d, e) ->
          (size_map, None,
           (build_op_words 0x0120l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e ]))
      | `OpEnqueueMarker (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x0123l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f ]))
      | `OpEnqueueKernel (a, b, c, d, e, f, g, h, i, j, k, l, m) ->
          (size_map, (Some b),
           (build_op_words 0x0124l
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                 word_of_id e; word_of_id f; word_of_id g; word_of_id h;
                 word_of_id i; word_of_id j; word_of_id k; word_of_id l ] @
                 (List.map word_of_id m))))
      | `OpGetKernelNDrangeSubGroupCount (a, b, c, d, e, f, g) ->
          (size_map, (Some b),
           (build_op_words 0x0125l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f; word_of_id g ]))
      | `OpGetKernelNDrangeMaxSubGroupSize (a, b, c, d, e, f, g) ->
          (size_map, (Some b),
           (build_op_words 0x0126l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f; word_of_id g ]))
      | `OpGetKernelWorkGroupSize (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x0127l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f ]))
      | `OpGetKernelPreferredWorkGroupSizeMultiple (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x0128l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f ]))
      | `OpRetainEvent a ->
          (size_map, None, (build_op_words 0x0129l [ word_of_id a ]))
      | `OpReleaseEvent a ->
          (size_map, None, (build_op_words 0x012al [ word_of_id a ]))
      | `OpCreateUserEvent (a, b) ->
          (size_map, (Some b),
           (build_op_words 0x012bl [ word_of_id a; word_of_id b ]))
      | `OpIsValidEvent (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x012cl
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpSetUserEventStatus (a, b) ->
          (size_map, None,
           (build_op_words 0x012dl [ word_of_id a; word_of_id b ]))
      | `OpCaptureEventProfilingInfo (a, b, c) ->
          (size_map, None,
           (build_op_words 0x012el
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpGetDefaultQueue (a, b) ->
          (size_map, (Some b),
           (build_op_words 0x012fl [ word_of_id a; word_of_id b ]))
      | `OpBuildNDRange (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x0130l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e ]))
      | `OpImageSparseSampleImplicitLod (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x0131l
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
                 (list_of_list_option
                    (apply_option words_of_image_operands e)))))
      | `OpImageSparseSampleExplicitLod (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x0132l
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
                 (words_of_image_operands e))))
      | `OpImageSparseSampleDrefImplicitLod (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x0133l
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                 word_of_id e ] @
                 (list_of_list_option
                    (apply_option words_of_image_operands f)))))
      | `OpImageSparseSampleDrefExplicitLod (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x0134l
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                 word_of_id e ] @ (words_of_image_operands f))))
      | `OpImageSparseSampleProjImplicitLod (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x0135l
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
                 (list_of_list_option
                    (apply_option words_of_image_operands e)))))
      | `OpImageSparseSampleProjExplicitLod (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x0136l
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
                 (words_of_image_operands e))))
      | `OpImageSparseSampleProjDrefImplicitLod (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x0137l
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                 word_of_id e ] @
                 (list_of_list_option
                    (apply_option words_of_image_operands f)))))
      | `OpImageSparseSampleProjDrefExplicitLod (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x0138l
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                 word_of_id e ] @ (words_of_image_operands f))))
      | `OpImageSparseFetch (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x0139l
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
                 (list_of_list_option
                    (apply_option words_of_image_operands e)))))
      | `OpImageSparseGather (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x013al
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                 word_of_id e ] @
                 (list_of_list_option
                    (apply_option words_of_image_operands f)))))
      | `OpImageSparseDrefGather (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x013bl
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                 word_of_id e ] @
                 (list_of_list_option
                    (apply_option words_of_image_operands f)))))
      | `OpImageSparseTexelsResident (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x013cl
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpNoLine -> (size_map, None, (build_op_words 0x013dl []))
      | `OpAtomicFlagTestAndSet (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x013el
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e ]))
      | `OpAtomicFlagClear (a, b, c) ->
          (size_map, None,
           (build_op_words 0x013fl
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpImageSparseRead (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x0140l
              ([ word_of_id a; word_of_id b; word_of_id c; word_of_id d ] @
                 (list_of_list_option
                    (apply_option words_of_image_operands e)))))
      | `OpSizeOf (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0141l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpTypePipeStorage a ->
          (size_map, None, (build_op_words 0x0142l [ word_of_id a ]))
      | `OpConstantPipeStorage (a, b, c, d, e) ->
          (size_map, (Some b),
           (build_op_words 0x0143l
              [ word_of_id a; word_of_id b; word_of_int c; word_of_int d;
                word_of_int e ]))
      | `OpCreatePipeFromPipeStorage (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0144l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpGetKernelLocalSizeForSubgroupCount (a, b, c, d, e, f, g) ->
          (size_map, (Some b),
           (build_op_words 0x0145l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f; word_of_id g ]))
      | `OpGetKernelMaxNumSubgroups (a, b, c, d, e, f) ->
          (size_map, (Some b),
           (build_op_words 0x0146l
              [ word_of_id a; word_of_id b; word_of_id c; word_of_id d;
                word_of_id e; word_of_id f ]))
      | `OpTypeNamedBarrier a ->
          (size_map, None, (build_op_words 0x0147l [ word_of_id a ]))
      | `OpNamedBarrierInitialize (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x0148l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpMemoryNamedBarrier (a, b, c) ->
          (size_map, None,
           (build_op_words 0x0149l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpModuleProcessed a ->
          (size_map, None, (build_op_words 0x014al (words_of_string a)))
      | `OpSubgroupBallotKHR (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x1145l
              [ word_of_id a; word_of_id b; word_of_id c ]))
      | `OpSubgroupFirstInvocationKHR (a, b, c) ->
          (size_map, (Some b),
           (build_op_words 0x1146l
              [ word_of_id a; word_of_id b; word_of_id c ]));;
let compile_to_words ops =
  let rec loop map ls =
    match ls with
    | [] -> (0l, [])
    | op :: t ->
        let (map, id_opt, words) = words_and_id_of_op map op in
        let id = (match id_opt with | Some i -> i | None -> 0l) in
        let (next_id, next_words) = loop map t
        in ((max id next_id), (words @ next_words)) in
  let (max_id, op_words) = loop IdMap.empty ops in
  let header =
    [ magic_number; version_word; generator_number; Int32.add max_id 1l; 0l ]
  in header @ op_words;;
