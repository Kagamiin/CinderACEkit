
; Symbols defined in the configuration file.
.import CopyData
.import EnterMap
.import Bankswitch
.import FillMemory
.import CopyString
.import LoadSAV1: far
.import SaveSAVtoSRAM1: far
.import hDMARoutine
.import DisplayChooseQuantityMenu

.import wItemQuantity
.import wMaxItemQuantity

; overlap with Multiply/Divide subroutine scratch space.
; .import hTemp0
; .import hTemp1
; .import hTemp2
; .import hTemp3
; .import hTemp4
; .import hTemp5
; .import hTemp6
; .import hTemp7
; .import hTemp8
; .import hTemp9

.import hTempA
.import hErrno

; executable.s
; .global LoadExecutable
; .global GetSegment
; .global GetExecutableName
; .global ValidateExecutable
; .global ValidateExecutableName
; .global ValidateSegment

; resident_utils.s
; .global CopyStringFromHLtoDE

; core.s
; - functions
.global FrontEnd
.global LoadAndRunFileByNumber
.global UpdateFileByNumber
; - utilities
.global LoadFileHeaderParamsByNumber
.global LoadSimpleBinaryHeaderParams
.global GetDirectoryEntryPointerByNumberImpl
.global LoadFileByDirectoryEntryPtr
.global LoadSimpleBinary
.global LoadSimpleBinaryHeader
.global GetNthPointerInListImpl
.global ReadEDCB
.global ReadDCB
.global ReadCB
; - memory definitions
.global wLoadedFilePointers
.global wFuncCache
.global wTempSP
.global wBoxHeapNextFreeArea
.global wBoxHeapMax
.global wLibToBeCalled
.global sNumFiles
.global sFuncDirectory
.global sFile1
.global FuncCacheSize: abs
.global FuncDirectorySize: abs


; libfile.s
; - functions
.global StorePayload
.global DeleteFileByNumber
; - utilities
.global CoreFuncPreamble
.global CoreFuncPreambleReturn
.global WriteSimpleBinary
.global WriteEDCB

; malloc.s
.global BoxHeapReset
.global BoxHeapUpdateMax
.global BoxHeapMalloc

;wExecutablePtr := $cc32
;wFuncBlock := $cc3f

CORE_BIT__IN_FRONT_END = 0
CORE_BIT__SKIP_LOAD = 1

CORE_FLAG_IN_FRONT_END = 1 << CORE_BIT__IN_FRONT_END
CORE_FLAG_SKIP_LOAD = 1 << CORE_BIT__SKIP_LOAD

