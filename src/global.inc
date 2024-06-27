CopyData = $00b1
EnterMap = $01d7
FillMemory = $166e
SwitchSRAMBankAndLatchClockData = $3e99
PrepareRTCDataAndDisableSRAM = $3ea9
wPredefID = $cc4e
wPredefHL = $cc4f
wPredefDE = $cc51
wPredefBC = $cc53
hDMARoutine = $ff80

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
.global LoadFileByNumber
.global UpdateFileByNumber
; - utilities
.global LoadFileHeaderParamsByNumber
.global LoadAndRunFileByNumber
.global GetDirectoryEntryPointerByNumberImpl
.global LoadFileByDirectoryEntryPtr
.global LoadSimpleBinary
.global LoadSimpleBinaryHeader
.global GetNthPointerInListImpl
.global ReadEDCB
.global ReadDCB

; libfile.s
; - functions
.global StorePayload
.global DeleteFileByNumber
; - utilities
.global CoreFuncPreamble
.global CoreFuncPreambleReturn
.global WriteSimpleBinary
.global WriteEDCB

;wExecutablePtr := $cc32
;wFuncBlock := $cc3f

; overlap with Multiply/Divide subroutine scratch space.
hTemp0 := $ff95
hTemp1 := $ff96
hTemp2 := $ff97
hTemp3 := $ff98
hTemp4 := $ff99
hTemp5 := $ff9a
hTemp6 := $ff9b
hTemp7 := $ff9c
hTemp8 := $ff9d
hTemp9 := $ff9e

hCoreFuncFlags := $ffa8

CORE_BIT__IN_FRONT_END = 0
CORE_BIT__SKIP_LOAD = 1

CORE_FLAG_IN_FRONT_END = 1 << CORE_BIT__IN_FRONT_END
CORE_FLAG_SKIP_LOAD = 1 << CORE_BIT__SKIP_LOAD

