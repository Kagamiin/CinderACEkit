.macpack sm83isa
.include "hardware.inc"
.include "global.inc"

sNumFiles = FrontEnd + 1
.export sNumFiles
.export FuncCache
.export sFuncDirectory
.export sFile1
.export FuncCacheSize
.export FuncDirectorySize

.import __BOOTSTRAP_AUX_RUN__, __KERNEL_LOAD__, __BOOTSTRAP_LOAD__, __KERNEL_RUN__
.import __KERNEL_LOADER_LOAD__, __KERNEL_LOADER_SIZE__, __KERNEL_SIZE__

.segment "KERNEL_STORAGE"

.segment "KERNEL_TERMINATOR"
	.byte $50

.segment "KERNEL"

FrontEnd:
	ld a, $00    ; number of files
	and a
	ret z
	ld [wMaxItemQuantity], a
	call DisplayChooseQuantityMenu
	and a
	ret nz
	ld a, [wItemQuantity]
	; fallthrough
; loads the file with index a and runs it.
; will crash if called with carry set.
LoadAndRunFileByNumber:
	push af             ; save file index
	; fallthrough
; if the carry flag is set, return the pointer to the file header
GetDirectoryEntryPointerByNumberImpl:
	ld hl, sFilePointersArrayStart
	; fallthrough
; if the carry flag is set, seek to the nth word inside the list in hl
GetNthPointerInListImpl:
	ld c, a
	ld b, 0
	add hl, bc
	add hl, bc
	ret c

	call LoadFileByDirectoryEntryPtr
	ret c
	call CloseSRAM       ; close SRAM for safety
	ld de, FrontEndCleanup
	push de              ; push return address
	jp hl                ; jump to payload
FrontEndCleanup:
	pop bc               ; restore file index
	ret c                ; skip autosave if program returned carry set
	ld a, $03
	call OpenSRAMInBank  ; open SRAM
	ld a, b              ; load file index into a
	call UpdateFileByNumber
	; FrontEndSentinel will close SRAM for us
	ret


; loads a file given its directory entry pointer and returns the run address in hl
LoadFileByDirectoryEntryPtr:
	ld a, [hli]
	ld h, [hl]
	ld l, h
	; fallthrough
; loads the simple binary file at hl and returns the run address in hl
LoadSimpleBinary:
	call LoadSimpleBinaryHeaderParams
	ret c

	push de
	call CopyData
	pop hl
	ret


; returns the load (run) address in de, length in bc, and pointer to the binary data in hl
LoadSimpleBinaryHeaderParams:
	ld a, [hli]
	sub a, 1            ; is file signature valid? (header starts with 0x00 byte)
	ccf
	ret c               ; return carry set if header doesn't start with 0x00
	; fallthrough
; reads e, d, c, b from hl; advances hl
ReadEDCB:
	ld e, [hl]
	inc hl
; reads d, c, b from hl; advances hl
ReadDCB:
	ld d, [hl]          ; read load address into de
	inc hl
; reads c, b from hl; advances hl
ReadCB:
	ld c, [hl]
	inc hl
	ld b, [hl]          ; read length into bc
	inc hl
	ret

; loads the file with index a (but doesn't run it)
; the run address is returned in hl
LoadFileByNumber:
	scf
	call GetDirectoryEntryPointerByNumberImpl
	jr LoadFileByDirectoryEntryPtr


; for the file with index a, returns the load (run) address in de, length in bc, and pointer to the binary data in hl
LoadFileHeaderParamsByNumber:
	scf
	call GetDirectoryEntryPointerByNumberImpl
	ld a, [hli]
	ld h, [hl]
	ld l, h
	jr LoadSimpleBinaryHeaderParams

; updates the contents of the file with index a
; at the end, hl points to the beginning of the next file, or the next byte of available space if file a was the last file
UpdateFileByNumber:
	call LoadFileHeaderParamsByNumber   ; get file load address, length, and data pointer
	jr c, @end               ; bail out if file is invalid
	push de
	ld d, h
	ld e, l                  ; swap de and hl
	pop hl
	call CopyData            ; update file contents in SRAM
	; fallthrough
@end:
	ret


.segment "SRAM_DATA"

sFilePointersArrayStart := sFilePointers - 2

sFilePointers:
	.res 32 * 2

sFuncDirectory:
	FuncDirectorySize = 16 * (2 * 2)
	; library ID (2 bytes; if lower byte and $80, signals end of list)
	; pointer to library header in SRAM (2 bytes)
	.res FuncDirectorySize
sFile1:

.segment "BOX_HEAP_FOOTER"

LoadedFilePointers:
	.res 32 * 2
FuncCache:
	FuncCacheSize = 4 * (2 + 2 + 1)
	; ptr to function in RAM (2 bytes)
	; library ID (2 bytes)
	; function ID (1 byte; bit 7 clear = valid entry)
	.res FuncCacheSize
TempSP:
	.res 2

.segment "BOOTSTRAP_AUX"
	.byte 0                    ; 01 wDayCareInUse = 0
FrontEndSentinel:
	call FrontEnd              ; 02 03 04
RestoreBoxData:
	ld b, ^LoadSAV1            ; 05 06
	ld hl, .loword(LoadSAV1)   ; 07 08 09
	jp Bankswitch              ; 0a 0b 0c
CloseSRAM:
	xor a                      ; 0d
	push af                    ; 0e
	jr CloseSRAMContinuation   ; 0f 10
OpenSRAMInBank:
	push af                    ; 11
	ld a, $0a                  ; 12 13
CloseSRAMContinuation:
	ld [$0000], a              ; 14 15 16
	pop af                     ; 17
	ld [$4000], a              ; 18 19 1a
	ret                        ; 1b
	
	.byte 0, 0                 ; 1c 1d  filler

.segment "BOOTSTRAP"

Startup:
	ld b, ^SaveSAVtoSRAM1              ; 01 02
	ld hl, .loword(SaveSAVtoSRAM1)     ; 03 04 05
	call Bankswitch                    ; 06 07 08
	call LoadKernel                    ; 09 0a 0b
	jr FrontEndSentinel                ; 0c 0d
LoadKernel:
	ld a, $03                          ; 0e 0f
	call OpenSRAMInBank                ; 10 11 12
	ld de, __KERNEL_LOAD__             ; 13 14 15
	ld hl, __KERNEL_RUN__              ; 16 17 18
	jp CopyString                      ; 19 1a 1b
	
	.byte $50   ; string terminator    ; 1c  will corrupt DA80 when copied, but that doesn't matter

.segment "KERNEL_LOADER"

OpenSRAMInBank_InLoader = __KERNEL_LOADER_LOAD__ + __KERNEL_LOADER_SIZE__ + __KERNEL_SIZE__ + (OpenSRAMInBank - FrontEndSentinel + 1)
Loader:
	ld a, $03                       ; 01 02
	call OpenSRAMInBank_InLoader    ; 03 04 05
	ld de, Kernel_start             ; 06 07 08
	ld hl, __KERNEL_LOAD__          ; 09 0a 0b
	call CopyString                 ; 0c 0d 0e
	ld hl, __BOOTSTRAP_AUX_RUN__    ; 0f 10 11
	ld bc, CloseSRAM                ; 12 13 14
	push bc                         ; 15
	jp CopyString                   ; 16 17 18
Kernel_start:



