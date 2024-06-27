.macpack sm83isa
.include "hardware.inc"
.include "global.inc"

sNumFiles = FrontEnd + 1
.export sNumFiles
.export sFuncCache
.export sFuncDirectory
.export sFile1
.export FuncCacheSize
.export FuncDirectorySize

.segment "SRAM_CODE"
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
	jp FrontEndJumpout


; loads a file given its directory entry pointer and returns the run address in hl
LoadFileByDirectoryEntryPtr:
	ld a, [hli]
	ld h, [hl]
	ld l, h
	; fallthrough
; loads the simple binary file at hl and returns the run address in hl
LoadSimpleBinary:
	call LoadSimpleBinaryHeaderParams

	push de
	bit 7, d            ; if bit 15 of de is cleared, this is meant to run
	;                   ; from SRAM
	call nz, CopyData
	pop hl
	set 7, h
	ret


; returns the load (run) address in de, length in bc, and pointer to the binary data in hl
LoadSimpleBinaryHeaderParams:
; NOTE: do not call directly with CORE_BIT__IN_FRONT_END set - use LoadAndRunFileByNumber instead
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

sFilePointersArrayStart := FilePointers - 2

sFilePointers:
	.res 32 * 2
sFuncCache:
	FuncCacheSize = 4 * (2 + 2 + 1)
	; ptr to function (2 bytes)
	; library ID (2 bytes)
	; function ID (1 byte; bit 7 clear = valid entry)
	.res FuncCacheSize
sFuncDirectory:
	FuncDirectorySize = 172
	; library ID (2 bytes; if lower byte and $80, signals end of list)
	; n = number of functions (1 byte)
	; n * ptrs to function (n * 2 bytes)
	.res FuncDirectorySize ; enough space for 9 libraries with 8 func. each
sFile1:

.segment "BOOTSTRAP"
FrontEndSentinel:
	call FrontEndBootstrap
	call PrepareRTCDataAndDisableSRAM  ; close SRAM in case it hasn't been closed
	ret
FrontEndBootstrap:
	ld a, $03            ; open SRAM
	call SwitchSRAMBankAndLatchClockData
	jp FrontEnd
FrontEndJumpout:
	call PrepareRTCDataAndDisableSRAM  ; close SRAM for safety
	ld de, FrontEndCleanup
	push de              ; push return address
	jp hl                ; jump to payload
FrontEndCleanup:
	pop bc               ; restore file index
	ret c                ; skip autosave if program returned carry set
	ld a, $03
	call SwitchSRAMBankAndLatchClockData  ; open SRAM
	ld a, b              ; load file index into a
	call UpdateFileByNumber
	; FrontEndSentinel will close SRAM for us
	ret



