.macpack sm83isa
.include "hardware.inc"
.include "global.inc"
.include "errno.inc"

.segment "KERNEL"

; writes e, d, c, b to hl; advances hl
WriteEDCB:
	ld [hl], e          ; write load address
	inc hl
WriteDCB:
	ld [hl], d
	inc hl
WriteCB:
	ld [hl], c          ; write length
	inc hl
	ld [hl], b
	inc hl
	ret

; sets the lib to be called upon calling Lib_Call
; the bc register specifies the library ID
Set_LibToBeCalled_BC:
	push hl
	ld hl, LibToBeCalled
	ld [hl], c
	inc hl
	ld [hl], b
	pop hl
	ret

; calls a relocatable system library using the bc register as the library ID
; the a register selects the function to be called
Lib_Call_BC:
	call Set_LibToBeCalled_BC
	jr Lib_Call

; calls a relocatable system library using only the a register
; top 5 bits of a selects a library from $0000 to $001f
; bottom 3 bits select one of the first 8 functions of the library
Lib_Shortcall:
	push af
	rrca
	rrca
	rrca
	and a, $1f
	ld [wLibToBeCalled], a
	xor a
	ld [wLibToBeCalled + 1], a
	pop af
	and a, $07
	; fallthrough

; calls a relocatable library
; wLibToBeCalled selects a library by its 16-bit ID
; the a register selects a function from the library
; clobbers hTempA
Lib_Call:
	push hl              ; space for jumpout address
	push hl              ; push registers
	push de
	push bc

	ld hl, wFuncCache    ; hl = start of wFuncCache
	ldh [hTempA], a     ; save function ID for later
@cacheLoop:
	ld a, <(wFuncCache + FuncCacheSize + 1)
	cp a, l              ; are we past the end (beginning) of wFuncCache?
	jr z, ReadDirectory  ; get out of the loop
	call ReadEDCB        ; de = function pointer, bc = library id, [hl] = function ID

	ldh a, [hTempA]
	cp a, [hl]           ; compare function ID with selected function ID
	inc hl
	jr nz, @cacheLoop
	
	ld a, [wLibToBeCalled]
	sub a, c
	ld c, a
	ld a, [wLibToBeCalled + 1]
	sub a, b
	or a, c
	jr nz, @cacheLoop
	jr PopRegsAndJumpOutToDE

Lib_Call_cancel_unbump:
	ldh [hErrno], a
	call BoxHeapUnbumpLastFile
	jr Lib_Call_cancel_scf

Lib_Call_cancel_pop:
	pop de
	; fallthrough
Lib_Call_cancel:
	ldh [hErrno], a
	; fallthrough
Lib_Call_cancel_scf:
	scf
	; fallthrough

; pokes de 3 words down the stack, then pops bc, de and hl (in this order)
; and if the carry flag is cleared, returns into the location previously pointed to by de
; otherwise, pops de back and returns it to the caller
; do not call, use a jp/jr instead
PopRegsAndJumpOutToDE:
	add sp, 8                 ; dive under jumpout address
	push de                   ; push jumpout address
	add sp, -6                ; rise onto saved bc value
	pop bc                    ; pop registers
	pop de
	pop hl
	ret nc                    ; jump out if function was found
	pop hl
	ret                       ; return to caller with carry set if function was not found

ReadDirectory:
@directoryLoop:
	bit 7, [hl]             ; is the current entry valid?
	jr z, @entryValid       ; if so, continue
@notFound:
	ld a, ERR_LIBRARY_NOT_FOUND
	jr Lib_Call_cancel

@entryValid:
	ld a, l                 ; check if we are past the end of sFuncDirectory
	sub a, <(sFuncDirectory + FuncDirectorySize + 1)
	ld a, h
	sbc a, >(sFuncDirectory + FuncDirectorySize + 1)
	jr nc, @notFound        ; if so, bail out

	call ReadCB             ; read library ID in bc; file ID is at [hl]

	ld a, [wLibToBeCalled]
	res 7, a
	sub a, c                ; compare library ID low byte
	ld c, a
	ld a, [wLibToBeCalled + 1]
	sub a, b                ; compare library ID high byte
	or a, c                 ; check if both results are zero
	jr nz, @directoryLoop   ; otherwise, the library ID doesn't match
	
	ld a, [hl]              ; grab file ID
	call CheckIfFileIsLoaded
	jr z, LoadLibrary     ; if file isn't loaded, jump over and go load it
	call FindFunction     ; if file is already loaded, go find the function in it
	jr nz, Lib_Call_cancel
	jr PopRegsAndJumpOutToDE

LoadLibrary:
	call LoadFileHeaderParamsByNumber
	jr c, Lib_Call_cancel  ; if file ID is not valid, bail out
	; bc = length, hl = pointer to file data in SRAM
	push hl                    ; save pointer to file data
	call BoxHeapMalloc         ; allocate space for the library to be loaded
	jr nc, @allocated          ; if allocation was successful, continue
@oom:
	ld a, ERR_OUT_OF_MEMORY
	jr Lib_Call_cancel
	
@allocated:
	pop hl                  ; restore pointer to file data
	push de                 ; save destination offset
	call CopyData           ; load library into WRAM
	pop bc                  ; restore start of library in WRAM

	ldh a, [hTempA]         ; restore function ID
	call RegisterFileAsLoaded
	call FindFunction
	jr c, Lib_Call_cancel_unbump
	jr nz, Lib_Call_cancel
	jr PopRegsAndJumpOutToDE

; finds a function selected by the a register inside the library loaded in WRAM at hl
; if it returns zero set and carry clear, de points to the function entry point in WRAM
; if it returns zero clear, hl points to the function entry point in WRAM
; if it returns carry set, the function index was out of bounds
FindFunction:
	push hl                 ; save start of library in WRAM
	ld de, $4
	add hl, de              ; seek to function count inside funcblock
	cp a, [hl]              ; compare number of funcs with selected func
	jr c, @inbounds
@oob:
	ld a, ERR_FUNCTION_INDEX_OUT_OF_BOUNDS
	pop hl
	scf
	ret

@inbounds:
	inc a
	ld e, a                 ; d = 0
	add hl, de              ; seek to selected function's offset
	add hl, de
	
	ld c, [hl]              ; read function offset
	inc hl
	ld b, [hl]
	pop hl                  ; restore start of library in WRAM
	add hl, bc              ; seek to start of selected function

	ld a, [wLibToBeCalled]
	ld c, a                 ; load low byte of library ID into c
	bit 7, a
	cpl                     ; ensure hErrno will be < $80 in case of error
	ret nz                  ; if bit 7 of the library ID is set, return without calling the function
	push hl                 ; push function ptr
	; fallthrough
	
@addFunctionToCache:
	push bc                 ; push partial library ID
	ld bc, FuncCacheSize - 5
	ld de, wFuncCache
	ld hl, wFuncCache + 5
	call CopyData           ; shift cache entries over
	ld h, d
	ld l, e
	pop bc                  ; pop partial library ID
	pop de                  ; pop function ptr
	ld a, [wLibToBeCalled + 1]
	ld b, a                 ; load high byte of library ID into b
	call WriteEDCB          ; write function ptr and library ID into cache entry
	ldh a, [hTempA]         ; restore function ID
	ld [hl], a              ; write function ID into new cache entry
	xor a                   ; set zero flag, unset carry flag
	ret

; checks if the file with index a is currently loaded.
; zero flag clear = file loaded, run location is returned in hl;
; zero flag set = file not loaded, hl is garbage
; clobbers a, de
CheckIfFileIsLoaded:
	ld hl, wLoadedFilePointers
	call GetNthPointerInList
	ld a, [hli]
	ld h, [hl]
	ld l, h
	bit 7, h
	ret

; scans all of the files and rebuilds the library directory from scratch
RescanAllFilesForLibs:
	ld bc, FuncCacheSize
	ld hl, wFuncCache
	ld a, $ff
	call FillMemory           ; wipe the entire cache
	ld bc, FuncDirectorySize
	ld hl, sFuncDirectory
	ld d, h
	ld e, l
	call FillMemory           ; wipe the entire directory
	; bc is now $0000, de points to sFuncDirectory
@loop:
	ld a, e
	sub a, <(sFuncDirectory + FuncDirectorySize)
	ld a, d
	sbc a, >(sFuncDirectory + FuncDirectorySize)
	jr nc, @end               ; bail out if we ran out of space in the directory

	ld a, [sNumFiles]
	cp a, c
	jr nc, @end               ; bail out if we just processed the last file
	inc c                     ; point c to the next file index
	push bc                   ; save file index
	push hl                   ;
	push de                   ; save write position
	ld a, c
	call LoadFileHeaderParamsByNumber
	
	; hl now points to the beginning of the binary
	inc hl
	inc hl                    ; skip past relative jump at the beginning
	ld a, [hli]
	cp a, $fc                 ; match function block header
	jr nz, @invalid

	ld b, 0
	ld c, [hl]                ; read number of funcs in c
	inc hl
	ld e, [hl]                ; read library ID in de
	inc hl
	ld d, [hl]
	sla c                     ; bc = 2 * number of funcs
	add hl, bc                ; seek to terminator byte
	ld a, [hl]
	cp a, $eb                 ; validate terminator byte
	jr nz, @invalid

	; fallthrough
@valid:
	xor a                     ; set zero flag
	; fallthrough
@invalid:
	pop de
	pop hl
	pop bc
	jr nz, :+
		call WriteEDCB
		dec hl
:
	ld d, h
	ld e, l
	jr @loop

; @invalid2:
; 	rlca                      ; unset zero flag
; 	jr @invalid

@end:
	ld a, $ff
	ld [de], a
	ret
