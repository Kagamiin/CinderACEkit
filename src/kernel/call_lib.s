.macpack sm83isa
.include "hardware.inc"
.include "global.inc"

.import sNumFiles
.import sFuncCache
.import sFuncDirectory
.import FuncCacheSize
.import FuncDirectorySize

.segment "KERNEL"

; writes e, d, c, b to hl; advances hl
WriteEDCB:
	ld [hl], e          ; write load address
	inc hl
	ld [hl], d
	inc hl
	ld [hl], c          ; write length
	inc hl
	ld [hl], b
	inc hl
	ret

; calls a relocatable system library using only the a register
; top 5 bits of a selects a library from $0000 to $001f
; bottom 3 bits select one of the first 8 functions of the library
Lib_Shortcall:
	push af
	rrca
	rrca
	rrca
	and a, $1f
	ldh [hTemp8], a
	xor a
	ldh [hTemp9], a
	pop af
	and a, $07
	; fallthrough

; calls a relocatable library
; hTemp8.hTemp9 selects a library by its 16-bit ID
; the a register selects a function from the library
Lib_Call:
	push hl              ; space for jumpout address
	push hl              ; push registers
	push de
	push bc

	ld hl, FuncCache    ; hl = start of sFuncCache
	push af
@cacheLoop:
	ld a, <(FuncCache + FuncCacheSize + 1)
	cp a, l              ; are we past the end (beginning) of sFuncCache?
	jr z, @cacheMiss     ; get out of the loop
	call ReadEDCB        ; de = function pointer, bc = library id, [hl] = function ID

	pop af
	cp a, [hl]           ; compare function ID with selected function ID
	inc hl
	jr nz, @cacheLoop
	
	push af
	ldh a, [hTemp8]
	sub a, c
	ld c, a
	ldh a, [hTemp9]
	sub a, b
	or a, c
	jr nz, @cacheLoop
	
	pop af
	jp PopRegsAndJumpOutToDE

@cacheMiss:

ReadDirectory:
@directoryLoop:
	bit 7, [hl]          ; is the current entry valid?
	jr nz, @cancel       ; if not, bail out

	call ReadEDCB         ; read library ID in de, library header pointer in bc

	ldh a, [hTemp8]
	res 7, a
	sub a, e             ; compare library ID low byte
	ld e, a
	ldh a, [hTemp9]
	sub a, d             ; compare library ID high byte
	or a, e              ; check if both results are zero
	jr nz, @directoryLoop
	
	ld l, e
	ld h, d
	call ReadDCB         ; read number of functions in d, library ID in bc
	pop af
	cp a, d              ; is the function ID in bounds?
	jr nc, @cancel2
	ld e, a
	ld d, 0
	add hl, de
	add hl, de
	
	ld e, d
	ld d, 0
	jr nz, :+
		cp a, e              ; is the function ID in bounds?
		jr nc, @cancel2      ; if not, return carry set
		ld e, a              ; skip to selected function
		cp a, a              ; set zero flag, clear carry flag
:
	add hl, de
	add hl, de
	push af
	ld a, l              ; check if we are past the end of sFuncDirectory
	sub a, <(sFuncDirectory + FuncDirectorySize + 1)
	ld a, h
	sbc a, >(sFuncDirectory + FuncDirectorySize + 1)
	cp a, h
	jr nc, @cancel

	pop af
	jr c, @directoryLoop

	ld e, [hl]
	inc hl
	ld d, [hl]

	push af              ; push function ID
	ldh a, [hTemp8]
	bit 7, a
	pop af
	ret nz
	
	push af
	push bc              ; push library ID
	push de              ; push function ptr over library ID
	ld bc, FuncCacheSize - 5
	ld de, sFuncCache
	ld hl, sFuncCache + 5
	call CopyData        ; shift cache entries over
	ld h, d
	ld l, e
	pop de
	pop bc
	call WriteEDCB       ; write function ptr and library ID into cache entry
	pop af
	ld [hl], a           ; write function ID into new cache entry
	xor a                ; clear carry flag
	jr PopRegsAndJumpOutToDE

@cancel:
	pop af
@cancel2:
	ldh a, [hTemp8]
	bit 7, a
	scf
	ret nz
	; fallthrough

; pokes de 3 words down the stack, then pops bc, de and hl (in this order)
; and returns into the location previously pointed to by de
; do not call, use a jp instead
PopRegsAndJumpOutToDE:
	add sp, 8                 ; dive under jumpout address
	push de                   ; push jumpout address
	add sp, -6                ; rise onto saved bc value
	pop bc                    ; pop registers
	pop de
	pop hl
	ret nc                    ; jump out if function was found
	pop hl
	ret

; scans all of the files and rebuilds the library directory from scratch
RescanAllFilesForLibs:
	ld bc, FuncCacheSize + FuncDirectorySize
	ld hl, sFuncCache
	ld a, $ff
	call FillMemory           ; wipe the entire cache + directory
	; bc is now $0000
	ld de, sFuncDirectory
@loop:
	ld a, [sNumFiles]
	cp a, c
	jr nc, @end               ; bail out if we just processed the last file
	inc c                     ; point c to the next file index
	push bc                   ; save file index
	push de                   ; save write position
	ld a, c
	call LoadFileHeaderParamsByNumber
	
	; hl now points to the beginning of the binary
	inc hl
	inc hl                    ; skip past relative jump at the beginning
	ld a, [hli]
	cp a, $fc                 ; match function block header
	jr nz, @invalid

	push hl                   ; save hl value at beginning of body
	push hl                   ; twice
	inc hl
	inc hl
	inc hl
	ld b, 0
	ld c, [hl]                ; read number of funcs in c
	inc c
	sla c
	inc c                     ; bc = 2 * number of funcs + 3
	pop hl                    ; reset hl to beginning of body
	add hl, bc                ; seek to terminator byte
	ld a, [hl]
	pop hl                    ; reset hl to beginning of body
	cp a, $eb                 ; validate terminator byte
	jr nz, @invalid

	push hl
	ld h, d
	ld l, e
	add hl, bc
	ld a, l
	sub a, <(sFuncDirectory + FuncDirectorySize)
	ld a, h
	sbc a, >(sFuncDirectory + FuncDirectorySize)
	pop hl
	jr nc, @invalid2
	; fallthrough
@valid:
	xor a                     ; set zero flag
	; fallthrough
@invalid:
	pop de
	call z, CopyData          ; if we're okay, write data into the destination
	pop bc
	jr @loop

@invalid2:
	rlca                      ; unset zero flag
	jr @invalid

@end:
	ld a, $ff
	ld [de], a
	ret
