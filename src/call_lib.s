.macpack sm83isa
.include "hardware.inc"
.include "global.inc"

.import sNumFiles
.import sFuncCache
.import sFuncDirectory
.import FuncCacheSize
.import FuncDirectorySize

.segment "SRAM_CODE2"

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

; 	ld hl, sFuncCache + FuncCacheSize - 1  ; hl = last byte of sFuncCache
; 	push hl              ; save hl for later
; 	and a, $7f           ; unset bit 7; entries with bit 7 set won't match
; 	push af              ; save function index on top of stack
; @cacheLoop:
; 	ld a, <(sFuncCache - 1)
; 	cp a, l              ; are we past the end (beginning) of sFuncCache?
; 	jr z, @cacheMiss     ; get out of the loop
; 
; 	ld d, [hl]           ; read function ID byte
; 	pop af
; 	cp a, d              ; compare with selected function ID
; 	push af
; 	; fallthrough
; @skipEntry:
; 	ld bc, -5
; 	add hl, bc           ; move to end of the previous cache entry
; 	jr nz, @cacheLoop    ; analyze it instead if the function ID didn't match
; 
; 	inc hl               ; move to beginning of current cache entry
; 	scf
; 	call ReadEDCB        ; read function ptr in de and library ID in bc
; 	ldh a, [hTemp8]      ;  (hl now points to function ID byte again)
; 	cp a, c              ; does the low byte match?
; 	jr nz, @skip         ; if not, skip to previous entry
; 	ldh a, [hTemp9]
; 	cp a, b              ; does the high byte match?
; 	jr z, @cacheHit      ; if so, we hit
; 	; fallthrough
; @skip:
; 	xor a                ; set zero flag
; 	jr @skipEntry
; 
; @cacheHit:
; 	pop af
; 	pop hl
; 	jp PopRegsAndJumpOutToDE
; 
; @cacheMiss:
; 	pop af
; 	pop hl
; 	inc hl

ReadDirectory:
	ld hl, sFuncDirectory
@directoryLoop:
	bit 7, [hl]          ; is the current entry valid?
	jr nz, @cancel       ; if not, bail out

	call ReadDCB         ; read number of functions in d, library ID in bc

	scf
	push af              ; carry flag is set
	ldh a, [hTemp8]
	res 7, a
	sub a, e             ; compare library ID ptr low byte
	ld b, a              ; store partial result in b
	ldh a, [hTemp9]
	sub a, d             ; compare library ID high byte
	or a, b              ; check if both results are zero

	ld b, 0
	jr nz, :+
		pop af
		cp a, c              ; is the function ID in bounds?
		jr nc, @cancel2      ; if not, return carry set
		ld c, a              ; skip to selected function
		cp a, a              ; set zero flag, clear carry flag
		push af
:
	add hl, bc
	add hl, bc
	ld a, l              ; check if we are past the end of sFuncDirectory
	sub a, <(sFuncDirectory + FuncDirectorySize + 1)
	ld a, h
	sbc a, >(sFuncDirectory + FuncDirectorySize + 1)
	cp a, h
	jr nc, @cancel2

	pop af
	jr c, @directoryLoop

; 	push af              ; push function ID
; 	push de              ; push library ID
	ld e, [hl]
	inc hl
	ld d, [hl]

	ldh a, [hTemp8]
	bit 7, a
	ret nz
; 	push de              ; push function ptr over library ID
; 	ld bc, FuncCacheSize - 5
; 	ld de, sFuncCache
; 	ld hl, sFuncCache + 5
; 	call CopyData        ; shift cache entries over
; 	pop de
; 	pop bc
; 	pop af
; 	call WriteEDCB       ; write function ptr and library ID into cache entry
; 	ld [hl], a           ; write function ID into new cache entry
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
	ret                       ; jump out

@cancel2:
	pop af
@cancel:
	ldh a, [hTemp8]
	bit 7, a
	scf
	ret nz

	pop bc
	pop de
	pop hl
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
