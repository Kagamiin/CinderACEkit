.macpack sm83isa
.include "hardware.inc"
.include "global.inc"
.include "funcblock.inc"

.segment "EXEC_LIB"
FB_SetAnchor

ExecutableLib_FunctionBlock:
	FB_Start
	FB_AddFunction LoadExecutable
	FB_AddFunction GetSegment
	FB_AddFunction GetExecutableName
	FB_AddFunction ValidateExecutable
	FB_AddFunction ValidateExecutableName
	FB_AddFunction ValidateSegment
	FB_End

; loads the executable at hl
; returns the run address in bc, if the executable is valid
; clobbers a
LoadExecutable:
	push hl
	call ValidateExecutable
	jr c, @notValid

	inc hl
	call ValidateExecutableName  ; advance past the name, will always be valid at this point
	ld a, [hli]
	ld c, a
	ld a, [hli]    ; read run address
	ld b, a
	push bc        ; save it for later
	
	push hl        ; save hl at the beginning of the segment header list
@seekToDataSectionLoop:
	call GetSegment
	jr z, :+
	ld h, d
	ld l, e
	jr @seekToDataSectionLoop
:
	ld hl, wExecutablePtr
	ld a, e
	ld [hli], a
	ld [hl], d

	pop hl         ; restore hl
@segmentListLoop:
	call GetSegment
	jr z, @done
	push de
	ld d, h
	ld e, l

	ld hl, wExecutablePtr
	ld a, [hli]
	ld h, [hl]
	ld l, a
	call CopyData

	pop hl         ; this is intentional, to transfer the pointer to the next header into hl
	jr @segmentListLoop
@done:
	pop bc
	pop hl
	ret
	
@notValid:
	scf
	; fallthrough
@end:
	pop hl
	ret

; for the segment header at hl,
; returns its load address at hl,
; returns its length at bc,
; and returns the pointer to the next header at de.
; - if the terminator was read, returns with the z flag set;
;   bc is garbage, hl is zero and de points to after the terminator.
GetSegment:
	ld a, [hli]
	ld c, a
	ld d, h
	ld e, l
	ld h, [hl]
	ld l, a
	
	inc de
	ld a, c
	and a
	ret z
	rla            ; clear zero flag
	
	ld a, [de]
	ld c, a
	
	inc de
	ld a, [de]
	ld b, a
	
	inc de
	ret

; copies the name of the executable at hl to de
; clobbers a, c
; updates de
; - de must point to a buffer at least 19 bytes long
; - if the executable name is invalid, an empty string will be copied and the function will return carry set
GetExecutableName:
	push hl
	inc hl
	push hl
	call ValidateExecutableName
	pop hl
	jr c, @notValid
	call CopyStringFromHLtoDE
	pop hl
	ret

@notValid:
	ld a, $50
	ld [de], a
	pop hl
	scf
	ret


; validates the executable at hl
; clobbers a, c
; returns the number of segments in b
ValidateExecutable:
	push hl
	push de
	ld a, [hli] 
	and a        ; is the executable type $00?
	jr nz, @notValid
	
	call ValidateExecutableName
	jr c, @notValid
	inc hl
	inc hl       ; skip over run address as it's irrelevant for validation

	ld c, 65
	ld b, 0
@ValidateSegmentsLoop:
	dec c
	jr z, @notValid           ; too many segments
	call ValidateSegment
	jr z, @segmentsLoopEnd    ; terminator found
	jr c, @notValid
	inc b
	jr @ValidateSegmentsLoop

@segmentsLoopEnd:
	ld a, b
	and a
	jr z, @notValid       ; zero segments
	
	xor a                 ; clear carry flag
	jr @end

@notValid:
	scf
	
@end:
	pop hl
	pop de
	ret

; reusable snippet to validate an executable's name at hl
; clobbers a, c
; updates hl
ValidateExecutableName:
	ld c, 19     ; 18 characters + 1 canary
@ValidateExecutableName:
	ld a, [hli]
	cp a, $50
	jr z, @valid
	dec c        ; is the name longer than 18 characters (excluding the terminator)?
	jr z, @notValid
	cp a, $7f    ; are there invalid characters in the name?
	jr c, @notValid
@valid:
	ret

@notValid:
	scf
	ret

; validates the executable's segment header at hl
; clobbers a, de
; advances hl to the next segment header
ValidateSegment:
	push bc
	call GetSegment
	jr z, @term          ; terminator

	ld a, h
	cp a, $a0
	jr c, @invalid       ; reject 0000..9fff
	cp a, $e0
	jr c, @validStart    ; accept a000..dfff

	cp a, $ff
	jr nz, @invalid      ; reject e000..feff
	ld a, l
	cp a, $80
	jr c, @invalid       ; reject ff00..ff7f
	cp a, $ff
	jr z, @invalid       ; reject ffff
	; fallthrough        ; accept ff80..fffe

@validStart:
	add hl, bc

	ld a, h
	cp a, $a0
	jr c, @invalid
	cp a, $fe
	jr c, @valid

	cp a, $ff
	jr nz, @invalid

	ld a, l
	cp a, $80
	jr c, @invalid
	cp a, $ff
	jr z, @invalid
	jr @valid

@invalid:
	scf
	; fallthrough
@term:
@valid:
	ld h, d
	ld l, e
	pop bc
	ret
