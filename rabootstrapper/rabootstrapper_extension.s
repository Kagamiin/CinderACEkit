.macpack sm83isa
.include "hardware.inc"
.include "rabootstrapper_defs.inc"

.import __PROGRAM_EXTENSION_SIZE__, __PROGRAM_EXTENSION_LOAD__

.segment "EXTENSION_LOADER"
Extension_Loader:
	ld bc, __PROGRAM_EXTENSION_SIZE__
	ld hl, __PROGRAM_EXTENSION_LOAD__
	ld de, Encoder1
	call CopyData
	ret

.segment "PROGRAM_EXTENSION"
Encoder2:
	ld de, wBuffer              ; de = input buffer
	pop hl                      ; hl = pointer to destination bit buffer
	push de                     ; save initial wBuffer position for later
	ld a, ^FlagAction           ; switch to bank 03
	call BankswitchCommon
BitOffsetSMC:
	ld a, 0                     ; current bit offset (SMC'd)
	ld c, a                     ; c = bit offset counter
	ld b, a                     ; b = bit offset compare value
@loop:
	ld a, [de]                  ; load a letter from wBuffer
	push de                     ; save wBuffer index because e is used for the return value
	call ConvertLetter
	ld d, e                     ; save e in d; e will be modified in the inner loop
	jr c, Encoder2End           ; bail out if the end of the buffer is reached

	ld a, b
	add a, 6                    ; we will be writing 6 bits, so offset bit compare value by 6
	ld b, a
	@inner:
		push bc             ; save bit offsets
		xor a
		srl e               ; shift a bit out from e
		rla                 ; shift into a
		ld b, a             ; load into b
		call .loword(FlagAction) ; write b into bit c of bitfield hl
		
		pop bc              ; restore bit offsets
		inc c               ; increment bit offset counter
		ld a, b
		cp a, c             ; compare bit offset counter to bit offset compare value
		jr nz, @inner

	ld a, d
	pop de
	ld [de], a                  ; store converted letter back in wBuffer
	inc de                      ; will be used to calculate checksum later
	jr @loop

Encoder2End:
	pop de
	ld a, d                     ; $cf - has bit 7 set
	ld [de], a                  ; store terminator in wBuffer
	inc de
	ld [de], a                  ; store canary terminator

	ld d, h
	ld e, l                     ; move destination buffer ptr from hl to de
	pop hl                      ; grab initial wBuffer position
	ld a, [hl]                  ; grab first converted letter
@CalcChecksumLoop:
	inc hl
	add a, [hl]                 ; add value to the checksum
	bit 7, [hl]                 ; is this the terminator?
	jr z, @CalcChecksumLoop     ; if not, loop

	ld [hl], a                  ; store checksum in hl
	push de                     ; save destination buffer ptr
	ld d, h
	ld e, l                     ; move number ptr from hl to de
	ld hl, $c400
	ld c, 1
	call PrintBCDNumber_loop    ; print checksum on screen

@convertGlitchTilesToLetters:
	dec l
	set 7, [hl]                 ; convert tiles $00-$06 to letters A-F
	jr nz, @convertGlitchTilesToLetters

@readJoypad:
	call JoypadLowSensitivity
	ldh a, [hJoy5]
	and %00000011               ; read only B and A buttons
	jr z, @readJoypad           ; if neither are pressed, read joypad again
	
	pop de
	rra
	jr nc, WriteMore            ; if A is not pressed, then B is pressed;
	;                           ; don't update destination pointer, go back and write more
	; otherwise, A is pressed
@updateBitOffsetAndGoWriteMore:
	ld hl, BitOffsetSMC + 1
	ld a, b                     ; load bit offset into a
	and a, %00000111            ; get remaining bit offset
	ld [hl], a                  ; store into bit offset starting value

	ld a, b                     ; load bit offset into a
	rrca
	rrca
	rrca                        ; divide by 8
	and a, %00011111            ; to get byte offset from bit offset
	ld c, a
	ld b, 0                     ; load byte offset into bc
	pop hl
	add hl, bc                  ; offset destination buffer ptr by byte offset
	ld d, h
	ld e, l                     ; move destination buffer ptr from hl to de
	jp WriteMore                ; go back and write another round of data

; returns the converted letter in e
ConvertLetter:
	push hl
	sub a, $7f        ; collapse range $00-$7e
	jr c, @c_end      ; if character is in this range, consider it a terminator and return carry
	ld e, a           ; e = character to be converted
	ld hl, @skipDeltaList
@loop:
	; accept range
	sub a, [hl]       ; traverse accepted range in integrator
	inc hl
	jr c, @end        ; if we underflowed the integrator, we're in an accepted range, so return

	; reject range
	ld d, a           ; d = temporary buffer for integrator
	sub a, [hl]       ; traverse rejected range in integrator
	jr c, @end        ; if we underflowed the integrator, we're in a rejected range;
	;                 ; the character is invalid, we don't care, just return whatever is in e

	cp a, d           ; compare integrator with temporary buffer - are they equal (is [hl] == 0)?
	jr z, @end        ; end of delta list, character is invalid too, just return whatever is in e

	ld d, a           ; save integrator back in temporary buffer
	ld a, e           ; load character to be converted
	sub a, [hl]       ; collapse rejected range
	ld e, a           ; store character in e
	ld a, d           ; load integrator in accumulator
	inc hl
	jr @loop

@end:
	or a
@c_end:
	pop hl
	ret

@skipDeltaList:
	.byte 1 + 26 + 6 + 26 ; +++ space + A-Z + ():;[] + a-z
	.byte 6 + 32 + 1      ; --- é'd'l's't'v + empty space + apostrophe
	.byte 3               ; +++ PkMn-
	.byte 2               ; --- 'r'm
	.byte 2               ; +++ ?!
	;.byte 7               ; --- duplicate dot, small katakana, arrows
	;.byte 1               ; +++ male symbol
	;.byte 1               ; --- ED symbol (it appears in the naming screen but is not typable)
	;.byte 5               ; +++ ×, dot, slash, comma, female symbol
	.byte 0               ; --- everything else (end of list)
