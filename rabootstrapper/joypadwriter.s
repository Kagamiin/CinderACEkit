.macpack sm83isa
.include "hardware.inc"

TextBoxBorder =   $1922
FlagAction    = $037669

hLoadedROMBank = $ffb8
hJoyInput      = $fff8

CHAR_SPACE       = $7f
CHAR_COLON       = $9c

.segment "PROGRAM"

JoypadWriter:
	ld a, ^FlagAction
	ld [$2000], a           ; switch to bank 03
	ldh [hLoadedROMBank], a
	ld hl, $da80            ; destination location
	push hl                 ; save destination start pointer
Next4Bytes:
	push hl                 ; save current destination pointer
	ld hl, $c3a0            ; top of screen
	ld bc, $0112            ; 18x1 box (with borders 20x3)
	call TextBoxBorder
	; b is 0 here
	ld c, 0
	ld e, c                 ; zero out last d-pad direction buffer
JoypadWriterLoopIncC:
	inc c                   ; advance symbol offset
JoypadWriterLoop:
	ld hl, $c3b4            ; second screen row (at the border tile)
	add hl, bc              ; add symbol offset
WaitForButtonReleaseAndTestButtons:
	ldh a, [hJoyInput]      ; poll input
	and a, $0f              ; we don't wait for the d-pad to be released, for convenience
	jr nz, WaitForButtonReleaseAndTestButtons
	ld [hl], CHAR_COLON     ; show a colon on the current screen position
	; fallthrough
TestButtons:
	halt                    ; wait for VBlank
	ld d, 5                 ; will be decremented at least once
	ldh a, [hJoyInput]      ; poll input
	push af                 ; save unmodified input byte
	; test d-pad direction
	:
		rla         ; shift a bit
		dec d       ; count down for each shifted bit
		jr z, :+    ; if 4 bits were shifted and none of them were 1, bail out
		jr nc, :-   ; if the shifted bit was 0, keep going
	ld e, d                 ; d-pad direction is stored in e and persists even when you release the d-pad, and even
	ld a, $f5               ; between loop iterations, so you can keep pressing the A button to write the same 2 bits
	add a, d                ; calculate index of char to show
	ld [hl], a              ; write number from 0 to 3 to the screen
	:
	pop af                  ; restore input byte 
@buttonA:
	rra                     ; test A button
	jr nc, @buttonB         ; if not pressed, go check B button
	
	ld a, e                 ; check if a d-pad direction has been selected
	and a
	jr z, JoypadWriterLoop  ; if not, bail out and loop

	ld a, $f5               ; calculate index of char to show
	add a, e
	ld [hl], a              ; write buffered symbol (number from 0 to 3) to the screen
	pop hl                  ; peek at current destination pointer
	push hl
	push bc                 ; save symbol offset
	dec c                   ; calculate bit offset
	sla c
	ld d, e                 ; copy selected symbol
	dec d                   ; remap from 1-4 to 0-3 range
	:
		push bc                  ; save bit offset because it'll be clobbered by FlagAction
		srl d                    ; shift a bit out from symbol
		rl b                     ; shift into b
		call .loword(FlagAction) ; write it to the output
		pop bc                   ; restore bit offset
		inc c                    ; increment it
		bit 0, c                 ; if it's odd, repeat
		jr nz, :-
	
	pop bc                  ; restore symbol offset
	ld a, 18
	sub a, c                ; is it equal to 18?
	jr nz, JoypadWriterLoopIncC ; if not, loop and increment symbol offset
	
	; a = 0 here
	ld b, 4                 ; number of bytes to compute checksum over
	:
		ld d, [hl]  ; load byte at destination
		inc hl      ; increment destination pointer
		add a, d    ; add lower half
		swap d      ; swap nibbles
		add a, d    ; add upper half
		dec b       ; decrement byte counter
		jr nz, :-   ; loop until 4 bytes have been checksummed

	xor a, [hl]             ; compare calculated checksum with the one written in the last 2 symbols
	and a, $0f              ; (only lower nibble)
	jr nz, JoypadWriterLoop ; if checksum doesn't match, don't advance to next code
	; fallthrough
@advance4Bytes:
	pop hl              ; restore destination pointer
	ld a, c
	cp a, 17            ; have we written at least 16 symbols? (always true when coming from @buttonA)
	ld c, 4             ; advance destination pointer by 4
	add hl, bc
	jr nc, Next4Bytes   ; if we have written at least 16 symbols, reinitialize at new address
	pop hl              ; otherwise, pop destination start address and return
	ret

@buttonB:
	rra                  ; test B button
	jr nc, @buttonSelect ; if not pressed, try Select button
	
	ld [hl], CHAR_SPACE  ; wipe current symbol position on screen
	dec c                ; decrement symbol offset
	jr nz, JoypadWriterLoop ; bail out and loop if we haven't underflowed
	inc c                ; increment it back and continue (to save space)
	; fallthrough
@buttonSelect:
	rra                  ; test Select button
	jr c, @advance4Bytes ; if pressed, advance to next 4 bytes, or return if we haven't written at least 16 symbols
@buttonStart:
	rra                  ; test Start button
	jr nc, TestButtons   ; if not pressed, loop
	pop hl               ; if pressed, pop destination address from stack
	ret                  ; return to destination start address (jumping to the payload)


