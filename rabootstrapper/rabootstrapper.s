.macpack sm83isa
.include "hardware.inc"
.include "rabootstrapper_defs.inc"

.segment "PROGRAM_BASE"
Rabootstrapper:
	ld de, WRITE_DESTINATION   ; destination buffer for data
WriteMore:
	push de                    ; store in stack
	ld hl, .loword(FRAG_ASKNAME_AFTERTEXTBOX)
	call FRAG_BANKSWITCH_BANK1
	; fallthrough
.segment "PROGRAM_EXTENSIBLE"
Encoder1:
	pop de                     ; basic encoder just to get us started, based on the existing
	ld hl, wBuffer             ; NicknameWriter encoder, but stripped down to the bare minimum.
@loop:
	ld a, [hli]                ; read letter L1
	add a, a                   ; a = (2 * L1) & 0xff
	jr nc, @end                ; if L1 < 0x80, end of buffer
	add a, [hl]                ; peek letter L2; a = (L2 + 2 * L1) & 0xff
	ld [de], a                 ; write result into destination
	inc de                     ; advance both buffer pointers
	inc hl                     ; (hl had not been advanced when L2 was peeked)
	jr @loop
@end:
	ld a, l
	cp a, <(wBuffer + 1)       ; did we stop on the first character?
	ret z
	jr WriteMore               ; go back and write another round of data
