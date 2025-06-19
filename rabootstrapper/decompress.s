.macpack sm83isa
.include "hardware.inc"
.include "global.inc"

; decompresses a payload from bc to hl
; clobbers de
; TODO: how to specify the secondary bitstream?
Decompress:
	ld a, [bc]
	and a, a
	ret z
	ld e, a
	inc bc
@literalLoop:
	; copy e bytes from bc to hl
	ld a, [bc]
	ld [hli], a
	inc bc
	dec e
	jr nz, @literalLoop
@lz:
	ld a, [bc]
	inc bc
	bit 7, a
	jr z, @short
@long:
	cp a, $ff
	jr z, Decompress
	res 7, a
	and a, a
	ret z
	ldh [hTempA], a
	ld a, [bc]
	inc bc
	jr @after
@short:
	ld d, a
	swap a
	and a, $0f
	inc a
	ldh [hTempA], a
	ld a, d
	and a, $0f
	; fallthrough
@after:
	push bc
	push hl
	cpl
	ld c, a
	ld a, $ff
	ld b, a
	add hl, bc
	ld d, h
	ld e, l
	pop hl
	ldh a, [hTempA]
	ld c, a
	call CopyBytesVerbatimOrMasked
	pop bc
	jr Decompress

; LZSS-masked: copies c bytes from de to hl
; for each byte to be copied, reads a bit from the secondary bitstream
; 0 = copy byte verbatim
; 1 = copy opcode masked (reads extra bits from bitstream)
CopyBytesVerbatimOrMasked:
	ld a, [de]
	call ReadBit
	call nc, GrabAndMaskOpcode
	ld [hli], a
	inc de
	dec c
	ret z
	jr CopyBytesVerbatimOrMasked

; given a source opcode in a, masks it, modifies it and returns the result in a
GrabAndMaskOpcode:
	bit 7, a
	jr nz, @upper
	bit 6, a
	jr z, @innerUpperBitsOrOuter
	;fallthrough
@inner:
	call ReadBit
	jr nc, @innerLowerBits
	; fallthrough
	@innerUpperBitsOrOuter:
		and a, %11000111
		ld [hl], a
		call Read3Bits
		rla
		rla
		rla
		or a, [hl]
		ret
	@innerLowerBits:
		and a, %11111000
		ld [hl], a
		call Read3Bits
		or a, [hl]
		ret

@upper:
	bit 6, a
	jr z, @inner
	jr @innerUpperBitsOrOuter

; reads 3 bits from the secondary bitstream into the a register
Read3Bits:
	xor a
	call ReadBit
	rlca
	call ReadBit
	rlca
	call ReadBit
	rlca
	ret


; reads a bit from the secondary bitstream and returns it in the carry flag
ReadBit:
	; TODO: implement

