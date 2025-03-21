.macpack sm83isa
.include "hardware.inc"

; decompresses the current opcode by copying the template from de and writing the result to hl
CopyOpcodeMasked:
	push bc
	ld a, [de]
	push de
	bit 7, a
	jr nz, @inverted
	call @bit6
@continue:
	and a, b
	ld c, 8
:
	rlc b
	

	

@bit6:
	bit 6, a
	ld b, $c0
	jr z, :+
	ld b, $c7
:
	ret

@inverted:
	cpl
	call @bit6
	cpl
	jr @continue

ReadBit:
	
	
