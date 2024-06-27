.macpack sm83isa
.include "hardware.inc"
.include "global.inc"

.segment "RESIDENT_1"
; copies a string from hl to de until a $50 terminator is found
; works just like CopyString at $3816, but inverts the register order :3
; (it actually saves space to include this routine rather than having to include extra code to handle usage of the ROM routine)
CopyStringFromHLtoDE:
	ld a, [hli]
	ld [de], a
	inc de
	cp a, "@"
	jr nz, CopyStringFromHLtoDE
	ret

