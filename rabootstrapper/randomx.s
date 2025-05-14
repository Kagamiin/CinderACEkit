.macpack sm83isa
.include "hardware.inc"

.segment UTILS

RandomX_2:
	pop hl
	push hl
	push de
	add hl, 10
	ldh a, [hDIV]
	add a, [hl]
	ld [hli], a
SeedSMC:
	ld de, $96C3
	
	ld a, d
	rra
	ld a, e
	rra
	xor a, d
	ld d, a
	ld a, e
	rra
	ld a, d
	rra
	xor a, e
	ld e, a
	xor a, d
	ld [hld], a
	ld [hl], e
	
	pop de
	ret

RandomX:
	ld hl, hRandomSub

	ld a, [hld]
	rra
	ld a, [hli]
	rra
	xor a, [hl]
	ld [hld], a
	ld a, [hli]
	rra
	ld a, [hld]
	rra
	xor a, [hl]
	ld [hli], a
	xor a, [hld]
	ld [hli], a

	ret
