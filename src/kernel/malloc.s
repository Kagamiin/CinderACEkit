.macpack sm83isa
.include "hardware.inc"
.include "global.inc"

.segment "KERNEL"

.import __KERNEL_RUN__, __BOX_HEAP_FOOTER_LOAD__, __BOX_HEAP_FOOTER_SIZE__

HEAP_START = __KERNEL_RUN__ - 1
HEAP_ABSOLUTE_MAX = $da80

BoxHeapReset:
	ld a, $ff
	ld hl, __BOX_HEAP_FOOTER_LOAD__
	ld bc, __BOX_HEAP_FOOTER_SIZE__
	call FillMemory
	ld a, <(__KERNEL_RUN__)
	ld hl, wBoxHeapNextFreeArea
	ld [hli], a
	ld a, >(__KERNEL_RUN__)
	ld [hli], a
	ld a, <(HEAP_ABSOLUTE_MAX)
	ld [hli], a
	ld a, >(HEAP_ABSOLUTE_MAX)
	ld [hli], a
	ret

; Sets the max heap determined via a static allocation at hl with length bc.
; Returns carry set if the static allocation overlaps the allocated area
BoxHeapUpdateMax:
	add hl, bc
	dec hl
	
	ld a, [wBoxHeapMax]
	sub a, l
	ld a, [wBoxHeapMax + 1]
	sbc a, h
	ret nc
	
	ld a, [wBoxHeapNextFreeArea]
	sub a, l
	ld a, [wBoxHeapNextFreeArea + 1]
	sbc a, h
	ret c
	
	ld a, l
	ld [wBoxHeapMax], a
	ld a, h
	ld [wBoxHeapMax + 1], a
	ret

; Allocates a block of memory with length bc.
; Returns the pointer to the start of the allocated area in de.
; Returns carry set if there's not enough space to allocate the required amount of memory.
BoxHeapMalloc:
	ld hl, wBoxHeapNextFreeArea
	ld a, [hli]
	ld d, [hl]
	sub a, c
	ld e, a
	ld a, d
	sbc a, b
	ld d, a
	
	ld a, [wBoxHeapMax]
	sub a, e
	ld a, [wBoxHeapMax + 1]
	sbc a, d
	ccf
	ret c

	ld [hl], d
	dec hl
	ld [hl], e
	ret
	
	
	
	
	
	
	
