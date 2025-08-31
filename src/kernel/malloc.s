.macpack sm83isa
.include "hardware.inc"
.include "global.inc"

.segment "KERNEL"

.import __KERNEL_RUN__, __BOX_HEAP_FOOTER_LOAD__, __BOX_HEAP_FOOTER_SIZE__
.import HEAP_ABSOLUTE_MAX

HEAP_START = __KERNEL_RUN__ - 1

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
	
; Invalidates the last file that was previously loaded and frees its memory.
; Does not update the function cache - the caller is responsible for doing so.
; Returns carry set if the last memory allocation does not correspond to a loaded file.
BoxHeapUnbumpLastFile:
	ld hl, wLoadedFilePointers
	ld d, $FF                          ; will be incremented to 0 in the first iteration
	; fallthrough
@loop:
	inc d                              ; increment file index
	ld a, <(wLoadedFilePointersEnd)
	cp a, l
	ret z

	call ReadCB
	ld a, [wBoxHeapNextFreeArea]
	sub a, c
	ld e, a
	ld a, [wBoxHeapNextFreeArea + 1]
	sub a, b
	or a, e
	jr nz, @loop
	; fallthrough
@found:
	dec hl
	res 7, [hl]                        ; mark file as unloaded
	ld a, d
	call LoadSimpleBinaryHeaderParams  ; load file header params to determine its size
	; bc = allocation size
	ld hl, BoxHeapNextFreeArea
	ld a, [hli]
	ld h, [hl]
	ld l, a
	add hl, bc                         ; unbump memory watermark
	ld [hl], h
	dec hl
	ld [hl], l
	xor a                              ; clear carry flag
	ret

	
	

	
	
	
	
	
