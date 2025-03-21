;    2–5 Pokémon
;    Tentacool as the first Pokémon
;    Onix as the second Pokémon
;    The Tentacool must have exactly the following amounts of remaining PP and number of PP Up used:
;        33 for 1st move (0 PP Up used)
;        34 for 2nd move (0 PP Up used)
;        19 for 3rd move (3 PP Up used)
;        41 for 4th move (3 PP Up used)
; Initial hl = 0xD163, a = 0x63, bc = 0x00B8; de is unreliable; stack top contains 0x30bc
bbbbb_bootstrap:
	ld [bc], a
	jr bbbbb_bootstrap_continuation; $22

bbbbb_bootstrap_continuation:
	ld hl, $d322
	jp hl



; b = 0
; hl = d322
bbbbb:  ; at d322
	ld c, 14    ; item 3's qty = value operand; SMC'd; incremented after each loop
	add bc, hl  ; add current offset and hl together into bc
	inc hl      ; preincrementing hl for padding and for later
	ld a, 0     ; item 5's qty = value operand; SMC'd to 0 after every call
	ld [bc], a  ; set byte at bc to item 5's qty
	inc [hl]    ; increment item 3's qty
	add l, 4    ; hl = addr of item 5's qty
	xor a
	ld [hl], a  ; zero out item 5's qty
	inc bc      ; dummy
	ret         ; toss 10× of this item to execute your payload on the next 8F use
	; 14 bytes, 7 items
	; written payload goes here

