
.macro FB_SetAnchor
	FB_Anchor .set *
.endmacro

; NOTE: the lower byte in library IDs must have bit 7 cleared.
.macro FB_Start lib_id, lib_name
	.ident (.sprintf ("%s_Anchor", lib_name)) = FB_Anchor
	NUM_FUNCTIONS .set 0
	jr @FB_end__
	.byte $fc                      ; function block signature
	.ident (.sprintf ("%s_NumFuncs", lib_name)):
	.byte COMPUTED_NUM_FUNCTIONS
	.ident (.sprintf ("%s_Lib_ID", lib_name)):
	.word lib_id & $ff7f
	.ident (.sprintf ("%s_Offsets_Start", lib_name)):
.endmacro

.macro FB_AddFunction sym
	.ident (.sprintf ("Offset_%s", sym)) = sym - FB_Anchor
	.word sym - FB_Anchor
	NUM_FUNCTIONS .set NUM_FUNCTIONS + 1
.endmacro

.macro FB_End
	COMPUTED_NUM_FUNCTIONS = NUM_FUNCTIONS
	.byte $eb          ; end of function block signature
@FB_end__:
.endmacro
