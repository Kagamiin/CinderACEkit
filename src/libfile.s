.macpack sm83isa
.include "hardware.inc"
.include "global.inc"
.include "funcblock.inc"

.import sNumFiles
.import sFile1

.segment "FILE_LIB"
	FB_SetAnchor
	FB_Start $0000, "FileLib"
	FB_AddFunction WriteSimpleBinary
	FB_AddFunction StorePayload
	FB_AddFunction DeleteFileByNumber
	FB_End

FileLib_FrontEnd:
	ret

; deletes the file with index b
DeleteFileByNumber:
	ld a, [sNumFiles]
	inc a
	ld c, a
	ld a, b
	cp a, c
	ret nc                         ; file index is higher than number of files
	and a
	ret z                          ; file index is zero
	scf
	call GetDirectoryEntryPointerByNumberImpl
	sub a, c                       ; calculate how many files there are past
	cpl                            ; the file to be deleted
	and a, a                       ; is this the last file?
	jr z, @end                     ; no need to copy anything
@copyFileLoop:
	push af
	push hl                        ; save current directory entry pointer
	inc hl
	inc hl                         ; get next directory entry pointer
	ld a, [hli]
	ld h, [hl]                     ; dereference it
	ld l, h
	call LoadSimpleBinaryHeaderParams
	push hl                        ; push file source pointer

	add sp, 2                      ; dive into directory entry pointer
	pop hl                         ; peek it
	add sp, -4                     ; rise back onto file source pointer

	ld a, [hli]                    ; dereference the directory entry pointer
	ld h, [hl]
	ld l, h
	call WriteSimpleBinary
	pop de                         ; discard file source pointer on stack

	ld d, h
	ld a, l                        ; prepare hl for writing
	pop hl                         ; fetch directory entry pointer back
	inc hl
	inc hl                         ; go to next directory entry pointer
	ld [hli], a                    ; overwrite destination pointer to put next
	ld [hl], d                     ; file right after the current one
	dec hl

	pop af                         ; recover loop count
	dec a                          ; decrement it
	jr nz, @copyFileLoop
	
@end:
	ld hl, sNumFiles
	dec [hl]
	ret

StorePayload_PopAndRet:
	pop hl
	pop bc
	scf
	ret

; creates a new file from data at hl with length bc.
; returns carry if there are already 32 files stored, or if there's not enough space to store the file.
StorePayload:
	push bc
	push hl
	ld a, [sNumFiles]
	cp a, 32
	jr nc, StorePayload_PopAndRet
	and a, a
	ld hl, File1
	ld de, FilePointers
	jr z, @writeDirEntry
	; fallthrough
	
@getNextEmptyByte:
	scf
	call GetDirectoryEntryPointerByNumberImpl
	push hl                  ; save directory entry pointer
	ld a, [hli]
	ld h, [hl]               ; dereference it
	ld l, h
	call LoadSimpleBinaryHeaderParams
	add hl, bc               ; go to empty space after the end of the file
	pop de                   ; restore directory entry pointer into de
	inc de
	inc de                   ; advance to next directory entry
	; fallthrough
@writeDirEntry:
	ld a, l
	ld [de], a               ; write new directory entry
	inc de
	ld a, h
	ld [de], a

	pop de                   ; restore load address argument into de
	pop bc                   ; restore length argument
	push de                  ; push payload location onto the stack
	call WriteSimpleBinary
	pop de
	ret c
	
	ld hl, sNumFiles
	inc [hl]                 ; increase number of files by 1
	xor a                    ; clear carry flag
	ret

; writes a file at hl, with load address de, length bc, and payload pointer at the top of the stack
WriteSimpleBinary:
	xor a
	ld [hli], a              ; write file signature
	call WriteEDCB
	
	push hl
	add hl, bc
	ld a, $bf                ; max SRAM offset high byte
	cp a, h
	pop hl
	ret c                    ; return carry set if hl is higher than $bfff

	ld d, h
	ld e, l                  ; swap de and hl
	add sp, 2
	pop hl                   ; peek at payload location
	add sp, -4
	call CopyData            ; commit data to SRAM
	ret
