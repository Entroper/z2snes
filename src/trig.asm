.p816
.a16
.i16

.export Sine
.export Cosine

.include "registers.inc"

SineTable:
.byte $00,$06,$0C,$12,$19,$1F,$25,$2B
.byte $31,$38,$3E,$44,$4A,$50,$56,$5C
.byte $61,$67,$6D,$73,$78,$7E,$83,$88
.byte $8E,$93,$98,$9D,$A2,$A7,$AB,$B0
.byte $B5,$B9,$BD,$C1,$C5,$C9,$CD,$D1
.byte $D4,$D8,$DB,$DE,$E1,$E4,$E7,$EA
.byte $EC,$EE,$F1,$F3,$F4,$F6,$F8,$F9
.byte $FB,$FC,$FD,$FE,$FE,$FF,$FF,$FF

; Calculates the sine of the value in A, where A is in units of "bytians".
; There are 256 bytians in a circle, so sin 64 = 1.0, which is $0100 in fixed-point.
; Returns the value in A as 16-bit fixed point, IIII IIII.FFFF FFFF
.proc Sine
	Temp = $00

	pha               ; save this before we mask it off
	and #$007F
	cmp #$0040        ; if we were 64 or 196
	bne UseTable
	lda #$0100        ; then the sine is 1.0
	sta Temp
	jmp CheckNegative

UseTable:
	sta Temp
	and #$0040        ; if we're in the range of 64-127
	beq Normalized
	lda #$0080        ; we do 128 - A to flip the curve horizontally
	sec
	sbc Temp          ; subtract the value we saved
	sta Temp

Normalized:           ; Temp is now in the range 0-63, so we look up the answer from the table
	ldx Temp
	lda SineTable, X
	and #$00FF        ; We only want the low byte
	sta Temp          ; Temp is now the answer

CheckNegative:
	pla
	and #$0080        ; check if input was 128-255
	beq Done
	lda Temp          ; if input was 128-255, we need to multiply by -1
	eor #$FFFF
	inc               ; which we do by flipping all the bits and adding 1
	sta Temp

Done:
	lda Temp
	rts
.endproc

.proc Cosine
	clc
	adc #$40          ; just add 90 degrees and calculate sine
	jmp Sine
.endproc
