.export JOYPAD1     = $0300     ; data read from joypad 1
.export JOYTRIGGER1 = $0302     ; trigger read from joypad 1
.export JOYHELD1    = $0304     ; held buttons read from joypad 1

.export GetJoypadInputs

.include "registers.inc"

.p816                           ; tell the assembler this is 65816 code
.a8
.i16

.proc GetJoypadInputs
	lda HVBJOY                          ; get joypad status
	and #$01                            ; check whether joypad done reading...
	beq GetJoypadInputs                 ; ...if not, wait a bit more
	; first, check for newly pressed buttons since last frame
	rep #$20                            ; set A to 16-bit
	lda JOY1L                           ; get new input from this frame
	ldy JOYPAD1                         ; get input from last frame
	sta JOYPAD1                         ; store new input from this frame
	tya                                 ; check for newly pressed buttons...
	eor JOYPAD1                         ; filter buttons that were not pressed last frame
	and JOYPAD1                         ; filter held buttons from last frame
	sta JOYTRIGGER1                     ; ...and store them
	; second, check for buttons held from last frame
	tya                                 ; get input from last frame
	and JOYPAD1                         ; filter held buttons from last frame...
	sta JOYHELD1                        ; ...store them
	rts
.endproc
