.p816
.a8
.i16

.import GetJoypadInputs

.import LoadOverworld
.import DoOverworldMovement
.import SetMode7Matrix

.include "registers.inc"

.segment "CODE"

.proc   ResetHandler
	sei                     ; disable interrupts
	clc                     ; clear the carry flag
	xce                     ; switch the 65816 to native (16-bit mode)
	rep #$10                ; set X and Y to 16-bit
	sep #$20                ; set A to 8-bit

	ldx #$1fff              ; set the stack pointer to $1fff
	txs

	jsr LoadOverworld

	jmp GameLoop            ; all initialization is done
.endproc

.proc   GameLoop
	wai                     ; wait for NMI / V-Blank

	jsr GetJoypadInputs
	jsr DoOverworldMovement

	jmp GameLoop
.endproc

.proc   NMIHandler
	lda RDNMI               ; read NMI status, acknowledge NMI

	jsr SetMode7Matrix

	rti
.endproc

.segment "VECTOR"
; native mode     COP,        BRK,          ABT,
.addr             $0000,      $0000,        $0000
;                 NMI,        RST,          IRQ
.addr             NMIHandler, $0000,        $0000

.word             $0000, $0000    ; four unused bytes

; emulation mode  COP,        BRK,          ABT,
.addr             $0000,      $0000,        $0000
;                 NMI,        RST,          IRQ
.addr             $0000,      ResetHandler, $0000
