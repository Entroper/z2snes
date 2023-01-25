;----- Memory Map WRAM ---------------------------------------------------------
HOR_SPEED   = $0300     ; the horizontal speed
VER_SPEED   = $0301     ; the vertical speed
JOYPAD1     = $0302     ; data read from joypad 1
JOYTRIGGER1 = $0304     ; trigger read from joypad 1
JOYHELD1    = $0306     ; held buttons read from joypad 1
OAMMIRROR   = $0400     ; location of OAMRAM mirror in WRAM
;-------------------------------------------------------------------------------

;----- Game Constants ----------------------------------------------------------
    ; we use these constants to check for collisions with the screen boundaries
SCREEN_LEFT     = $00   ; left screen boundary = 0
SCREEN_RIGHT    = $ff   ; right screen boundary = 255
SCREEN_TOP      = $00   ; top screen boundary = 0
SCREEN_BOTTOM   = $df   ; bottom screen boundary = 223
    ; a simple constant to define the sprite movement speed
SPRITE_SPEED    = $02   ; the sprites will move 2 pixel per frame
    ; this makes the code a bit more readable
SPRITE_SIZE     = $08   ; sprites are 8 by 8 pixel
OAMMIRROR_SIZE  = $0220 ; OAMRAM can hold data for 128 sprites, 4 bytes each
    ; constants to use as masks
UP_BUTTON       = $0800
DOWN_BUTTON     = $0400
LEFT_BUTTON     = $0200
RIGHT_BUTTON    = $0100
;-------------------------------------------------------------------------------

;----- Assembler Directives ----------------------------------------------------
.p816                           ; tell the assembler this is 65816 code
.a8
.i16
;-------------------------------------------------------------------------------

.import LoadVRAM
.import LoadCGRAM
.import UpdateOAMRAM

.include "registers.inc"

.segment "SPRITEDATA"
SpriteData: .incbin "graphics/Sprites.vra"
ColorData:  .incbin "graphics/SpriteColors.pal"
;-------------------------------------------------------------------------------

.segment "CODE"
;-------------------------------------------------------------------------------
;   This is the entry point of the demo
;-------------------------------------------------------------------------------
.proc   ResetHandler
        sei                     ; disable interrupts
        clc                     ; clear the carry flag
        xce                     ; switch the 65816 to native (16-bit mode)
        rep #$10                ; set X and Y to 16-bit
        sep #$20                ; set A to 8-bit
        lda #$8f                ; force v-blanking
        sta INIDISP
        stz NMITIMEN            ; disable NMI
        ; set the stack pointer to $1fff
        ldx #$1fff              ; load X with $1fff
        txs                     ; copy X to stack pointer

        ; load sprites into VRAM
        tsx                     ; save current stack pointer
        pea $0000               ; push VRAM destination address to stack
        pea SpriteData          ; push sprite data source address to stack
        lda #$80                ; number of bytes (128/$80) to transfer
        pha
        jsr LoadVRAM            ; transfer sprite data to VRAM
        txs                     ; restore old stack pointer

        ; load color data into CGRAM
        tsx                     ; save current stack pointer
        lda #$80                ; destination address in CGRAM
        pha
        pea ColorData           ; color data source address
        lda #$20                ; number of bytes (32/$20) to transfer
        pha
        jsr LoadCGRAM           ; transfer color data into CGRAM
        txs                     ; restore old stack pointer

        ; initialize OAMRAM mirror
        ldx #$00
        ; upper-left sprite
        lda #(SCREEN_RIGHT/2 - SPRITE_SIZE) ; sprite 1, horizontal position
        sta OAMMIRROR, X
        inx                                 ; increment index
        lda #(SCREEN_BOTTOM/2 - SPRITE_SIZE); sprite 1, vertical position
        sta OAMMIRROR, X
        inx
        lda #$00                            ; sprite 1, name
        sta OAMMIRROR, X
        inx
        lda #$00                            ; no flip, palette 0
        sta OAMMIRROR, X
        inx
        ; upper-right sprite
        lda #(SCREEN_RIGHT/2)               ; sprite 3, horizontal position
        sta OAMMIRROR, X
        inx                                 ; increment index
        lda #(SCREEN_BOTTOM/2 - SPRITE_SIZE); sprite 3, vertical position
        sta OAMMIRROR, X
        inx
        lda #$01                            ; sprite 3, name
        sta OAMMIRROR, X
        inx
        lda #$00                            ; no flip, palette 0
        sta OAMMIRROR, X
        inx
        ; lower-left sprite
        lda #(SCREEN_RIGHT/2 - SPRITE_SIZE) ; sprite 2, horizontal position
        sta OAMMIRROR, X
        inx                                 ; increment index
        lda #(SCREEN_BOTTOM/2)              ; sprite 2, vertical position
        sta OAMMIRROR, X
        inx
        lda #$02                            ; sprite 2, name
        sta OAMMIRROR, X
        inx
        lda #$00                            ; no flip, palette 0
        sta OAMMIRROR, X
        inx
        ; lower-right sprite
        lda #(SCREEN_RIGHT/2)                ; sprite 4, horizontal position
        sta OAMMIRROR, X
        inx                                 ; increment index
        lda #(SCREEN_BOTTOM/2)              ; sprite 4, vertical position
        sta OAMMIRROR, X
        inx
        lda #$03                            ; sprite 4, name
        sta OAMMIRROR, X
        inx
        lda #$00                            ; no flip, palette 0
        sta OAMMIRROR, X
        inx
        ; move the other sprites off screen
        rep #$20                            ; set A to 16-bit
        lda #$f180                          ; Y = 241, X = -128
OAMLoop:
        sta OAMMIRROR, X
        inx
        inx
        cpx #(OAMMIRROR_SIZE - $20)
        bne OAMLoop
        ; correct bit 9 of horizontal/X position, set size to 8x8
        lda #$5555
OBJLoop:
        sta OAMMIRROR, X
        inx
        inx
        cpx #OAMMIRROR_SIZE
        bne OBJLoop

        sep #$20                            ; set A to 8-bit
        ; correct extra OAM byte for first four sprites
        ldx #$0200
        lda #$00
        sta OAMMIRROR, X

        ; set initial horizontal and vertical speed
        lda #SPRITE_SPEED
        sta HOR_SPEED
        sta VER_SPEED

        ; make Objects visible
        lda #$10
        sta TM
        ; release forced blanking, set screen to full brightness
        lda #$0f
        sta INIDISP
        ; enable NMI, turn on automatic joypad polling
        lda #$81
        sta NMITIMEN

        jmp GameLoop            ; all initialization is done
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Executed during V-Blank
;-------------------------------------------------------------------------------
.proc   GameLoop
        wai                                 ; wait for NMI / V-Blank

        ; read joypad 1
        ; check whether joypad is ready
WaitForJoypad:
        lda HVBJOY                          ; get joypad status
        and #$01                            ; check whether joypad done reading...
        beq WaitForJoypad                   ; ...if not, wait a bit more
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

        ; check the dpad, if any of the directional buttons where pressed or held,
        ; move the sprites accordingly
CheckUpButton:
        lda #$0000                          ; set A to zero
        ora JOYTRIGGER1                     ; check whether the up button was pressed this frame...
        ora JOYHELD1                        ; ...or held from last frame
        and #UP_BUTTON
        beq CheckUpButtonDone               ; if neither has occured, move on
        ; else, move sprites up
        ldy #$0000                          ; Y is the loop counter
        ldx #$0001                          ; set offset to 1, to manipulate sprite vertical positions
        sep #$20                            ; set A to 8-bit
MoveSpritesUp:
        lda OAMMIRROR, X
        sec
        sbc VER_SPEED
        bcc CorrectVerticalPositionDown     ; if vertical position is below zero, correct it down
        sta OAMMIRROR, X
        inx                                 ; increment X by 4
        inx
        inx
        inx
        iny
        cpy #$0004                          ; unless Y = 4, continue loop
        bne MoveSpritesUp
CheckUpButtonDone:
        rep #$20                            ; set A to 16-bit

CheckDownButton:
        lda #$0000                          ; set A to zero
        ora JOYTRIGGER1                     ; check whether the down button was pressed this frame...
        ora JOYHELD1                        ; ...or held from last frame
        and #DOWN_BUTTON
        beq CheckDownButtonDone             ; if neither has occured, move on
        ; else, move sprites down
        ldy #$0000                          ; Y is the loop counter
        ldx #$0001                          ; set offset to 1, to manipulate sprite vertical positions
        sep #$20                            ; set A to 8-bit
        ; check if sprites move below buttom boundry
        lda OAMMIRROR, X
        clc
        adc VER_SPEED
        cmp #(SCREEN_BOTTOM - 2 * SPRITE_SIZE)
        bcs CorrectVerticalPositionUp
MoveSpritesDown:
        lda OAMMIRROR, X
        clc
        adc VER_SPEED
        sta OAMMIRROR, X
        inx                                 ; increment X by 4
        inx
        inx
        inx
        iny
        cpy #$0004                          ; unless Y = 4, continue loop
        bne MoveSpritesDown
CheckDownButtonDone:
        rep #$20                            ; set A to 16-bit
        jmp CheckLeftButton                 ; continue input check

CorrectVerticalPositionDown:
        sep #$20                            ; set A to 8-bit
        stz OAMMIRROR + 1                   ; sprite 1, vertical position
        stz OAMMIRROR + 5                   ; sprite 3, vertical position
        lda #SPRITE_SIZE
        sta OAMMIRROR + 9                   ; sprite 2, vertical position
        sta OAMMIRROR + 13                  ; sprite 4, vertical position
        rep #$20                            ; set A to 16-bit
        jmp CheckLeftButton                 ; continue input check

CorrectVerticalPositionUp:
        sep #$20                            ; set A to 8-bit
        lda #(SCREEN_BOTTOM - 2 * SPRITE_SIZE)
        sta OAMMIRROR + 1                   ; sprite 1, vertical position
        sta OAMMIRROR + 5                   ; sprite 3, vertical position
        lda #(SCREEN_BOTTOM - SPRITE_SIZE)
        sta OAMMIRROR + 9                   ; sprite 2, vertical position
        sta OAMMIRROR + 13                  ; sprite 4, vertical position
        rep #$20                            ; set A to 16-bit

CheckLeftButton:
        lda #$0000                          ; set A to zero
        ora JOYTRIGGER1                     ; check whether the up button was pressed this frame...
        ora JOYHELD1                        ; ...or held from last frame
        and #LEFT_BUTTON
        beq CheckLeftButtonDone             ; if neither has occured, move on
        ; else, move sprites up
        ldy #$0000                          ; Y is the loop counter
        ldx #$0000                          ; set offset to 0, to manipulate sprite horizontal positions
        sep #$20                            ; set A to 8-bit
MoveSpritesLeft:
        lda OAMMIRROR, X
        sec
        sbc HOR_SPEED
        bcc CorrectHorizontalPositionRight
        sta OAMMIRROR, X
        inx                                 ; increment X by 4
        inx
        inx
        inx
        iny
        cpy #$0004                          ; unless Y = 4, continue loop
        bne MoveSpritesLeft
CheckLeftButtonDone:
        rep #$20                            ; set A to 16-bit

CheckRightButton:
        lda #$0000                          ; set A to zero
        ora JOYTRIGGER1                     ; check whether the down button was pressed this frame...
        ora JOYHELD1                        ; ...or held from last frame
        and #RIGHT_BUTTON
        beq CheckRightButtonDone            ; if neither has occured, move on
        ; else, move sprites down
        ldy #$0000                          ; Y is the loop counter
        ldx #$0000                          ; set offset to 0, to manipulate sprite horizontal positions
        sep #$20                            ; set A to 8-bit
        ; check whether sprites move beyond right boundry
        lda OAMMIRROR, X
        clc
        adc HOR_SPEED
        cmp #(SCREEN_RIGHT - 2 * SPRITE_SIZE)
        bcs CorrectHorizontalPositionLeft
MoveSpritesRight:
        lda OAMMIRROR, X
        clc
        adc HOR_SPEED
        sta OAMMIRROR, X
        inx                                 ; increment X by 4
        inx
        inx
        inx
        iny
        cpy #$0004                          ; unless Y = 4, continue loop
        bne MoveSpritesRight
CheckRightButtonDone:
        rep #$20                            ; set A to 16-bit
        jmp InputDone

CorrectHorizontalPositionRight:
        sep #$20                            ; set A to 8-bit
        stz OAMMIRROR + 0                   ; sprite 1, horizontal position
        stz OAMMIRROR + 8                   ; sprite 2, horizontal position
        lda #SPRITE_SIZE
        sta OAMMIRROR + 4                   ; sprite 3, horizontal position
        sta OAMMIRROR + 12                  ; sprite 4, horizontal position
        jmp InputDone

CorrectHorizontalPositionLeft:
        sep #$20                            ; set A to 8-bit
        lda #(SCREEN_RIGHT - 2 * SPRITE_SIZE)
        sta OAMMIRROR + 0                   ; sprite 1, horizontal position
        sta OAMMIRROR + 8                   ; sprite 2, horizontal position
        lda #(SCREEN_RIGHT - SPRITE_SIZE)
        sta OAMMIRROR + 4                   ; sprite 3, horizontal position
        sta OAMMIRROR + 12                  ; sprite 4, horizontal position

InputDone:
        sep #$20                            ; set A back to 8-bit

        jmp GameLoop
.endproc

.proc   NMIHandler
	lda RDNMI               ; read NMI status, acknowledge NMI

	tsx                     ; save old stack pointer
	pea OAMMIRROR           ; push mirror address to stack
	jsr UpdateOAMRAM        ; update OAMRAM
	txs                     ; restore old stack pointer

	rti
.endproc

.segment "VECTOR"
; native mode   COP,        BRK,        ABT,
.addr           $0000,      $0000,      $0000
;               NMI,        RST,        IRQ
.addr           NMIHandler, $0000,      $0000

.word           $0000, $0000    ; four unused bytes

; emulation m.  COP,        BRK,        ABT,
.addr           $0000,      $0000,      $0000
;               NMI,        RST,        IRQ
.addr           $0000,      ResetHandler, $0000
