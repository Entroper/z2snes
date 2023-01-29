.p816
.a8
.i16

.include "registers.inc"
.include "joypad.inc"

.export LoadOverworld
.export DoOverworldMovement
.export SetMode7Matrix

.import JOYPAD1

.import Sine
.import Cosine

OverworldTiles:          .incbin "graphics/overworld-tiles.vra"
OverworldTilePaletteMap: .incbin "graphics/overworld-tile-palettes.bin"
OverworldPalette:        .incbin "graphics/overworld.pal"

OverworldTilemap:                 .incbin "maps/overworld-tilemap.bin"
OverworldWestHyruleCompressed:    .incbin "maps/overworld-west.bin"
OverworldEastHyruleCompressed:    .incbin "maps/overworld-east.bin"
OverworldDeathMountainCompressed: .incbin "maps/overworld-death-mountain.bin"
OverworldMazeIslandCompressed:    .incbin "maps/overworld-maze-island.bin"

OverworldMap = $7E7000 ; store the decompressed overworld map from $7E:7000-7E:7FFF

MAPPOSX  = $1000
MAPPOSY  = $1002
MINPOSX  = $0080
MAXPOSX  = $0380
MINPOSY  = $0070
MAXPOSY  = $0390
MAPANGLE = $1004
MAPZOOM  = $1006
MAXZOOM  = $7F   ; zoom value is fixed point, where $0040 is 1.0
MINZOOM  = $20   ; we do this for precision reasons with mode 7 multiply

; $188, $178
.proc LoadOverworld
	sep #$20                ; set A to 8-bit
	lda #$8f                ; force v-blanking
	sta INIDISP
	stz NMITIMEN            ; disable NMI

	jsr LoadOverworldMapData
	jsr LoadOverworldCharacters
	jsr LoadOverworldGraphics
	jsr LoadOverworldPalette

	rep #$20                ; set A to 16-bit
	lda #$0188              ; initial x scroll
	sta MAPPOSX
	lda #$0178              ; initial y scroll
	sta MAPPOSY
	stz MAPANGLE            ; initial rotation (none)
	lda #$0040              ; initial zoom (1x)
	sta MAPZOOM

	sep #$20                ; set A to 8-bit
	lda #$07
	sta BGMODE
	lda #$80
	sta M7SEL               ; use CGRAM color 0 off the edge, no flipping
	stz M7A
	lda #$01
	sta M7A                 ; initialize Mode 7 matrix with identity
	stz M7B
	stz M7B
	stz M7C
	stz M7C
	stz M7D
	sta M7D

	jsr SetMode7Matrix      ; set up Mode 7 transform parameters

	lda #$01                ; enable BG1
	sta TM

	lda #$0f
	sta INIDISP             ; release forced blanking, set screen to full brightness
	lda #$81
	sta NMITIMEN            ; enable NMI, turn on automatic joypad polling

	rts
.endproc

.proc LoadOverworldMapData
	; Here we decompress the map data.  We need to store this in memory to refer to
	; later, so we do the full decompression before loading characters to VRAM.
	; Map data is RLE, with the upper 4 bytes being a count, and the lower 4 being the map tile.
	; An overworld map is 64x64, 4096 bytes ($1000).  We can store this from $7E:7000-7E:7FFF.
	MapValue = $00
	rep #$20        ; set A to 16-bit so we can
	lda #$0000      ; clear the high byte
	sep #$20        ; set A to 8-bit
	rep #$10        ; set X,Y to 16-bit
	ldy #$0000
	ldx #$0000

MapLoop:
	lda OverworldWestHyruleCompressed, Y ; load a byte, byte is RRRR VVVV
	sta MapValue
	lsr                                  ; RRRR is the run length - 1
	lsr                                  ; VVVV is the value
	lsr                                  ; shift down to get 0000 RRRR
	lsr
	clc
	adc #$01                             ; add 1 to get the run length
	phy                                  ; need to use Y to repeat the run
	tay                                  ; put run length in Y
	lda MapValue
	and #$0F                             ; get the value to repeat 0000 VVVV

RLELoop:
	sta OverworldMap, X                  ; store the value to the map
	inx                                  ; next map position
	dey                                  ; for the length of the run
	bne RLELoop

	ply                                  ; get the compressed index back
	iny                                  ; go to the next compressed byte
	cpx #$1000                           ; length of decompressed map data
	bne MapLoop                          ; keep going until we fill the map

	rts
.endproc

.proc LoadOverworldCharacters
	; The overworld tile map describes the 4 characters belonging to each overworld square.
	; We've got the entire decompressed map, so we just walk through it and put the characters
	; into VRAM.  The size of the map is exactly the size of the mode 7 background: 64x64 map
	; squares, each 2x2 characters, for 128x128.
	rep #$20                    ; set A to 16-bit so we can
	lda #$0000                  ; clear the high byte
	sep #$20                    ; set A to 8-bit
	rep #$10                    ; set X,Y to 16-bit
	ldx #$0000
	stx VMADDL                  ; start at VRAM address 0
	stz VMAINC                  ; increment VRAM when writing to VMADDL

MapLoop:
	ldy #$40                    ; 64 map squares per row
RowLoopTop:
	phy                         ; save the index
	ldy #$0000                  ; zero the high part of Y
	lda OverworldMap, X         ; get a map square
	clc
	asl                         ; multiply by 4
	asl
	tay                         ; use it to index
	lda OverworldTilemap, Y     ; the overworld tilemap (upper-left character)
	sta VMDATAL                 ; write it to VRAM
	lda OverworldTilemap + 2, Y ; get the upper-right character
	sta VMDATAL                 ; write it to VRAM
	inx                         ; next map square
	ply                         ; get the column index back
	dey
	bne RowLoopTop

	ldy #$40                    ; process the same row again, bottom tiles this time
	rep #$20                    ; set A to 16-bit
	txa
	sec
	sbc #$40                    ; go back to the beginning of the row
	tax
	lda #$0000                  ; clear high byte of A to avoid shenanigans
	sep #$20                    ; set A to 8-bit

RowLoopBottom:
	phy                         ; save the index
	ldy #$0000                  ; zero the high part of Y
	lda OverworldMap, X         ; get a map square
	clc
	asl                         ; multiply by 4
	asl
	tay                         ; use it to index
	lda OverworldTilemap + 1, Y ; the overworld tilemap (lower-left character)
	sta VMDATAL                 ; write it to VRAM
	lda OverworldTilemap + 3, Y ; get the lower-right character
	sta VMDATAL                 ; write it to VRAM
	inx                         ; next map square
	ply                         ; get the column index back
	dey
	bne RowLoopBottom

	cpx #$1000                  ; size of map data
	bne MapLoop

	rts
.endproc

.proc LoadOverworldGraphics
	; Tile data is in 2bpp planar format, and we need to convert to 8bpp linear for mode 7.
	rep #$20        ; set A to 16-bit so we can
	lda #$0000      ; clear the high byte
	sep #$20        ; set A to 8-bit
	rep #$10        ; set X,Y to 16-bit
	ldx #$0000
	stx VMADDL      ; start at VRAM address 0
	lda #$80
	sta VMAINC      ; increment VRAM 2 bytes when high byte is written

	Tile             = $00
	PixelValue       = $02
	TilePaletteIndex = $04
TileLoop:
	rep #$20                       ; set A to 16-bit
	txa                            ; X is the index into the tile data, we also need the index into the tile palette map
	lsr                            ; A tile is 16 bytes, and a palette is 4 bytes, so the palette index is the
	lsr                            ; tile index divided by 4...
	and #$FFFC                     ; with the lower two bits masked off (these will be the 2bpp color value of the tile)
	sta TilePaletteIndex           ; save the tile palette index
	sep #$20                       ; set A to 8-bit

    lda OverworldTiles, X          ; load a byte of tile data
	sta Tile                       ; put it in memory
	lda OverworldTiles + 8, X      ; load the same byte from the other bitplane, 8 bytes after
	sta Tile + 1                   ; put it in memory
	phx                            ; save tile index
	ldy #$0008                     ; loop 8 times, once for each bit (a pixel is one bit from each of the two bitplanes)
PixelLoop:
	lda Tile                       ; load byte from the low bitplane
	asl                            ; shift the high bit into carry
	sta Tile                       ; save the shifted byte
	lda #$00                       ; clear A
	rol                            ; rotate carry into A, A now has the value from the low bitplane
	sta PixelValue                 ; store that in memory
	lda Tile + 1                   ; load byte from the high bitplane
	asl                            ; shift the high bit into carry
	sta Tile + 1                   ; save the shifted byte
	lda #$00                       ; clear A
	rol                            ; rotate carry into A, A now has the value from the high bitplane
	asl                            ; shift it to its proper position
	ora PixelValue                 ; combine it with the value from the low bitplane, A now has the 2bpp value
	sta PixelValue                 ; save that to memory
	stz PixelValue + 1             ; this needs to be zeroed because A will be 16-bit when it's read

	rep #$20                       ; set A to 16-bit
	lda TilePaletteIndex           ; get tile palette index
	ora PixelValue                 ; now we have the actual palette index (this is where we do the 16-bit read)
	tax                            ; store it in X
	sep #$20                       ; set A to 8-bit
	lda OverworldTilePaletteMap, X ; get the actual pixel value
	sta VMDATAH                    ; store the pixel to VRAM

	dey
	bne PixelLoop                  ; loop to process 8 bits from each bitplane, one row of pixels

	plx                            ; get tile index back
	inx                            ; advance to the next byte
	rep #$20                       ; set A to 16-bit
	txa
	and #$0007                     ; get the lower 3 bits of the tile index
	bne :+                         ; if it's divisible by 8...
	txa                            ; get the original value back
	clc
	adc #$08                       ; skip 8 bytes (because we already read them, they were the high bitplane)
	tax
	sep #$20                       ; set A to 8-bit
:
	cpx #$0230                     ; length of tile data
	bne TileLoop

	rts
.endproc

.proc LoadOverworldPalette
	rep #$20        ; set A to 16-bit so we can
	lda #$0000      ; clear the high byte
	sep #$20        ; set A to 8-bit
	rep #$10        ; set X,Y to 16-bit

	stz CGADD       ; start at CGRAM address 0
	ldx #$0000
Loop:
	lda OverworldPalette, X ; get a byte of palette data
	sta CGDATA              ; write it to CGRAM
	inx
	cpx #$005C              ; length of palette data
	bne Loop

	rts
.endproc

.proc DoOverworldMovement
	rep #$20                            ; set A to 16-bit
	; check the dpad, if any of the directional buttons are pressed,
	; move the screen accordingly
CheckUpButton:
	lda JOYPAD1                         ; read joypad buttons pressed
	and #BUTTON_UP
	beq CheckDownButton
	lda MAPPOSY
	cmp #MINPOSY
	beq CheckDownButton
	dec
	sta MAPPOSY

CheckDownButton:
	lda JOYPAD1
	and #BUTTON_DOWN
	beq CheckLeftButton
	lda MAPPOSY
	cmp #MAXPOSY
	beq CheckLeftButton
	inc
	sta MAPPOSY

CheckLeftButton:
	lda JOYPAD1
	and #BUTTON_LEFT
	beq CheckRightButton
	lda MAPPOSX
	cmp #MINPOSX
	beq CheckRightButton
	dec
	sta MAPPOSX

CheckRightButton:
	lda JOYPAD1
	and #BUTTON_RIGHT
	beq CheckXButton
	lda MAPPOSX
	cmp #MAXPOSX
	beq CheckXButton
	inc
	sta MAPPOSX

CheckXButton:
	lda JOYPAD1
	and #BUTTON_X
	beq CheckYButton
	lda MAPZOOM
	and #$00FF        ; just read one byte
	cmp #MINZOOM
	beq CheckYButton
	dec
	sta MAPZOOM

CheckYButton:
	lda JOYPAD1
	and #BUTTON_Y
	beq CheckRButton
	lda MAPZOOM
	and #$00FF
	cmp #MAXZOOM
	beq CheckRButton
	inc
	sta MAPZOOM

CheckRButton:
	lda JOYPAD1
	and #BUTTON_R
	beq CheckLButton
	lda MAPANGLE
	dec
	sta MAPANGLE

CheckLButton:
	lda JOYPAD1
	and #BUTTON_L
	beq Done
	lda MAPANGLE
	inc
	sta MAPANGLE

Done:
	rts
.endproc

.proc SetMode7Matrix
	sep #$20          ; set A to 8-bit
	lda MAPPOSX
	sta M7X
	lda MAPPOSX + 1
	sta M7X
	lda MAPPOSY
	sta M7Y
	lda MAPPOSY + 1
	sta M7Y

	TempX = $00
	TempY = $02
	rep #$20          ; set A to 16-bit
	lda MAPPOSX
	sec
	sbc #$80
	sta TempX
	lda MAPPOSY
	sec
	sbc #$70
	sta TempY

	sep #$20          ; set A to 8-bit
	lda TempX
	sta BG1HOFS
	lda TempX + 1
	sta BG1HOFS
	lda TempY
	sta BG1VOFS
	lda TempY + 1
	sta BG1VOFS

	; Now we do trig.
	TempCos    = $02
	TempSin    = $04
	TempNegSin = $06
	rep #$20          ; set A to 16-bit
	lda MAPANGLE
	jsr Cosine        ; get the cosine
	sta TempCos
	sep #$20          ; set A to 8-bit
    lda TempCos       ; multiply -- this will be signed, fixed-point where A is IIII IIII.FFFF FFFF
	sta M7A           ; and B is II.FF FFFF
	lda TempCos + 1
	sta M7A
	lda MAPZOOM
	sta M7B
	lda MPYL          ; we need the result shifted down six times, but it's 24-bit, and we only need
	asl               ; 16 bits of it, so we can shift it up twice and take the two high bytes
	sta TempCos
	lda MPYM
	rol
	sta TempCos + 1
	lda MPYH
	rol
	sta TempCos + 2   ; this will overwrite, but if we do it in the right order, doesn't matter
	lda TempCos
	asl
	lda TempCos + 1
	rol
	sta TempCos
	lda TempCos + 2
	rol
	sta TempCos + 1

	rep #$20          ; set A to 16-bit
	lda MAPANGLE
	jsr Sine          ; get the sine
	sta TempSin
	sep #$20          ; set A to 8-bit
    lda TempSin       ; multiply -- this will be signed, fixed-point where A is IIII IIII.FFFF FFFF
	sta M7A           ; and B is II.FF FFFF
	lda TempSin + 1
	sta M7A
	lda MAPZOOM
	sta M7B
	lda MPYL          ; we need the result shifted down six times, but it's 24-bit, and we only need
	asl               ; 16 bits of it, so we can shift it up twice and take the two high bytes
	sta TempSin
	lda MPYM
	rol
	sta TempSin + 1
	lda MPYH
	rol
	sta TempSin + 2   ; this will overwrite, but if we do it in the right order, doesn't matter
	lda TempSin
	asl
	lda TempSin + 1
	rol
	sta TempSin
	lda TempSin + 2
	rol
	sta TempSin + 1

	rep #$20          ; set A to 16-bit
	lda TempSin       ; get the negative sine
	eor #$FFFF
	inc
	sta TempNegSin

	; Now we've done the trig calculations, set up the matrix.
	rep #$10          ; set X and Y to 16-bit
	sep #$20
	lda TempCos
	sta M7A
	lda TempCos + 1
	sta M7A
	lda TempNegSin
	sta M7B
	lda TempNegSin + 1
	sta M7B
	lda TempSin
	sta M7C
	lda TempSin + 1
	sta M7C
	lda TempCos
	sta M7D
	lda TempCos + 1
	sta M7D

	rts
.endproc
