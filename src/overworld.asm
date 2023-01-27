.p816
.a8
.i16

.include "registers.inc"

.export DoOverworld

OverworldTiles:          .incbin "graphics/overworld-tiles.vra"
OverworldTilePaletteMap: .incbin "graphics/overworld-tile-palettes.bin"
OverworldPalette:        .incbin "graphics/overworld.pal"

OverworldTilemap:                 .incbin "maps/overworld-tilemap.bin"
OverworldWestHyruleCompressed:    .incbin "maps/overworld-west.bin"
OverworldEastHyruleCompressed:    .incbin "maps/overworld-east.bin"
OverworldDeathMountainCompressed: .incbin "maps/overworld-death-mountain.bin"
OverworldMazeIslandCompressed:    .incbin "maps/overworld-maze-island.bin"

OverworldMap = $7E7000 ; store the decompressed overworld map from $7E:7000-7E:7FFF

.proc DoOverworld
	sep #$20                ; set A to 8-bit
	lda #$8f                ; force v-blanking
	sta INIDISP
	stz NMITIMEN            ; disable NMI

	jsr LoadOverworldMapData
	jsr LoadOverworldCharacters
	jsr LoadOverworldGraphics
	jsr LoadOverworldPalette

	sep #$20                ; set A to 8-bit
	lda #$07
	sta BGMODE
	stz M7A
	lda #$01
	sta M7A                 ; initialize Mode 7 matrix with identity
	stz M7B
	stz M7B
	stz M7C
	stz M7C
	stz M7D
	sta M7D
	stz M7X                 ; X and Y don't matter much yet
	stz M7Y

	stz BG1HOFS             ; initialize offsets
	stz BG1HOFS
	stz BG1VOFS
	stz BG1VOFS

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
	stz VMADDL                  ; start at VRAM address 0
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
	rep #$20                ; set A to 16-bit
	lda OverworldTiles, X   ; A now has one row of pixel data, HHHHHHHH LLLLLLLL high and low bits of each pixel
	sta Tile                ; working location

	txa                     ; X is the index into the tile data, we also need the index into the tile palette map
	lsr                     ; A tile is 16 bytes, so divide by 16 to get the tile palette index
	lsr
	lsr
	lsr
	clc
	asl                     ; Then shift it back up because palettes have 4 colors
	asl
	sta TilePaletteIndex    ; save the tile palette index

	phx                     ; save tile index
	sep #$20                ; set A to 8-bit
	ldy #$0008              ; loop 8 times, once per pixel in the row
PixelLoop:
	lda Tile                ; load first byte
	and #$01                ; get the low bit
	sta PixelValue
	lda $01                 ; load second byte
	and #$01                ; get the high bit
	clc
	asl                     ; shift it left so it's the high bit
	ora PixelValue          ; combine the bits from the bitplanes, A now has the 2bpp value of the pixel
	sta PixelValue          ; save 2bpp value
	stz PixelValue + 1      ; this will be 16-bit when we read it in a second

	rep #$20                ; set A to 16-bit
	lda TilePaletteIndex    ; get tile palette index
	ora PixelValue          ; now we have the actual palette index (this is where we do the 16-bit read)
	tax                     ; store it in X
	sep #$20                ; set A to 8-bit
	lda OverworldTilePaletteMap, X ; get the actual pixel value
	sta VMDATAH             ; store the pixel to VRAM

	rep #$20                ; set A to 16-bit
	lda Tile                ; get the original pixel data
	lsr                     ; shift it down for the next pixel
	sta Tile                ; put it back in memory
	sep #$20                ; set A to 8-bit
	dey
	bne PixelLoop

	plx                     ; get tile index back
	inx                     ; advance two bytes
	inx
	cpx #$0230              ; length of tile data
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
