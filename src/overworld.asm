OverworldTiles: .incbin "graphics/overworld-tiles.vra"
OverworldTilemap:     ; Indexes into above table, 4 tiles per overworld square
.byte $0C,$0D,$0E,$0F ; Town               
.byte $20,$20,$20,$20 ; Grotto             
.byte $10,$11,$12,$13 ; Palace             
.byte $08,$08,$09,$09 ; Bridge             
.byte $1C,$1C,$1C,$1C ; Desert             
.byte $1D,$1D,$1D,$1D ; Grass              
.byte $18,$19,$1A,$1B ; Forest             
.byte $1F,$1F,$1F,$1F ; Swamp              
.byte $0A,$0B,$FE,$FE ; Graveyard          
.byte $FE,$FE,$FE,$FE ; Road               
.byte $1E,$1E,$1E,$1E ; Lava               
.byte $14,$15,$16,$17 ; Mountain           
.byte $1E,$1E,$1E,$1E ; Water              
.byte $1E,$1E,$1E,$1E ; Water (walkable)   
.byte $04,$05,$06,$07 ; Rock               
.byte $00,$01,$02,$03 ; Spider             
