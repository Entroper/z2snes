.ifndef REGISTERS_INC
REGISTERS_INC = 1

; https://snes.nesdev.org/wiki/MMIO_register_table
; https://www.youtube.com/watch?v=-4OOuRvTXrM (Retro Game Mechanics Explained)

INIDISP     = $2100     ; Fxxx BBBB (F)orced blanking, screen (B)rightness
INISET      = $2133     ; EXxx HOiI (E)xternal sync, e(X)ternal BG, (H)i-res, (O)verscan, obj (i)nterlace, screen (I)nterlace

OBJSEL      = $2101     ; SSSN NbBB OBJ (S)prite size, (N)ame select, name (B)ase address
OAMADDL     = $2102     ; address for accessing OAM
OAMADDH     = $2103     ; Pxxx xxxB (P)riority, address high (B)it
OAMDATA     = $2104     ; data for OAM write
OAMDATAR    = $2138     ; Read OAM data

BGMODE      = $2105     ; 4321 PMMM Background (#) tile size, BG3 (P)riority, BG (M)ode
MOSAIC      = $2106     ; SSSS 4321 Mosaic (S)ize, mosaic BG (#) enable
BG1SC       = $2107     ; AAAA AAYX Tilemap VRAM (A)ddress, vertical and horizontal tilemap count
BG2SC       = $2108
BG3SC       = $2109
BG4SC       = $210A
BG12NBA     = $210B     ; 2222 1111 BG (#) base address
BG34NBA     = $210C     ; 4444 3333 BG (#) base address
BG1HOFS     = $210D     ; BG 1 horizontal scroll
BG1VOFS     = $210E     ; BG 1 vertical scroll
BG2HOFS     = $210F     ; 10 bits for mode 0-6
BG2VOFS     = $2110     ; 13 bits for mode 7
BG3HOFS     = $2111
BG3VOFS     = $2112
BG4HOFS     = $2113
BG4VOFS     = $2114

VMAINC      = $2115     ; Mxxx RRII VRAM address increment (M)ode, (R)emapping, (I)ncrement size
VMADDL      = $2116     ; address for VRAM read and write
VMADDH      = $2117
VMDATAL     = $2118     ; data for VRAM write
VMDATAH     = $2119
VMDATALR    = $2139     ; data for VRAM read
VMDATAHR    = $213A

M7SEL       = $211A     ; RFxx xxYX Mode 7 tilemap (R)epeat, (F)ill, flip vertical/horizontal
M7A         = $211B     ; Mode 7 matrix parameters
M7B         = $211C     ; Where H = BG1HOFS, V = BG1VOFS
M7C         = $211D     ; | x' | = | A B | x | H + x - X | + | X |
M7D         = $211E     ; | y' |   | C D |   | V + y - Y |   | Y |
M7X         = $211F
M7Y         = $2120

CGADD       = $2121     ; address for CGRAM read and write
CGDATA      = $2122     ; data for CGRAM write
CGDATAR     = $213B     ; data for CGRAW read

W12SEL      = $2123     ; DdCc BbAa Enable (ABCD) and Invert (abcd) windows for BG1 (AB) and BG2 (CD)
W34SEL      = $2124     ; BG3 and BG4
WOBJSEL     = $2125     ; Object and color window (obj in lower 4 bits)
WH0         = $2126     ; Window 1 left position
WH1         = $2127     ; Window 1 right position
WH2         = $2128     ; Window 2 left position
WH3         = $2129     ; Window 2 right position
W0L         = WH0       ; Aliases
W0R         = WH1
W1L         = WH2
W1R         = WH3
WBGLOG      = $212A     ; 4433 2211 Window mask logic for BG layers (00 = OR, 01 = AND, 10 = XOR, 11 = XNOR)
WOBJLOG     = $212B     ; xxxx CCOO Window mask logic for obj and color layers
TM          = $212C     ; xxxO 4321 Main screen layer enable (Through Main)
TS          = $212D     ; xxxO 4321 Sub screen layer enable (Through Sub)
TMW         = $212E     ; Through Main Window
TSW         = $212F     ; Through Sub Window
CGWSEL      = $2130     ; MMSS xxFD (M)ain/(S)ub screen (00 = enable, 01 = color window, 10 = color window inverse, 11 = disable), use (F)ixed color, enable (D)irect color
CGADSUB     = $2131     ; AHBO 4321 Color math (A)dd/subtract, (H)alf, (B)ackground, layers
COLDATA     = $2132     ; BGRC CCCC write Fixed (C)olor data to (RGB) channel

MPYL        = $2134     ; 24-bit signed multiplication result
MPYM        = $2135
MPYH        = $2136

SLHV        = $2137     ; Software latch for H/V counters
OPHCT       = $213C     ; Output horizontal counter
OPVCT       = $213D     ; Output vertical counter

STAT77      = $213E     ; TRHx VVVV Obj (T)ime up, obj (R)ange up, (H)elper, PPU1 (V)ersion
STAT78      = $213F     ; FLxR VVVV (F)rame odd, external (L)atch, (R)egion (0 = NTSC, 1 = PAL), PPU2 (V)ersion

APUIO0      = $2140     ; APU read/write
APUIO1      = $2141
APUIO2      = $2142
APUIO3      = $2143

WMDATA      = $2180     ; WRAM data
WMADDL      = $2181     ; WRAM address
WMADDM      = $2182
WMADDH      = $2183

JOYOUT      = $4016     ; Joypad output (latches standard controllers)
JOUSER0     = $4016     ; Joypad 1 serial input
JOYSER1     = $4017     ; Joypad 2 serial input

NMITIMEN    = $4200     ; NxVH xxxJ VBlank e(N)able, timer IRQ mode (VH), (J)oypad auto-read enable

WRIO        = $4201     ; 21xx xxxx Joypad port 1/2 I/O write
RDIO        = $4213     ; read

WRMPYA      = $4202     ; Unsigned multiply A
WRMPYB      = $4203     ; Unsigned multiply B, starts 8-cycle multiply
WRDIVL      = $4204     ; Unsigned dividend
WRDIVH      = $4205
WRDIVB      = $4206     ; unsigned divisor, starts 16-cycle divide
RDDIVL      = $4214     ; unsigned quotient
RDDIVH      = $4215
RDMPYL      = $4216     ; unsigned product or remainder
RDMPYH      = $4217

HTIMEL      = $4207     ; H counter target for IRQ timer
HTIMEH      = $4208
VTIMEL      = $4209     ; V counter target for IRQ timer
VTIMEH      = $420A

MDMAEN      = $420B     ; 7654 3210 DMA enable register
HDMAEN      = $420C     ; 7654 3210 HDMA enable register

MEMSEL      = $420D     ; FastROM enable (LSB)

RDNMI       = $4210     ; Nxxx VVVV VBlank (N)MI flag, CPU (V)ersion
TIMEUP      = $4211     ; Timer IRQ flag (MSB)

HVBJOY      = $4212     ; VHxx xxxJ H/V blank flags and (J)oypad auto-read in progress

JOY1L       = $4218     ; auto-read data for joypads
JOY1H       = $4219
JOY2L       = $421A
JOY2H       = $421B
JOY3L       = $421C
JOY3H       = $421D
JOY4L       = $421E
JOY4H       = $421F

DMABBASE    = $2100     ; Base address for B-bus DMA transfers

; DMA transfer formats:
; 000: 1 byte write once to 1 register
; 001: 2 bytes write once to 2 registers
; 010: 2 bytes write twice to 1 register
; 011: 4 bytes write twice to 2 registers
; 100: 4 bytes write once to 4 registers
; Address increment modes:
; 00: Increment
; 10: Decrement
; x1: No update
DMA0PARAM   = $4300     ; DIxA AFFF DMA Channel 0 (D)irection, (I)ndirect, (A)ddress increment mode, (F)ormat
DMA0ADDB    = $4301     ; DMA Channel 0 B-Bus address (PPU)
DMA0ADDAL   = $4302     ; DMA Channel 0 A-Bus address
DMA0ADDAM   = $4303
DMA0ADDAH   = $4304
DMA0AMTL    = $4305     ; DMA Channel 0 number of bytes to transfer
DMA0AMTH    = $4306
DMA0INDH    = $4307     ; DMA Channel 0 intermediate data address during HDMA indirect transfer
DMA0ADDA2L  = $4308     ; DMA Channel 0 intermediate A-bus address
DMA0ADDA2H  = $4309     
DMA0LINES   = $430A     ; RLLL LLLL HDMA (R)eload, and (L)ine counter
DMA0UNUSED  = $430B     ; DMA Channel 0 unused register

DMA1PARAM   = $4310
DMA1ADDB    = $4311
DMA1ADDAL   = $4312
DMA1ADDAM   = $4313
DMA1ADDAH   = $4314
DMA1AMTL    = $4315
DMA1AMTH    = $4316
DMA1INDH    = $4317
DMA1ADDA2L  = $4318
DMA1ADDA2H  = $4319
DMA1LINES   = $431A
DMA1UNUSED  = $431B

DMA2PARAM   = $4320
DMA2ADDB    = $4321
DMA2ADDAL   = $4322
DMA2ADDAM   = $4323
DMA2ADDAH   = $4324
DMA2AMTL    = $4325
DMA2AMTH    = $4326
DMA2INDH    = $4327
DMA2ADDA2L  = $4328
DMA2ADDA2H  = $4329
DMA2LINES   = $432A
DMA2UNUSED  = $432B

DMA3PARAM   = $4330
DMA3ADDB    = $4331
DMA3ADDAL   = $4332
DMA3ADDAM   = $4333
DMA3ADDAH   = $4334
DMA3AMTL    = $4335
DMA3AMTH    = $4336
DMA3INDH    = $4337
DMA3ADDA2L  = $4338
DMA3ADDA2H  = $4339
DMA3LINES   = $433A
DMA3UNUSED  = $433B

DMA4PARAM   = $4340
DMA4ADDB    = $4341
DMA4ADDAL   = $4342
DMA4ADDAM   = $4343
DMA4ADDAH   = $4344
DMA4AMTL    = $4345
DMA4AMTH    = $4346
DMA4INDH    = $4347
DMA4ADDA2L  = $4348
DMA4ADDA2H  = $4349
DMA4LINES   = $434A
DMA4UNUSED  = $434B

DMA5PARAM   = $4350
DMA5ADDB    = $4351
DMA5ADDAL   = $4352
DMA5ADDAM   = $4353
DMA5ADDAH   = $4354
DMA5AMTL    = $4355
DMA5AMTH    = $4356
DMA5INDH    = $4357
DMA5ADDA2L  = $4358
DMA5ADDA2H  = $4359
DMA5LINES   = $435A
DMA5UNUSED  = $435B

DMA6PARAM   = $4360
DMA6ADDB    = $4361
DMA6ADDAL   = $4362
DMA6ADDAM   = $4363
DMA6ADDAH   = $4364
DMA6AMTL    = $4365
DMA6AMTH    = $4366
DMA6INDH    = $4367
DMA6ADDA2L  = $4368
DMA6ADDA2H  = $4369
DMA6LINES   = $436A
DMA6UNUSED  = $436B

DMA7PARAM   = $4370
DMA7ADDB    = $4371
DMA7ADDAL   = $4372
DMA7ADDAM   = $4373
DMA7ADDAH   = $4374
DMA7AMTL    = $4375
DMA7AMTH    = $4376
DMA7INDH    = $4377
DMA7ADDA2L  = $4378
DMA7ADDA2H  = $4379
DMA7LINES   = $437A
DMA7UNUSED  = $437B

.endif
