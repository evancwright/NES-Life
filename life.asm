OCCUPIED equ $80 
OAM_ADDR equ $2003
OAM_DATA equ $2004
CURSOR_SPR equ 6
board equ $200 ; 256 squares (1/4K)
;neighborCounts equ $300 (1/4K)
tileRam equ $409 ; 960 bytes  $409- $7c9  (1K)

 
;;;;;;;;;;;;;;;
  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  
  .rsset $0000       ; put pointers in zero page
pointerLo  .rs 1   ; pointer variables are declared in RAM
pointerHi  .rs 1   ; low byte first, high byte immediately after
boardPtrLo .rs 1
boardPtrHi .rs 1
PPULo .rs 1
PPUHi .rs 1

Index .rs 1
tempY1 .rs 1
tempY2 .rs 1 
counter2 .rs 1
drawing .rs 1  ; 0 = not drawing , 1 = drawing

upLeftOffset .rs 1
upOffset .rs 1 
upRightOffset .rs 1

leftOffset .rs 1
rightOffset .rs 1
downLeftOffset .rs 1
downOffset .rs 1
downRightOffset .rs 1
counter .rs 1
ncount .rs 1 ; neighbor count - temp variable

  .bank 0
  .org $C000 
RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  
  

  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

	LDA #$00
	STA $2005 ;why twice?
	STA $2005

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0200, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0300, x
  INX
  BNE clrmem
 
	jsr InitBoard 
	
	;test code to see if slicing up the drawing works
	ldx #0
	stx counter

	jsr CopyBoardToTileMap
	
	lda #0
	sta counter	

;test worked 
	lda #0
	sta counter2


vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1


 
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2


;;  LDA #%10000000   ;intensify blues
;;  STA $2001
  
	jsr LoadPalettes
	jsr InitSprites ;hide all except 1
	
 
	LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
	STA $2000

	LDA #%00011110   ; enable sprites, enable background, no clipping on left side
;	LDA #%00001110   ; disable sprites, enable background, no clipping on left side
	STA $2001
  
Forever:

	lda drawing  ; wait for NMI to set draw back to 0
	bne Forever

	lda #0
	jsr CountNeighbors
	jsr AddAndRemoveOrganisms
	jsr CopyBoardToTileMap
	
	;ready to copy name table to PPU
	
	lda #0
	sta counter
	
	;set data ptr
	lda #HIGH(tileRam)
	sta pointerHi
	lda #LOW(tileRam)
	sta pointerLo
	
	;set PPU addr
	lda #$20 ;reset PPU
	sta PPUHi
	lda #$00
	sta PPULo
 
  
	lda #1
	sta drawing
	JMP Forever     ;jump back to Forever, infinite loop

InitBoard
	ldy #0
	lda #0
	sta Index
	sta drawing
lpx:	
	sta board,y
	iny 
	bne lpx

	;blinker
	lda #OCCUPIED
;	ldy #17
;	sta board,y
;	ldy #33
;	sta board,y
;	ldy #49
;	sta board,y
	
	;glider
	ldy #120
	sta board,y	
	ldy #136
	sta board,y
	ldy #152
	sta board,y
	ldy #151
	sta board,y
	ldy #134
	sta board,y

	
	lda #HIGH(tileRam)
	sta pointerHi
	lda #LOW(tileRam)
	sta pointerLo

	lda #$20
	sta PPUHi
	lda #$00
	sta PPULo


	lda #HIGH(board)
	sta boardPtrHi
	lda #LOW(board)
	sta boardPtrLo
	
	rts


;sets the 8 pallets and bg color
LoadPalettes 
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down

	rts
	
;populates the attribute table with all the same palette
SetAttrTable
	rts
  
;copies the board into the tilemap in ram
CopyBoardToTileMap

	lda #HIGH(board)
	sta boardPtrHi
	lda #LOW(board)
	sta boardPtrLo

	lda #HIGH(tileRam)
	sta pointerHi
	lda #LOW(tileRam)
	sta pointerLo
	 
	ldx #0
	stx counter2
.lp1b 
	jsr DrawBoardRowToRAM
	inc counter2
	lda counter2
	cmp #15
	bne .lp1b	 
	rts

;Draw board (draws 32 tiles)
;Upates the name to reflect what is in the board
WriteBoardToPPU 
 	LDA $2002  ; read PPU status to reset the high/low latch
	lda PPUHi
	STA $2006             ; write the high byte of $2000 address
	lda PPULo
	STA $2006             ; write the low byte of $2000 address
;copy 32 tiles byte
	ldy #0
innerLp:
	lda [pointerLo],y
	sta $2007
	iny
	cpy #32
	bne innerLp
	
	inc counter
	lda counter
	cmp #30 ; copy 30 rows (30 x 8 = 240 pixels)
	beq reset1
	;add 32 to data pointer
	clc
	lda pointerLo
	adc #32
	sta pointerLo
	lda pointerHi
	adc #0
	sta pointerHi
	;add 32 to data pointer
	clc
	lda PPULo
	adc #32
	sta PPULo
	lda PPUHi
	adc #0
	sta PPUHi

 	rts
reset1:
	lda #0
	sta counter
	sta drawing ; clear draw flag
	lda #$20 ;reset PPU
	sta PPUHi
	lda #$00
	sta PPULo
	rts
 
;copies 1 row from the board to the tilemap in ram
;(64 tiles) 
;updates BoardPtr
DrawBoardRowToRAM
  	ldx #0 ;draw 16 tops
	ldy #0
	sty tempY1
	sty tempY2
drawLp1:
	ldy tempY1
	lda [boardPtrLo],y
	bpl empty1
	iny
	sty tempY1
	ldy tempY2
	lda #$42  ; draw top half of organism
	sta [pointerLo],y
	iny 
	lda #$43
	sta [pointerLo],y
	jmp d1
empty1:
	iny
	sty tempY1
	ldy tempY2
	lda #$40  ; draw empty lower half
	sta  [pointerLo],y
	iny
	lda #$41
	sta  [pointerLo],y
d1:	
	iny
	sty tempY2
 	inx
	cpx #16
	bne drawLp1
	;draw 16 bottoms

	ldy #0 ; backup board ptr
	sty tempY1
	ldx #0
drawLp2:
	ldy tempY1
	lda [boardPtrLo],y
	bpl empty2
	iny
	sty tempY1
	ldy tempY2
	lda #$52
	sta [pointerLo],y
	iny 
	lda #$53
	sta [pointerLo],y
	jmp d2
empty2:
	iny
	sty tempY1
	ldy tempY2
	lda #$50
	sta [pointerLo],y
	iny
	lda #$51
	sta [pointerLo],y
d2:	
	iny
	sty tempY2
	inx
	cpx #16
	bne drawLp2
	;add 16 to board ptr
	clc
	lda boardPtrLo
	adc #16
	sta boardPtrLo
	lda boardPtrHi
	adc #0
	sta boardPtrHi
	;add 64 to ram ptr
	clc
	lda pointerLo
	adc #64
	sta pointerLo
	lda pointerHi
	adc #0
	sta pointerHi
 	
xt:	rts 

 

;update board
CountNeighbors
	lda #239
	sta upLeftOffset
	lda #240
	sta upOffset
	lda #241
	sta upRightOffset 
	lda #255
	sta leftOffset
	lda #1
	sta rightOffset
	lda #15
	sta downLeftOffset
	lda #16
	sta downOffset
	lda #17
	sta downRightOffset
	ldx #0
updLp:
	lda #0
	sta ncount
	ldy upLeftOffset
	lda board,y
	bpl .skip1
	inc ncount
.skip1
	ldy upOffset
	lda board,y
	bpl .skip2
	inc ncount
.skip2	
	ldy upRightOffset
	lda board,y
	bpl .skip3
	inc ncount
.skip3	
	ldy leftOffset
	lda board,y
	bpl .skip4
	inc ncount
.skip4	
	ldy rightOffset
	lda board,y
	bpl .skip5
	inc ncount
.skip5
	ldy downLeftOffset
	lda board,y
	bpl .skip6
	inc ncount
.skip6
	ldy downOffset
	lda board,y
	bpl .skip7
	inc ncount
.skip7
	ldy downRightOffset
	lda board,y
	bpl .skip8
	inc ncount
.skip8
	;OR the neighbor count onto the lower 4 bits
	lda board,x
	and #$F0 ; clear count
	ora ncount
	sta board,x
	
	;update offsets to neighbors
	inc upLeftOffset
	inc upOffset
	inc upRightOffset
	
	inc leftOffset
	inc rightOffset
	
	inc downLeftOffset
	inc downOffset
	inc downRightOffset
	
	;update loop counter
	inx
	beq .x
	jmp updLp
.x	
	rts

;removes or adds ccells to the board
AddAndRemoveOrganisms
	ldx #0
.updLpA
	;is the cell occupied
	lda board,x
	bmi .CheckLives
	;cell is empty
	lda board,x ; does it spawn a new life form
	and #$0F ; clear life bit
	cmp #3
	bne .x2
	lda board,x
	ora #OCCUPIED
	sta board,x ; put an organism there
	jmp .x2
.CheckLives ; sad or overcrowded
	lda board,x ; does it spawn a new life form
	and #$0F ; clear life bit 
	cmp #0  ; lt
	beq .killIt
	cmp #1
	beq .killIt ; lt
	cmp #2
	beq .x2
	cmp #3
	beq .x2
;	cmp #4  
;	bcs killIt ; gte
;	jmp x2
.killIt
	lda #0
	sta board,x
	and #$7F  ; 01111111 ; clear organism
	sta board,x
.x2
	inx
	bne .updLpA
	rts

;hides all sprites except sprite 0 (the cursor)
InitSprites
.vblankwait2      ; Second wait for vblank, PPU is ready after this
	BIT $2002
	BPL .vblankwait2

	
	LDA $2002  ; read PPU status to reset the high/low latch

	lda #0  ; spr0 y
	sta OAM_ADDR ;write the OAM address
	lda #124  
	sta OAM_DATA ;write the data to the OAM	
	sta OAM_DATA ;write the data to the OAM	
	sta OAM_DATA ;write the data to the OAM	
	sta OAM_DATA ;write the data to the OAM	

	LDA $2002  ; read PPU status to reset the high/low latch
	lda #1  ; spr0's  tile
	sta OAM_ADDR ;write the OAM address
	lda #CURSOR_SPR
	sta OAM_DATA ;write the data to the OAM	
	
	LDA $2002  ; read PPU status to reset the high/low latch
	lda #3  ;spr0 x
	sta OAM_ADDR ;write the OAM address	 
	lda #124
	sta OAM_DATA  ;write the data to the OAM
	
	;now hide all the other sprites
	lda #3 ;y coord of 2nd sprite
	ldy #248
	ldx #0
.lp
	; doesn't help LDX $2002  ; read PPU status to reset the high/low latch
	;write the OAM address
	sta OAM_ADDR
	;write the data to the OAM
	sta OAM_DATA 
	;add 4 to addr
	clc
	adc #4
	bcc .lp ; if we didn't roll-over, keep looping
	rts

MoveRight
;	lda cursorX
;	and $F0
;	sta  ; write sprite x to OEM memory
		
	rts

palette:
  .db $0f,$20,$1A,$0F,  $0f,$20,$17,$0F,  $0f,$20,$21,$0F,  $0f,$20,$17,$0F   ;;background palette
  .db $22,$1C,$15,$14,  $22,$02,$38,$3C,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ;;sprite palette

NMI:
	pha
	txa
	pha
	tya
	pha

   lda drawing
   beq .x
;  lda counter
 ; cmp #30
 ; beq nmix
 jsr WriteBoardToPPU
;nmix:  
.x
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA $2005
  STA $2005
  
  pla 
  tay
  pla 
  tax
  pla
  rti

;;;;;;;;;;;;;;
;; How the NMI's worked
;;Count Neighbors
;;AddRemove
;; ??? frames to copy board to ram
;; copy RAM to PPPU
;; (repeat) 
;;;;;;;;;;;;;;  
  
  
  
  .bank 1
  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
  
;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "life.chr"   ;includes 8KB graphics file from SMB1