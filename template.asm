
board equ $200 ; 256 squares
neighborCounts equ $300
upLeftOffset equ $400
upOffset equ $401
upRightOffset equ $402
leftOffset equ $403
rightOffset equ $404
downLeftOffset equ $405
downOffset equ $406
downRightOffset equ $407
counter equ $408
tileRam equ $409

 
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

vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1

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
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2


;;  LDA #%10000000   ;intensify blues
;;  STA $2001
  
	jsr LoadPalettes
	jsr InitBoard

	;test code to see if slicing up the drawing works
	ldx #0
	stx counter
lp1: 
	jsr DrawBoardRowToRAM
	inc counter
	lda counter
	cmp #15
	bne lp1	 

	lda #0
	sta counter

	lda #HIGH(tileRam)
	sta pointerHi
	lda #LOW(tileRam)
	sta pointerLo

	lda #$20
	sta PPUHi
	lda #$0
	sta PPULo
;	jsr WriteBoardToPPU
 
	lda #0
	sta counter2
lp1a: 
	jsr WriteBoardToPPU ; writes 32 tile
	inc counter2
	lda counter2
	cmp #30
	bne lp1a	

 
 
	LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
	STA $2000

;	LDA #%00011110   ; enable sprites, enable background, no clipping on left side
	LDA #%00001110   ; disable sprites, enable background, no clipping on left side
	STA $2001
  
Forever:
	;lda counter
	;disable PPU
	;re-enable PPU
 ; jsr CountNeighbors
  ;jsr AddAndRemoveOrganisms	

  JMP Forever     ;jump back to Forever, infinite loop

InitBoard
	ldy #0
	lda #0
	sta Index
lpx:	
	sta board,y
	iny 
	bne lpx

	lda #1
	sta board
	ldy #16
	sta board,y
	ldy #239
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
  
  


;Draw board (draws 32 tiles)
;Upates the name to reflect what is in the board
WriteBoardToPPU 
 	LDA $2002  ; read PPU status to reset the high/low latch
;	LDA #$20
	lda PPUHi
	STA $2006             ; write the high byte of $2000 address
;	lda #$0
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
	cmp #30
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

;	dex
;	bne outerLp
 	rts
reset1:
	lda #HIGH(tileRam)
	sta pointerHi
	lda #LOW(tileRam)
	sta pointerLo
	lda #0
	sta counter	           
	lda #$20
	sta PPUHi
	lda #$00
	sta PPULo
	rts
 
;copies 1 row fron 
DrawBoardRowToRAM
  	ldx #0 ;draw 16 tops
	ldy #0
	sty tempY1
	sty tempY2
drawLp1:
	ldy tempY1
	lda [boardPtrLo],y
	beq empty1
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
	beq empty2
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
	;inc line counter
;;	inc counter
;;	lda counter
 ;;   cmp #15
 ;;   bne xt	
xt:	rts 

 

;update board
CountNeighbors
	lda #223
	sta upLeftOffset
	lda #224
	sta upOffset
	lda #225
	sta upRightOffset 
	lda #15
	sta leftOffset
	lda #1
	sta rightOffset
	lda #31
	sta downLeftOffset
	lda #16
	sta downOffset
	lda #17
	sta downRightOffset
	ldx #0
updLp:
	clc
	lda #0
	
	ldy upLeftOffset
	adc board,y
	ldy upOffset
	adc board,y
	ldy upRightOffset
	adc board,y

	ldy leftOffset
	adc board,y
	ldy rightOffset
	adc board,y

	ldy downLeftOffset
	adc board,y
	ldy downOffset
	adc board,y
	ldy downRightOffset
	adc board,y
	
	sta neighborCounts,x

	
	inc upLeftOffset
	inc upOffset
	inc upRightOffset
	inc leftOffset
	inc downLeftOffset
	inc downOffset
	inc downRightOffset
	
	inx
	bne updLp
	
	rts

;
AddAndRemoveOrganisms
	ldx #0
updLpA:
	;is the cell occupied
	lda board,x
	bne CheckLives
	lda neighborCounts,x ; does it spawn a new life form
	cmp #3
	bne x2
	lda #1
	sta board,x ; put an organism there
	jmp x2
CheckLives:
	cmp #3  ; lt
	bcc killIt
	cmp #4  
	bcs killIt ; gte
	jmp x2
killIt:
	lda #0
	sta board,x
x2:
	inx
	bne updLpA
	rts

palette:
  .db $0f,$20,$1A,$0F,  $0f,$20,$17,$0F,  $0f,$20,$21,$0F,  $0f,$20,$17,$0F   ;;background palette
  .db $22,$1C,$15,$14,  $22,$02,$38,$3C,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ;;sprite palette

NMI:
  ;jsr WriteBoardToPPU
  rti
 
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