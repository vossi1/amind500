; "A Mind Was Born" 
; disassembled by DASM6502b v.3.1 by Marat Fayzullin
; modified by Vossi 02/2019
; comments by Vossi 05/2020
!cpu 6510
!to "amind500z.prg", cbm
; ***************************************** CONSTANTS *********************************************
SYSTEMBANK				= 15		; systembank
CODEBANK				= 0			; codebank 0-9 for basic loader
RASTERLINE				= $32
; ************************************** P500 REGISTER ********************************************
VR_MODEY				= $11
VR_RASTER				= $12
VR_MEMPT				= $18
VR_IRQ					= $19
VR_EIRQ					= $1a
VR_BGRCOL				= $21
; ***************************************** ADDRESSES *********************************************
!addr CodeBank			= $00		; code bank register
!addr IndirectBank		= $01		; indirect bank register
!addr HW_IRQ			= $fffe		; Hardware interrupt vector
!addr Screen			= $0c00		; screen memory
!addr VICbase			= $d800		; VIC
!addr SIDbase			= $da00		; SID
!addr CIAbase			= $dc00		; CIA
!addr TPI1base			= $de00		; TPI1
!addr TPI2base			= $df00		; TPI2
!addr temp				= $0200
; ***************************************** ZERO PAGE *********************************************
!zone zeropage
!addr clock				= $4a
!addr mel_lfsr			= $4b
!addr clock_hi			= $57
; *************************************** BASIC LOADER ********************************************
!zone basic
*= $0003
		!byte $29, $00
		!byte $0a, $00, $81, $49, $b2, $34, $33, $a4, $35, $30, $3a		; fori=xxtoyy:
		!byte $dc, $30+CODEBANK, $3a, $41, $b2, $c2, $28, $49, $29, $3a	; bank1:a=peek(i):
		!byte $dc, $31, $35, $3a, $97, $49, $2c, $41, $3a				; bank15:pokei,a:
temp2:	!byte $82, $3a, $9e, $34, $33, $00, $00, $00					; next:sysxx
	; 10 fori=43to50:bank1:a=peek(i):bank15:pokei,a:next:sys43
; *************************************** ZONE BANKINIT *******************************************
!zone bankinit
; $2b basic loader starts here in bank 15 with SYS 43
		sei
		lda #CODEBANK
		sta CodeBank
; from here in codebank
		jmp init
; ***************************************** ZONE DATA *********************************************
!zone data
; $33 hardware pointers
VIC:	!word VICbase
sid_ptr:!word SIDbase-1
CIA:	!word CIAbase
TPI1:	!word TPI1base
TPI2:	!word TPI2base
vic_ptr:!word VICbase+$1c
vm_ptr:	!word Screen
; $41 SID register mirror $d400-$d418
sid_mir:!byte $00, $00, $00, $19, $41,$1c, $d0	; osc 1
;		!byte $00, $dc, $00, $00, $11 overlapped VIC bgr-color mirror reg $20-$24 / sid_mir+3 + 4
		!byte $00, $dc, $00, $00, $11, $d0, $e0 ; osc 2
		!byte $0b, $10, $33, $0e, $61, $90, $f5 ; osc 3
		!byte $07, $00, $ff, $1f 				; SID filter regs
; $4a script: 1.byte = ZP-address, 2.byte = value
script:	!byte <(freqtbl+$08), $1f
		!byte <(sid_mir+$0a), $41
		!byte <(mod_op1), 	 $24
		!byte <(sid_mir+$0b), $25
		!byte <(sid_mir+$0b), $53
		!byte <(sid_mir+$0b), $61
		!byte <(mod_op1),	 $29
		!byte <(sid_mir+$11), $0f
; $5a frequency tables
basstbl:
		!byte $2B, $AA, $02, $62
freqtbl:
		!byte $00, $18, $26, $20, $12, $24, $13, $10, $ff ; last byte reserved for poke
; ***************************************** ZONE IRQ **********************************************
!zone code
; $77 interrupt routine
irq:	inc clock						; increase counter lo +2
		inc clock
		bne noc1
		inc clock_hi					; increase counter hi
; $7f
noc1:	lda #$61
		sta <(sid_mir)+2*7+4			; SID osc3 control reg
		lax clock_hi					; ILLEGAL opcode A, X = address
		cpx #$3f
		beq highpass
		bcc noend
		brk
; $8c
highpass:
		ldy #$6d
		sty <(sid_mir)+$18
		sty <(mod_op2)
; $4a
noend:	
		lsr
		asr #$1c						; ILLEGAL opcode (A & imm) /2
		tay
		lda clock
		and #$30
		bne noduck
		dec <(sid_mir)+2*7+4			; SID osc2 control reg
; $9e
noduck:	
		cpx #$2f
		beq bassoff
		bcs nointro
		ldx #$02
; $a6
nointro:
		cmp #$10
		beq bassoff
		txa
		and #$03
		tax
		lda <(basstbl),x
		sta <(sid_mir)
		and #$03						; value on orginal address $ab		
		jmp skip
;		!byte $2d						; and absolute instruction
; $b3
bassoff:
		lxa #$00						; ILLEGAL opcode clears A, X
skip:	bcs bassdone
		lax <(script+1),y				; ILLEGAL opcode A, X = address
		ldx <(script),y
		sta $00,x						; store script value to script address
		lda clock						; load random clcok value
		asr #$0e						; ILLEGAL opcode A = (A & imm) /2
		tax								; ^ random 0-$e rightshiftet -> 0-7
		sbx #$f8						; ILLEGAL opcode X = (A & X) - imm
		stx vm_ptr+1					; ^ random 8-$f
		eor #$07
; $c8
bassdone:
		sta <(sid_mir)+0*7+1			; SID osc1 frequency hi
		lda clock
		and #$0f
		bne nomel
		lda #$b8
		sre mel_lfsr					; ILLEGAL opcode mem = (mem / 2) : A = A eor mem
		bcc noc2
		sta mel_lfsr
; $d8
noc2:	and #$07
		tax
		lda <(freqtbl),x
		sta <(sid_mir)+1*7+1			; SID osc2 frequency hi
; $df
nomel:
		ldy #$08
; $e1
vicloop:
		lax <(sid_mir)+3,y				; ILLEGAL opcode A, X = address
		sta (vic_ptr),y
		dey
		bpl vicloop
		tay
; $
loop:	
		lax <(sid_mir)-1,y				; ILLEGAL opcode A, X = address
		sta (sid_ptr),y
		dey
		bne loop
		rts
; ***************************************** ZONE MAIN *********************************************
; $f1 main routine
main:	ldy #$06
		lda (CIA),y 					; grab CIA timer lo as random value
; $f5
mod_op1:ldy #$c3
; $f7
mod_op2:ora temp
		sta temp
;		pha
		asr #$04						; ILLEGAL opcode (A & imm) /2
		ldy #$30						; video matrix at $0c00, font at $0000
		lda (vm_ptr),y
;		sta temp+1
		inc vm_ptr
;		lda (vm_ptr),y
;		sta temp+2
;		lda temp
;		adc temp+1
;		adc temp+2
;		adc (vm_ptr),y
;		inc vm_ptr
;		adc (vm_ptr),y
		ror
		ora clock_hi
		ldy #$58
		eor <(mod_op1)
;		ora <(mod_op1)
		sta (vm_ptr),y
		ldy #$1c+1
		lda (sid_ptr),y
		sta temp
		jmp main
; **************************************** IRQ HANDLER ********************************************
!zone irqhandler
IRQ_Handler:
		pha
		txa
		pha
		tya
		pha
		ldy #VR_IRQ
		lda (VIC),y						; load VIC interrupt reg and mask bit 1
		and #$01
		beq endirq						; skip if source is not raster interrupt

		jsr irq

		lda #RASTERLINE					; set rasterline again
		ldy #VR_RASTER
		sta (VIC),y
		lda #$81
		ldy #VR_IRQ
		sta (VIC),y						; clear VIC raster interrupt
endirq:	pla
		tay
		pla
		tax
		pla
		rti
; ***************************************** ZONE INIT *********************************************
!zone init
		; $b2 initialization routine
init:	ldx #$ff						; init new stack in codebank
		txs
		lda #>IRQ_Handler
		sta HW_IRQ+1
		lda #<IRQ_Handler
		sta HW_IRQ
		lda #SYSTEMBANK
		sta IndirectBank				; set bank indirect reg to bank 15
		ldy #$06
		lda (TPI1),y					; load TRI1 control register
		and #$0f						; clear CA, CB control bits#4-7 vic bank 0/15 select 
		ora #$a0						; set bit#5,4=10 CA=low -> Video matrix in bank 0
		sta (TPI1),y					; set bit#7,6=10 CB=high -> Characterset in bank 0 
		ldy #$02
		lda (TPI2),y					; load TPI2 port c
		and #$3f						; clear bit#6,7 vic 16k select bank $0000-$3fff
		sta (TPI2),y					; store to TPI2 port c
		lda #$30
		ldy #VR_MEMPT					; VIC reg $18 memory pointers
		sta (VIC),y						; set VM13-10=$3 screen at $0c00, CB13-11=0 char at $0000
;		lda #$7f						; bit#7=0 clears/mask out all 5 irq sources with bit#0-4 = 1
;		ldy #$0d						; CIA interrupt control register
;		sta (CIA),y						; disable all hardware interrupts
		lda #$00
		ldy #$05
		sta (TPI1),y					; set TPI1 reg $5 interrupt mask reg = $00 - disable all irq
		ldy #VR_BGRCOL
		sta (VIC),y						; Background color black
;		jsr KERNAL_CLR					; Kernal clear screen routine

; P500 Hardware interrupt vector setup, enable VIC raster IRQ
		lda #$01
		ldy #VR_EIRQ
		sta (VIC),y						; VIC enable raster interrupt
		lda #$50
		ldy #VR_MODEY
		sta (VIC),y						; VIC RC8 = 0, ECM=1, DEN=1, 24 rows, Y=0
		lda #RASTERLINE
		ldy #VR_RASTER
		sta (VIC),y						; VIC set raster reg
		cli
		jmp main						; start main code
