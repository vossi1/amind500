; "A Mind Was Born" 
; disassembled by DASM6502b v.3.1 by Marat Fayzullin
; modified by Vossi 02/2019
; ported to P500, comments by Vossi 05/2020
!cpu 6510
!to "amind500.prg", cbm
; ***************************************** CONSTANTS *********************************************
ZPDIFF					= -$0800
; ***************************************** ADDRESSES *********************************************
!addr SID				= $0a		; SID mirror starts at $0a
!addr KERNAL_IRQ		= $ea7e
!addr HW_RESET			= $fffc
; *************************************** BASIC LOADER ********************************************
!zone basic
*= $0801
		!byte $0d, $08
pt_d3ff:!byte $ff, $d3
		!byte $9E, $32, $32, $32, $35;, $00, $00, $00
		; 54271 SYS 2225
;*= $0003
;		!byte $37, $00, $0A, $00, $97, $38, $33, $2C, $31, $32, $30, $3A, $97, $38, $34, $2C
;		!byte $31, $36, $39, $3A, $97, $38, $35, $2C, $30, $3A, $97, $38, $36, $2C, $31, $33
;		!byte $33, $3A, $97, $38, $37, $2C, $30, $3A, $97, $38, $38, $2C, $32, $33, $34, $3A
;		!byte $9E, $38, $33, $00, $00, $00, $9E, $31, $37, $36, $00, $00, $00
;		; 10 poke83,120:poke84,169:poke85,0:poke86,133:poke87,0:poke88,234:sys83
; ***************************************** ZERO PAGE *********************************************
!zone zeropage
clock					= $13
mel_lfsr				= $14
clock_msb				= $20
vmptr					= $cb
mod_opl					= $d5
mod_op2					= $d7
; ***************************************** ZONE CODE *********************************************
!zone code
*= $080a
; $0a SID register morror $d400-$d418
		!byte $00, $00, $00, $19, $41			; osc 1
pt_d01c:!byte $1c, $d0
;		!byte $00, $dc, $00, $00, $11			  overlapped VIC BGR color mirror $d020-$d024
		!byte $00, $dc, $00, $00, $11, $d0, $e0 ; osc 2
		!byte $0b, $10, $33, $0e, $61, $90, $f5 ; osc 3
		!byte $07, $00; $ff, $1f 				; SID common regs - last two overlapped with script
; script: 1.byte = ZP-address, 2.byte = value
script:	!byte $ff, $1f
		!byte $14, $41
		!byte $d5, $24
		!byte $15, $25
		!byte $15, $53
		!byte $15, $61
		!byte $d5, $29
		!byte $1b, $0f
; interrupt routine
irq:	inc clock							; increase counter lowyte 2 bytes
		inc clock
		bne noc1
		inc clock_msb						; increase counter highbyte
noc1	lda #$61
		sta SID+2*7+4						; SID osc2 control reg
		lax clock_msb						; ILLEGAL opcode
		cpx #$3f
		beq highpass
		bcc noend
		lsr $d011
		jmp (HW_RESET)
highpass:
		ldy #$6d
l084d:	sty SID+$18
		sty mod_op2
noend:	
		lsr
		asr #$1c							; ILLEGAL opcode
		tay
		lda clock
		and #$30
		bne noduck
		dec SID+2*7+4						; SID osc2 control reg
noduck:	
		cpx #$2f
		beq bassoff
		bcs nointro
		ldx #$02
nointro:
		cmp #$10
		beq bassoff
		txa
		and #$03
		tax
		lda <(basstbl+ZPDIFF),x
		sta SID
		!byte $2d							; and absolute
bassoff:
		lxa #$00							; ILLEGAL opcode
		bcs bassdone
		lax <(script+1+ZPDIFF),y			; ILLEGAL opcode
		ldx <(script+ZPDIFF),y
		sta $00,x
		lda clock
		asr #$0e							; ILLEGAL opcode
		tax
		sbx #256-8							; ILLEGAL opcode
		stx vmptr+1
		eor #$07
bassdone:
		sta SID+0*7+1						; SID osc1 frequency hi
		lda clock
		and #$0f
		bne nomel
		lda #$b8
		sre mel_lfsr						; ILLEGAL opcode
		bcc noc2
		sta mel_lfsr
noc2:	and #$07
		tax
		lda <(freqtbl+ZPDIFF),x
		sta SID+1*7+1						; SID osc2 frequency hi
nomel:
		ldy #$08
vicloop:
		lax SID+3,y							; ILLEGAL opcode
		sta (pt_d01c+ZPDIFF),y
		dey
		bpl vicloop
		tay
loop:	
		lax SID-1,y							; ILLEGAL opcode
		sta (pt_d3ff+ZPDIFF),y
		dey
		bne loop
		jmp KERNAL_IRQ
; code starts here at $08b4
init:	sei								; disable interrupts
		stx $0286						; Background color = X / $00 at startup
		stx $d021						; two writes for different c64 kernals
		jsr $e544						; clear screen
		ldx #$fd						; code copied to ZP from $03-$ff
initlp:	lda $0802,x
		sta $02,x
		dex
		bne initlp
		stx $0315						; set irq vector hi = $00, lo already $031 -> IRQ = $0031
		jmp start+ZPDIFF				; start code
; main routine
start:	lda #$50						; VIC ECM, 24 lines
		sta $d011						
		cli								; enable interrupts
mainlp:	lda $dc04 						; grab CIA timer lo as random value
		ldy #$c3
		ora $d41c
		pha
		asr #$04						; ILLEGAL opcode
		ldy #$30						; video matrix at $0c00, font at $0000
		sty $d018						; set VIC reg memory pointers
		adc (vmptr),y
		inc vmptr
		adc (vmptr),y
		ror
		ora clock_msb
		ldy #$58
		ora mod_opl
		sta (vmptr),y
		bne mainlp						; branch always
; frequency table
basstbl:
		!byte $2B, $AA, $02, $62
freqtbl:
		!byte $00, $18, $26, $20, $12, $24, $13, $10 