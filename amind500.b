; "A Mind Was Born" 
; disassembled by DASM6502b v.3.1 by Marat Fayzullin
; modified by Vossi 02/2019
; ported to P500, comments by Vossi 05/2020
!cpu 6510
!to "amind500.prg", cbm
; ***************************************** CONSTANTS *********************************************
SYSTEMBANK				= $0f		; systembank

ZPDIFF					= -$0100
; ***************************************** ADDRESSES *********************************************
!addr CodeBank			= $00		; code bank register
!addr IndirectBank		= $01		; indirect bank register
!addr KERNAL_IRQ		= $fbf8
!addr HW_RESET			= $fffc
; *************************************** BASIC LOADER ********************************************
!zone basic
*= $0003
		!byte $37, $00, $0A, $00, $97, $38, $33, $2C, $31, $32, $30, $3A, $97, $38, $34, $2C
		!byte $31, $36, $39, $3A, $97, $38, $35, $2C, $30, $3A, $97, $38, $36, $2C, $31, $33
		!byte $33, $3A, $97, $38, $37, $2C, $30, $3A, $97, $38, $38, $2C, $32, $33, $34, $3A
		!byte $9E, $38, $33, $00, $00, $00, $9E, $31, $37, $36, $00, $00, $00
		; 10 poke83,120:poke84,169:poke85,0:poke86,133:poke87,0:poke88,234:sys83
; ***************************************** ZERO PAGE *********************************************
!zone zeropage
*= $0040
		!word $0002						; +1 = $0003 code start in bank 15

zp_ptr					= $40			; pointer to $0002

clock					= $13
mel_lfsr				= $14
clock_msb				= $20
vmptr					= $cb
; ***************************************** ZONE MAIN *********************************************
!zone main
; bank 15 - 6 bytes poked from BASIC
; $0053									; sys83 goes here in bank 15
;		sei								; disable interrrupts
;		lda #$00
;		sta $00							; switch to code bank 0
; $0058	nop								; from here the CPU continues in bank 0
*= $0058
		sei								; disable interrrupts
		nop
		ldx #SYSTEMBANK
		stx IndirectBank				; indirect bank = 15

		ldy #$ff						; copies code to bank 15 ZP from $03-$ff
codecpy:lda $0102,y
		sta (zp_ptr),y
		dey
		bne codecpy

		jmp switch
; switch routine		
*= $00b4
switch: 								; X already = SYSTEMBANK
		stx CodeBank					; switch to copied code in bank 15
		nop
		nop
		nop

; ***************************************** ZONE CODE *********************************************
!zone code
*= $0103
pt_d9ff:!byte $ff, $d9							; SID base = $da00
pt_d81c:!byte $1c, $d8							; VIC reg $1c
		!byte $32, $32, $35;, $00, $00, $00
;
; $0a SID register mirror $d400-$d418
sid_mir:!byte $00, $00, $00, $19, $41, $1c, $d0	; osc 1
;		!byte $00, $dc, $00, $00, $11			  overlapped VIC BGR color mirror $d020-$d024
		!byte $00, $dc, $00, $00, $11, $d0, $e0 ; osc 2
		!byte $0b, $10, $33, $0e, $61, $90, $f5 ; osc 3
		!byte $07, $00; $ff, $1f 				; SID common regs - last two overlapped with script
; script: 1.byte = ZP-address, 2.byte = value
script:	!byte $ff, $1f
		!byte <(sid_mir+ZPDIFF)+$0a, $41
		!byte <(mod_op1+ZPDIFF), $24
		!byte <(sid_mir+ZPDIFF)+$0b, $25
		!byte <(sid_mir+ZPDIFF)+$0b, $53
		!byte <(sid_mir+ZPDIFF)+$0b, $61
		!byte <(mod_op1+ZPDIFF), $29
		!byte <(sid_mir+ZPDIFF)+$11, $0f
; $31 interrupt routine
irq:	inc clock							; increase counter lowyte 2 bytes
		inc clock
		bne noc1
		inc clock_msb						; increase counter highbyte
noc1	lda #$61
		sta <(sid_mir+ZPDIFF)+2*7+4					; SID osc3 control reg
		lax clock_msb						; ILLEGAL opcode A, X = address
		cpx #$3f
		beq highpass
		bcc noend
		lsr $d811
		jmp (HW_RESET)
; $4b
highpass:
		ldy #$6d
		sty <(sid_mir+ZPDIFF)+$18
		sty <(mod_op2+ZPDIFF)
; $51
noend:	
		lsr
		asr #$1c							; ILLEGAL opcode (A & imm) /2
		tay
		lda clock
		and #$30
		bne noduck
		dec <(sid_mir+ZPDIFF)+2*7+4					; SID osc2 control reg
; $5d
noduck:	
		cpx #$2f
		beq bassoff
		bcs nointro
		ldx #$02
; $65
nointro:
		cmp #$10
		beq bassoff
		txa
		and #$03
		tax
		lda <(basstbl+ZPDIFF),x
		sta <(sid_mir+ZPDIFF)
		!byte $2d							; and absolute
; $72
bassoff:
		lxa #$00							; ILLEGAL opcode clears A, X
		bcs bassdone
		lax <(script+1+ZPDIFF),y			; ILLEGAL opcode A, X = address
		ldx <(script+ZPDIFF),y
		sta $00,x
		lda clock
		asr #$0e							; ILLEGAL opcode A = (A & imm) /2
		tax
		sbx #$34		; **********		; ILLEGAL opcode X = (A & X) - imm
		stx vmptr+1
		eor #$07
; $87
bassdone:
		sta <(sid_mir+ZPDIFF)+0*7+1					; SID osc1 frequency hi
		lda clock
		and #$0f
		bne nomel
		lda #$b8
		sre mel_lfsr						; ILLEGAL opcode mem = (mem / 2) : A = A eor mem
		bcc noc2
		sta mel_lfsr
; $97
noc2:	and #$07
		tax
		lda <(freqtbl+ZPDIFF),x
		sta <(sid_mir+ZPDIFF)+1*7+1					; SID osc2 frequency hi
; $9e
nomel:
		ldy #$08
; $a0
vicloop:
		lax <(sid_mir+ZPDIFF)+3,y						; ILLEGAL opcode A, X = address
		sta (pt_d81c+ZPDIFF),y
		dey
		bpl vicloop
		tay
		bit $00	; ****** DUMMY for $a9 stop key flag!
; $aa
loop:	
		lax <(sid_mir+ZPDIFF)-1,y							; ILLEGAL opcode A, X = address
		sta (pt_d9ff+ZPDIFF),y
		dey
		bne loop
		jmp KERNAL_IRQ
; $b4 init
init:	nop
		nop
		nop
		lda #$40
		sta $d818
		ldx #$31						; set irq vector lo
		stx $0300
		ldx #$00						; Background color = X
		stx $d821
		stx $0301						; set irq vector hi = $00
		jmp start+ZPDIFF				; start code
; $cc main routine
start:	lda #$50						; VIC ECM, 24 lines
		sta $d811						
		cli								; enable interrupts
; $d2
mainlp:	lda $dc06 						; grab CIA timer2 lo as random value
mod_op1:ldy #$c3
mod_op2:ora $da1c						; SID
		pha
		asr #$04						; ILLEGAL opcode (A & imm) /2
		ldy #$30						; video matrix at $d000, font at $c000
		sty $d7ff	; ***** DUMMY		; set VIC reg memory pointers
		adc (vmptr),y
		inc vmptr
		adc (vmptr),y
		ror
		ora clock_msb
		ldy #$58
		ora <(mod_op1+ZPDIFF)
		sta (vmptr),y
		bne mainlp						; branch always
; frequency table
; $f3
basstbl:
		!byte $2B, $AA, $02, $62
; $f7
freqtbl:
		!byte $00, $18, $26, $20, $12, $24, $13, $10 