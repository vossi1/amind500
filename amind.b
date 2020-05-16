; "A Mind Was Born" 
; disassembled by DASM6502b v.3.1 by Marat Fayzullin
; modified by Vossi 02/2019
; comments by Vossi 05/2020
!cpu 6510
!to "amind.prg", cbm
; ***************************************** CONSTANTS *********************************************
ZD						= $0800 	; difference code address to later zeropage address 
; ***************************************** ADDRESSES *********************************************
!addr Color				= $0286		; C64 actual color
!addr IRC_VECTOR		= $0314		; C64 IRQ vector
!addr KERNAL_CLR		= $e544		; C64 clear screen - fills also color RAM
!addr KERNAL_IRQ		= $ea7e		; C64 end of system interrupt routine
!addr HW_RESET			= $fffc		; CPU Hardware reset vector
!addr VIC				= $d000		; VIC base address
!addr SID				= $d400		; SID base address
!addr CIA				= $dc00		; CIA base address
; *************************************** BASIC LOADER ********************************************
!zone basic
*= $0801
		!byte $0d, $08
start:
sid_ptr:!byte $ff, $d3
		!byte $9E, $32, $32, $32, $35;, $00, $00, $00	Last 3 bytes = overlapped sid reg mirror
		; 54271 SYS 2225
; ***************************************** ZERO PAGE *********************************************
!zone zeropage
!addr clock				= $13
!addr mel_lfsr			= $14
!addr clock_hi			= $20
!addr vm_ptr			= $cb		; pointer to video matrix / screen memory
; ***************************************** ZONE CODE *********************************************
!zone code
*= $080a
; $0a SID register mirror $d400-$d418
sid_mir:!byte $00, $00, $00, $19, $41;,$1c, $d0	; osc 1
vic_ptr:!byte $1c, $d0							; last 2 osc1 bytes used as vic-pointer reg $1c
;		!byte $00, $dc, $00, $00, $11			  overlapped VIC bgr-color mirror reg $20-$24
		!byte $00, $dc, $00, $00, $11, $d0, $e0 ; osc 2
		!byte $0b, $10, $33, $0e, $61, $90, $f5 ; osc 3
		!byte $07, $00; $ff, $1f 				; SID filter regs - last 2 overlapped with script
; $21 script: 1.byte = ZP-address, 2.byte = value
script:	!byte <(freqtbl-ZD+$08), $1f
		!byte <(sid_mir-ZD+$0a), $41
		!byte <(mod_op1-ZD), 	 $24
		!byte <(sid_mir-ZD+$0b), $25
		!byte <(sid_mir-ZD+$0b), $53
		!byte <(sid_mir-ZD+$0b), $61
		!byte <(mod_op1-ZD),	 $29
		!byte <(sid_mir-ZD+$11), $0f
; $31 interrupt routine
irq:	inc clock						; increase counter lo +2
		inc clock
		bne noc1
		inc clock_hi					; increase counter hi
; $38
noc1:	lda #$61
		sta <(sid_mir-ZD)+2*7+4			; SID osc3 control reg
		lax clock_hi					; ILLEGAL opcode A, X = address
		cpx #$3f
		beq highpass
		bcc noend
		lsr VIC+$11
		jmp (HW_RESET)					; jump to reset at end of demo
; $4b
highpass:
		ldy #$6d
		sty <(sid_mir-ZD)+$18
		sty <(mod_op2-ZD)
; $51
noend:	
		lsr
		asr #$1c						; ILLEGAL opcode (A & imm) /2
		tay
		lda clock
		and #$30
		bne noduck
		dec <(sid_mir-ZD)+2*7+4			; SID osc2 control reg
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
		lda <(basstbl-ZD),x
		sta <(sid_mir-ZD)
		!byte $2d						; and absolute instruction
; $72
bassoff:
		lxa #$00						; ILLEGAL opcode clears A, X
		bcs bassdone
		lax <(script+1-ZD),y			; ILLEGAL opcode A, X = address
		ldx <(script-ZD),y
		sta $00,x						; store script value to script address
		lda clock						; load random clcok value
		asr #$0e						; ILLEGAL opcode A = (A & imm) /2
		tax								; ^ random 0-$e rightshiftet -> 0-7
		sbx #$f8						; ILLEGAL opcode X = (A & X) - imm
		stx vm_ptr+1					; ^ random 8-$f
		eor #$07
; $87
bassdone:
		sta <(sid_mir-ZD)+0*7+1		; SID osc1 frequency hi
		lda clock
		and #$0f
		bne nomel
		lda #$b8
		sre mel_lfsr					; ILLEGAL opcode mem = (mem / 2) : A = A eor mem
		bcc noc2
		sta mel_lfsr
; $97
noc2:	and #$07
		tax
		lda <(freqtbl-ZD),x
		sta <(sid_mir-ZD)+1*7+1			; SID osc2 frequency hi
; $9e
nomel:
		ldy #$08
; $a0
vicloop:
		lax <(sid_mir-ZD)+3,y			; ILLEGAL opcode A, X = address
		sta (vic_ptr-ZD),y
		dey
		bpl vicloop
		tay
; $a8
loop:	
		lax <(sid_mir-ZD)-1,y			; ILLEGAL opcode A, X = address
		sta (sid_ptr-ZD),y
		dey
		bne loop
		jmp KERNAL_IRQ
; $b2 initialization routine
init:	sei								; disable interrupts
		stx Color						; Background color = X / $00 at startup
		stx VIC+$21						; two writes for different c64 kernals
		jsr KERNAL_CLR					; Kernal clear screen routine
		ldx #end-start+1				; copies code to ZP from $03-$fe
initlp:	lda start-1,x
		sta $02,x
		dex
		bne initlp
		stx IRC_VECTOR+1				; set irq vector hi = $00, lo already $031 -> IRQ = $0031
		jmp main-ZD						; start main code
; main routine
main:	lda #$50						; VIC ECM, 24 lines
		sta VIC+$11						
		cli								; enable interrupts
mainlp:	lda CIA+$04 						; grab CIA timer lo as random value
mod_op1:ldy #$c3
mod_op2:ora SID+$1c
		pha
		asr #$04						; ILLEGAL opcode (A & imm) /2
		ldy #$30						; video matrix at $0c00, font at $0000
		sty VIC+$18						; set VIC reg memory pointers
		adc (vm_ptr),y
		inc vm_ptr
		adc (vm_ptr),y
		ror
		ora clock_hi
		ldy #$58
		ora <(mod_op1-ZD)
		sta (vm_ptr),y
		bne mainlp						; branch always
; frequency table
basstbl:
		!byte $2B, $AA, $02, $62
freqtbl:
		!byte $00, $18, $26, $20, $12, $24, $13, $10
end: