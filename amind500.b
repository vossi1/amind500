; "A Mind Was Born" 
; disassembled by DASM6502b v.3.1 by Marat Fayzullin
; modified by Vossi 02/2019
; ported to P500, comments by Vossi 05/2020
!cpu 6510
!to "amind500.prg", cbm
; ***************************************** CONSTANTS *********************************************
SYSTEMBANK				= $0f		; systembank
ZPDIFF					= -code+3
FILTER					= 1 		; 7 = original / filter all 3 voices (only 8580 SID)
									;^1 = filters only bass for 6581 SID (P500 / CBM2)
; ***************************************** ADDRESSES *********************************************
!addr CodeBank			= $00		; code bank register
!addr IndirectBank		= $01		; indirect bank register
!addr IRQ_vector		= $0300		; IRQ_vector
!addr VIC				= $d800		; VIC base address
!addr SID				= $da00		; SID base address
!addr CIA				= $dc00		; CIA base address
!addr KERNAL_IRQ		= $fbf8		; kernal irq jump in from $0300 vector
!addr HW_RESET			= $fffc		; hardware reset vector
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
zp_ptr:	!word $0002						; +1 = $0003 code start in bank 15
CIA_ptr:!word CIA

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
codecpy:lda code-1,y
		sta (zp_ptr),y
		dey
		bne codecpy
		ldy #$0e
		lda #$01
		sta (CIA_ptr),y						; start timer A with phi2 speed, continous mode

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
*= $0203
code:
pt_d9ff:!byte <(SID-1), >(SID-1)				; SID base -1
pt_d81c:!byte <(VIC+$1c), >(VIC)				; VIC reg $1c
		!byte $32, $32, $35;, $00, $00, $00
;
; $0a SID register mirror $d400-$d418
sid_mir:!byte $00, $00, $00, $19, $41, $1c, $d0	; osc 1
;		!byte $00, $dc, $00, $00, $11			  overlapped VIC BGR color mirror $d020-$d024
		!byte $00, $dc, $00, $00, $11, $d0, $e0 ; osc 2
		!byte $0b, $10, $33, $0e, $61, $90, $f5 ; osc 3
		!byte $07, $00, $f8+FILTER, $1f 		; SID common regs - addded filter selection
; script: 1.byte = ZP-address, 2.byte = value
script:	!byte $ff, $18							; dummy write to unused address $ff
		!byte <(sid_mir+ZPDIFF)+$0a, $41
		!byte <(mod_op1+ZPDIFF), 	 $24
		!byte <(sid_mir+ZPDIFF)+$0b, $25
		!byte <(sid_mir+ZPDIFF)+$0b, $53
		!byte <(sid_mir+ZPDIFF)+$0b, $61
		!byte <(mod_op1+ZPDIFF), 	 $29
		!byte <(sid_mir+ZPDIFF)+$11, $0f
; $33 interrupt routine
irq:	inc clock						; increase counter lowyte 2 bytes
		inc clock
		bne noc1
		inc clock_msb					; increase counter highbyte
noc1	lda #$61
		sta <(sid_mir+ZPDIFF)+2*7+4		; SID osc3 control reg
		lax clock_msb					; ILLEGAL opcode A, X = address
		cpx #$3f
		beq highpass
		bcc noend
;		lsr $d811						; not needed - reset does not work!
;		jmp (HW_RESET)					; ^ 3 bytes saved
		brk					; saves 2 bytes for splitting script first 2 bytes from sid-mirror
; $4b
highpass:
		ldy #$6d
		sty <(sid_mir+ZPDIFF)+$18
		sty <(mod_op2+ZPDIFF)
; $51
noend:	
		lsr
		asr #$1c						; ILLEGAL opcode (A & imm) /2
		tay
		lda clock
		and #$30
		bne noduck
		dec <(sid_mir+ZPDIFF)+2*7+4		; SID osc2 control reg
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
		and #$03				; needed the 3 extra bytes saved from above 
		bpl skip
;		!byte $2d		and #$ab00 - changed because at #$03 target value moves 2 bytes
; $72
bassoff:
		lxa #$00						; ILLEGAL opcode clears A, X
skip:
		bcs bassdone
		lax <(script+1+ZPDIFF),y		; ILLEGAL opcode A, X = address
		ldx <(script+ZPDIFF),y
		sta $00,x
		lda clock
		asr #$0e						; ILLEGAL opcode A = (A & imm) /2
		tax								;   gets random value 0-$e -> 0-7
		sbx #$34						; ILLEGAL opcode X = (A & X) - imm
		stx vmptr+1						;   0-7 - $34 -> $cc-$d3
		eor #$07
; $87
bassdone:
		sta <(sid_mir+ZPDIFF)+0*7+1		; SID osc1 frequency hi
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
		lda <(freqtbl+ZPDIFF),x
		sta <(sid_mir+ZPDIFF)+1*7+1		; SID osc2 frequency hi
; $9e
nomel:
		ldy #$08
; $a0
vicloop:
		lax <(sid_mir+ZPDIFF)+3,y		; ILLEGAL opcode A, X = address
		sta (pt_d81c+ZPDIFF),y
		dey
		bpl vicloop
		tay
; $a8
		bit $3f		; **** DUMMY - KERNAL IRQ writes $3f here to $a9 (stop key flag)
; $aa
loop:	
		lax <(sid_mir+ZPDIFF)-1,y		; ILLEGAL opcode A, X = address
		sta (pt_d9ff+ZPDIFF),y
		dey
		bne loop
		jmp KERNAL_IRQ
; $b4 init
init:	nop								; jump in from bank 0
		nop
		nop
		lda #$40						; VIC memory pointers: vm=$d000, char=$c000
		sta VIC+$18
		ldx #<(irq+ZPDIFF)				; set irq vector
		stx IRQ_vector
		ldx #$00						; Background color = black
		stx VIC+$21
		stx IRQ_vector+1				; set irq vector hi = $00
		jmp start+ZPDIFF				; start code
; $cc main routine
start:	lda #$50						; VIC ECM, 24 lines
		sta VIC+$11						
		cli								; enable interrupts
; $d2
mainlp:	lda CIA+$04						; grab CIA timer1 lo as random value
mod_op1:ldy #$c3
mod_op2:ora SID+$1c						; read SID envelope 3
		pha
		asr #$04						; ILLEGAL opcode (A & imm) /2
		ldy #$30						; video matrix at $d000, font at $c000
; $df
		sty $ffff	; ***** DUMMY - KERNAL IRQ key-sub writes $ffff here in $e0,$e1
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