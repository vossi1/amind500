; "A Mind Was Born" 
; disassembled by DASM6502b v.3.1 by Marat Fayzullin
; modified by Vossi 02/2019
; ported to P500, comments by Vossi 05/2020
!cpu 6510
!to "amind500b0.prg", cbm
; ***************************************** CONSTANTS *********************************************
SYSTEMBANK				= 15		; systembank
CODEBANK				= 0			; codebank 0-9 for basic loader
RASTERLINE				= $fe
FILTER					= 1 		; 7 = original / filter all 3 voices (only 8580 SID)
									;^1 = filters only bass for 6581 SID (P500 / CBM2)
; ************************************** P500 REGISTER ********************************************
VR_MODEY				= $11		; VIC mode register
VR_RASTER				= $12		; raster
VR_MEMPT				= $18		; memory pointers
VR_IRQ					= $19		; interrupt
VR_EIRQ					= $1a		; enebale interrupt
VR_BGRCOL				= $21		; background color
; ***************************************** ADDRESSES *********************************************
!addr CodeBank			= $00		; code bank register
!addr IndirectBank		= $01		; indirect bank register
!addr HW_IRQ			= $fffe		; Hardware interrupt vector
!addr HW_RESET			= $fffc		; Hardware reset vector
!addr temp				= $0300		; some temp bytes
!addr ExitCode			= $0400		; exit routine (copied to bank 15 for switching back to kernal)
!addr ScreenRam			= $0c00		; new screen memory in bank 0
!addr ColorRam			= $d400		; color RAM address in bank 15 (not moveable, fixed in bank 15)
; ***************************************** ZERO PAGE *********************************************
!zone zeropage						; Linus's variables inside the SID register shadow
!addr clock				= $46		; 16 bit counter that controls the sound and the flow
!addr clock_hi			= $53
!addr mel_lfsr			= $47		; shift reg for the melody
; *************************************** BASIC LOADER ********************************************
!zone basic		; The P500 has a bug (and some more)- the SYS command always jumps to bank 15 !!!
*= $0003		; This basic loader copies a small switch routine (53-60) to bank 15 and starts it
				; there with SYS 53 ($0035). The basic loader ends with the three zero bytes at $2a
				; but the space from $2b till $34 is needed to use basic variables in the loader!!!
				; So I used the space in bank 0 for some needed pointers to bank 15. I also used
				; the basic line number as always needed pointer to the VIC registers ;)
		!byte $29, $00
VIC:	!byte $00, $d8, $81, $49, $b2, $35, $33, $a4, $36, $30, $3a		; fori=53to60:
		!byte $dc, $30+CODEBANK, $3a, $41, $b2, $c2, $28, $49, $29, $3a	; bank0:a=peek(i):
		!byte $dc, $31, $35, $3a, $97, $49, $2c, $41, $3a				; bank15:pokei,a:
		!byte $82, $3a, $9e, $35, $33, $00, $00, $00					; next:sys53
	; 10 fori=53to60:bank0:a=peek(i):bank15:pokei,a:next:sys53
; ********************************** 10B ZP HOLE - IO POINTER *************************************
!zone iopointer	; This area is in bank 15 not available! - BASIC needs it for RAM pointers !!!
*= $002b					; I/O pointer table - here is room for 5 pointers in the new codebank
sid_ptr:	!word $da00-1		; SID - 1 for loop (Linus's copy loop need SID minus 1)
vic_ptr:	!word $d800+$1c		; VIC + $1c	for vicloop (Linu's VIC loop needs SID plus $1c)
CIA:		!word $dc00			; CIA base address
TPI1:		!word $de00			; TPI1 base address
TPI2:		!word $df00			; TPI2 base address
; *************************************** ZERO INITBANK *******************************************
!zone initbank	; SYS 53 ($0035) from basic starts this mirrored code in bank 15 
*= $0035		; 
; $35 basic loader starts here in bank 15
		sei						; disable interrupts - This is very important because in the new 
								; codebank is still no interrupt vector and handler installed!
		lda #CODEBANK			; load new codebank number
		sta CodeBank			; store in code bank select register of the 6509
; $3a from here in codebank!
		jmp init				; from here the CPU runs in the shadowed code in the new codebank
								; it instantly jumps to the init routine behind the stack to set up
								; everything for the demo code.
; ***************************************** ZONE DATA *********************************************
!zone data
; hardware pointers
!addr vm_ptr 	= TPI1			; use TPI pointer address after init-routine as universal pointer
; $3d SID register shadow $d400-$d418
sid_mir:!byte $00, $00, $00, $19, $41,$1c, $d0	; osc 1
;		!byte $00, $dc, $00, $00, $11 overlapped VIC bgr-color mirror reg $20-$24 / sid_mir+3 + 4
		!byte $00, $dc, $00, $00, $11, $d0, $e0 ; osc 2
		!byte $0b, $10, $33, $0e, $61, $90, $f5 ; osc 3
		!byte $07, $00, $f8+FILTER, $1f 		; SID filter regs - addded filter selection
; $4a script: 1.byte = ZP-address, 2.byte = value
script:	!byte <(freqtbl+$08), $1f				; dummy write to unused address
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
; $77 interrupt routine					; The complete sound production code from Linus runs in the
										; interrupt and is called 50/60 times per second from the
										; irq handler routine behind the stack.
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
;		lsr								; here I saved 3 bytes from Linus's version
		jmp ExitCode					; jumps to exitcode
; $8e
highpass:
		ldy #$6d
		sty <(sid_mir)+$18
		sty <(mod_op2)
; $93
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
		bpl skip
;		!byte $2d						; This AND absolute instruction loaded in the original code
										; $03 from $ab, but I moved the code 2 bytes and it doesn't
										; worked anymore. I solved this with and+bpl and neded the
										; three previous bytes ;)
; $b6
bassoff:
		lxa #$00						; ILLEGAL opcode clears A, X
skip:	bcs bassdone
		lax <(script+1),y				; ILLEGAL opcode A, X = address
		ldx <(script),y
		sta $00,x						; store script value to script address
		lda clock						; load random clcok value
		asr #$0e						; ILLEGAL opcode A = (A & imm) /2
		tax								; ^ random 0-$e rightshiftet -> 0-7
		sbx #$f7						; ILLEGAL opcode X = (A & X) - imm
		stx vm_ptr+1					; ^ random 8-$f
		eor #$07
; $cb
bassdone:
		sta <(sid_mir)+0*7+1			; SID osc1 frequency hi
		lda clock
		and #$0f
		bne nomel
		lda #$b8
		sre mel_lfsr					; ILLEGAL opcode mem = (mem / 2) : A = A eor mem
		bcc noc2
		sta mel_lfsr
; $db
noc2:	and #$07
		tax
		lda <(freqtbl),x
		sta <(sid_mir)+1*7+1			; SID osc2 frequency hi
; $e2
nomel:
		ldy #$08
; $e4
vicloop:
		lax <(sid_mir)+3,y				; ILLEGAL opcode A, X = address
		sta (vic_ptr),y
		dey
		bpl vicloop
		tay
; $ec
loop:	
		lax <(sid_mir)-1,y				; ILLEGAL opcode A, X = address
		sta (sid_ptr),y
		dey
		bne loop
		rts
; ***************************************** ZONE MAIN *********************************************
; $f4 main routine
main:	ldy #$04						; grab from previous started Timer A lo as random value
		lda (CIA),y						; systembank is already selected as indirect bank to access
; $f8									; the I/O chips.
mod_op1:ldy #$c3						; mod_op1/2 will be changed in the sound program later
; $fa
mod_op2:ora temp
		jmp main2						; main code continues behind the stack
										; our code doesn't fit completely in the zero page because
										; of the bank switching, raster irq, timer init, tpi init!
; ***************************************** STACK HOLE ********************************************
; $0100-$01ff
; ***************************************** ZONE MAIN2 ********************************************
; $0200 main routine
*= $0200
main2:	pha								; pushes continued values to the stack as character data ;)
		asr #$04						; ILLEGAL opcode (A & imm) /2
		tax								; remember in x
		lda #CODEBANK					; switch to indirect bank 0 for pointer operations here
		sta IndirectBank				; vm_ptr accesses the new screen memory at $0c00
		ldy #$30
		lda (vm_ptr),y					; video matrix at $0c00, font at $0000
		sta temp+1
		inc vm_ptr
		lda (vm_ptr),y
		sta temp+2
		txa								; restore from x
		adc temp+1
		adc temp+2
		ror
		ora clock_hi
		ldy #$58
		ora <(mod_op1)
		sta (vm_ptr),y
		lda #SYSTEMBANK					; switch back to systembank for cia, sid access
		sta IndirectBank
		ldy #$1c+1						; read SID envelope 3 as pumping value for the picture and
		lda (sid_ptr),y					; store it temporary because the later ORA instruction is
		sta temp						; not directly to systembank possible (only LDA/STA (),Y )  
		jmp main						; jump back to loop start in zeropage
; **************************************** IRQ HANDLER ********************************************
!zone irqhandler	; IRQ handler in new codebank - the system interrupt-routine does not run here!
IRQ_Handler:		; Called 50/60 times per second depending of the VIC 6567 or 6569
		pha
		txa
		pha
		tya
		pha
		lda IndirectBank
		pha								; save all regs and the actual indirect bank
		lda #SYSTEMBANK
		sta IndirectBank				; switch indirect bank to 15 for I/O access
		ldy #VR_IRQ
		lda (VIC),y						; load VIC interrupt reg and mask bit 1
		and #$01
		beq endirq						; skip if source is not raster interrupt

		jsr irq							; call Linus's sound interrupt routine

		lda #RASTERLINE					; set rasterline again
		ldy #VR_RASTER
		sta (VIC),y						; store it in the raster reg
		lda #$81
		ldy #VR_IRQ						; activate raster irq again
		sta (VIC),y						; clear VIC raster interrupt
endirq:	pla
		sta IndirectBank				; retsore all regs and the remembered indirect bank
		pla
		tay
		pla
		tax
		pla
		rti								; return from interrupt
; ***************************************** ZONE INIT *********************************************
!zone init		; This routine setups the new stack, irq vector in bank 0, the vic banks + memory,
				; starts the timer, clears the color ram and copies the exit code to the free RAM
				; area at $0400 in bank 15.
		; $b2 initialization routine
init:	ldx #$ff						; init new stack in codebank
		txs
; Hardware interrupt vector setup
		lda #>IRQ_Handler		; inits the hardware irq vector in the new codebank to the 
		sta HW_IRQ+1			; irq handler here behind this init code.
		lda #<IRQ_Handler
		sta HW_IRQ
		lda #SYSTEMBANK
		sta IndirectBank				; set bank indirect reg to bank 15
		ldy #$06
		lda (TPI1),y					; load TPI1 control register
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
		lda #$7f						; bit#7=0 clears/mask out all 5 irq sources with bit#0-4 = 1
		ldy #$0d						; CIA interrupt control register
		sta (CIA),y						; disable all hardware interrupts
		ldy #$0e
		lda #$81
		sta (CIA),y						; start timer A with phi2 speed, continous mode
		lda #$00
		ldy #$05
		sta (TPI1),y					; set TPI1 reg $5 interrupt mask reg = $00 - disable all irq
; init color RAM with BLACK		
		ldx #>ColorRam
		stx vm_ptr+1					; set pointer to Color RAM
		ldx #$04						; a already $00 = black, clear 4 pages
		tay								; clear y
colorlp:sta (vm_ptr),y
		iny
		bne colorlp						; next byte
		inc vm_ptr+1
		dex
		bne colorlp						; next pages
; copy exitcode					; this loop copies the exit code to $0400 in this new codebank and
		lda #>ExitCode			; also in bank 15 to switch to there.
		sta vm_ptr+1					; set pointer to ExitCode
		ldy #end-exit
exitlp:	lda exit,y						; copy exitcode
		sta ExitCode,y
		sta (vm_ptr),y
		dey
		bpl exitlp
		lda #>ScreenRam
		sta vm_ptr+1					; init vm_ptr with $0c00 = screen RAM
										; vm_ptr is now used to access the VIC screen memory
; Enable VIC raster IRQ as 50/60 timer for Linus's sound-code generation
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
		jmp main						; start Linus's main code
; Exit code - copied to $0400 in codebank and systembank
exit:	sei								; disables interrupts
		lda #$00
		ldy #$18+1						; SID volume off
		sta (sid_ptr),y
		ldy #4+1						; stop all 3 oscillators
		sta (sid_ptr),y
		ldy #1*7+4+1
		sta (sid_ptr),y
		ldy #2*7+4+1
		sta (sid_ptr),y
		lda #SYSTEMBANK
		sta CodeBank					; switch to systembank
; from here in systembank
		nop
		jmp (HW_RESET)					; call system reset
end: