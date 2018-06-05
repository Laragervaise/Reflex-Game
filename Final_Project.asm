/*
 * Projet.asm
 *
 *   Date :  30.05.2018
 *   Author: Léo Corsini & Lara Gervaise
 */ 
; Reflex Game
; use register r18 which corresponds to a0 and r19 which corresponds to a1, , r25, r24, r23 & r16

.include "macros.asm" 
.include "definitions.asm" 

; === interrupt table ===
.org	0
	jmp	reset
.org	INT0addr
	jmp	ext_int0
.org	INT1addr
	jmp	ext_int1
.org	INT2addr
	jmp	ext_int2
.org	INT3addr
	jmp	ext_int3

; === interrupt service routines ===
ext_int0:
	in a1, TCNT0			; get the player's time (not decimal yet)
    LDIZ 2*joueur1			; load the winner's name
	jmp affichageGagnant
	reti
ext_int1:
	in a1, TCNT0
    LDIZ 2*joueur2
	jmp affichageGagnant
	reti
ext_int2:
	in a1, TCNT0
    LDIZ 2*joueur3
	jmp affichageGagnant
	reti
ext_int3:
	in a1, TCNT0
    LDIZ 2*joueur4
    jmp affichageGagnant
	reti

;=== macros ===
.macro	WAIT_CUSTOM ; 0,3s max
	ldi zl, low(@0)
	ldi zh, high(@0)

	loop:
	nop
	nop
	nop
	nop
	nop
	dec zl
	brne loop
	dec zh
	ldi zl, 0xff
	brne loop
.endmacro


;=== strings ===
joueur1 : .db "joueur1         ", 0
joueur2 : .db "joueur2         ", 0
joueur3 : .db "joueur3         ", 0
joueur4 : .db "joueur4         ", 0

go : .db "go!", 0
troplent : .db "trop lent !", 0
finprgm : .db "éteindre carte", 0

; === music score ===
elise:
.db mi3,rem3
.db mi3,rem3,mi3,si2,re3,do3, la2,mi,la,do2,mi2,la2
.db 0 ;odd nomber of byte -> assembler will do padding!

; === constantes ===
.equ random_1=6
.equ durationla=127
.equ random_3=4

;===== sous-routines====

LCD_putstring:		;display a string
; in z 
    lpm	 			        ; load program memory into r0
	tst r0	 		        ; test for end of string
	breq done	 
	mov a0, r0	 	        ; load argument
	rcall	LCD_putc
	adiw zl, 1	 		    ; increase pointer address
	rjmp LCD_putstring	    ; restart until end of string
done:	ret

waiting_loop :
	WAIT_CUSTOM 60000
    WAIT_CUSTOM 60000
	WAIT_CUSTOM 60000
	WAIT_CUSTOM 60000
	ret


;===== sous-routines changement de format ======
putdec: ; display in decimal
;in a1
;use w and a0
	com a1
	mov u,a1
	ldi a1,'0'-1
	ldi w,100
_putdec2:
	inc a1
	sub u,w
	brsh _putdec2
	add u,w
	mov a0, a1
	rcall LCD_putc		;display digit2
	ldi a0,'0'-1
	ldi w,10
_putdec1:
	inc a1
	sub u,w
	brsh _putdec1
	add u,w
	mov a0, a1
	rcall LCD_putc		;display digit1
	ldi a1,'0'
	add a1,u
	rcall LCD_putc		;display digit 0
	ret


reset :
    LDSP RAMEND         ; set up stack pointer (SP) 
    OUTI DDRD, $00      ; configure portD (buttons) as input
    OUTI DDRB, $ff      ; configure portB (LEDs) as output
    OUTI PORTB, 0xff    ; turn off quickly the LEDs
	OUTI ASSR, (1<<AS0)	;set up for timer : asynchronous mode
	OUTI TCCR0, 6		; set up for timer : time scale

    sbi DDRE, SPEAKER   ; make pin SPEAKER an output
    rcall LCD_init      ; initialize the LCD
    rcall encoder_init  ; initialize rotary encoder
    ldi r25, random_1   ; initialize the waiting time before the signal
	ldi r24, durationla ; allow to repeat the la for the bip
    jmp resetNbJoueurs

.include "lcd.asm" 
.include "printf.asm" 
.include "encoder.asm"
.include "menu.asm"
.include "sound.asm"

resetNbJoueurs :
    ldi a0, 0				 ; initialize number of players
choixNbJoueurs :        
    WAIT_MS 1           
    rcall encoder   
	CYCLIC a0, 1, 4
   PRINTF LCD
.db FHEX, a, CR, 0          ; print a (nb de joueurs)
    sbis PIND, 7
    rjmp resetEcritureNom
    rjmp choixNbJoueurs 

 resetEcritureNom :
	WAIT_CUSTOM 60			; for the human time of pushing a button
	OUTI EIMSK,0b0000001    ; enable INT0
	rcall LCD_clear
	ldi r23, 0
    mov c1, a0      
	ldi a0, 'a'             ; save the number of players
ecritureNom :
    WAIT_MS 1
    rcall encoder
    CYCLIC a0, 'a', 'z'
    rcall lcd_putc          ; write character to LCD
    rcall lcd_cursor_left   ; reposition cursor to previous character
	brtc PC+2               ; if button press then cursor to right
    rcall lcd_cursor_right
    sbis PIND, 7            ; if button 7 pressed
    jmp joueurNumero        ; writing next player name
    rjmp ecritureNom

joueurNumero :				; allow to go to the next name
	WAIT_CUSTOM 60
	rcall LCD_clear
	inc r23
	clz
	cpi r23, 1		    	; if only one player, 
	breq  fin		    	; jmp to the selection of the game mode
	OUTI EIMSK,0b00000011   ; select the interrupts enabled
	cpi r23, 2
	breq  fin
	OUTI EIMSK,0b00000111
	cpi r23, 3
	breq  fin
	OUTI EIMSK,0b00001111
	cpi r23, 4
	breq fin
fin :
	clz
	cp c1, r23
	breq resetGameMode
	rjmp ecritureNom

resetGameMode :
	OUTI PORTB, 0b11111111 ; clear all the LEDs
    rcall LCD_clear
    WAIT_CUSTOM 60
    ADDI r25, random_3  ; modify the waiting time before the signal
    ldi a0, 0           ; initialize game mode
gameMode : 
    rcall encoder       ; poll encoder
    CYCLIC a0, 0, 3     ; make cyclic adjustements
    PRINTF LCD
.db CR, a, 0            ; print a (game mode)                
    rcall menui
.db     "visual reflex 1|visual reflex 2|sound reflex 1 |sound reflex 2 ", 0
    WAIT_MS 1
    sbis PIND, 7        ; if button 7 pressed
    jmp choixSignal     ; signal
    rjmp gameMode

choixSignal :
	push a0				; because LCD_putstring modify a0
	rcall LCD_clear
	LDIZ 2*go
	rcall LCD_putstring	; display "go!"
    clz
    cln
	pop a0
    cpi a0, 0
    breq signalVisuel1
    cpi a0, 1
    _BREQ signalVisuel2
    cpi a0, 2
    _BREQ signalAuditif1
    cpi a0, 3
    _BREQ signalAuditif2

signalVisuel1 :
	WAIT_CUSTOM 60000		
    dec r25
	brpl SignalVisuel1		; waiting loop before the signal
    OUTI PORTB, 0b10101010	; signal
	sei                 	; set global interrupt
	OUTI TCNT0, 0b00000000	; timer begins!
	WAIT_CUSTOM 60000
	OUTI PORTB, 0b11111111
	rcall waiting_loop	    ; waiting time before the message "trop lent!"
	rjmp lent

signalAuditif1 :
	WAIT_CUSTOM 60000
    dec r25
	OUTI TCNT0, 0b00000000
	brpl signalAuditif1
la440 :						; signal
	clz
    sbi	PORTE,SPEAKER
	WAIT_US	 1140
	cbi	PORTE,SPEAKER
	WAIT_US	 1140
	sei
	dec r24					; to repeat the "la" some seconds
	brne la440
	rcall waiting_loop
	rjmp lent

signalVisuel2 :
    OUTI PORTB, 0b10101010	; blinking LEDs
	OUTI TCNT0, 0b00000000
	WAIT_CUSTOM 30000
	OUTI PORTB, 0b11111111
	WAIT_CUSTOM 30000
    dec r25
    brpl signalVisuel2
	sei
    rcall waiting_loop
	rjmp lent

signalAuditif2 :
	LDIZ 2*elise		 ; pointer z to begin of musical score
	clz
playMusic:	
	lpm					 ; load note to a0
	adiw zl, 1			 ; increment the pointer in order to go to the next note
	dec r25		 
	tst r0				 ; test en of the music (NUL)
	breq end
	mov a0, r0			 ; move note to a0
	ldi b0, 100		  	 ; load play duration (125ms)
	rcall sound			 ; play the sound
	rjmp playMusic
end:
	OUTI TCNT0, 0b00000000
	sei
	rcall waiting_loop
	rjmp lent

lent :
	cli
	rcall LCD_clear
	LDIZ 2*troplent
	rcall LCD_putstring
	rcall waiting_loop
	rjmp resetReplay

affichageGagnant :
	cli
	rcall LCD_clear
    rcall LCD_putstring	; display the winner
    rcall waiting_loop
	rcall LCD_clear
	rcall putdec		; display the score
	rcall waiting_loop
    rjmp resetReplay

resetReplay :
    rcall LCD_clear
    clz					; clear Z to enable a good result for next comparisons
    ldi a0, 0           ; initialize choice
    rjmp replay
replay :
    rcall encoder       ; poll encoder
    CYCLIC a0, 0, 1     ; make cyclic adjustements
    PRINTF LCD
.db CR, a, 0            ; print a (choice)
    rcall menui
.db     "replay|finish", 0
    WAIT_MS 1
    cpi a0, 0
    sbis PIND, 7
    breq intermediate	
    sbis PIND, 7		
    jmp finProgram
    rjmp replay
intermediate :			; because here, the macro cannot work 
    rjmp resetGameMode  ; (because if PD7 is pressed it skips only the first line of the macro)

finProgram :
    rcall LCD_clear
	LDIZ 2*finprgm
	rcall LCD_putstring
    rcall waiting_loop
	rcall waiting_loop
