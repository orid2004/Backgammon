.286
.386
LOCALS @@
IDEAL
MODEL small
STACK 100h
DATASEG
;-------------------------Variables----------------

note dw 2394h ; 1193180 / 131 -> (hex)
message db ?
	;	* * * * * * * * * * * *m* * * * * * * * * * * *
brd1 db 0,0,0,0,0,5,0,3,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,2 ;player 1. DIRECTION <------- DOWN
brd1copy db 0,0,0,0,0,5,0,3,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,2
	;	* * * * * * * * * * * *m* * * * * * * * * * * *
brd2 db 2,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,3,0,5,0,0,0,0,0 ;player 2. DIRECTION --------> UP
brd2copy db 2,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,3,0,5,0,0,0,0,0


hicube db ? ;higher cube number
locube db ? ;lower cube number 
dishicube db ?
dislocube db ?
rndcube1 db ?
rndcube2 db ?

cubeflag db ?
lodoubleflag db 0
hidoubleflag db 0

canRemove1 db 0 ;Bool
canRemove2 db 0 ;Bool

eaten1 db 0 ;Number of eaten checkers --> board1 by player 2
eaten2 db 0 ;Number of eaten checkers --> board2 by player 1
eatenIndex db 160

won1 db ?
won2 db ?

;bools
sucMove db 0 ; Successfull move global variable

r1 dw ?

can1enter db ? ;white
can2enter db ? ;red

can1move db ? ;white
can2move db ? ;red

;xtoindex arr
arr dw -1, 33, 56, 78, 100, 125, 145, 175, 195, 216, 236, 258, 281, 306
;-------------------------------------------------

bg db 'bg.bmp', 0 
bgs db 'bgs.bmp', 0 
wht db 'white2.bmp', 0
red db 'red.bmp', 0
wball db 'wball.bmp', 0
rball db 'rball.bmp', 0
c1 db 'c1.bmp', 0
c2 db 'c2.bmp', 0
c3 db 'c3.bmp', 0
c4 db 'c4.bmp', 0
c5 db 'c5.bmp', 0
c6 db 'c6.bmp', 0
pichandle db '', 0
filehandle dw ?
Header db 54 dup (0)
Palette db 256*4 dup (0)
ScrLine db 320 dup (0)
ErrorMsg db 'Error', 13, 10 ,'$'
imgW dw ?
file dw ?
imgH dw ?
x dw ?
y dw ?

xToIndexValue db ?
;--------------------------

; ----------------- A. define variable here -----------
prG_Range_Result_LFSR dw 0, 2, 4  ; 3x 16-bit words : Range, result , LFSR
prG16_range EQU   word offset prG_Range_Result_LFSR
prG16_result EQU  word offset prG_Range_Result_LFSR +2
prG16_LFSR  EQU   word offset prG_Range_Result_LFSR +4
prGStat db ?
;-------------------B. Messages

;==============  POINTER array ========== BEWARE OF ESTED procs !!
;offset: 0   , +2  ,  +4 , +6     ,  +8    ,  +10    , +12       , +14   , +16     , +18 , +20   ,   +22   ,   +24

P_msg0			equ 0 
P_msg1 			equ 2
P_msg2 			equ 4
P_msg_HAK 		equ 6		; push [P+P_msg_HAK] ; 1 dw param
; ----------------------------------------------------- 

CODESEG
proc OpenFile
	; Open file
	mov ah, 3Dh
	xor al, al
	mov dx, [file]
	int 21h
	jc openerror
	mov [filehandle], ax
	ret
	openerror :
	mov dx, offset ErrorMsg
	mov ah, 9h
	int 21h
	ret
endp OpenFile


proc ReadHeader
; Read BMP file header, 54 bytes
	mov ah,3fh
	mov bx, [filehandle]
	mov cx,54
	mov dx,offset Header
	int 21h
	ret
endp ReadHeader


proc ReadPalette
	; Read BMP file color palette, 256 colors * 4 bytes (400h)
	mov ah,3fh
	mov cx,400h
	mov dx,offset Palette
	int 21h
	ret
endp ReadPalette


proc CopyPal
	; Copy the colors palette to the video memory
	; The number of the first color should be sent to port 3C8h
	; The palette is sent to port 3C9h
	mov si,offset Palette
	mov cx,256
	mov dx,3C8h
	mov al,0
	; Copy starting color to port 3C8h
	out dx,al
	; Copy palette itself to port 3C9h
	inc dx
	PalLoop:
	; Note: Colors in a BMP file are saved as BGR values rather than RGB .
	mov al,[si+2] ; Get red value .
	shr al,2 ; Max. is 255, but video palette maximal
	; value is 63. Therefore dividing by 4.
	out dx,al ; Send it .
	mov al,[si+1] ; Get green value .
	shr al,2
	out dx,al ; Send it .
	mov al,[si] ; Get blue value .
	shr al,2
	out dx,al ; Send it .
	add si,4 ; Point to next color .
	loop PalLoop
	ret
endp CopyPal


proc CopyBitmap
	; BMP graphics are saved upside-down .
	; Read the graphic line by line (200 lines in VGA format),
	; displaying the lines from bottom to top.
	mov ax, 0A000h
	mov es, ax
	mov cx, [imgH]
	@@PrintBMPLoop :
	push cx
	; di = cx*320, point to the correct screen line
	;bottom left pixel of image
	mov di,cx
	shl cx,6
	shl di,8
	add di,cx
	;Wanted position 320y + x
	mov dx, [y] ;320y
	;dx = 320*dx
	mov [r1], dx
	shl dx, 6
	shl [r1], 8
	add dx, [r1]
	;dx = dx + x
	add dx, [x] ;x
	add di, dx ;Add pixels and move to wanted position
	; Read one line
	mov ah,3fh
	mov cx,[imgW]
	mov dx,offset ScrLine
	int 21h
	; Copy one line into video memory
	cld ; Clear direction flag, for movsb
	mov cx,[imgW]
	dec cx
	mov si,offset ScrLine
	rep movsb
	pop cx
	loop @@PrintBMPLoop
	ret 
endp CopyBitmap


proc CopyBitmapChecker
	; BMP graphics are saved upside-down .
	; Read the graphic line by line (200 lines in VGA format),
	; displaying the lines from bottom to top.
	mov ax, 0A000h
	mov es, ax
	mov cx, [imgH]
	@@PrintBMPLoop :
	push cx
	; di = cx*320, point to the correct screen line
	;bottom left pixel of image
	mov di,cx
	shl cx,6
	shl di,8
	add di,cx
	;Wanted position 320y + x
	mov dx, [y] ;320y
	;dx = 320*dx
	mov [r1], dx
	shl dx, 6
	shl [r1], 8
	add dx, [r1]
	;dx = dx + x
	add dx, [x] ;x
	add di, dx ;Add pixels and move to wanted position
	; Read one line
	mov ah,3fh
	mov cx,[imgW]
	mov dx,offset ScrLine
	int 21h
	; Copy one line into video memory
	cld ; Clear direction flag, for movsb
	mov cx,[imgW]
	dec cx
	mov si,offset ScrLine
	@@masking:
		cmp [ds:si],0 ;check if the pixel color is black. If so, don't show it on the screen
		je @@skip_color
		mov al,[ds:si]
		mov [es:di], al ;show the pixel on the screen
		@@skip_color:
			inc si
			inc di
			loop @@masking
			
	 ; Copy line to the screen
	 ;rep movsb is same as the following code :
	 ;mov es:di, ds:si
	 ;inc si
	 ;inc di
	 ;dec cx
	 ;loop until cx=0
	pop cx
	loop @@PrintBMPLoop
	ret 
endp CopyBitmapChecker


proc CloseFile
	mov ah,3Eh
	mov bx, [word ptr filehandle]
	int 21h
	ret
endp CloseFile


proc rollCubes
	mov [prGStat], 0
	push [prG16_result]
	call PRT_WORD_HEX_val  
    call prGrand16  
	mov [prGStat], 1
	push [prG16_result]
	call PRT_WORD_HEX_val  
    call prGrand16  
	mov al, [locube]
	mov [dislocube], al
	mov [rndcube1], al
	mov ah, [hicube]
	mov [dishicube], ah
	mov [rndcube2], ah
	cmp al, ah
	jg switch
	ret
	switch:
		mov [locube], ah
		mov [dislocube], ah
		mov [hicube], al
		mov [dishicube], al
		ret
endp rollCubes


proc Empty_Pop ; pop >NULL 1 word
	ret 2 ; wipe input from stack by routine 
endp Empty_Pop


proc PRT_WORD_HEX_val ; (byval dw value in stack : BP+4)
;                Prints 4 hex on screen
; set bp,	push registers to be restored at end :  
    PUSH BP
    mov	bp, sp   
	pushF   ; flags , and all registers that will be changed here
    PUSH AX ;  
    PUSH BX ;  
    PUSH CX ;  
    PUSH DX ;  
;	uses temp BX , DX, CX
	mov BX,[BP+4]  ; ax cleared for interrupts, Dl will be for printout 
    mov ah, 2  ; char out int 21h,2
	mov cx,4 ; 4 times
PRT_WORD_HEX_val_loop_nibble: 
;   rot 4 left (msNibble in BL 4 lsb's, value=0-15)
    ROL BX , 4 
	mov DL,BL 
	and DL,15 ; leave low nibble
	add DL, 48; 0-9 is now ascii
	cmp DL, 58
	jb PRT_WORD_HEX_val_prt_char ; skip if number
	add DL, 7 ; make 11-15 into A-F 
PRT_WORD_HEX_val_prt_char:
	int 21h	 
	loop PRT_WORD_HEX_val_loop_nibble
;	restore reg
	pop Dx
	pop CX
	pop BX
	pop AX
	popF
    POP BP
	ret 2 ; wipe input from stack by routine
endp ;BIN16_2_hex
;==========================

;=================================
;==================================== 

;=================================
;# # # # # # # # # # # # Pseudo-Random Galois RANDom 16 bit   # # # # # # # # # # # # # # # # # # 	 
;                        -      -      -      ----   --
; THE PROCEDURE CODE  (parameters are passed by fixed vars in Data Segments:	 
proc prGrand16 		; like hi level language:  " prGrand16 ( dw prG16_range, dw prG16_result , dw prG16_LFSR)" 
        push AX 
        push CX 
        push DX 
        pushF 
		CMP [prG16_range],0 
		JZ init_LFSR
      ; else: churn 16 times
        mov AX , [prG16_LFSR]
		mov DX , 0				; now ready to divide (DX:AX) by a 16 bit number
	    mov cx , 17
	prGrand16_loop1: 
		roR AX, 1           ; ROR rotates , also pushes the previous LSB to CF 
		jnc prGrand16_skip1
		xor AX, 3400h		; The Galois xor with 1 on 3 bits (13,12,10)
	prGrand16_skip1 : 
		loop prGrand16_loop1    	
	; results handling:
		mov  [prG16_LFSR], AX   ; keep new PRG value in memory !
		mov CX , [prG16_range]  ; WARNING - if your program messes with [prG16_range] - divide by ZERO  will "kill you"
		div CX
		mov  [prG16_result], DX ; push the remainder (holding a number between 0 to range-1) to memory
		jmp prGrand16_exit
	; "New-seed" code (put a new seed in the Pseudo-random generator):	
	init_LFSR:               
;	; Using the read-clock - (DH:DL) gets the (sec:fractions) 
        mov ah,2ch
		int 21h              ; int 21h,2ch contaminates CX & DX 
		mov [prG16_LFSR] , dx ; new random seed in place
		mov [prG16_result] , dx ; new random seed in place
		
    ; read directly from clock: 0040:006ch
;        MOV CX , [0040h:006Ch]
;		MOV  [prG16_LFSR] , CX
;		MOV  [prG16_result] , CX
        mov [prG16_range], 0ffffh ; Default: returns an ALMOST full 16 bit pseudo number (0-65534)		     
	; exit code:	
	prGrand16_exit:
		mov ax, [prG16_result]
		mov ah, 0
		mov bl, 6
		xor dx, dx
		div bl
		inc ah
		cmp [prGStat], 0
		je @@lowc
		mov [hicube], ah
		jmp @@next	
		@@lowc:
			mov [locube], ah	
		@@next:
        popF                ; restoring the registers to org condition before this call
        pop DX 
        pop CX 
        pop AX 
		RET
endp prGrand16


proc movePlayer
	;Parameters: Start index, move
	;CX NOTE: negative cx for player 2 --> Direction: UP. positive cx for player 1 --> Direction: Down
	push bp
	mov bp, sp
	xor ax, ax
	xor cx, cx
	mov ax, [bp+4] ;Get checker index, 0 - right bottom. 23 - top right [11]
	mov cx, [bp+6] ;Get legal \ ilegal move [5]
	cmp cx, 0
	jg @@player1Main
	jng @@player2Main
	;bx is the current player's board. cx > 0 -- player 1 -- bx offest brd1 || cx < 0 -- player2 -- bx offest brd2
	@@player1Main:
		mov bx, offset brd1 
		mov si, offset brd2 
		cmp [eaten1], 0 
		je @@next
		mov [sucMove], 0
		jmp @@endproc
	@@player2Main:
		mov bx, offset brd2 
		mov si, offset brd1
		cmp [eaten2], 0 ;Move is ilegal while there are pending eaten checkers
		je @@next
		mov [sucMove], 0
		jmp @@endproc
	@@next:	
		add bx, ax ;move to checker brd1
		add si, ax ;move to checker brd2
		cmp [byte bx], 0 ;can't move 0 checkers
		jg @@move 
		mov [sucMove], 0 ;Aware main program of failed moves
		jmp @@endproc ;END MOVE
	@@move:
		;Move is legal in case of enemy checkers
		dec [byte bx] ;Remove checker. In case of false move checker will be restored,
		sub bx, cx ;Move to new index
		;Check board limits
		;If move is outside the board, a player can remove a checker from the board if legal. If not - ilegal move.
		cmp cx, 0 
		jg @@checkStart ;Positive value, check board 1
		jng @@checkEnd ;Negative value, check board 2
		;check 4 limits end-start
		@@checkStart: ;Board1 ends in index 0 (Direction Down)
			cmp bx, offset brd1
			jl @@checkCanRemove1 ;Checker out of board limits
			jmp @@normalMove ;Move is legal in case of board limits
		
		@@checkEnd: ;Board2 ends in index 23 (Direction Up)
			mov dx, offset brd2
			add dx, 23
			cmp bx, dx
			jg @@checkCanRemove2 ;Checker out of board limits
			jmp @@normalMove ;Move is legal in case of board limits
			
		@@checkCanRemove1:
			push ax
			push bx
			push cx
			call canRemoveCheckers
			pop cx
			pop bx
			pop ax
			cmp [canRemove1], 1
			jne @@false ;Move is ilegal, outside board range.
			mov [sucMove], 1
			jmp @@endproc	
			
		@@checkCanRemove2:
			push ax
			push bx
			push cx
			call canRemoveCheckers
			pop cx
			pop bx
			pop ax
			cmp [canRemove2], 1
			jne @@false ;Move is ilegal, outside board range.
			mov [sucMove],1
			jmp @@endproc
			
		@@normalMove:
			;Check if move is legal in case of enemy checkers
			sub si, cx ; Test move on board2
			cmp [byte si], 1 ; Check number of enemy checkers
			jg @@false ; Move is ilegal. More than 1 enemy checkers
			je @@eat ; 1 Enemy checker, eatable
			jmp @@continue
			@@eat:
				call eat
					
			@@continue: ;Finish legal move
				mov [sucMove], 1 ;Aware main program of successful move
				inc [byte bx] ;Add checker to new index
				jmp @@endproc 		
			
	;Return Note: 1[successful], 0[failed]
	@@false:
		add bx, cx ;Return to original index in main board
		inc [byte bx] ;Restore the checker
		mov [sucMove], 0 ;Aware main program of failed moves
		jmp @@endproc
		
	@@endproc:
		pop bp
		ret 4
endp movePlayer
	
	
proc testMovePlayer
	;Parameters: Start index, move
	;CX NOTE: negative cx for player 2 --> Direction: UP. positive cx for player 1 --> Direction: Down
	push bp
	mov bp, sp
	xor ax, ax
	xor cx, cx
	mov ax, [bp+4] ;Get checker index, 0 - right bottom. 23 - top right [11]
	mov cx, [bp+6] ;Get legal \ ilegal move [5]
	mov [sucMove], 0
	cmp cx, 0
	jg @@player1Main
	jng @@player2Main
	;bx is the current player's board. cx > 0 -- player 1 -- bx offest brd1 || cx < 0 -- player2 -- bx offest brd2
	@@player1Main:
		mov bx, offset brd1 
		mov si, offset brd2 
		cmp [eaten1], 0 
		je @@next
		mov [sucmove], 0
		jmp @@endproc
		
	@@player2Main:
		mov bx, offset brd2 
		mov si, offset brd1
		cmp [eaten2], 0 ;Move is ilegal while there are pending eaten checkers
		je @@next
		mov [sucMove], 0
		jmp @@endproc
		
	@@next:	
		add bx, ax ;move to checker brd1
		add si, ax ;move to checker brd2
		cmp [byte bx], 0 ;can't move 0 checkers
		jg @@move 
		mov [sucMove], 0 ;Aware main program of failed movesc
		jmp @@endproc ;END MOVE
		
	@@move:
		;Move is legal in case of enemy checkers
		sub bx, cx ;Move to new index
		;Check board limits
		;If move is outside the board, a player can remove a checker from the board if legal. If not - ilegal move.
		cmp cx, 0 
		jg @@checkStart ;Positive value, check board 1
		jng @@checkEnd ;Negative value, check board 2
		;check 4 limits end-start
		@@checkStart: ;Board1 ends in index 0 (Direction Down)
			cmp bx, offset brd1
			jng @@checkCanRemove1 ;Checker out of board limits
			jmp @@normalMove ;Move is legal in case of board limits
		
		@@checkEnd: ;Board2 ends in index 23 (Direction Up)
			mov dx, offset brd2
			add dx, 23
			cmp bx, dx
			jg @@checkCanRemove2 ;Checker out of board limits
			jmp @@normalMove ;Move is legal in case of board limits
			
		@@checkCanRemove1:
			push ax
			push bx
			push cx
			call canRemoveCheckers
			pop cx
			pop bx
			pop ax
			cmp [canRemove1], 1
			jne @@false ;Move is ilegal, outside board range.
			mov [sucMove], 1
			jmp @@endproc
		
		@@checkCanRemove2:
			push ax
			push bx
			push cx
			call canRemoveCheckers
			pop cx
			pop bx
			pop ax
			cmp [canRemove2], 1
			jne @@false ;Move is ilegal, outside board range.
			mov [sucMove],1
			jmp @@endproc
		
		@@normalMove:
			;Check if move is legal in case of enemy checkers
			sub si, cx ; Test move on board2
			cmp [byte si], 1 ; Check number of enemy checkers
			jg @@false ; Move is ilegal. More than 1 enemy checkers
			je @@eat ; 1 Enemy checker, eatable
			jmp @@continue
			@@eat:
				;call eat [do nothing]		
			@@continue: ;Finish legal move
				mov [sucMove], 1 ;Aware main program of successful move
				jmp @@endproc 		
			
	;Return Note: 1[successful], 0[failed]
	@@false:
		add bx, cx ;Return to original index in main board
		mov [sucMove], 0 ;Aware main program of failed moves
		jmp @@endproc
		
	@@endproc:
		pop bp
		ret 4
endp testMovePlayer
	

proc eat
	dec [byte si] ;Remove eaten enemy checker
	cmp cx, 0
	jg @@incEaten2 ;Player 1 (main) eats player 2
	jng @@incEaten1 ;Player 2 (main) eats player 1
	@@incEaten1: 
		inc [eaten1] ;Aware the main program of the eaten checker
		jmp @@endproc
		
	@@incEaten2:
		inc [eaten2] ;Aware the main program of the eaten checker

	@@endproc:
		ret 
endp eat


proc moveEaten
	push bp
	mov bp, sp
	;Parameters: cx[Legal \ ilegal move]
	;CX NOTE: negative cx for player 2 --> Direction: UP. positive cx for player 1 --> Direction: Down
	xor cx, cx
	mov cx, [bp+4] ;Get legal \ ilegal move
	cmp cx, 0
	jg @@player1Main
	jng @@player2Main
	;bx is the current player's board. cx > 0 -- player 1 -- bx offest brd1 || cx < 0 -- player2 -- bx offest brd2
	@@player1Main:
		mov bx, offset brd1 
		add bx, 24 ;index 24
		sub bx, cx ;Get to index (cx > 0)
		mov si, offset brd2 
		add si, 24
		sub si, cx ;Get to index sub board
		jmp @@next
		
	@@player2Main:
		mov bx, offset brd2 
		dec bx ;index -1
		sub bx, cx ;Get to position (cx < 0)
		mov si, offset brd1
		dec si ;index 24
		sub si, cx ;Get to index sub board
		
	;Both boards are in position
	@@next:	
		cmp [byte si], 1 ;check for enemy checkers
		jg @@failed ;Failed
		je @@eat
		@@continue:
			inc [byte bx]
			cmp cx, 0
			jg @@dec1
			jmp @@dec2
			
		@@dec1:
			dec [eaten1]
			jmp @@endproc
			
		@@dec2:
			dec [eaten2]
			jmp @@endproc
	
	@@eat:
		call eat
		jmp @@continue

	@@failed:
		jmp @@endproc

	@@endproc:
		pop bp
		ret 2	
endp moveEaten


proc testMoveEaten
	push bp
	mov bp, sp
	;Parameters: cx[Legal \ ilegal move]
	;CX NOTE: negative cx for player 2 --> Direction: UP. positive cx for player 1 --> Direction: Down
	xor cx, cx
	mov cx, [bp+4] ;Get legal \ ilegal move
	cmp cx, 0
	jg @@player1Main
	jng @@player2Main
	;bx is the current player's board. cx > 0 -- player 1 -- bx offest brd1 || cx < 0 -- player2 -- bx offest brd2
	@@player1Main:
		mov si, offset brd2 
		add si, 24
		sub si, cx ;Get to index sub board
		jmp @@next
		
	@@player2Main:

		mov si, offset brd1
		dec si ;index 24
		sub si, cx ;Get to index sub board	
	;Both boards are in position
	@@next:	
		cmp [byte si], 1 ;check for enemy checkers
		jg @@failed ;Failed
		jmp @@endproc

	@@failed:
		cmp cx, 0
		jng @@subp2
		dec [can1enter]
		jmp @@endproc

		@@subp2:
			dec [can2enter]

	@@endproc:
		pop bp
		ret 2	
endp testMoveEaten


proc checkForNoMove
	mov [can1move], 0
	mov [can2move], 0
	push ax
	push bx
	push cx
	push dx	
	mov bx, offset brd1
	dec bx
	mov dx, 0
	mov cx, 24
	@@check1:
		inc bx
		cmp [byte bx], 0
		jne @@nxt
		loop @@check1	
		@@nxt:
		push bx
		push cx
		push dx
		xor ax, ax
		mov al, [locube]
		cmp al, 0
		je @@l1
		push ax
		mov dx, bx
		sub dx, offset brd1
		push dx
		call testMovePlayer	
		
		@@l1:	
		pop dx
		pop cx
		pop bx	
		xor ax, ax
		mov al, [sucMove]
		add dx, ax
		;----------------------
		push bx
		push cx
		push dx
		xor ax, ax
		mov al, [hicube]
		cmp al, 0
		je @@l2
		push ax
		mov dx, bx
		sub dx, offset brd1
		push dx
		call testMovePlayer
		
		@@l2:
		pop dx
		pop cx
		pop bx
		xor ax, ax
		mov al, [sucMove]
		add dx, ax
		cmp dx, 0
		jne @@good1
		loop @@check1
		
	mov cx, 24
	mov dx, 0
	mov bx, offset brd2
	dec bx
	@@check2:
		inc bx
		cmp [byte bx], 0
		jne @@next
		loop @@check2
		
		@@next:
		push bx
		push cx
		push dx
		xor ax, ax
		mov al, [locube]
		cmp al, 0
		je @@l3
		neg ax
		push ax
		mov dx, bx
		sub dx, offset brd2
		push dx
		call testMovePlayer
	
		@@l3:
		pop dx
		pop cx
		pop bx
		xor ax, ax
		mov al, [sucMove]
		add dx, ax
		;----------------------
		push bx
		push cx
		push dx
		xor ax, ax
		mov al, [hicube]
		cmp al, 0
		je @@l4
		neg ax
		push ax
		mov dx, bx
		sub dx, offset brd2
		push dx
		call testMovePlayer
	
		@@l4:
		pop dx
		pop cx
		pop bx
		xor ax, ax
		mov al, [sucMove]
		add dx, ax
		cmp dx, 0
		jne @@good2
		loop @@check2
	
	@@ret:
	pop dx
	pop cx
	pop bx
	pop ax
	ret
	
	@@good1:
		mov [can1move], 1
		mov cx, 24
		mov dx, 0
		mov bx, offset brd2
		dec bx
		jmp @@check2
		
	@@good2:
		mov [can2move], 1
		jmp @@ret	
endp checkForNoMove


proc checkFornoEnter
	mov [can1enter], 2
	mov [can2enter], 2
	xor ax, ax
	xor bx, bx
	mov al, [locube]
	mov bl, [hicube]
	cmp ax, 0
	je @@l1
	push ax
	call testMoveEaten
	neg ax
	push ax
	call testMoveEaten
	
	@@l1:
	cmp bx, 0
	je @@ret
	push bx
	call testMoveEaten
	neg bx
	push bx
	call testMoveEaten
	
	@@ret:
	ret 
endp checkFornoEnter


proc canRemoveCheckers
	;Parameters - NONE
	;NOTE - function starts the last step of backgammon game if possible. Uses the flag canRemove1 & canRemove2
	mov bx, offset brd1
	add bx, 23
	mov cx, 18 ;loop first 18 indexes
	@@again:
		cmp [byte bx], 0 
		jne @@next ;if any checker is in first 18 checkers, false, jump to player2
		dec bx
		loop @@again	
	mov [canRemove1], 1 ;True, player1 can remove his checkers
	
	@@next:
		mov bx, offset brd2
		mov cx, 18
		@@again2:
			cmp [byte bx], 0
			jne @@endproc ;if any checker is in first 18 checkers, false, jump to endproc
			inc bx
			loop @@again2
			
		mov [canRemove2], 1 ;True, player2 can remove his checkers
	@@endproc:
		ret 
endp canRemoveCheckers


proc checkEndGame
	mov bx, offset brd1
	mov cx, 24 ;loop through all board
	@@again1:
		cmp [byte bx], 0 ;search for in-game checkers
		jne @@next ;failed
		inc bx
		loop @@again1
		
	mov [won1], 1
	@@next:
		mov bx, offset brd2
		mov cx, 24 ;loop through all board

	@@again2:
		cmp [byte bx], 0 ;search for in-game checkers
		jne @@endproc ;failed
		inc bx
		loop @@again2
		
	mov [won2], 1
	@@endproc:
		ret 
endp checkEndGame


proc printObj
	call openfile
	call ReadHeader
	call ReadPalette
	call CopyPal
	call CopyBitmap
	call closefile
	ret
endp printObj


proc printChecker
	call openfile
	call ReadHeader
	call ReadPalette
	call CopyPal
	call CopyBitmapChecker
	call closefile
	ret
endp printChecker


proc updateScreen
	mov ax,0h
	int 33h
	mov [imgw], 320
	mov [imgh], 200
	mov [x], 0
	mov [y], 0
	mov [file], offset bg
	call printObj	
	mov [imgW], 16
	mov [imgH], 16
	mov cx, 24
	mov bx, offset brd1
	mov [file], offset wht
	mov [x], 288
	mov [y], 172
	@@lower:
		push cx
		xor cx, cx
		mov cl, [bx] ;number of checkers in the index
		push bx
		cmp cx, 0
		je @@movNextIndex
		
		@@again:
			push cx
			call printChecker
			sub [y], 17
			pop cx
			loop @@again

		@@movNextIndex:
			;increase y
			pop bx
			mov [y], 172
			sub [x], 22
			pop cx
			cmp cx, 19
			je @@mid
			cmp cx, 7
			je @@mid
			jmp @@next

		@@mid:	
			sub [x], 28
			
		@@next:
			cmp cx, 13
			je @@changeColor
			inc bx ;next index
			loop @@lower ;print next index
			jmp @@changeHeight
			@@changeColor:
				mov bx, offset brd2
				mov [file], offset red
				mov [x], 288
				mov [y], 172
				loop @@lower ;print next index
		
	@@changeHeight:
		mov cx, 24
		mov bx, offset brd1
		add bx, 23
		mov [file], offset wht
		mov [x], 288
		mov [y], 5
	
	@@upper:
		push cx
		xor cx, cx
		mov cl, [bx] ;number of checkers in the index
		push bx
		cmp cx, 0
		je @@movNextIndex2
		@@again2:
			push cx
			call printChecker
			add [y], 17
			pop cx
			loop @@again2

		@@movNextIndex2:
			;increase y
			pop bx
			mov [y], 5
			sub [x], 22
			pop cx
			cmp cx, 19
			je @@mid2
			cmp cx, 7
			je @@mid2
			jmp @@next2

		@@mid2:	
			sub [x], 28
			
		@@next2:
			cmp cx, 13
			je @@changeColor2
			dec bx ;next index
			loop @@upper ;print next index
			jmp @@endproc
			@@changeColor2:
				mov bx, offset brd2
				add bx, 23
				mov [file], offset red
				mov [x], 288
				mov [y], 5
				loop @@upper ;print next index

	@@endproc:
		;UPDATE CUBES
		mov [imgw], 16
		mov [imgh], 16
		mov [x], 220
		mov [y], 90
		mov al, [dislocube]
		@@again3:		
			cmp al, 1
			je on
			cmp al, 2
			je tw
			cmp al, 3
			je th
			cmp al, 4
			je fo
			cmp al, 5
			je fi
			cmp al, 6
			je sx
			
			on:
				mov [file], offset c1
				jmp show
			tw:
				mov [file], offset c2
				jmp show
			th:
				mov [file], offset c3
				jmp show
			fo:
				mov [file], offset c4
				jmp show
			fi:
				mov [file], offset c5
				jmp show
			sx:
				mov [file], offset c6
				jmp show
				
			show:
				call printObj
				cmp [x], 240
				je @@return
				mov [x], 240
				mov al, [dishicube]
				jmp @@again3
				
		@@return:
			xor cx, cx
			mov cl, [eaten1]
			cmp cx, 0
			je @@next3
			@@showEaten:
				mov [file], offset wht
				mov [x], 155
				xor ax, ax
				mov al, [eatenIndex]
				mov [y], ax
				push cx
				call printChecker
				sub [eatenIndex], 20
				pop cx
				loop @@showEaten
			
			@@next3:
				mov cl, [eaten2]
				cmp cx, 0
				je @@noShow
				@@showEaten2:
					mov [file], offset red
					mov [x], 155
					xor ax, ax
					mov al, [eatenIndex]
					mov [y], ax
					push cx
					call printChecker
					sub [eatenIndex], 20
					pop cx
					loop @@showEaten2
							
			@@noShow:
				mov [eatenIndex], 160
				ret
endp updateScreen


proc xToIndex
	;arr dw -1, 33, 56, 78, 100, 125, 145, 175, 195, 216, 236, 258, 281, 306
	cmp ax, 14
	jng @@failed
	cmp ax, 306
	jg @@failed
	mov cx, 13
	mov bx, offset arr
	add bx, 2
	@@check:
		cmp ax, [word bx]
		jg @@no
		cmp ax, [word bx-2]
		jng @@no
		cmp cx, 7
		je @@failed
		jg subTwo
		sub cl, 1
		mov [xToIndexValue], cl
		jmp @@endproc
		subTwo:
			sub cl, 2
			mov [xToIndexValue], cl
			jmp @@endproc
	
		@@no:
			add bx, 2
			loop @@check
			
	@@failed:
		mov [xToIndexValue], -1
		jmp @@endproc

	@@endproc:
		ret
endp xToIndex


proc drawTurn	
	push bp
	mov bp, sp	
	mov [x], 1
	mov [y], 108
	mov cx, 20
	@@again:
		push cx
		mov cx, 10
		
		@@line:
			mov bh,0h
			push cx
			mov cx,[x]
			mov dx,[y]
			mov al,[bp+4]
			mov ah,0ch
			int 10h
			pop cx
			inc [x]
			loop @@line
		
		sub [y], 1
		sub [x], 10
		pop cx 
		loop @@again
		
	cmp [x], 308
	je @@ret
	mov [x], 308
	mov [y], 108
	mov cx, 20
	jmp @@again
	
	@@ret:
	mov ax,0h
	int 33h
	mov ax,1h
	int 33h
	pop bp
	ret 2
endp drawTurn


proc sleep
	push cx
	push dx
	push ax
	MOV CX, 0FH
	MOV DX, 4240H
	MOV AH, 86H
	INT 15H
	pop ax
	pop dx
	pop cx
	ret
endp sleep


proc resetGame
	mov bx, offset brd1
	mov si, offset brd1copy
	mov cx, 24
	
	@@copy1:
		mov al, [si]
		mov [bx], al
		inc bx
		inc si
		loop @@copy1
		
	mov bx, offset brd2
	mov si, offset brd2copy
	mov cx, 24
	
	@@copy2:
		mov al, [si]
		mov [bx], al
		inc bx
		inc si
		loop @@copy2
	
	mov [note], 2394h
	mov [eaten1], 0
	mov [eaten2], 0
	mov [eatenIndex], 160
	mov [won1], 0
	mov [won2], 0
	mov [lodoubleflag],0
	mov [hidoubleflag], 0
	mov [sucMove], 0
	mov ax, @data
	mov ds, ax
	pop bx
	pop bx
	jmp start
	ret
endp resetGame


proc waitForNewGame
	mov [x], 0
	mov [y], 0
	mov [imgW], 320
	mov [imgH], 200
	mov [file], offset bgs
	call printObj
	mov ah, 0h
	int 16h
	call newGame
	RET

endp waitForNewGame


proc newGame
	mov [x], 0
	mov [y], 0
	mov [imgW], 320
	mov [imgH], 200
	mov [file], offset bg
	call printObj
	call sleep
	call rollCubes
	@@showColors:
		mov [x], 1
		mov [y], 108
		mov cx, 20
		mov al, 12
		@@again2:
			push cx
			mov cx, 10
			@@line:
				mov bh,0h
				push cx
				mov cx,[x]
				mov dx,[y]
				mov ah,0ch
				int 10h
				pop cx
				inc [x]
				loop @@line
			
			sub [y], 1
			sub [x], 10
			pop cx 
			loop @@again2
			
		cmp [x], 308
		je @@showcube
		mov [x], 308
		mov [y], 108
		mov cx, 20
		mov al, 0ffh
		jmp @@again2
	
	@@showcube:
		mov [imgw], 16
		mov [imgh], 16
		mov [x], 220
		mov [y], 90
		mov al, [rndcube1]
		@@again:		
			cmp al, 1
			je @@on
			cmp al, 2
			je @@tw
			cmp al, 3
			je @@th
			cmp al, 4
			je @@fo
			cmp al, 5
			je @@fi
			cmp al, 6
			je @@sx
			
			@@on:
				mov [file], offset c1
				jmp @@show
			@@tw:
				mov [file], offset c2
				jmp @@show
			@@th:
				mov [file], offset c3
				jmp @@show
			@@fo:
				mov [file], offset c4
				jmp @@show
			@@fi:
				mov [file], offset c5
				jmp @@show
			@@sx:
				mov [file], offset c6
				jmp @@show
				
			@@show:
				call printObj
				cmp [x], 30
				je @@ret
				mov [x], 30
				mov al, [rndcube2]
				call sleep
				jmp @@again
	
	@@ret:
	call sleep 
	ret
endp newGame


proc main
	@@newGame:
		call waitForNewGame
		mov al, [rndcube1]
		mov ah, [rndcube2]
		cmp ah, al
		je rollagain
		jg @@player2prep
		jng @@player1prep
		
		rollagain:
			jmp @@newGame
	
	@@player1prep:
		mov [lodoubleflag], 0
		mov [hidoubleflag], 0
		call rollCubes
		mov al, [locube]
		mov ah, [hicube]
		cmp al, ah
		jne @@noChange
		mov [lodoubleflag], 1
		mov [hidoubleflag], 1
		@@noChange:
		call updateScreen
		push 0ffh
		call drawTurn
		xor ax, ax
		xor bx, bx
		xor cx, cx
		xor dx, dx
	
	@@player1:
		cmp [eaten1], 0
		je @@goodmove
		call checkFornoEnter
		cmp [can1enter], 0
		jne @@goodmove ;1 or 2 options
		call sleep
		call sleep
		jmp @@player2prep ;no options
		@@goodmove:	
			cmp [eaten1], 0
			jg @@allgood
			call checkForNoMove
			cmp [can1move], 1
			je @@allgood
			call sleep
			call sleep
			jmp @@player2prep
	
		@@allgood:
		xor ax, ax
		xor bx, bx
		xor cx, cx
		;Check if round has ended (both cubes are 0 value)
		call canRemoveCheckers
		mov al, [locube]
		add al, [hicube]
		cmp al, 0
		jne @@continue
		jmp @@player2prep

		@@continue:
		; Initializes the mouse
		mov ax,0h
		int 33h
		; Show mouse
		mov ax,1h
		int 33h
		; Loop until mouse click
		@@mouseWait: 
			; open speaker
			mov ax,3h
			int 33h
			push bx
			and bx, 00000001b
			cmp bx, 0
			jg @@loMove
			pop bx
			and bx, 00000010b
			cmp bx, 0
			jg @@hiMove
			jmp @@mouseWait
			
		@@loMove:
			cmp [locube], 0
			jne @@runLowMove
			cmp [hicube], 0
			je @@mouseWait
			mov [cubeflag], 1
			jmp @@next
				
			@@runLowMove:
				mov [cubeflag], -1 ;negative - low move (flag)
				jmp @@next
		
		@@himove:
			cmp [hicube], 0
			je @@mouseWait
			mov [cubeflag], 1 ;positive - high move (flag)
		
		@@next:
			shr cx, 1 ;div cx by 2
			mov ax, cx
			call xToIndex
			cmp [xToIndexValue], -1
			jng @@mouseWait
			cmp dx, 80 ;Upper board
			jng @@upperBoard
			cmp dx, 120
			jg @@normal
			jmp @@mouseWait
			@@upperBoard:
				xor bx, bx
				mov bl, 23
				xor ax, ax
				mov al, [xToIndexValue]
				sub bx, ax ;lower index + same x upper index = 23
				mov [xToIndexValue], bl
				
			@@normal:
			xor cx,cx
			cmp [cubeflag], 0
			jg @@pushHigher
			mov cl, [locube]
			jmp @@pushMove
			
			@@pushHigher:
				mov cl,[hicube]
			
			@@pushMove:
				push cx ;push move
				cmp [eaten1], 0
				je @@movechecker
				mov ax, 24
				sub ax, cx
				cmp al, [xToIndexValue]
				je @@yes
				jmp @@mouseWait
				@@yes:
				call moveEaten
				jmp @@suc
				@@movechecker:
					xor cx, cx
					mov cl, [xToIndexValue]
					push cx ;push index last
					call movePlayer
			
			cmp [sucMove], 1
			je @@suc
			jmp @@mouseWait
			
			@@suc:
				call updateScreen
				call checkEndGame
				cmp [won1], 1
				jne @@nofinish
				mov [x], 110
				mov [y], 50
				mov [file], offset wball
				mov [imgw], 100
				mov [imgh], 100
				call printChecker
				call sleep
				call sleep
				call resetGame
				jmp start

				@@nofinish:
				push 0ffh
				call drawTurn
				cmp [cubeflag], 0
				jg @@zeroHigher
				cmp [lodoubleflag], 1
				jne @@oneLowTurn
				mov [lodoubleflag], 0
				jmp @@player1
				@@oneLowTurn:
					mov [locube], 0
					jmp @@player1
			
			@@zeroHigher:
				cmp [hidoubleflag], 1
				jne @@oneHighTurn
				mov [hidoubleflag], 0
				jmp @@player1
				
				@@oneHighTurn:
					mov [hicube], 0
					jmp @@player1


	@@player2prep:
		mov [lodoubleflag], 0
		mov [hidoubleflag], 0
		call rollCubes
		mov al, [locube]
		mov ah, [hicube]
		cmp al, ah
		jne @@noChange2
		mov [lodoubleflag], 1
		mov [hidoubleflag], 1
		
		@@noChange2:
		call updateScreen
		push 12
		call drawTurn
		mov [cubeflag], 0
		xor ax, ax
		xor bx, bx
		xor cx, cx
		
	@@player2:
		cmp [eaten2], 0
		je @@goodmove2
		call checkFornoEnter
		cmp [can2enter], 0
		jne @@goodmove2 ;1 or 2 options
		call sleep
		call sleep
		jmp @@player1prep ;no options
		
		@@goodmove2:
			cmp [eaten2], 0
			jg @@allgood2
			call checkForNoMove
			cmp [can2move], 1
			je @@allgood2
			call sleep
			call sleep
			jmp @@player1prep
		
		@@allgood2:
		xor ax, ax
		xor bx, bx
		xor cx, cx
		mov al, [locube]
		add al, [hicube]
		cmp al, 0
		jne @@continue2
		jmp @@player1prep

		@@continue2:
		; Initializes the mouse
		call canRemoveCheckers
		mov ax,0h
		int 33h
		; Show mouse
		mov ax,1h
		int 33h
		; Loop until mouse click
		@@mouseWait2:
			mov ax,3h
			int 33h
			push bx
			and bx, 1b
			cmp bx, 0
			jg @@loMove2
			pop bx
			and bx, 10b
			cmp bx, 0
			jg @@hiMove2
			jmp @@mouseWait2
			
			
		@@loMove2:
			cmp [locube], 0
			jne @@runLowMove2
			cmp [hicube], 0
			je @@mouseWait2
			mov [cubeflag], 1
			jmp @@next2
			
			@@runLowMove2:
				mov [cubeflag], -1 ;negative - low move (flag)
				jmp @@next2
		
		@@himove2:
			cmp [hicube], 0
			je @@mouseWait2
			mov [cubeflag], 1 ;positive - high move (flag)
		
		@@next2:
			shr cx, 1 ;div cx by 2
			mov ax, cx
			call xToIndex
			cmp [xToIndexValue], -1
			jng @@mouseWait2
			cmp dx, 80 ;Upper board
			jng @@upperBoard2
			cmp dx, 120
			jg @@normal2
			jmp @@mouseWait2
			
			@@upperBoard2:
				xor bx, bx
				mov bl, 23
				xor ax, ax
				mov al, [xToIndexValue]
				sub bx, ax ;lower index + same x upper index = 23
				mov [xToIndexValue], bl

			@@normal2:	
				xor cx,cx
				cmp [cubeflag], 0
				jg @@pushHigher2
				mov cl, [locube]
				jmp @@pushMove2	
			
			@@pushHigher2:
				mov cl,[hicube]
			
			@@pushMove2:
				@@moveeaten2:
				neg cx
				push cx ;push move
				cmp [eaten2], 0
				je @@movechecker2
				neg cx
				dec cx
				cmp cl, [xToIndexValue]
				je @@yes2
				jmp @@mouseWait2
				@@yes2:
					call moveEaten
					jmp @@suc2
				
				@@movechecker2:
					xor cx, cx
					mov cl, [xToIndexValue]
					push cx ;push index last
					call movePlayer

			cmp [sucMove], 1
			je @@suc2
			jmp @@mouseWait2
			
			@@suc2:
				call updateScreen
				call checkEndGame
				cmp [won2], 1
				jne @@nofinish2		
				mov [x], 110
				mov [y], 50
				mov [file], offset rball
				mov [imgw], 100
				mov [imgh], 100
				call printChecker
				call sleep
				call sleep
				call resetGame
				
				@@nofinish2:
				push 12
				call drawTurn
				cmp [cubeflag], 0
				jg @@zeroHigher2
				cmp [lodoubleflag], 1
				jne @@oneLowTurn2
				mov [lodoubleflag], 0
				jmp @@player2
				
				@@oneLowTurn2:
					mov [locube], 0
					jmp @@player2
			
			@@zeroHigher2:
				cmp [hidoubleflag], 1
				jne @@oneHighTurn2
				mov [hidoubleflag], 0
				jmp @@player2
				
				@@oneHighTurn2:
					mov [hicube], 0
					jmp @@player2
	@@endproc: 
		ret
endp main

start :
	mov ax, @data
	mov ds, ax
	; Graphic mode
	mov ax, 13h
	int 10h
	call main

exit :
	mov ax, 4c00h
	int 21h
END start