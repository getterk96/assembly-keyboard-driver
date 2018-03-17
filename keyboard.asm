Title           KeyboardInt9Interrupt

INCLUDE         Irvine16.inc

.data
PIC_CMD         EQU     20h
NONSPEC_EOI     EQU     20h

.code

                public  _keys
_keys           db      256 dup (0)

toASCII         db      0,27,'1234567890-=',0,9
				db      'qwertyuiop[]',10,0,'as'
				db      'dfghjkl;',39,96,0,92,'zxcv'
				db      'bnm,./',0,42,0,' ',0,0,0,0,0,0
				db      0,0,0,0,0,0,0,0,0,0,'-',0,0,0,'+',0
				db      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
				db      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
				db      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
				db      0,27,'!@#$%^&*()_+',0,9
				db      'QWERTYUIOP{}',13,0,'AS'
				db      'DFGHJKL:',34,126,0,124,'ZXCV'
				db      'BNM<>?',0,42,0,' ',0,0,0,0,0,0
				db      0,0,0,0,0,0,0,'789-456+1'
				db      '230.',0,0,0,0,0,0,0,0,0,0,0,0
				db      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
				db      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

buffer          db      64 DUP(0)
btail           db      0
pcursor         db      0
cword           db      7

_Set_New_Int9           PROC
				cli
				push    ds          	         ; save DS
				mov     ax, @code    	         ; initialize DS
				mov     ds, ax
				mov     ah, 25h      	         ; set interrupt vector
				mov     al, 09h      	         ; for interrupt 09h
				mov     dx, OFFSET New_Int9
				int     21h
				pop     ds                       ; restore DS
				sti
				ret
_Set_New_Int9           ENDP


New_Int9                PROC
				pushad
				in      al, 61h                  ; basic operations
				or      al, 80h
				out     61h, al
				and     al, 7fh
				out     61h, al

				in      al, 60h                  ; get the scancode
				mov     ah, al
				xor     bx, bx
				mov     bl, ah
				and     bl, 01111111b            ; get the keycode despite the press status

				rol     ah, 1
				jc      keyrelease

keypress:       mov     _keys[bx], 1             ; key pressed
				xor     ax, ax

				mov     ah, _keys[3ah]           ; get CapLock status
				mov     al, _keys[45h]           ; get NumLock status
				mov     bh, _keys[2ah]           ; get left shift status
				or      bh, _keys[36h]           ; get right shift status

checkword:      cmp     bl, 16                   ; check if the key is alphabet
				jc      checknum
				cmp     bl, 25
				jbe     Capslock
				cmp     bl, 30
				jc      checknum
				cmp     bl, 38
				jbe     Capslock
				cmp     bl, 44
				jc      checknum
				cmp     bl, 51
				jae     checknum
CapsLock:       xor     bh, ah

checknum:       cmp     bl, 71                   ; check if the key is number
				jc      forallkeys
				cmp     bl, 73
				jbe     NumLock
				cmp     bl, 75
				jc      forallkeys
				cmp     bl, 77
				jbe     NumLock
				cmp     bl, 79
				jc      forallkeys
				cmp     bl, 84
				jae     forallkeys
NumLock:        mov     bh, al

forallkeys:  	ror     bh, 1                    ; get the final key value
				add     bl, bh                   
				xor     bh, bh                   ; clear for index use
				mov     al, toASCII[bx]          ; get translated ascii value

checkfuntion:   cmp     al, 0                    ; check if the key is a function key
				jne     checkbuffer
				and     bl, 01111111b            ; reget the key value despite the press status
Function:       cmp     bx, 14                   ; check if the Backspace is pressed
				je      BackspaceK
				cmp     bx, 42                   ; check if the LShift is pressed
				je      int9_done  
				cmp     bx, 54                   ; check if the RShift is pressed
				je      int9_done  
				cmp     bx, 58                   ; check if the Caplock is pressed
				je      int9_done
				cmp     bx, 59                   ; check if the F1 is pressed
				je      F1
				cmp     bx, 60                   ; check if the F2 is pressed
				je      F2
				cmp     bx, 61                   ; check if the F3 is pressed
				je      F3
				cmp     bx, 62                   ; check if the F4 is pressed
				je      F4
				cmp     bx, 63                   ; check if the F5 is pressed
				je      F5
				cmp     bx, 64                   ; check if the F6 is pressed
				je      F6
				cmp     bx, 65                   ; check if the F7 is pressed
				je      F7
				cmp     bx, 66                   ; check if the F8 is pressed
				je      F8
				cmp     bx, 67                   ; check if the F9 is pressed
				je      F9
				cmp     bx, 68                   ; check if the F10 is pressed
				je      F10
				cmp     bx, 87                   ; check if the F11 is pressed
				je      F11
				cmp     bx, 88                   ; check if the F12 is pressed
				je      F12
				cmp     bx, 69                   ; check if the Numlock is pressed
				je      int9_done
				cmp     bx, 71                   ; check if the Home is pressed
				je      HomeK
				cmp     bx, 75                   ; check if the Toleft is pressed
				je      ToleftK
				cmp     bx, 77                   ; check if the Toright is pressed
				je      TorightK
				cmp     bx, 79                   ; check if the End is pressed
				jne     int9_done
				jmp     EndK

HomeK:          cmp     [pcursor], 0
                je      int9_done
                mov     bh, 0
				mov     ah, 03h
				int     10h
				sub     dl, [pcursor]
				mov     ah, 02h
				int     10h
				mov     [pcursor], 0
				jmp     int9_done

EndK:           mov     bh, 0
				mov     ah, 03h
				int     10h
				mov     bl, [btail]
				sub     bl, [pcursor]
				add     dl, bl
				mov     ah, 02h
				int     10h
				mov     dl, [btail]
				mov     [pcursor], dl
				jmp     int9_done

BackspaceK:     cmp     [pcursor], 0
				je      int9_done
                xor     cx, cx
                xor     bx, bx
				mov     cl, [btail]
				sub     cl, [pcursor]
				cmp     cl, 0
				je      back2
				mov     bl, [pcursor]
				dec     bl
back1:          inc     bl
                mov     dl, buffer[bx]
				dec     bl
				mov     buffer[bx], dl
				inc     bl
				loop    back1
back2:  		mov     bl, [btail]
				dec     bl
				mov     buffer[bx], 0
				dec     [pcursor]
				mov     bh, 0
				mov     ah, 03h
				int     10h
				dec     dl
				mov     ah, 02h
				int     10h

				mov     ax, @code                ; initialize ES
				mov     es, ax
				mov     ah, 13h
				xor     cx, cx
				mov     cl, [btail]
				sub     cl, [pcursor]
				push    ax
				xor     ax, ax
				mov     ax, OFFSET buffer
				add     al, [pcursor]
				mov     bp, ax
				pop     ax
				mov     al, 0
				mov     bl, [cword]
				int     10h
			    dec     [btail]
				jmp     int9_done

F1:             mov     [cword], 1
				jmp     int9_done
F2:             mov     [cword], 2
				jmp     int9_done
F3:             mov     [cword], 3
				jmp     int9_done
F4:             mov     [cword], 4
				jmp     int9_done
F5:             mov     [cword], 7
				jmp     int9_done
F6:             mov     di, 131
				mov     bx, 50
				jmp     playsound
F7:             mov     di, 147
				mov     bx, 50
				jmp     playsound
F8:             mov     di, 165
				mov     bx, 50
				jmp     playsound
F9:             mov     di, 175
				mov     bx, 50
				jmp     playsound
F10:            mov     di, 196
				mov     bx, 50
				jmp     playsound
F11:            mov     di, 220
				mov     bx, 50
				jmp     playsound
F12:            mov     di, 247
				mov     bx, 50

playsound:      mov     al, 0b6h
				out     43h, al
				mov     dx, 14h
				mov     ax, 4f38h
				div     di
				out     42h, al
				mov     al, ah
				out     42h, al
				in      al, 61h
				mov     ah, al
				or      al, 3
				out     61h, al
sound1:         mov     cx, 6801
sound2:         loop    sound2
				dec     bx
				jnz     sound1
				mov     al, ah
				out     61h, al
				jmp     int9_done

ToleftK:		cmp     [pcursor], 0
                je      int9_done
                mov     bh, 0
				mov     ah, 03h
				int     10h
				dec     dl
				mov     ah, 02h
				int     10h
				dec     [pcursor]
				jmp     int9_done

TorightK:		mov     bh, [btail]
                cmp     bh, [pcursor]
				je      int9_done
                mov     bh, 0
				mov     ah, 03h
				int     10h
				inc     dl
				mov     ah, 02h
				int     10h
				inc     [pcursor]
				jmp     int9_done

checkbuffer:    cmp     [btail], 62              ; if the buffer is full, then only the enter key can be accepted
				jne     moverallafter            ; else put the key into buffer
checkend:       cmp     al, 10                   ; check for Enter
				jne     int9_done
				
moverallafter:  xor     bx, bx                   ; move the rest keys in buffer if must
				mov     bl, [btail]
				sub     bl, [pcursor]
				cmp     bl, 0
				je      intobuffer
				mov     cx, bx
				mov     bl, [btail]
move1:          dec     bl
				mov     dl, buffer[bx]
				inc     bl
				mov     buffer[bx], dl
				dec     bl
				loop    move1

intobuffer:   	xor     bx, bx                   ; put into buffer
				mov     bl, [pcursor]
				mov     buffer[bx], al
				inc     bx
				inc     [btail]
				cmp     al, 10
				jne     print
				mov     buffer[bx], 13
				inc     bx
				mov     [btail], bl

print:      	mov     ax, @code                ; initialize ES
				mov     es, ax
				mov     bh, 0
				mov     ah, 03h
				int     10h
				mov     ah, 13h
				xor     cx, cx
				mov     cl, [btail]
				sub     cl, [pcursor]
				push    ax
				xor     ax, ax
				mov     ax, OFFSET buffer
				add     al, [pcursor]
				mov     bp, ax
				pop     ax
				mov     al, 0
				mov     bl, [cword]
				int     10h

				xor     bx, bx                    ; check if enter is pressed
				mov     bl, [btail]
				dec     bl
				cmp     buffer[bx], 13       
				jne     setcursor
				mov     [btail], 0                ; then clear the buffer
				mov     [pcursor], 0

setcursor:      mov     bh, 0                     ; set the cursor
				mov     ah, 03h
				int     10h
				cmp     buffer[bx], 13
				jne     set1
				inc     dh
				mov     dl, 0
				jmp     set2
set1:           inc     dl
				inc     [pcursor]
set2:           mov     ah, 02h
				int     10h
				jmp     checkrowfull

keyrelease:     mov     _keys[bx], 0              ; key released

checkrowfull:   mov     ah, 03h
				int     10h
				cmp     dh, 20
				jb      int9_done
				mov     ax, 03h
				int     10h
int9_done:      mov     al, NONSPEC_EOI
				out     PIC_CMD, al
				popad
				iret
New_Int9                ENDP

main                    PROC
				mov     ax, 03h
				int     10h
				call    _Set_New_Int9
				exit
main                    ENDP

END                     main
