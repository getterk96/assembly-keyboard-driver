TITLE Mouse Driver Under DOSbox (mouse.asm)

.486
code segment use16
	assume cs:code
	org 100h

begin:
	mov ax, cs
	mov ds, ax
	mov es, ax

	; 清屏
	mov al, 0
	mov bh, 7
	mov ch, 0
	mov cl, 0
	mov dh, 24
	mov dl, 79
	mov ah, 6
	int 10h

	; 设置光标位置到左上角
	mov bh, 0
	mov dh, 0
	mov dl, 0
	mov ah, 2
	int 10h

    ; 设为640*480 16色模式
	mov ax, 0012h
	int 10h

	; 显示欢迎文字
	mov bp, OFFSET GreetMessage
	mov cx, GreetMessagelen
	mov dh, 0
	mov dl, 0
	mov bh, 0
	mov al, 0
	mov bl, 7
	mov ah, 13h
	int 10h

	; 画虚拟鼠标
	pusha

	mov linecolor, 0fh		;白色边框

	mov lineend1x, 240
	mov lineend1y, 150
	mov lineend2x, 400
	mov lineend2y, 150
	call drawhorline

	mov lineend1x, 400
	mov lineend1y, 150
	mov lineend2x, 400
	mov lineend2y, 450
	call drawverline

	mov lineend1x, 240
	mov lineend1y, 450
	mov lineend2x, 400
	mov lineend2y, 450
	call drawhorline

	mov lineend1x, 240
	mov lineend1y, 150
	mov lineend2x, 240
	mov lineend2y, 450
	call drawverline

	mov rectcolor, 0fh		;白色矩形
	call drawleftbutton
	call drawrightbutton
	popa

	mov ax, 0c201h
	int 15h

	;handler 程序是 ps/2鼠标的 中断处理程序
	;当安装这个handler后，即使此程序退出，handler程序仍然可发挥作用
	mov ax, seg handler
	mov es, ax
	mov ax, 0c207h         ; c207功能是安装handler的功能
	mov bx, OFFSET handler
	int 15h

	jc err_1               ; 如果 CF=1, c207功能调用失败
	jmp c1

err_1:
    jmp error_end

c1:
    mov ax, 0c200h         ; c200功能是打开ps/2鼠标
	mov bh, 1
	int 15h
	jc err_2               ; 如果CF=1,则c200功能调用失败
	jmp c2

err_2:
    jmp error_end

;使用新中断
    cli
    push ds
    mov ax, cs
    mov ds, ax
    mov ax, 2533h           ; 重写33号中断
    mov dx, OFFSET New33h
    int 21h
    pop ds
    sti

c2:
    call save_mouse

; 检测是否按下Esc
scan1:
    mov ah, 11h
	int 16h
	jz scan1       ; 没有按键
    mov ah, 10h
    int 16h
    cmp al, 1Bh    ; 不是Esc不退出 1Bh: Esc key
    jne scan1

	;清理安装的handler
	mov ax, seg nullhandler
	mov es, ax
	mov bx, OFFSET nullhandler
	mov ax, 0c207h
	int 15h

	mov ax, 0c201h
	int 15h

	mov bx, 0
	mov dx, 0
	mov ax, 4f05h
	int 10h

	jmp return

error_end:
	call showerror

return:
	mov al, 02
	mov ah, 0
	int 10h
	mov ah, 4ch
	int 21h

New33h PROC
    .if ax == 00h
        jmp init
    .endif
    .if ax == 01h
        jmp show
    .endif
    .if ax == 02h
        jmp hide
    .endif
    .if ax == 03h
        jmp get_pos
    .endif
    .if ax == 04h
        jmp set_pos
    .endif
    .if ax == 01h
        jmp get_click
    .endif
    iret

init:
    mov ax, 0ffffh
    mov bx, 2
    iret

show:
    call show_mouse
    iret

hide:
    iret

get_pos:
    mov cx, posX
    mov dx, posY
    iret

set_pos:
    mov posX, cx
    mov posY, dx
    call show_mouse
    iret

get_click:
    .if bx == 0
        jmp lbc     ;左键
    .endif
    .if bx == 1
        jmp rbc     ;右键
    .endif
    iret
mov bx, 1   ;不支持双击检查，按键次数为1
lbc:
    mov cx, lbcX
    mov dx, lbcY
    iret
rbc:
    mov cx, rbcX
    mov dx, rbcY
    iret
New33h ENDP

nullhandler PROC far
	ret
nullhandler ENDP

;系统调用功能system call this function
;鼠标的状态放到函数的堆栈SP+10处
;X轴方向的移动量放到堆栈的SP+8处
;Y轴方向的移动量放到堆栈的SP+6处
handler PROC far
	push cs
	pop ds

	mov ax, ss
	mov es, ax
	mov bx, sp

	add bx, 6			;push SP+6
	mov dx, es:[bx]
	push dx
	add bx, 2			;push SP+8
	mov dx, es:[bx]
	push dx
	add bx, 2			;push SP+10
	mov dx, es:[bx]
	push dx

first:
    pop dx   			;dx = SP+10, status
	mov STATE, dl

	;保存老的鼠标的x, y坐标信息
	mov ax, posX
	mov posXold, ax
	mov ax, posY
	mov posYold, ax

;左键
	test dx, 1
	jnz ld         	     ;按下左键
	mov rectcolor, 0fh   ;没有按下左键按钮变白
	call drawleftbutton
	jmp n
ld:
    mov ax, posX
    mov lbcX, ax
    mov ax, posY
    mov lbcY, ax
	mov rectcolor, 03h    ;左键变色
	call drawleftbutton
;右键
n:
	test dx, 2
	jnz rd                ;按下右键
	mov rectcolor, 0fh    ;右键没有按下按钮变白
	call drawrightbutton
	jmp second
rd:
    mov ax, posX
    mov rbcX, ax
    mov ax, posY
    mov rbcY, ax
	mov rectcolor, 02h     ;右键变色
	call drawrightbutton

;X位移处理过程
second:
	pop dx  			 ;dx = SP+8, xmove
	cmp dx, 0
	jnz movedx           ;有x方向的移动
	jmp third
movedx:
	test STATE, 10h      	;00010000 Bit 4 = 1 , x negative
	jnz  xnegative
	add posX, dx

	;防止鼠标移出屏幕
	cmp posX, 639
	jnb big640
	jmp third
big640:
	mov posX, 639
	jmp third
xnegative:
	neg dl                 ;负方向相反数
	sub posX, dx
	cmp posX, 0            ;设置最小为 0
	jl xless0
	jmp third
xless0:
	mov posX, 0
	jmp third

;Y位移处理过程
third:
	pop dx   				;dx = SP+6, ymove
	cmp dx, 0
	jnz movedy
	jmp complete

movedy:
	test STATE, 20h    		;00100000 Bit 5 = 1, y negative
	jnz ynegative

	;如果非负就直接
	sub posY, dx
	cmp posY, 0
	jl yless0
	jmp complete
yless0:
	mov posY, 0
	jmp complete

ynegative:
	neg dl
	add posY, dx
	cmp posY, 479
	jnb big480
	jmp complete
big480:
	mov posY, 479
	jmp complete

complete:
	push cs
	pop ax
	mov es, ax
	mov ds, ax
	mov si, OFFSET savenew
	mov di, OFFSET saveold
	mov cx, mousetypelen*2
	cld
	rep movsb

	;使用saveold恢复原屏幕值
	call restore

	;保存新鼠标位置的屏幕值到savenew缓冲中
	call save_mouse

showms:
	call show_mouse                 ;画鼠标

	;显示鼠标坐标信息
	pusha
	mov ax, posX
	mov bx, OFFSET showbuffer1
	call btoasc
	mov ax, posY
	mov bx, OFFSET showbuffer2
	call btoasc
	mov ax, seg showbuffer1
	mov es, ax
	mov bp, OFFSET show1
	mov cx, 25
	mov dh, 6      ;行
	mov dl, 23     ;列
	mov bh, 0
	mov al, 0
	mov bl, 7
	mov ah, 13h
	int 10h
	mov bp, OFFSET show2
	mov cx, 9
	mov dh, 6      ;行
	mov dl, 49     ;列
	mov bh, 0
	mov al, 0
	mov bl, 7
	mov ah, 13h
	int 10h
	popa

over:   ret
handler ENDP

;恢复老鼠标位置屏幕
restore PROC far
	pushad
	mov ebx, 0
	mov edx, 0
	mov ecx, 0
	mov eax, 0

	mov ax, mousetypelen
	shr ax, 1                ;count/2 (WORD)
	mov mousetypecounter, ax ;mousetypecounter = mousetypelen/2
	mov di, 0
	mov si, 0

restorepixel:
	mov cx, posXold          ;将前一个鼠标位置x坐标放到cx中
	mov ax, mousetype[di]    ;将鼠标中的一个点放到ax中
	push ax
	shr ax, 8
	and ax, 0fh
	add cx, ax               ;cx存储x坐标
	pop ax
	mov dx, posYold
	and ax, 0fh
	add dx, ax               ;dx存储y坐标

    mov ah, 0ch
    mov al, saveold[si]
    mov bh, 0
    int 10h
	inc si

CT:
    add di, 2                   ;next word
	dec mousetypecounter
	jnz restorepixel
	popad
	ret

restore ENDP

;保存当前鼠标位置的屏幕内容
save_mouse PROC far
	pushad
	mov ebx, 0
	mov edx, 0
	mov ecx, 0
	mov eax, 0
	mov ax, mousetypelen
	shr ax, 1
	mov mousetypecounter, ax
	mov di, 0
	mov si, 0
savepixel:
	mov cx, posX
	mov ax, mousetype[di]
	push ax
	shr ax, 8
	and ax, 0fh
	add cx, ax             ;cx存储x坐标
	pop ax
	mov dx, posY
	and ax, 0fh
	add dx, ax             ;dx存储y坐标
    mov ah, 0dh
    mov bh, 0
    int 10h
    mov savenew[si], al
	inc si

    add di, 2               ;next word
	dec mousetypecounter
	jnz savepixel
	popad
	ret
save_mouse ENDP

;功能 :显示鼠标
show_mouse PROC far
	pushad
	mov eax, 0
	mov ebx, 0
	mov ecx, 0
	mov edx, 0

	mov ax, mousetypelen
	shr ax, 1
	mov mousetypecounter, ax
	mov di, 0
	mov si, 0

lodrmos:
	mov cx, posX
	mov ax, mousetype[di]
	push ax
	shr ax, 8
	and ax, 0fh
	add cx, ax             ;cx存储x坐标

	pop ax
	mov dx, posY
	and ax, 0fh
	add dx, ax             ;dx存储y坐标

	cmp bx, 639
	jg next                ;超过边框不用画
	jmp notexceed

notexceed:
	mov ah, 0ch
    mov al, mousecolor[si]
    mov bh, 0
    int 10h
	inc si

next:
    add di, 2               ;next word
	dec mousetypecounter
	jnz lodrmos
	popad
	ret
show_mouse ENDP

;画水平线
drawhorline PROC far
	pusha
	mov cx, lineend2x
	sub cx, lineend1x
	inc cx              ;循环次数
    mov bx, lineend1x   ;bx暂存posx
    mov dx, lineend1y

horlinedraw:
    push cx
    mov cx, bx
    mov ah, 0ch
    mov al, linecolor
    mov bh, 0
    int 10h
    inc cx
    mov bx, cx
    pop cx
	loop horlinedraw
	popa
	ret
drawhorline ENDP

;画竖直线
drawverline PROC far
    pusha
    mov cx, lineend2y
    sub cx, lineend1y
    inc cx              ;循环次数
    mov bx, lineend1x   ;暂存x
    mov dx, lineend1y

verlinedraw:
    push cx
    mov cx, bx
    mov ah, 0ch
    mov al, linecolor
    mov bh, 0
    int 10h
    inc dx
    mov bx, cx
    pop cx
    loop verlinedraw
    popa
    ret
drawverline ENDP

;画矩形
drawrect PROC far
	pusha
	mov ax, rectend1x
	mov lineend1x, ax
	mov ax, rectend2x
	mov lineend2x, ax
	mov cx, rectend2y
	sub cx, rectend1y
	inc cx
	mov al, rectcolor
	mov linecolor, al
	mov ax, rectend1y
	mov lineend1y, ax

rectdraw:
	call drawhorline
	inc lineend1y
	loop rectdraw
	popa
	ret
drawrect ENDP

;画左键
drawleftbutton PROC far
	pusha
	mov rectend1x, 250
	mov rectend1y, 160
	mov rectend2x, 310
	mov rectend2y, 260
	call drawrect
	popa
	ret
drawleftbutton ENDP

;画右键
drawrightbutton PROC far
	pusha
	mov rectend1x, 330
	mov rectend1y, 160
	mov rectend2x, 390
	mov rectend2y, 260
	call drawrect
	popa
	ret
drawrightbutton ENDP

btoasc PROC far
	mov si, 5
	mov cx, 10
btoasc1:
	xor dx, dx
	div cx
	add dl, 30h
	dec si
	mov [bx][si], dl
	or si, si
	jnz btoasc1
	ret
btoasc ENDP

showerror PROC far
	cmp ah, 01
	je e1
	cmp ah, 02
	je e2
	cmp ah, 03
	je e3
	cmp ah, 04
	je e4
	cmp ah, 05
	je e5
	jmp unknow
e1:
	mov bp, OFFSET c200error1
	mov cx, c200error1len
	jmp show
e2:
	mov bp, OFFSET c200error2
	mov cx, c200error2len
	jmp show
e3:
	mov bp, OFFSET c200error3
	mov cx, c200error3len
	jmp show
e4:
	mov bp, OFFSET c200error4
	mov cx, c200error4len
	jmp show
e5:
	mov bp, OFFSET c200error5
	mov cx, c200error5len
	jmp show
unknow:
	mov bp, OFFSET c200errorunknow
	mov cx, c200errunlen
    jmp show

show:
	mov dh, 5
	mov dl, 0
	mov bh, 0
	mov al, 0
	mov bl, 7
	mov ah, 13h
	int 10h
	ret
showerror ENDP

;鼠标形状
mousetype  WORD      0000h,0001h,0002h,0003h,0004h,0005h,0006h,0007h
		   WORD      0008h,0009h,000ah,000bh,000ch
	       WORD      0101h,0202h,0303h,0404h,0505h,0606h,0707h,0808h,0909h
	       WORD      010bh,020ah,0309h,0409h,0509h,0609h,0709h,0809h
	       WORD      030bh,030ch,040dh,040eh,050fh
	       WORD      060ah,060bh,070ch,070dh,080eh
	       WORD      0610h,0710h,0810h
	       WORD      0102h,0103h,0104h,0105h,0106h,0107h,0108h,0109h,010ah
		   WORD      0203h,0204h,0205h,0206h,0207h,0208h,0209h
		   WORD      0304h,0305h,0306h,0307h,0308h
		   WORD      0405h,0406h,0407h,0408h
		   WORD      0506h,0507h,0508h
		   WORD      0607h,0608h
		   WORD      0708h
		   WORD      030ah,040ah,050ah
		   WORD      040bh,050bh
		   WORD      040ch,050ch,060ch
		   WORD      050dh,060dh
		   WORD      050eh,060eh,070eh
		   WORD      060fh,070fh,080fh

mousetypelen        EQU $-mousetype
mousetypecounter    WORD ?
saveold             BYTE mousetypelen*2 dup (0h)    ;保存鼠标位置的屏幕图象
savenew             BYTE mousetypelen*2 dup (0h)    ;用于和saveold交替使用
mousecolor          BYTE 43 dup (0), 47 dup(0fh)    ;鼠标颜色
STATE               BYTE ?                          ;鼠标状态
posX                WORD 0                          ;鼠标左上角的x
posY                WORD 0                          ;鼠标左上角的y
lbcX                WORD 0                          ;左键按下的x
lbcY                WORD 0                          ;左键按下的y
rbcX                WORD 0                          ;右键按下的x
rbcY                WORD 0                          ;右键按下的y
posXold             WORD 0                          ;前次的鼠标x
posYold             WORD 0                          ;前次的鼠标y

GreetMessage    BYTE " This is a simple mouse driver display.", 0dh, 0ah, 0
                BYTE "You can move or click the mouse, then watch.", 0dh, 0ah, 0
                BYTE "Press Esc if you want to quit.", 0
GreetMessagelen EQU $-GreetMessage
show1           BYTE 'Mouse position: x = '
showbuffer1     BYTE 0,0,0,0,0
show2           BYTE 'y = '
showbuffer2     BYTE 0,0,0,0,0
c200error1      BYTE 'Invalid function'
c200error1len   EQU $-c200error1
c200error2      BYTE 'Invalid input'
c200error2len   EQU $-c200error2
c200error3      BYTE 'Interface error'
c200error3len   EQU $-c200error3
c200error4      BYTE 'need to resend'
c200error4len   EQU $-c200error4
c200error5      BYTE 'no device handler installed'
c200error5len   EQU $-c200error5
c200errorunknow BYTE 'Unknow reason wrong!'
c200errunlen    EQU $-c200errorunknow

lineend1x WORD 0
lineend1y WORD 0
lineend2x WORD 0
lineend2y WORD 0
rectend1x WORD 0
rectend1y WORD 0
rectend2x WORD 0
rectend2y WORD 0
linecolor BYTE 0fh
rectcolor BYTE 0fh

code ends
end begin