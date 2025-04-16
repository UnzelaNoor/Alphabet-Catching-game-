[org 0x0100]
jmp start
fc: dw 0,0,0,0,0
message: db 'Press space bar key to start the game',0 
loser: db 'GAME OVER!',0
livesleft: db 0
position1: dw 6, 40, 80, 120, 150
rand: dw 0
randnum: dw 0
savedRow: dw 0
savedCol :dw 0 
score: dw 0
scorestr: db 'score:', 0
test1: db 0
oldisr: dd 0
number: db 0
x: dw 23 
y: dw 24
attribute: dw 0xDC
tick1: dw 0,0,0,0,0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;clearscreen;;;;;;;;;;;;;;;;;;;;;;;;;
clrscr:
mov ax, 0xb800
mov es,ax
mov di,0
nextt: 
mov word [es:di], 0x0720
add di,2
cmp di, 4000
jne nextt
ret
;;;;;;;;;;;;;;;;;;;;;;;fill a color on screen;;;;;;;;;;;;;;;;;;;;;;;;
fillscreen:
mov ax, 0xb800
mov es,ax
mov di,0
nexttt: 
mov word [es:di], 0x7120
add di,2
cmp di, 4000
jne nexttt
ret
;;;;;;;;;;;;;;;;;;;;;;;;;;clear command for the box;;;;;;;;;;;;;;;;;;
clear:  
push ax 
push es 
push di
mov ax, 0xb800 
mov es,ax 
mov di,3680 
l2: 
mov word [es:di], 0x0720
add di,2
cmp di, 3838
jne l2 
pop di 
pop es 
pop ax 
ret
;;;;;;;;;;;;;;;movement of bar(interrupt 9h);;;;;;;;;;;;;;;;;
kbisr:
pusha
 push word[x] 
 push word [y] 
 push word[attribute] 
 mov ax, 6
 push ax  
 call drawBar
in al, 0x60
 cmp al, 0x4b
 je left
cmp al, 0x4d
 je right 
jmp nomatch

 left: 
 mov al, 0x20
 out 0x20,al
 
 call clear  
 sub word[y],2   
 push word[x] 
 push word [y] 
 push word[attribute] 
 mov ax, 6
 push ax  
 call drawBar
 jmp end 

 right:
 mov al, 0x20
 out 0x20,al
 call clear
 add word[y],2   
 push word[x] 
 push word [y] 
 push word[attribute] 
 mov ax, 6
 push ax  
 call drawBar
 jmp end 

 nomatch:
 popa
 jmp far [cs:oldisr]
 
 end: 
 mov al, 0x20
 out 0x20,al
popa
 iret
;;;;;;;;;;;;;;;;;;;;;;;draw box at bottom;;;;;;;;;;;;;;;;;;;;;;;;
drawBar:
    push bp
    mov bp, sp
    push es
    push ax
    push bx
    push cx
    push si
    push di  
    ; Calculate di within the specified range [3680, 3838]
    mov ax, 0xb800
    mov es, ax
    mov al, 80
    mul byte [bp+10]
    add ax, [bp+8]
    shl ax, 1
    mov di, ax
    mov ah, 0x01
    mov al, [bp+6]
    mov cx, [bp+4]
    bar1: 
 cmp di, 3680
 jb skipDrawing 
 cmp di, 3838
 jae skipDrawing
        mov word [es:di], ax
        add di, 2 
        loop bar1
    skipDrawing:
    pop di
    pop si
    pop cx
    pop bx
    pop ax
    pop bp
    pop es
    ret 8

;;;;;;;;;;;;;;;;;;;start and end prompts;;;;;;;;;;;;;;;;;
printstr:  
 push bp
mov bp, sp
push es
push ax
push cx
push si
push di
push ds
mov ax, [bp+4]
push ax 
call strlen 
cmp ax, 0 
jz exit 
mov cx, ax 
mov ax, 0xb800
mov es, ax 
mov al, 80 
mul byte [bp+8] 
add ax, [bp+10] 
shl ax, 1 
mov di,ax 
mov si, [bp+4] 
mov ah, [bp+6] 
cld 
nextchar: lodsb 
stosw 
loop nextchar 
exit: 
pop ds
pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 8   

strlen: 
push bp 
mov bp,sp 
push es 
push cx 
push di 
 
les di,[bp+4]  
mov cx,0xffff 
xor al,al 
repne scasb 
mov ax, 0xffff 
sub ax, cx  
dec ax 
pop di
pop cx
pop es
pop bp
ret 2 
;;;;;;;;;;;;;;;falling alphabets (interrupt 8h);;;;;;;;;;;;;;;;;
timer:
push ax
push di
push es
inc word [cs:tick1]
inc word [cs:tick1+2]
inc word [cs:tick1+4]
inc word [cs:tick1+6]
inc word [cs:tick1+8]


cmp word [cs:tick1],3
jne col1
mov word [cs:tick1], 0
sub sp,2
push word 6
push word [cs:fc]
push word [cs:position1]
call suffer
pop word [cs:position1]

col1:
cmp word [cs:tick1+2],4
jne col3
mov word [cs:tick1+2], 0
sub sp,2
push word 40
push word [cs:fc+2]
push word [cs:position1+2]
call suffer
pop word [cs:position1+2]

col3:
cmp word [cs:tick1+4], 1
jne col4
mov word [cs:tick1+4], 0
sub sp,2
push word 80
push word [cs:fc+4]
push word [cs:position1+4]
call suffer
pop word [cs:position1+4]

col4:
cmp word [cs:tick1+6],2
jne col5
mov word [cs:tick1+6], 0
sub sp,2
push word 120
push word [cs:fc+6]
push word [cs:position1+6]
call suffer
pop word [cs:position1+6]

col5:
cmp word [cs:tick1+8],5
jne exittimer
mov word [cs:tick1+8], 0
sub sp,2
push word 150
push word [cs:fc+8]
push word [cs:position1+8]
call suffer
pop word [cs:position1+8]

exittimer:
mov al, 0x20
out 0x20,al
pop es
pop di
pop ax
iret

;;;;;;;;;;;;;;make em fall;;;;;;;;;;;;;;;;;;;;;;
suffer:
push bp
mov bp, sp
pusha
mov bx, [bp+8]
mov ax, 0xb800
mov es,ax
mov di, [bp+4]
cmp di, 3680
jae checkforbar
nextb:
mov al, [bp+6]
mov ah, 0x07
mov word [es:di], ax
mov word [es:di-160] , 0x0720
add di,160 
mov [bp+10], di 
doagain: 
popa 
pop bp
ret 6

reachedlastline:
mov word [es:di], 0x0720
call deduct
mov word [es:di-160], 0x0720
mov di, bx 
mov [bp+10], di 

jmp doagain

checkforbar:
cmp di, 3840
jae reachedlastline
cmp word [es:di], 0x01DC
je barpresent
jmp nextb
barpresent:
mov word [es:di], 0x01DC
add word [score], 10
mov word [es:di-160], 0x0720
mov di, bx
mov [bp+10], di 
jmp doagain
;;;;;;;;;;;;;;;check for lives to end game;;;;;;;;;;;;;;;
deduct:
inc byte [cs:livesleft]
cmp byte [cs:livesleft], 10
jne returnback
call endgame
returnback:
ret
;;;;;;;;;;;;;;;;;;;ending prmopt;;;;;;;;;;;;;;;
endgame:
call fillscreen
mov ax, 34
push ax 
mov ax, 12
push ax
mov ax, 0x71 
push ax 
mov ax, loser
push ax 
call printstr
mov ax, 8
push ax 
mov ax, 18
push ax
mov ax, 0x71
push ax 
mov ax, scorestr
push ax 
call printstr
push word [score]
call calculatescore
jmp terminate
;;;;;;;;;;;;;;;;;;;;;;;;random alphabet generator;;;;;;;;;;;;;;;;;
; taking n as parameter, generate random number from 0 to n nad return in the stack
randG:
   push bp
   mov bp, sp
   pusha
   cmp word [rand], 0
   jne next2

  MOV     AH, 00h   ; interrupt to get system timer in CX:DX 
  INT     1AH
  inc word [rand]
  mov     [randnum], dx
 ; mov [es:di], dx
  jmp next1

  next2:
  mov     ax, 25173          ; LCG Multiplier
  mul     word  [randnum]     ; DX:AX = LCG multiplier * seed
  add     ax, 13849          ; Add LCG increment value
  ; Modulo 65536, AX = (multiplier*seed+increment) mod 65536
  mov     [randnum], ax          ; Update seed = return value

 next1:xor dx, dx
 mov ax, [randnum]
 mov cx, [bp+4]
 inc cx
 div cx
 
 mov [bp+6], dx
 popa
 pop bp
 ret 2
 
;;;;;;;;;;;;;;;;score prompt generator;;;;;;;;;;;;;;;;;;;;;
calculatescore:
 push bp
 mov bp, sp
 pusha
 
 mov ax, 0xb800
 mov es, ax ; point es to video base
 mov ax, [bp+4] ; load number in ax
 mov bx, 10 ; use base 10 for division
 mov cx, 0 ; initialize count of digits
 mov di,0
 
nextdigit1:
 mov dx, 0 ; zero upper half of dividend
 div bx ; divide by 10
 add dl, 0x30 ; convert digit into ascii value
mov [test1+di],dl
add di,1
 inc cx ; increment count of values
 cmp ax, 0 ; is the quotient zero
 jnz nextdigit1 ; if no divide it again
 
 mov cx,di
 mov si,0
 dec di
 
 reverse:
 mov al,[test1+di]
 mov [number+si],al
 inc si
 dec di
 cmp di,-1
 jne reverse
 
 mov si,0x1160
 
 mov ah, 0x13		; service 13 - print string
     
		mov bp, number ;offset of string
		mov al, 1			
		mov bh, 0			; output on page 0
		
		mov bl, 0x71
		mov dx, si; row 10 column 3
		
		;es:bp = ds:message
		push ds
		pop es	
		; es=ds segment of string
		INT 0x10
 endscore:
 popa
 pop bp
 ret 2 
;;;;;;;;;;;;;;;;;;starting prompt;;;;;;;;;;;;;;;;;;
startgame:
	call fillscreen
	mov ax, 25
push ax 
mov ax, 13
push ax
mov ax, 0x71 
push ax 
mov ax, message
push ax 
call printstr
r1:  
mov ah,0 
int 0x16  
cmp ah, 0x39

je p  
jmp r1  
p: 
call clrscr
ret
;;;;;;;;;;;;;;;;;;main program;;;;;;;;;;;;;;;;;;;;;;;;;
start:
call startgame
xor ax,ax
mov es, ax
; Save the cursor position
   mov ah, 03h ; Function to get cursor position
   int 10h
   mov [savedRow], dh
   mov [savedCol], dl
   mov cl,0
  ; Set cursor position
  mov ah, 02h ; Function to set cursor position
  mov dh, 26   ; Row
  mov dl, 1; Column
    int 10h

 printAlphabet:
    ; Generate and print the first random number
    sub sp, 2
    push 25
    call randG
    pop dx
    add dx, 'A' ; Convert the number to ASCII of alphabet
	cmp dx, 'Z'
	jg printAlphabet
	mov [cs:fc], dx
	 printAlphabet1:
    ; Generate and print the first random number
    sub sp, 2
    push 25
    call randG
    pop dx
    add dx, 'A' ; Convert the number to ASCII of alphabet
	cmp dx, 'Z'
	jg printAlphabet1
	mov [cs:fc+2], dx
	 printAlphabet2:
    ; Generate and print the first random number
    sub sp, 2
    push 25
    call randG
    pop dx
    add dx, 'A' ; Convert the number to ASCII of alphabet
	cmp dx, 'Z'
	jg printAlphabet2
	mov [cs:fc+4], dx
	 printAlphabet3:
    ; Generate and print the first random number
    sub sp, 2
    push 25
    call randG
    pop dx
    add dx, 'A' ; Convert the number to ASCII of alphabet
	cmp dx, 'Z'
	jg printAlphabet3
	mov [cs:fc+6], dx
	 printAlphabet4:
    ; Generate and print the first random number
    sub sp, 2
    push 25
    call randG
    pop dx
    add dx, 'A' ; Convert the number to ASCII of alphabet
	cmp dx, 'Z'
	jg printAlphabet4
	mov [cs:fc+8], dx

	xor ax,ax
	mov es,ax
cli
mov ax, [es:9*4]
mov [oldisr], ax
mov ax, [es:9*4+2]
mov [oldisr+2], ax
sti
cli
mov word [es: 9*4], kbisr
mov word [es:9*4+2], cs
mov word [es: 8*4], timer
mov [es: 8*4+2], cs
sti



 loop1:
 jmp loop1

	
terminate:
	xor ax,ax
	mov es,ax
mov ax,[oldisr]
mov [es:9*4],ax

mov ax,[oldisr+2]
mov [es:9*4+2],ax
 ; Restore the cursor position
    mov ah, 02h ; Function to set cursor position
    mov dh, [savedRow]
    mov dl, [savedCol]
    int 10h
	 mov ah, 4Ch  ; DOS function: terminate program
    xor al, al   ; Return code 0
    int 21h      ; Call DOS interrupt