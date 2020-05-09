.model small
.stack 100h
.data

controlsMessage             db "                                Controls:",0Dh ,0Ah
                            db "               Use Left and Right Arrows to move paddle",0Dh ,0Ah
                            db "               Press Enter to start game",0Dh ,0Ah
                            db "               Press Esc to Exit",0Dh ,0Ah
                            db "               Press Enter to skip this tutorial",0Dh ,0Ah,'$'
                          
               
messageWin                  db "                                     YOU WIN                                      ",0Dh ,0Ah,'$'




screenBuffer                db 2000 dup(0)                                        ; screen buffer for 80x25

playField                   db 792 dup (00h)                                      ; ammount of elements in level


level                       db 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
                            db 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
                            db 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
                            db 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00 
                            db 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
                            db 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00 
                            db 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
                            db 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
                            db 00, 00, 50, 50, 00, 00, 50, 50, 00, 00, 50, 50, 00, 00, 50, 50, 00, 00, 50, 50, 00, 00, 50, 50, 00, 00, 50, 50, 00, 00, 50, 50, 00, 00, 50, 50, 00, 00, 50, 50, 00, 00, 50, 50  
                            db 50, 50, 00, 00, 50, 50, 00, 00, 50, 50, 00, 00, 50, 50, 00, 00, 50, 50, 00, 00, 50, 50, 00, 00, 50, 50, 00, 00, 50, 50, 00, 00, 50, 50, 00, 00, 50, 50, 00, 00, 50, 50, 00, 00 
                            ;db 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
                            ;db 50, 50, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00                            
                            db 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
                            db 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
                            db 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00 
                            db 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
                            db 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
                            db 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00 
                            db 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
                            db 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00 
                            db 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00 

levelCount                  dw 22 ;22                                                          ;platforms
                         

levelsAmmount               dw 1 
currentLevel                dw 0     

verticalMovement            dw 0
horizontalMovement          dw 0
                  
ballPositionY               dw 18
ballPositionX               dw 11
                   
paddlePosition              dw 0                   
previousTime                dw 0    
score                       dw 0                       
winCount                    dw 0

.code

    
initScreen proc near
    push cx
    push ax
    push si
    push ds  
    push bx
    push dx 
        
    mov ax, 0b800h
    mov ds, ax 
        
    xor bx, bx
    mov cx, 2000
    loopScreen:                           ; clear screen
        mov [bx], ' '
        inc bx
        mov [bx], 07h
        inc bx
    loop loopScreen
    
    xor si, si
    mov ax, 80                           ;80 upper line
    firstLine:                           ;print upper red line
        mov [si], ' '
        inc si
        mov [si], 40h
        inc si
        dec ax
        cmp ax, 0
        je firstLineEnd
    jmp firstLine
    firstLineEnd:
        mov ax, 23                      ;two borders heights
        columns:                        ;print red columns 
            mov [si], ' '
            inc si
            mov [si], 40h
            inc si
            add si, 156                 ;80(each line)*2(symbols)-4
            mov [si], ' '
            inc si
            mov [si], 40h
            inc si
            dec ax
            cmp ax, 0
        je columnsEnd
        jmp columns
    columnsEnd:
    mov ax, 80                          
    secondLine:                         ;bottom
        mov [si], ' '
        inc si
        mov [si], 40h
        inc si
        dec ax
        cmp ax, 0
        je secondLineEnd
    jmp secondLine
    secondLineEnd:
    mov cx, 2
    glass:                             ; print board of game
        mov al, 160                    
        mul cl
        add ax, 4
        mov si, ax
        mov [si], ' '
        inc si
        mov [si], 70h
        inc si
        add si, 88                     ;board width
        mov [si], ' '
        inc si
        mov [si], 70h
        inc cx
        cmp cx, 23                    ;cx = 2, 21 rows + 2 = 23 
    je glassEnd
    jmp glass
    glassEnd:                         ;upper white line
        mov cx, 2
        glassBottom:
            mov al, 2
            mul cl
            add ax, 320              ;two lines 160*2
            mov si, ax
            mov [si], ' '
            inc si
            mov [si], 70h
            inc cx
            cmp cx, 48               ;44 field size + 2
            je glassBottomEnd
        jmp glassBottom
    glassBottomEnd:
        mov [908], 'S'               
        mov [910], 'c'
        mov [912], 'o'
        mov [914], 'r'
        mov [916], 'e'
        mov [918], ':'
        xor bh, bh                     ; video page
        mov dh, 25                     ; off cursor
        mov ah, 02
        int 10h  
        pop dx
        pop bx
        pop ds
        pop si
        pop ax
        pop cx
    ret
endp
           
printScore proc
    pusha
    xor cx, cx    
    mov ax, score
    xor dx, dx 
    mov si, 10
    loadStack:   
        div si 			  		
        add dl, '0'
        push dx    
        xor dx, dx 
        inc cx        
        cmp ax, 0
        jne loadStack   
        mov bx, 920                 ;position of numbers(look printScreen)
    printStack:
        pop dx 
        push ds
        mov ax, 0b800h
        mov ds, ax
        mov [bx], dl
        inc bx
        mov [bx], 02h              ;light green
        inc bx
        pop ds           
    loop printStack          
    popa 
    ret   
endp                
            
initPlayField proc near
    push cx
    push bx
    push ax  
    push es
    push di
    push si
      
    mov ax, ds
    mov es, ax
    mov si, offset level
    mov di, offset playField  
    mov cx, 792                            ;playfield size (18 rows and 44 symbols)
    loop11:
        rep movsb                          ;fullfil level
    mov bx, offset levelCount
    mov ax, [bx]
    mov winCount, ax                       ;set ammount of blocks to win
    pop si
    pop di
    pop es 
    pop ax
    pop bx
    pop cx
    ret
endp

displayPlayField proc near
    push ax
    push es
    push cx
    push di
    push si
    mov ax, 0B800h
    mov es, ax                
    mov cx, 19
    mov di, 487                ;set address ES:DI (487)
    mov si, offset playField   ;set address DS:SI
    loop1:
        push cx
        mov cx, 44             
        loop2:
            movsb              ;sent string element by element, from DS:SI to ES:DI
            inc di
        loop loop2
        add di, 72             ;right shift
        pop cx
    loop loop1
    pop si
    pop di
    pop cx
    pop es
    pop ax
    ret
endp
        

printLose proc near    
    push ds
    mov ax, 0b800h
    mov ds, ax
    mov bx, 1642                                 
    mov [bx], 'Y'
    inc bx
    mov [bx], 07h
    inc bx
    mov [bx], 'o'
    inc bx
    mov [bx], 07h
    inc bx
    mov [bx], 'u'
    inc bx
    mov [bx], 07h
    inc bx
    mov [bx], ' '
    inc bx
    mov [bx], 07h
    inc bx
    mov [bx], 'l'
    inc bx
    mov [bx], 07h
    inc bx
    mov [bx], 'o'
    inc bx
    mov [bx], 07h
    inc bx
    mov [bx], 's'
    inc bx
    mov [bx], 07h
    inc bx
    mov [bx], 'e'
    inc bx
    mov [bx], 07h
    inc bx
    mov bx, 1798   
    mov [bx], 'P'
    inc bx
    mov [bx], 07h
    inc bx
    mov [bx], 'r'
    inc bx
    mov [bx], 07h
    inc bx        
    mov [bx], 'e'
    inc bx
    mov [bx], 07h
    inc bx
    mov [bx], 's'
    inc bx
    mov [bx], 07h
    inc bx
    mov [bx], 's'
    inc bx
    mov [bx], 07h
    inc bx
    mov [bx], ' '
    inc bx
    mov [bx], 07h
    inc bx
    mov [bx], 'E'
    inc bx
    mov [bx], 07h
    inc bx
    mov [bx], 'n'
    inc bx
    mov [bx], 07h
    inc bx
    mov [bx], 't'
    inc bx
    mov [bx], 07h
    inc bx
    mov [bx], 'e'
    inc bx
    mov [bx], 07h
    inc bx
    mov [bx], 'r'
    inc bx
    mov [bx], 07h
    inc bx 
    mov [bx], ' '
    inc bx
    mov [bx], 07h
    inc bx
    pop ds
    ret
endp

displayPaddle proc near       
    push ds
    
    mov bx, offset paddlePosition
    mov dx, [bx]                    ; word from DS:BX
    
    mov ax, 0b800h
    mov ds, ax
    mov bx, 3527                    ;position in buffer (3527, 22 rows * 160 + shift(7))
    mov cx, 44
    loop21:    
        mov [bx], 00h
        add bx, 2
    loop loop21   
    mov bx, 3527                    
    add bx, dx                      ;by two bytes
    add bx, dx                      ;by two bytes
    mov cx, 4
    loop31:    
        mov [bx], 0E0h
        add bx, 2
    loop loop31
    pop ds    
    ret
endp

welcomeScreen proc near 
    push ax
    push bx 
    push dx
    push ds    
    mov ax, 0B800h
    mov ds, ax
    xor bx, bx
    mov cx, 2000
    loopScreenWelcome:                 ; clear screen
        mov [bx], ' '
        inc bx
        mov [bx], 07h
        inc bx
    loop loopScreenWelcome
    pop ds
    mov ah, 0
	mov al, ' '                        ; ASCII code of symbol
	xor bh, bh 		                   ; page number = 0
	mov bl, 50                         ; symbol atribute (LIGHT GREEN)
	mov cx, 200                        ; number of repeat of ' '
	int 10h
    mov ah, 9h
    mov dx, offset controlsMessage
    int 21h          
   
    waitEnterWelcome: 
        mov ah, 1                         ; wait for symbol
        int 16h
    jz waitEnterWelcome
        xor ah, ah                        ; keyboard interupt
        int 16h
        cmp ah, 1Ch                       ; check enter key
        je EnterWelcome
        cmp ah, 01h                       ; check on escape key
        jne waitEnterWelcome
        mov ah, 00                        ; return video mode
        mov al, 03
        int 10h
        mov ah, 4Ch                       ; escape from program
        int 21h
    EnterWelcome:
        pop dx
        pop bx
        pop ax      
    ret
endp

moveBall proc near  
    push dx    
    cmp verticalMovement, 0
    jne moveDown                                  ; if ball can move down
    cmp ballPositionY, 0
    jne notUpCol                                  ; if ball is not collised with up line
    mov verticalMovement, 1
    notUpCol:              
        jmp horizontalCheck
    moveDown:
        cmp ballPositionY, 18
        jne notDownCol                            ; if ball is not collised with down line
        mov bx, offset paddlePosition
        mov ax, [bx]
        cmp ax, ballPositionX                     ; check ball position and paddle left position
        jg paddleLose                             ; and if not in position goto paddlelose
        add ax, 3
        cmp ax, ballPositionX                     ; check ball position and paddle right position
        jl paddleLose                             ; and if not in position goto paddlelose
        mov verticalMovement, 0
        jmp notDownCol
    paddleLose:
        mov ax, 01h
        pop dx
        ret
    notDownCol: 
    horizontalCheck:
        cmp horizontalMovement, 0
        jne moveLeft
        cmp ballPositionX, 43                     ;44-1
        jne changeBallPos
        mov horizontalMovement, 1
        jmp changeBallPos
    moveLefT:   
        cmp ballPositionX, 0
        jne changeBallPos   
        mov horizontalMovement, 0
    changeBallPos: 
        cmp horizontalMovement, 1
        jne moveRight
        dec ballPositionX 
        call checkCollision
        cmp dx, 00h
        je verticalMove
        inc ballPositionX
        mov horizontalMovement, 0  
        jmp verticalMove
    moveRight:
        inc ballPositionX 
        call checkCollision 
        cmp dx, 00h
        je verticalMove
        dec ballPositionX
        mov horizontalMovement, 1 
    verticalMove:
        cmp verticalMovement, 1
        jne moveUp
        inc ballPositionY 
        call checkCollision  
        cmp dx, 00h
        je moveEnd
        dec ballPositionY 
        mov verticalMovement, 0
        jmp moveEnd
    moveUp:
        dec ballPositionY  
        call checkCollision
        cmp dx, 00h
        je moveEnd
        inc ballPositionY  
        mov verticalMovement, 1
    moveEnd:   
        call checkCollision  
        xor ax, ax
        pop dx
    ret
endp

displayBall proc near           
    push ax
    push bx
    push cx
    push ds

    mov bx, offset ballPositionY
    mov ax, [bx]
    add ax, 3                  ;upper shift
    mov cl, 160                
    mul cl                      
    mov bx, offset BallPositionX
    mov cx, [bx]
    add ax, cx                 ;2 bytes(1st)
    add ax, cx                 ;(2nd)
    add ax, 7                  ;right shift
    mov bx, ax
    mov ax, 0b800h
    mov ds, ax
    mov [bx], 70h
    
    pop ds
    pop cx
    pop bx
    pop ax
    ret
endp

checkCollision proc near                    
    push ax
    push bx
    push cx    
    xor dx, dx
    mov ax, ballPositionY
    mov cl, 44                             
    mul cl
    mov cx, ballPositionX
    add ax, cx
    mov bx, offset playField
    add bx, ax                     
    cmp [bx], 00h
    je notCollision
    add score, 10
    dec winCount
    call printScore
    mov dx, 01h 
    mov [bx], 00h
    and ax, 01h
    cmp ax, 00h
    je deleteInc
    dec bx
    mov [bx], 00h
    jmp notCollision
    deleteInc:
        inc bx
        mov [bx], 00h
    notCollision: 
        pop cx
        pop bx
        pop ax
    ret
endp

paddleStart proc near
    mov bx, offset paddlePosition
    mov ax, [bx]
    add ax, 2
    mov ballPositionX, ax
    mov ballPositionY, 18
    mov horizontalMovement, 0
    mov verticalMovement, 0 
    call displayPlayField
    call displayBall   
    call displayPaddle
    paddleLoop:
    mov ah, 1
    int 16h
    jz noKeyPressed1
    xor ah, ah
    int 16h
    cmp ah, 4Dh                 ;right arrow
    jne notRight1
    cmp paddlePosition, 40
    jge notRight1
    inc paddlePosition
    inc ballPositionX
    call displayPlayField
    call displayPaddle 
    call displayBall    
    notRight1:  
    cmp ah, 4Bh                 ;left arrow
    jne notLeft1
    cmp paddlePosition, 0
    je notLeft1
    dec paddlePosition
    dec ballPositionX
    call displayPlayField
    call displayPaddle 
    call displayBall
    notLeft1:
    cmp ah, 01h                 ;escape
    jne notEscape1
    jmp exit
    notEscape1:
        cmp ah, 1ch             ;enter
        jne notEnter1
        ret
    notEnter1: 
    noKeyPressed1:
    jmp paddleLoop
    ret
endp    

printWin proc near   
    push ax
    push bx 
    push dx
    push ds    
    xor bh, bh
    mov dh, 4
    xor dl, dl
    mov ah, 02
    int 10h
    mov ax, 0B800h
    mov ds, ax
    xor bx, bx
    mov cx, 2000
    loopScreenWin:     
        mov [bx], ' '
        inc bx
        mov [bx], 07h
        inc bx
        loop loopScreenWin
        pop ds
        mov ah, 9h
        mov dx, offset messageWin
        int 21h          
    
    waitEnterWin: 
        mov ah, 1
        int 16h
        jz waitEnterWin
        xor ah, ah
        int 16h
        cmp ah, 1Ch
        je EnterWin
        cmp ah, 01h
        jne waitEnterWin
        mov ah, 00
        mov al, 03
        int 10h
        mov ah, 4Ch
        int 21h
    EnterWin:
        xor bh, bh
        mov dh, 25
        mov ah, 02
        int 10h
        pop dx
        pop bx
        pop ax      
    ret
endp

main:
    
    mov ax, @data
    mov ds, ax
    mov ax, 3
    xor ah, ah
    int 10h       
    call welcomeScreen
    
    restart:
        mov score, 0 
        mov currentLevel, 0
        mov previousTime, 0  
        mov ballPositionY, 18
        mov ballPositionX, 2
        mov horizontalMovement, 0
        mov verticalMovement, 0    
        call initScreen
        call printScore 
        call initPlayField
        call checkCollision   
        call displayPlayField
        call displayBall   
        call displayPaddle
        call paddleStart
        mov ah, 01h                  ;change system timer value for update screen
        xor cx, cx                   ;high value
        xor dx, dx                   ;low value
        int 1ah                      
    start:                     
        mov ah, 01h                  ; check symbol from keyboard
        int 16h
        jz noKeyPressed              ; if null
        xor ah, ah                   ; wait symbol
        int 16h
        cmp ah, 4Dh                  ; check symbol with -> button
        jne notRight                 ; if ah != 4Dh
        cmp paddlePosition, 40       ; check paddle position with end of play field
        jge notRight                 ; if pos >= 40
        inc paddlePosition
        call displayPlayField
        call displayPaddle 
        call displayBall    
    notRight:  
        cmp ah, 4Bh                  ; check symbol with <- button
        jne notLeft                  ; if ah != 4Bh
        cmp paddlePosition, 0        ; check paddle position with start of play field
        je notLeft                   ; if == 0
        dec paddlePosition
        call displayPlayField
        call displayPaddle 
        call displayBall
    notLeft:
        cmp ah, 01h
        jne noKeyPressed
        jmp exit                     ; exit from program on esc
    noKeyPressed: 
        mov ah, 00h
        int 1ah
        push dx                      ; push low value of timer in stack 
        mov ax, previousTime         ; get previous value of timer
        sub dx, ax
        mov ax, dx
        pop dx
        cmp ax, 3                    ; if ax < 3
        jl notMove                   ; go to not move
        mov previousTime, dx         ; save previous time
        call moveBall 
        cmp ax, 00h                  ;if ball not colised 
        je notLose                   ; goto not lose
        call printLose
        jmp waitEnter
    notLose:  
        call displayPlayField
        call displayPaddle 
        call displayBall
    notMove:
        cmp winCount, 0
        jne start
        inc currentLevel   
        push ax
        mov ax, levelsAmmount
        cmp currentLevel, ax  
        pop ax
        jne notWin
        call printWin
        jmp restart
    notWin:  
        mov previousTime, 0  
        mov ballPositionY, 18
        mov ballPositionX, 11
        mov horizontalMovement, 0
        mov verticalMovement, 0    
        call initScreen
        call printScore 
        call initPlayField
        call checkCollision   
        call displayPlayField
        call displayBall   
        call displayPaddle
        call paddleStart
        mov ah, 01h
        xor cx, cx
        xor dx, dx
        int 1ah      
        jmp start
    waitEnter:
        mov ah, 1
        int 16h
        jz waitEnter
        xor ah, ah
        int 16h
        cmp ah, 1Ch
        jne notEnter
        jmp restart
    notEnter:
        cmp ah, 01
        jne waitEnter  
    exit:
        mov ah, 00
        mov al, 03
        int 10h
        mov ah, 4Ch
        int 21h
end main