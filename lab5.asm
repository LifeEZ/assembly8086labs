.model small

.stack 100h

.data


string      db  201         ;MAX NUMBER OF CHARACTERS ALLOWED (200).
            db  ?           ;NUMBER OF CHARACTERS ENTERED BY USER.
            db  201 dup(0)  ;CHARACTERS ENTERED BY USER.


res_handle dw 0
handle dw 0


filename db "result.txt", 0
filepath db 50 DUP(0)
;filename db "C:\result.txt", 0
;filepath db "C:\a.txt", 0 

is_end db 0
currentSymbol dw 0
eolFlag db 0
symbolFound db 0

buffer dw 100 dup(0)


line_begining_dx dw 0
line_begining_ax dw 0


find_err db "Can't find file!", 10, 13, 0, '$'
path_err db "Can't find file path!", 10, 13, 0, '$' 
toomany_err db "To many opened file!", 10, 13, 0, '$'
accessdenied_err db "Access denied!", 10, 13, 0, '$'
incorrectacces_err db "Incorrect access mode!", 10, 13, 0, '$'
string_err_msg    db "Size of string is 0, reenter string!$"
;3abcd
;efghi
;jklmnop
;qrstuvw
;xyz
.code
changeOffset MACRO min, newOffset
    local cnt_change, cnt_changeoffset
    mov ah, 42h
    mov al, 1
    mov bx, handle
    mov dx, newOffset
    mov cl, 0
    cmp cl, min
    je cnt_change
    
    neg dx
    mov cx, 0FFFFh
    jmp cnt_changeoffset
    
    cnt_change:
    mov cx, 0
    
    cnt_changeoffset:
    int 21h
 ENDM
;=====================================================================================================================================================================================
;=====================================================================================================================================================================================
;=====================================================================================================================================================================================
print MACRO str ;adress of string in dx
    push ax 
    push dx
    mov dx, offset str
    mov ah, 09h
    int 21h
    pop dx
    pop ax
ENDM
;=====================================================================================================================================================================================
;=====================================================================================================================================================================================
;=====================================================================================================================================================================================
newline PROC   
    mov ah, 0Eh   
    mov al,0Dh
    int 10h                      
    mov ah, 0Eh   
    mov al,0Ah
    int 10h  
    ret
newline ENDP
;=====================================================================================================================================================================================
;=====================================================================================================================================================================================
;=====================================================================================================================================================================================
openFile PROC
    jmp openFile_start 
    
    cant_find_error:
    print find_err
    mov is_end, 1
    jmp openFile_fin
    
    path_error:
    print path_err
    mov is_end, 1
    jmp openFile_fin
    
    toomany_error:
    print toomany_err
    mov is_end, 1
    jmp openFile_fin
    
    access_error:
    print accessdenied_err
    mov is_end, 1
    jmp openFile_fin
    
    accessmode_error:
    print incorrectacces_err
    mov is_end, 1
    jmp openFile_fin
    
    openFile_start:
    mov dx, offset filepath ;address of file to dx
    mov al, 2h ;openfile (rw)
    mov ah, 3Dh
    int 21h
    jc openFile_fin_err ;if error occurs, terminate program
    mov bx, ax ;put handler to file in bx
    mov handle, bx
    jmp openFile_fin
    
    openFile_fin_err:
    cmp ax, 02h
    je cant_find_error
    cmp ax, 03h
    je path_error
    cmp ax, 04h
    je toomany_error
    cmp ax, 05h
    je access_error
    cmp ax, 0Ch
    je accessmode_error
    
    openFile_fin:
    ret
openFile ENDP 
;=====================================================================================================================================================================================
;=====================================================================================================================================================================================
;=====================================================================================================================================================================================
createFile PROC
    jmp createFile_start 
    
    cant_find_error_1:
    print find_err
    mov is_end, 1
    jmp createFile_fin
    
    path_error_1:
    print path_err
    mov is_end, 1
    jmp createFile_fin
    
    toomany_error_1:
    print toomany_err
    mov is_end, 1
    jmp createFile_fin
    
    access_error_1:
    print accessdenied_err
    mov is_end, 1
    jmp createFile_fin
    
    accessmode_error_1:
    print incorrectacces_err
    mov is_end, 1
    jmp createFile_fin
    
    createFile_start:
    mov dx, offset filename ;address of file to dx
    mov ah, 3Ch
    mov al, 00h
    mov cx, 0000h
    int 21h ;call the interupt
    jc createFile_fin_err ;if error occurs, terminate program
    mov bx, ax ;put handler to file in bx
    mov res_handle, bx
    jmp createFile_fin
    
    createFile_fin_err:
    cmp ax, 02h
    je cant_find_error_1
    cmp ax, 03h
    je path_error_1
    cmp ax, 04h
    je toomany_error_1
    cmp ax, 05h
    je access_error_1
    cmp ax, 0Ch
    je accessmode_error_1
    
    createFile_fin:
    ret
createFile ENDP
;=====================================================================================================================================================================================
;=====================================================================================================================================================================================
;=====================================================================================================================================================================================

writeLine PROC
    push ax
    push bx
    push cx
    push dx
    ;seting pos to chosen coord
    mov ah, 42h
    mov al, 0
    mov bx, handle
    mov cx, line_begining_dx
    mov dx, line_begining_ax
    int 21h
    writeLineFor:
        mov bx, handle
        mov cx, 1 ;read one symbol
        mov ah, 3Fh ;read from the opened file (its handler in bx) 40h(to write)
        mov dx, offset currentSymbol;where to read (what to write)
        int 21h
        cmp ax, 0
        je writeLineEnd
        cmp currentSymbol, 0Ah
        je writeLineFor
        cmp currentSymbol, 0Dh
        je writeLineEnd


        mov bx, res_handle
        mov cx, 1 ;read one symbol
        mov ah, 40h ;read from the opened file (its handler in bx) 40h(to write)
        mov dx, offset currentSymbol;where to read (what to write)
        int 21h
    jmp writeLineFor
    writeLineEnd:
    mov bx, res_handle
    mov cx, 1 ;read one symbol
    mov ah, 40h ;read from the opened file (its handler in bx) 40h(to write)
    mov dx, offset currentSymbol;where to read (what to write)
    int 21h

    pop dx
    pop cx
    pop bx
    pop ax
    ret
writeLine ENDP
;=====================================================================================================================================================================================
;=====================================================================================================================================================================================
;=====================================================================================================================================================================================
checkSymbol PROC
    push cx
    push si
    push dx

    cmp currentSymbol, 0Dh
    jne keepingOn
    mov eolFlag, 1
    jmp checkSymbolEnd

    keepingOn:
    mov si, offset string + 2
    checkSymbolFor:
        xor dx, dx
        mov dl, [si]
        cmp currentSymbol, dx
        jne checkSymbolContinue
        mov symbolFound, 1
        checkSymbolContinue:
        inc si
        cmp dx, "$"
    jne checkSymbolFor
    checkSymbolEnd:

    pop dx
    pop si
    pop cx
    ret
checkSymbol ENDP
;=====================================================================================================================================================================================
;=====================================================================================================================================================================================
;=====================================================================================================================================================================================
writeToNew PROC
    push ax
    push bx
    push cx
    push dx

    checkingLines:
        ;save pos of line
        changeOffset 0, 1
        mov line_begining_dx, dx ;create line_begining dx var
        dec ax
        mov line_begining_ax, ax ;create line_begining ax var
        changeOffset 1, 1

        ;reseting eolFlag
        mov eolFlag, 0
        mov symbolFound, 0

        checkingSymbols:
            mov bx, handle
            mov cx, 1 ;read one symbol
            mov ah, 3Fh ;read from the opened file (its handler in bx) 40h(to write)
            mov dx, offset currentSymbol;where to read (what to write)
            int 21h
            cmp ax, 0
            je writeToNewEnd
            call checkSymbol
            cmp eolFlag, 1
        jne checkingSymbols

        endChecking:

        cmp symbolFound, 0
        jne notWritingLine
        call writeLine
        notWritingLine:


    jmp checkingLines
    
    ;close file
    writeToNewEnd:
    
    mov ah, 3Eh
    mov bx, res_handle
    int 21h

    mov ah, 3Eh
    mov bx, handle
    int 21h
    
    pop dx 
    pop cx
    pop bx
    pop ax     
    ret
writeToNew ENDP
;=====================================================================================================================================================================================
;=====================================================================================================================================================================================
;=====================================================================================================================================================================================
start:
mov cl, es:80h
mov ax, @data
mov ds, ax
cmp cl, 0 ;fi size 0, no parametrs
je terminate

mov si, 81h ;skip space
xor di,di

inc si ;skip space
dec cl ;skip space

get_parm:
    mov al, es:si
    inc si
    mov [filepath + di] , al  ; помещаем его в filepath 
    inc di
loop get_parm

jmp string_input

string_error:
    call newline
    mov ah, 9
    mov dx, offset string_err_msg
    int 21h
    call newline
    jmp string_input

string_input:
    mov ah, 0Ah
    mov dx, offset string
    int 21h                 

    mov si, offset string + 1
    mov cl, [si]
    mov ch, 0
    cmp cx, 0
    je  string_error 
    inc cx
    add si, cx
    mov al, '$'
    mov [si], al

call openFile
cmp is_end, 1
je terminate

call createFile
cmp is_end, 1
je terminate

call writeToNew

terminate: 

    mov ax, 4C00h
    int 21h
    int 20h
    ends

end start