            .model tiny

            org 100h

            jmp main
                        
newline     PROC   
    
            mov ah, 0Eh   
            mov al,0Dh
            int 10h
                             
            mov ah, 0Eh   
            mov al,0Ah
            int 10h  
            
            ret
            ENDP

space_chk   PROC
            mov si, offset word + 1
            mov cl, [ si ]
            mov ch, 0           ;GETTING SIZE TO CX
            mov si, offset word + 2
            dec si              ;SETTING POINTER TO STR
            spa_loop:
                inc si
                cmp [ si ], ' ' ;COMPARE WITH SPACE
                jne not_space   
                mov ax, 1       ;IF FOUND SET FLAG TO 1
                jmp end_spa_loop
                not_space:
                dec cx
                jnz spa_loop
            end_spa_loop:               
                ret
            ENDP

set_wd_size PROC
            mov di, offset word + 1
            mov al, [ di ]
            mov ah, 0
            ret
            ENDP

set_str_size PROC
            mov si, offset string + 1
            mov cl, [ si ]
            mov ch, 0
            ret
            ENDP

find_wd     PROC
            mov si, offset string + 2
            dec si              ;SET POINTER TO STR-1
            xor dx, dx          ;CLEAR DX  
            search:
                mov di, offset word + 2  ;SET POINTER TO WORD
                inc si                   ;INCREMENT STR
                cmp [ si ], '$'          ;IF CHAR IS '$' -> RETURN
                je  end_dwd
                cmp [ si ], ' '          ;IF 'SPACE' -> NEXT ITERATION
                je  search
                
            found:
                mov dl, [ di ]           
                cmp dl, [ si ]           ;COMPARE STR[I] AND WORD[I]
                jne skip                 ;IF NOT EQUALS SKIP THE WORD
                inc di
                inc si
                cmp [ di ], '$'          ;IF WORD[I] IS '$' SKIP SPACES TO NEXT WORD
                jne found
                 
            skip_spaces:    
                inc si
                cmp [ si ], '$'          ;IF '$' -> RETURN
                je  end_dwd
                cmp [ si ], ' '          ;IF ' ' -> REPEAT TILL NOT A ' '
                je  search
                
                inc wd_found             ;SET FLAG TO 1
                jmp end_dwd              ;RETURN
                
            skip:
                cmp [ si ], ' '          ;IF NEXT CHAR IS ' ' RETURN TO THE BEGINING
                je  search
                cmp [ si ], '$'          ;IF '$' -> RETURN
                je  end_dwd
                inc si                   ;SKIP UNTIL ' ' OR '$'
                jmp skip
                 
            end_dwd:           
                ret
            ENDP

delete_wd   PROC
            mov di, si                   ;SET DI TO BEGINING OF WORD TO DELETE
            xor cx, cx                   ;CLEAR CX
            xor dx, dx                   ;CLEAR DX
            dec di                       ;DEC POINTER TO INCREMENT IT LATER
            mov ax, 1                    ;SET FLAG
            dec cx   
            find_size:
                inc di  
                inc cx
                cmp [ di ] , ' '
                je  find_end_size
                cmp [ di ] , '$'
                je  find_end_size
                jmp find_size
                    
            find_end_size:
                cmp ax, 1               ;CHECKING FLAG
                jne continue
                mov di, offset string + 1 ;CHANGING SIZE BY SUBSTRACTING CX
                mov dx, [ di ]            
                mov dh, 0
                sub dx, cx
                mov [ di ], dl 
                mov di, si                ;SET POTINTER SI TO DI
                mov ax, 0                 ;SET FLAG TO 0
                continue:
                    inc dx
                    inc di
                    cmp [ di ] , '$'
                jne  find_end_size
                            
            shift_left_1:
                push cx                   ;PUSHING CX TO REMEMBER
                mov cx , dx               ;MOVING DX TO CX TO USE LOOP
                mov di , si               ;UPDATING POINTER
                shift_left_2:
                    mov ah, [ di ]
                    inc di
                    mov bh, [ di ]
                    mov [ di ], ah
                    dec di
                    mov [ di ], bh

                    inc di
                    loop shift_left_2
                pop cx
                cmp cx, 0
                je fff   
            loop shift_left_1
            fff:
       
            ret
            ENDP

main:
            mov ax, @data
            mov ds, ax 

;MESSAGE                        
            mov ah, 9
            mov dx, offset string_message               
            int 21h                   
            call newline

string_input:
                   
;CAPTURE string FROM KEYBOARD.                                    
            mov ah, 0Ah         ;SERVICE TO CAPTURE STRING FROM KEYBOARD.
            mov dx, offset string
            int 21h                 

;CHANGE CHR(13) BY '$'.
            mov si, offset string + 1 ;NUMBER OF CHARACTERS ENTERED.
            mov cl, [ si ]      ;MOVE LENGTH TO CL.
            mov ch, 0           ;CLEAR CH TO USE CX.
            cmp cx, 0
            je  string_error 
            inc cx              ;TO REACH CHR(13).
            add si, cx          ;NOW SI POINTS TO CHR(13).
            mov al, '$'
            mov [ si ], al      ;REPLACE CHR(13) BY '$'.

;MESSAGE    
            call newline        
            mov ah, 9
            mov dx, offset word_message               
            int 21h  
            call newline
                                                       
word_input:
                                                          
;CAPTURE word FROM KEYBOARD.                                    
            mov ah, 0Ah         ;SERVICE TO CAPTURE STRING FROM KEYBOARD.
            mov dx, offset word
            int 21h                 

;CHANGE CHR(13) BY '$'.
            mov si, offset word + 1 ;NUMBER OF CHARACTERS ENTERED.
            mov cl, [ si ]      ;MOVE LENGTH TO CL.
            mov ch, 0           ;CLEAR CH TO USE CX.
            cmp cx, 0
            je  size_error  
            inc cx              ;TO REACH CHR(13). 
            add si, cx          ;NOW SI POINTS TO CHR(13).
            mov al, '$'
            mov [ si ], al      ;REPLACE CHR(13) BY '$'.
            
;CHECK FOR SPACES IN word
            mov ax, 0           ;RESET FLAG
            call space_chk            
            cmp ax, 1           ;SET FLAG TO 1 IF SPACE IS FOUND
            je  word_error
            
;CHECK FOR THE SAME OR GRATER SIZE OF word
            mov si, offset string + 1
            mov cl, [ si ]
            mov ch, 0
            mov si, offset word + 1
            mov al, [ si ]
            mov ah, 0
            cmp ax, cx
            jae same_size
            
;DELETE AFTER word
find_and_delete:
            call find_wd
            cmp wd_found, 0
            je not_found          
            call delete_wd
            cmp [ si ], '$'
            jne find_and_delete                     
;DISPLAY STRING.
found_msg:  
            call newline
            mov ah, 9
            mov dx, offset found_message
            int 21h
            call newline                   
            mov ah, 9           ;SERVICE TO DISPLAY STRING.
            mov dx, offset string + 2 ;MUST END WITH '$'.
            int 21h
            jmp final

size_error:
            call newline
            mov ah, 9
            mov dx, offset string_err_msg
            int 21h
            call newline
            jmp word_input

string_error:
            call newline
            mov ah, 9
            mov dx, offset string_err_msg
            int 21h
            call newline
            jmp string_input
            
word_error:
            call newline
            mov ah, 9
            mov dx, offset word_err_msg
            int 21h
            call newline
            jmp word_input
            
same_size:
            call newline
            mov ah, 9
            mov dx, offset same_size_message
            int 21h
            jmp final 
            
not_found:
            call newline
            mov ah, 9
            mov dx, offset not_found_message
            int 21h             
            
final:            
            mov ah, 4ch
            int 21h
                        
string_message    db "Enter string:$"
            
word_message      db "Enter word:$"

found_message     db "Modified string:$"

not_found_message db "No matches found!$"    

word_err_msg      db "That's not a word(space entered), reenter word!$"

string_err_msg    db "Size of string is 0, reenter string!$"

same_size_message db "Word has the same or grater size, than string, not able to find!$"

wd_found db 0          
            
string      db  201         ;MAX NUMBER OF CHARACTERS ALLOWED (200).
            db  ?           ;NUMBER OF CHARACTERS ENTERED BY USER.
            db  201 dup(0)  ;CHARACTERS ENTERED BY USER.
            
word        db  201         ;MAX NUMBER OF CHARACTERS ALLOWED (200).
            db  ?           ;NUMBER OF CHARACTERS ENTERED BY USER.
            db  201 dup(0)  ;CHARACTERS ENTERED BY USER.
            
            end main