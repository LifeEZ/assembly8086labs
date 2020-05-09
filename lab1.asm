org 100h
mov dx, offset msg
mov ah, 9h
int 21h                    
mov ah, 4Ch
int 21h 
msg db "Hello world!", 0dh, 0ah, 07h, "$"

;.model small
;.stack 100h
;.data
;msg db "Hello world!", 07h, "$"
;.code
;main:
;   mov dx, offset msg
;   mov ah, 9h
;   int 21h                    
;   ret
;end main 
    