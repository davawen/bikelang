
global _start
section .text
; builtin functions

__builtin_print_number1:
    mov rcx, 0
    .loop:
        xor dl, dl ; nullify rdx

        mov r8b, 10
        idiv r8b ; divide assembled register dl:al by r8b
        ; quotient goes in al, remainder goes in dl

        add dl, 0x30 ; offset remainder to ASCII '0'

        ; push printable remainder to stack
        sub rsp, 1
        mov BYTE [rsp], dl ; ASCII characters are one byte wide, so we only want that

        ; continue until remaining number is 0
        inc rcx
        cmp rax, 0
        jnz .loop

    push rcx ; save rcx for cleanup

    ; the stack grows downwards, so the characters pushed in reverse order are now in the good order
    lea rsi, [rsp+8] 
    mov rdx, rcx
    mov rax, 1
    mov rdi, 1
    syscall

    pop rcx
    add rsp, rcx ; cleanup
    ret

__builtin_print_number2:
    mov rcx, 0
    .loop:
        xor dx, dx ; nullify rdx

        mov r8w, 10
        idiv r8w ; divide assembled register dx:ax by r8w
        ; quotient goes in ax, remainder goes in dx

        add dx, 0x30 ; offset remainder to ASCII '0'

        ; push printable remainder to stack
        sub rsp, 1
        mov BYTE [rsp], dl ; ASCII characters are one byte wide, so we only want that

        ; continue until remaining number is 0
        inc rcx
        cmp rax, 0
        jnz .loop

    push rcx ; save rcx for cleanup

    ; the stack grows downwards, so the characters pushed in reverse order are now in the good order
    lea rsi, [rsp+8] 
    mov rdx, rcx
    mov rax, 1
    mov rdi, 1
    syscall

    pop rcx
    add rsp, rcx ; cleanup
    ret

__builtin_print_number4:
    mov rcx, 0
    .loop:
        xor edx, edx ; nullify rdx

        mov r8d, 10
        idiv r8d ; divide assembled register edx:eax by r8d
        ; quotient goes in eax, remainder goes in edx

        add edx, 0x30 ; offset remainder to ASCII '0'

        ; push printable remainder to stack
        sub rsp, 1
        mov BYTE [rsp], dl ; ASCII characters are one byte wide, so we only want that

        ; continue until remaining number is 0
        inc rcx
        cmp rax, 0
        jnz .loop

    push rcx ; save rcx for cleanup

    ; the stack grows downwards, so the characters pushed in reverse order are now in the good order
    lea rsi, [rsp+8] 
    mov rdx, rcx
    mov rax, 1
    mov rdi, 1
    syscall

    pop rcx
    add rsp, rcx ; cleanup
    ret

__builtin_print_number8:
    mov rcx, 0
    .loop:
        xor rdx, rdx ; nullify rdx

        mov r8, 10
        idiv r8 ; divide assembled register rdx:rax by r8
        ; quotient goes in rax, remainder goes in rdx

        add rdx, 0x30 ; offset remainder to ASCII '0'

        ; push printable remainder to stack
        sub rsp, 1
        mov BYTE [rsp], dl ; ASCII characters are one byte wide, so we only want that

        ; continue until remaining number is 0
        inc rcx
        cmp rax, 0
        jnz .loop

    push rcx ; save rcx for cleanup

    ; the stack grows downwards, so the characters pushed in reverse order are now in the good order
    lea rsi, [rsp+8] 
    mov rdx, rcx
    mov rax, 1
    mov rdi, 1
    syscall

    pop rcx
    add rsp, rcx ; cleanup
    ret

; user program:
main:
    ; enter stack frame
    push rbp
    mov rbp, rsp
    sub rsp, 16

    mov DWORD [rbp-4], 12
    
    mov eax, DWORD [rbp-4]
    imul eax, 10
    
    mov DWORD [rbp-16], eax
    
    mov eax, DWORD [rbp-16]
    mov DWORD [rbp-12], eax
    
    
    mov rax, 1
    mov rdi, 1
    mov rsi, user_str0
    mov rdx, 9
    syscall
    
    mov eax, DWORD [rbp-12]
    call __builtin_print_number4
    
    mov rax, 1
    mov rdi, 1
    mov rsi, user_str1
    mov rdx, 1
    syscall
    

    ; leave stack frame
    mov rsp, rbp
    pop rbp
    ret


_start:
    call main

    mov rax, 60
    xor rdi, rdi
    syscall ; sys_exit

section .data
user_str0: db 97, 32, 42, 32, 49, 48, 32, 61, 32
user_str1: db 10

