; write out ascii characters from ' ' (10#32# 8#40#) to '~' (10#126# 8#176#)
tty_tps = 177564  ; tty control register
tty_tpb = 177566  ; tty character buffer register

mov #40, r0                    ; start at octal 40, decimal 32

1$: bit #200, @#tty_tps        ; wait for tty to be ready
    beq 1$                     ; busy wait
    movb r0, @#tty_tpb         ; send current character to buffer
    inc r0                     ; next character
    cmp r0, #177               ; stop at octal 177, decimal 127
    bne 1$                     ; not there yet --> go back and do the next character
2$: bit #200, @#tty_tps        ; another wait loop
    beq 2$
    movb #12, @#tty_tpb        ; octal 12 = decimal 10 = new line
3$: bit #200, @#tty_tps        ; wait for tty to be ready
    beq 3$
    halt                       ; done
   