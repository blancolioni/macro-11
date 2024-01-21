    ; 173700 -- load bos from 1700000 of rf0 (RF11)

    mov    #177472,r0
    mov    #3,-(r0)        ; rf11.dae.ta[6:5] = 3
    mov    #140000,-(r0)   ; rf11.dar = 140000
    mov    #54000,-(r0)    ; rf11.cma = 54000
    mov    #-2000,-(r0)    ; rf11.wc = -2000 (1K words)
    mov    #5,-(r0)        ; rf11.dcs = read,go
1$:    tstb    (r0)        ; done?
    bge    1$              ; no, loop
    jmp    @#54000         ; jump to bos
