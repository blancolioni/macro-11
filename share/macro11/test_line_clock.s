line_clock_vector = 100
line_clock_psw    = 102
line_clock_ctrl   = 177546
tick_count = 10000

      mov pc, r0
      add #vector-2, r0
      mov r0, @#line_clock_vector
      mov #340, @#line_clock_psw
      mov #100, @#line_clock_ctrl
      mov #0, @#tick_count
      mtps #0
1$:   wait
      mov @#tick_count, r0
      cmp r0, #310
      bne 1$
      mov #0, @#tick_count
      mov #0, @#line_clock_ctrl
2$:   bit #200, @#line_clock_ctrl
      beq 2$
      inc @#tick_count
      cmp @#tick_count, #310
      bne 2
      halt

vector: inc @#tick_count
        rti