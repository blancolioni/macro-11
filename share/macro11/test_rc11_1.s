RCLA = 8#177440#

   mov #64, r0
1: mov @#RCLA, r1
   cmp r0, r1
   bpl 1
   halt
