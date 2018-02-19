
  DOSSEG
  .MODEL small
  .STACK 100h
  .DATA
msga DB 'START$'
msgb DB 'STOP$'
msgc DB '  answer = -13111  $'
  .CODE

  mov ax, @data
  mov ds,ax
  mov ah,9
  mov dx,OFFSET msga
  int 21h

  mov bx,35
  call fib

  mov ax, @data
  mov ds,ax
  mov ah,9
  mov dx,OFFSET msgb

  cmp bx,-13111
  jne lab1
  mov dx,OFFSET msgc
lab1:
  int 21h

  mov ah,4ch
  int 21h

fib:
  cmp bx,3
  jge lab2
  mov bx,1
  ret
lab2:
  push bx
  sub  bx,1
  call fib
  pop  ax
  xchg ax,bx
  push ax
  sub  bx,2
  call fib
  pop  ax
  add  bx,ax
  ret

  END
