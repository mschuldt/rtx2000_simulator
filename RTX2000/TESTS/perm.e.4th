\ /* perm.c */
\ Improved source code assembler based on GNU C for RTX 2000

EMPTY
: XY " DOS XY perm.e.4th" EVALUATE ;

DECIMAL
load gnutool.4th

VARIABLE permarray   24   CELL- ALLOT
VARIABLE pctr   4   CELL- ALLOT

#REGS 100 - REG-ADDR $FFC0 AND  UBR!

: do_error
 ." Error in Perm."  cr
   ;

: Swap_el  ( ^a ^b -- )  \ swap a & b
 ( DUP @ SWAP ) [ $E140 , ]   ( -- ^a b ^b )
 >R                           ( -- ^a b )
 OVER @                       ( -- ^a b a )
 R>                           ( -- ^a b a ^b )
 !                            ( -- ^a b )
 SWAP  ! ;

: Initialize  ( -- )
 6 FOR 
   I
   I 1 + 2* [ permarray ] SYMBOL_+ !
 NEXT 
 ;

 64 FRAME_SIZE !
: Permute   ( n -- )
 1 pctr +!
 [ 2 ] DUP_U!  1 -
 IF
   2 U@ 1 -
   -64 FP+!  RECURSE 64 FP+!
   2 U@ 2* [ permarray ] SYMBOL_+
   [ 3 ] DUP_U!   2 -
   [ permarray 2 + ] LITERAL
   OVER
   U>
   IF                                ( -- k )
      drop exit
   ELSE                              ( -- k )
     BEGIN                           ( -- k )
       [ 4 ] DUP_u! 3 U@  Swap_el    ( -- k )
       2 U@ 1 - 
       -64 FP+! RECURSE  64 FP+! 
       4 U@                          ( -- k )
       3 U@ OVER   Swap_el           ( -- k )
       2 - DUP   [ permarray 2 + ] LITERAL  U<
     UNTIL                           ( -- k )
     DROP EXIT
   THEN
 THEN
 ;

 64 FRAME_SIZE !
 : main
#REGS 100 - REG-ADDR $FFC0 AND  UBR!
 1 >R
 BEGIN 
   0 pctr !
   Initialize
   7  -64 FP+!   Permute 64 FP+!
   pctr @ 8660 -
   IF  do_error  THEN
   R> 1 + DUP_>R
   250 U>
 UNTIL
 R>DROP
 ;

.( max 250) cr
