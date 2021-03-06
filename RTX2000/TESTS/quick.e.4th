\ /* quick.c */
\ Improved source code assembler based on GNU C for RTX 2000

EMPTY
: XY " DOS XY quick.e.4th" EVALUATE ;

DECIMAL
load gnutool.4th

#REGS 100 - REG-ADDR $FFC0 AND  UBR!

VARIABLE seed   4   CELL- ALLOT
VARIABLE top   4   CELL- ALLOT
VARIABLE littlest   4   CELL- ALLOT
VARIABLE biggest   4   CELL- ALLOT
VARIABLE sortlist   10004   CELL- ALLOT

: .DATA
  1000 2 DO I SORTLIST + @ .  2 +LOOP ;

: do_error
   ." Error in Quick." cr
   ;

: Initrand
 9219 seed !   ;

: Rand
 seed @ 1309 * 13849 + seed TUCK_! ;

 64 FRAME_SIZE !
: Initarr
 Initrand
 0 biggest !
 0 littlest !
 [ sortlist 2 + ] LITERAL
 4999 FOR                  ( -- i )
   Rand                    ( -- i temp )
   32668 -  SWAP           ( -- temp i )
   [ $E0C0 , ]  ( DDUP ! ) ( -- temp i )
   OVER  biggest @ >       ( -- temp i flg )
   IF                      ( -- temp i )
     OVER biggest !        ( -- temp i )
   ELSE                    ( -- temp i )
     OVER  littlest @ <    ( -- temp i flg )
     IF                    ( -- temp i )
       OVER littlest !     ( -- temp i )
     THEN                  ( -- temp i )
   THEN                    ( -- temp i )
   NIP  2 +                ( -- i )
 NEXT                      ( -- i )
 DROP
 ;

64 FRAME_SIZE !
: Quicksort
 OVER 2 U!
 [ 3 ] DUP_U! 5 U!
 6 U! 7 U!
 2 U@ u2/  3 U@ u2/ +  -2 AND @ 4 U!

 2 U@                  ( -- i )
 5 U@                  ( -- i j )
 BEGIN                 ( -- i j )
   SWAP                ( -- j i )
   BEGIN               ( -- j i )
     @+2 SWAP  4 U@ >= ( -- j i )
   UNTIL               ( -- j i )
   2 -   SWAP          ( -- i j )
   BEGIN               ( -- i j )
     @-2 SWAP 4 U@ <=  ( -- i j )
   UNTIL               ( -- i j )
   2 +                 ( -- i j )
   OVER OVER  U>       ( -- i j flg )
   IF                  ( -- i j )
     -1                ( -- i j flg )
   ELSE                ( -- i j )
     >R                ( -- i )
     @+0               ( -- *i i )
     R>                ( -- *i i j )
     @+0               ( -- *i i *j j )
     >R SWAP           ( -- *i *j i )
     !+2               ( -- *i i )
     SWAP R>           ( -- i *i j )
     !-2               ( -- i j )
     0                 ( -- i j flg )
   THEN                ( -- i j flg )
 UNTIL                 ( -- i j )
 5 U!  2 U!

 6 U@  5 U@  U<
 IF 
   7 U@   6 U@   5 U@
   -64 FP+!  recurse  64 FP+!
 THEN 
 2 U@   3 U@   U<
 IF 
   7 U@   2 U@   3 U@
   -64 FP+!  recurse  64 FP+!
 THEN 
 ;

 64 FRAME_SIZE !
: main
#REGS 100 - REG-ADDR $FFC0 AND  UBR!
 0 2 U!
 [ sortlist 2 + ] LITERAL 3 U!
 BEGIN 
 -64 FP+!
   Initarr
 64 FP+!
 sortlist
 [ sortlist 2 + ] LITERAL
 [ sortlist 10000 + ] LITERAL
 -64 FP+!
   Quicksort
 64 FP+!
 3 U@ @    littlest @ -
 [ 0033 ] BRANCHNZ
 [ sortlist 10000 + ] LITERAL @
 biggest @ -
 IF 
 [ 0033 ] LABEL
 -64 FP+!
   do_error
 64 FP+!
 THEN 
 2 U@ 1 + [ 2 ] DUP_U!
 49 >
 UNTIL 

 EXIT
   ;

.( max 49) cr
