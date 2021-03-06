\ /* queens.c */
\ Improved hand-done assembler source code GNU C for RTX 2000

EMPTY
: XY " DOS XY queens.e.4th" EVALUATE ;

DECIMAL
load gnutool.4th

VARIABLE tries   4   CELL- ALLOT

#REGS 100 - REG-ADDR $FFC0 AND  UBR!

: do_error
  ." Error in Queens."  cr ;

 64 FRAME_SIZE !
: Try              ( c x i q a b-- )
 1 tries +!        ( -- c x i q a b )
 3 U!              ( -- c x i q a )
 4 U!              ( -- c x i q )
 0 OVER !          ( -- c x i q )
 7 U!              ( -- c x i )
 8 U!              ( -- c x )
 6 U!              ( -- c )
 2 U!              ( -- )
 0                 ( -- j )
 BEGIN             ( -- j )
   DUP   8  <>     ( -- j  flag)
   7 U@  @  0= AND ( -- j flag )
 WHILE             ( -- j )
     0 7 U@  !                           ( -- j )
     1 +  DUP                            ( -- j j )
     2* 3 U@  + @                        ( -- j b[j] )
     IF                                  ( -- j )
       DUP 8 U@ +  [ 9 ] DUP_U!  2* 4 U@ +  @  ( -- j flag )
       IF                                      ( -- j )
         8 U@ OVER - [ 10 ] DUP_U! 2* 2 U@ +  14 + @     ( -- j flag )
         IF                                 ( -- j )
           DUP   8 U@ 2* 6 U@  + !          ( -- j )
           0     OVER 2* 3 U@  + !          ( -- j )
           0     9 U@ 2* 4 U@  + !          ( -- j )
           0    10 U@ 2* 2 U@  + 14 + !     ( -- j )
           8 U@ 7 <=                        ( -- j flag )
           IF                               ( -- j )
             5 U!
             2 U@   6 U@  8 U@ 1 +   7 U@   4 U@   3 U@
             -64 FP+! RECURSE 64 FP+!
             5 U@                           ( -- j )
             7 U@ @                         ( -- j flag )
             IF ELSE 
               -1  OVER  2* 3 U@  +  !      ( -- j )
               -1  9 U@  2* 4 U@  +  !      ( -- j )
               -1 10 U@  2* 2 U@  + 14 + !  ( -- j )
             THEN                           ( -- j )
           ELSE                             ( -- j )
             -1 7 U@  !                     ( -- j )
           THEN                             ( -- j )
         THEN                               ( -- j )
       THEN                                 ( -- j )
     THEN                                   ( -- j )
   REPEAT                                   ( -- j )
   DROP  ;

 192 FRAME_SIZE !

: Doit
 -7              ( -- i )
 BEGIN           ( -- i )
   DUP 0>        ( -- i )
   IF            ( -- i )
     DUP 9 <     ( -- i )
     IF          ( -- i )
       -1  OVER 2* UBR@ + 28 - !       ( -- i )
     THEN        ( -- i )
   THEN          ( -- i )
   DUP 1 >       ( -- i )
   IF            ( -- i )
     -1  OVER 2* UBR@ + 64 - !       ( -- i )
   THEN          ( -- i )
   DUP 8 <       ( -- i )
   IF            ( -- i )
     -1  OVER 2* UBR@ + 82 - !       ( -- i )
   THEN          ( -- i )
   1 +     DUP   ( -- i )
   16 >
 UNTIL           ( -- i )
 DROP            ( -- )
 UBR@ 96 -  UBR@ 116 -  1  UBR@ 120 -  UBR@ 64 -  UBR@ 28 -
 -192 FP+!   Try   192 FP+!
 [ -62 ] MEM_ARG@
 0= IF
   -192 FP+!   do_error 192 FP+!
   1000 tries +!
 THEN
 ;

 64 FRAME_SIZE !

: main
#REGS 100 - REG-ADDR $FFC0 AND  UBR!
 1 >R

 BEGIN
 0 tries !
 -64 FP+!
   Doit
 64 FP+!
 tries @ 113 -
 IF
   -64 FP+!   do_error 64 FP+!
 THEN
 R> 1 + DUP_>R
 2500
 U>
 UNTIL
 R>DROP
 ;

.( 2500 max ) cr
