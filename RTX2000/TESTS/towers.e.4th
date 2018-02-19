\ /* towers.c */
\ Improved source code assembler based on GNU C for RTX 2000

EMPTY
: XY " DOS XY towers.e.4th" EVALUATE ;

DECIMAL
load gnutool.4th

#REGS 100 - REG-ADDR $FFC0 AND  UBR!

VARIABLE movesdone   4   CELL- ALLOT
VARIABLE freelist   4   CELL- ALLOT
VARIABLE cellspace   76   CELL- ALLOT
VARIABLE stack   8   CELL- ALLOT

: do_error
   ." Error in Towers." cr
   ;

: Error
   ." Error in Towers." cr
   ;

: Makenull  ( ADDR -- )
 0 SWAP ! ;

 64 FRAME_SIZE !
: Getelement   ( -- addr )
 freelist @ DUP 0>
 IF
   DUP
   2* 2*  [ cellspace 2 + ] SYMBOL_+  @
   freelist !
   EXIT
 THEN
 Error
 ;

64 FRAME_SIZE !
: Push  ( i ^s -- )
 DUP @ 3 U!
 4 U!  5 U!
 3 U@ 0>
 IF 
   3 U@ 2* 2* [ cellspace ] SYMBOL_+ @
   5 U@ <=
   IF 
     Error
     exit
   THEN 
 THEN 
 Getelement                             ( -- el )
 DUP 2* 2* [ cellspace ] SYMBOL_+  DUP  ( -- el ^el ^el )
 [ 3 ]  U@_SWAP                      ( -- el ^el s_val ^el )
 2 + !                               ( -- el ^el )
 SWAP  4 U@  !                       ( -- ^el )
 [ 5 ]  U@_SWAP !
 ;

64 FRAME_SIZE !
: Init
 3 U!   [ 4 ] dup_U!
 Makenull
 3 U@   [ 5 ] DUP_U! 0>
 IF 
   5 U@                       ( -- discctr )
   BEGIN                      ( -- discctr ) 
     DUP  4 U@                ( -- discctr discctr s )
     -64 FP+! Push  64 FP+!   ( -- discctr )
     1 - DUP                  ( -- discctr )
   WHILE REPEAT               ( -- discctr )
   DROP                       ( -- )
   EXIT                       ( -- )
 THEN 
 ;

: Pop   ( s -- )
 DUP @                                 ( -- s *s )
 DUP 0>                                ( -- s *s flg )
 IF
   DUP  >R 2* 2* [ cellspace ] SYMBOL_+   ( -- s adr )
   @+2                                 ( -- s size adr+2 )
   @+0                                 ( -- s size next adr+2 )
   freelist @_SWAP                     ( -- s size next free adr+2 )
   !                                   ( -- s size next )
   R>     freelist !                   ( -- s size next )
   ROT  !                              ( -- s size next )
   EXIT
 THEN 
 drop drop
 Error
 0
 ;

: Move  ( ^s1 ^s2 -- )
  SWAP Pop SWAP
  -64 FP+!   Push 64 FP+!
  movesdone @ 1 +  movesdone ! 
 ;

64 FRAME_SIZE !
: tower   ( i j k -- )
 [ 099 ] LABEL   \ Tail recursion elimination target
 DUP 1 -
 IF
   2 U! 3 U! 
   [ 4 ] DUP_U!
   6 3 U@ - 4 U@ - [ 8 ] dup_U!
   2 U@ 1 -
   -64 FP+!  recurse  64 FP+!
   4 U@ 2*   [ stack ] SYMBOL_+
   3 U@ 2*   [ stack ] SYMBOL_+
   Move
   8 U@   3 U@   2 U@   1 -
   [ 099 ] BRANCH
 ELSE 
   DROP
   SWAP   2*   [ stack ] SYMBOL_+
   SWAP   2*   [ stack ] SYMBOL_+
   Move
   EXIT
 THEN 
 ;

64 FRAME_SIZE !
: main
#REGS 100 - REG-ADDR $FFC0 AND  UBR!
 0 >R

 BEGIN 
   17 FOR
     I  DUP 1 + 2* 2* [ cellspace 2 + ] SYMBOL_+ !
   NEXT 
   18 freelist !
   [ stack 2 + ] LITERAL    14
   -64 FP+!   Init 64 FP+!
   [ stack 4 + ] LITERAL   Makenull
   [ stack 6 + ] LITERAL   Makenull
   0 movesdone !
   1  2   14  
   -64 FP+!   tower 64 FP+!
   movesdone @ 16383 -
   IF    do_error   THEN
   R> 1 + DUP_>R
   34
   U>
 UNTIL 
 R>DROP
 ;

.( max 34) cr
