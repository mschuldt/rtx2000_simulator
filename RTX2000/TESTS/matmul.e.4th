\ /* matmul.c */
\ Improved source code assembler based on GNU C for RTX 2000

EMPTY
: XY " DOS XY matmul.e.4th" EVALUATE ;

DECIMAL

load gnutool.4th
#REGS 100 - REG-ADDR $FFC0 AND  UBR!

VARIABLE seed   4   CELL- ALLOT
VARIABLE imr   3364   CELL- ALLOT
VARIABLE imb   3364   CELL- ALLOT
VARIABLE ima   3364   CELL- ALLOT


: Initrand   ( -- )
 9219 seed ! ;

: Rand  ( -- n)
 seed @ 1309 * 13849 + seed TUCK_!   ;

: Initmatrix
 84 + 
 39 FOR
   39 FOR                    ( -- ^mat[j] )
     Rand                    ( -- ^mat[j] temp )
     DUP  120 / 120 *        ( -- ^mat[j] temp x )
     - 60 -                  ( -- ^mat[j] x )
     SWAP                    ( -- x ^mat[j] )
     !+2                     ( -- ^mat[j] )
   NEXT                      ( -- ^mat[j] )
   2 + 
 NEXT
 DROP 
 ;

: Innerproduct   ( ^result a b row col -- )
 >R                          ( -- ^result a b row )
 SWAP >R                     ( -- ^result a row )
 82 * + 80 +                 ( -- ^result ^a )
 R>  R> 2* + 3280 +  SWAP    ( -- ^result ^b ^a )

 0                           ( -- ^result ^b ^a acc )
 39 FOR                      ( -- ^result ^b ^a acc )
   >R                        ( -- ^result ^b ^a )
   OVER  @                   ( -- ^result ^b ^a b  )
   OVER  @                   ( -- ^result ^b ^a b a )
   *                         ( -- ^result ^b ^a prod )
   >R                        ( -- ^result ^b ^a )
   SWAP -82 + SWAP           ( -- ^result ^b ^a )
   2 -                       ( -- ^result ^b ^a )
   R>  R>  +                 ( -- ^result ^b ^a acc )
 NEXT                        ( -- ^result ^b ^a acc )
 NIP NIP                     ( -- ^result acc )
 SWAP !                      ( -- )
 ;

 64 FRAME_SIZE !
 : main
#REGS 100 - REG-ADDR $FFC0 AND  UBR!
 1 >R

 BEGIN 
  Initrand
  ima  Initmatrix
  imb  Initmatrix

 1 >R

 BEGIN 
   I  82 *  [ imr ] SYMBOL_+  3 U!
   1 2 U!
   BEGIN 
     2 U@  2*
     3 U@  +
     ima
     imb
     I
     2 U@
     Innerproduct
     2 U@ 1 + [ 2 ] dup_U!
     40 u>
   UNTIL 
   R> 1 + DUP_>R
   40 U>
 UNTIL 
  R>DROP
  R> 1 + DUP_>R
  24 U>
 UNTIL 
 R>DROP
   ;

.( max 24) cr
