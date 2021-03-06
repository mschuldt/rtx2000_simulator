\ /* Eratosthenes Sieve Prime Number Program in C from Byte January 1983 */
\ Improved source code assembler based on GNU C for RTX 2000

EMPTY
: XY " DOS XY sieve.e.4th" EVALUATE ;

load gnutool.4th

VARIABLE myflags   8192   CELL- ALLOT
: flags  " [ myflags ] literal " evaluate ; immediate

DECIMAL
 #REGS 100 - REG-ADDR $FFC0 AND  UBR!

: do_error
  ." Error in Sieve."   CR
   ;

 64 FRAME_SIZE ! 
: main
 #REGS 100 - REG-ADDR $FFC0 AND  UBR!

 1  ( LIT)       ( 6  top> empty )
 [ 0020 ADD_INDEX ] >R   ( 137  top> #65 )

 ( TYPE 1 LOOP BEGIN)  ( 7  top> empty )

 BEGIN
 0  2 U!   \ Count is in user # 2

 1 flags    ( -- 1 flags )

 8190 FOR          ( -- 1 addr )
   [ $F8C1 , ] ( DDUP_C!_1+ )     ( -- 1 addr' )
 NEXT              ( -- 1 addr' )
 DROP DROP ( -- )

 0  >R

 BEGIN
  flags I +  C@
  IF   I   2*   3   +           ( -- j )
       DUP I  +                 ( -- j k )
    DUP 8191 u<
    IF                          ( -- j k )
      BEGIN
        0  OVER  flags +  C!    ( -- j k )
        OVER_+                  ( -- j k )
        DUP  8190   U>          ( -- j k flag )
      UNTIL
    THEN                        ( -- j k )
    DROP  DROP
    1  2 U@ +   2 U!   ( increment count )
  THEN 
 R>  1 +  DUP_>R 
 8190   U>
 UNTIL
 R>DROP  

 2 U@  1899   - 
 IF
   do_error  
 THEN

 R>  1  +  DUP_>R 
 349   U>
 UNTIL
 R>DROP  
   ; 

.( max 349) cr
