\ /* bubble.c */
\ Improved source code assembler based on GNU C for RTX 2000

EMPTY
: XY " DOS XY bubble.e.4th" EVALUATE ;

DECIMAL
load gnutool.4th


VARIABLE seed   4   CELL- ALLOT

VARIABLE top   4   CELL- ALLOT

VARIABLE littlest   4   CELL- ALLOT

VARIABLE biggest   4   CELL- ALLOT

VARIABLE sortlist   10004   CELL- ALLOT

: .DATA
  1000 2 DO I SORTLIST + @ .  2 +LOOP ;

#REGS 100 - REG-ADDR $FFC0 AND  UBR!

: do_error
 ." Error3 in Bubble."  CR
   ;

: Initrand    ( --)
  9219  seed  ! ;

: Rand ( -- n)
 seed @ 1309  * 13849  +  seed TUCK_!  ;


: bInitarr   ( --)
 Initrand
 0  biggest !
 0  littlest !
 [ sortlist 2 + ] LITERAL    ( -- i )

 999 FOR
  Rand               ( -- i temp )
  32767  -           ( -- i list )
  OVER ( OVER SWAP ! ) [ $E080 , ]  ( -- i list )
  DUP  biggest @  >
  IF                 ( -- i list )
     biggest !       ( -- i )
  ELSE               ( -- i list )
    DUP  littlest  @ <
    IF               ( -- i list )
       littlest  !   ( -- i )
    ELSE             ( -- i list )
       DROP          ( -- i )
    THEN             ( -- i )
  THEN               ( -- i )
  2  +               ( -- i )
 NEXT                ( -- i )
 DROP
 ;

64 FRAME_SIZE !
: main  ( --)
#REGS 100 - REG-ADDR $FFC0 AND  UBR!
 1   2 U!
 [ sortlist 2 + ] LITERAL   3 U!

 BEGIN
   bInitarr
   [ sortlist 1000 + ] LITERAL    ( -- top )
   BEGIN                        ( -- top )
     DUP  3 U@ U>
   WHILE
       [ sortlist 2 + ] LITERAL   ( -- top i )
       BEGIN     ( -- top i )
          OVER OVER U>
       WHILE     ( -- top i )
          DUP   @+2       ( -- top i  *i i+2)
          @               ( -- top i  *i *i+2 )
          >
          IF   ( exchange *i with *[i+2] )      ( -- top i )
            @+2   @-2  !+2  !-2                 ( -- top i )
          THEN            ( -- top i )
          2 +             ( -- top i+2 )
       REPEAT           ( -- top i )
       DROP             ( -- top )
       2 -             ( -- top )
   REPEAT             ( -- top )
   DROP 
    [ sortlist 2 + ]   LITERAL  @ littlest  @  -
   [ sortlist 1000 + ] LITERAL  @  biggest @   -
   OR IF 
      do_error
   THEN 

   2 U@  1 + [ 2 ] DUP_U!
   29 >
 UNTIL 
 ;

.( max 29) cr
