\ /* bubble.c */
\ CESYS C generated code

EMPTY
: XY " DOS XY BUBBLE.4a" EVALUATE ;

DECIMAL
load GNUTOOL.4

VARIABLE seed   4   CELL- ALLOT
VARIABLE top   4   CELL- ALLOT
VARIABLE littlest   4   CELL- ALLOT
VARIABLE biggest   4   CELL- ALLOT
VARIABLE sortlist   10004   CELL- ALLOT

: .DATA
  1000 2 DO I SORTLIST + @ .  2 +LOOP ;

 100 REG-ADDR $FFC0 AND  UBR!

 : do_error  ( FUNC )   ( 3  top> empty )
 ." Error3 in Bubble."  CR 
   ;  ( END )     ( 13  top> #2x )( RTX 2000 code generation)

: initrand
  $2403 seed ! ;

: rand
  seed @ 
  $51d 
  * 
  $3619 + 
  seed ! 
  seed @ 
  ;

: bInitarr
  17 g@ 64 + 17 g!
  initrand 
  17 g@ 64 - 17 g! 
  $0  biggest ! 
  $0 littlest ! 
  $1 0 u!


[ 43 ] label

  0 u@  $1f4 <= 
  1 and 
  [ 44 ] branchz

  17 g@ 64 + 17 g! 
  rand  
  17 g@ 64 - 17 g! 
  1 u! 
  1 u@ $8001 + 
    sortlist  0 u@  2* + ! 
  sortlist  0 u@ 2* + @
  biggest  @  > 
  1 and 
  [ 46 ] branchz 

  sortlist 0 u@   2* +  @
  biggest ! 
  [ 47 ]  branch

[ 46 ] label
  littlest @ 
  sortlist   0 u@ 2* + @ 
  >  1 and
  [ 48 ] branchz
  sortlist   0 u@ 2* + @ 
  littlest !

[ 48 ] label

[ 47 ] label

[ 45 ] label
 0 u@ $1 + 0 u! 
  [ 43 ] branch

[ 44 ] label
 ; 

: main

 100 REG-ADDR $FFC0 AND  UBR! $1 2 u!

[ 50 ] label
  $1e  
  2 u@  
dup . 
  > 1 and 
  [ 51 ] branchz
  17 g@ 64 + 17 g!
  bInitarr 
  17 g@ 64 - 17 g! 
  $1f4 top !

[ 53 ] label
  top @ $1  > 1 and 
  [ 54 ] branchz
  $1 0 u!

[ 55 ] label
  top @ 0 u@  > 1 and 
  [ 56 ] branchz
  sortlist   0 u@ 2* +  @ 
  sortlist   0 u@ $1 + 2* + @ 
  > 1 and 
  [ 57 ] branchz
  sortlist   0 u@ 2* + @  1 u! 
  sortlist   0 u@ $1 + 2* + @ 
  sortlist   0 u@ 2* + ! 
  1 u@  sortlist  0 u@ $1 + 2* + !

[ 57 ] label
  0 u@ $1 + 0 u! 
  [ 55 ] branch

[ 56 ] label
  top @  $ffff  +  top ! 
  [ 53 ] branch

[ 54 ] label
  [ sortlist 2 + ] literal @ 
  littlest @ 
  <> 1 and dup 
  [ 60 ] branchz
  [ 59 ] branch

[ 60 ] label
  drop [ sortlist 1000 + ] literal @ biggest @ 
  <> 1 and
[ 59 ] label
  [ 58 ] branchz
  17 g@ 64 + 17 g! 
    do_error
  17 g@ 64 - 17 g!

[ 58 ] label

[ 52 ] label
  2 u@ $1 + 2 u! 
  [ 50 ] branch

[ 51 ] label
 ;

.( max ) $1e . cr
