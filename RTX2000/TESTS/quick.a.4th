\ /* quick.c */
\ CESYS C version

EMPTY
: XY " DOS XY quick.a.4th" EVALUATE ;

DECIMAL
load gnutool.4th

 100 REG-ADDR $FFC0 AND  UBR!

VARIABLE seed   4   CELL- ALLOT
VARIABLE top   4   CELL- ALLOT
VARIABLE littlest   4   CELL- ALLOT
VARIABLE biggest   4   CELL- ALLOT
VARIABLE sortlist   10004   CELL- ALLOT

: .DATA
  1000 2 DO I SORTLIST + @ .  2 +LOOP ;

: do_error   ." Error in Quick." cr  ;


: Initrand
  $2403 seed ! ;

: Rand
  seed @ $51d  * $3619 + seed !
  seed @ ;

: Initarr
  17 g@ 64 + 17 g!
    Initrand
  17 g@ 64 - 17 g! 
  $0 biggest !
  $0 littlest !
  $1 0 u!

[ 43 ] label
  0 u@ $1388  <= 1 and
  [ 44 ] branchz
  
  17 g@ 64 + 17 g!
    Rand 
  17 g@ 64 - 17 g!
  1 u! 
  1 u@ $8064 +   sortlist 0 u@ 2* + !
  sortlist 0 u@ 2* + @ biggest @  > 1 and
  [ 46 ] branchz

  sortlist 0 u@ 2* + @ biggest !
  [ 47 ] branch

[ 46 ] label
  littlest @ sortlist 0 u@ 2* + @  > 1 and
  [ 48 ] branchz

  sortlist 0 u@ 2* + @ littlest !

[ 48 ] label
[ 47 ] label
[ 45 ] label
  0 u@ $1 + 0 u!
  [ 43 ] branch

[ 44 ] label
 ;

: Quicksort
  0 u! 1 u! 2 u!
  1 u@ 3 u! 2 u@ 4 u!
  0 u@ 1 u@ 2 u@ + $2  / 2* + @ 5 u!

[ 50 ] label
[ 53 ] label

  5 u@ 0 u@ 3 u@ 2* + @  > 1 and
  [ 54 ] branchz

  3 u@ $1 + 3 u!
  [ 53 ] branch

[ 54 ] label
[ 55 ] label
  0 u@ 4 u@ 2* + @ 5 u@  > 1 and
  [ 56 ] branchz

  4 u@ $ffff + 4 u!
  [ 55 ] branch

[ 56 ] label
  3 u@ 4 u@  <= 1 and
  [ 57 ] branchz

  0 u@ 3 u@ 2* + @ 6 u!
  0 u@ 4 u@ 2* + @ 0 u@ 3 u@ 2* + !
  6 u@ 0 u@ 4 u@ 2* + !
  3 u@ $1 + 3 u!
  4 u@ $ffff + 4 u!

[ 57 ] label
[ 52 ] label
  3 u@ 4 u@  <= 1 and
  [ 58 ] branchz

  [ 50 ] branch

[ 58 ] label
[ 51 ] label
  4 u@ 1 u@  > 1 and
  [ 59 ] branchz

  4 u@ 1 u@ 0 u@ 
  17 g@ 64 + 17 g!
RECURSE   17 g@ 64 - 17 g!

[ 59 ] label
  2 u@ 3 u@  > 1 and
  [ 60 ] branchz

  2 u@ 3 u@ 0 u@ 
  17 g@ 64 + 17 g!
RECURSE   17 g@ 64 - 17 g!

[ 60 ] label
 ;

: main
 100 REG-ADDR $FFC0 AND  UBR! $0 0 u!

[ 62 ] label
  $32 0 u@
dup .
  > 1 and
  [ 63 ] branchz
  
  17 g@ 64 + 17 g!
    Initarr 
  17 g@ 64 - 17 g!

  $1388 $1 sortlist 
  17 g@ 64 + 17 g!
    Quicksort 
  17 g@ 64 - 17 g!

  [ sortlist 2 + ] literal @ littlest @  <> 1 and dup
  [ 67 ] branchz

  [ 66 ] branch

[ 67 ] label
   drop
  [ sortlist 10000 + ] literal @ biggest @  <> 1 and

[ 66 ] label
  [ 65 ] branchz
  
  17 g@ 64 + 17 g!
    do_error
  17 g@ 64 - 17 g!

[ 65 ] label
[ 64 ] label

  0 u@ $1 + 0 u!
  [ 62 ] branch

[ 63 ] label   ;

.( max ) $32 . cr
