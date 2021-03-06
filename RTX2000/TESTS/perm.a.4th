\ /* perm.c */
\ CESYS compiled

EMPTY
: XY " DOS XY perm.4a" EVALUATE ;

DECIMAL
load gnutool.4th

VARIABLE permarray   24   CELL- ALLOT
VARIABLE pctr   4   CELL- ALLOT

 100  REG-ADDR $FFC0 AND  UBR!

: do_error  ." Error in Perm."  cr    ;

: Swap_el 
	0 u! 1 u! 0 u@ @ 2 u! 1 u@ @ 0 u@ ! 2 u@ 1 u@ ! ;

: Initialize
  $1 0 u!

[ 42 ] label
  0 u@ $7 <= 1 and 
  [ 43 ] branchz

 0 u@ $ffff + permarray
  0 u@ 2* + !

[ 44 ] label
  0 u@ $1 + 0 u! 
  [ 42 ] branch

[ 43 ] label
  ;

: Permute
  >r pctr @ $1 + pctr ! r@ $1 <> 1 and
  
  [ 46 ] branchz

  r@ $ffff + 
  17 g@ 64 + 17 g!
    RECURSE    17 g@ 64 - 17 g! 
  r@ $ffff + 1 u!

[ 47 ] label
  $1 1 u@ <= 1 and 
  [ 48 ] branchz

  permarray  1 u@ 2* + 
  permarray  r@ 2* + 
  17 g@ 64 + 17 g! 
    Swap_el
  17 g@ 64 - 17 g! 
  r@ $ffff + 
  17 g@ 64 + 17 g! 
    RECURSE    17 g@ 64 - 17 g! 
  permarray  1 u@ 2* + 
  permarray  r@ 2* + 
  17 g@ 64 + 17 g! 
    Swap_el 
  17 g@ 64 - 17 g!

[ 49 ] label
  1 u@ $1 - 1 u! 
  [ 47 ] branch

[ 48 ] label
[ 46 ] label
  r> drop ;

: main
 100  REG-ADDR $FFC0 AND  UBR!
  $1 0 u!

[ 51 ] label
  0 u@
                           dup . 
  $fa <= 1 and 
  [ 52 ] branchz

  $0 pctr ! 
  17 g@ 64 + 17 g! 
    Initialize 
  17 g@ 64 - 17 g! 
  $7 
  17 g@ 64 + 17 g!
    Permute
  17 g@ 64 - 17 g! 
  pctr @ $21d4
  <> 1 and 
  [ 54 ] branchz

  17 g@ 64 + 17 g!
    do_error
  17 g@ 64 - 17 g!

[ 54 ] label
[ 53 ] label
  0 u@ $1 + 0 u! 
  [ 51 ] branch

[ 52 ] label
  ;

.( max ) $fa . cr
