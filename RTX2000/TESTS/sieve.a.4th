\ /* Eratosthenes Siee Prime Number Program in C from Byte January 1983 */
\ CESYS output modified for AppForth

EMPTY
: XY " DOS XY sieve.a.4th " EVALUATE ;

load gnutool.4th

VARIABLE flags   8192   CELL- ALLOT

DECIMAL

: do_error ." Error in Sieve."   CR    ;

: main  ( FUNC )   ( 3  top> empty )
  64 REG-ADDR $FFC0 AND  UBR!

  $1 2 u!
[ 41 ] LABEL
  $15e    2 u@ 
dup .
   > 
  1 and 
  [ 42 ] branchz

  $0 1 u! 
  $0 3 u!

[ 44 ] LABEL
  3 u@    $1ffe <= 
  1 and 
  [ 45 ] branchz
  $1   flags 3 u@ +  c!

[ 46 ] LABEL
  3 u@    $1 +    3 u! 
  [ 44 ] branch

[ 45 ] LABEL
  $0 3 u!

[ 47 ] LABEL

  3 u@    $1ffe    <=
  1 and 
  [ 48 ] branchz

  flags    3 u@ +    c@   c>i
  [ 50 ] branchz

  3 u@  3 u@ +   $3 +   0 u! 
  3 u@  0 u@ +   4 u!

[ 51 ] LABEL
  4 u@    $1ffe    <= 
  1 and 
  [ 52 ] branchz

  $0     flags    4 u@ +   c!

[ 53 ] LABEL
  4 u@  0 u@ +    4 u! 
  [ 51 ] branch

[ 52 ] LABEL
  1 u@  $1 +  1 u!

[ 50 ] LABEL
[ 49 ] LABEL
  3 u@  $1 +  3 u!   
  [ 47 ] branch

[ 48 ] LABEL
[ 43 ] LABEL
  2 u@  $1 +  2 u! 
  [ 41 ] branch

[ 42 ] LABEL
  1 u@ 
  $76b 
  <> 
  1 and 
  [ 54 ] branchz
  64 FP+!
        do_error
  -64 FP+!
[ 54 ] LABEL
  ;

.( max ) $15e . cr
