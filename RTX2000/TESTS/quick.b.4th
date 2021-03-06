\ /* quick.c */
\ GNU C for RTX 2000

EMPTY
: XY " DOS XY quick.b.4th" EVALUATE ;

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

 64 FRAME_SIZE !
: Initrand
 9219
 seed
 !
   ;

 64 FRAME_SIZE !
: Rand

 seed
 @
 1309
 *
 13849
 +
 seed

 TUCK_!
   ;

 64 FRAME_SIZE !
: Initarr

 -64 FP+!
   Initrand
 64 FP+!

 0
 biggest
 !
 0
 littlest
 !

 1
 [ 0011 ADD_INDEX ] >R

 [ 0011 ] LABEL

 -64 FP+!
   Rand
 64 FP+!

 [ 0011 ]  INDEX
 2*
 [ sortlist ] SYMBOL_+
 SWAP
 32668
 -
 SWAP !

 [ 0011 ]  INDEX
 2*
 [ sortlist ] SYMBOL_+
 @
 biggest
 @
 >
 [ 008 ] BRANCHZ
 [ 0011 ]  INDEX
 2*
 [ sortlist ] SYMBOL_+
 @
 biggest
 !
 [ 007 ] BRANCH

 [ 008 ] LABEL
 [ 0011 ]  INDEX
 2*
 [ sortlist ] SYMBOL_+
 @
 littlest
 @
 <
 [ 107 ] BRANCHZ
 [ 0011 ]  INDEX
 2*
 [ sortlist ] SYMBOL_+
 @
 littlest
 !
 [ 007 ] LABEL
 [ 107 ] LABEL

 R>
 1
 +
 DUP_>R
 5000
 U>
 [ 0011 ] BRANCHZ
 [ 0011 DROP_INDEX ] R>DROP

 EXIT  [ 0012 ] LABEL
   ;

 64 FRAME_SIZE !
: Quicksort

 OVER
 2 U!
 [ 3 ] DUP_U!
 2 U@
 3 U@  +
 4 U!
 5 U!
 6 U!
 7 U!
 4 U@
 0<
 [ 0014 ] BRANCHZ

 1
 4 U@ +   4 U!
 [ 0014 ] LABEL
 4 U@
 -2
 AND
 7 U@  +
 @
 8 U!

 [ 0015 ] LABEL
 2 U@
 2*
 7 U@  +
 @
 8 U@
 <
 [ 0028 ] BRANCHZ

 [ 0020 ] LABEL
 2 U@
 1
 +
 DUP
 2*
 7 U@  +
 @_SWAP
 2 U!
 8 U@
 >=
 [ 0020 ] BRANCHZ

 [ 0028 ] LABEL
 5 U@
 2*
 7 U@  +
 @
 8 U@
 >
 [ 0027 ] BRANCHZ

 [ 0023 ] LABEL
 5 U@
 1
 -
 DUP
 2*
 7 U@  +
 @
 9 U!
 5 U!
 8 U@
 9 U@
 >=
 [ 0023 ] BRANCHZ

 [ 0027 ] LABEL

 2 U@
 5 U@
 <=
 [ 0017 ] BRANCHZ

 2 U@
 2*
 7 U@  +
 @

 2 U@
 2*
 7 U@  +
 5 U@
 2*
 7 U@  +
 @
 SWAP
 !

 5 U@
 2*
 7 U@  +
 !

 2 U@
 1
 +
 -1
 5 U@ +   5 U!

 2 U!
 [ 0017 ] LABEL

 2 U@
 5 U@
 >
 [ 0015 ] BRANCHZ

 6 U@
 5 U@
 <
 [ 0025 ] BRANCHZ
 7 U@
 6 U@
 5 U@

 -64 FP+!
  recurse  64 FP+!

 [ 0025 ] LABEL

 2 U@
 3 U@
 <
 [ 0026 ] BRANCHZ
 7 U@
 2 U@
 3 U@

 -64 FP+!
 recurse  64 FP+!
 EXIT  [ 0026 ] LABEL
   ;

 64 FRAME_SIZE !
: main
#REGS 100 - REG-ADDR $FFC0 AND  UBR!

 0
 2 U!
 [
 sortlist
 2
 +
 ] LITERAL
 3 U!

 [ 0035 ] LABEL

 -64 FP+!
   Initarr
 64 FP+!

 sortlist
 1
 5000

 -64 FP+!
   Quicksort
 64 FP+!

 3 U@
 @
 littlest
 @
 -
 [ 0034 ] BRANCHNZ
 [
 sortlist
 10000
 +
 ] LITERAL
 @
 biggest
 @
 -
 [ 0032 ] BRANCHZ
 [ 0034 ] LABEL

 -64 FP+!
   do_error
 64 FP+!

 [ 0032 ] LABEL
 2 U@
 1
 +
 [ 2 ] DUP_U!
dup .
 49
 >
 [ 0035 ] BRANCHZ

 EXIT  [ 0036 ] LABEL
   ;

.( max 49) cr
