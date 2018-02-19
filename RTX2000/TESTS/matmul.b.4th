\ /* matmul.c */
\ GNU C for RTX 2000

EMPTY
: XY " DOS XY matmul.b.4th" EVALUATE ;

DECIMAL

load gnutool.4th
#REGS 100 - REG-ADDR $FFC0 AND  UBR!

VARIABLE seed   4   CELL- ALLOT
VARIABLE imr   3364   CELL- ALLOT
VARIABLE imb   3364   CELL- ALLOT
VARIABLE ima   3364   CELL- ALLOT

: Initrand
 9219
 seed
 !
   ;

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

: Initmatrix

 1
 2 U!
 3 U!

 [ 0011 ] LABEL

 1
 4 U!
 2 U@
 82
 *
 3 U@  +
 5 U!

 [ 0010 ] LABEL
 -64 FP+!
   Rand
 64 FP+!

 4 U@
 2*
 5 U@  +
 OVER
 120
 /
 120
 *
 ROT
 SWAP-
 60
 -
 SWAP !
 4 U@
 1
 +
 [ 4 ] DUP_U!
 40
 >
 [ 0010 ] BRANCHZ

 [ 0012 ] LABEL
 2 U@
 1
 +
 [ 2 ] DUP_U!
 40
 >
 [ 0011 ] BRANCHZ

 EXIT  [ 0013 ] LABEL
   ;

: Innerproduct

 2 U@
 2 U!
 3 U!
 4 U!

 0
 2_PICK
 !
 1
 5 U!
 6 U!
 7 U!

 5 U@
 40
 <=
 [ 0019 ] BRANCHZ
 3 U@
 82
 *
 6 U@  +
 2 U@
 2*
 8 U!
 9 U!

 [ 0018 ] LABEL
 5 U@
 2*
 9 U@  +
 5 U@
 82
 *
 4 U@  +
 8 U@  +
 @_SWAP
 @_SWAP
 *
 7 U@
 @ +
 7 U@  !
 5 U@
 1
 +
 [ 5 ] DUP_U!
 40
 >
 [ 0018 ] BRANCHZ

 EXIT  [ 0019 ] LABEL
   ;

: main
#REGS 100 - REG-ADDR $FFC0 AND  UBR!

 1
 [ 0032 ADD_INDEX ] >R

 [ 0032 ] LABEL

 -64 FP+!
   Initrand
 64 FP+!

 ima

 -64 FP+!
   Initmatrix
 64 FP+!

 imb

 -64 FP+!
   Initmatrix
 64 FP+!

 1
 [ 0031 ADD_INDEX ] >R

 [ 0031 ] LABEL

 1
 2 U!
 [ 0031 ]  INDEX
 82
 *
 3 U!

 [ 0030 ] LABEL
 2 U@
 2*
 [ imr ] SYMBOL_+
 2 U@
 [ 2 ] MEM_ARG!
 3 U@  +
 ima
 imb
 [ 0031 ]  INDEX

 -64 FP+!
   Innerproduct
 64 FP+!
 2 U@
 1
 +
 [ 2 ] DUP_U!
 40
 >
 [ 0030 ] BRANCHZ

 [ 0033 ] LABEL
 R>
 1
 +
 DUP_>R
 40
 U>
 [ 0031 ] BRANCHZ
 [ 0031 DROP_INDEX ] R>DROP
 [ 0034 ] LABEL
 R>
 1
 +
 DUP_>R
dup .
 24
 U>
 [ 0032 ] BRANCHZ
 [ 0032 DROP_INDEX ] R>DROP

 EXIT  [ 0035 ] LABEL
   ;

.( max 24) cr
