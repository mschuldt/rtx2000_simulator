\ /* matmul.c */
\ GNU C  2020/3000

EMPTY
: XY " DOS XY matmul.c.4th" EVALUATE ;

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

 [ 0011 ] LABEL

 1
 OVER
 82
 *
 3_pick       +
 SWAP

 [ 0010 ] LABEL
   Rand

 OVER
 2*
 3_pick  +
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
 1
 +
 0_pick
 40
 >
 [ 0010 ] BRANCHZ

DROP   DROP

 [ 0012 ] LABEL
 1
 +
 0_pick
 40
 >
 [ 0011 ] BRANCHZ

DROP DROP
 EXIT  [ 0013 ] LABEL
   ;

: Innerproduct
\         /* computes the inner product of A[row,*] and B[*,column] */

 2 U@

 0
 5_pick
 !
 1

 2_PICK
 82
 *
 5_pick       +
 2_pick
 2*

 [ 0018 ] LABEL
 2_pick
 2*
 2_pick     +
 3_pick
 82
 *
 7_pick       +
 2_pick       +
 @_SWAP
 @_SWAP
 *
 8_pick     +!
 1
 3_pick
 +
 0_PICK
 4_PUT
 40
 >
 [ 0018 ] BRANCHZ

DROP
DROP
DROP
DROP
DROP
DROP
DROP
DROP

 EXIT  [ 0019 ] LABEL
   ;

: main
#REGS 100 - REG-ADDR $FFC0 AND  UBR!

 1
 [ 0032 ADD_INDEX ] >R

 [ 0032 ] LABEL

   Initrand

 ima

   Initmatrix

 imb

   Initmatrix

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
 DUP 2*
 [ imr ] SYMBOL_+
 SWAP
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
dup . 24
 U>
 [ 0032 ] BRANCHZ
 [ 0032 DROP_INDEX ] R>DROP

 EXIT  [ 0035 ] LABEL
   ;

.( max 24) cr
