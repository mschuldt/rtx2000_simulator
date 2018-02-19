\ /* perm.c */
\ GNU C  2020/3000

EMPTY
: XY " DOS XY PERM.4c" EVALUATE ;

DECIMAL
load GNUTOOL.4

VARIABLE permarray   24   CELL- ALLOT
VARIABLE pctr   4   CELL- ALLOT

#REGS 100 - REG-ADDR $FFC0 AND  UBR!

: do_error
 ." Error in Perm."  cr
   ;

: Swap_el

 1_pick
 @
 1_pick
 @
 3_pick
 !
 SWAP         !
 DROP
   ;

: Initialize

 1
 [ 007 ADD_INDEX ] >R

 [ 007 ] LABEL

 [ 007 ]  INDEX
 2*
 [ permarray ] SYMBOL_+
 [ 007 ]  INDEX
 1
 -
 SWAP !
 R>
 1
 +
 DUP_>R
 7
 U>
 [ 007 ] BRANCHZ
 [ 007 DROP_INDEX ] R>DROP
 EXIT  [ 008 ] LABEL
   ;

: Permute

 1
 pctr
 +!

 [ 2 ] DUP_U!
 1
 -
 [ 0010 ] BRANCHZ

 2 U@
 1
 -

 -64 FP+!
   recurse  64 FP+!

 2 U@
 1
 -
 0_pick
 0>
 [ 0015 ] BRANCHZ

 2 U@
 2*
 4 U!

 [ 0014 ] LABEL

 0_pick
 2*
 4 U@
 [ permarray ] SYMBOL_+
 SWAP
 [ permarray ] SYMBOL_+

   Swap_el

 3 U!
 2 U@
 1
 -

 -64 FP+!
   recurse  64 FP+!

 3 U@   0_pick
 2*
 4 U@
 [ permarray ] SYMBOL_+
 SWAP
 [ permarray ] SYMBOL_+

   Swap_el

 -1     +
 0_pick
 0>
 [ 0014 ] BRANCHNZ

DROP
 [ 0015 ] LABEL
 EXIT  [ 0010 ] LABEL
   ;

: main
#REGS 100 - REG-ADDR $FFC0 AND  UBR!

 1
 [ 0021 ADD_INDEX ] >R

 [ 0021 ] LABEL

 0
 pctr
 !

   Initialize

 7

 -64 FP+!
   Permute
 64 FP+!

 pctr
 @
 8660
 -
 [ 0019 ] BRANCHZ
 -64 FP+!
   do_error
 64 FP+!

 [ 0019 ] LABEL
 R>
 1
 +
 DUP_>R
dup . 250
 U>
 [ 0021 ] BRANCHZ
 [ 0021 DROP_INDEX ] R>DROP

 EXIT  [ 0022 ] LABEL
   ;

.( max 250) cr
