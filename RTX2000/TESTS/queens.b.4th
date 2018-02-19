\ /* queens.c */
\ GNU C for RTX 2000

EMPTY
: XY " DOS XY queens.4b" EVALUATE ;

DECIMAL
load GNUTOOL.4

VARIABLE tries   4   CELL- ALLOT

#REGS 100 - REG-ADDR $FFC0 AND  UBR!

64 frame_size !
: do_error
 ." Error in Queens." cr
   ;

64 frame_size !
: Try

 [ 2 ] MEM_ARG@
 2 U!
 3 U!
 4 U!
 [ 3 ] MEM_ARG@

 1
 tries
 +!

 OVER
 0
 SWAP
 !

 0

 5 U!
 6 U!
 [ 7 ] DUP_U!

 SWAP
 8 U!
 @
 [ 0010 ] BRANCHNZ

 [ 009 ] LABEL
 5 U@
 8
 -
 [ 004 ] BRANCHZ

 5 U@
 1
 +

 0
 7 U@  !

 DUP
 2*
 3 U@  +
 SWAP
 5 U!
 @
 [ 003 ] BRANCHZ
 8 U@
 5 U@  +
 DUP
 2*
 4 U@  +
 SWAP
 9 U!
 @
 [ 103 ] BRANCHZ
 8 U@
 5 U@
 -
 DUP
 2*
 2 U@  +
 SWAP
 10 U!
 14
 +
 @
 [ 203 ] BRANCHZ

 8 U@
 2*
 6 U@  +
 [ 5 ]  U@_SWAP
 !

 5 U@
 2*
 3 U@  +
 [ 0 ] LIT_SWAP
 !

 9 U@
 2*
 4 U@  +
 [ 0 ] LIT_SWAP
 !

 10 U@
 2*
 2 U@  +
 14
 +
 0
 SWAP !

 8 U@
 7
 <=
 [ 006 ] BRANCHZ

 2 U@
 [ 2 ] MEM_ARG!
 6 U@
 [ 3 ] MEM_ARG!
 8 U@
 1
 +
 7 U@
 4 U@
 3 U@

 -64 FP+!
  recurse   64 FP+!

 7 U@
 @
 [ 303 ] BRANCHNZ

 5 U@
 2*
 3 U@  +
 [ -1 ] LIT_SWAP
 !

 9 U@
 2*
 4 U@  +
 [ -1 ] LIT_SWAP
 !

 10 U@
 2*
 2 U@  +
 14
 +
 -1
 SWAP !

 [ 403 ] BRANCH

 [ 006 ] LABEL
 -1
 7 U@  !

 [ 003 ] LABEL
 [ 103 ] LABEL
 [ 203 ] LABEL
 [ 303 ] LABEL
 [ 403 ] LABEL
 7 U@
 @
 [ 009 ] BRANCHZ

 [ 0010 ] LABEL
 EXIT  [ 004 ] LABEL
   ;

192 frame_size !
: Doit

 -7
 2 U!

 [ 0017 ] LABEL

 2 U@
 0>
 [ 0014 ] BRANCHZ
 2 U@
 8
 <=
 [ 0114 ] BRANCHZ
 2 U@
 2*
 UBR@ +
 [ -1 ] LIT_SWAP
 28
 -
 !
 [ 0014 ] LABEL
 [ 0114 ] LABEL

 2 U@
 1
 >
 [ 0015 ] BRANCHZ
 2 U@
 2*
 UBR@ +
 [ -1 ] LIT_SWAP
 64
 -
 !

 [ 0015 ] LABEL
 2 U@
 7
 <=
 [ 0016 ] BRANCHZ
 2 U@
 2*
 UBR@ +
 [ -1 ] LIT_SWAP
 82
 -
 !
 [ 0016 ] LABEL

 2 U@
 1
 +
 [ 2 ] DUP_U!

 16
 >
 [ 0017 ] BRANCHZ

 [ 0019 ] LABEL

 UBR@
 96
 -
 [ 2 ] MEM_ARG!
 UBR@
 116
 -
 [ 3 ] MEM_ARG!
 1
 UBR@
 120
 -
 UBR@
 64
 -
 UBR@
 28
 -

 -192 FP+!
   Try
 192 FP+!

 [ -62 ] MEM_ARG@
 [ 0018 ] BRANCHNZ
 -192 FP+!
   do_error
 192 FP+!
 1000
 tries
 +!
 EXIT  [ 0018 ] LABEL
   ;

64 frame_size !
: main
#REGS 100 - REG-ADDR $FFC0 AND  UBR!

 1
 [ 0025 ADD_INDEX ] >R

 [ 0025 ] LABEL

 0
 tries
 !

 -64 FP+!
   Doit
 64 FP+!

 tries
 @
 113
 -
 [ 0023 ] BRANCHZ
 -64 FP+!
   do_error
 64 FP+!
 [ 0023 ] LABEL
 R>
 1
 +
 DUP_>R
dup .
 2500
 U>
 [ 0025 ] BRANCHZ
 [ 0025 DROP_INDEX ] R>DROP
 EXIT  [ 0026 ] LABEL
   ;

.( max 2500) cr
