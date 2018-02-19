\ /* towers.c */
\ GNU C for RTX 2000

EMPTY
: XY " DOS XY towers.b.4th" EVALUATE ;

DECIMAL
load gnutool.4th

#REGS 100 - REG-ADDR $FFC0 AND  UBR!

VARIABLE movesdone   4   CELL- ALLOT
VARIABLE freelist   4   CELL- ALLOT
VARIABLE cellspace   76   CELL- ALLOT
VARIABLE stack   8   CELL- ALLOT

: do_error
   ." Error in Towers." cr
   ;

: Error
   ." Error in Towers." cr
   ;

 64 FRAME_SIZE !
: Makenull

 2*
 [ stack ] SYMBOL_+
 [ 0 ] LIT_SWAP
 !
   ;

 64 FRAME_SIZE !
: Getelement

 freelist
 @
 0>
 [ 005 ] BRANCHZ

 freelist
 @

 DUP
 2*
 2*
 [ cellspace ] SYMBOL_+
 2
 +
 @
 freelist
 !
2 U!
 [ 006 ] BRANCH

 [ 005 ] LABEL

 -64 FP+!
   Error
 64 FP+!
 [ 006 ] LABEL

 2 U@
   ;

 64 FRAME_SIZE !
: Push

 0
 2 U!

 DUP
 2*
 [ stack ] SYMBOL_+
 3 U!
 4 U!
 5 U!
 3 U@
 @
 0>
 [ 008 ] BRANCHZ

 4 U@
 2*
 [ stack ] SYMBOL_+
 @
 2*
 2*
 [ cellspace ] SYMBOL_+
 @
 5 U@
 <=
 [ 108 ] BRANCHZ

 1
 2 U!

 -64 FP+!
   Error
 64 FP+!

 [ 008 ] LABEL
 [ 108 ] LABEL

 2 U@
 [ 0010 ] BRANCHNZ

 -64 FP+!
   Getelement
 64 FP+!

 DUP
 2*
 2*
 [ cellspace ] SYMBOL_+
 4 U@
 2*
 [ stack ] SYMBOL_+
 @_SWAP
 2
 +
 !

 4 U@
 2*
 [ stack ] SYMBOL_+
 OVER
 SWAP !

 2*
 2*
 [ cellspace ] SYMBOL_+
 [ 5 ]  U@_SWAP
 !
 EXIT  [ 0010 ] LABEL
   ;

 64 FRAME_SIZE !
: Init

 OVER
 4 U!              3 U!

 -64 FP+!
   Makenull
 64 FP+!

 3 U@
 [ 5 ] DUP_U!
 0>
 [ 0016 ] BRANCHZ

 [ 0015 ] LABEL
 5 U@
 4 U@

 -64 FP+!
   Push
 64 FP+!
 -1
 5 U@ +   5 U!
 5 U@
 0>
 [ 0015 ] BRANCHNZ

 EXIT  [ 0016 ] LABEL
   ;

 64 FRAME_SIZE !
: Pop

 DUP
 2*
 [ stack ] SYMBOL_+
 SWAP
 2 U!
 @
 0>
 [ 0018 ] BRANCHZ

 2 U@
 2*
 [ stack ] SYMBOL_+
 @

 2*
 2*
 [ cellspace ] SYMBOL_+
 @
 2 U@
 2*
 [ stack ] SYMBOL_+
 @

 2*
 2*
 [ cellspace ] SYMBOL_+
 2
 +
 @
 2 U@
 2*
 [ stack ] SYMBOL_+
 @
 2*
 2*
 [ cellspace ] SYMBOL_+
 freelist
 @_SWAP
 2
 +
 !

 2 U@
 2*
 [ stack ] SYMBOL_+
 @
 freelist
 !

 2 U@
 2*
 [ stack ] SYMBOL_+
 !
 EXIT

 [ 0018 ] LABEL
 -64 FP+!
   Error
 64 FP+!
 EXIT  [ 0017 ] LABEL
   ;

 64 FRAME_SIZE !
: Move

 2 U!

 -64 FP+!
   Pop
 64 FP+!
 2 U@

 -64 FP+!
   Push
 64 FP+!

 1
 movesdone
 +!
   ;

 64 FRAME_SIZE !
: tower

 2 U!
 3 U!
 4 U!

 2 U@
 1
 -
 [ 0022 ] BRANCHNZ

 4 U@
 3 U@

 -64 FP+!
   Move
 64 FP+!
 EXIT

 [ 0022 ] LABEL

 6
 3 U@
 -
 4 U@
 -
 [ 8 ] DUP_U!
 4 U@

 SWAP

 2 U@
 1
 -

 -64 FP+!
 recurse  64 FP+!

 4 U@
 3 U@

 -64 FP+!
   Move
 64 FP+!

 8 U@
 3 U@
 2 U@
 1
 -

 -64 FP+!
 recurse  64 FP+!

 EXIT  [ 0023 ] LABEL
   ;

 64 FRAME_SIZE !
: main
#REGS 100 - REG-ADDR $FFC0 AND  UBR!

 0
 [ 0033 ADD_INDEX ] >R

 [ 0033 ] LABEL

 1
 [ 0031 ADD_INDEX ] >R

 [ 0031 ] LABEL

 [ 0031 ]  INDEX
 2*
 2*
 [ cellspace ] SYMBOL_+
 [ 0031 ]  INDEX
 1
 -
 SWAP
 2
 +
 !
 R>
 1
 +
 DUP_>R
 18
 U>
 [ 0031 ] BRANCHZ
 [ 0031 DROP_INDEX ] R>DROP

 [ 0034 ] LABEL

 18
 freelist
 !

 1
 14

 -64 FP+!
   Init
 64 FP+!

 2

 -64 FP+!
   Makenull
 64 FP+!

 3

 -64 FP+!
   Makenull
 64 FP+!

 0
 movesdone
 !

 1
 2
 14

 -64 FP+!
   tower
 64 FP+!

 movesdone
 @
 16383
 -
 [ 0027 ] BRANCHZ

 -64 FP+!
   do_error
 64 FP+!

 [ 0027 ] LABEL
 R>
 1
 +
 DUP_>R
dup .
 34
 U>
 [ 0033 ] BRANCHZ
 [ 0033 DROP_INDEX ] R>DROP
 EXIT  [ 0035 ] LABEL
   ;

.( max 34) cr
