\ /* towers.c */
\ GNU C  2020/3000

EMPTY
: XY " DOS XY towers.c.4th" EVALUATE ;

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
 exit
 [ 006 ] BRANCH

 [ 005 ] LABEL

 -64 FP+!
   Error
 64 FP+!
 [ 006 ] LABEL

 0
   ;

 64 FRAME_SIZE !
: Push

 0

 1_pick
 2*
 [ stack ] SYMBOL_+
 @
 0>

 [ 008 ] BRANCHZ

 1_pick
 2*
 [ stack ] SYMBOL_+
 @
 2*
 2*
 [ cellspace ] SYMBOL_+
 @
 3_pick
 <=
 [ 108 ] BRANCHZ

 1
 1_PUT

 -64 FP+!
   Error
 64 FP+!

 [ 008 ] LABEL
 [ 108 ] LABEL

 [ 0010 ] BRANCHNZ

   Getelement

 0_pick
 2*
 2*
 [ cellspace ] SYMBOL_+
 2_pick
 2*
 [ stack ] SYMBOL_+
 @_SWAP
 2
 +
 !

 1_pick
 2*
 [ stack ] SYMBOL_+
 1_pick
 SWAP !

 2*
 2*
 [ cellspace ] SYMBOL_+
 2_pick    SWAP
 !
 [ 0010 ] LABEL

  drop
  drop

   ;

 64 FRAME_SIZE !
: Init

 OVER

   Makenull

 0_pick
 0>
 [ 0016 ] BRANCHZ

   SWAP 4 U!

 [ 0015 ] LABEL

 [ 5 ] DUP_U!
 4 U@

   Push
 -1
 5 U@ +
 0_pick
 0>
 [ 0015 ] BRANCHNZ

  drop   exit

 [ 0016 ] LABEL
 drop drop
   ;

 64 FRAME_SIZE !
: Pop

 0_pick
 2*
 [ stack ] SYMBOL_+
 @
 0>
 [ 0018 ] BRANCHZ

 0_pick
 2*
 [ stack ] SYMBOL_+
 @

 2*
 2*
 [ cellspace ] SYMBOL_+
 @
 1_pick
 2*
 [ stack ] SYMBOL_+
 @

 2*
 2*
 [ cellspace ] SYMBOL_+
 2
 +
 @
 2_pick
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

 2_pick
 2*
 [ stack ] SYMBOL_+
 @
 freelist
 !

 2_pick
 2*
 [ stack ] SYMBOL_+
 !
 NIP
 EXIT

 [ 0018 ] LABEL
  DROP

 -64 FP+!
   Error
 64 FP+!
 EXIT  [ 0017 ] LABEL
   ;

 64 FRAME_SIZE !
: Move

 2 U!

   Pop
 2 U@

   Push

 1
 movesdone
 +!
   ;

 64 FRAME_SIZE !
: tower

 0_pick
 1
 -
 [ 0022 ] BRANCHNZ

 DROP

 -64 FP+!
   Move
 64 FP+!
 EXIT

 [ 0022 ] LABEL

 6
 2_pick
 -
 3_pick
 -
 [ 8 ] DUP_U!
 SWAP  2 U!  SWAP  3 U!
 OVER  4 U!

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

   Makenull

 3

   Makenull

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
 dup .  34
 U>
 [ 0033 ] BRANCHZ
 [ 0033 DROP_INDEX ] R>DROP
 EXIT  [ 0035 ] LABEL
   ;

.( max 34) cr
