\ /* Eratosthenes Siee Prime Number Program in C from Byte January 1983 */
\ GNU C  2020/3000

EMPTY
: XY " DOS XY sieve.c.4th " EVALUATE ;

load gnutool.4th

VARIABLE flags   8192   CELL- ALLOT

DECIMAL
 #REGS 100 - REG-ADDR $FFC0 AND  UBR!

: do_error
  ." Error in Sieve."   CR
   ;

: main
 #REGS 100 - REG-ADDR $FFC0 AND  UBR!

 0

 1
 [ 0019 ADD_INDEX ] >R

 [ 0019 ] LABEL

 DROP  0

 0
 [ 009 ADD_INDEX ] >R

 [ 009 ] LABEL

 1
 flags
 [ 009 ]  INDEX +
 C!
 R>
 1
 +
 DUP_>R
 8190
 U>
 [ 009 ] BRANCHZ
 [ 009 DROP_INDEX ] R>DROP
 [ 0023 ] LABEL

 0
 [ 0018 ADD_INDEX ] >R

 [ 0018 ] LABEL

 flags
 [ 0018 ]  INDEX +
 C@
 [ 0012 ] BRANCHZ

 [ 0018 ]  INDEX
 2*
 3
 +
 [ 0018 ]  INDEX
 OVER_+

 8190
 1_PICK  >
 [ 0021 ] BRANCHZ

 [ 0017 ] LABEL

 0
 flags
 2_PICK +   
 C!

 OVER_+
 0_PICK
 8190
 >
 [ 0017 ] BRANCHZ

 [ 0021 ] LABEL

 DROP DROP
 1  +

 [ 0012 ] LABEL
 R>
 1
 +
 DUP_>R
 8190
 U>
 [ 0018 ] BRANCHZ
 [ 0018 DROP_INDEX ] R>DROP

 [ 0022 ] LABEL
 R>
 1
 +
 DUP_>R
dup .  349
 U>
 [ 0019 ] BRANCHZ
 [ 0019 DROP_INDEX ] R>DROP

 [ 0024 ] LABEL

 1899
 -
 [ 0020 ] BRANCHZ
   do_error
 EXIT  [ 0020 ] LABEL
   ;

.( max 349) cr
