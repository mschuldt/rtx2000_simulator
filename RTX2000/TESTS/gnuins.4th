DECIMAL
\ pjk tools for supporting GNU C grubby code for RTX 2000
\ 7/90

\ --- instrumented version that counts up number of picks,
\     swaps, and ROTs that aren't combined.


: D+!  DUP >R  D@ D+ R> D! ;
variable dup-count  0 ,
: +dup  1.  dup-count D+! ;
: -dup -1.  dup-count D+! ;
variable over-count  0 ,
: +over  1.  over-count D+! ;
variable swap-count  0 ,
: +swap  1.  swap-count D+! ;
variable rot-count  0 ,
: +rot   1.  rot-count  D+! ;
variable drop-count  0 ,
: +drop  1.  drop-count  D+! ;
variable user@-count  0 ,
: +user@  1.  user@-count  D+! ;
variable user!-count  0 ,
: +user!  1.  user!-count  D+! ;
variable bicompare-count  0 ,
: +bicompare  1.  bicompare-count  D+! ;
variable uncompare-count  0 ,
: +uncompare  1.  uncompare-count  D+! ;
variable branchnz-count  0 ,
: +branchnz  1.  branchnz-count  D+! ;
variable i-count  0 ,
: +i  1.  i-count  D+! ;

\ temporary register space
2500 constant #REGS
CREATE REG-BASE  #REGS 2* ALLOT

VARIABLE FRAME_SIZE   64 frame_size !

: RECURSE  LAST @  >CODE  >TA  U2/ , ;  IMMEDIATE

: REG-ADDR   ( index -- addr )
   DUP #REGS < NOT ABORT" too many REGS"
   2* REG-BASE + ;

#REGS 64 -  REG-ADDR  CONSTANT FP_START

\ label processor
1000 constant #LABELS

\ each label element has 3 possible values:
\  0 = unreferenced
\  <>0 & low bit clear = defined (value is address)
\  <>0 & low bit set = undefined (-value is patch address for branch instruction)
\ This scheme counts on there being at *most*
\           1 unresolved reference to any label
CREATE LABEL-BASE #LABELS 2* ALLOT

LABEL-BASE  #LABELS 2*  0  FILL  \ 0 is unreferenced

: LABEL-ADDR   ( index -- addr )
   DUP #LABELS < NOT ABORT" too many REGS" 
   2* LABEL-BASE + ;

: far_RESOLVE  ( patch_addr target_addr -- )
    SWAP ! ;

: far_LABEL ( n -- )  \ create a reference to a label
  NOOPTIMIZE  OPTIMIZE   ( flush optimizer buffer) 
  LABEL-ADDR  DUP @     ( -- addr value )
  ?DUP IF
    DUP 1 AND  IF   \ resolve reference by patching
                $FFFE AND   HERE  far_RESOLVE
    ELSE  1 ABORT" multiply defined label"  THEN
  THEN    ( -- addr )
  HERE SWAP ! ; IMMEDIATE

: REFER ( n -- addr )  \ get the address to a label
  NOOPTIMIZE  OPTIMIZE   ( flush optimizer buffer) 
  LABEL-ADDR  DUP @     ( -- addr value )
  ?DUP IF  NIP ( -- value )
    DUP 1 AND   IF  1 ABORT" multiple unresolved refs to label"  THEN
  ELSE  \ new label, create unresolved reference
    HERE 2 +  1 OR SWAP !  HERE 
  THEN   ;

: FAR_BRANCHNZ  ( n -- )  \ create a branch-if-not-zero
   2 ALLOT REFER  ( BRANCH )   HERE 2 +
  -2 ALLOT " IF  8888  >R;  THEN "  EVALUATE
  SWAP far_RESOLVE  ; IMMEDIATE

: FAR_BRANCHZ  ( n -- )  \ create a branch-if-zero
  " 0= BRANCHNZ " evaluate  ; immediate

: FAR_BRANCH  ( n -- )  \ create an unconditional branch
  REFER  ( BRANCH )   HERE 2 +
  " 8888  >R; "  EVALUATE
  SWAP far_RESOLVE  ; IMMEDIATE


\ Stuff replicated from kernel

\ extract lower 9 bits
: OFFSET-BITS ( a - a')
   $03FF AND ;

\ extract page bits
: PAGE-BITS ( a - a')
   $FC00 AND ;

\ true if reachable from the current location via branch
: ?REACHABLE ( to from) 
   2+  SWAP PAGE-BITS  SWAP PAGE-BITS -  ABS
   $0400 >  ABORT" unreachable branch target" ;

\ b/s/f returns $600 if backward, $200 if forward, 0 if same page
: B/S/F ( to from - branch_type_bits)
   2+  SWAP PAGE-BITS  SWAP PAGE-BITS -
   DUP IF ( not same page)
      0< IF   ( backward)   $0600
         ELSE ( forward)    $0200
         THEN
   THEN ;

\ resolve the branch
: RESOLVE ( at to)
   SWAP
   DDUP ?REACHABLE
   DDUP B/S/F ( to at branch)
   ROT OFFSET-BITS  2/ OR  ( at offset)
   OVER @  OR  SWAP !  OPTIMIZE ;

: LABEL ( n -- )  \ create a reference to a label
  NOOPTIMIZE  OPTIMIZE   ( flush optimizer buffer) 
  LABEL-ADDR  DUP @     ( -- addr value )
  ?DUP IF
    DUP 1 AND  IF   \ resolve reference by patching
      $FFFE AND 2 - 
        DUP @ $F800 AND  OVER !
       HERE  ( HEX OVER U. DUP U. ." RESOLVE" ) RESOLVE
    ELSE  1 ABORT" multiply defined label"  THEN
  THEN    ( -- addr )
  HERE SWAP ! ; IMMEDIATE

: BRANCHZ  ( n -- )  \ create a branch-if-zero
   REFER   HERE  <?branch> ,   SWAP  RESOLVE   ;  IMMEDIATE

: BRANCH  ( n -- )  \ create an unconditional branch
   REFER   HERE  <branch> ,   SWAP  RESOLVE   ;  IMMEDIATE

: BRANCHNZ  ( n -- )  \ create a branch-if-not-zero
  " +branchnz " evaluate
  " IF [ SWAP ] BRANCH THEN " evaluate  ; immediate


\ : MY_+LOOP   \ Works in a more intuitive manner than Rick's
\  COMPILE <my+loop>  >ta ,  [COMPILE] \\ ;  IMMEDIATE

\ Looping needs to handle nested loops case
: C_DO     DROP  "  DO " EVALUATE ; IMMEDIATE
: C_LOOP   DROP  "  LOOP " EVALUATE ; IMMEDIATE
: C_+LOOP   DROP  "  +LOOP " EVALUATE ; IMMEDIATE

VARIABLE I_VALUE  0 I_VALUE !
VARIABLE J_VALUE  0 J_VALUE !

: ADD_INDEX  ( n -- )
  J_VALUE @  ABORT" Too many index nesting levels"
  I_VALUE @  J_VALUE !
  I_VALUE ! ;

: DROP_INDEX  ( n -- )
  I_VALUE @ = 0= ABORT" Mismatched DROP_INDEX"
  J_VALUE @  I_VALUE !
  0 J_VALUE ! ;

: INDEX  ( n -- )
  DUP  I_VALUE @ =   IF "  I +i " EVALUATE   DROP EXIT  THEN
\  DUP  J_VALUE @ =   IF "  J +i " EVALUATE   DROP EXIT  THEN
  . 1 ABORT" Invalid index value " ; IMMEDIATE


\ Words replicated to RAM for better speed
: 0>  ( n -- f)  +uncompare  0 > ;

: >  +bicompare [ $AAC1 ,  ( over over xor 0< ) ]
    IF   [ $AF61 ,  ( drop not 0< exit ) ]  THEN
         [ $A461 ,  ( swap- 0<    exit ) ]  ;

: <=  +bicompare  [ $AAC1 ,  ( over over xor 0< ) ]
    IF   [ $AE61 ,  ( drop  0<  exit ) ]  THEN
         [ $A441 ,  ( swap- 0< ) ]  not exit ;

: <  +bicompare  [ $AAC1 ,  ( over over xor 0< ) ]
    IF   [ $AE61 ,  ( drop 0< exit ) ]  THEN
         [ $AC61 ,  ( - 0<    exit ) ]  ;

: >=  +bicompare  [ $AAC1 ,  ( over over xor 0< ) ]
    IF   [ $Af61 ,  ( drop not 0< exit ) ]  THEN
         [ $AC41 ,  ( - 0< ) ]   not  ;

: 0<   +uncompare ( n -- f)  $8000 and  IF  -1  exit  then  0 exit ;
: 0=   +uncompare ( n -- f)  IF  0 EXIT  THEN  -1 ;
: 0<>  +uncompare ( n -- f)  IF  -1 EXIT  THEN  0 ;
: =  +bicompare  ( n n -- f)  XOR IF  0 EXIT  THEN  -1 ;
: <>  +bicompare ( n n -- f)  XOR IF -1 EXIT  THEN  0 ;

: UM* ( u u -- ud)
  \ ( RTX 2001 )   MD! 0 0 + 2*' 15 TIMES U*' ;
    ( RTX 2000 )    MULU MLR@ MHR@ ;

: *   ( n n -- n)
  \ ( RTX 2001 )   MD! 0 0 + 2*' 15 TIMES U*' DROP ;
    ( RTX 2000 )    MULU MLR@ MHR@ DROP ;

: UM/MOD ( ud u -- ur uq)
 MD! D2*  U/1' 13 TIMES U/'  U/" SWAP ;

: N>U  ( n -- u f)
 DUP 0< IF  NEGATE 1  ELSE  0  THEN ;
: D>UD  ( d -- ud f)
 DUP 0< IF  DNEGATE 1  ELSE  0  THEN ;

: */MOD ( n1 n2 n3 -- r q)
 N>U >R >R   M*  D>UD R@ SWAP >R
 UM/MOD  R> ( sign)  R> MD! ( divisor)  R@ ( sign) -
 IF  ( signs are different)  OVER
    IF  ( rem <> 0)  1+ SWAP MD@ SWAP - SWAP  THEN
    NEGATE  THEN
 R> IF  SWAP NEGATE SWAP THEN ;  ( rem gets sign of divisor )

: /MOD ( n1 n2 -- r q)  1 SWAP */MOD ;
: MOD ( n1 n2 -- n3)  /MOD DROP ;
: */ ( n1 n2 n3 -- n4)  */MOD NIP ;
: / ( n1 n2 -- q)  /MOD NIP ;


: U>  $A444 ,  $A101 ,  " +bicompare " evaluate ;  IMMEDIATE
: U<  $AC44 ,  $A101 ,  " +bicompare " evaluate ;  IMMEDIATE



: MEM_ARG!  " UBR@  [ 2* frame_size @ - 40 + ] literal +  ! "
       evaluate ; immediate

: MEM_ARG@  dup 31 >  over 0  < or
   IF   " [ 2* 4 +  ] UBR@  literal +  @ " evaluate
  ELSE  $1f AND 20 +  $CE00 OR  ,  
  THEN ; immediate

: TUCK_!  $E080 , ; immediate


: LIT_SWAP  dup  31 >  over 0 < or
  IF    NOOPTIMIZE $D000 ,  ,  OPTIMIZE 
  ELSE  " LITERAL  SWAP " EVALUATE
  THEN ; immediate
: SYMBOL_+  " LITERAL + " EVALUATE ; IMMEDIATE
: SYMBOL_SWAP  " LIT_SWAP " evaluate ; IMMEDIATE
: @_SWAP   $E000 , ; immediate
: OVER_+  $A800 , ; immediate
: CELL- " 2 - " evaluate ; immediate
: fp+!  " UBR@  +  UBR! " evaluate ; immediate

: DUP_U!   $1f AND  $C080 OR ,  " +user!" evaluate ; IMMEDIATE
: U@_SWAP  $1f AND $C000 OR , " +user@" evaluate ; immediate
: U@@  " U@ +user@" evaluate ; immediate
: U!!  " U! +user!" evaluate ; immediate

: ROT  >R SWAP R> SWAP +rot ;
: DUP  DUP +dup ;
: OVER OVER +over ;
: SWAP SWAP +swap ;
: DROP DROP +drop ;

: +!  ( n a --)  DUP >R  @ + R> ! ;

: B@  C@  DUP $80 AND  IF  $FF00 OR  EXIT  THEN ;
: DDUP   OVER OVER ;
: TUCK    SWAP OVER ;
: DUP_>R   " DUP >R " evaluate ;  IMMEDIATE
: R>DROP   " R> DROP " evaluate ;  IMMEDIATE

\ : 1_PUT  " NIP " evaluate ; immediate
\ : 2_PUT  " >R NIP R> SWAP                     " evaluate ; immediate
\ : 3_PUT  SR!  >R >R DROP  SR@  R> R>                    ;
\ : 4_PUT  SR!  >R >R >R DROP  SR@  R> R> R>              ;
\ : 5_PUT  SR!  >R >R >R >R DROP  SR@  R> R> R> R>        ;
\ : 6_PUT  SR!  >R >R >R >R >R DROP  SR@  R> R> R> R> R>  ;

: 2_PICK >R             OVER      R> SWAP ;





\ : 3_PICK   >R >R          OVER SR!  R> R> SR@          ;
\ : 4_PICK   >R >R >R       OVER SR!  R> R> R> SR@        ;
\ : 5_PICK   >R >R >R >R    OVER SR!  R> R> R> R> SR@     ;
\ : 6_PICK   >R >R >R >R >R OVER SR!  R> R> R> R> R> SR@  ;
  
cr cr .( To get instrumented readings, type in INS after the ok prompt) cr cr


: ins   0. over-count D!
         0. dup-count D!
         0. rot-count D!
         0. swap-count D!
         0. drop-count D!
         0. user@-count D!
         0. user!-count D!
         0. bicompare-count D!
         0. uncompare-count D!
         0. i-count D!
         0. branchnz-count D!
       " main" evaluate
 CR  dup-count D@ d. ." DUPs"
 CR  over-count D@ d. ." OVERs"
 CR  swap-count D@ d. ." SWAPs"
 CR  rot-count D@ d. ." ROTs"
 CR  drop-count D@ d. ." DROPs"
 CR  user@-count D@ d. ." U@s"
 CR  user!-count D@ d. ." U!s"
cr cr 
 CR  bicompare-count D@ d. ." binary compares  < > ..."
 CR  branchnz-count D@ d. ." BRANCHNZs"
 CR  i-count D@ d. ." Is"
 CR  uncompare-count D@ d. ." unary compares  0< 0= ..."
 cr cr ;

cr

