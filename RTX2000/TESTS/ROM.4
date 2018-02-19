\ COPYRIGHT 1990 HARRIS CORPORATION  ALL RIGHTS RESERVED
\ Rick VanNorman Wed  04-18-1990  09:35:46

\ --------- Meta compiler

VARIABLE H'  ( dictionary pointer for target system )
   0 ,  ( offset for compilation)

: TEST-ROM       0 H' 2+ ! ;
: MAKE-ROM   $A000 H' 2+ ! ;

\ use TEST-ROM or MAKE-ROM to define the compilation type

\ MAKE-ROM

: ROM/RAM
   CR ." Compile for Rom? y/n (default is YES) "   KEY  DUP 32 MAX EMIT
   CR ." System being compiled for "
   UPPER ASCII N = IF
      ." RAM."    TEST-ROM
   ELSE
      ." ROM."    MAKE-ROM
   THEN ;

ROM/RAM

\ ----------

: [INLINE]   $DE00 , ;  IMMEDIATE
: HIDE    SMUDGE ;
: REVEAL  UNSMUDGE ;
: RECLAIM RECOVER ;
: LABEL   HEADER ;

\ ---------- META WORDS

: CASE   " OVER = IF DROP "  EVALUATE ; IMMEDIATE
: ESAC   " EXIT THEN "      EVALUATE ; IMMEDIATE

: ,"
   ASCII " WORD  COUNT  DUP C, ( a #)
   0 DO  DUP C@ C,  1+  LOOP DROP ;

\ ----------

: SAVE-IMAGE ( a # SAVE filename)
   BL WORD COUNT CREATE-FILE ( handle) >R
   BEGIN ( a #)
      DUP 1024 > WHILE
      OVER 1024 R@ WRITE-FILE DROP
      SWAP 1024 + SWAP 1024 -
   REPEAT
   R@ WRITE-FILE DROP
   R> CLOSE-FILE ;

: .EVALUATE ( a #)
   DDUP  CR TYPE  EVALUATE ;

: ?SAVE
   " H' 2+ @ $A000 = " EVALUATE
   IF
      " DOS COPY T.BIN Z.BIN" .EVALUATE
      " $A000 $4000 SAVE-IMAGE T.BIN" .EVALUATE
   THEN ;

CREATE SAVE #THREADS CELLS ALLOT

VARIABLE KEEPH

\ save the current state of FORTH for restoration after metacompilation
<forth> SAVE #THREADS CELLS CMOVE    HERE KEEPH !


: {
   dA @ HERE  H' D@ H ! dA !  H' D!  ALIGN ;

: }
   STATE @  IF  { { H' @  H' 2+ @ - U2/ , IS-CODE THEN { ; IMMEDIATE

\ ----------

VARIABLE RAM
VARIABLE 'ALU-X
VARIABLE 'SHIFT-X

\ aliases with which to reach ROM words
: GAP           ALLOT ;
: ALLOT         NEGATE  RAM +! ;
: OLD,          , ;
: OLD-C,        C, ;
: HHERE         HERE 2- >ta ; IMMEDIATE
: OLDCREATE     CREATE ;
: OLDIMM        IMMEDIATE ;
: OLD>ta        >ta ;
: OLDliteral    <literal> ;
: T'            ' >ta ;
: OLD.          U. ;

: VARIABLE    2 ALLOT  HEADER  $DE20 ,  RAM @ , ;

: SILENT
   2 ALLOT
   {    \ header goes in host
      HEADER  H' @  H' 2+ @ - U2/ , IS-CODE
   {    \ body goes in target
      $DE20 , RAM @ , ;

\ ----------

: STATS
   BASE @ >R HEX
      CR
      CR ." Code: " HERE $A000 - 5 U.R    ."  bytes"
      CR ." Data: " RAM @ NEGATE 5 U.R    ."  bytes"
      CR
   R> BASE ! ;

\ ----------

: SCAN ( a1-a2)             ( search for a target word )
   @  BEGIN DUP  1 $A000 WITHIN  WHILE @  REPEAT ;

: TRIM ( a1 a2 - a2 )   ( scans a linked list for target words)
   DUP  >R >ta           ( compute target address )
   SWAP  !  R>           ( store corrected address )
   DUP L>NAME 1+ DUP C@ $07F AND SWAP C! ; ( reveal)

: CLIP ( a)         ( relinks target words to separate dict )
   DUP BEGIN DUP SCAN DUP WHILE TRIM REPEAT SWAP ! @ , ;

: PRUNE             ( relink the target dictionary )
   { HERE #THREADS 0 DO  <forth>  R@ 2* +  CLIP LOOP {
   SAVE <forth> #THREADS CELLS CMOVE KEEPH @ H ! ( rest host dic) ;


\ ----------

\ prep the compilation area
$A000 $4000 $FF FILL   $A020 H' ! 0 RAM !

{   \ Kernel starts here

\ ---------

{ : <do> }  SWAP R> SWAP >R SWAP >R >R; ; RECLAIM

{ : <loop> }
   R>           \ retadr                \R limit index
   R> 1+ DUP    \ retadr index' index'  \R limit
   R@ SWAP >R   \ retadr index' limit   \R limit index'
   XOR IF       \ retadr                \R limit index'
      ( continue looping)
      @ >R;
   THEN
   ( exit loop)
      R>DROP  R>DROP
      2+ >R;  ; RECLAIM

{ : <+loop> }
   R> SWAP              \ ret incr                \r limit index
   DUP 0< IF            \ negative increment
      R@ +              \ ret i'
      DUP R@            \ ret i' i' i
      SWAP-             \ ret i' x
      R> R@ - 1+        \ ret i' x y
      - cU2/ 0<
      IF                \ done with loop
         DROP R>DROP    \ ret
         2+ >R;
      THEN              \ continue looping
      >R                \ ret                   \r limit index
      @ >R;
   THEN                 \ positive increment
   R@ +                 \ ret index'            \r limit index
   DUP R> -             \ ret i' x              \r limit
   OVER R@ -            \ ret i' x y            \r limit
   - cU2/ 0<
   IF                   \ done with loop
      DROP R>DROP       \ ret
      2+ >R;
   THEN                 \ continue looping
   >R                   \ ret                   \r limit index
   @ >R; ;

\ ---------

\ host words                                     13:56 09/20/88

{
   : DO  ( limit index )
      COMPILE SWAP COMPILE >R COMPILE >R  HERE ; IMMEDIATE

   : LOOP
      COMPILE <loop>  >ta ,  [COMPILE] \\ ;  IMMEDIATE

   : +LOOP
      COMPILE <+loop>  >ta , [COMPILE] \\ ;  IMMEDIATE

}

\ ---------       

\ initialized variables
SILENT   FLINE#
VARIABLE FENCE
VARIABLE VLINK
VARIABLE dA
VARIABLE STATE
SILENT   'KEY?
SILENT   'EMIT?
SILENT   'KEY
SILENT   'EMIT
SILENT   'header
SILENT   'abort
SILENT   'idle
SILENT   -RETURN
SILENT   -OPT
VARIABLE BASE

\ non-initialized
SILENT   COLD
VARIABLE SPAN
SILENT   hld
SILENT   LAST-ADDR
VARIABLE #TIB
VARIABLE H
SILENT   OPTIMIZING
SILENT   STEP
VARIABLE DPL
VARIABLE #OUT
VARIABLE >IN
VARIABLE CURRENT
VARIABLE CONTEXT
VARIABLE LAST
VARIABLE CPU-TYPE
SILENT   bal
SILENT   leaf
VARIABLE HANDLE
SILENT   LINE#

\ data allocations
130 ALLOT  VARIABLE 'TIB
130 ALLOT  VARIABLE BUFF
 14 ALLOT  VARIABLE <forth>
542 ALLOT  VARIABLE INT-VECTORS   ( 32 17 * 2 - )

VARIABLE MATCHING

\ misc constants
    8 CONSTANT #THREADS
    2 CONSTANT CELL
  $20 CONSTANT BL
$A020 CONSTANT RETURN

 0 CONSTANT FALSE
-1 CONSTANT TRUE

\ compiler constants
$8800 CONSTANT <?branch>
$9000 CONSTANT <branch>
$9800 CONSTANT <next>

\ ----------

\ the rom'ed initial values for the FORTH vocabulary
CREATE LINKS   16 GAP

\ ----------

: 0=  ( n -- f)  IF  0 EXIT  THEN  -1 ;
: 0<> ( n -- f)  IF  -1 EXIT  THEN  0 ;
: =  ( n n -- f)  XOR 0= ;
: <> ( n n -- f)  = NOT ;
: <=  ( n n -- f)  > NOT ;
: >=  ( n n -- f)  < NOT ;
: ?DUP  ( n -- n n OR 0 -- 0)  DUP  IF DUP THEN ;
: ABS  ( n -- u)  DUP 0<  IF NEGATE THEN ;
: CELL+ ( n -- n+2) 2+ ;
: CELLS ( n -- n*2) 2* ;
: ROT  ( a b c -- b c a)  >R SWAP R> SWAP ;
: -ROT  ( a b c -- b c a)  SWAP >R SWAP R> ;
: +!  ( n a --)  DUP >R  @ + R> ! ;
: U<  ( u u -- f)  - DROP 0 0 -c ;
: 0>  ( n -- f)  0 > ;
: WITHIN ( n l h -- f)  OVER - >R - R> U< ;
: TUCK  ( n1 n2 - n2 n1 n2)  SWAP OVER ;
: CYCLES  ( n)  FOR NEXT ;
: OFF ( a)  0 SWAP ! ;
: ON ( a)  -1 SWAP ! ;

\ ----------

: DDROP  ( d -)  DROP DROP ;
: DDUP  ( d - d d)  OVER OVER ;
: DSWAP  ( d1 d2 - d2 d1)  ROT >R ROT R> ;
: DOVER  ( d1 d2 - d1 d2 d1)   D>R DDUP   DR> DSWAP ;
: DROT  ( d1 d2 d3 - d2 d3 d1)   D>R DSWAP   DR> DSWAP ;
: D@  ( a - d)  @+2 @ SWAP ;
: D!  ( d a)  !+2 ! ;

: D+  ( d d -- d)  >R SWAP >R + R> R> +c ;
: D-  ( d d -- d)  >R SWAP >R - R> R> -c ;

: D<  ( d1 d2 -- f)  D- NIP 0< ;

: DNEGATE ( d -- -d)  SWAP NEGATE SWAP 0SWAP-c ;

: D0=  ( d -- f)  OR 0= ;
: D=  ( d1 d2 -- f)  SWAP >R XOR  SWAP  R> XOR D0= ;
: D<>  ( d d -- f)  D= NOT ;
: D0<  ( d -- f)  NIP 0< ;

: DU<  ( d d - f)
  ROT DDUP =  IF  -ROT  DNEGATE  D+ 0<
              ELSE  SWAP U< NIP  THEN  NIP ;

: DABS  ( d -- d)  DUP 0< IF DNEGATE THEN ;

: DMAX  ( d d - d)  DOVER DOVER D<  IF DSWAP THEN  DDROP ;
: DMIN  ( d d - d)  DOVER DOVER D< NOT  IF DSWAP THEN  DDROP ;

: D>  ( d d - f)  DSWAP D< ;

\ ---------

{ : N>U } ( n -- u f)
  DUP 0< IF  NEGATE 1  ELSE  0  THEN ;
{ : D>UD } ( d -- ud f)
  DUP 0< IF  DNEGATE 1  ELSE  0  THEN ;

: DSQRT ( ud -- root )
  32768 SR!  0 MD!  D2*  S1'  13 TIMES S'  S"  DROP ;
: SQRT  ( u -- root )  0 DSQRT ;

: M*  ( n n -- d)
  STEP @ IF  CR@ DUP DUP 2* 0< $10 AND OR >R  8 AND $10 OR CR!
             MD!  0 $0E TIMES *' *"  R> CR!  EXIT THEN
          MULS MLR@ MHR@  ;

: * ( n n -- n)  M* DROP ;
: UM* ( u u -- ud)
   STEP @ IF  MD! 0 0 + 2*' 15 TIMES U*' EXIT THEN
   MULU MLR@ MHR@ ;

: UM/MOD ( ud u -- ur uq)
  MD! D2*  U/1' 13 TIMES U/'  U/" SWAP ;

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

: MIN ( n n -- n)  DDUP > IF NIP EXIT  THEN  DROP ;
: MAX ( n n -- n)  DDUP < IF NIP EXIT  THEN  DROP ;

\ --------- UART INTERFACE

: REG@  ( r -- n)
   0 $018 G!                  ( Quiet pattern )
   DUP  $018 G!               ( Select uart register )
   NOP                       ( wait 1 PCLK cycle )
   $1000 OR DUP $018 G!        ( DISTR [RD] true )
   NOP NOP NOP               ( wait 3 PCLK cycles )
   $018 G@ $00FF AND           ( read data from In port 0 )
   SWAP
   $EFFF AND $018 G!           ( DISTR [RD] false )
   0000  $018 G! NOP ;        ( Quiet pattern )

: REG!  ( n r)
   SWAP $FF AND SWAP
   0 $018 G!                 ( Quiet pattern )
   DUP $018 G! NOP           ( Select UART register )
   OR                       ( Build data/reg mask, select uart)
   $2000 OR  DUP             ( Set write bit )
   $018 G!  NOP NOP          ( DOSTR [WR] true )
   $DFFF  AND                ( Clear write bit )
   $018 G!  NOP              ( DOSTR [WR] false )
   0 $018 G!  NOP   ;        ( Quiet pattern )

: INIT-8250
    $080 $0700 REG!    ( set DLAB bit )
    $008 $0100 REG!    ( set baud rate at 9600 )
    $000 $0300 REG!    ( 2.4576MHz / 16 = 8    ==>19200 baud )
    $003 $0700 REG!    ( 8-data, 1-stop, no parity )
    $003 $0900 REG!    ( Disable OUT1, OUT2; clear RTS, DTR )
   $0100 REG@ DROP   ( clear receiver data ready interrupt )
   $0B00 REG@ DROP   ( clear receiver errors interrupt )
   $0D00 REG@ DROP   ( clear modem status changes interrupt )
   $0500 REG@ DROP   ( clear THRE interrupt )
     $00 $0300 REG! ;  ( enable no interrupts )

\ --------- BAUD RATE

: BAUD ( baud )
  $25800. ROT UM/MOD NIP           \ calculate divisor
  $080 $700 REG!                    \ set DLAB bit
       $100 REG!                    \ set baud rate
  000 $0300 REG!
  003 $0700 REG! ;  ( 8-data, 1-stop, no parity )

\ ---------

{ : 256* }  2* 2* 2* 2*  2* 2* 2* 2* ;
{ : 256/ }  2/ 2/ 2/ 2/  2/ 2/ 2/ 2/ ;

: RCV? ( - f)    $0B00 REG@    1 AND ;
: XMT? ( - f)    $0B00 REG@  $20 AND ;

: RCV ( - c)    BEGIN RCV?  UNTIL  $0100 REG@ ;
: XMT ( c -)    BEGIN XMT?  UNTIL  $0100 REG! ;

: #RCV ( - n)    RCV RCV 256* + ;
: #XMT ( n -)    DUP XMT 256/ XMT ;

: RCVS ( a #)
   ?DUP IF 0 DO  RCV OVER C!  1+  LOOP  THEN DROP ;
: XMTS ( a #)
   ?DUP IF 0 DO  DUP C@ XMT   1+  LOOP  THEN DROP ;

: CMD ( n)   0 XMT XMT ;

\ ----------

{ : 0>BL } ( char - char)   DUP 0= IF BL OR THEN ;

: KEY   'KEY @ EXECUTE ;
: EMIT   0>BL  'EMIT @ EXECUTE  1 #OUT +! ;

: KEY?   'KEY? @ EXECUTE ;
: EMIT?  'EMIT? @ EXECUTE ;

: CR   13 EMIT 10 EMIT  0 #OUT ! ;

: TYPE ( a #)
   FOR  -ZERO
      C@+1 SWAP EMIT
   THEN NEXT DROP ;

: SPACE  BL EMIT ;
: SPACES ( n)   0 MAX  FOR -ZERO SPACE THEN NEXT ;

\ --------- Convert binary to ASCII string

: HERE  ( -- a )  H @ ;

: PAD  ( -- a )  HERE 128 + ;

{ : PICKINGS }   HERE  1024 + ;

: TIB   'TIB @ ;

: HOLD ( c - ) ( Add c to output string )
  -1 hld +!  hld @ C! ;

: DIGIT ( n - c)  ( Convert a number n to its ASCII equivalent)
  DUP  9 > 7 AND +  48 + ;

: SIGN  ( n - ) ( Append - sign to output string)
  0<  IF 45 HOLD THEN ;

: <#   PAD hld !    ; ( Prepare to start conversion )

: #  ( d - d')   ( Convert one digit from d )
  0 BASE @  DUP >R  UM/MOD  R> SWAP >R UM/MOD SWAP R> SWAP
  DIGIT HOLD ;

: #S  ( d - 0) ( Convert d til reduced to 0 )
  BEGIN  #  DDUP D0=  UNTIL ;

: #>  ( d - a n)     ( Output the converted string to terminal)
  DDROP  hld @ PAD OVER - ;

\ ----------

{ : B. } ( c)  0 <# # # #>      TYPE SPACE ;

: (d.)  ( d - a c)
   TUCK  DABS <# #S ROT SIGN #> ;

: D.   ( d)    (d.) TYPE SPACE ;
:  .   ( n)    DUP 0< D. ;
: U.   ( u)    0 D. ;

: D.R  ( d n)   >R  (d.) R>  OVER - SPACES  TYPE ;
: U.R  ( u n)   0 SWAP D.R ;

: ? ( a)  @ . ;
            
\ ----------

: DECIMAL  10 BASE ! ;
: HEX      16 BASE ! ;
: BINARY    2 BASE ! ;

\ ---------

: CMOVE  ( s d c)
   FOR -ZERO
      >R  C@+1 SWAP  R> C!+1
   THEN NEXT DDROP ;

: CMOVE> ( s d c)
   DUP >R + 1- SWAP R@ + 1- SWAP R>
   FOR -ZERO
      >R  C@-1 SWAP  R> C!-1
   THEN NEXT DDROP ;

: COUNT ( a - a n)
   C@+1 SWAP ;

: FILL  ( a c n)
   SWAP ?DUP  IF
      >R  SWAP R> 1- TIMES DUPC!+1
   THEN  DDROP ;

: ERASE ( a c )  0 FILL ;
: BLANK  ( a c)  BL FILL ;

: /STRING ( a # n - a #)   ROT OVER + -ROT - ;

: SKIP ( a # char - a #)
   >R BEGIN DUP WHILE OVER C@ R@ - IF  R>DROP EXIT  THEN
      1 /STRING  REPEAT  R>DROP ;

: SCAN ( a # char - a #)
   >R BEGIN DUP WHILE OVER C@ R@ = IF  R>DROP EXIT  THEN
      1 /STRING  REPEAT  R>DROP ;

: -TRAILING ( a # - a #)
   DDUP BEGIN  DDUP BL SKIP  DUP WHILE  DSWAP DDROP
      BL SCAN  REPEAT  DDROP  NIP - ;

\ put a string at an address
: $PLACE ( f # t)
   DDUP C!  1+ SWAP CMOVE ;

\ append a string to an existing string at an address
: $APPEND ( f # t)
   >R  R@ COUNT + ( f # t')  OVER
   R@ C@ + R> C!  SWAP CMOVE ;

: ALIGN   HERE  1 AND  H +! ;
: EVEN ( a - a)   DUP 1 AND + ;

\ ---------

{ : (")  }  R> COUNT  DDUP       + EVEN >R;   ; RECLAIM
{ : dot" }  R> COUNT  DDUP TYPE  + EVEN >R;   ; RECLAIM

\ ---------- META WORDS, EXECUTE ON HOST

{  : "    COMPILE (")   ASCII " STRING ; IMMEDIATE
   : ."   COMPILE dot"  ASCII " STRING ; IMMEDIATE }

\ ----------

: ABORT
   'abort @ EXECUTE ;

{ : <abort"> }
   IF    HERE COUNT TYPE SPACE R> COUNT TYPE ABORT  THEN
   R> COUNT  + EVEN >R;  ; RECLAIM

\ ---------- META WORDS, EXECUTE ON HOST

{  : ABORT"  COMPILE <abort"> ASCII " STRING ; IMMEDIATE }

\ ----------

: huh?   0= ABORT" ?" ;

: UPPER ( char - char)
   DUP  ASCII a ASCII z 1+ WITHIN IF  32 -  THEN ;

\ ---------

: -DIGIT  ( c base - n f)
   >R  DUP  ASCII 9 >
   IF  DUP ASCII @ >  7 AND  -  THEN
   ASCII 0 -  DUP R> U<  ;

: CONVERT ( +d a - +d a)
   BEGIN  1+ DUP >R C@ UPPER  BASE @ -DIGIT
   WHILE  SWAP BASE @ UM* DROP ROT BASE @ UM* D+  R>
   REPEAT DROP R> ;

: (VAL?) ( a # - d 2 , n 1 , 0)
   PAD 1- OVER - TUCK >R CMOVE
   BL PAD 1- DUP DPL ! C!  0 0 R>
   DUP C@  ASCII - =  DUP >R  - 1-
   BEGIN CONVERT  DUP C@  DUP ASCII : =
      SWAP ASCII , ASCII / 1+ WITHIN OR
   WHILE  DUP DPL ! REPEAT
   R> SWAP >R IF DNEGATE THEN
   PAD 1- DPL @ - 1- DPL !  R> PAD 1- = ( valid)
   IF  DPL @ 0< IF DROP 1 ELSE 2 THEN ELSE DDROP 0 THEN ;

: VAL? ( a # - d 2 , n 1 , 0)
   BASE @ >R
      OVER C@  ASCII $ = IF  HEX     1 /STRING  ELSE
      OVER C@  ASCII # = IF  DECIMAL 1 /STRING  ELSE
      OVER C@  ASCII % = IF  BINARY  1 /STRING  THEN THEN THEN
      (VAL?)
   R> BASE ! ;

\ ---------

: PICK  ( n -- s[n])
  MD@ >R  SR@ >R           ( Save registers)
  MD! PICKINGS MD@    TIMES !+2
  2- DUP SR!
  MD@  TIMES @-2 DROP SR@ @
  R> SR!  R> MD! ;         ( restore regs)

: ROLL  ( n -- s[n])
  ?DUP IF
     MD@ >R  SR@ >R           ( Save registers)
     MD! PICKINGS MD@    TIMES !+2
     2- DUP SR!
     2- MD@  1- TIMES @-2 DROP SR@ @
     R> SR!  R> MD!           ( restore regs)
  THEN ;

\ ---------

: DEPTH  ( -- n)
   0 SPR@+ $0FF AND  CPU-TYPE @ 2 <  IF  3  ELSE  1  THEN  - ;

: ?STACK
   DEPTH 0< ABORT" Stack underflow " ;

: ?COMP  STATE @ 0= ABORT" is compilation only" ;

: .S
   ?STACK
   CR  DEPTH FOR -ZERO  R@ PICK U.  THEN NEXT  ." <top " ;

\ ---------

: L>NAME ( a1 - a2)
   2+ ;

: >CODE ( a1 - a2 )
   L>NAME  COUNT $1F AND  +  EVEN ;

: >NAME ( cfa - nfa)
   BEGIN 1- DUP C@ $080 AND UNTIL DUP 1 AND - ;

: >LINK ( cfa - lfa)
   >NAME 2- ;

\ ----------

: PREVIOUS ( - a #)
   LAST @  L>NAME ;

: USE ( a - )
   LAST @  >CODE ! ;

\ ---------

: SOURCE ( - a #)   TIB #TIB @ ;
: /SOURCE ( - a #)   SOURCE >IN @ /STRING ;
: ACCEPT ( n f)   IF 1+ THEN >IN +! ;

: WORD ( c - a)
   >R /SOURCE OVER R> SWAP >R >R R@ SKIP OVER SWAP R> SCAN
   OVER R> - SWAP ACCEPT OVER - HERE DUP >R
   DDUP C!  1+ SWAP CMOVE  BL R@ COUNT + C!  R> ;

\ ---------

: COMPARE ( a1 a2 # - flag)
   FOR  -ZERO ( nfa text)
      OVER C@ UPPER  OVER C@ UPPER
      <> IF  DDROP R>DROP  0 EXIT  THEN
      SWAP 1+ SWAP 1+
   THEN  NEXT
   DDROP -1 ;

\ ----------

: >HASH ( saddr - n )  1+ C@ 7 AND 2* 2+ $10 MOD ;

\ is the namefield the same as the text?
{ : SAME? ( text nfa - f) }
   OVER C@  OVER C@ $1F AND <> IF  DDROP 0 EXIT  THEN
   1+  SWAP  COUNT COMPARE ;

: thread ( thread string - cfa true | string false)
   DUP >HASH  ROT + @ ( string link)
   BEGIN ( s link)
      DUP WHILE ( s link)
      OVER OVER L>NAME
      SAME? IF ( s link)
         NIP  DUP >CODE  SWAP  L>NAME C@
         $40 AND IF  1  ELSE  -1  THEN
         EXIT
      THEN
      @
   REPEAT ;

\ search dictionary for a match with the packed name at  a .
\ Return execution address and -1 or 1 ( IMMEDIATE) if found;
\ a 0  if not found.

: FIND ( a - a 0 | a - w -1 | a - w 1)
   DUP C@ ( a l) DUP
   IF  31 MIN OVER C! ( a)
      CONTEXT @ SWAP thread ( a -1/0/1) DUP
      IF  EXIT  THEN
      CONTEXT @ <forth> <>
      IF  DROP  <forth> SWAP thread  THEN   EXIT
   THEN ;

: '  ( - a n)  BL WORD  FIND huh? ;

\ ---------

{ : NULL@COL } ( col - col)
   0 OVER HERE + C! ;

{ : @CHAR } ( col - col char)
   DUP HERE + C@ ;

{ : ~ENCODE } ( ... char - adr)
   BASE @ >R  DECIMAL
   <#  BL HOLD
   ?DUP ( non-zero)
   IF   ( was control)  DUP 64 XOR HOLD  DUP
   ELSE ( was special)  KEY 0 #S
   THEN
   ASCII ~ HOLD  #>
   ( a #)   SWAP OVER ( # a #)  PAD 2 + SWAP CMOVE
   1- PAD 1+ C!  PAD 1+
   R> BASE ! ;

{ : ~STROKE } ( ... max col char - max col)
   ~ENCODE ( ... a) FIND IF EXECUTE  ELSE DROP  THEN ;

{ : STROKE } ( ... col char - col)
   OVER HERE + C@ 0= >R
   DDUP EMIT  HERE + C!  1+
   R> IF  NULL@COL  THEN ;

: STROKES ( col a # - col)
   ?DUP IF ( col a #)
      0 DO ( col a)
         DUP >R  C@ STROKE  R> 1+
      LOOP
   THEN DROP ;

: EXPECT ( a #)
   HERE OVER 2 + ERASE  ( text is accumulated at HERE)
   0 BEGIN ( ... max col)
      DDUP > WHILE
      KEY  DUP 32 -  95 U<
      IF STROKE  ELSE ~STROKE  THEN
   REPEAT  SPAN !
   HERE ROT ROT CMOVE ;

\ escape key -- kill the current line
: ~[  ( col - 0)
   BEGIN
      DUP WHILE  8 EMIT 32 EMIT 8 EMIT  1- REPEAT
   NULL@COL ;

\ backspace key
: ~H ( max col - max col)
   DUP IF   8 EMIT 32 EMIT 8 EMIT  1-  THEN  NULL@COL ;

: ~M ( max col - max col)
   SPACE MIN DUP ;

\ ---------

: DOS-COMMAND ( a #)
   $30 CMD DUP #XMT XMTS RCV DROP ;

: SHELL   CR ." Type EXIT to return to App-Forth ... "
          " COMMAND " DOS-COMMAND ;
: DOS     0 WORD COUNT DOS-COMMAND ;

\ note: uses the address where word deposits text!
{ : DO-DOS }
   HERE COUNT BUFF $PLACE ( copy text that word parsed)
   "  " BUFF $APPEND  0 WORD COUNT BUFF $APPEND
   BUFF COUNT DOS-COMMAND ;

\ a standard set of dos commands

: A:       DO-DOS ;  \ dos
: B:       DO-DOS ;  \ dos
: C:       DO-DOS ;  \ dos
: D:       DO-DOS ;  \ dos
: CD       DO-DOS ;  \ dos
: COPY     DO-DOS ;  \ dos
: DEL      DO-DOS ;  \ dos
: DIR      DO-DOS ;  \ dos
: DIR/W    DO-DOS ;  \ dos
: RENAME   DO-DOS ;  \ dos
: EDIT     CR CR DO-DOS ;  \ generic utility name

: BYE   255 CMD ;

\ ----------

: LISTING
   BL WORD COUNT ( a #)  DUP huh?
   $2D CMD  DUP ( #) #XMT  XMTS
   RCV ( file opened) huh?
   RCV ( file closed) huh? ;

: LIST ( n)
   0 MAX  DUP LINE# !
   $2E CMD  #XMT
   RCV ( file opened) huh?
   RCV ( file closed) huh?
   #RCV LINE# ! ;

: L   LINE# @ LIST CR ;

: N   16 LINE# +!  L ;
: B  -16 LINE# +!  L ;

: ~73   B ;
: ~81   N ;

: ~71      0 LIST ;
: ~79  32767 LIST ;

: ~132   -1 LINE# +! L ;
: ~118    1 LINE# +! L ;

\ --------- WORDS SUPPORT

: ?PAUSE
   KEY? IF  KEY  27 = IF  'idle @ EXECUTE  THEN
            KEY  27 = IF  'idle @ EXECUTE  THEN   THEN ;

{ : SCAN>UPPER } ( a # char - a #)
   >R BEGIN DUP WHILE OVER C@ UPPER R@ = IF  R>DROP EXIT  THEN
      1 /STRING  REPEAT  R>DROP ;

\ Search for a pattern in a body of text.  if flag is
\ true on exit, a match was found and its address is
\ returned.  otherwise, the address is meaningless
: SEARCH ( pattern # text # - addr f)
   3 PICK C@ UPPER
   BEGIN  DUP >R SCAN>UPPER  DUP  WHILE
      DOVER DOVER DROP SWAP COMPARE
      IF ( match!)
         DROP >R DDROP R>  R>DROP  -1 EXIT  THEN
      1 /STRING  R>
   REPEAT ( p # t #)
   DSWAP DDROP R>DROP ;

: ?line ( n)
   #OUT @ +  72 > IF CR THEN ;

: ID. ( nfa)
   COUNT  31 AND  DUP ?line
   ( #) 0 DO  C@+1 SWAP  $7F AND EMIT  LOOP  DROP  SPACE ;

\ if we are matching and the name contains the pattern, display
\ name.  if not matching, always display the name
{ : ?.NAME } ( nfa)
   MATCHING @ IF
      DUP  HERE COUNT  ROT COUNT 31 AND SEARCH NIP
      0= IF  DROP EXIT  THEN
   THEN   ID. SPACE  1 PAD 2- +! ;

{ : LARGEST } ( a n -- a' n')
   OVER 0 SWAP ROT
   FOR -ZERO  DDUP @ U<  IF -ROT DDROP DUP @ OVER THEN
      CELL+  THEN NEXT DROP ;

{ : ANOTHER } ( - nfa)
   PAD #THREADS LARGEST DUP
   IF  DUP @ ROT ! L>NAME  ELSE  NIP  THEN ;

: WORDS
   BL WORD C@ 0<> MATCHING !   0 PAD 2- !
   CONTEXT @  PAD  #THREADS CELLS CMOVE
   CR BEGIN  ?PAUSE ANOTHER ?DUP WHILE ?.NAME REPEAT
   CR ." Found " PAD 2- ? ." words. " ;

\ ---------

: CLIP   ( target link - )  \ all threads of a vocabulary
  #THREADS  1-  FOR  SWAP OVER @
     BEGIN  DDUP SWAP U< NOT  WHILE @  REPEAT
     >R OVER R> SWAP !  SWAP 2+  NEXT  DDROP ;

: CROP   ( target - )  \ all vocabs and words thru target
  VLINK @  BEGIN  DDUP U< WHILE @  REPEAT VLINK !
  VLINK @  BEGIN  DUP  WHILE  DDUP 2+ @ CLIP @  REPEAT  DDROP ;

: FORGET
  ' DUP $8000 U<  IF ." Can't forget a protected word "  DROP
   ELSE  >LINK  DUP  CROP  H !  THEN  ;

: EMPTY  FENCE @ DUP CROP H ! ;

\ ---------

: ALLOT ( n)
   MATCHING $100 -  OVER -  HERE U< ABORT" Dictionary full"   H +! ;

\ this will probably break if a literal is comma'ed and compilation
\ resumes immediately
: ,  ( n)
  STATE @  IF  HERE LAST-ADDR !  0 -OPT !
     -RETURN @ 1- 0 MAX -RETURN !  THEN  2 ALLOT  HERE 2- ! ;

: C, ( n)   1 ALLOT  $0FF AND  HERE 1- C! ;

: RECOVER  -2 ALLOT ;

\ ---------

\ build a dictionary header out of the following blank delimited text
\ and link it into the CURRENT vocabulary.  allocate no executable code
: <header>
   ALIGN
   HERE LAST !  0 ,
   BL WORD              \ a
   COUNT                \ a+1 #
   DUP huh?             \ a+1 #
   HERE >R              \ a+1 #         \R nfa
   $1F AND DUP
   $80 OR C,            \ a+1 #         \R nfa
   0 DO                 \ a+1
      DUP C@ C,  1+
   LOOP DROP            \               \R nfa
   ALIGN
   R> >HASH CURRENT @ + \ thread
   LAST @               \ thread link
   OVER @ OVER !        \ thread link
   SWAP ! ;

: HEADER
   'header @ EXECUTE ;

: CREATE
   HEADER $BE27 , ;     ( pc@ operation)

\ ---------

: >ta  ( a1 - a2)  dA @ - ;

: SHIFT ( value count -- value')
   ?DUP
   IF  DUP 0<
      IF    NEGATE 1- TIMES U2/
      ELSE  1- TIMES 2*
      THEN
   THEN ;

{ : DOES }  ( - )  R> U2/ USE ;
{ : DOES>  COMPILE DOES  COMPILE R>  NOP ; IMMEDIATE }

\ ---------

: STRING ( char)
   WORD  COUNT  DUP C, ( a #)
   0 DO  DUP C@ C,  1+  LOOP DROP
   ALIGN ;

: <literal> ( n)
   DUP 0 <  OVER $1F > OR
   IF  $DE00 , ,  -2 LAST-ADDR +!
   ELSE  $BE40 OR , THEN ;

: LITERAL ( n)
  STATE @ IF  <literal>  THEN ;  IMMEDIATE

: VAL, ( .... n n #)
   DUP BEGIN  ROT >R           1- ?DUP 0= UNTIL
       BEGIN      R> <literal> 1- ?DUP 0= UNTIL ;

\ ---------

: [']  ?COMP ' >ta <literal> ; IMMEDIATE  HIDE
: [COMPILE]  ?COMP  '  >ta U2/ , ; IMMEDIATE HIDE
: COMPILE  ?COMP  R> DUP 2+ >R @ ,  ;

: ASCII
  BL WORD 1+ C@
  STATE @ IF  <literal>  THEN ; IMMEDIATE HIDE

: ."
  COMPILE dot"  ASCII " STRING ;   IMMEDIATE HIDE
: "
   COMPILE (") ASCII " STRING ; IMMEDIATE
: ABORT"
  COMPILE <abort">  ASCII " STRING ;  IMMEDIATE HIDE

\ ---------

\ THIS IS *NOT* A ROMABLE STRUCTURE!!!! MUST BE FIXED!!!!

: VOCABULARY
   CREATE  HERE  VLINK @ , VLINK !
   HERE 2+ DUP , #THREADS CELLS DUP ALLOT ERASE
   DOES>  2+ @  CONTEXT ! ;

: DEFINITIONS ( -- )  CONTEXT @ CURRENT ! ;

{ : DOFORTH } R>  2+ @ CONTEXT ! ;
: FORTH  DOFORTH [ 0 OLD, <forth> OLD,   REVEAL


\ ---------

{ : INIT-LINKS } \ jam the forth threads
   [  LINKS  OLD>ta OLDliteral ] <forth> #THREADS CELLS CMOVE
   FORTH DEFINITIONS ;

: VOCS
   CR  VLINK @
   BEGIN  DUP  WHILE
      DUP  ['] FORTH 2+ = IF ." FORTH "
         ELSE DUP 2 - >LINK L>NAME ID. THEN
      @  REPEAT  DROP ;

\ ---------

: @L ( page addr - n)  DPR@ >R SELDPR SWAP DPR!  @  R> DPR! ;
: !L ( n page addr)    DPR@ >R SELDPR SWAP DPR!  !  R> DPR! ;
: C@L ( page addr - c) DPR@ >R SELDPR SWAP DPR!  C@  R> DPR! ;
: C!L ( c page addr)   DPR@ >R SELDPR SWAP DPR!  C!  R> DPR! ;

: CMOVEL ( sourcepage sourceoffset destpage destoffset c )
  ?DUP IF
   1- FOR  DOVER C@L >R  DDUP  R>  -ROT  C!L
      1+ DSWAP 1+ DSWAP  NEXT
  THEN DDROP DDROP  ;

: EXECUTEL ( p a)  >R  IPR! NOP ;

\ ---------

{ : PRINTABLE } ( char - char)
   DUP 32 127 WITHIN NOT IF DROP ASCII . THEN ;

{ : DUMP-HEADER } ( a)
   CR  7 SPACES  15 FOR DUP 15 AND 3 U.R 1+ NEXT
       2 SPACES  15 FOR DUP 15 AND 1 U.R 1+ NEXT DROP
   CR  8 SPACES  15 FOR  ." -- "  NEXT ;

{ : DUMP-BYTES } ( page addr)
   15 FOR  DDUP C@L B. 1+  NEXT  DDROP ;

{ : DUMP-ASCII } ( page addr)
   15 FOR  DDUP C@L  PRINTABLE EMIT  1+ NEXT  DDROP ;

{ : DUMP-LINE } ( page addr)
   CR   DDUP SWAP 6 D.R ASCII : EMIT SPACE
   DDUP DUMP-BYTES SPACE DUMP-ASCII ;

: DUMPL ( page addr #)
   BASE @ >R  HEX
      OVER DUMP-HEADER  16 / 0 MAX  FOR ( page addr)
         ?PAUSE  DDUP DUMP-LINE  16 +
      NEXT  DDROP
   R> BASE ! ;

: DUMP ( a #)
   DPR@ -ROT DUMPL ;

\ ---------

: DISABLE ( interrupts)  $0010 CR@OR  CR! ;
: ENABLE ( interrupts)    CR@ CR! ;
: PSUNDER  ." Parameter Stack Underflow " CR
  CPU-TYPE @ 2 <  IF  0     \ 2000 - for par. stack underflow
    ELSE  0 SPR@+ $FF00 AND SPR! THEN ;  \ 2010/2001A

: >VECTOR  ( n--a)  15 SWAP-  2* 2* 2* 2* 2*  INT-VECTORS + ;

: SET-INT-VECTORS
  RETURN  $0D >VECTOR
    $0E FOR  DDUP  !  $20 +  NEXT  DDROP  ( ! NOP RET )
  ['] PSUNDER  2 >R  U2/ R@ >VECTOR ! RETURN R> >VECTOR 2+ ! ;

: !INTERRUPT  ( a n)
  >VECTOR  DUP >R  SWAP  >ta U2/  SWAP !  RETURN  R> 2+ ! ;

: @INTERRUPT  ( n -- a)
  >VECTOR  @  2* ;

: SET-INT-MASK
  IBC@ $0EFF AND  INT-VECTORS  $F800 AND  OR  IBC!
  $FFFE IMR!  ( FFFA = Parameter Stack Underflow unmasked )
  $F0F0 SLR!  ( Set stack limits register) ;

: MASK ( n)  IMR@ OR IMR! ;
: UNMASK ( n)  -1 XOR IMR@ AND IMR! ;
: MASKED? ( n - n')  IMR@ AND ;

   2 CONSTANT EI1         $400 CONSTANT EI3
   4 CONSTANT PSU         $800 CONSTANT EI4
   8 CONSTANT RSU        $1000 CONSTANT EI5
 $10 CONSTANT PSV        $2000 CONSTANT SWI
 $20 CONSTANT RSV
 $40 CONSTANT EI2
 $80 CONSTANT TIMER0
$100 CONSTANT TIMER1
$200 CONSTANT TIMER2

\ ---------

\ OPTIMIZE  NOOPTIMIZE  NOHWMULT  HWMULT

: OPTIMIZE    OPTIMIZING ON ;     \ Turn on optimization
: NOOPTIMIZE  OPTIMIZING OFF ;    \ Turn off optimization

: NOHWMULT   STEP ON ;      \ Step math
: HWMULT     STEP OFF ;     \ Hardware multiplier

: \\  -OPT ON ; IMMEDIATE HIDE   \ Break optimization

\ ---------

{ : ?lit ( flag) }
   ABORT" literal error" ;

{ : IO-CLASS ( ??? opcode - ??? ) }
   STATE @ IF ( compiling)
      HERE 2- @ DUP  $FFE0 AND $BE40 = NOT ?lit
      RECOVER   $1F AND  OR  ,
   ELSE ( interpreting)
      SWAP  DUP $20 > OVER 0< OR ?lit
      OR , RETURN , -4 ALLOT HERE EXECUTE
   THEN ;

: G! ( n g)    $BE80 IO-CLASS ; IMMEDIATE HIDE
: G@ ( g - n)  $BE00 IO-CLASS ; IMMEDIATE HIDE

: U! ( n u)    $CE80 IO-CLASS ; IMMEDIATE HIDE
: U@ ( u - n)  $CE00 IO-CLASS ; IMMEDIATE HIDE

\ ----------

\ decompiler for rtx 2000

\ ---------- APPLICATION WORDS

: LSLN ( n # - n)
   ?DUP IF  0 DO  2*  LOOP  THEN ;

: LSRN ( n # - n)
   ?DUP IF  0 DO  U2/  LOOP  THEN ;

\ ---------- DECOMPILER: MAY BE REMOVED, FROM HERE TO THE ***XYZZY*** REFERENCE

{ : INDEX ( addr n - addr) }
   ?DUP IF  0 DO  COUNT +  LOOP  THEN ;

{ : $. ( addr index) }
   INDEX COUNT TYPE SPACE ;

{ : PRINT$ ( index) }
   R> SWAP $. ;

\ ----------

SILENT   LOCATION

{ : THIS }
   LOCATION @ @ ;

SILENT   INCREMENT

{ : INCR }
   INCREMENT ! ;

\ ---------- returns

{ : .RET }
   THIS $20 AND  IF  ." exit " THEN ;

\ ---------- invert bit

\ note: no opcode screening here!!!!!!
{ : .not }
   THIS 8 LSRN  $0F AND
   DUP 1 = SWAP 15 = OR IF ." not " THEN ;

\ ---------- calls

{ : HEADER? ( cfa - flag) }
   DUP >NAME COUNT $1F AND  ?DUP IF    + EVEN =
                                  ELSE  DDROP 0  THEN ;

{ : .NAME ( nfa) }
   COUNT $1F AND
   FOR -ZERO
       DUP C@ 32 MAX 127 MIN EMIT 1+
   THEN NEXT
   DROP ;

{ : .PROC }
   THIS 2*  DUP HEADER? IF    >NAME .NAME
                        ELSE  U. ." call"
                        THEN ;

{ : .QUOTE }
   ASCII " EMIT  SPACE
   LOCATION @ 2+ COUNT  DUP 1+ EVEN INCREMENT +!  TYPE
   ASCII " EMIT ;

{ : .LOOP }
   4 INCR
   ." loop " LOCATION @ 2+ @ U. ;

{ : .CALL }
   2 INCR
   THIS
   [INLINE] dot"     CASE ." ."     .QUOTE ESAC
   [INLINE] <abort"> CASE ." abort" .QUOTE ESAC
   [INLINE] (")      CASE           .QUOTE ESAC
   [INLINE] <loop>   CASE       .LOOP  ESAC
   [INLINE] <+loop>  CASE ." +" .LOOP  ESAC
                     DROP .PROC        ;

\ ---------- branches

{ : .TARGET }
   THIS $600 AND  9 LSRN .
   THIS $1FF AND  1 LSLN . ;

{ : .BRANCH }
   2 INCR
   THIS $1800 AND  11 LSRN
   0 CASE  ." ?dup 0branch "  .TARGET  ESAC
   1 CASE  ." 0branch "       .TARGET  ESAC
   2 CASE  ." branch "        .TARGET  ESAC
     DROP  ." next "          .TARGET  ;

\ ---------- strings

{ LABEL $cccc  ] }   PRINT$ [
   0 OLD-C,     \ 0
   0 OLD-C,     \ 1
   ," and"      \ 2
   ," nor"      \ 3
   ," swap-"    \ 4
   ," swap-c"   \ 5
   ," or"       \ 6
   ," nand"     \ 7
   ," +"        \ 8
   ," +c"       \ 9
   ," xor"      \ 10
   ," xnor"     \ 11
   ," -"        \ 12
   ," -c"       \ 13
   0 OLD-C,     \ 14
   0 OLD-C,     \ 15

   0 OLD-C,     \ 0
   0 OLD-C,     \ 1
   ," and"      \ 2
   ," nor"      \ 3
   ," -"        \ 12
   ," -c"       \ 13
   ," or"       \ 6
   ," nand"     \ 7
   ," +"        \ 8
   ," +c"       \ 9
   ," xor"      \ 10
   ," xnor"     \ 11
   ," swap-"    \ 4
   ," swap-c"   \ 5
   0 OLD-C,     \ 14
   0 OLD-C,     \ 15

{ LABEL $shift  ] }   PRINT$ [
   0 OLD-C,     \ 0
   ," 0<"       \ 1
   ," 2*"       \ 2
   ," 2*c"      \ 3
   ," cU2/"     \ 4
   ," c2/"      \ 5
   ," u2/"      \ 6
   ," 2/"       \ 7
   ," n2*"      \ 8
   ," n2*c"     \ 9
   ," d2*"      \ 10
   ," d2*c"     \ 11
   ," cud2/"    \ 12
   ," cd2/"     \ 13
   ," ud2/"     \ 14
   ," d2/"      \ 15

\ ----------

{ : $INLINE }
   R> COUNT TYPE SPACE ;

{ LABEL .dup   ] }   $INLINE  [  ," dup"
{ LABEL .drop  ] }   $INLINE  [  ," drop"
{ LABEL .swap  ] }   $INLINE  [  ," swap"
{ LABEL .over  ] }   $INLINE  [  ," over"
{ LABEL .g@    ] }   $INLINE  [  ," g@"
{ LABEL .u@    ] }   $INLINE  [  ," u@"
{ LABEL .g!    ] }   $INLINE  [  ," g!"
{ LABEL .u!    ] }   $INLINE  [  ," u!"
{ LABEL .nip   ] }   $INLINE  [  ," nip"
{ LABEL .tuck  ] }   $INLINE  [  ," tuck"

\ ----------

{ : .SHIFT }
   THIS $0F AND $SHIFT ;

SILENT SWAPPING

{ : -SWAP }
   16 SWAPPING ! ;

{ : .CCCC }
   THIS 8 LSRN $0F AND  SWAPPING @ +  $CCCC  0 SWAPPING ! ;

{ : .AAA }
   THIS 8 LSRN $0E AND $CCCC ;

\ ----------

{ : NOT/SHIFT/RET }
   .NOT .SHIFT .RET ;

{ : CCCC/SHIFT/RET }
   .CCCC .SHIFT .RET ;

{ : .ALU }
   2 INCR
   THIS $0EC0 AND
      $0000 CASE  NOT/SHIFT/RET  ESAC
      $0E00 CASE  .DROP .DUP               NOT/SHIFT/RET  ESAC
      $0040 CASE  .NIP                     NOT/SHIFT/RET  ESAC
      $0E40 CASE  .DROP                    NOT/SHIFT/RET  ESAC
      $0080 CASE  .NIP .DUP                NOT/SHIFT/RET  ESAC
      $0E80 CASE  .SWAP                    NOT/SHIFT/RET  ESAC
      $00C0 CASE  .DUP                     NOT/SHIFT/RET  ESAC
      $0EC0 CASE  .OVER                    NOT/SHIFT/RET  ESAC  DROP
   THIS $00C0 AND
      $0000 CASE  .OVER -SWAP             CCCC/SHIFT/RET  ESAC
      $0040 CASE                          CCCC/SHIFT/RET  ESAC
      $0080 CASE  .TUCK                   CCCC/SHIFT/RET  ESAC
            DROP  .OVER .OVER             CCCC/SHIFT/RET  ;

\ ---------- registers and i/o

{ : .SHORT }
   THIS $1F AND . ;

{ : NOT/RET }
   .NOT .RET ;

{ : CCCC/RET }
   .CCCC .RET ;


{ : .LIT/REG }
   2 INCR
   THIS $0EC0 AND
      $0000 CASE  .SHORT .G@ .DROP         NOT/RET  ESAC
      $0E00 CASE  .SHORT .G@               NOT/RET  ESAC
      $0080 CASE  .SHORT .OVER .SWAP .G!   NOT/RET  ESAC
      $0E80 CASE  .SHORT .G!               NOT/RET  ESAC
      $0040 CASE  .SHORT .DROP             NOT/RET  ESAC
      $0E40 CASE  .SHORT                   NOT/RET  ESAC
      $0EC0 CASE  .SHORT .NIP              NOT/RET  ESAC DROP
   THIS $00C0 AND
      $0000 CASE  .SHORT ." g@ over "     CCCC/RET  ESAC
      $0080 CASE  .SHORT ." g@ " -SWAP    CCCC/RET  ESAC
      $0040 CASE  .SHORT ." over "        CCCC/RET  ESAC
      $0040 CASE  .SHORT ." over "        CCCC/RET  ESAC
      $00C0 CASE  .SHORT -SWAP            CCCC/RET  ESAC
            DROP  .SHORT ." ???g@ "       CCCC/RET  ;

\ ----------

{ : .FOLLOWING }
   ." lit " LOCATION @ 2+ @ U. ;

{ : .LITERAL }
   4 INCR
   THIS $0EC0 AND
      $0000 CASE  .FOLLOWING .SWAP     .RET ESAC
      $0E00 CASE  .FOLLOWING           .RET ESAC
      $0E80 CASE  .DROP     .FOLLOWING .RET ESAC  DROP
   THIS $00C0 AND
      $0000 CASE  .FOLLOWING .OVER     CCCC/RET  ESAC
      $0080 CASE  .FOLLOWING -SWAP     CCCC/RET  ESAC
            DROP  ." ?long?"           CCCC/RET  ;

\ ---------- user space

{ : .USER }
   2 INCR
   THIS $0EC0 AND
      $0000 CASE   .SHORT .U@ -SWAP            NOT/RET  ESAC
      $0E00 CASE   .SHORT .U@                  NOT/RET  ESAC
      $0080 CASE   .DUP     .SHORT .U!         NOT/RET  ESAC
      $0E80 CASE            .SHORT .U!         NOT/RET  ESAC  DROP
   THIS $00C0 AND
      $0000 CASE   .SHORT .U@ .OVER          CCCC/RET  ESAC
      $0080 CASE   .SHORT .U@                CCCC/RET  ESAC
            DROP   .SHORT ." ?user? "        CCCC/RET  ;

\ ----------

SILENT   BYTES

{ : .CXX }
   BYTES @ IF ." c" THEN ;

{ : .@ }
   .CXX   ." @ " ;
{ : .! }
   .CXX   ." ! " ;

{ : T1 }
   .SHORT -SWAP .AAA .RET ;

{ : T2 }
   .DUP .@ .SWAP T1 ;

{ : .MEM }
   2 INCR
   THIS $0EC0 AND
      $0000 CASE .@ .SWAP              NOT/RET  ESAC
      $0E00 CASE .@                    NOT/RET  ESAC
      $0080 CASE .OVER .SWAP .!        NOT/RET  ESAC
      $0E80 CASE .!                    NOT/RET  ESAC  DROP
   THIS $0FC0 AND
      $0040 CASE .NIP .DUP .@ .SWAP       .RET  ESAC
      $0140 CASE      .DUP .@ .SWAP       .RET  ESAC
      $0E40 CASE .NIP .@ .SHORT           .RET  ESAC
      $0F40 CASE      .@ .SHORT           .RET  ESAC
      $00C0 CASE .OVER .OVER .!           .RET  ESAC
      $01C0 CASE .TUCK       .!           .RET  ESAC
      $0EC0 CASE .OVER .SWAP .! .SHORT    .RET  ESAC
      $0FC0 CASE             .! .SHORT    .RET  ESAC  DROP
   THIS $00C0 AND
      $0000 CASE .@ .OVER             CCCC/RET  ESAC
      $0080 CASE .@ -SWAP             CCCC/RET  ESAC DROP
   THIS $01C0 AND
      $0040 CASE .NIP T2 ESAC
      $0140 CASE      T2 ESAC
      $00C0 CASE .OVER .OVER .!     T1 ESAC
      $01C0 CASE .TUCK       .!     T1 ESAC
            DROP ." ?mem?"                                   ;

\ ----------

{ : SPECIAL }
   THIS  $A020 = IF  0 EXIT THEN
   -1 THIS  $FFDF AND
      $A000 CASE  ." nop"       ESAC
      $BF40 CASE  ." lit -1"    ESAC
      $A0C1 CASE  ." s>d"       ESAC
      $BCC0 CASE  ." negate"    ESAC
      $B4C1 CASE  ." 1-"        ESAC
      $E0C0 CASE  ." 2dup ! "   ESAC
      $B08D CASE  ." seldpr"    ESAC
      $B00D CASE  ." selcpr"    ESAC
      $F0C0 CASE  ." 2dup C!"   ESAC
      $B090 CASE  ." softint"   ESAC
      $B010 CASE  ." -softint"  ESAC
      $B096 CASE  ." mulu"      ESAC
      $B097 CASE  ." muls"      ESAC
      $BE01 CASE  ." r>"        ESAC
      $B001 CASE  ." r>drop"    ESAC
      $BE80 CASE  ." r>drop>r"  ESAC
      $BE81 CASE  ." >r"        ESAC
      $BEA7 CASE  ." >r;"       ESAC
            DROP           1+   ;

\ ----------

{ : UN-ONE }
   LOCATION @ 5 U.R
   THIS       6 U.R   SPACE SPACE
   SPECIAL IF EXIT THEN
   THIS  12 LSRN
      $8 CASE  .BRANCH        ESAC
      $9 CASE  .BRANCH        ESAC
      $A CASE  .ALU           ESAC
      $B CASE  .LIT/REG       ESAC
      $C CASE  .USER          ESAC
      $D CASE  .LITERAL       ESAC
      $E CASE  BYTES OFF .MEM ESAC
      $F CASE  BYTES ON  .MEM ESAC
         DROP  .CALL          ;

: UN ( address)
   BASE @ >R HEX
   LOCATION !  0 SWAPPING !
   32 BEGIN
      32 = 15 AND ( either 15 or 0)
      FOR
         CR UN-ONE
         INCREMENT @ LOCATION +!
      NEXT
      KEY  DUP 32 <
   UNTIL DROP
   R> BASE ! ;

: SEE
   '  UN ;

\ --------- END OF DECOMPILER, CUT THRU HERE   ***XYZZY***

{ : N. } ( n)
   ."  = " 0 <# # # # # #>  TYPE 6 SPACES ;

: .REG ( --- )
 SPR@  BASE @ >R HEX   MD@ >R  ( SPR) >R
 CR  6 SPACES  ." *** RTX Machine Registers ***" CR
 ." T  " DUP N.  ." N  " OVER N.  CPU-TYPE @ 2 <>
 IF 0 0 THEN  MHR@ MLR@  TC2@ TC1@ TC0@  UBR@  IBC@  CPR@  UPR@
 DPR@  IPR@  IMR@  IVR@  R> ( SPR)  SR@  R@  SQ@  CR@
 DR> R@ ( PC) -ROT D>R   ." PC " N. CR
 ." CR " N. ." SQ "  N.  ." MD " N. CR
 ." SR " N. ." SPR"  N. ." IVR"  N. CR
 ." IMR" N. ." IPR"  N. ." DPR"  N. CR
 ." UPR" N. ." CPR"  N. ." IBC"  N. CR
 ." UBR" N. ." TC0"  N. ." TC1"  N. CR
 ." TC2" N.   CPU-TYPE @ 2 = IF ." RX "  N. ." RH "
 ELSE ." MLR"  N. ." MHR"  THEN  N. CR   CPU-TYPE @
 1 >  IF ." SUR" SUR@ N. CR THEN  R> MD!  R> BASE ! ;

\ ---------

: DOES>
   COMPILE DOES  COMPILE R>  -OPT ON ; IMMEDIATE HIDE

: BSET ( a pat)           OVER C@ OR   SWAP C! ;
: BCLR ( a pat)   -1 XOR  OVER C@ AND  SWAP C! ;

{ : TIMMEDIATE }   PREVIOUS     $40 BSET ;
: SMUDGE           PREVIOUS 1+  $80 BSET ;
: UNSMUDGE         PREVIOUS 1+  $80 BCLR ;
: IS-CODE          PREVIOUS     $20 BSET ;

\ ---------

{ : PREV-INSTR } ( - a ins t/f)
   OPTIMIZING @ NOT  IF  0 EXIT  THEN
   -OPT @ IF 0  EXIT  THEN
   LAST-ADDR @ @ $9FFF U<  IF  0 EXIT  THEN
   LAST-ADDR @ DUP @ -1 ;

: <exit>
   PREV-INSTR
   IF  -RETURN @
      IF  DDROP  RETURN ,
      ELSE  $20 OR  SWAP !  THEN
   ELSE  RETURN ,  THEN ;

\ ---------

: ]  STATE ON -OPT ON  ;
: [  STATE OFF ; IMMEDIATE

: :
   HEADER SMUDGE  0 bal !  0 leaf !  ] ;  HIDE

: ;
   ?COMP  <exit>  -OPT ON  STATE OFF  UNSMUDGE ; IMMEDIATE  HIDE

: EXIT
   ?COMP  <exit> ; IMMEDIATE  HIDE

\ ---------

: COMPILING ( a flag)
   STATE @ = IF ( not immediate)
      DUP >LINK L>NAME  C@ $20 AND
      IF ( this is a "code" reference, which the meta compiler exploits)
         @ ( value in cfa is either an opcode or a subroutine address)
         DUP 0< IF ( opcode, strip return bit)  $FFDF AND  THEN  ,
      ELSE  ( subroutine)
         >ta U2/ ,
      THEN
   ELSE  ( immediate)   EXECUTE  THEN ;

: INTERPRET
   BEGIN  BL WORD DUP C@ 0= IF DROP EXIT THEN  FIND
      ?DUP IF ( found)
         STATE @ IF COMPILING ELSE DROP EXECUTE THEN
      ELSE ( not found)
         COUNT VAL?  DUP huh?
         STATE @ IF  VAL,  ELSE  DROP  THEN
      THEN  AGAIN ;

: EVALUATE ( a #)
   'TIB @ >R #TIB @ >R  #TIB ! 'TIB !  >IN @ >R  0 >IN !
   INTERPRET   R> >IN !  R> #TIB !  R> 'TIB ! ;

: QUERY
   TIB 80 EXPECT  0 >IN !  SPAN @ #TIB ! ;

: OK?
   ?STACK  STATE @ 0= IF ."  ok"  THEN ;

: QUIT  0 STATE !
   -1 CPU-TYPE @ 2 < + SPR@+  255 AND SPR!  ( clear ret stack )
   BEGIN
      CR  DEPTH ?DUP IF  0 U.R  ." ) "  THEN
      QUERY SPACE INTERPRET
      OK?
   AGAIN ;  RECLAIM

\ --------- NEW DISK INTERFACE

: READ-FILE ( a # handle - #read)
   $22 CMD  ( handle) XMT  ( #) #XMT   #RCV TUCK RCVS ;

: WRITE-FILE ( a # handle - #written)
   $23 CMD  ( handle) XMT  DUP ( #) #XMT  XMTS  #RCV ;

: CLOSE-FILE ( handle)
   $21 CMD  ( handle) XMT ;

: SEEK-FILE ( position. dir handle - position.)
   $24 CMD  ( handle) XMT  ( dir) XMT
   ( position) #XMT #XMT  #RCV #RCV ;

: FILEPOS ( handle - pos.)
   >R 0 0 1 R> SEEK-FILE ;

: FILESIZE ( handle - size.)
   >R 0 0 2 R> SEEK-FILE ;

: OPEN-FILE ( a # - handle)
   $20 CMD  DUP ( #) #XMT  XMTS  RCV  DUP huh? ;

: CREATE-FILE ( a # - handle)
   $25 CMD  DUP ( #) #XMT  XMTS  RCV  DUP huh? ;

: DELETE-FILE ( a #)
   $26 CMD  DUP ( #) #XMT  XMTS  RCV  DUP huh? ;

: READ-LINE ( a # handle - 0 false | # true)
   $02F CMD  XMT #XMT  #RCV  RCV ( a # flag)
   IF ( data to be read)
      TUCK ( # a #) RCVS  -1
   ELSE ( end of file)
      DDROP 0 0
   THEN ;

: STRIP-CONTROLS
   ?DUP IF  0 DO  DUP C@ 32 MAX OVER C! 1+ LOOP  THEN DROP ;

: LOAD
   BL WORD COUNT OPEN-FILE  DUP huh? ( handle)
   FLINE# @ >R  0 FLINE# !
   HANDLE @ >R  HANDLE !
   BEGIN
      1 FLINE# +!
      BUFF DUP 132 HANDLE @ READ-LINE WHILE
      DDUP STRIP-CONTROLS
      EVALUATE
   REPEAT
   DDROP
   HANDLE @ CLOSE-FILE
   R> HANDLE !
   R> FLINE# ! ;

\ ---------

: LED ( n)   0<> $8000 AND $18 G! ;

{ : COLD? } ( - flag \ true is cold, 0 is warm)
   COLD @ 12345 12347 WITHIN NOT IF ( random)
      12345 COLD !  -1 EXIT ( is cold)
   THEN
   0  COLD @ 12345 = IF ( nominal run mode)
      1 COLD +!  -1 LED
      200 FOR  -1 CYCLES  NEXT
      -1 COLD +!  0 LED  1-
   THEN
   12345 COLD ! ;

\ ---------

\ Determines if multiplier is present.  If multiplier is
\   present, reading its output register (16) won't affect
\   the value of the stack pointer, since it doesn't push the
\   stack memory.

: MULTIPLIER? ( - f)     \ 1 = multiplier present  0 = no
  0 0                    \ protect top of stack
  SPR@ $0FF AND  >R
  $16 G@
  SPR@ $0FF AND  R> =
    IF  DDROP 1  HWMULT
    ELSE  DDROP DROP 0  NOHWMULT  THEN
  ;

\ Determines CPU type - 2000 or 2010/2001A by reading
\   the initial value of the Parameter Stack pointer register.
\   For 2010/2001A the value will be 0.  This must be the first
\   call before anything is pushed on the stack.  Sets CPU-TYPE
\   to indicate CPU: 1 = 2000  2 = 2001A  3 = 2010
\   (lsb = 1 => multiplier present)

\ simple kludge for now ... only 2000 has hw mult set
: CPU-TYPE?
   SPR@ $0FF AND 0<>
     IF ( RTX 2000)
        1
     ELSE ( RTX 2001A or RTX 2010)
        2  >R MULTIPLIER? R> OR
        NOHWMULT ( kludge)
     THEN
   CPU-TYPE ! ;

\ ---------

{ : DISCLAIMER }   ( print if power up or cold start )
   CR ." This software is provided by Harris Corporation, Semiconductor Sector"
   CR ." as a courtesy to its customers free of charge.  The programs and other"
   CR ." contents of the disk are not released products, and therefore are not"
   CR ." supported by Harris Corporation and no warranty of any kind applies."
   CR ." Any liability resulting from use of this software is specifically"
   CR ." disclaimed by Harris Corporation."
   CR  ;

: COPYRIGHT
   CR ." App-Forth  Tue  05-01-1990  12:37:44"
   CR
   CR ." COPYRIGHT 1990 HARRIS CORPORATION"
   CR ." ALL RIGHTS RESERVED"
   CR ;

\ ----------

: sys-abort
   CPU-TYPE @ 2 <  IF  $-100 SPR@+  ELSE  0 SPR@+  THEN
   $FF00 AND  SPR!              \ Clear parameter stack
   CPU-TYPE @ 1 = IF 0 THEN    \ 0 for stack underflow if 2000
   FLINE# @ ?DUP IF
      ."  ... error at line "
      BASE @  DECIMAL  SWAP .  BASE !
      HANDLE @ CLOSE-FILE
   THEN
   'TIB 2+ 'TIB !
   FLINE# OFF
   'idle @ EXECUTE ;

\ ---------

OLDCREATE ROM
                  10 OLD,  \ BASE
                   1 OLD,  \ -OPT
                   0 OLD,  \ -RETURN
             T' QUIT OLD,  \ 'idle
        T' sys-abort OLD,  \ 'abort
         T' <header> OLD,  \ 'header
              T' XMT OLD,
              T' RCV OLD,
             T' XMT? OLD,
             T' RCV? OLD,
                   0 OLD,  \ STATE
                   0 OLD,  \ dA
         T' FORTH 2+ OLD,  \ VLINK
               $8000 OLD,  \ FENCE
                   0 OLD,  \ fline#

\ ---------

: RESET
   CPU-TYPE?  R>DROP
   COLD? IF
      ( RAMTEST)
      INIT-LINKS
      ROM BASE 30 CMOVE
      FENCE @ H !
      1 >R ( disclaimer)
   ELSE
      0 >R ( no disclaimer)
   THEN
   INT-VECTORS 544 ERASE  SET-INT-VECTORS  SET-INT-MASK
   INIT-8250  OPTIMIZE
   COPYRIGHT
   R> IF DISCLAIMER THEN
   ABORT ;  RECLAIM


\ ----------

T' RESET  U2/  $A000 !

\ ---------

: CONSTANT
   HEADER  $DE20 , , ;

: DCONSTANT
   HEADER  SWAP <literal> <literal> <exit> ;

: VARIABLE     CREATE  0 , ;
: DVARIABLE    CREATE  0 , 0 , ;

\ ---------

: .(  ASCII ) WORD  COUNT TYPE ; IMMEDIATE

: J ( -- n)
   R> R> R> R@ SWAP >R SWAP >R SWAP >R; ; RECLAIM

: ( ASCII ) WORD DROP ;  IMMEDIATE  HIDE

: \   #TIB @ >IN ! ;   IMMEDIATE HIDE

\ ---------

\ this may not optimize properly following a shift operation!!!!!!
: (NOT)
   PREV-INSTR
   IF  DUP $0F00 AND  DUP $0000 =  SWAP $0E00 =  OR \ No ALU code
      OVER $F010 AND  $A010 <>  AND         \ Not 17-bit math
      IF  -1  ELSE  DDROP 0  THEN
   ELSE  0  THEN
   IF  $0100 OR SWAP !                     \ Set inverse bit
   ELSE  $A100 ,  THEN ;                   \ Standalone

: NOT
   STATE @  IF  (NOT)  ELSE <NOT> THEN ; IMMEDIATE HIDE

\ ---------

{ : BACK-SHIFT }
   PREV-INSTR
   IF   DUP $F01F AND $A000 =
      IF  -1  ELSE  DDROP 0  THEN
   ELSE  0  THEN
   IF  ROT ( mask)  OR  SWAP !
   ELSE   $A000 OR , THEN ;

\ ---------

{ : TEST->LIT }  ( opmask a ins testmask -- t / mask f)
   OVER AND  $0E00 =
   IF  $F0FF AND  $0080 OR  ROT  OR  SWAP  ! -1
   ELSE  DDROP 0  THEN ;

{ : ALU->LIT }  ( mask a ins -- t / mask f)
   $0F80  TEST->LIT ;

{ : ALU->MEM }  ( mask a ins -- t / mask f)
   $0FC0  TEST->LIT ;

{ : ALU }  ( n - n')
  DUP >R  R@ $0400 =
          R@ $0500 =
          R@ $0C00 =
          R> $0D00 =  OR OR OR
  IF  $0800 XOR  THEN ;

{ : BACK-ALU }  ( mask --)
  PREV-INSTR
    IF  DUP $-0C SHIFT  DUP $0A =
       IF DDROP DROP 0  ELSE  DUP $0B =
       IF DROP ALU->LIT ELSE  DUP $0C =
       IF DROP ALU->MEM ELSE  DUP $0D =
       IF DROP ALU->LIT ELSE  DUP $0E =
       IF DROP ALU->MEM ELSE      $0F =
       IF ALU->MEM ELSE DDROP 0          THEN THEN THEN THEN THEN THEN
  ELSE  0 THEN  0=  IF  ALU $A040 OR , THEN ;

{
   : aluCODE 'ALU-X @ U2/ OLDCREATE  [COMPILE] OLDIMM
      [COMPILE] HIDE   -2 GAP  OLD,  <ALU> ;
   : shCODE 'SHIFT-X @ U2/ OLDCREATE  [COMPILE] OLDIMM
      [COMPILE] HIDE   -2 GAP  OLD,  <SHIFT> ;
}

{ : CRIMM }  CREATE TIMMEDIATE ;
: <SHIFT> $A000 OR , RETURN , ;
: <ALU> ALU $A040 OR , RETURN , ;

\ ---------

$A012 UCODE 2*'
$BE03 UCODE CR@      $BE83 UCODE CR!    $B683 UCODE CR@OR
$BE06 UCODE SR@      $BE86 UCODE SR!
$BE07 UCODE PC@      $BE05 UCODE SQ@
$BE09 UCODE SPR@     $BE89 UCODE SPR!   $B889 UCODE SPR@+
$BE0C UCODE IPR@     $BE8C UCODE IPR!
$BE0E UCODE UPR@     $BE8E UCODE UPR!
$BE0F UCODE CPR@     $BE8F UCODE CPR!
$BE11 UCODE UBR@     $BE91 UCODE UBR!
$BE13 UCODE TC0@     $BE93 UCODE TC0!
$BE14 UCODE TC1@     $BE94 UCODE TC1!
$BE15 UCODE TC2@     $BE95 UCODE TC2!
$BE84 UCODE MD!      $BE04 UCODE MD@
$BE08 UCODE IMR@     $BE88 UCODE IMR!
$BE0B UCODE IVR@
$BE8B UCODE SLR! ( 2000/2010)
$BE8B UCODE SVR! ( 2001A)
$BE0D UCODE DPR@     $BE8D UCODE DPR!
$BE10 UCODE IBC@     $BE90 UCODE IBC!
$BE16 UCODE MLR@     $BE17 UCODE MHR@
$BE17 UCODE RH@      $BE87 UCODE RH!
$BE16 UCODE RX@      $BE86 UCODE RX!
$BE0A UCODE SUR@     $BE8A UCODE SUR!
$A55A UCODE S'       $A558 UCODE S"       $A51A UCODE S1'
$A49D UCODE *"       $A89C UCODE U*'      $A89D UCODE *'
$A41A UCODE U/1'     $A45A UCODE U/'      $A458 UCODE U/"
$AE40 UCODE DROP     $AEC0 UCODE OVER     $A0C0 UCODE DUP
$AE80 UCODE SWAP     $A000 UCODE NOP      $A480 UCODE SOS-
$BE00 UCODE R@       $B4C1 UCODE 1-       $BF40 UCODE -1
$BE00 UCODE I
$B4C2 UCODE 2-       $B8C1 UCODE 1+       $B8C2 UCODE 2+
$FE00 UCODE C@       $FE80 UCODE C!
$BCC0 UCODE NEGATE   $AC41 UCODE <        $A441 UCODE  >
$A040 UCODE NIP      $BDC0 UCODE 0SWAP-c  $E942 UCODE @+2
$B096 UCODE MULU     $B097 UCODE MULS     $E9C2 UCODE !+2
$B08D UCODE SELDPR   $B00D UCODE SELCPR   $EE00 UCODE @
$B090 UCODE SOFTINT  $B010 UCODE -SOFTINT $EE80 UCODE !
$F8C1 UCODE DUPC!+1  $A100 UCODE <NOT>    $E542 UCODE @-2
$F9C1 UCODE C!+1     $F5C1 UCODE C!-1     $E842 UCODE DUP@+2
$F941 UCODE C@+1     $F541 UCODE C@-1

\ ---------

: aluCODE   CRIMM  <ALU>
 DOES> HHERE STATE @
 IF  @ $F00 AND ALU BACK-ALU  ELSE  EXECUTE  THEN ;
 'ALU-X ! HIDE

: shCODE    CRIMM  <SHIFT>
 DOES> HHERE STATE @  IF  @ BACK-SHIFT  ELSE  EXECUTE  THEN ;
 'SHIFT-X !  HIDE

$01 shCODE 0<        $02 shCODE 2*        $03 shCODE 2*C
$04 shCODE cU2/      $05 shCODE c2/       $06 shCODE U2/
$07 shCODE 2/        $08 shCODE N2*       $09 shCODE N2*c
$0A shCODE D2*       $0B shCODE D2*c      $0C shCODE CUD2/
$0D shCODE cD2/      $0E shCODE UD2/      $0F shCODE D2/

$200 aluCODE AND    $300 aluCODE NOR     $400 aluCODE -
$500 aluCODE -c     $600 aluCODE OR      $700 aluCODE NAND
$800 aluCODE +      $900 aluCODE +c      $A00 aluCODE XOR
$B00 aluCODE XNOR   $C00 aluCODE SWAP-   $D00 aluCODE SWAP-c

\ ---------

{ : noret } ( opcode)
   ?COMP  ,  1 -RETURN ! ;

: R>        $BE01 noret ;     IMMEDIATE HIDE
: R>DROP    $B001 noret ;     IMMEDIATE HIDE
: R>DROP>R  $BE80 noret ;     IMMEDIATE HIDE
: >R        $BE81 noret ;     IMMEDIATE HIDE
: >R;       $BEA7 noret ;     IMMEDIATE HIDE

: DR>       COMPILE R> COMPILE R> ;  IMMEDIATE HIDE
: D>R       COMPILE >R COMPILE >R ;  IMMEDIATE HIDE

: EXECUTE
   STATE @ IF
      $BE87 noret
   ELSE
      EXECUTE
   THEN ; IMMEDIATE HIDE

: TIMES   $BE82 noret ; IMMEDIATE HIDE

\ ---------

: UCODE  HEADER  $020 OR , IS-CODE ;  HIDE

: LONG ( p a)
   DDUP  0 LAST @ 2+  DSWAP  HERE LAST @ 2+ - DUP >R  CMOVEL
   R> 2- NEGATE ALLOT  SWAP  , U2/ ,  TIMMEDIATE
   DOES>  STATE @
   IF  D@  <literal>  COMPILE CPR!  ,
   ELSE  D@ SWAP EXECUTEL THEN ;

\ ----------

\ extract lower 9 bits
{ : OFFSET-BITS ( a - a') }
   $03FF AND ;

\ extract page bits
{ : PAGE-BITS ( a - a') }
   $FC00 AND ;

\ true if reachable from the current location via branch
{ : ?REACHABLE ( to from) }
   2+  SWAP PAGE-BITS  SWAP PAGE-BITS -  ABS
   $0400 >  ABORT" unreachable branch target" ;

\ b/s/f returns $600 if backward, $200 if forward, 0 if same page
{ : B/S/F ( to from - branch_type_bits) }
   2+  SWAP PAGE-BITS  SWAP PAGE-BITS -
   DUP IF ( not same page)
      0< IF   ( backward)   $0600
         ELSE ( forward)    $0200
         THEN
   THEN ;

\ resolve the branch
{ : RESOLVE ( at to) }
   SWAP
   DDUP ?REACHABLE
   DDUP B/S/F ( to at branch)
   ROT OFFSET-BITS  2/ OR  ( at offset)
   OVER @  OR  SWAP !  -OPT ON ;

\ ----------

\ force a branch at the leaf address to HERE
{ : set-leaf ( leaf-address) }
   <branch> OVER !  HERE RESOLVE ;

\ gathers leafs
: rake ( address of end of loop)
   leaf @
   BEGIN  DDUP U< WHILE  DUP @  SWAP set-leaf  REPEAT
   leaf ! DROP ;

\ ----------

{ : ?bal }    bal @ < huh? ;
{ : -bal }    bal @ huh?  -1 bal +!  DUP @ huh? ;

: BEGIN   HERE 1 bal +!  -OPT ON ;                        IMMEDIATE

: IF     [COMPILE] BEGIN  <?branch> , ;                   IMMEDIATE
: THEN   0 ?bal  -1 bal +!  HERE RESOLVE ;                IMMEDIATE
: ELSE   0 ?bal  [COMPILE] BEGIN <branch> ,
         SWAP  [COMPILE] THEN ;                           IMMEDIATE

: UNTIL   -bal  HERE <?branch> ,  SWAP RESOLVE ;          IMMEDIATE
: AGAIN   -bal  HERE  <branch> ,  SWAP RESOLVE ;          IMMEDIATE
: WHILE   bal @ huh? [COMPILE] IF  SWAP ;                 IMMEDIATE
: REPEAT  1 ?bal  [COMPILE] AGAIN [COMPILE] THEN ;        IMMEDIATE

: -ZERO   DROP  [COMPILE] BEGIN  <branch> ,  HERE SWAP ;  IMMEDIATE
: FOR     COMPILE >R  [COMPILE] BEGIN ;                   IMMEDIATE
: NEXT    -bal  HERE  <next> ,  SWAP RESOLVE ;            IMMEDIATE

\ ----------

: DO  ( limit index )
   COMPILE SWAP COMPILE >R COMPILE >R  [COMPILE] BEGIN ;  IMMEDIATE

: LEAVE
   COMPILE R>DROP  COMPILE R>DROP
   HERE  leaf @ ,  leaf ! -RETURN ON ;                  IMMEDIATE

: LOOP
   -bal  COMPILE <loop>  DUP >ta ,  rake -OPT ON ;      IMMEDIATE

: +LOOP ( increment)
   -bal  COMPILE <+loop> DUP >ta ,  rake -OPT ON ;      IMMEDIATE

\ ---------

: IMMEDIATE TIMMEDIATE ;

\ ----------

STATS

LINKS } PRUNE ( Addr left by PRUNE)
\ DUP $A000 - HEX CR U. DECIMAL .( bytes compiled.  Max is 4000h.)
   SWAP $010 CMOVE

CR ?SAVE

: TEST   $A000 >R ;


