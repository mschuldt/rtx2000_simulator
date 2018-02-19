\ COPYRIGHT 1990 HARRIS CORPORATION  ALL RIGHTS RESERVE
\ Rick VanNorman Wed  04-11-1990  10:24:56

\ --------- Meta compiler

: HIDE    SMUDGE ;
: REVEAL  UNSMUDGE ;

DECIMAL

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
   CR DDUP TYPE EVALUATE ;

: ?SAVE
   " H' 2+ @ $A000 = " EVALUATE
   IF
      " $A000 $4000 SAVE-IMAGE T.BIN" .EVALUATE
   THEN ;

CREATE SAVE #THREADS CELLS ALLOT

VARIABLE KEEPH

VARIABLE H'  ( dictionary pointer for target system )
   0 ,  ( offset for compilation)

: TEST-ROM       0 H' 2+ ! ;
: MAKE-ROM   $A000 H' 2+ ! ;

\ use TEST-ROM or MAKE-ROM to define the compilation type

MAKE-ROM

\ save the current state of FORTH for restoration after metacompilation
HEADS SAVE #THREADS CELLS CMOVE    HERE KEEPH !


: {
   DA @ HERE  H' D@ H ! DA !  H' D! ;

: }
   STATE @  IF  H' @  H' 2+ @ - U2/ , IS-CODE THEN { ; IMMEDIATE

\ ----------

VARIABLE RAM
VARIABLE 'ALU-X
VARIABLE 'SHIFT-X

\ aliases with which to reach ROM words
: GAP           ALLOT ;
: ALLOT         RAM +! ;
: VARIABLE      RAM @ CONSTANT  2 ALLOT ;
: 2VARIABLE     RAM @ CONSTANT  4 ALLOT ;
: OLD,          , ;
: HHERE         HERE 2- >TA ; IMMEDIATE
: OLDCREATE     CREATE ;
: OLDIMM        IMMEDIATE ;
: OLD>TA        >TA ;
: OLDliteral    literal ;
: ''            ' ;

: SCAN ( a1-a2)             ( search for a target word )
   @  BEGIN DUP  1 $A000 WITHIN  WHILE @  REPEAT ;

: TRIM ( a1 a2 - a2 )   ( scans a linked list for target words)
   DUP  >R >TA           ( compute target address )
   SWAP  !  R>           ( store corrected address )
   DUP LINK>NAME 1+ DUP C@ $07F AND SWAP C! ; ( reveal)

: CLIP ( a)         ( relinks target words to separate dict )
   DUP BEGIN DUP SCAN DUP WHILE TRIM REPEAT SWAP ! @ , ;

: PRUNE             ( relink the target dictionary )
   { HERE #THREADS 0 DO  HEADS  R@ 2* +  CLIP LOOP {
   SAVE HEADS #THREADS CELLS CMOVE KEEPH @ H ! ( rest host dic) ;


\ ----------

\ prep the compilation area
$A000 $4000 $FF FILL   $A020 H' ! $F000 RAM !

{

\ --------- Scr 9

\ do  loop  +loop                                13:56 03/15/89
\ Kernel starts here
{ : do }  SWAP R> SWAP >R SWAP >R >R; ;

{ : loop }  R> ( ret addr)  R> 1+ DUP  ( index+1)
  R@ SWAP >R  XOR
\  IF 0 ELSE -1 THEN IF 2+ THEN >R; ;

  IF  >R; EXIT THEN
  2+ >R;  ;

{ : +loop  ( inc) }
  SR@ SWAP  R> SWAP SR!                ( SAVE RET ADR, INC->SR)
  R> R@ 1-  OVER SR@ + DUP >R OVER     ( RETURN STACK UPDATED)
  SR@ 0<  IF > NOT >R > ELSE > >R > NOT THEN   ( DECR/INCR)
  R> AND  IF 2+ THEN  SWAP SR!  >R; ;  RECOVER

\ --------- Scr 12

\ host words                                     13:56 09/20/88

{
   : DO  ( limit index )  LEAVING @  0 LEAVING !
      COMPILE do  HERE ;   IMMEDIATE

   : LOOP COMPILE loop HERE SWAP BRANCH , FIXUP HERE FIX-LEAVES
      COMPILE R>DROP  COMPILE R>DROP  1 -RETURN ! ; IMMEDIATE

   : +LOOP COMPILE +loop HERE SWAP BRANCH , FIXUP HERE FIX-LEAVES
      COMPILE R>DROP   COMPILE R>DROP   1 -RETURN ! ; IMMEDIATE

   : -ZERO  DROP  HERE BRANCH ,  HERE  SWAP ;  IMMEDIATE

}


\ --------- Scr 15

VARIABLE INT-VECTORS 32 17 * ALLOT
VARIABLE BASE
VARIABLE -OPT
VARIABLE -RETURN
VARIABLE 'idle
VARIABLE 'abort
VARIABLE 'header
VARIABLE 'EMIT?
VARIABLE 'KEY?
VARIABLE 'EMIT
VARIABLE 'KEY
VARIABLE STATE
VARIABLE DA
VARIABLE VLINK
VARIABLE FENCE
VARIABLE FLINE#
VARIABLE COLD
VARIABLE SPAN
VARIABLE PTR
VARIABLE LAST-ADDR
VARIABLE #TIB
VARIABLE H
VARIABLE OPTIMIZING
VARIABLE STEP
VARIABLE DPL
CREATE LINKS 16 GAP
VARIABLE THREADS 14 ALLOT
VARIABLE PICKINGS   510 ALLOT
VARIABLE DEL
VARIABLE LEAVING
VARIABLE 'TIB  130 ALLOT
VARIABLE BUFF   130 ALLOT
VARIABLE #OUT
VARIABLE >IN
VARIABLE HEADS 14 ALLOT
VARIABLE CURRENT
VARIABLE CONTEXT
VARIABLE 'SEARCH 14 ALLOT
VARIABLE 'ALWAYS
VARIABLE LAST
VARIABLE CPU-TYPE
VARIABLE HANDLE

    8 CONSTANT #THREADS
    8 CONSTANT #SEARCH
    2 CONSTANT CELL
  $20 CONSTANT BL
$A020 CONSTANT RETURN

\ --------- Scr 18

 0 CONSTANT FALSE
-1 CONSTANT TRUE

: 0=  ( n -- f)  IF   0 EXIT  THEN  -1 ;
: 0<> ( n -- f)  IF  -1 EXIT  THEN   0 ;
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

\ --------- Scr 21

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

\ --------- Scr 33

: N>U ( n -- u f)
  DUP 0< IF  NEGATE 1 EXIT THEN 0 ;
: D>UD ( d -- ud f)
  DUP 0< IF  DNEGATE 1 EXIT THEN 0 ;

: DSQRT ( ud -- root )
  32768 SR!  0 MD!  D2*  S1'  13 OF( S'  S"  DROP ;
: SQRT  ( u -- root )  0 DSQRT ;

: M*  ( n n -- d)
  STEP @ IF  CR@ DUP DUP 2* 0< $10 AND OR >R  8 AND $10 OR CR!
             MD!  0 $0E OF( *' *"  R> CR! EXIT  THEN
  MULS MLR@ MHR@  ;

: * ( n n -- n)  M* DROP ;
: UM* ( u u -- ud)
  STEP @ IF  MD! 0 0 + 2*' 15 OF( U*' EXIT
  MULU MLR@ MHR@  ;

: UM/MOD ( ud u -- ur uq)
  MD! D2*  U/1' 13 OF( U/'  U/" SWAP ;

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

: MIN ( n n -- n)  DDUP > IF SWAP THEN DROP ;
: MAX ( n n -- n)  DDUP < IF SWAP THEN DROP ;

\ --------- Scr 24 UART INTERFACE

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

\ --------- Scr 43 BAUD RATE

: BAUD ( baud )
  $25800. ROT UM/MOD NIP           \ calculate divisor
  $080 $700 REG!                    \ set DLAB bit
       $100 REG!                    \ set baud rate
  000 $0300 REG!
  003 $0700 REG! ;  ( 8-data, 1-stop, no parity )

\ --------- Scr 27

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
   ?DUP IF
      0 DO
         C@+1 SWAP EMIT
      LOOP
   THEN DROP ;

: SPACE  BL EMIT ;
: SPACES ( n) 0 MAX FOR -ZERO SPACE THEN NEXT ;

\ --------- Scr 36 Convert binary to ASCII string

: HERE  ( -- a )  H @ ;

: PAD  ( -- a )  HERE 128 + ;

: TIB   'TIB @ ;

: HOLD ( c - ) ( Add c to output string )
  -1 PTR +!  PTR @ C! ;

: DIGIT ( n - c)  ( Convert a number n to its ASCII equivalent)
  DUP  9 > 7 AND +  48 + ;

: SIGN  ( n - ) ( Append - sign to output string)
  0<  IF 45 HOLD THEN ;

: <#   PAD PTR !    ; ( Prepare to start conversion )

: #  ( d - d')   ( Convert one digit from d )
  0 BASE @  DUP >R  UM/MOD  R> SWAP >R UM/MOD SWAP R> SWAP
  DIGIT HOLD ;

: #S  ( d - 0) ( Convert d til reduced to 0 )
  BEGIN  #  DDUP D0=  UNTIL ;

: #>  ( d - a n)     ( Output the converted string to terminal)
  DDROP  PTR @ PAD OVER - ;

{ : N. } ( n)  0 <# # # # # #>  TYPE 6 SPACES ;
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

\ --------- Scr 40

: CMOVE  ( s d c) ?DUP IF
  1-  FOR  >R C@+1 SWAP R> C!+1  NEXT THEN DDROP ;

: CMOVE> ( s d c)  ?DUP  IF
  DUP >R + 1- SWAP R@ + 1- SWAP R>
  1-  FOR  >R C@-1 SWAP R> C!-1  NEXT THEN DDROP ;

: COUNT ( a - a n) DUP 1+  SWAP C@ ;

: FILL  ( a c n) SWAP ?DUP  IF
  >R  SWAP R> 1- OF( DUPC!+1 THEN  DDROP  ;

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

\ --------- Scr 42

{ : (")  }  R> COUNT  DDUP       + EVEN >R ;
{ : dot" }  R> COUNT  DDUP TYPE  + EVEN >R ;

\ ---------- META WORDS, EXECUTE ON HOST

{  : "    COMPILE (")  34 STRING ; IMMEDIATE
   : ."   COMPILE dot" 34 STRING ; IMMEDIATE }

\ ----------

: ABORT
   'abort @ EXECUTE ;

{ : abort" }
   IF    HERE COUNT TYPE SPACE R> COUNT TYPE ABORT
   ELSE  R> COUNT  + EVEN >R
   THEN  ;

\ ---------- META WORDS, EXECUTE ON HOST

{  : ABORT"  COMPILE abort" 34 STRING ; IMMEDIATE }

\ ----------

: huh?   0= ABORT" ?" ;

: >UPPER ( char - char)
   DUP  ASCII a ASCII z 1+  WITHIN IF  32 -  THEN ;

\ --------- Scr 47

: -DIGIT  ( c base - n f)
   >R  DUP  ASCII 9 >
   IF  DUP ASCII @ >  7 AND  -  THEN
   ASCII 0 -  DUP R> U<  ;

: CONVERT ( +d a - +d a)
   BEGIN  1+ DUP >R  C@ >UPPER BASE @ -DIGIT
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

\ --------- Scr 49

: PICK  ( n -- s[n])
  MD@ >R  SR@ >R           ( Save registers)
  MD! PICKINGS  MD@    OF( !+2
  2- DUP SR!
  MD@  OF( @-2 DROP SR@ @
  R> SR!  R> MD! ;         ( restore regs)

: ROLL  ( n -- s[n])
  ?DUP IF
     MD@ >R  SR@ >R           ( Save registers)
     MD! PICKINGS  MD@    OF( !+2
     2- DUP SR!
     2- MD@  1- OF( @-2 DROP SR@ @
     R> SR!  R> MD!           ( restore regs)
  THEN ;

\ --------- Scr 51

: DEPTH  ( -- n)
   0 SPR@+ $0FF AND  CPU-TYPE @ 2 <  IF  3  ELSE  1  THEN  - ;

: ?STACK
   DEPTH 0< ABORT" Stack underflow " ;

: ?COMP  STATE @ 0= ABORT" is compilation only" ;

: .S
  ?STACK
  CR  DEPTH FOR -ZERO  R@ PICK U.  THEN NEXT  ." <top " ;

\ ----------

: LINK>NAME ( a1 - a2)
   2+ ;

: LINK>CODE ( a1 - a2 )
   LINK>NAME  COUNT $1F AND  +  EVEN ;

: CODE>LINK ( a1 - a2 )
   BEGIN 1- DUP C@ $080 AND UNTIL 2-  DUP 1 AND - ;

: PREVIOUS ( - a #)
   LAST @  LINK>NAME DUP C@ ;

: USE ( a - )
   LAST @ LINK>CODE ! ;


\ --------- Scr 56

: SOURCE ( - a #)   TIB #TIB @ ;
: /SOURCE ( - a #)   SOURCE >IN @ /STRING ;
: ACCEPT ( n f)   IF 1+ THEN >IN +! ;

: WORD ( c - a)
   >R /SOURCE OVER R> SWAP >R >R R@ SKIP OVER SWAP R> SCAN
   OVER R> - SWAP ACCEPT OVER - HERE DUP >R
   DDUP C!  1+ SWAP CMOVE  BL R@ COUNT + C!  R> ;

\ --------- Scr 53

: >HASH ( saddr - n )  1+ C@ 7 AND 2* 2+ $10 MOD ;

{ : SAME? } ( a1 a2 - f)   DDUP 1+  SWAP 1+
  DSWAP C@ $01F AND  SWAP C@ $01F AND
  OVER =              ( COMPARE COUNT BYTE)
  IF  -1 DEL !
     1- FOR
        OVER C@ >UPPER OVER C@ >UPPER =
      IF  1+ SWAP 1+ SWAP ( inc addresses)
      ELSE  R>DROP 0 >R ( exit) 0 DEL !  THEN
    NEXT  DDROP  DEL @ EXIT
  THEN
  DDROP DROP 0 ;

{ : (FIND) } ( thread a1 - a2 n)
   DUP >HASH  ROT  + @ DUP
   IF  BEGIN  DUP  >R  LINK>NAME OVER  SAME?
      IF  DROP R> DUP  LINK>CODE  SWAP LINK>NAME C@
         $040 AND IF  1 ( imm)
                 ELSE  -1 ( not imm) THEN 1 ( found it)
      ELSE  R> @  DUP
         IF  0  ELSE  DROP 0 1  ( end of link chain )  THEN
      THEN   UNTIL  THEN  ;

: FIND  ( a1 - a2 n)
   CONTEXT  #SEARCH 2+
   1-  FOR  DDUP @ DUP
      IF  SWAP  (FIND) DUP
        IF  R>DROP 0 >R DSWAP DDROP
        ELSE  DDROP 2+  THEN
      ELSE  DDROP 2+  THEN   NEXT
   DUP CONTEXT #SEARCH 2+ 2* + =  IF  DROP 0  THEN ;

: '  ( - a n)  BL WORD  FIND  huh? ;

\ --------- Scr 32

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

\ --------- Scr 59

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
: EDIT     DO-DOS ;  \ generic utility name

: BYE   255 CMD ;

\ ----------

: LISTING
   BL WORD COUNT ( a #)  DUP huh?
   $2D CMD  DUP ( #) #XMT  XMTS
   RCV ( file opened) huh?
   RCV ( file closed) huh? ;

VARIABLE LINE#

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

\ --------- Scr 61

\ WORDS SUPPORT

: ?PAUSE
   KEY? IF  KEY  27 = IF  'idle @ EXECUTE  THEN
            KEY  27 = IF  'idle @ EXECUTE  THEN   THEN ;

: COMPARE ( a1 a2 # - f)
   BEGIN ( a1 a2 #)
      >R  OVER C@ >UPPER  OVER C@ >UPPER
      <> IF  R>DROP  DDROP  0 EXIT  THEN
      1 1 D+  R> 1-  ?DUP 0=
   UNTIL  DDROP -1 ;

{ : SCAN>UPPER } ( a # char - a #)
   >R BEGIN DUP WHILE OVER C@ >UPPER R@ = IF  R>DROP EXIT  THEN
      1 /STRING  REPEAT  R>DROP ;


\ Search for a pattern in a body of text.  if flag is
\ true on exit, a match was found and its address is
\ returned.  otherwise, the address is meaningless
: SEARCH ( pattern # text # - addr f)
   3 PICK C@ >UPPER
   BEGIN  DUP >R SCAN>UPPER DUP  WHILE
      DOVER DOVER DROP SWAP COMPARE
      IF ( match!)
         DROP >R DDROP R>  R>DROP  -1 EXIT  THEN
      1 /STRING  R>
   REPEAT ( p # t #)
   DSWAP DDROP R>DROP ;

VARIABLE MATCHING
VARIABLE MATCHES

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
   THEN   ID. SPACE  1 MATCHES +! ;

{ : LARGEST } ( a n -- a' n')
   OVER 0 SWAP ROT
   FOR -ZERO  DDUP @ U<  IF -ROT DDROP DUP @ OVER THEN
      CELL+  THEN NEXT DROP ;

{ : ANOTHER } ( - nfa)
   THREADS #THREADS LARGEST DUP
   IF  DUP @ ROT ! LINK>NAME  ELSE  NIP  THEN ;

: WORDS
   BL WORD C@ 0<> MATCHING !  0 MATCHES !
   CONTEXT @  THREADS  #THREADS CELLS CMOVE
   CR BEGIN  ?PAUSE ANOTHER ?DUP WHILE ?.NAME REPEAT
   CR ." Found " MATCHES ? ." words. " ;

\ --------- Scr 65

: CLIP   ( target link - )  \ all threads of a vocabulary
  #THREADS  1-  FOR  SWAP OVER @
     BEGIN  DDUP SWAP U< NOT  WHILE @  REPEAT
     >R OVER R> SWAP !  SWAP 2+  NEXT  DDROP ;

: CHECK ( target vlink)   \ check if comp or search vocs deleted
  OVER CURRENT @ U< CURRENT @ HEADS <> AND
  ABORT" Can't forget compilation vocabulary "  OVER 'SEARCH
  #SEARCH 1- FOR  DUP @ 0<> IF DUP @ HEADS <> >R  DDUP @ U< R>
     AND  ABORT" Can't forget search vocabulary" THEN 2+
  NEXT  DDROP  ;

: CROP   ( target - )  \ all vocabs and words thru target
  VLINK @  BEGIN  DDUP U< WHILE @  CHECK  REPEAT VLINK !
  VLINK @  BEGIN  DUP  WHILE  DDUP 2+ @ CLIP @  REPEAT  DDROP ;

: FORGET
  ' DUP $8000 U<  IF ." Can't forget a protected word "  DROP
   ELSE  CODE>LINK  DUP  CROP  H !  THEN  ;

: EMPTY  FENCE @ DUP CROP H ! ;

\ --------- Scr 66

{ : TALLOT } ( n)
  $E000 HERE - OVER $0100 + > NOT ABORT" Dictionary full " H +! ;

: ,  ( n)
  STATE @  IF  HERE LAST-ADDR !  -OPT OFF
     -RETURN @ 1- 0 MAX -RETURN !  THEN  2 TALLOT  HERE 2- ! ;

: C, ( n)   1 TALLOT  $0FF AND  HERE 1- C! ;

: RECOVER  -2 TALLOT ;  HIDE

\ --------- Scr 67

\ build a dictionary header out of the following blank delimited text
\ and link it into the CURRENT vocabulary.  allocate no executable code
: header
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

\ --------- Scr 68

: >TA  ( a1 - a2)  DA @ - ;

: SHIFT ( value count -- value')
   ?DUP
   IF  DUP 0<
      IF    NEGATE 1- OF( U2/
      ELSE  1- OF( 2*
      THEN
   THEN ;

{ : DOES }  ( - )  R> U2/ USE ;
{ : DOES>  COMPILE DOES  COMPILE R>  NOP ; IMMEDIATE }

\ --------- Scr 69

: STRING ( char)
   WORD  COUNT  DUP C, ( a #)
   0 DO  DUP C@ C,  1+  LOOP DROP
   ALIGN ;

: literal ( n)
   DUP 0 <  OVER $1F > OR
   IF  $DE00 , ,  -2 LAST-ADDR +!
   ELSE  $BE40 OR , THEN ;

: LITERAL ( n)
  STATE @ IF  literal  THEN ;  IMMEDIATE

: VAL, ( .... n n #)
   DUP BEGIN  ROT >R         1- ?DUP 0= UNTIL
       BEGIN      R> literal 1- ?DUP 0= UNTIL ;

\ --------- Scr 70

: [']  ?COMP ' >TA literal ; IMMEDIATE  HIDE
: [COMPILE]  ?COMP  '  >TA U2/ , ; IMMEDIATE HIDE
: COMPILE  R> DUP 2+ >R @ ,  ;

: ASCII
  BL WORD 1+ C@
  STATE @ IF  literal  THEN ; IMMEDIATE HIDE

: ."   ?COMP
  COMPILE dot"  ASCII " STRING ;   IMMEDIATE HIDE
: "    ?COMP
   COMPILE (") ASCII " STRING ; IMMEDIATE
: ABORT"  ?COMP
  COMPILE abort"  ASCII " STRING ;  IMMEDIATE HIDE

\ --------- Scr 72

\ THIS IS *NOT* A ROMABLE STRUCTURE!!!! MUST BE FIXED!!!!

: VOCABULARY
   CREATE  HERE  VLINK @ , VLINK !
   HERE 2+ DUP , #THREADS CELLS DUP TALLOT ERASE
   DOES>  2+ @  CONTEXT ! ;

: DEFINITIONS ( -- )  CONTEXT @ CURRENT ! ;

{ : DOFORTH } R>  2+ @ CONTEXT ! ;
: FORTH  DOFORTH [ 0 OLD, HEADS OLD,   REVEAL


\ --------- Scr 73

: ALWAYS ( -- )  CONTEXT @ 'ALWAYS ! ;
: ONLY ( -- )
  'SEARCH #SEARCH CELLS ERASE  CONTEXT @ 'SEARCH ! ;

: -CONT ( -- )  'SEARCH  #SEARCH 1-
  FOR  DUP @ CONTEXT @ =
     IF  R>DROP 0 >R  DUP 2+  SWAP OVER  'SEARCH #SEARCH 2* +
        SWAP-  CMOVE 0  0 'SEARCH #SEARCH 2* 2- + !
     ELSE  2+  THEN  NEXT  DROP ;

: ALSO ( -- )  -CONT  'SEARCH DUP 2+ #SEARCH 2* 2- CMOVE>
  CONTEXT @  'SEARCH ! ;
: -ALSO ( -- )  -CONT   'SEARCH @ CONTEXT ! ;

{ : .VOC } ( a)  ?DUP  IF
  DUP ['] FORTH 4 + @ = IF ." FORTH " DROP
     ELSE  6 - LINK>NAME ID.  THEN
  ELSE ." --- "  THEN ;

: .CONTEXT  CR ." CONTEXT: " CONTEXT @ .VOC ;
: .CURRENT  CR ." CURRENT: " CURRENT @ .VOC ;
: .ALWAYS   CR ." ALWAYS:  " 'ALWAYS @ .VOC ;

{ : INIT-LINKS } \ jam the forth threads
   [  LINKS  OLD>TA OLDliteral ] HEADS #THREADS CELLS CMOVE
   FORTH ONLY  ALSO  DEFINITIONS  ALWAYS ;

: .ORDER
    CR  ." ORDER:   "  'SEARCH
    #SEARCH 1- FOR  DUP @  .VOC 2+  NEXT  DROP ;

: ORDER  .CONTEXT  .ORDER  .ALWAYS  .CURRENT ;

: VOCS
   CR  VLINK @
   BEGIN  DUP  WHILE
      DUP  ['] FORTH 2+ = IF ." FORTH "
         ELSE DUP 2 - CODE>LINK LINK>NAME ID. THEN
      @  REPEAT  DROP ;

\ --------- Scr 77

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

\ --------- Scr 79

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
      KEY? IF  R>DROP 0 >R  THEN
      DDUP DUMP-LINE  16 +
   NEXT DDROP    R> BASE ! ;

: DUMP ( a #)
   DPR@ -ROT DUMPL ;

\ --------- Scr 81

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
  >VECTOR  DUP >R  SWAP  >TA U2/  SWAP !  RETURN  R> 2+ ! ;

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

\ --------- Scr 86

\ OPTIMIZE  NOOPTIMIZE  NOHWMULT  HWMULT

: OPTIMIZE    OPTIMIZING ON  ;  \ Turn on optimization
: NOOPTIMIZE  OPTIMIZING OFF ;  \ Turn off optimization

: NOHWMULT  STEP ON  ;    \ Step math
: HWMULT    STEP OFF ;    \ Hardware multiplier

: \\  -OPT ON ; IMMEDIATE HIDE   \ Break optimization

\ --------- Scr 87

{ : (IO) }
 DUP $20 > OVER 0< OR
 ABORT" literal value 0-31 must precede instruction"
 OR , RETURN , -4 TALLOT HERE EXECUTE  ;

{ : <G!> }  ( n g)    $BE80 SWAP (IO) ;
{ : <U!> }  ( n u)    $CE80 SWAP (IO) ;
{ : <G@> }  ( g - n)  $BE00 SWAP (IO) ;
{ : <U@> }  ( u - n)  $CE00 SWAP (IO) ;

{ : LIT>INST } ( mask)
  HERE 2- @ DUP  $FFE0 AND $BE40 = NOT
  ABORT" Literal value 0-31 must precede instruction "
  -2 TALLOT   $1F AND  OR  , ;

{ : [G!] }  ( n g)    $BE80 LIT>INST ;
{ : [U!] }  ( n u)    $CE80 LIT>INST ;
{ : [G@] }  ( g - n)  $BE00 LIT>INST ;
{ : [U@] }  ( u - n)  $CE00 LIT>INST ;

: G@ ( g - n )
  STATE @  IF [G@] ELSE <G@> THEN ; IMMEDIATE HIDE

: G! ( n g )
  STATE @  IF [G!] ELSE <G!> THEN ; IMMEDIATE HIDE

: U@ ( u - n )
  STATE @  IF [U@] ELSE <U@> THEN ; IMMEDIATE HIDE

: U! ( n u)
  STATE @  IF [U!] ELSE <U!> THEN ; IMMEDIATE HIDE

\ --------- Scr 90

\ .REG                                           10:13 01/16/90
: .REG ( --- )
 SPR@  BASE @ >R HEX   MD@ >R  ( SPR) >R
 CR  6 SPACES  ." *** RTX Machine Registers ***" CR
 ." T   =  " DUP N.  ." N   =  " OVER N.  CPU-TYPE @ 2 <>
 IF 0 0 THEN  MHR@ MLR@  TC2@ TC1@ TC0@  UBR@  IBC@  CPR@  UPR@
 DPR@  IPR@  IMR@  IVR@  R> ( SPR)  SR@  R@  SQ@  CR@
 DR> R@ ( PC) -ROT D>R   ." PC  =  " N. CR
 ." CR  =  " N. ." SQ  =  "  N.  ." MD  =  " N. CR
 ." SR  =  " N. ." SPR =  "  N. ." IVR =  "  N. CR
 ." IMR =  " N. ." IPR =  "  N. ." DPR =  "  N. CR
 ." UPR =  " N. ." CPR =  "  N. ." IBC =  "  N. CR
 ." UBR =  " N. ." TC0 =  "  N. ." TC1 =  "  N. CR
 ." TC2 =  " N.   CPU-TYPE @ 2 = IF ." RX  =  "  N. ." RH  =  "
 ELSE ." MLR =  "  N. ." MHR =  "  THEN  N. CR   CPU-TYPE @
 1 >  IF ." SUR =  " SUR@ N. CR THEN  R> MD!  R> BASE ! ;

\ --------- Scr 93

{ : BITS9-0 } ( a -- a')    ( extracts lower 9 addr bits)
   $03FF AND ;
{ : BITS15-10 } ( a -- a')  ( extracts page addr bits)
   $FC00 AND ;
{ : BIG-JUMP? } ( ai a -- ai a f)
   DDUP  >TA  BITS15-10 SWAP  >TA  BITS15-10  -  ABS $0400 > ;

$8800 CONSTANT ?BRANCH
$9000 CONSTANT BRANCH

: FIXUP  ( ai a -- )
   BIG-JUMP?
   IF  ." Branch offset too large "  DDROP
   ELSE  DUP  >TA BITS9-0 2/ ( ->word)
      >R ( extract addr bits & shift)
      ( a ai->) >TA  BITS15-10  OVER 2+ >TA  BITS15-10  -
      DUP IF ( ai n)  ( not current page)
             0> IF $0200 ( next ) ELSE $0600 ( prev) THEN
          ELSE  ( ai n) DROP $0000 ( current)  THEN   R> OR
      ( ai m)  OVER @ OR SWAP ! THEN ;

: FIX-LEAVES  ( old LEAVING  addr --)
   >R  BEGIN  LEAVING @  ?DUP  WHILE
      DUP @ LEAVING !              ( Pointer to next in chain)
      BRANCH OVER !  R@ FIXUP  REPEAT
   R>DROP  LEAVING ! ;                ( Next higher DO )

\ --------- Scr 96

: DOES>
  ?COMP  COMPILE DOES  COMPILE R>  -OPT ON ; IMMEDIATE HIDE

{ : TIMMEDIATE }   PREVIOUS $040 OR  SWAP C! ;

: IS-CODE          PREVIOUS $020 OR  SWAP C! ;

: SMUDGE     LAST @ LINK>NAME 1+ DUP C@ $080 OR  SWAP C! ;
: UNSMUDGE   LAST @ LINK>NAME 1+ DUP C@ $07F AND SWAP C! ;

\ --------- Scr 97

{ : PREV-INSTR } ( - a ins t/f)
   OPTIMIZING @ NOT  IF  0 EXIT  THEN
   -OPT @ IF  0 EXIT  THEN
   LAST-ADDR @ @ $9FFF U<  IF  0 EXIT  THEN
   LAST-ADDR @ DUP @ -1 ;

: exit
   PREV-INSTR
   IF  -RETURN @
      IF  DDROP  RETURN ,
      ELSE  $20 OR  SWAP !  THEN
   ELSE  RETURN ,  THEN ;

\ --------- Scr 98

: ]  STATE ON  -OPT ON  ;
: [  STATE OFF ; IMMEDIATE

: :  HEADER SMUDGE ] ;  HIDE

: ;
   ?COMP  exit  STATE OFF  -OPT ON  UNSMUDGE ;  IMMEDIATE HIDE

: EXIT
   ?COMP  exit ; IMMEDIATE  HIDE

\ --------- Scr 99

: COMPILING ( a flag)
   STATE @ = IF ( not immediate)
      DUP CODE>LINK LINK>NAME  C@ $20 AND
      IF ( this is a "code" reference, which the meta compiler exploits)
         @ ( value in cfa is either an opcode or a subroutine address)
         DUP 0< IF ( opcode, strip return bit)  $FFDF AND  THEN  ,
      ELSE  ( subroutine)
         >TA U2/ ,
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
   AGAIN ;  RECOVER

\ --------- Scr 101  NEW DISK INTERFACE

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

\ --------- Scr 108

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

\ --------- Scr 109

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

: CPU-TYPE?
  SPR@ $0FF AND 0<>
    IF   1                                     \ 2000
    ELSE  2  >R MULTIPLIER? R> OR  THEN        \ 2010/2001A
  CPU-TYPE ! ;

\ --------- Scr 111

\ .MESSAGE                                       09:20 01/16/90

{ : DISCLAIMER }   ( print if power up or cold start )
   CR ." This software is provided by Harris Corporation, Semiconductor Sector"
   CR ." as a courtesy to its customers free of charge.  The programs and other"
   CR ." contents of the disk are not released products, and therefore are not"
   CR ." supported by Harris Corporation and no warranty of any kind applies."
   CR ." Any liability resulting from use of this software is specifically"
   CR ." disclaimed by Harris Corporation."
   CR  ;

: COPYRIGHT
   CR ." App-Forth  Thu  04-12-1990  16:06:19 Version 0.01"
   CR
   CR ." COPYRIGHT 1990 HARRIS CORPORATION"
   CR ." ALL RIGHTS RESERVED"
   CR ;

\ ----------

: abort
   CPU-TYPE @ 2 <  IF  $-100 SPR@+  ELSE  0 SPR@+  THEN
   $FF00 AND  SPR!              \ Clear parameter stack
   CPU-TYPE @ 1 = IF 0 THEN    \ 0 for stack underflow if 2000
   FLINE# @ ?DUP IF
      ."  ... error at line "
      BASE @  DECIMAL  SWAP .  BASE !
      HANDLE @ CLOSE-FILE
   THEN
   'TIB 2+ 'TIB !
   0 FLINE# !
   'idle @ EXECUTE ;

\ --------- Scr 112

OLDCREATE ROM
                  10 OLD,  \ BASE
                   1 OLD,  \ -OPT
                   0 OLD,  \ -RETURN
      '' QUIT OLD>TA OLD,  \ 'idle
     '' abort OLD>TA OLD,  \ 'abort
    '' header OLD>TA OLD,  \ 'header
       '' XMT OLD>TA OLD,  \ emit?
      '' RCV? OLD>TA OLD,  \ key?
       '' XMT OLD>TA OLD,  \ emit
       '' RCV OLD>TA OLD,  \ key
                   0 OLD,  \ STATE
                   0 OLD,  \ DA
  '' FORTH 2+ OLD>TA OLD,  \ VLINK
               $8000 OLD,  \ FENCE
                   0 OLD,  \ fline#

\ --------- Scr 113

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
   ABORT ;  RECOVER


\ ----------

'' RESET  OLD>TA  U2/  $A000 !

\ --------- Scr 114

: CONSTANT  CREATE -2 TALLOT literal  exit ;
   HIDE

: DCONSTANT  CREATE -2 TALLOT
   SWAP literal literal  exit ;
   HIDE
{ : TVARIABLE } CREATE 0 , ;
{ : TDVARIABLE } TVARIABLE  2 TALLOT ;

\ --------- Scr 115

: .(  ASCII ) WORD  COUNT TYPE ; IMMEDIATE
: >BODY ( a -- a) ;
\ : I R@ ;
: I ( -- n)  R>  R@ SWAP  >R; ;
: J ( -- n)  R> R> R> I SWAP >R SWAP >R SWAP >R; ;
: ( ASCII ) WORD DROP ;  IMMEDIATE  HIDE
: \   #TIB @ >IN ! ;   IMMEDIATE HIDE

: >R;   $BEA7 , ; IMMEDIATE HIDE

\ --------- Scr 116

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

\ --------- Scr 117

{ : BACK-SHIFT }
   PREV-INSTR
   IF   DUP $F01F AND $A000 =
      IF  -1  ELSE  DDROP 0  THEN
   ELSE  0  THEN
   IF  ROT ( mask)  OR  SWAP !
   ELSE   $A000 OR , THEN ;

\ --------- Scr 118

{ : TEST->LIT }  ( opmask a ins testmask -- t / mask f)
   OVER AND  $0E00 =
   IF  $F0FF AND  $0080 OR  ROT  OR  SWAP  ! -1 EXIT  THEN
   DDROP 0 ;

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

\ --------- Scr 121

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
$BE0B UCODE IVR@     $BE8B UCODE SLR! ( 2000/2010)
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

\ --------- Scr 124

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

\ --------- Scr 126

{ : noret } , -RETURN ! ;

: R> ?COMP  1 $BE01 noret ;           IMMEDIATE HIDE
: DR> ?COMP  COMPILE R> COMPILE R> ;  IMMEDIATE HIDE
: R>DROP ?COMP  1 $B001 noret ;       IMMEDIATE HIDE
: R>DROP>R ?COMP  1 $BE80 noret ;     IMMEDIATE HIDE
: >R ?COMP  1 $BE81 noret ;           IMMEDIATE HIDE
: D>R ?COMP  COMPILE >R COMPILE >R ; IMMEDIATE HIDE
: EXECUTE STATE @ IF 1 $BE87 noret ELSE EXECUTE THEN ; IMMEDIATE
 HIDE

: OF(    ?COMP  1 $BE82 noret ; IMMEDIATE HIDE
: TIMES  ?COMP  1 $BE82 noret ; IMMEDIATE HIDE
: EXTRA( ?COMP  1 $BE82 noret ; IMMEDIATE HIDE

\ --------- Scr 127

: UCODE  HEADER  $020 OR , IS-CODE ;  HIDE

: LONG ( p a)
   DDUP  0 LAST @ 2+  DSWAP  HERE LAST @ 2+ - DUP >R  CMOVEL
   R> 2- NEGATE TALLOT  SWAP  , U2/ ,  TIMMEDIATE
   DOES>  STATE @
   IF  D@  literal  COMPILE CPR!  ,
   ELSE  D@ SWAP EXECUTEL THEN ;

\ --------- Scr 128

\ 2010 Opcodes                                   15:41 03/20/90

\ BE12 UCODE MXR@            B00E UCODE DSLL
\ BE92 UCODE MXR!            B012 UCODE SMACS
\ B00C UCODE CLEARACC        B092 UCODE SMACA
\ B017 UCODE MULACS
\ B015 UCODE MULACM
\ B014 UCODE MULSUB
\ B013 UCODE MULM
\ B016 UCODE MULACU
\ B011 UCODE RSACC
\ B008 UCODE 0=2010
\ B00F UCODE NORM
\ B00A UCODE DSRL
\ B009 UCODE DSRA

\ --------- Scr 129

: IF  ( -- a) ?COMP
  HERE  ?BRANCH , ;   IMMEDIATE

: ELSE ( a1 -- a2)  ?COMP
  HERE  BRANCH , SWAP HERE FIXUP ; IMMEDIATE

: THEN ( a)   ?COMP
  HERE  FIXUP  -OPT ON  ;   IMMEDIATE

: BEGIN  ( -- a->)  ?COMP
  LEAVING @ 0 LEAVING !  HERE  -OPT ON  ;   IMMEDIATE

: (AGAIN)  ( a->)
  HERE SWAP  BRANCH ,  FIXUP HERE FIX-LEAVES ;
: AGAIN  ( a->)  ?COMP  (AGAIN) ; IMMEDIATE

: REPEAT ( BEGIN WHILE --)   ?COMP
  >R SWAP (AGAIN)  R> HERE FIXUP  -OPT ON ;  IMMEDIATE

: UNTIL  ( a->)   ?COMP
  HERE SWAP ?BRANCH ,  FIXUP HERE FIX-LEAVES ;  IMMEDIATE

: WHILE  ( LEAVE-ptr BEGIN -- BEGIN LEAVE-ptr WHILE)
  ?COMP   SWAP HERE ?BRANCH , ;   IMMEDIATE

: LOOP   ?COMP  COMPILE loop  (AGAIN)
  COMPILE R>DROP   COMPILE R>DROP   1 -RETURN ! ; IMMEDIATE
 HIDE

: +LOOP   ?COMP  COMPILE +loop  (AGAIN)
  COMPILE R>DROP   COMPILE R>DROP   1 -RETURN ! ; IMMEDIATE
 HIDE

: DO  ( limit index )  ?COMP  LEAVING @  0 LEAVING !
  COMPILE do  HERE ;   IMMEDIATE HIDE

: LEAVE
  ?COMP  HERE LEAVING @ , LEAVING ! ;  IMMEDIATE HIDE

: FOR  ?COMP  COMPILE >R HERE ; IMMEDIATE HIDE
: NEXT  ?COMP  HERE $9800 , SWAP FIXUP ; IMMEDIATE HIDE
: -ZERO  DROP  HERE BRANCH ,  HERE  SWAP ;  IMMEDIATE HIDE

\ --------- Scr 133

: VARIABLE TVARIABLE ;
: DVARIABLE TDVARIABLE ;
: ALLOT TALLOT ;
: IMMEDIATE TIMMEDIATE ;

\ ----------

LINKS } PRUNE ( Addr left by PRUNE)
 DUP $A000 - HEX U. DECIMAL .( bytes compiled.  Max is 4000h.)
   SWAP $010 CMOVE

?SAVE

: TEST   0 COLD !  $A000 >R ;


