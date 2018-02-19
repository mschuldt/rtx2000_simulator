\ TIGRE for the RTX 2000  using AppForth
\ (C) Copyright 1990, all rights reserved -- Philip Koopman
DECIMAL

\ ---------- OPCODES FOR SPEED

: U>   " \\ SWAP- CU2/ NOT 0< " EVALUATE ; IMMEDIATE

\ ----------

\ Portability & trace words
: CELLS  2* ;

: t\  [COMPILE] \ ; immediate   \ Make a nop for run-time trace

\ Heap space management
5000 CONSTANT #HEAP-CELLS       \ Size of heap for tree data
VARIABLE HEAP-SPACE   #HEAP-CELLS  CELLS 2*  100 + ALLOT
HEAP-SPACE  #HEAP-CELLS CELLS 2* +  CONSTANT HEAP-LIMIT

: INIT-HEAP  ( -- )
   HEAP-SPACE  4 G!  HEAP-LIMIT 6 G! ;

: HEAP_ALLOCATE   ( nbytes -- first.addr )
  4 G@  +  4 DUP_G!
  DUP  6 G@  U>
  IF -1 ABORT" Out of heap space!" THEN ;

\ Initial tree building helpers
: ^   CELLS 2*   HEAP-SPACE +  >ta  U2/ ;
: ^'  ?COMP  '  >TA  U2/ <literal>   ; IMMEDIATE
: h,  SWAP   4 G@  !  1 CELLS   4 G@ + 4 G!
             4 G@  !  1 CELLS   4 G@ + 4 G! ;

\ Compilation helping words
: ]cells  " CELLS ] literal " evaluate  ; IMMEDIATE
: 2args   " R> @ 2* execute " evaluate ( Evaluate 1st argument )
          " R@ @ 2* execute " evaluate ( Evaluate 2nd argument ) ; IMMEDIATE
: strict  " [ {lit} ] LITERAL OVER R> 2 !- ! " EVALUATE ; IMMEDIATE

\   I x -> x    ( perform a jump to x )
: {I}   ( -- )    ( RS: ^myrhs -- )   t\ ."  I "
  R> @  2* >R;  ;

\   K c x -> I c  ( perform a jump to c )
: {K}   ( -- )    ( RS: ^parentrhs ^myrhs -- )   t\ ."  K "
  R> @  R>DROP  2* >R;  ;

\   S f g x ->  (f x) (g x) ( perform a jump to rhs address )
: {S}   ( -- )    ( RS: ^grandrhs ^parentrhs ^myrhs -- )   t\ ."  S "
   R> @                         \ f
   R> @                         \ f g
   R@ @ DUP                     \ f g x x
   [ 4 ]cells HEAP_ALLOCATE     \ f g x x a
   [ 3 ]cells +                 \ f g x x d
   4 !-                         \ f g x b
   2 !+  DUP>R                  \ f g c
   4 !-                         \ f a
   0 !+  U2/                    \ ^a
   R>    U2/                    \ ^a ^c
   R>                           \ ^a ^c f
   2 !-                         \ ^a e
   0 !+                         \ e
   >R; ;

$DE20 CONSTANT {LIT}

\   + x y -> LIT sum  ( perform a return )
: {+}   ( -- )    ( RS: raddr ^parentrhs ^myrhs -- )  t\ ."  + "
   2args +  strict ;


: {-}   ( -- )    ( RS: raddr ^parentrhs ^myrhs -- )  t\ ."  - "
   2args - strict ;

: {<}   ( -- )    ( RS: raddr ^parentrhs ^myrhs -- )  t\ ."  < "
   2args < strict ;

\   IF c x y -> I x || I y
: {IF}   ( -- )    ( RS: ^grandrhc ^parentrhs ^myrhs -- )  t\ ."  IF "
   R> @ 2* EXECUTE
   IF
      R> @
      ^' {I}
      OVER R>
      2 !-
      !
      2* >R;
   THEN
   R>DROP
   R>
   2 @-
   ^' {I}
   SWAP !
   2* >R; ;

\ Fast constants for small integers
: {1} 1 ;
: {2} 2 ;
: {3} 3 ;

\ suc(n) = n+1
\ ((S ((S (K +) ) (K 1) ) ) I ).
: SUC  ( n -- suc )
  INIT-HEAP
   ( 0 )  2 ^   1 ^ h,
     ( 1 )  {lit}  SWAP h,
     ( 2 )  3 ^  ^' {I} h,
       ( 3 )  ^' {S}  4 ^ h,
         ( 4 )  5 ^  7 ^ h,
           ( 5 )  ^' {S}  6 ^ h,
             ( 6 )  ^' {K}  ^' {+} h,
           ( 7 )  ^' {K}  8 ^ h,
             ( 8 )  {lit}   1 h,
  2 CELLS HEAP_ALLOCATE DROP HEAP-SPACE >R;  ;

\ computes nth Fibonacci number
\ ((S ((S ((S (K IF)) ((S <)  (K 3)))) (K 1)))
\     ((S ((S (K +)) ((S (K CYCLE)) ((S -) (K 1)))))
\    ((S (K CYCLE)) ((S -) (K 2))))).
\ Note: CYCLE refers to node 2, which is root of function subtree
: <FIB>  ( n -- suc )
  INIT-HEAP
   ( 0 )  2 ^   1 ^ h,
     ( 1 )  {lit}  SWAP h,
     ( 2 )  3 ^   15 ^ h,
       ( 3 )  ^' {S}   4 ^ h,
         ( 4 )  5 ^   13 ^ h,
           ( 5 )  ^' {S}   6 ^ h,
             ( 6 )  7 ^   9 ^ h,
               ( 7 )  ^' {S}   8 ^ h,
                 ( 8 )  ^' {K}   ^' {IF} h,
               ( 9 )  10 ^   11 ^ h,
                 ( 10 )  ^' {S}   ^' {<} h,
                 ( 11 )  ^' {K}  ^' {3} h,
                            ( 12 )  {lit}   3 h,
           ( 13 )  ^' {K}  ^' {1} h,
                            ( 14 )  {lit}   1 h,
       ( 15 )  16 ^   27 ^ h,
         ( 16 )  ^' {S}   17 ^ h,
           ( 17 )  18 ^   20 ^ h,
             ( 18 )  ^' {S}   19 ^ h,
               ( 19 )  ^' {K}   ^' {+} h,
             ( 20 )  21 ^   23 ^ h,
               ( 21 )  ^' {S}   22 ^ h,
                 ( 22 )  ^' {K}   2 ^ h,   \ cycle
               ( 23 )  24 ^   25 ^ h,
                 ( 24 )  ^' {S}   ^' {-} h,
                 ( 25 )  ^' {K}   ^' {1} h,
                            ( 26 )  {lit}   1 h,
         ( 27 )  28 ^   30 ^ h,
           ( 28 )  ^' {S}   29 ^ h,
             ( 29 )  ^' {K}   2 ^ h,   \ cycle
           ( 30 )  31 ^   32 ^ h,
             ( 31 )  ^' {S}   ^' {-} h,
             ( 32 )  ^' {K}   ^' {2} h,
                            ( 33 )  {lit}   2 h,
    2 CELLS HEAP_ALLOCATE DROP
     ;

: FIB  <FIB>    HEAP-SPACE >R;  ;

: FIB-TABLE  ( -- )
  CR  ."  n    FIB(n)"
  CR  ." ---   ------"  CR
  13 1 DO
       I  3 U.R   I FIB  7 U.R  CR
  LOOP  ;

: TEST
\   TICKS
   999 FOR 12 FIB DROP  NEXT
 \  TICKS SWAP- ( ticks)
 \    547 ( raps/fib / 10, appx)
 \    182 ( ticks/sec * 10)
 \    ROT */ . ." K RAPS" 
 ;

\ : XX CR TIMER 10 FOR 999 FOR 12 <FIB> NEXT NEXT CLICK ." ( / 10) OVERHEAD" CR
\         TIMER 999 FOR 12 FIB DROP NEXT CLICK ." TOTAL RUN TIME" CR ;

