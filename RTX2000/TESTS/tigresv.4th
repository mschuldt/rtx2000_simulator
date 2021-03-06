\ TIGRE for the RTX 2000  using AppForth
\ (C) Copyright 1990, all rights reserved -- Philip Koopman

: xy " dos xy tigre.4 " evaluate ;

DECIMAL
FORGET TASK : TASK ;

: CELLS  2* ;

2 CONSTANT CELL-SIZE
2 CELLS CONSTANT HEAP-CELL-SIZE
: HEAP-CELLS  2* 2* ;
: t\  [compile] \ ; immediate

5000 CONSTANT #HEAP-CELLS      

VARIABLE HEAP-SPACE   #HEAP-CELLS  HEAP-CELL-SIZE *  100 + ALLOT
HEAP-SPACE  #HEAP-CELLS HEAP-CELL-SIZE * + CONSTANT HEAP-LIMIT


\ VARIABLE  2 U
: INIT-HEAP  ( -- )
  here 100 + ubr! HEAP-SPACE   2 U! ;

: HEAP_ALLOCATE   ( nbytes -- first.addr )   t\ ."  ALLOCATE " DUP .
  2 U@  +  DUP  2 U!
  DUP HEAP-LIMIT >  ABORT" Out of heap space!"  ;

: ^   HEAP-CELLS  HEAP-SPACE +  >ta  U2/ ;
: ^'  ?COMP  '  >ta  U2/ <literal>   ; IMMEDIATE
: h,  SWAP   2 U@  !  1 CELLS   2 U@ + 2 U!
             2 U@  !  1 CELLS   2 U@ + 2 U! ;

: ]cells  " CELLS ] literal " evaluate  ; IMMEDIATE
: eval    " [ here 8 + ] literal >R >R; " evaluate  ; IMMEDIATE


\   I x -> x    ( perform a jump to x )
: {I}   ( -- )    ( RS: ^myrhs -- )   t\ ."  I "
  R> @  2* >R;  ;

\   K c x -> I c  ( perform a jump to c )
: {K}   ( -- )    ( RS: ^parentrhs ^myrhs -- )   t\ ."  K "
  R> @  R> DROP  2* >R;  ;

\   S f g x ->  (f x) (g x) ( perform a jump to rhs address )
: {S}   ( -- )    ( RS: ^grandrhs ^parentrhs ^myrhs -- )   t\ ."  S "
  [ 2 HEAP-CELLS ] literal HEAP_ALLOCATE    ( -- addr )
  R> @  OVER !                       \ graph: (f -) (- -)
  R> @  OVER      [ 2 ]cells + !     \ graph: (f -) (g -)
  R@ @  DDUP SWAP [ 1 ]cells + !     \ graph: (f x) (g -)
        OVER      [ 3 ]cells + !     \ graph: (f x) (g x)
  U2/ DUP  R@    [ -1 ]cells + !     \ graph: ((f x) (g x)
                  [ 1 ]cells +  R@ ! \ graph: ((f x) (g x))
  R>  [ -1 ]cells +  >R;  ;

\   LIT  ( perform a return from evaluation call
: {LIT}   ( -- n )    ( RS: raddr ^myrhs -- )   t\ ."  LIT "
  R> @ ;

\   + x y -> LIT sum  ( perform a return )
: {+}   ( -- )    ( RS: raddr ^parentrhs ^myrhs -- )  t\ ."  + "
  R> @ 2* eval      ( evaluate 1st argument )
  R@ @ 2* eval   +  ( evaluate 2nd argument )
  DUP R@ !    ^' {LIT}  R>  [ -1 ]cells +  ! ;

\   - x y -> LIT sum  ( perform a return )
: {-}   ( -- )    ( RS: raddr ^parentrhs ^myrhs -- )  t\ ."  - "
  R> @ 2* eval      ( evaluate 1st argument )
  R@ @ 2* eval   -  ( evaluate 2nd argument )
  DUP R@ !    ^' {LIT}  R>  [ -1 ]cells +  ! ;

\   < x y -> LIT sum  ( perform a return )
: {<}   ( -- )    ( RS: raddr ^parentrhs ^myrhs -- )  t\ ."  < "
  R> @ 2* eval      ( evaluate 1st argument )
  R@ @ 2* eval   <  ( evaluate 2nd argument )
  DUP R@ !    ^' {LIT}  R>  [ -1 ]cells +  ! ;

\   IF c x y -> I x || I y
: {IF}   ( -- )    ( RS: ^grandrhc ^parentrhs ^myrhs -- )  t\ ."  IF "
  R> @ 2* eval      ( evaluate argument )
  IF  R> @   R@ !   ELSE  R> DROP  THEN
  ^' {I}  R@  [ -1 ]cells +  ! 
  R> @  2*  >R;  ;

\ Fast constants for small integers
: {1} 1 ;
: {2} 2 ;
: {3} 3 ;


\ suc(n) = n+1
\ ((S ((S (K +) ) (K 1) ) ) I ).
: SUC  ( n -- suc )
  INIT-HEAP
   ( 0 )  2 ^   1 ^ h,
     ( 1 )  ^' {LIT}  SWAP h,
     ( 2 )  3 ^  ^' {I} h,
       ( 3 )  ^' {S}  4 ^ h,
         ( 4 )  5 ^  7 ^ h,
           ( 5 )  ^' {S}  6 ^ h,
             ( 6 )  ^' {K}  ^' {+} h,
           ( 7 )  ^' {K}  8 ^ h,
             ( 8 )  ^' {LIT}   1 h,
  2 CELLS HEAP_ALLOCATE DROP HEAP-SPACE >R; 
 ;

\ computes nth Fibonacci number
\ ((S ((S ((S (K IF)) ((S <)  (K 3)))) (K 1)))
\     ((S ((S (K +)) ((S (K CYCLE)) ((S -) (K 1)))))
\    ((S (K CYCLE)) ((S -) (K 2))))).
\ Note: CYCLE refers to node 2, which is root of function subtree
: <FIB>  ( n -- suc )
  INIT-HEAP
   ( 0 )  2 ^   1 ^ h,
     ( 1 )  ^' {LIT}  SWAP h,
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
                            ( 12 )  ^' {LIT}   3 h,
           ( 13 )  ^' {K}  ^' {1} h,
                            ( 14 )  ^' {LIT}   1 h,
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
                            ( 26 )  ^' {LIT}   1 h,
         ( 27 )  28 ^   30 ^ h,
           ( 28 )  ^' {S}   29 ^ h,
             ( 29 )  ^' {K}   2 ^ h,   \ cycle
           ( 30 )  31 ^   32 ^ h,
             ( 31 )  ^' {S}   ^' {-} h,
             ( 32 )  ^' {K}   ^' {2} h,
                            ( 33 )  ^' {LIT}   2 h,
 ;

: FIB  <FIB>    2 CELLS HEAP_ALLOCATE DROP  HEAP-SPACE >R; 
 ;

: FIB-TABLE  ( -- )
  CR  ."  n    FIB(n)"
  CR  ." ---   ------"  CR
  13 1 DO
       I  3 U.R   I FIB  7 U.R  CR
  LOOP  ;

: MYDUMP  CR HEX
   0
   20 FOR  
   DUP HEAP-SPACE + U. ." --  " 
   DUP HEAP-SPACE + @  2* U.  2 SPACES  2+
   DUP HEAP-SPACE + @  2* U.  2 SPACES  2+  
   CR NEXT  DROP ;

: TEST ( -- )
  INIT-HEAP
   ( 0 )  1 ^   3 ^ h,
     ( 1 )  ^' {<}  2 ^ h,
       ( 2 )  ^' {LIT}   10  h,
     ( 3 )  ^' {LIT}     20  h,
  2 CELLS HEAP_ALLOCATE DROP   HEAP-SPACE >R; 
 ;


: XX FOR 12 <FIB> NEXT ;
: YY FOR 12 FIB DROP NEXT ;
