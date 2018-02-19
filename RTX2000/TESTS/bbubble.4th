\ /* bubble.c */
\ GNU C for RTX 2000

EMPTY
: XY " DOS XY bbubble.4" EVALUATE ;

DECIMAL
load gnutool.4th

VARIABLE seed   4   CELL- ALLOT

VARIABLE top   4   CELL- ALLOT

VARIABLE littlest   4   CELL- ALLOT

VARIABLE biggest   4   CELL- ALLOT

VARIABLE sortlist   1004   CELL- ALLOT

: .DATA
  1000 2 DO I SORTLIST + @ .  2 +LOOP ;

#REGS 100 - REG-ADDR $FFC0 AND  UBR!

\ gcc_compiled.:


( RTX 2000 code generation)

 64 FRAME_SIZE ! 

 : initrand  ( FUNC )   ( 3  top> empty )
 9219  ( LIT)       ( 6  top> empty )
 seed  ( SYMBOL)    ( 6  top> #0x )
 !                ( 6  top> #0x #0x )
   ;  ( END )   

 ( 6  top> empty )

( RTX 2000 code generation)

 64 FRAME_SIZE ! 

 : rand  ( FUNC )   ( 3  top> empty )
 seed  ( SYMBOL)    ( 6  top> empty )
 @                ( 6  top> #0x )
 1309  ( LIT)       ( 6  top> #0x )
 *                ( 6  top> #0x #0x )
 13849  ( LIT)       ( 8  top> #64d )
 +                ( 8  top> #0x #0x )
 seed  ( SYMBOL)    ( 10  top> #0x )
 TUCK_! ( #0)    ( 10  top> #0x #0x )
   ;  ( END )   

 ( 11  top> #2 )

( RTX 2000 code generation)

 64 FRAME_SIZE ! 

 : binitarr  ( FUNC )   ( 3  top> empty )
 -64 FP+!  ( Link)   ( 7  top> empty )
   initrand  ( CALL)    ( 7  top> empty )
 64 FP+!  ( Unlink)  ( 7  top> empty )
 0  ( LIT)       ( 9  top> empty )
 biggest  ( SYMBOL)    ( 9  top> #0x )
 !                ( 9  top> #0x #0x )
 0  ( LIT)       ( 10  top> empty )
 littlest  ( SYMBOL)    ( 10  top> #0x )
 !                ( 10  top> #0x #0x )
 1  ( LIT)       ( 13  top> #64 )
 [ 0010 ADD_INDEX ] >R   ( 86  top> #64 )
 ( TYPE 1 LOOP BEGIN)  ( 14  top> empty )
 [ 0010 ] LABEL     ( 74  top> empty )
 -64 FP+!  ( Link)   ( 23  top> empty )
   rand  ( CALL)    ( 23  top> empty )
 64 FP+!  ( Unlink)  ( 23  top> #2x )
 [ 0010 ]  INDEX  ( 28  top> #67 )
 2*               ( 28  top> #64x #67 )
 [ sortlist ] SYMBOL_+  ( 29  top> #69d #67 )
 OVER          ( #67)  ( 30  top> #70 #67 )
 10000  ( LIT)       ( 30  top> #0x #70 #67 )
 /                ( 30  top> #0x #0x #70 #67 )
 10000  ( LIT)       ( 31  top> #71d #70 #67 )
 *                ( 31  top> #0x #0x #70 #67 )
 2_PICK   ( LAST #67)  ( 32  top> #72d #70 #67d )
 SWAP-            ( 32  top> #0x #72d #70 #67d )
 5000  ( LIT)       ( 34  top> #73d #70d #67d )
 -                ( 34  top> #0x #0x #70d #67d )
 SWAP !           ( 34  top> #0x #70d #67d )
 DROP             ( 34  top> #67d )
 [ 0010 ]  INDEX  ( 37  top> empty )
 2*               ( 37  top> #64x )
 [ sortlist ] SYMBOL_+  ( 38  top> #76d )
 @                ( 40  top> #77d )
 biggest  ( SYMBOL)    ( 41  top> #78 )
 @                ( 41  top> #0x #78 )
 >                ( 43  top> #79x #78x )
 [ 007 ] BRANCHZ   ( 43  top> #0x )
 [ 0010 ]  INDEX  ( 47  top> empty )
 2*               ( 47  top> #64x )
 [ sortlist ] SYMBOL_+  ( 48  top> #81d )
 @                ( 49  top> #82d )
 biggest  ( SYMBOL)    ( 49  top> #0x )
 !                ( 49  top> #0x #0x )
 [ 006 ] BRANCH    ( 50  top> empty )
 [ 007 ] LABEL     ( 52  top> empty )
 [ 0010 ]  INDEX  ( 56  top> empty )
 2*               ( 56  top> #64x )
 [ sortlist ] SYMBOL_+  ( 57  top> #84d )
 @                ( 58  top> #85d )
 littlest  ( SYMBOL)    ( 59  top> #86 )
 @                ( 59  top> #0x #86 )
 <                ( 61  top> #87x #86x )
 [ 106 ] BRANCHZ   ( 61  top> #0x )
 [ 0010 ]  INDEX  ( 65  top> empty )
 2*               ( 65  top> #64x )
 [ sortlist ] SYMBOL_+  ( 66  top> #89d )
 @                ( 67  top> #90d )
 littlest  ( SYMBOL)    ( 67  top> #0x )
 !                ( 67  top> #0x #0x )
 [ 006 ] LABEL     ( 72  top> empty )
 [ 106 ] LABEL     ( 72  top> empty )
 R>               ( 73  top> empty )
 1  ( LIT)       ( 73  top> #64x )
 +                ( 73  top> #0x #64x )
 DUP_>R   ( #64 )  ( 17  top> #64 )
 500  ( LIT)       ( 17  top> #64x )
 U>               ( 18  top> #66x #64x )
 [ 0010 ] BRANCHZ   ( 18  top> #0x )
 [ 0010 DROP_INDEX ] R>DROP  ( 79  top> empty )
 EXIT  [ 0011 ] LABEL     ( 87  top> empty )
   ;  ( END )   

 ( 81  top> empty )

( RTX 2000 code generation)

 64 FRAME_SIZE ! 

 : main  ( FUNC )   ( 3  top> empty )
#REGS 100 - REG-ADDR $FFC0 AND  UBR!
 -64 FP+!  ( Link)   ( 7  top> empty )
   binitarr  ( CALL)    ( 7  top> empty )
 64 FP+!  ( Unlink)  ( 7  top> empty )
 500  ( LIT)       ( 9  top> empty )
 top  ( SYMBOL)    ( 90  top> #0x )
 TUCK_! ( #0)    ( 90  top> #0x #0x )
 1  ( LIT)       ( 92  top> #66x )
 >                ( 94  top> #67x #66x )
 [ 0021 ] BRANCHZ   ( 94  top> #0x )
 [                ( 96  top> empty )
 sortlist  ( SYMBOL)    ( 96  top> empty )
 2  ( LIT)       ( 96  top> #0x )
 +                ( 96  top> #0x #0x )
 ] LITERAL        ( 96  top> #0x )
 2 U!            ( 12  top> #72 )
 ( LOOP_BEGIN)    ( 12  top> empty )
 [ 0019 ] LABEL     ( 73  top> empty )
 1  ( LIT)       ( 20  top> empty )
 top  ( SYMBOL)    ( 84  top> #64 )
 @                ( 84  top> #0x #64 )
 OVER          ( #64)  ( 85  top> #68d #64 )
 3 U!            ( 85  top> #64 #68d #64x )
 <                ( 87  top> #68x #64x )
 [ 0020 ] BRANCHZ   ( 87  top> #0x )
 ( LOOP_BEGIN)    ( 22  top> empty )
 [ 0018 ] LABEL     ( 62  top> empty )
 3 U@            ( 30  top> empty )
 2*               ( 30  top> #64x )
 [ sortlist ] SYMBOL_+  ( 31  top> #70d )
 3 U@            ( 33  top> #71 )
 2*               ( 33  top> #64x #71 )
 2 U@  +         ( 34  top> #73d #71 )
 @_SWAP           ( 35  top> #74 #71d )
 @_SWAP           ( 35  top> #71x #75 )
 >                ( 38  top> #76x #75x )
 [ 0017 ] BRANCHZ   ( 38  top> #0x )
 3 U@            ( 42  top> empty )
 2*               ( 42  top> #64x )
 [ sortlist ] SYMBOL_+  ( 43  top> #78d )
 @                ( 44  top> #79d )
 3 U@            ( 47  top> #65 )
 2*               ( 47  top> #64x #65 )
 [ sortlist ] SYMBOL_+  ( 48  top> #81d #65 )
 3 U@            ( 50  top> #82 #65 )
 2*               ( 50  top> #64x #82 #65 )
 2 U@  +         ( 51  top> #84d #82 #65 )
 @_SWAP           ( 52  top> #85d #82d #65 )
 !                ( 52  top> #82x #0x #65 )
 3 U@            ( 55  top> #65 )
 2*               ( 55  top> #64x #65 )
 2 U@  +         ( 56  top> #87d #65 )
 !                ( 57  top> #88d #65d )
 [ 0017 ] LABEL     ( 58  top> empty )
 3 U@            ( 61  top> empty )
 1  ( LIT)       ( 61  top> #64x )
 +                ( 61  top> #0x #64x )
 top  ( SYMBOL)    ( 24  top> #64 )
 @                ( 24  top> #0x #64 )
 OVER          ( #64)  ( 25  top> #68d #64 )
 3 U!            ( 25  top> #64 #68d #64x )
 >=               ( 26  top> #68x #64x )
 [ 0018 ] BRANCHZ   ( 26  top> #0x )
 ( LOOP_END)      ( 67  top> empty )
 [ 0020 ] LABEL     ( 86  top> empty )
 -1  ( LIT)       ( 72  top> empty )
 top  ( SYMBOL)    ( 72  top> #0x )
 +!               ( 72  top> #0x #0x )
 top  ( SYMBOL)    ( 14  top> empty )
 @                ( 14  top> #0x )
 1  ( LIT)       ( 16  top> #66x )
 <=               ( 17  top> #67x #66x )
 [ 0019 ] BRANCHZ   ( 17  top> #0x )
 ( LOOP_END)      ( 78  top> empty )
 EXIT  [ 0021 ] LABEL     ( 93  top> empty )
   ;  ( END )   

 ( 80  top> empty )
