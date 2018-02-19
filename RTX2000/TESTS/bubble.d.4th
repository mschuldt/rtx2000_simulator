\ /* bubble.c */
\ Improved source code GNU C for RTX 2000

EMPTY
: XY " DOS XY bubble.d.4th" EVALUATE ;

DECIMAL
load gnutool.4th


VARIABLE seed   4   CELL- ALLOT

VARIABLE top   4   CELL- ALLOT

VARIABLE littlest   4   CELL- ALLOT

VARIABLE biggest   4   CELL- ALLOT

VARIABLE sortlist   10004   CELL- ALLOT

: .DATA
  1000 2 DO I SORTLIST + @ .  2 +LOOP ;

#REGS 100 - REG-ADDR $FFC0 AND  UBR!

: do_error
 ." Error3 in Bubble."  CR
   ;



 64 FRAME_SIZE ! 

 : Initrand  ( FUNC )   ( 3  top> empty )
 9219  ( LIT)       ( 6  top> empty )
 seed  ( SYMBOL)    ( 6  top> #0x )
 !                ( 6  top> #0x #0x )
   ;  ( END )   

 ( 6  top> empty )

( RTX 2000 code generation)

 64 FRAME_SIZE ! 

 : Rand  ( FUNC )   ( 3  top> empty )
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

 : bInitarr  ( FUNC )   ( 3  top> empty )
 -64 FP+!  ( Link)   ( 7  top> empty )
   Initrand  ( CALL)    ( 7  top> empty )
 64 FP+!  ( Unlink)  ( 7  top> empty )
 0  ( LIT)       ( 9  top> empty )
 biggest  ( SYMBOL)    ( 9  top> #0x )
 !                ( 9  top> #0x #0x )
 0  ( LIT)       ( 10  top> empty )
 littlest  ( SYMBOL)    ( 10  top> #0x )
 !                ( 10  top> #0x #0x )
 [                ( 13  top> empty )
 sortlist  ( SYMBOL)    ( 13  top> empty )
 2  ( LIT)       ( 13  top> #0x )
 +                ( 13  top> #0x #0x )
 ] LITERAL        ( 13  top> #0x )
 [                ( 65  top> #66 )
 sortlist  ( SYMBOL)    ( 65  top> #66 )
 1000  ( LIT)       ( 65  top> #0x #66 )
 +                ( 65  top> #0x #0x #66 )
 ] LITERAL        ( 65  top> #0x #66 )
 OVER          ( #66)  ( 66  top> #68d #66 )
 2 U!            ( 66  top> #66 #68d #66x )
 U>               ( 68  top> #68x #66x )
 [ 0012 ] BRANCHNZ  ( 68  top> #0x )
 ( LOOP_BEGIN)    ( 15  top> empty )
 [ 0011 ] LABEL     ( 53  top> empty )
 -64 FP+!  ( Link)   ( 25  top> empty )
   Rand  ( CALL)    ( 25  top> empty )
 64 FP+!  ( Unlink)  ( 25  top> #2x )
 DUP           ( #70)  ( 30  top> #70 )
 32767  ( LIT)       ( 30  top> #0x #70 )
 -                ( 30  top> #0x #0x #70 )
 OVER          ( #70)  ( 32  top> #65 #70 )
 32767  ( LIT)       ( 32  top> #0x #65 #70 )
 -                ( 32  top> #0x #0x #65 #70 )
 2 U@  !         ( 32  top> #0x #65 #70 )
 biggest  ( SYMBOL)    ( 35  top> #65 #70 )
 @                ( 35  top> #0x #65 #70 )
 ROT              ( 36  top> #72d #65 #70 )
 3 U!            ( 36  top> #70 #72d #65 )
 OVER          ( #65)  ( 36  top> #72d #65 )
 4 U!            ( 36  top> #65 #72d #65x )
 >                ( 37  top> #72x #65x )
 [ 008 ] BRANCHZ   ( 37  top> #0x )
 3 U@            ( 38  top> empty )
 32767  ( LIT)       ( 38  top> #70x )
 -                ( 38  top> #0x #70x )
 biggest  ( SYMBOL)    ( 38  top> #0x )
 !                ( 38  top> #0x #0x )
 [ 007 ] BRANCH    ( 39  top> empty )
 [ 008 ] LABEL     ( 41  top> empty )
 littlest  ( SYMBOL)    ( 43  top> empty )
 @                ( 43  top> #0x )
 4 U@            ( 44  top> #73d )
 >                ( 44  top> #65x #73d )
 [ 107 ] BRANCHZ   ( 45  top> #0x )
 4 U@            ( 46  top> empty )
 littlest  ( SYMBOL)    ( 46  top> #65x )
 !                ( 46  top> #0x #65x )
 [ 007 ] LABEL     ( 51  top> empty )
 [ 107 ] LABEL     ( 51  top> empty )
 2 U@            ( 52  top> empty )
 2  ( LIT)       ( 52  top> #66x )
 +                ( 52  top> #0x #66x )
 [                ( 18  top> #66 )
 sortlist  ( SYMBOL)    ( 18  top> #66 )
 1000  ( LIT)       ( 18  top> #0x #66 )
 +                ( 18  top> #0x #0x #66 )
 ] LITERAL        ( 18  top> #0x #66 )
 OVER          ( #66)  ( 19  top> #68d #66 )
 2 U!            ( 19  top> #66 #68d #66x )
 U>               ( 20  top> #68x #66x )
 [ 0011 ] BRANCHZ   ( 20  top> #0x )
 ( LOOP_END)      ( 58  top> empty )
 EXIT  [ 0012 ] LABEL     ( 67  top> empty )
   ;  ( END )   

 ( 60  top> empty )

( RTX 2000 code generation)

 64 FRAME_SIZE ! 

 : main  ( FUNC )   ( 3  top> empty )
#REGS 100 - REG-ADDR $FFC0 AND  UBR!
 1  ( LIT)       ( 6  top> empty )
 2 U!            ( 121  top> #65 )
 [                ( 124  top> empty )
 sortlist  ( SYMBOL)    ( 124  top> empty )
 2  ( LIT)       ( 124  top> #0x )
 +                ( 124  top> #0x #0x )
 ] LITERAL        ( 124  top> #0x )
 3 U!            ( 7  top> #71 )
 ( LOOP_BEGIN)    ( 7  top> empty )
 [ 0026 ] LABEL     ( 95  top> empty )
 -64 FP+!  ( Link)   ( 15  top> empty )
   bInitarr  ( CALL)    ( 15  top> empty )
 64 FP+!  ( Unlink)  ( 15  top> empty )
 [                ( 19  top> empty )
 sortlist  ( SYMBOL)    ( 19  top> empty )
 1000  ( LIT)       ( 19  top> #0x )
 +                ( 19  top> #0x #0x )
 ] LITERAL        ( 19  top> #0x )
 [ 4 ] DUP_U!         ( 116  top> #67 )
 3 U@            ( 116  top> #67x )
 U>               ( 118  top> #71x #67x )
 [ 0028 ] BRANCHZ   ( 118  top> #0x )
 ( LOOP_BEGIN)    ( 22  top> empty )
 [ 0023 ] LABEL     ( 65  top> empty )
 [                ( 32  top> empty )
 sortlist  ( SYMBOL)    ( 32  top> empty )
 2  ( LIT)       ( 32  top> #0x )
 +                ( 32  top> #0x #0x )
 ] LITERAL        ( 32  top> #0x )
 [ 5 ] DUP_U!         ( 111  top> #66 )
 4 U@            ( 111  top> #66x )
 U<               ( 113  top> #67x #66x )
 [ 0027 ] BRANCHZ   ( 113  top> #0x )
 ( LOOP_BEGIN)    ( 34  top> empty )
 [ 0022 ] LABEL     ( 55  top> empty )
 5 U@            ( 40  top> empty )
 @                ( 40  top> #66x )
 5 U@            ( 41  top> #75 )
 2  ( LIT)       ( 41  top> #66x #75 )
 +                ( 41  top> #0x #66x #75 )
 @                ( 41  top> #0x #75 )
 OVER          ( #75)  ( 42  top> #76d #75 )
 6 U!            ( 42  top> #75 #76d #75x )
 >                ( 43  top> #76x #75x )
 [ 0021 ] BRANCHZ   ( 43  top> #0x )
 5 U@            ( 48  top> empty )
 2  ( LIT)       ( 48  top> #66x )
 +                ( 48  top> #0x #66x )
 @                ( 48  top> #0x )
 5 U@  !         ( 48  top> #0x )
 6 U@            ( 50  top> empty )
 5 U@            ( 50  top> #75x )
 2  ( LIT)       ( 50  top> #66x #75x )
 +                ( 50  top> #0x #66x #75x )
 !                ( 50  top> #0x #75x )
 [ 0021 ] LABEL     ( 51  top> empty )
 5 U@            ( 54  top> empty )
 2  ( LIT)       ( 54  top> #66x )
 +                ( 54  top> #0x #66x )
 [ 5 ] DUP_U!         ( 36  top> #66 )
 4 U@            ( 36  top> #66x )
 U<               ( 37  top> #67x #66x )
 [ 0022 ] BRANCHNZ  ( 37  top> #0x )
 ( LOOP_END)      ( 60  top> empty )
 [ 0027 ] LABEL     ( 112  top> empty )
 4 U@            ( 64  top> empty )
 2  ( LIT)       ( 64  top> #67x )
 -                ( 64  top> #0x #67x )
 [ 4 ] DUP_U!         ( 26  top> #67 )
 3 U@            ( 26  top> #67x )
 U>               ( 27  top> #71x #67x )
 [ 0023 ] BRANCHNZ  ( 27  top> #0x )
 ( LOOP_END)      ( 70  top> empty )
 [ 0028 ] LABEL     ( 117  top> empty )
 3 U@            ( 74  top> empty )
 @                ( 74  top> #71x )
 littlest  ( SYMBOL)    ( 75  top> #80 )
 @                ( 75  top> #0x #80 )
 -                ( 77  top> #81x #80x )
 [ 0025 ] BRANCHNZ  ( 77  top> #0x )
 [                ( 79  top> empty )
 sortlist  ( SYMBOL)    ( 79  top> empty )
 1000  ( LIT)       ( 79  top> #0x )
 +                ( 79  top> #0x #0x )
 ] LITERAL        ( 79  top> #0x )
 @                ( 79  top> #0x )
 biggest  ( SYMBOL)    ( 80  top> #83 )
 @                ( 80  top> #0x #83 )
 -                ( 82  top> #84x #83x )
 [ 0016 ] BRANCHZ   ( 82  top> #0x )
 [ 0025 ] LABEL     ( 85  top> empty )
 -64 FP+!  ( Link)   ( 88  top> empty )
   do_error  ( CALL)    ( 88  top> empty )
 64 FP+!  ( Unlink)  ( 88  top> empty )
 [ 0016 ] LABEL     ( 93  top> empty )
 2 U@            ( 94  top> empty )
 1  ( LIT)       ( 94  top> #65x )
 +                ( 94  top> #0x #65x )
 [ 2 ] DUP_U!         ( 10  top> #65 )
 29  ( LIT)       ( 10  top> #65x )
 >                ( 11  top> #68x #65x )
 [ 0026 ] BRANCHZ   ( 11  top> #0x )
 ( TYPE 1 LOOP END)  ( 100  top> empty )
 [ 0029 ] LABEL     ( 122  top> empty )
   ;  ( END )   

.( max 29)
