: this ;

variable #shortlit
variable #lit
variable #litalu
variable #call
variable #0<
variable #0>
variable #0_PICK
variable #1_PICK
variable #1_PUT
variable #2*
variable #2_PICK
variable #3_pick
variable #3_PUT
variable #4_pick
variable #4_PUT
variable #5_pick
variable #6_pick
variable #7_PICK
variable #8_pick
variable #9_pick
variable #!
variable #*
variable #+
variable #+!
variable #-
variable #/
variable #<
variable #<=
variable #>
variable #>=
variable #>R
variable #@
variable #@_SWAP
variable #AND
variable #BRANCH
variable #BRANCHNZ
variable #BRANCHZ
variable #C!
variable #C@
variable #DROP
variable #DUP
variable #DUP_>R
variable #DUP_U!
variable #EXIT
variable #FP+!
variable #INDEX
variable #LITERAL
variable #LIT_SWAP
variable #MEM_ARG!
variable #MEM_ARG@
variable #NIP
variable #OVER
variable #OVER_+
variable #R>
variable #R>DROP
variable #ROT
variable #SWAP
variable #SWAP-
variable #SYMBOL_+
variable #TUCK_!
variable #U!
variable #U>
variable #U@
variable #UBR@



: doit

0. #shortlit D!
0. #lit D!
0. #litalu D!
0. #call D!
0. #0< D!
0. #0> D!
0. #0_PICK D!
0. #1_PICK D!
0. #1_PUT D!
0. #2* D!
0. #2_PICK D!
0. #3_pick D!
0. #3_PUT D!
0. #4_pick D!
0. #4_PUT D!
0. #5_pick D!
0. #6_pick D!
0. #7_PICK D!
0. #8_pick D!
0. #9_pick D!
0. #! D!
0. #* D!
0. #+ D!
0. #+! D!
0. #- D!
0. #/ D!
0. #< D!
0. #<= D!
0. #> D!
0. #>= D!
0. #>R D!
0. #@ D!
0. #@_SWAP D!
0. #AND D!
0. #BRANCH D!
0. #BRANCHNZ D!
0. #BRANCHZ D!
0. #C! D!
0. #C@ D!
0. #DROP D!
0. #DUP D!
0. #DUP_>R D!
0. #DUP_U! D!
0. #EXIT D!
0. #FP+! D!
0. #INDEX D!
0. #LITERAL D!
0. #LIT_SWAP D!
0. #MEM_ARG! D!
0. #MEM_ARG@ D!
0. #NIP D!
0. #OVER D!
0. #OVER_+ D!
0. #R> D!
0. #R>DROP D!
0. #ROT D!
0. #SWAP D!
0. #SWAP- D!
0. #SYMBOL_+ D!
0. #TUCK_! D!
0. #U! D!
0. #U> D!
0. #U@ D!
0. #UBR@ D!


" main" evaluate




  ."   shortlit= " #shortlit D@ DDUP D.
  ."   lit= " #lit D@ DDUP D.  D+
  ."   call= " #call D@ DDUP D.  D+
  ."   0<= " #0< D@ DDUP D.  D+
  ."   0>= " #0> D@ DDUP D.  D+ CR
  ."   0_PICK= " #0_PICK D@ DDUP D.  D+
  ."   1_PICK= " #1_PICK D@ DDUP D.  D+
  ."   1_PUT= " #1_PUT D@ DDUP D.  D+
  ."   2*= " #2* D@ DDUP D.  D+ CR
  ."   2_PICK= " #2_PICK D@ DDUP D.  D+
  ."   3_pick= " #3_pick D@ DDUP D.  D+
  ."   3_PUT= " #3_PUT D@ DDUP D.  D+
  ."   4_pick= " #4_pick D@ DDUP D.  D+ CR
  ."   4_PUT= " #4_PUT D@ DDUP D.  D+
  ."   5_pick= " #5_pick D@ DDUP D.  D+
  ."   6_pick= " #6_pick D@ DDUP D.  D+
  ."   7_PICK= " #7_PICK D@ DDUP D.  D+ CR
  ."   8_pick= " #8_pick D@ DDUP D.  D+
  ."   9_pick= " #9_pick D@ DDUP D.  D+
  ."   != " #! D@ DDUP D.  D+
  ."   *= " #* D@ DDUP D.  D+ CR
  ."   += " #+ D@ DDUP D.  D+
  ."   +!= " #+! D@ DDUP D.  D+
  ."   -= " #- D@ DDUP D.  D+
  ."   /= " #/ D@ DDUP D.  D+ CR
  ."   <= " #< D@ DDUP D.  D+
  ."   <== " #<= D@ DDUP D.  D+
  ."   >= " #> D@ DDUP D.  D+
  ."   >== " #>= D@ DDUP D.  D+ CR
  ."   >R= " #>R D@ DDUP D.  D+
  ."   @= " #@ D@ DDUP D.  D+
  ."   @_SWAP= " #@_SWAP D@ DDUP D.  D+
  ."   AND= " #AND D@ DDUP D.  D+ CR
  ."   BRANCH= " #BRANCH D@ DDUP D.  D+
  ."   BRANCHNZ= " #BRANCHNZ D@ DDUP D.  D+
  ."   BRANCHZ= " #BRANCHZ D@ DDUP D.  D+
  ."   C!= " #C! D@ DDUP D.  D+ CR
  ."   C@= " #C@ D@ DDUP D.  D+
  ."   DROP= " #DROP D@ DDUP D.  D+
  ."   DUP= " #DUP D@ DDUP D.  D+
  ."   DUP_>R= " #DUP_>R D@ DDUP D.  D+ CR
  ."   DUP_U!= " #DUP_U! D@ DDUP D.  D+
  ."   EXIT= " #EXIT D@ DDUP D.  D+
  ."   FP+!= " #FP+! D@ DDUP D.  D+
  ."   INDEX= " #INDEX D@ DDUP D.  D+ CR
  ."   LITERAL= " #LITERAL D@ DDUP D.  D+
  ."   LIT_SWAP= " #LIT_SWAP D@ DDUP D.  D+
  ."   MEM_ARG!= " #MEM_ARG! D@ DDUP D.  D+
  ."   MEM_ARG@= " #MEM_ARG@ D@ DDUP D.  D+ CR
  ."   NIP= " #NIP D@ DDUP D.  D+
  ."   OVER= " #OVER D@ DDUP D.  D+
  ."   OVER_+= " #OVER_+ D@ DDUP D.  D+
  ."   R>= " #R> D@ DDUP D.  D+ CR
  ."   R>DROP= " #R>DROP D@ DDUP D.  D+
  ."   ROT= " #ROT D@ DDUP D.  D+
  ."   SWAP= " #SWAP D@ DDUP D.  D+
  ."   SWAP-= " #SWAP- D@ DDUP D.  D+ CR
  ."   SYMBOL_+= " #SYMBOL_+ D@ DDUP D.  D+
  ."   TUCK_!= " #TUCK_! D@ DDUP D.  D+
  ."   U!= " #U! D@ DDUP D.  D+
  ."   U>= " #U> D@ DDUP D.  D+ CR
  ."   U@= " #U@ D@ DDUP D.  D+
  ."   UBR@= " #UBR@ D@ DDUP D.  D+  CR
CR   ."   litalu= " #litalu D@ DDUP D. DNEGATE D+
cr  ." total instructions = " D. CR
 ;



 : #inc  DUP >R  D@ 1. D+ R> D! ;
 : #dec  DUP >R  D@ -1. D+ R> D! ;



 : shortlit #shortlit #inc   ;
 : lit #lit #inc    ;
 : litalu #litalu #inc    ;
 : call #call #inc   ;



 : 0< #0< #inc  0<  ;
 : 0> #0> #inc  0>  ;
 : 0_PICK #0_PICK #inc  0_PICK  ;
 : 1_PICK #1_PICK #inc  1_PICK  ;
 : 1_PUT #1_PUT #inc  1_PUT  ;
 : 2* #2* #inc  2*  ;
 : 2_PICK #2_PICK #inc  2_PICK  ;
 : 3_pick #3_pick #inc  3_pick  ;
 : 3_PUT #3_PUT #inc  3_PUT  ;
 : 4_pick #4_pick #inc  4_pick  ;
 : 4_PUT #4_PUT #inc  4_PUT  ;
 : 5_pick #5_pick #inc  5_pick  ;
 : 6_pick #6_pick #inc  6_pick  ;
 : 7_PICK #7_PICK #inc  7_PICK  ;
 : 8_pick #8_pick #inc  8_pick  ;
 : 9_pick #9_pick #inc  9_pick  ;
 : ! #! #inc  !  ;
 : * #* #inc  *  ;
 : + #+ #inc  +  ;
 : +! #+! #inc  +!  ;
 : - #- #inc  -  ;
 : / #/ #inc  /  ;
 : < #< #inc  <  ;
 : <= #<= #inc  <=  ;
 : > #> #inc  >  ;
 : >= #>= #inc  >=  ;
 : @ #@ #inc  @  ;
 : @_SWAP #@_SWAP #inc  @_SWAP  ;
 : AND #AND #inc  AND  ;
 : C! #C! #inc  C!  ;
 : C@ #C@ #inc  C@  ;
 : DROP #DROP #inc  DROP  ;
 : DUP #DUP #inc  DUP  ;
 : NIP #NIP #inc  NIP  ;
 : OVER #OVER #inc  OVER  ;
 : OVER_+ #OVER_+ #inc  OVER_+  ;
 : ROT #ROT #inc  ROT  ;
 : SWAP #SWAP #inc  SWAP  ;
 : SWAP- #SWAP- #inc  SWAP-  ;
 : TUCK_! #TUCK_! #inc  TUCK_!  ;
 : U> #U> #inc  U>  ;
 : UBR@ #UBR@ #inc  UBR@  ;

 : xDUP_>R " #DUP_>R #inc  DUP_>R " evaluate ; immediate
 : x>R     " #>R #inc  >R " evaluate  ; immediate
 : xEXIT   " #EXIT #inc  EXIT " evaluate ; immediate
 : xR>     " #R> #inc  R> " evaluate ; immediate
 : xR>DROP " #R>DROP #inc  R>DROP " evaluate ; immediate


 : xU!  "  U! #U! #inc " evaluate  ; immediate
 : xU@  "  U@ #U@ #inc " evaluate  ; immediate
 : xINDEX  " INDEX #INDEX #inc " evaluate    ; immediate
 : xLIT_SWAP  " LIT_SWAP #LIT_SWAP #inc " evaluate    ; immediate
 : xMEM_ARG!  " MEM_ARG! #MEM_ARG! #inc " evaluate    ; immediate
 : xMEM_ARG@  " MEM_ARG@ #MEM_ARG@ #inc " evaluate    ; immediate
 : xBRANCH  "  BRANCH #BRANCH #inc " evaluate    ; immediate
 : xBRANCHZ  " BRANCHZ #BRANCHZ #inc " evaluate    ; immediate
 : xBRANCHNZ  " BRANCHNZ #BRANCHNZ #inc " evaluate    ; immediate
 : xSYMBOL_+  " SYMBOL_+ #SYMBOL_+ #inc " evaluate   ; immediate
 : xLITERAL  " LITERAL #LITERAL #inc " evaluate   ; immediate
 : xFP+! " FP+! #FP+! #inc " evaluate   ; immediate
 : xDUP_U!  " DUP_U! #DUP_U! #inc " evaluate    ; immediate
