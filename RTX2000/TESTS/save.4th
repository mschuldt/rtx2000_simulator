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

