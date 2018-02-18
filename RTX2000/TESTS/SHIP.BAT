@echo off
goto start

This file builds the shippable disk for APPFORTH on drive a: from the
directory RTX2000 on the current drive.

Rick VanNorman  Tue  05-01-1990

:start
cd \rtx2000
del *.bak
cd \
del rtx2000.zip
pkzip -rP rtx2000 rtx2000\*.*

a:
echo y | del *.*
c:

@echo on

copy rtx2000.zip a:
copy rtx2000\install.bat a:
copy c:\bin\pkunzip.exe a:
copy c:\bin\ticker.com a:

@echo .
@echo Ship completed.


