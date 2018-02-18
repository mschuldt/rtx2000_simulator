@echo off
goto start

Batch file to install appforth on a pc
Rick VanNorman  Tue  05-01-1990

This installs from A: to C: -- change the drive constant if you wish

:start

echo A directory rtx2000 will be created in the root of drive C
echo and rtx2000 will be uncompressed from a .ZIP file into this
echo directory.  If you wish to install it on a drive other
echo than C: please edit the INSTALL.BAT file appropriately.

a:ticker Any key to continue, control-c to abort

c:
cd \
a:pkunzip -d -o a:rtx2000
cd rtx2000

echo Installation complete -- type TERM to enter the terminal environment


