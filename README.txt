RTX2000 Simulator

Build and run on Ubuntu:
 cd SIM2000
 make
 ./run


It appears this simulator contains support for the RTX2000, RTX2001A, and RTX2010.
To set the chip type set the which_chip variable in main.c before building.


This program has been ported to GNU/Linux. Files originally from:
  http://users.ece.cmu.edu/~koopman/forth/RTX2000_SIMULATOR.ZIP
  http://users.ece.cmu.edu/~koopman/forth/RTX2000_APPFORTH.ZIP


======= The text from SIM2000/README.1ST follows =============

                                                      10/24/97

I pulled this RTX 2000 simulator from an ancient floppy disk.
It was written for my personal use while I was at Harris, and
is not a commercial product by any stretch of the imagination.
I figure that Harris is long past caring about this, so I'm
giving it to people who need help using the RTX 2000 as an
after-the-fact technical support exercise.  However, this is
still copyrighted by Harris Semiconductor and should be treated
accordingly.

To run, just execute sim.bat, which will execute the simulator
while loading an image of Rick VanNorman's AppForth.  What you
get is an RTX 2000 simulation running a Forth compiler.  The
source code is all there for both the simulator and AppForth
(and probably some spare code that isn't used).  I recompiled
it with Borland Turbo C++ 4.0 and it worked, so it should be
complete (it got a heap of compiler warnings, but it did
compile -- I suppose I've learned a lot more about C programming
since I wrote the original code :-) )
Beyond that, you're on your own.

Notes:

- This is hacked-together C code.  I think it is complete except
for single-step functions not working (I never got to them).

- Harris Semiconductor does not officially endorse the release of
this simulator -- so don't hassle them if there is a problem
with it (you use it at your own risk and expense).  Similarly,
I offer no warrantees express or implied.

- If you have questions about the code -- you're on your own.
I don't have time to support this, even if I *did* remember
what I did all those years ago.

Cheers,

Phil Koopman
koopman@cmu.edu
