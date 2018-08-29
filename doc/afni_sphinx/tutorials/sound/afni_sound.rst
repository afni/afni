.. _tut_sound:

==========================
**Sound features in AFNI**
==========================

.. contents:: :local:

Overview
--------

Sound features are available in AFNI (as of 29 Aug 2018; version >=
AFNI_18.2.15).

You can now play sounds from the AFNI GUI. AFNI *itself* does not
drive the audio output.  It just creates the sound file, and then
plays it through some external program.

The best program package I know for this sound-playing (and simple
editing) purpose is ``sox``, available for both Mac and Linux. To see
if this package is on your system, type the command::
  
  which sox

Other programs that AFNI can use to play sound are ``afplay`` (comes on
Macs), ``mplayer``, and ``aplay`` (Linux).

Keypresses, formats and functionality
-------------------------------------

From an AFNI graph viewer, pressing the "p" (for play) key will
generate sound from the central subgraph. (AFNI will not let you play
sound if your X11 connection is remote from the system on which AFNI
is running.)

If the graph is edited using a transform from the Opt menu, the
transformed data will be used to create the sound. If the first few
points of the graph are "ignored" (use the "I" keypress), those values
will not be used in the sound.

Pressing the "P" (upper case) key will generate sound from a
combination of the central subgraph and the average of all the graphs
in the viewer window. To see this average graph, select menu item
FIM->Edit Ideal->Ideal=WinAver

The sound is generated and launched to be played in the external
program. If you want to kill the sound before it finishes, press the
"K" key into the graph viewer.

Alas, there is (yet) no facility to see a bouncing ball that moves
across the graph data in synchrony with the sound.

New AFNI program ``1dsound`` can take a .1D file (columns of ASCII
numbers) and generate a sound file on the command line.

Sound files are in the very simple and old Sun ".au" format. The sox
program can be used to convert to a different format, merge multiple
files, add reverberation, etc.

Example
-------

Here is a sample script, just for fun (produces 10 minute long file
afni.au for your listening pleasure).

::

  1deval -num 3000 -expr 'lran(3)+(a-a)+0.5*z'                         > j1.1D
  1deval -num 3000 -expr 'abs(lran(5))*step(uran(1)-0.90)+(a-a)+0.9*z' > j2.1D
  1dsound -prefix j1.au j1.1D
  1dsound -prefix j2.au j2.1D
  1dplot -sepscl j?.1D &
  sox -m j?.au afni.au gain -15 reverb 99 reverb 99
  \rm j?.??


