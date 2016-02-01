
.. _U_misc_bg0:

**********************************
What is ... ?  Unix, tcsh and AFNI
**********************************
A brief overview of Unix, tcsh and AFNI

   Descriptions, examples, comments, sample commands, syntax and getting help...

.. _U_mb0_unix:

What is Unix?
=============
Unix is a type of operating system (a standard), first developed in 1969

Examples of Unix operating systems:

   Solaris, OpenSolaris, Irix, AIX, HP-UX, OS X, Linux, FreeBSD
   (actually, Linux and FreeBSD are not formally Unix, but are very similar)

Comments:

   - has graphical environment, but strength is in command-line capabilities
   - hundreds or more usually thousands of programs come with systems
     (not just a handful that have screen icons)

Sample commands
---------------

A Unix system often has thousands of programs, including::

   ls, cat, less, mv, cp, date, ssh, vi, rm

Syntax
------

Many characters special meanings on a Unix system, depending on the shell::

   variables ($), quotes (', ", `), wildcards (\*, ?, []), pipes (|), redirection (>)

Getting help
------------

There are many places to get help...

   - 'man' is short for manual, the on-line manual for unix commands::

         man ls
         man less
         man man

   - 'info' is a newer help system

   - books, having one that also covers shell programming can be helpful
     (in our case, the focus is on tcsh rather than bash)

   - our on-line tutorials

   - examples with the class data

   - our message board

   - asking neighbors

.. _U_mb0_tcsh:

What is tcsh?
=============
T-shell is a Unix shell: a command-line interpreter

   When the user types a command and hits <Enter>, the shell
   processes that command and decides what to do:

         - processes special characters
         - decides what program to run, if any
         - runs the program, passing along any options and parameters

Examples of similar shell programs::

   sh, bash, csh, tcsh, ksh, zsh

.. note:: regarding :command:`tcsh`

   - it is just one of many common Unix programs
   - the actual program (a file on disk) is generally /bin/tcsh
   - it has its own syntax
   - it has its own sub-commands (cd, echo, set, ...)
   - it is not has powerful as bash, but is more simple and readable

Sample commands
---------------

tcsh has many sub-commands: commands that do not exist on the system,
but are just part of the shell, e.g.::

   cd, echo, set, setenv, alias, foreach, while, bg, exit, ...

For example, 'cd' is not a program, it just tells the shell that you want
to be "sitting in a new location".

Syntax (characters that mean something special)::

   home directories (~), history (!), jobs (%), redirecting stderr (&gt;&amp;)

Getting help
-------------
Since 'tcsh' is just a Unix program, "man tcsh" is one way to get help.::

         man tcsh

Again, consider getting a book that covers tcsh.


.. _U_mb0_afni:

What is AFNI?
=============

   - AFNI is a suite of data analysis and viewing tools
   - well over 500 programs, scripts and plugins

Examples of similar packages (there are many):

   - AFNI, FSL, SPM, BrainVoyager

.. note:: regarding **AFNI**

   - it is good for viewing (and hopefully understanding) data
   - it is written on top of X11 and for Unix systems
   - it does not work directly on Windows (but there are options)
   - it is free (costs nothing)
   - it is open source (one can see and modify the code)

Sample commands
---------------

   afni, suma, 3dcalc, afni_proc.py, 3dDeconvolve

   Syntax (characters that mean something special)::

      - sub-brick selection ([$,..]), range (<>), index ({}), transposition (')
      - many programs have their own special syntax

   .. note:: The special syntax characters overlap with those of the shell,
             meaning they need to be hidden from the shell if they are to be
             passed on to an AFNI program.

Getting help
------------

   Virtually every AFNI program provides help using the -help option, as in
   these examples::

      afni_proc.py -help
      afni_proc.py -help | less
      afni_proc.py -h_view


   - this output is also available at: `help: all AFNI programs <http://afni.nimh.nih.gov/pub/dist/doc/program_help>`_
   - course material is available at: `help: course handouts <http://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts>`_
   - class data is available at: `AFNI data packages <http://afni.nimh.nih.gov/pub/dist/edu/data>`_
