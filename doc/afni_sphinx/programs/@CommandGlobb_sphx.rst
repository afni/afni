*************
@CommandGlobb
*************

.. _@CommandGlobb:

.. contents:: 
    :depth: 4 

.. code-block:: none

    \012Usage: @CommandGlobb -com <Program Command line> -session <Output Dir> -newxt <extension> -list <Brick 1> <Brick 2> ...
    \012<Program Command line> : The entire command line for the program desired
    The command is best put between single quotes, do not use the \ to break a long line within the quotes
    <Brik*> : a list of bricks (or anything)
    <extension> : if the program requires a -prefix option, then you can specify the extension
     which will get appended to the Brick names before +orig
    <Output Dir> : The output directory 
    \012example
    @CommandGlobb -com '3dinfo -v' -list *.HEAD
    will execute 3dinfo -v on each of the A*.HEAD headers
    \012@CommandGlobb -com '3dZeropad -z 4' -newxt _zpd4 -list ADzst*vr+orig.BRIK
    will run 3dZeropad with the -z 4 option on all the bricks ADzst*vr+orig.BRIK
