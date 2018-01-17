************
1dFlagMotion
************

.. _1dFlagMotion:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage: 1dFlagMotion [options] MotionParamsFile 
    
       Produces an list of time points that have more than a 
       user specified amount of motion relative to the previous 
       time point. 
     Options:
      -MaxTrans    maximum translation allowed in any direction 
                     [defaults to 1.5mm]
      -MaxRot      maximum rotation allowed in any direction 
                      [defaults to 1.25 degrees]
    
    ** The input file must have EXACTLY 6 columns of input, in the order:
         roll pitch yaw delta-SI delta-LR delta-AP
       (angles in degrees first, then translations in mm)
    
    ** The program does NOT accept column '[...]' selectors on the input
       file name, or comments in the file itself.  As a palliative, if the
       input file name is '-', then the input numbers are read from stdin,
       so you could do something like the following:
         1dcat mfile.1D'[1..6]' | 1dFlagMotion -
       e.g., to work with the output from 3dvolreg's '-dfile' option
       (where the first column is just the time index).
    
    ** The output is in a 1D format, with comments on '#' comment lines,
       and the list of points exceeding the motion bounds listed being
       intercalated on normal (non-comment) lines.
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
