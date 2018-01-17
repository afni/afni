**********
3daxialize
**********

.. _3daxialize:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage: 3daxialize [options] dataset
    Purpose: Read in a dataset and write it out as a new dataset
             with the data brick oriented as axial slices.
             The input dataset must have a .BRIK file.
             One application is to create a dataset that can
             be used with the AFNI volume rendering plugin.
    
    Options:
     -prefix ppp  = Use 'ppp' as the prefix for the new dataset.
                   [default = 'axialize']
     -verb        = Print out a progress report.
    
    The following options determine the order/orientation
    in which the slices will be written to the dataset:
     -sagittal    = Do sagittal slice order [-orient ASL]
     -coronal     = Do coronal slice order  [-orient RSA]
     -axial       = Do axial slice order    [-orient RAI]
                     This is the default AFNI axial order, and
                     is the one currently required by the
                     volume rendering plugin; this is also
                     the default orientation output by this
                     program (hence the program's name).
    
     -orient code = Orientation code for output.
                    The code must be 3 letters, one each from the
                    pairs {R,L} {A,P} {I,S}.  The first letter gives
                    the orientation of the x-axis, the second the
                    orientation of the y-axis, the third the z-axis:
                     R = Right-to-left         L = Left-to-right
                     A = Anterior-to-posterior P = Posterior-to-anterior
                     I = Inferior-to-superior  S = Superior-to-inferior
                    If you give an illegal code (e.g., 'LPR'), then
                    the program will print a message and stop.
              N.B.: 'Neurological order' is -orient LPI
     -frugal      = Write out data as it is rotated, a sub-brick at
                    a time. This saves a little memory and was the
                    previous behavior.
                    Note the frugal option is not available with NIFTI
                    datasets
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
    For the gruesome details, see the output of 'afni -help'.
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
