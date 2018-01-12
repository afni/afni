.. contents:: 
    :depth: 4 

*********
3dproject
*********

.. code-block:: none

    Projection along cardinal axes from a 3D dataset
    Usage: 3dproject [editing options]
            [-sum|-max|-amax|-smax] [-output root] [-nsize] [-mirror]
            [-RL {all | x1 x2}] [-AP {all | y1 y2}] [-IS {all | z1 z2}]
            [-ALL] dataset
    
    Program to produce orthogonal projections from a 3D dataset.
      -sum     ==> Add the dataset voxels along the projection direction
      -max     ==> Take the maximum of the voxels [the default is -sum]
      -amax    ==> Take the absolute maximum of the voxels
      -smax    ==> Take the signed maximum of the voxels; for example,
                    -max  ==> -7 and 2 go to  2 as the projected value
                    -amax ==> -7 and 2 go to  7 as the projected value
                    -smax ==> -7 and 2 go to -7 as the projected value
      -first x ==> Take the first value greater than x
      -nsize   ==> Scale the output images up to 'normal' sizes
                   (e.g., 64x64, 128x128, or 256x256)
                   This option only applies to byte or short datasets.
      -mirror  ==> The radiologists' and AFNI convention is to display
                   axial and coronal images with the subject's left on
                   the right of the image; the use of this option will
                   mirror the axial and coronal projections so that
                   left is left and right is right.
    
      -output root ==> Output projections will named
                       root.sag, root.cor, and root.axi
                       [the default root is 'proj']
    
      -RL all      ==> Project in the Right-to-Left direction along
                       all the data (produces root.sag)
      -RL x1 x2    ==> Project in the Right-to-Left direction from
                       x-coordinate x1 to x2 (mm)
                       [negative x is Right, positive x is Left]
                       [OR, you may use something like -RL 10R 20L
                            to project from x=-10 mm to x=+20 mm  ]
    
      -AP all      ==> Project in the Anterior-to-Posterior direction along
                       all the data (produces root.cor)
      -AP y1 y2    ==> Project in the Anterior-to-Posterior direction from
                       y-coordinate y1 to y2 (mm)
                       [negative y is Anterior, positive y is Posterior]
                       [OR, you may use something like -AP 10A 20P
                            to project from y=-10 mm to y=+20 mm  ]
    
      -IS all      ==> Project in the Inferior-to-Superior direction along
                       all the data (produces root.axi)
      -IS y1 y2    ==> Project in the Inferior-to-Superior direction from
                       z-coordinate z1 to z2 (mm)
                       [negative z is Inferior, positive z is Superior]
                       [OR, you may use something like -IS 10I 20S
                            to project from z=-10 mm to z=+20 mm  ]
    
      -ALL         ==> Equivalent to '-RL all -AP all -IS all'
    
    * NOTE that a projection direction will not be used if the bounds aren't
       given for that direction; thus, at least one of -RL, -AP, or -IS must
       be used, or nothing will be computed!
    * NOTE that in the directions transverse to the projection direction,
       all the data is used; that is, '-RL -5 5' will produce a full sagittal
       image summed over a 10 mm slice, irrespective of the -IS or -AP extents.
    * NOTE that the [editing options] are the same as in 3dmerge.
       In particular, the '-1thtoin' option can be used to project the
       threshold data (if available).
    
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
