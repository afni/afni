***********
3dRowFillin
***********

.. _3dRowFillin:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dRowFillin [options] dataset
    Extracts 1D rows in the given direction from a 3D dataset,
    searches for blank (zero) regions, and fills them in if
    the blank region isn't too large and it is flanked by
    the same value on either edge.  For example:
         input row = 0 1 2 0 0 2 3 0 3 0 0 4 0
        output row = 0 1 2 2 2 2 3 3 3 0 0 4 0
    
    OPTIONS:
     -maxgap N  = set the maximum length of a blank region that
                    will be filled in to 'N' [default=9].
     -dir D     = set the direction of fill to 'D', which can
                    be one of the following:
                      A-P, P-A, I-S, S-I, L-R, R-L, x, y, z, 
                      XYZ.OR, XYZ.AND
                    The first 6 are anatomical directions;
                    x,y, and z, are reference to the dataset
                    internal axes. 
                 XYZ.OR means do the fillin in x, followed by y,
                    followed by z directions.
                 XYZ.AND is like XYZ.OR but only accept voxels that
                    would have been filled in each of the three fill
                    calls. 
             Note that with XYZ* options, the fill value depends on
             on the axis orientation. So you're better off sticking
             to single valued dsets when using them. 
             See also -binary option below
     -binary: Turn input dataset to 0 and 1 before filling in.
              Output will also be a binary valued dataset.
     -prefix P  = set the prefix to 'P' for the output dataset.
    
    N.B.: If the input dataset has more than one sub-brick,
          only the first one will be processed.
    
    * The intention of this program is to let you fill in slice gaps
      made when drawing ROIs with the 'Draw Dataset' plugin.  If you
      draw every 5th coronal slice, say, then you could fill in using
        3dRowFillin -maxgap 4 -dir A-P -prefix fredfill fred+orig
    * This program is moderately obsolescent, since I later added
        the 'Linear Fillin' controls to the 'Draw Dataset' plugin.
    
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
