.. _ahelp_rotcom:

******
rotcom
******

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: rotcom '-rotate aaI bbR ccA -ashift ddS eeL ffP' [dataset]
    
    Prints to stdout the 4x3 transformation matrix+vector that would be
    applied by 3drotate to the given dataset.
    
    The -rotate and -ashift options combined must be input inside single
    quotes (i.e., as one long command string):
     * These options follow the same form as specified by '3drotate -help'.
     * That is, if you include the '-rotate' component, it must be followed
       by 3 angles.
     * If you include the '-ashift' component, it must be followed by 3 shifts;
     * For example, if you only want to shift in the 'I' direction, you could use
         '-ashift 10I 0 0'.
     * If you only want to rotate about the 'I' direction, you could use
         '-rotate 10I 0R 0A'.
    
    Note that the coordinate order for the matrix and vector is that of
    the dataset, which can be determined from program 3dinfo.  This is the
    only function of the 'dataset' command line argument.
    
    If no dataset is given, the coordinate order is 'RAI', which means:
        -x = Right      [and so +x = Left     ]
        -y = Anterior   [    so +y = Posterior]
        -z = Inferior   [    so +z = Superior ]
    For example, the output of command
       rotcom '-rotate 10I 0R 0A'
    is the 3 lines below:
    0.984808 -0.173648  0.000000  0.000
    0.173648  0.984808  0.000000  0.000
    0.000000  0.000000  1.000000  0.000
    
