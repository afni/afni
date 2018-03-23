.. _ahelp_3dWinsor:

********
3dWinsor
********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dWinsor [options] dataset
    Apply a 3D 'Winsorizing' filter to a short-valued dataset.
    
    Options:
     -irad rr   = include all points within 'distance'
                    rr in the operation, where distance
                    is defined as sqrt(i*i+j*j+k*k), and
                    (i,j,k) are voxel index offsets
                    [default rr=1.5]
    
     -cbot bb   = set bottom clip index to bb
                    [default = 20% of the number of points]
     -ctop tt   = set top clip index to tt
                    [default = 80% of the number of points]
    
     -nrep nn   = repeat filter nn times [default nn=1]
                    if nn < 0, means to repeat filter until
                    less than abs(n) voxels change
    
     -keepzero  = don't filter voxels that are zero
     -clip xx   = set voxels at or below 'xx' to zero
    
     -prefix pp = use 'pp' as the prefix for the output
                    dataset [default pp='winsor']
    
     -mask mmm  = use 'mmm' as a mask dataset - voxels NOT
                    in the mask won't be filtered
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
