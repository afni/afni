.. contents:: 
    :depth: 4 

**********
3ddot_beta
**********

.. code-block:: none

    
      Beta version of updating 3ddot.  Right now, *only* doing eta2 tests,
      and only outputting a full matrix to a text file.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + COMMAND: 3ddot_beta  -input FILE  -doeta2    \
               {-mask MASK } -prefix PREFIX 
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + OUTPUT:
         1) A single text file with the correlation-like matrix.  If the input
            data set has N bricks, then the matrix will be NxN.
    
      + RUNNING:
        -input FILE      :file with N bricks.
        -prefix PREFIX   :output test file will be called PREFIX_eta2.dat.
    
        -doeta2          :right now, required switch (more tests might be
                          present in the future, if demand calls for it).
    
        -mask   MASK     :can include a mask within which to take values.
                          Otherwise, data should be masked already.
    
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + EXAMPLE:
        3ddot_beta                    \
          -input  RSFC_MAPS_cat+orig  \
          -mask   mask.nii.gz         \
          -doeta2                     \
          -prefix My_Matrix_File   
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
