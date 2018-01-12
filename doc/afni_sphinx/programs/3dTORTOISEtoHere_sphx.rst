.. contents:: 
    :depth: 4 

****************
3dTORTOISEtoHere
****************

.. code-block:: none

    
      Convert standard TORTOISE DTs (diagonal-first format) to standard
      AFNI (lower triangular, row-wise) format.  NB: Starting from
      TORTOISE v2.0.1, there is an 'AFNI output' format as well, which
      would not need to be converted.
    
      Part of FATCAT (Taylor & Saad, 2013) in AFNI.
    
      *** NB: this program is likely no longer necessary if using 'AFNI
      ***     export' from TORTOISE!
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + COMMAND: 3dTORTOISEtoHere -dt_tort DTFILE  {-scale_fac X }   \
               {-flip_x | -flip_y | -flip_z}  -prefix PREFIX 
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + OUTPUT:
         1) An AFNI-style DT file with the following ordering of the 6 bricks:
              Dxx,Dxy,Dyy,Dxz,Dyz,Dzz.
            In case it is useful, one can apply 'flips' to the eventual (or
            underlying, depending how you look at it) eigenvector directions,
            as well as rescale the associated eigenvalues.
    
      + RUNNING:
        -dt_tort DTFILE  :diffusion tensor file, which should have six bricks
                          of DT components ordered in the TORTOISE manner, i.e.,
                          diagonals first:
                          Dxx,Dyy,Dzz,Dxy,Dxz,Dyz.
        -prefix PREFIX   :output file name prefix. Will have N+1 bricks when
                          GRADFILE has N rows of gradients.
        -flip_x          :change sign of first element of (inner) eigenvectors.
        -flip_y          :change sign of second element of (inner) eigenvectors.
        -flip_z          :change sign of third element of (inner) eigenvectors.
                  -> Only a single flip would ever be necessary; the combination
                     of any two flips is mathematically equivalent to the sole
                     application of the remaining one.
                     Normally, it is the *gradients* that are flipped, not the
                     DT, but if, for example, necessary files are missing, then
                     one can apply the requisite changes here.
        -scale_fac  X    :optional switch to rescale the DT elements, dividing
                          by a number X>0.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + EXAMPLE:
          3dTORTOISEtoHere          \
          -dt_tort DTI/DT_DT+orig   \
          -scale_fac 1000           \
          -prefix AFNI_DT   
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      If you use this program, please reference the introductory/description
      paper for the FATCAT toolbox:
            Taylor PA, Saad ZS (2013).  FATCAT: (An Efficient) Functional
            And Tractographic Connectivity Analysis Toolbox. Brain 
            Connectivity 3(5):523-535.
