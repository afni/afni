.. _ahelp_3dEigsToDT:

**********
3dEigsToDT
**********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
      Convert set of DTI eigenvectors and eigenvalues to a diffusion tensor,
      while also allowing for some potentially useful value-scaling and vector-
      flipping.
    
      May be helpful in converting output from different software packages.
      Part of FATCAT (Taylor & Saad, 2013) in AFNI.
      It is essentially the inverse of the existing AFNI command: 3dDTeig.
    
      Minor note and caveat:
      This program has been checked for consistency with 3dDWItoDT outputs (that
      is using its output eigenvalues and eigenvectors to estimate a DT, which
      was then compared with that of the original 3dDWItoDT fit).
      This program will *mostly* return the same DTs that one would get from
      using the eigenvalues and eigenvectors of 3dDWItoDT to very high agreement
      The values generally match to <10**-5 or so, except in CSF where there can
      be small/medium differences, apparently due to the noisiness or non-
      tensor-fittability of the original DWI data in those voxels.
      However, these discrepancies *shouldn't* really affect most cases of using
      DTI data.  This is probably generally true for reconstructing DTs of most
      software program output:  the results match well for most WM and GM, but
      there might be trouble in partial-volumed and CSF regions, where the DT
      model likely did not fit well anyways.  Caveat emptor.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + COMMAND: 3dEigsToDT -eig_vals NAME1 -eig_vecs NAME2 {-mask MASK } \
               {-flip_x | -flip_y | flip_z} {-scale_eigs X} -prefix PREFIX 
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + OUTPUT:
         1) AFNI-format DT file with 6 subbricks in the same format as output
              by, for example, 3dDWItoDT (the lower triangular, row-wise
              elements of the tensor in symmetric matrix form)
                [0] Dxx
                [1] Dxy
                [2] Dyy
                [3] Dxz
                [4] Dyz
                [5] Dzz
    
      + RUNNING:
        -eig_vals NAME1  :Should be a searchable descriptor for finding all
                          three required eigenvalue files.  Thus, on a Linux
                          commandline, one would expect:
                          $ ls NAME1
                          to list all three eigenvalue files in descending order
                          of magnitude. This program will also only take
                          the first three matches (not including doubling of
                          BRIK/HEAD files in AFNI-format).
        -eig_vecs NAME2  :Should be a searchable descriptor for finding all
                          three required eigenvector files.  Thus, on a Linux
                          commandline, one would expect:
                          $ ls NAME2
                          to list all three eigenvector files in order matching
                          the eigenvalue files. This program will also only take
                          the first three matches (not including doubling of
                          BRIK/HEAD files in AFNI-format).
                  -> Try to make NAME1 and NAME2 as specific as possible, so
                     that the search&load gets everything as right as possible.
                     Also, if using the wildcard character, '*', then make sure
                     to enclose the option value with apostrophes (see EXAMPLE,
                     below).
        -prefix PREFIX   :output file name prefix. Would suggest putting a 'DT'
                          label in it.
        -mask   MASK     :can include a mask within which to calculate uncert.
                          Otherwise, data should be masked already.
    
        -flip_x          :change sign of first element of eigenvectors.
        -flip_y          :change sign of second element of eigenvectors.
        -flip_z          :change sign of third element of eigenvectors.
                  -> Only a single flip would ever be necessary; the combination
                     of any two flips is mathematically equivalent to the sole
                     application of the remaining one.
        -scale_eigs  X   :rescale the eigenvalues, dividing by a number that is
                          X>0. Could be used to reintroduce the DW scale of the
                          original b-values, if some other program has
                          remorselessly scaled it away.
    
    * * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * **
    
      + EXAMPLE:
          3dEigsToDT            \
          -eig_vals 'DTI/DT_L*'   \
          -eig_vecs 'DTI/DT_V*'   \
          -prefix DTI/NEW_DT    \
          -scale_eigs 1000      \
          -flip_y
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      If you use this program, please reference the introductory/description
        paper for the FATCAT toolbox:
        Taylor PA, Saad ZS (2013). FATCAT: (An Efficient) Functional And
        Tractographic Connectivity Analysis Toolbox. Brain Connectivity.
