.. _ahelp_3dDTeig:

*******
3dDTeig
*******

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dDTeig [options] dataset
    Computes eigenvalues and eigenvectors for an input dataset of
     6 sub-bricks Dxx,Dxy,Dyy,Dxz,Dyz,Dzz (lower diagonal order).
     The results are stored in a 14-subbrick bucket dataset.
     The resulting 14-subbricks are
      lambda_1,lambda_2,lambda_3,
      eigvec_1[1-3],eigvec_2[1-3],eigvec_3[1-3],
      FA,MD.
    
    The output is a bucket dataset.  The input dataset
    may use a sub-brick selection list, as in program 3dcalc.
     Options:
      -prefix pname = Use 'pname' for the output dataset prefix name.
        [default='eig']
    
      -datum type = Coerce the output data to be stored as the given type
        which may be byte, short or float. [default=float]
    
      -sep_dsets = save eigenvalues,vectors,FA,MD in separate datasets
    
      -uddata = tensor data is stored as upper diagonal 
                instead of lower diagonal
    
     Mean diffusivity (MD) calculated as simple average of eigenvalues.
     Fractional Anisotropy (FA) calculated according to Pierpaoli C, Basser PJ.
     Microstructural and physiological features of tissues elucidated by
     quantitative-diffusion tensor MRI, J Magn Reson B 1996; 111:209-19
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
    For the gruesome details, see the output of 'afni -help'.
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
