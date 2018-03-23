.. _ahelp_3dDWItoDT:

*********
3dDWItoDT
*********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
     Usage: 3dDWItoDT [options] gradient-file dataset
    
     Computes 6 principle direction tensors from multiple gradient vectors
     and corresponding DTI image volumes.
     The program takes two parameters as input :  
        a 1D file of the gradient vectors with lines of ASCII floats:
                Gxi, Gyi, Gzi.
        Only the non-zero gradient vectors are included in this file (no G0 
        line). 
        ** Now, a '1D' file of b-matrix elements can alternatively be input,
            and *all* the gradient values are included!**
        A 3D bucket dataset with Np+1 sub-briks where the first sub-brik is the
        volume acquired with no diffusion weighting.
    
     OUTPUTS: 
         + you can output all 6 of the independent tensor values (Dxx, Dyy, 
           etc.), as well as all three eigenvalues (L1, L2, L3) and 
           eigenvectors (V1, V2, V3), and useful DTI parameters FA, MD and
           RD.
         + 'Debugging bricks' can also be output, see below.
    
     Options:
       -prefix pname = Use 'pname' for the output dataset prefix name.
                       [default='DT']
    
       -automask = mask dataset so that the tensors are computed only for
                   high-intensity (presumably brain) voxels.  The intensity 
                   level is determined the same way that 3dClipLevel works.
    
       -mask dset = use dset as mask to include/exclude voxels
    
     -bmatrix_NZ FF = switch to note that the input dataset is b-matrix, 
                   not gradient directions, and there is *no* row of zeros 
                   at the top of the file, similar to the format for the grad
                   input: N-1 rows in this file for N vols in matched data set.
                   There must be 6 columns of data, representing either elements
                   of G_{ij} = g_i*g_j (i.e., dyad of gradients, without b-value
                   included) or of the DW scaled version, B_{ij} = b*g_i*g_j.
                   The order of components is: G_xx G_yy G_zz G_xy G_xz G_yz.
     -bmatrix_Z FF = similar to '-bmatrix_NZ' above, but assumes that first
                   row of the file is all zeros (or whatever the b-value for
                   the reference volume was!), i.e. there are N rows to the
                   text file and N volumes in the matched data set.
    
     -bmatrix_FULL FF = exact same as '-bmatrix_Z FF' above (i.e. there are N
                   rows to the text file and N volumes in the matched data set)
                   with just a lot more commonsensical name.  Definitely would
                   be preferred way to go, for ease of usage!
    
       -scale_out_1000 = increase output parameters that have physical units
                   (DT, MD, RD, L1, L2 and L3) by multiplying them by 1000. This
                   might be convenient, as the input bmatrix/gradient values 
                   can have their physical magnitudes of ~1000 s/mm^2, for
                   which typical adult WM has diffusion values of MD~0.0007
                   (in physical units of mm^2/s), and people might not like so
                   many decimal points output; using this option rescales the
                   input b-values and would lead to having a typical MD~0.7
                   (now in units of x10^{-3} mm^2/s).  If you are not using
                   bmatrix/gradient values that have their physical scalings,
                   then using this switch probably wouldn't make much sense.
                   FA, V1, V2 and V3 are unchanged.
    
     -bmax_ref THR = if the 'reference' bvalue is actually >0, you can flag
                     that here.  Otherwise, it is assumed to be zero.
                     At present, this is probably only useful/meaningful if
                     using the '-bmatrix_Z ...' or '-bmatrix_FULL ...' 
                     option, where the reference bvalue must be found and 
                     identified from the input info alone.
    
       -nonlinear = compute iterative solution to avoid negative eigenvalues.
                    This is the default method.
    
       -linear = compute simple linear solution.
    
       -reweight = recompute weight factors at end of iterations and restart
    
       -max_iter n = maximum number of iterations for convergence (Default=10).
                     Values can range from -1 to any positive integer less than
                     101. A value of -1 is equivalent to the linear solution.
                     A value of 0 results in only the initial estimate of the
                     diffusion tensor solution adjusted to avoid negative
                     eigenvalues.
    
       -max_iter_rw n = max number of iterations after reweighting (Default=5)
                        values can range from 1 to any positive integer less
                        than 101.
    
       -eigs = compute eigenvalues, eigenvectors, fractional anisotropy and mean
               diffusivity in sub-briks 6-19. Computed as in 3dDTeig
    
       -debug_briks = add sub-briks with Ed (error functional), Ed0 (orig.
                      error), number of steps to convergence and I0 (modeled B0
                      volume).
                      [May, 2017] This also now calculates two goodness-of-fit
                      measures and outputs a new PREFIX_CHI* dset that has two
                      briks:
                         brik [0]: chi^2_p,
                         brik [1]: chi^2_c.
                      These values are essentially calculated according to
                      Papadakis et al. (2003, JMRI), Eqs. 4 and 3,
                      respectively (in chi^2_c, the sigma value is the
                      variance of measured DWIs *per voxel*). Note for both
                      chi* values, only DWI signal values are used in the
                      calculation (i.e., where b>THR; by default,
                      THR=0.01, which can be changed using '-bmax_ref ...').
                      In general, chi^2_p values seem to be <<1, consistent
                      with Papadakis et al.'s Fig. 4; the chi^2_c values are
                      are also pretty consistent with the same fig and seem to
                      be best viewed with the upper limit being roughly =Ndwi
                      or =Ndwi-7 (with the latter being the given degrees
                      of freedom value by Papadakis et al.)
       -cumulative_wts = show overall weight factors for each gradient level
                         May be useful as a quality control
    
       -verbose nnnnn = print convergence steps every nnnnn voxels that survive
                        to convergence loops (can be quite lengthy).
    
       -drive_afni nnnnn = show convergence graphs every nnnnn voxels that
                           survive to convergence loops. AFNI must have NIML
                           communications on (afni -niml)
    
       -sep_dsets = save tensor, eigenvalues, vectors, FA, MD in separate
                    datasets
    
       -csf_val n.nnn = assign diffusivity value to DWI data where the mean
                        values for b=0 volumes is less than the mean of the
                        remaining volumes at each voxel. The default value is
                        '1.0 divided by the max bvalue in the grads/bmatrices'.
                        The assumption is that there are flow artifacts in CSF
                        and blood vessels that give rise to lower b=0 voxels.
                        NB: MD, RD L1, L2, L3, Dxx, Dyy, etc. values are all
                        scaled in the same way.
    
       -min_bad_md N  = change the min MD value used as a 'badness check' for
                        tensor fits that have veeery (-> unreasonably) large MD
                        values. Voxels where MD > N*(csf_val) will be treated
                        like CSF and turned into spheres with radius csf_val 
                        (default N=100).
       -csf_fa n.nnn  = assign a specific FA value to those voxels described
                        above The default is 0.012345678 for use in tractography
                        programs that may make special use of these voxels
    
       -opt mname =  if mname is 'powell', use Powell's 2004 method for 
                     optimization. If mname is 'gradient' use gradient descent
                     method. If mname is 'hybrid', use combination of methods.
                     MJD Powell, "The NEWUOA software for unconstrained 
                     optimization without derivatives", Technical report DAMTP
                     2004/NA08, Cambridge University Numerical Analysis Group --
                     http://www.damtp.cam.ac.uk/user/na/reports.html
    
       -mean_b0 = use mean of all b=0 volumes for linear computation and initial
                  linear for nonlinear method
    
     Example:
      3dDWItoDT -prefix rw01 -automask -reweight -max_iter 10 \
                -max_iter_rw 10 tensor25.1D grad02+orig.
    
     The output is a 6 sub-brick bucket dataset containing 
         Dxx, Dxy, Dyy, Dxz, Dyz, Dzz
     (the lower triangular, row-wise elements of the tensor in symmetric matrix
     form). Additional sub-briks may be appended with the -eigs and -debug_briks
     options.  These results are appropriate as the input to 3dDTeig.
    
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
