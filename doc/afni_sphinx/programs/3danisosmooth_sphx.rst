*************
3danisosmooth
*************

.. _3danisosmooth:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3danisosmooth [options] dataset
    Smooths a dataset using an anisotropic smoothing technique.
    
    The output dataset is preferentially smoothed to preserve edges.
    
    Options :
      -prefix pname = Use 'pname' for output dataset prefix name.
      -iters nnn = compute nnn iterations (default=10)
      -2D = smooth a slice at a time (default)
      -3D = smooth through slices. Can not be combined with 2D option
      -mask dset = use dset as mask to include/exclude voxels
      -automask = automatically compute mask for dataset
        Can not be combined with -mask
      -viewer = show central axial slice image every iteration.
        Starts aiv program internally.
      -nosmooth = do not do intermediate smoothing of gradients
      -sigma1 n.nnn = assign Gaussian smoothing sigma before
        gradient computation for calculation of structure tensor.
        Default = 0.5
      -sigma2 n.nnn = assign Gaussian smoothing sigma after
        gradient matrix computation for calculation of structure tensor.
        Default = 1.0
      -deltat n.nnn = assign pseudotime step. Default = 0.25
      -savetempdata = save temporary datasets each iteration.
        Dataset prefixes are Gradient, Eigens, phi, Dtensor.
        Ematrix, Flux and Gmatrix are also stored for the first sub-brick.
        Where appropriate, the filename is suffixed by .ITER where 
        ITER is the iteration number. Existing datasets will get overwritten.
      -save_temp_with_diff_measures: Like -savetempdata, but with 
        a dataset named Diff_measures.ITER containing FA, MD, Cl, Cp, 
        and Cs values.
      -phiding = use Ding method for computing phi (default)
      -phiexp = use exponential method for computing phi
      -noneg = set negative voxels to 0
      -setneg NEGVAL = set negative voxels to NEGVAL
      -edgefraction n.nnn = adjust the fraction of the anisotropic
        component to be added to the original image. Can vary between
        0 and 1. Default =0.5
      -datum type = Coerce the output data to be stored as the given type
        which may be byte, short or float. [default=float]
      -matchorig - match datum type and clip min and max to match input data
      -help = print this help screen
    
    References:
      Z Ding, JC Gore, AW Anderson, Reduction of Noise in Diffusion
       Tensor Images Using Anisotropic Smoothing, Mag. Res. Med.,
       53:485-490, 2005
      J Weickert, H Scharr, A Scheme for Coherence-Enhancing
       Diffusion Filtering with Optimized Rotation Invariance,
       CVGPR Group Technical Report at the Department of Mathematics
       and Computer Science,University of Mannheim,Germany,TR 4/2000.
      J.Weickert,H.Scharr. A scheme for coherence-enhancing diffusion
       filtering with optimized rotation invariance. J Visual
       Communication and Image Representation, Special Issue On
       Partial Differential Equations In Image Processing,Comp Vision
       Computer Graphics, pages 103-118, 2002.
      Gerig, G., Kubler, O., Kikinis, R., Jolesz, F., Nonlinear
       anisotropic filtering of MRI data, IEEE Trans. Med. Imaging 11
       (2), 221-232, 1992.
    
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
    For the gruesome details, see the output of 'afni -help'.
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
