*****
3dSeg
*****

.. _3dSeg:

.. contents:: 
    :depth: 4 

.. code-block:: none

    3dSeg segments brain volumes into tissue classes. The program allows
    for adding a variety of global and voxelwise priors. However for the moment,
    only mixing fractions and MRF are documented.
    
    I do not recommend you use this program for quantitative segmentation,
    at least not yet. I have a lot of emotional baggage to overcome on that
    front.
    
    Example 1: Segmenting a skull-stripped T1 volume with:
                  Brain mask, No prior volumes, Uniform mixing fraction
               3dSeg    -anat anat.nii    -mask AUTO \
                        -classes 'CSF ; GM ; WM' -bias_classes 'GM ; WM' \
                        -bias_fwhm 25 -mixfrac UNI -main_N 5 \
                        -blur_meth BFT
    Options:
    
    
    
       -anat ANAT: ANAT is the volume to segment
       -mask MASK: MASK only non-zero voxels in MASK are analyzed.
            MASK is useful when no voxelwise priors are available.
            MASK can either be a dataset or the string 'AUTO'
            which would use AFNI's automask function to create the mask.
    
       -blur_meth BMETH: Set the blurring method for bias field estimation.
         -blur_meth takes one of: BFT, BIM, 
                 BFT: Use Fourier smoothing, masks be damned.
                 BIM: Blur in mask, slower, more accurate, not necessarily 
                      better bias field estimates.
                 BNN: A crude blurring in mask. Faster than BIM but it does
                      not result in accurate FWHM. This option is for 
                      impatient testing. Do not use it.
                 LSB: Localstat moving average smoothing. Debugging only. 
                      Do not use.
         default: BFT
       -bias_fwhm BIAS_FWHM: The amount of blurring used when estimating the
                          field bias with the Wells method.
                          [Wells et. al. IEEE TMI 15, 4, 1997].
                          Use 0.0 to turn off bias field estimation.
    
         default: 25.0
       -classes 'CLASS_STRING': CLASS_STRING is a semicolon delimited
                             string of class labels. At the moment
                             CLASS_STRING can only be 'CSF; GM; WM'
         default: CSF; GM; WM
       -Bmrf BMRF: Weighting factor controlling spatial homogeneity of the 
                classifications. The larger BMRF, the more homogenious the
                classifications will be.
                See Berthod et al. Image and Vision Computing 14 (1996),
                MRFs are also used in FSL's FAST program.
                BMRF = 0.0 means no MRF, 1.0 is a start. 
                Use this option if you have noisy data and no good 
                voxelwise priors.
         default: 0.0
       -bias_classes 'BIAS_CLASS_STRING': A semcolon demlimited string of 
                                       classes that contribute to the 
                                       estimation of the bias field.
         default: 'GM; WM'
       -prefix PREF: PREF is the prefix for all output volume that are not 
                  debugging related.
         default: Segsy
       -overwrite: An option common to almost all AFNI programs. It is 
                automatically turned on if you provide no PREF.
       -debug LEVEL: Set debug level to 0(default), 1, or 2 
       -mixfrac 'MIXFRAC': MIXFRAC sets up the volume-wide (within mask)
                        tissue fractions while initializing the 
                        segmentation (see IGNORE for exception).
                        You can specify the mixing fractions
                        directly such as with '0.1 0.45 0.45', or with
                        the following special flags:
                  'UNI': Equal mixing fractions 
                  'AVG152_BRAIN_MASK': Mixing fractions reflecting AVG152
                                       template.
                  'IGNORE': Ignore mixing fraction while computing posterior
                            probabilities for all the iterations, not just at the
                            initialization as for the preceding variants
         default: UNI
       -mixfloor 'FLOOR': Set the minimum value for any class's mixing fraction.
                       The value should be between 0 and 1 and not to exceed
                       1/(number of classes). This parameter should be kept to
                       a small value.
         default: 0.0001
       -gold GOLD: A goldstandard segmentation volume should you wish to
                   compare 3dSeg's results to it.
       -gold_bias GOLD: A goldstandard bias volume should you wish to
                   compare 3dSeg's bias estimate to it.
    
       -main_N Niter: Number of iterations to perform.
         default: 5
       -cset CSET: Initial classfication. If CSET is not given,
                initialization is carried out with 3dkmean's engine.
    
       -labeltable LT: Label table containing integer keys and corresponding labels.
       -vox_debug 1D_DBG_INDEX: 1D index of voxel to debug.
           OR
       -vox_debug I J K: where I, J, K are the 3D voxel indices 
                         (not RAI coordinates in mm).
       -vox_debug_file DBG_OUTPUT_FILE: File in which debug information is output
