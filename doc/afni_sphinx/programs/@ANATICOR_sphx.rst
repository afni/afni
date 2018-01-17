*********
@ANATICOR
*********

.. _@ANATICOR:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Script to produce a residual time series cleaned by ANATICOR model.
    
    Usage: 
    @ANATICOR    <-ts TimeSeriesVol>  
                 <-polort polort>
                 <-motion motion.1D>
                 <-aseg aseg.nii>  
                 <-prefix output>
                 [<-radius r >] 
                 [<-view VIEW>]
                 [<-nuisance nuisance.1D>]
                 [<-no_ventricles>]
                 [<-Rsq_WMe>]
                 [<-coverage>]
                 [-verb] [-dirty] [-echo]
    
    Parameters
       -ts TimeSeriesVol: Time series volume
          The time series should have had the following done already:
             Despiking (if necessary)
             RetroIcor, and RVT correction
             Time shifting, and volume registration
        We strongly recommend you do the preprocessing with afni_proc.py,
          for example:
               afni_proc.py  -subj_id ID  -dsets EPI+orig.HEAD \ 
                   -blocks despike ricor tshift volreg regress \ 
                   -tcat_remove_first_trs 4 \ 
                   -ricor_regs_nfirst 0 \ 
                   -ricor_regs oba.slibase.1D \ 
                   -ricor_regress_method per-run \ 
                   -regress_no_motion 
        This is an example for preprocessing, and you should carefully 
          look into your study design and the script made by afni_proc.py.
          See the RETROICOR examples in the help text of afni_proc.py.
       -polort polort: Polynomial for linear trend removal.
                       Use the same order as for afni_proc.py
       -motion motion.1D: head motion parameters from 3dvolreg 
                          Also created by afni_proc.py
       -aseg aseg.nii: aseg file from FreeSurfer's segmentation.
                       This aseg volume must be in register with the EPI
                       time series. Otherwise you're wasting your time.
                       This script will automatically make tissue masks
                       from this file. Do not replace aseg with aparc
                       volumes for example. If you want other methods
                       for providing tissue masks, complain to HJJ, 
                       Email address below.
       -prefix output: Use output (residual time series) for a prefix
       -radius r: The radius of a local sphere mask, r mm
                  default = 15 mm, which what was used in HJJ et al. 2010 
                  for high resolution 1.7x1.7x3mm data. For typical, about
                  3x3x5 resolution, a radius of 30 mm seems to do fine.
                  You should check out the coverage of WMeLocal regressor
                  using -coverage option.
       -view VIEW: Set the view of the output data. Default is +orig
                   Choose from +orig, +acpc, or +tlrc.
       -nuisance nuisance.1D: Other nuisance regressors.
                  Each regressor is a column in .1D file
       -no_ventricles: not include LVe regressor
       -Rsq_WMe: produce an explained variance map for WMeLOCAL regressor.
                 This may help measuring the sptial pattern of local 
                 artifact like the paper of Jo et al. (2010, Neuroimage).
       -coverage: produce a spatial coverage map of WMeLOCAL regressor
       -dirty: Keep temporary files
       -verb: Verbose flag
       -echo: Echo each script command for debugging
    
    Please reference the following paper if you publish results from 
     this script.
    'Mapping sources of correlation in resting state FMRI, with 
     artifact detection and removal'
           Jo, et al., Neuroimage, Vol 52 (2), 571-582, 2010.
           [http://dx.doi.org/10.1016/j.neuroimage.2010.04.246]
    
    Written by Hang Joon Jo. 
               hangjoon.jo@nih.gov (Last Update on 12/15/2010)
