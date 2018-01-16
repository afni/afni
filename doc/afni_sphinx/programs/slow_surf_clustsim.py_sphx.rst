*********************
slow_surf_clustsim.py
*********************

.. _slow_surf_clustsim.py:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    =============================================================================
    slow_surf_clustsim.py    - generate a tcsh script to run clustsim on surface
    
    ------------------------------------------
    
       examples:
    
       1. basic: give 3 required inputs, all else is default
    
          While a blur of 4.0 is the default, it is included for clarity.
    
            slow_surf_clustsim.py -save_script surf.clustsim        \
                -uvar spec_file sb23_lh_141_std.spec                \
                -uvar surf_vol sb23_SurfVol_aligned+orig            \
                -uvar blur 4.0                                      \
                -uvar vol_mask mask_3mm+orig                        \
    
    
       2. more advanced, but still based on EPI analysis
    
          Specify p-values, blur size and number of iterations, along with the
          script name and results directory, use 10000 iterations, instead of
          the default 1000.
    
            slow_surf_clustsim.py -save_script surf.clustsim        \
                -uvar spec_file sb23_lh_141_std.spec                \
                -uvar surf_vol sb23_SurfVol_aligned+orig            \
                -uvar vol_mask mask_3mm+orig                        \
                -uvar pthr_list 0.05 0.01 0.002 0.001 0.0002 0.0001 \
                -uvar blur 8.0                                      \
                -uvar niter 10000                                   \
                -save_script csim.10000                             \
                -uvar results_dir clust.results.10000
    
    
       3. basic, but on the surface (so no vol_mask is provided)
    
            slow_surf_clustsim.py -save_script surf.sim.3           \
                -on_surface yes                                     \
                -uvar blur 3.0                                      \
                -uvar spec_file sb23_lh_141_std.spec                \
                -uvar surf_vol sb23_SurfVol_aligned+orig
    
          One can also add a surface mask via '-uvar surf_mask smask_lh.gii'.
    
    
       Note: it is appropriate to use a volume mask on the same grid as the data to
             be analyzed, which is to say either the EPI grid (for functional
             analysis) or perhaps the anatomical grid (for anatomical analysis,
             such as of thickness measures).
    
       Note: the niter values should match between this program and
             quick.alpha.vals.py.
    
    ------------------------------------------
    
       applying the results:
    
          The result of processing should be one z.max.* file for each uncorrected
          p-value input to the program (or each default).  These files contain the
          maximum cluster sizes (in mm^2), per z-score/p-value, and are named using
          the corresponding p-value, e.g. z.max.area.0.001 corresponds to p=0.001.
    
          To get the cluster size required for some uncorrected p-value, run 
          quick.alpha.vals.py on the z.max.area file corresponding to the desired
          p-value, and note the cluster area required for the chosen corrected p.
    
          For example, running this:
    
               quick.alpha.vals.py -niter 1000 z.max.area.0.001
    
          might show that a minimum cluster size of 113 mm^2 would correspond to a
          corrected p=0.05.
    
          Use of -niter should match that from slow_surf_clustsim.py.
    
    ------------------------------------------
    
       script outline:
    
            set control variables
            create and enter results directory
            convert p-value list (pthr_list) to z-scores (zthr_list)
            create dummy time series of length itersize
            for each iter ( iteration list )
                3dcalc: generate noise volume
                3dVol2Surf: map noise to surface
                SurfSmooth: blur to FWHM
                for each index ( itersize list )
                    for each zthr ( zthr_list )
                        SurfClust: make clust file clust.out.$iter.$index.$zthr
            extract lists of maximum areas
    
    ------------------------------------------
    
       terminal options:
    
          -help                     : show this help
          -hist                     : show module history
          -show_default_cvars       : list default control variables
          -show_default_uvars       : list default user variables
          -show_valid_opts          : list valid options
          -ver                      : show current version
    
       other options
          -on_surface yes/no        : if yes, start from noise on the surface
                                      (so no volume data is involved)
          -print_script             : print script to terminal
          -save_script FILE         : save script to given file
          -uvar value ...           : set the user variable
                                      (use -show_default_uvars to see user vars)
          -verb LEVEL               : set the verbosity level
    
    -----------------------------------------------------------------------------
    R Reynolds    June 2011
    =============================================================================
