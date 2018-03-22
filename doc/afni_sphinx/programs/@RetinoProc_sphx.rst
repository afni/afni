***********
@RetinoProc
***********

.. _ahelp_@RetinoProc:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
       @RetinoProc is a script to process retinotpic FMRI data.
    
     It estimates visual field angles and visual field maps using AFNI's
     3dRetinoPhase, and SurfRetinMap 
    
    The Options:
    ===========
    +++ Latency estimation:
     -phase : Use phase of fundamental frequency to estimate latency (default)
     -delay : Use delay relative to reference time series to estimate latency
              You should be better off using the -delay option, especially in 
              noisy situations
              To graph the reference time series relative to which response
              latency is estimated you will need to run @RetinoProc command
              first. The reference time series are generated at run time.
              The reference time series are in ascii files called ECC.1D and
              POL.1D. You can easily plot them with 1dplot. You can also get
              the commands that generated them (using the program waver) from
              files called: ECC.WAVER.log and POL.WAVER.log.
    
    +++ Stimulus, and Time Series Parameters:
     -TR TR: TR, in seconds, of retinotopic scans
     -period_ecc TECC: Period, in seconds, of eccentricity (rings) and
     -period_pol TPOL: polar angle (wedges) stimuli, respectively.
                 The period is the duration the stimulus takes to complete
                 a full cycle. In other terms, if you were to point at one
                 part of the stimulus and follow that part with your finger, 
                 the period is the duration it takes your finger to get back 
                 to the starting position.
                 The period is independent of the number
                 of rings/wedges used. For most sane people, TECC
                 and TPOL have the same value.
     -pre_ecc PREECC: PREECC and PREPOL are the durations, in sec, before the 
     -pre_pol PREPOL: each of the two stimuli began. The duration is relative
                      to the beginning of the retinotopic time series, 
                      after the pre-steadystate images have been removed.
     -on_ecc N_BLOCKS ON_ECC : Number of stimulation blocks in both directions
     -on_pol N_BLOCKS ON_POL : followed by the duration of stimulation in sec.
                               per visual location.
     -var_on_ecc N_BLOCKS MIN_ON_ECC MAX_ON_ECC STEP_ON_ECC: Use multiple 
     -var_on_pol N_BLOCKS MIN_ON_POL MAX_ON_POL STEP_ON_POL: on durations 
                               and create multiple reference time series
                               for 3dRetinoPhase. See -multi_ref_ts option
                               in 3dRetinoPhase. Leave -var_ options alone
                               If you don't know what you're doing with it.
               All ON_ values are in seconds. STEP_* must be multiple of TR.
    
               Options -*on* are only useful if you use -delay.
    
     -nwedges NWED: Number of wedges in polar stimulus, and number of rings.
     -nrings NRING: in eccentricity stimulus.
    
     -fwhm_pol FWPOL: Target smoothness, in mm, for the polar and for the 
     -fwhm_ecc FWECC: eccentricity stimuli. 
                      Note that the script outputs results for both smoothed
                      and unsmoothed time series.
    
     -ignore IGN: Ignore IGN volumes from the beginning of each time series.
                  When IGN is not 0, make sure that PREECC and PREPOL values
                  represents the durations AFTER IGN volumes are taken out.
                  This option is useless if you input surface-based 
                  time series such as with option -lh_ccw
     -no_tshift:  Do not correct for slice timing. Assume it has been done.
                  This option is useless if you input surface-based 
                  time series such as with option -lh_ccw
    
    +++ Volumetric input:
        Time series datasets
     -ccw CCW_1 CCW_2 ...: Specify the retinotopic time series for each of the
     -clw CLW_1 CLW_2 ...: four stimulus types. You can have multiple runs of
     -exp EXP_1 EXP_2 ...: each type. 
     -con CON_1 CON_2 ...: 
    
        Reference and Anatomical Volumes
     -epi_ref EpiRef: Specify a volume from the EPI time series to which all
                    EPI volumes are aligned. 
                    Default is the 4th sub-brick of the first epi time series
                    on the command line.
     -epi_anat_ref EpiAnatRef: Specify a volume from the EPI time series that
                    is better suited for aligning the T1 to it than EpiRef 
                    might be. EpiAnatRef is usually a pre-steadystate volume
                    which still shows anatomical contrast. This volume is
                    first registered to EpiRef, then its registered version
                    is used as a targe for registering AVol. If not set, 
                    EpiAnatRef is set to be EpiRef.
     -noVR: Skip time series volume registration step. There will be no
                    regression of motion estimates with this option
     -no_volreg: Same as -noVR
     -anat_vol AVol: T1 volume acquired during the same session as the
                     retinotopic scans. This volume is assumed to need
                     registration to EpiRef volume. The registration
                     is carried out automatically by the script, producing
                     a dataset we will call AVol@Epi.
     -anat_vol@epi AVol@Epi: Instead of letting the script align AVol
                     to your EpiRef, you can supply AVol@Epi directly
                     and skip the registration. Of course, you should 
                     be sure that AVol@Epi is indeed aligned with  EpiRef
     -surf_vol SVol: SVol is the Surface Volume for the cortical surfaces.
                     SVol is created when you first run @SUMA_Make_Spec_*
                     scripts. This volume is eventually aligned to AVol@Epi
                     with @SUMA_AlignToExperiment in order to create SVol@Epi
     -surf_vol@epi SVol@Epi: SVol that has been aligned to the experiment's
                     EPI data. If you use this option, you would be providing
                     the output of @SUMA_AlignToExperiment step mentioned 
                     above, allowing the script to skip running it.
                     To be sure you have the right volume, you should be sure
                     the surfaces align with the EPI data.
             Check for this with AFNI and SUMA using:
             suma -spec SPL -sv SVol@Epi & ; afni -niml &
       Note this option used to be called -surf_vol_alndepi
    
    +++ Volume --> Surface options
        Maps by gray matter intersection:
       -gm : Map voxels that intersect gray matter as defined by the bounding
             smoothed white matter and pial surfaces. (default)
    
        Maps by single surface intersections:
       -wm : Map voxels that intersect the smoothed white matter surface only
             This seems to give cleaner maps, perhaps by being less encumbered
             by pial voxels that may have aliased sampling.
       -pial: Map voxels that intersect the pial surface only
       -midlayer: Map voxels that intersect the surface lying midway between 
                  smoothed white matter and pial surfaces
       -layer FRAC: Map voxels that intersect the surface that is a fraction 
                    FRAC of the cortical thickness away from the smoothed 
                    white matter surface.
                In other terms:
                    -wm       ==  -layer 0.0
                    -pial     ==  -layer 1.0
                    -midlayer ==  -layer 0.5
    
    +++ Surface-based input: 
        Surfaces:
     -spec_left  SPL: SPL, and SPR are the spec files for the left and
     -spec_right SPR: right hemispheres, respectively.
    
        Time series datasets: For use when time series have already been 
                              mapped onto the surface.
     -lh_ccw CCW_1 CCW_2 ...: Specify the datasets containing retinotopic time
     -lh_clw CLW_1 CLW_2 ...: series that have already been mapped to the 
     -lh_exp EXP_1 EXP_2 ...: surface for each of the four stimulus types. 
     -lh_con CON_1 CON_2 ...: You can have multiple runs of each type. 
                              The script assumes that nuisance parameters 
                              have already been regressed out of these time
                              series.
        For the right hemisphere, replace -lh_ in the option names with -rh_
        It makes no sense to use these options along with -ccw, -clw, -exp, 
        or -con.
    
    +++ Misc Parameters:
     -dorts ORT1D: Detrend time series using columns in ORT1D file
                   The length of the time series in ORT1D should match
                   that of the time series being fed to 3dDetrend
                   Also, the this option applies to all the time series
                   being processed so that assumes they all have the same
                   lengths.
       Alternately, you can specify a separate ORT file for each dataset on
       the command line with:
     -ccw_orts CCW_1_ORT.1D CCW_2_ORT.1D ...: These options should parallel
     -clw_orts CLW_1_ORT.1D CLW_2_ORT.1D ...: -ccw, -clw, -exp, -con options
     -exp_orts EXP_1_ORT.1D EXP_2_ORT.1D ...: from above. 
     -con_orts CON_1_ORT.1D CON_2_ORT.1D ...: 
           You don't have to specify all or none of *_orts options.
           However, any *_orts option should have as many ORT files
           as its equivalent time series option.
           For example, if you used:
              -ccw       CCW1.nii CCW2.nii CCW3.nii
           to specify orts for these three datasets you need:
              -ccw_orts   ORT_CCW1.1D ORT_CCW2.1D ORT_CCW3.1D
           If for some reason you don't need orts for CCW2.nii, 
           use the string NONE to indicate that:
               -ccw_orts   ORT_CCW1.1D NONE ORT_CCW3.1D
    
     -sid SID: SID is a flag identifying the subject
     -out_dir DIR: Directory where processing results are to be stored
     -echo: Turn on the command echoing to help with debugging script failure
     -echo_edu: Turn on command echoing for certain programs only 
                as opposed to the shell's echoing
     -A2E_opts 'A2E_OPTS': Pass options A2E_OPTS to @SUMA_AlignToExperiment
                           You might use for example,
                           -A2E_opts '-strip_skull surf_anat' since SVol
                           usually has a skull, but AVol@Epi does not.
                           This could help with the alignment in certain
                           difficult cases
               For details on these options see @SUMA_AlignToExperiment -help
     -AEA_opts 'AEA_OPTS': Pass options AEA_OPTS to align_epi_anat.py, which 
                           is the tool used to align T1 anat to EPI.
                           For example if 3dSkullStrip is failing to 
                           strip the epi and you can add:
                           -AEA_opts '-epi_strip 3dAutomask' 
                           or perhaps:
                           -AEA_opts '-epi_strip 3dAutomask -partial_coverage'
               For details on these options see align_epi_anat.py -help
     -fetch_demo: Get the demo archive, do not install it. 
                  (see Sample Data below)
     -install_demo: Get it, install it, and start processing the 1st example
    
    The process:
    ============
       The full process consists of the following steps:
       - Copy input data in the results directory
       - Time shift and register volumetric epi data to EpiRef
       - Align EpiAnatRef to EpiRef to produce a NEW EpiAnatRef
       - Align AVol to (new) EpiAnatRef to produce AVol@Epi
       - Align SVol to AVol@Epi to produce SVol@Epi
       - Detrend components of no interest from time series volumes
       - Map time series to Surfaces
       - Smooth time series on the surfaces
       - Run 3dRetinoPhase on time series to produce field angle dataset
       - Run SurfRetinoMap on field angle data to produce visual field ratio
         datasets.
       - Create a script to show the results with little pain.
         The script is named @ShowResult and is stored in DIR/ 
    
    Sample Data:
    ============
    You can download a test dataset, generously contributed by Peter J. Kohler
     and Sergey V. Fogelson from:
           afni.nimh.nih.gov/pub/dist/tgz/AfniRetinoDemo.tgz
    A README file in the archive will point you to sample scripts that 
    illustrate the usage of @RetinoProc.
    
    You can also use -fetch_demo to have this script get it for you.
    
    References:
    ===========
       [1] RW Cox. AFNI: Software for analysis and visualization of functional
                         magnetic resonance neuroimages.  
                         Computers and Biomedical Research, 29: 162-173, 1996.
       [2] Saad Z.S., et al.  SUMA: An Interface For Surface-Based Intra- And
                          Inter-Subject Analysis With AFNI.
         Proc. 2004 IEEE International Symposium on Biomed. Imaging, 1510-1513
       [3] Saad, Z.S., et al. Analysis and use of FMRI response delays. 
             Hum Brain Mapp, 2001. 13(2): p. 74-93.
       [4] Saad, Z.S., et al., Estimation of FMRI Response Delays.
             Neuroimage, 2003. 18(2): p. 494-504.
       [5] Warnking et al. FMRI Retinotopic Mapping - Step by Step.
             Neuroimage 17, (2002)
    
    Acknowledgments:
    ================
       Peter J. Kohler, and Sergey V. Fogelson: for feedback and sample data
       Michael Beauchamp: for a motivating script and webpage on retintopy
       Ikuko Mukai, Masaki Fukunaga, and Li-Wei Kuo: for difficult data and
                                        making the case for a -delay option
       Jonathan Polimeni: for retinotopy trade secrets
    
    Kvetching:
    ============
    Questions and Comments are best posted to AFNI's message board:
       https://afni.nimh.nih.gov/afni/community/board/
    
          Ziad S. Saad      Aug. 2010 
