.. contents:: 
    :depth: 4 

***********************
@SUMA_AlignToExperiment
***********************

.. code-block:: none

    
    Usage: 
    
    @SUMA_AlignToExperiment                                                 \
        <-exp_anat Experiment_Anatomy> <-surf_anat Surface_Anatomy>         \
        [dxyz] [-wd] [-prefix PREFIX]                                       \
        [-EA_clip_below CLP] [-align_centers] [-ok_change_view]             \
        [-strip_skull WHICH]
    
    Creates a version of Surface Anatomy that is registered to Experiment
    Anatomy.
    
    Mandatory parameters:
    
        <-exp_anat Experiment_Anatomy>
            Name of high resolution anatomical data set in register with
            experimental data.
    
        <-surf_anat Surface_Anatomy> 
            Path and name of high resolution antomical data set used to
            create the surface.
    
      NOTE: In the old usage, there were no -exp_anat and -surf_anat flags
      and the two volumes had to appear first on the command line and in
      the proper order.
    
    Optional parameters:
    
       [-dxyz DXYZ]: This optional parameter indicates that the anatomical
                    volumes must be downsampled to dxyz mm voxel
                    resolution before registration. That is only necessary
                    if 3dvolreg runs out of memory.  You MUST have
                    3dvolreg that comes with afni distributions newer than
                    version 2.45l. It contains an option for reducing
                    memory usage and thus allow the registration of large
                    data sets.
    
       [-out_dxyz DXYZ]: Output the final aligned volume at a cubic
                    voxelsize of DXYZmm. The default is based on the grid
                    of ExpVol.
    
       [-wd]: Use 3dWarpDrive's general affine transform (12 param)
                    instead of 3dvolreg's 6 parameters.  If the anatomical
                    coverage differs markedly between 'Experiment Anatomy'
                    and 'Surface Anatomy', you might need to use
                    -EA_clip_below option or you could end up with a very
                    distorted brain.  The default now is to use -coarserot
                    option with 3dWarpDrive, this should make the program
                    more robust. If you want to try running without it the
                    add -ncr with -wd I would be interested in examining
                    cases where -wd option failed to produce a good
                    alignment.
    
       [-al]: Use 3dAllineate to do the 12 parameter alignment. Cost
                    function is 'lpa'.
    
       [-al_opt 'Options for 3dAllineate']: Specify set of options between
                    quotes to pass to 3dAllineate.
    
       [-ok_change_view]: Be quiet when view of registered volume is
                    changed to match that of the Experiment_Anatomy, even
                    when rigid body registration is used.
    
       [-strip_skull WHICH]: Use 3dSkullStrip to remove non-brain tissue
                    and potentially improve the alignment. WHICH can be
                    one of 'exp_anat', 'surf_anat', 'both', or 'neither'
                    (default).  In the first case, the skull is removed
                    from Experiment_Anatomy dataset, in the second it is
                    removed from the surf_anat dataset.  With 'both' the
                    skull is removed from Experiment_Anatomy and
                    Surface_Anatomy.
    
       [-skull_strip_opt 'Options For 3dSkullStrip']: Pass the options
                    between quotes to 3dSkullStrip.
    
       [-align_centers]: Adds an additional transformation to align the
                    volume centers. This is a good option to use when
                    volumes are severely out of alignment.
    
       [-init_xform XFORM0.1D]: Apply affine transform in XFORM0.1D to
                    Surface_Anatomy before beginning registration.  After
                    convergence, combine XFORM.1D and the the registration
                    matrix to create the output volume To verify that
                    XFORM0.1D does what you think it should be doing, try:
                        3dWarp -matvec_out2in XFORM0.1D           \
                               -prefix pre.SurfVol SurfVol+orig
                    and verify that 'pre.SurfVol+orig' is transformed by
                    XFORM0.1D as you expected it to be.
                    XFORM0.1D can be obtained in a variety of ways. One
                    of which involves extracting it from a transformed
                    volume.  For example, say you want to perform an
                    initial rotation that is equivalent to:
                        3drotate -matvec_order RotMat.1D          \
                              -prefix struct.r struct+orig 
                    The equivalent XFORM0.1D is obtained with:
                        cat_matvec 'struct.r+orig::ROTATE_MATVEC_000000' -I  \
                               > XFORM0.1D  
                    See cat_matvec -help for more details on extracting
                    appropriate affine transforms from dataset headers.
                    See also Example 4 below.
    
       [-EA_clip_below CLP]: Set slices below CLPmm in 'Experiment
                    Anatomy' to zero.  Use this if the coverage of
                    'Experiment Anatomy' dataset extends far below the
                    data in 'Surface Anatomy' dataset.  To get the value
                    of CLP, use AFNI to locate the slice below which you
                    want to clip and set CLP to the z coordinate from
                    AFNI's top left corner. Coordinate must be in RAI,
                    DICOM.
    
       [-prefix PREFIX]: Use PREFIX for the output volume. Default is the
                    prefix
    
       [-surf_anat_followers Fdset1 Fdset2 ...]: Apply the same alignment
                    transform to datasets Fdset1, Fdset2, etc.  This must
                    be the last option on the command line.  All
                    parameters following it are considered datasets.  You
                    can transform other follower dsets manually by
                    executing:
                        3dAllineate -master Experiment_Anatomy              \
                            -1Dmatrix_apply Surface_Anatomy_Alnd_Exp.A2E.1D \
                            -input Fdset                                    \
                            -prefix Fdset_Alnd_Exp+orig                     \
                            -final NN
    
       [-followers_interp KERNEL]: Set the interpolation mode for the
                    follower datasets. Default is NN, which is appropriate
                    for ROI datasets.  Allowed KERNEL values are: NN,
                    linear, cubic, or quintic
                    Note: For atlas datasets, the KERNEL is forced to NN
                    regardless of what you set here.
            of the 'Surface Anatomy' suffixed by _Alnd_Exp.
    
       [-atlas_followers]: Automatically set the followers to be atlases
                    in the directory of -surf_anat. This way all the
                    parcellations will get aligned to the experiment.
    
       [-echo]: Echo all commands to terminal for debugging
    
       [-keep_tmp]: Keep temporary files for debugging. Note that you
                    should delete temporary files before rerunning the
                    script.
    
       [-overwrite_resp RESP]: Answer 'overwrite' questions automatically.
                    RESP (response) should be one of O/S/Q/A:
                        O : overwrite previous result
                        S : skip this step (applying previous result)
                        Q : quit the script
                        A : pause script and ask at each occurance
    
    
    NOTE: You must run the script from the directory where Experiment
    Anatomy resides.
    
    Example 1: For datasets with no relative distortion and comparable
               coverage.  Using 6 param. rigid body transform.
    @SUMA_AlignToExperiment                                   \
        -exp_anat DemoSubj_spgrsa+orig.                       \
        -surf_anat ../FreeSurfer/SUMA/DemoSubj_SurfVol+orig.
    
    Example 2: For datasets with some distortion and different coverage.
               Using 12 param. transform and clipping of areas below
               cerebellum:
    @SUMA_AlignToExperiment                                      \
        -exp_anat ABanat+orig. -surf_anat DemoSubj_SurfVol+orig. \
        -wd -prefix DemoSubj_SurfVol_WD_Alnd_Exp                 \
        -EA_clip_below -30
    
    Example 3: For two monkey T1 volumes with very different resolutions
               and severe shading artifacts.
    
    @SUMA_AlignToExperiment                                      \
        -surf_anat MOanat+orig. -al                              \
        -exp_anat MoExpanat+orig.                                \
        -strip_skull both -skull_strip_opt -monkey               \
        -align_centers                                           \
        -out_dxyz 0.3
    
    Example 4: When -exp_anat and -surf_anat have very different
               orientations Here is an egregious case where -exp_anat (EA)
               was rotated severely out of whack relative to -surf_anat
               (SV), AND volume centers were way off.  With the 'Nudge
               Dataset' plugin, it was determined that a 60deg.  rotation
               got SV oriented more like ExpAnat.  The plugin can be made
               to spit out an the 3dRotate command to apply the roation:
                    3drotate                                        \
                        -quintic -clipit                            \
                        -rotate 0.00I 60.00R 0.00A                  \
                        -ashift 0.00S 0.00L 0.00P                   \
                        -prefix ./SV_rotated+orig SV+orig
               We will get XFROM.1D from that rotated volume:
                    cat_matvec 'SV_rotated+orig::ROTATE_MATVEC_000000' -I \
                        > XFORM0.1D
               and tell @SUMA_AlignToExperiment to apply both center
               alignment and XFORM0.1D
                    @SUMA_AlignToExperiment                        \
                        -init_xform XFORM0.1D -align_centers       \
                        -surf_anat SV+orig  -exp_anat EA+orig      \
                        -prefix SV_A2E_autAUTPre   
               Note 1: 'Nudge Dataset' can also be used to get the centers
               aligned, but that would be more buttons to press.
               Note 2: -init_xform does not need to be accurate, it is
               just meant to get -surf_anat to have a comparable
               orientation.
    
    Global Help Options:
    --------------------
    
       -h_web: Open webpage with help for this program
       -hweb: Same as -h_web
       -h_view: Open -help output in a GUI editor
       -hview: Same as -hview
       -all_opts: List all of the options for this script
       -h_find WORD: Search for lines containing WORD in -help
                     output. Seach is approximate.
    
    More help may be found at:
    https://afni.nimh.nih.gov/ssc/ziad/SUMA/SUMA_doc.htm
    
    Ziad Saad (saadz@mail.nih.gov)
    SSCC/NIMH/ National Institutes of Health, Bethesda Maryland
