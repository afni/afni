.. _ahelp_3dQwarp:

*******
3dQwarp
*******

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: 3dQwarp [OPTIONS]
    
    * Computes a nonlinearly warped version of source_dataset to match base_dataset.
     ++ The detail allowed in the warping is set by the '-minpatch' option.
     ++ The discrete warp computed herein is a representation of an underlying
        piecewise polynomial C1 diffeomorphism.
     ++ See the OUTLINE OF WARP OPTIMIZATION METHOD section, far below, for details.
    
    * The simplest way to use this program is via the auto_warp.py script.
    * Or to use the @SSwarper script, for warping a T1-weighted dataset to
      the (human brain) MNI 2009 template dataset supplied with AFNI binaries.
    
    * Input datasets must be on the same 3D grid (unlike program 3dAllineate)!
     ++ Or you will get a fatal error when the program checks the datasets!
     ++ However, you can use the '-allineate' option in 3dQwarp to do
        affine alignment before the nonlinear alignment, which will also
        resample the aligned source image to the base dataset grid.
     ++ OR, you can use the '-resample' option in 3dQwarp to resample the
        source dataset to the base grid before doing the nonlinear stuff,
        without doing any preliminary affine alignment.
    
    * 3dQwarp CAN be used on 2D images -- that is, datasets with a single
      slice. How well it works on such datasets has not been investigated
      much, but it DOES work (and quickly, since the amount of data is small).
     ++ You CAN input .jpg or .png files as the source and base images.
     ++ 3dQwarp will convert RGB images to grayscale and attempt to align those.
        The output will still be in dataset format (not image format) and
        will be in grayscale floating point (not color). To get the warped
        image output in .jpg or .png format, you can open the output dataset
        in the AFNI GUI and save the image -- after turning off crosshairs!
      + To get an RGB copy of a warped image, you have to apply the warp to
        each channel (R, G, B) separately and then fuse the results.
     ++ Applying this program to 2D images is mostly for fun; the actual
        utility of it in brain imaging is not clear to Emperor Zhark.
    
    * Input datasets should be reasonably well aligned already
      (e.g., as from an affine warping via 3dAllineate).
     ++ The standard result from 3dAllineate will resample the affinely
        aligned dataset to the same 3D grid as the -base dataset, so this
        new dataset will be ready to run in 3dQwarp against the same base.
     ++ Again, the '-allineate' option can now do this for you, inside 3dQwarp.
    
    * Input datasets should be 'alike'.
     ++ For example, if the '-base' dataset is skull stripped, then the '-source'
        dataset should be skull stripped also -- e.g., via 3dSkullStrip.
     ++ If the datasets have markedly different contrasts (e.g., T1 and T2), then
        using a non-standard matching function such as '-nmi' or '-hel' or '-lpa'
        might work better than the default Pearson correlation matching function.
    
    ******************************************************************************
    * If the input datasets do NOT overlap reasonably well (please look at them  *
    * them in AFNI), or when the source is in scanner space and the base is in a *
    * template space (e.g., MNI), then you need to use '-allineate', or you will *
    * probably get                                                               *
    *  (a) a very bad result (or a program crash)                                *
    *  (b) that takes a long time and a lot of memory to compute.                *
    * 'Overlap well' means that the datasets match well in coordinate space.     *
    * In some cases, datasets may match well voxel-wise, but the xyz coordinates *
    * defined in the dataset headers do not match -- in such a case, 3dQwarp     *
    * will fail. This is why Zhark urges you to LOOK at the overlap in AFNI,     *
    * which uses coordinates for display matching, not voxel indexes. Or use     *
    * the '-allineate' option to get 3dAllineate to line up the dataset by       *
    * brute force, just to be safe (at the cost of a little extra CPU time).     *
    ******************************************************************************
    
    * Outputs of 3dQwarp are the warped dataset and the warp that did it.
     ++ These datasets are stored in float format, no matter what the
        data type of the source dataset.
     ++ Various other optional outputs are described later.
    
    * Simple example:
        3dQwarp -allineate -blur 0 5              \
                -base ~/abin/MNI152_1mm+tlrc.HEAD \
                -source sub637_T1.nii             \
                -prefix sub637_T1qw.nii
      which will produce a dataset warped to match the MNI152 T1 template
      at a 1 mm resolution. Since the MNI152 template is already pretty
      blurry, the amount of blurring applied to it is set to zero, while
      the source dataset (presumably not blurry) will be Gaussian blurred
      with a FWHM of 5 mm.
    
    * Matching uses the 'clipped Pearson' method by default, and
      can be changed to 'pure Pearson' with the '-pear' option.
     ++ The purpose of 'clipping' is to reduce the impact of outlier values
        (small or large) on the correlation.
     ++ For the adventurous, you can also try these matching functions:
          '-hel' for Hellinger distance
          '-mi'  for Mutual Information
          '-nmi' for Normalized Mutual Information
        These options have NOT been extensively tested for usefulness,
        and should be considered experimental at this infundibulum.
     ++ The 'local' correlation options are also now available:
          '-lpc' for Local Pearson minimization (i.e., EPI-T1 registration)
          '-lpa' for Local Pearson maximization
        These options also have not been extensively tested.
     ** If you use '-lpc', then '-maxlev 0' is automatically set. If you want
        to go to more refined levels, you can set '-maxlev' AFTER '-lpc' on the
        command line. Using maxlev > 1 is not recommended for EPI-T1 alignment.
     ** For aligning EPI to T1, the '-lpc' option can be used; my advice
        would be to do something like the following:
          3dSkullStrip -input SUBJ_anat+orig -prefix SUBJ_anatSS
          3dbucket -prefix SUBJ_epiz SUBJ_epi+orig'[0]'
          align_epi_anat.py -anat SUBJ_anat+orig                            \
                            -epi SUBJ_epiz+orig -epi_base 0 -partial_axial  \
                            -epi2anat -master_epi SUBJ_anat+orig            \
                            -big_move
          3dQwarp -source SUBJ_anatSS+orig.HEAD   \
                  -base   SUBJ_epiz_al+orig       \
                  -prefix SUBJ_anatSSQ            \
                  -lpc -verb -iwarp -blur 0 3
          3dNwarpApply -nwarp  SUBJ_anatSSQ_WARPINV+orig  \
                       -source SUBJ_epiz_al+orig          \
                       -prefix SUBJ_epiz_alQ
        * Zeroth, the T1 is prepared by skull stripping and the EPI is prepared
          by extracting just the 0th sub-brick for registration purposes.
        * First, the EPI is aligned to the T1 using the affine 3dAllineate, and
          at the same time resampled to the T1 grid (via align_epi_anat.py).
        * Second, it is nonlinearly aligned ONLY using the global warping -- it is
          futile to try to align such dissimilar image types precisely.
        * The EPI is used as the base in 3dQwarp so that it provides the weighting,
          and so partial brain coverage (as long as it covers MOST of the brain)
          should not cause a problem (we hope).
        * Third, 3dNwarpApply is used to take the inverse warp from 3dQwarp to
          transform the EPI to the T1 space, since 3dQwarp transformed the T1 to
          EPI space. This inverse warp was output by 3dQwarp using '-iwarp'.
        * Someday, this procedure may be incorporated into align_epi_anat.py :-)
     ** It is vitally important to visually look at the results of this process! **
    
    * For aligning T1-weighted anatomical volumes, Zhark recommends that
      you use the 3dUnifize program to (approximately) spatially uniformize
      and normalize their intensities -- this helps in the matching process,
      especially when using datasets from different scanners.
     ++ Skull stripping a la 3dSkullStrip is also a good idea (prior to 3dUnifize),
        even if you are registering datasets from the same subject; see the
        SAMPLE USAGE section below for an example.
     ++ If you ultimately want a non-3dUnifize-d transformed dataset, you can use
        the output WARP dataset and 3dNwarpApply to transform the un-3dUnifize-d
        source dataset; again, see the SAMPLE USAGE section below.
     ++ Some people prefer to nonlinearly align datasets with the 'skull' left on.
        You are free to try this, of course, but we have not tested this method.
        We give you tools; you build things with them.
    
    * If for some deranged reason you have datasets with very non-cubical voxels,
      they should be resampled to a cubical grid before trying 3dQwarp. For example,
      if you have acquired 1x1x4 mm T1-weighted structural volumes (why?), then
      resample them to 1x1x1 mm before doing any other registration processing.
      For example:
        3dAllineate -input anatT1_crude+orig -newgrid 1.0 \
                    -prefix anatT1_fine -final wsinc5     \
                    -1Dparam_apply '1D: 12@0'\'
      This operation can also now be done using the '-allineate' or '-resample'
      options to 3dQwarp.
    
    ** Please note that this program is very CPU intensive, and is what computer
       scientists call a 'pig' (i.e., run time from 10s of minutes to hours).
    
    ------------
    SAMPLE USAGE
    ------------
    * For registering a T1-weighted anat to a mildly blurry template at about
      a 1x1x1 mm resolution (note that the 3dAllineate step, to give the
      preliminary alignment, will also produce a dataset on the same 3D grid
      as the TEMPLATE+tlrc dataset, which 3dQwarp requires):
    
        3dUnifize -prefix anatT1_U -input anatT1+orig
        3dSkullStrip -input anatT1_U+orig -prefix anatT1_US -niter 400 -ld 40
        3dAllineate -prefix anatT1_USA -base TEMPLATE+tlrc    \
                    -source anatT1_US+orig -twopass -cost lpa \
                    -1Dmatrix_save anatT1_USA.aff12.1D        \
                    -autoweight -fineblur 3 -cmass
        3dQwarp -prefix anatT1_USAQ -duplo -blur 0 3 \
                -base TEMPLATE+tlrc -source anatT1_USA+tlrc
    
      You can then use the anatT1_USAQ_WARP+tlrc dataset to transform other
      datasets (that were aligned with the input anatT1+orig) in the same way
      using program 3dNwarpApply, as in
    
        3dNwarpApply -nwarp 'anatT1_USAQ_WARPtlrc anatT1_USA.aff12.1D' \
                     -source NEWSOURCE+orig -prefix NEWSOURCE_warped
    
      For example, if you want a warped copy of the original anatT1+orig dataset
      (without the 3dUnifize and 3dSkullStrip modifications), put 'anatT1' in
      place of 'NEWSOURCE' in the above command.
    
      Note that the '-nwarp' option to 3dNwarpApply has TWO filenames inside
      single quotes. This feature tells that program to compose (catenate) those
      2 spatial transformations before applying the resulting warp. See the -help
      output of 3dNwarpApply for more sneaky/cunning ways to make the program warp
      datasets (and also see the example just below).
    
       ** PLEASE NOTE that if you use the '-allineate' option in 3dQwarp, to   **
       ** do the 3dAllineate step inside 3dQwarp, then you do NOT catenate     **
       ** the affine and nonlinear warps as in the 3dNwarpApply example above, **
       ** since the output nonlinear warp will ALREADY have be catenated with  **
       ** the affine warp -- this output warp is the transformation directly   **
       ** between the '-source' and '-base' datasets (as is reasonable IZHO).  **
    
      If the NEWSOURCE+orig dataset is integer-valued (e.g., anatomical labels),
      then you would use the '-ainterp NN' with 3dNwarpApply, to keep the program
      from interpolating the voxel values.
    
    * If you use align_epi_anat.py to affinely transform several EPI datasets to
      match a T1 anat, and then want to nonlinearly warp the EPIs to the template,
      following the warp generated above, the procedure is something like this:
    
        align_epi_anat.py -anat anatT1+orig -epi epi_r1+orig \
                          -epi_base 3 -epi2anat -big_move    \
                          -child_epi epi_r2+orig epi_r3+orig
    
        3dNwarpApply -source epi_r1+orig                                \
                     -nwarp 'anatT1_USAQ_WARP+tlrc anatT1_USA.aff12.1D' \
                     -affter epi_r1_al_reg_mat.aff12.1D                 \
                     -master WARP -newgrid 2.0                          \
                     -prefix epi_r1_AQ
    
        (mutatis mutandis for 'child' datasets epi_r2, epi_r3, etc.).
    
      The above procedure transforms the data directly from the un-registered
      original epi_r1+orig dataset, catenating the EPI volume registration
      transformations (epi_r1_al_reg_mat.aff12.1D) with the affine anat to
      template transformation (anatT1_USA.aff12.1D) and with the nonlinear
      anat to template transformation (anatT1_USAQ_WARP+tlrc).  3dNwarpApply
      will use the default 'wsinc5' interpolation method, which does not blur
      the results much -- an important issue for statistical analysis of the
      EPI time series.
    
      Various functions, such as volume change fraction (Jacobian determinant)
      can be calculated from the warp dataset via program 3dNwarpFuncs.
    
    --------------------
    COMMAND LINE OPTIONS (too many of them)
    --------------------
     -base   base_dataset   = Alternative way to specify the base dataset.
    
     -source source_dataset = Alternative way to specify the source dataset.
                             * You can either use both '-base' and '-source',
                               OR you can put the base and source dataset
                               names last on the command line.
                             * But you cannot use just one of '-base' or '-source'
                               and then put the other input dataset name at the
                               end of the command line!
                           *** Please note that if you are using 3dUnifize on one
                               dataset (or the template was made with 3dUnifize-d
                               datasets), then the other dataset should also be
                               processed the same way for better results. This
                               dictum applies in general: the source and base
                               datasets should be pre-processed the same way,
                               as far as practicable.
    
     -prefix ppp  = Sets the prefix for the output datasets.
                   * The source dataset is warped to match the base
                     and gets prefix 'ppp'. (Except if '-plusminus' is used.)
                   * The final interpolation to this output dataset is
                     done using the 'wsinc5' method. See the output of
                       3dAllineate -HELP
                     (in the "Modifying '-final wsinc5'" section) for
                     the lengthy technical details.
                   * The 3D warp used is saved in a dataset with
                     prefix '{prefix}_WARP' -- this dataset can be used
                     with 3dNwarpApply and 3dNwarpCat, for example.
                     * To be clear, this is the warp from source dataset
                       coordinates to base dataset coordinates, where the
                       values at each base grid point are the xyz displacments
                       needed to move that grid point's xyz values to the
                       corresponding xyz values in the source dataset:
                         base( (x,y,z) + WARP(x,y,z) ) matches source(x,y,z)
                       Another way to think of this warp is that it 'pulls'
                       values back from source space to base space.
                   * 3dNwarpApply would use '{prefix}_WARP' to transform datasets
                     aligned with the source dataset to be aligned with the
                     base dataset.
                  ** If you do NOT want this warp saved, use the option '-nowarp'.
                -->> (But: This warp is usually the most valuable possible output!)
                   * If you want to calculate and save the inverse 3D warp,
                     use the option '-iwarp'. This inverse warp will then be
                     saved in a dataset with prefix '{prefix}_WARPINV'.
                   * This inverse warp could be used to transform data from base
                     space to source space, if you need to do such an operation.
                   * You can easily compute the inverse later, say by a command like
                      3dNwarpCat -prefix Z_WARPINV 'INV(Z_WARP+tlrc)'
                     or the inverse can be computed as needed in 3dNwarpApply, like
                      3dNwarpApply -nwarp 'INV(Z_WARP+tlrc)' -source Dataset.nii ...
    
     -allineate   = This option will make 3dQwarp run 3dAllineate first, to align
       *OR*         the source dataset to the base with an affine transformation.
     -allin         It will then use that alignment as a starting point for the
       *OR*         nonlinear warping.
     -allinfast    * With '-allineate', the source dataset does NOT have to be on
                     the same 3D grid as the base, since the intermediate output
                     of 3dAllineate (the substitute source) will be on the grid
                     as the base.
                   * If the datasets overlap reasonably already, you can use the
                     option '-allinfast' (instead of '-allineate') to add the
                     option '-onepass' to the 3dAllineate command line, to make
                     it run faster (by avoiding the time-consuming coarse pass
                     step of trying lots of shifts and rotations to find an idea
                     of how to start). But you should KNOW that the datasets do
                     overlap well before using '-allinfast'.
              -->>** The final output warp dataset is the warp directly between
                     the original source dataset and the base (i.e., the catenation
                     of the affine matrix from 3dAllineate and the nonlinear warp
                     from the 'warpomatic' procedure in 3dQwarp).
              -->>** The above point means that you should NOT NOT NOT use the
                     affine warp output by the '-allineate' option in combination
                     with the nonlinear warp output by 3dQwarp (say, when using
                     3dNwarpApply), since the affine warp would then be applied
                     twice -- which would be WRONG WRONG WRONG.
              -->>** The final output warped dataset is warped directly from the
                     original source dataset, NOT from the substitute source.
                   * The intermediate files from 3dAllineate (the substitute source
                     dataset and the affine matrix) are saved, using 'prefix_Allin'
                     in the filenames. If you wish to have them deleted, use the
                     option '-allinkill' in addition to '-allineate'.
                 *** The following 3dQwarp options CANNOT be used with -allineate:
                       -plusminus  -inilev  -iniwarp
                 *** However, you CAN use -duplo with -allineate.
                   * The '-awarp' option will output the computed warp from the
                     intermediea 3dAllineate-d dataset to the base dataset,
                     in case you want that for some reason. This option will
                     only have meaning if '-allineate' or '-allinfast' is used.
                     The prefix of the '-awarp' output will have the string
                     '_AWARP' appended to the {prefix} for the output dataset.
    
     -allineate_opts '-opt ...'
       *OR*        * This option lets you add extra options to the 3dAllineate
     -allopt         command to be run by 3dQwarp. Normally, you won't need
                     to do this.
                   * All the extra options for the 3dAllineate command line
                     should be enclosed inside a pair of quote marks; e.g.,
                        -allopt '-cost lpa -verb'
                   * If '-emask' is used in 3dQwarp, the same option will be
                     passed to 3dAllineate automatically, so you don't have to
                     do that yourself.
                 *** Do NOT attempt to use the (obsolescent) '-nwarp' option in
                     3dAllineate from inside 3dQwarp -- bad things will probably
                     happen, and you won't EVER get any Christmas presents again!
    
     -resample    = This option simply resamples the source dataset to match the
                    base dataset grid. You can use this if the two datasets
                    overlap well (as seen in the AFNI GUI), but are not on the
                    same 3D grid.
                   * If they don't overlap very well, use '-allineate' instead.
                   * As with -allineate, the final output dataset is warped
                     directly from the source dataset, not from the resampled
                     source dataset.
                   * The reampling here (and with -allineate) is done with the
                     'wsinc5' method, which has very little blurring artifact.
                   * If the base and source datasets ARE on the same 3D grid,
                     then the -resample option will be ignored.
                   * You CAN use -resample with these 3dQwarp options:
                       -plusminus  -inilev  -iniwarp  -duplo
                     In particular, '-iniwarp' and '-resample' will work
                     together if you need to re-start a warp job from the
                     output of '-allsave'.
                   * Unless you are in a hurry, '-allineate' is better.
    
     -nowarp      = Do not save the _WARP file.
                   * By default, the {prefix}_WARP dataset will be saved.
    
     -iwarp       = Do compute and save the _WARPINV file.
                   * By default, the {prefix}_WARPINV file is NOT saved.
    
     -nodset      = Do not save the warped source dataset (i.e., if you only
                    need the _WARP).
                   * By default, the warped source dataset {prefix} is saved.
    
     -awarp       = If '-allineate' is used, output the nonlinear warp that
                    transforms from the 3dAllineate-d affine alignment of
                    source-to-base to the base. This warp (output {prefix}_AWARP)
                    combined with the affine transformation {prefix}.aff12.1D is
                    the same as the final {prefix}_WARP nonlinear transformation
                    directly from source-to-base.
                   * The '-awarp' output is mostly useful when you need to have
                     this incremental nonlinear warp for various purposes; for
                     example, it is used in the @SSwarper script.
                   * '-awarp' will not do anything unless '-allineate' is also
                     used, because it doesn't have anything to do!
                   * By default, this {prefix}_AWARP file is NOT saved.
    
     -pear        = Use strict Pearson correlation for matching.
                   * Not usually recommended, since the 'clipped Pearson' method
                     used by default will reduce the impact of outlier values.
                   * No partridges or trees are implied by this option.
    
     -noneg       = Replace negative values in either input volume with 0.
                   * If there ARE negative input values, and you do NOT use -noneg,
                     then strict Pearson correlation will be used, since the
                     'clipped' method only is implemented for non-negative volumes.
                   * '-noneg' is not the default, since there might be situations
                     where you want to align datasets with positive and negative
                     values mixed.
                   * But, in many cases, the negative values in a dataset are just
                     the result of interpolation artifacts (or other peculiarities),
                     and so they should be ignored. That is what '-noneg' is for.
                   * Therefore, '-noneg' is recommended for most applications.
    
     -nopenalty   = Don't use a penalty on the cost function; the goal
                    of the penalty is to reduce grid distortions.
                   * If there penalty is turned off AND you warp down to
                     a fine scale (e.g., '-minpatch 11'), you will probably
                     get strange-looking results.
    
     -penfac ff   = Use the number 'ff' to weight the penalty.
                    The default value is 1. Larger values of 'ff' mean the
                    penalty counts more, reducing grid distortions,
                    insha'Allah; '-nopenalty' is the same as '-penfac 0'.
    
     -useweight   = With '-useweight', each voxel in the base automask is weighted
                    by the intensity of the (blurred) base image. This makes
                    white matter count more in T1-weighted volumes, for example.
               -->>* [24 Mar 2014] This option is is now the default.
    
     -noweight    = If you want a binary weight (the old default), use this option.
                    That is, each voxel in the base volume automask will be
                    weighted the same in the computation of the cost functional.
    
     -weight www  = Instead of computing the weight from the base dataset,
                    directly input the weight volume from dataset 'www'.
                   * Useful if you know what over parts of the base image you
                     want to emphasize or de-emphasize the matching functional.
    
     -wball x y z r f =
                    Enhance automatic weight from '-useweight' by a factor
                    of 1+f*Gaussian(FWHM=r) centered in the base image at
                    DICOM coordinates (x,y,z) and with radius 'r'. The
                    goal of this option is to try and make the alignment
                    better in a specific part of the brain.
                   * Example:  -wball 0 14 6 30 40
                     to emphasize the thalamic area (in MNI/Talairach space).
                   * The 'r' parameter must be positive!
                   * The 'f' parameter must be between 1 and 100 (inclusive).
                   * '-wball' does nothing if you input your own weight
                     with the '-weight' option.
                   * '-wball' does change the binary weight created by
                     the '-noweight' option.
                   * You can only use '-wball' once in a run of 3dQwarp.
                 *** The effect of '-wball' is not dramatic. The example
                     above makes the average brain image across a collection
                     of subjects a little sharper in the thalamic area, which
                     might have some small value. If you care enough about
                     alignment to use '-wball', then you should examine the
                     results from 3dQwarp for each subject, to see if the
                     alignments are good enough for your purposes.
    
     -wmask ws f  = Similar to '-wball', but here, you provide a dataset 'ws'
                    that indicates where to increase the weight.
                   * The 'ws' dataset must be on the same 3D grid as the base
                      dataset.
                   * 'ws' is treated as a mask -- it only matters where it
                     is nonzero -- otherwise, the values inside are not used.
                   * After 'ws' comes the factor 'f' by which to increase the
                     automatically computed weight. Where 'ws' is nonzero,
                     the weighting will be multiplied by (1+f).
                   * As with '-wball', the factor 'f' should be between 1 and 100.
                   * You cannot use '-wball' and '-wmask' together!
    
     -wtprefix p  = Saves auto-computed weight volume to a dataset with prefix 'p'.
                    If you are sufficiently dedicated, you could manually edit
                    this volume, in the AFNI GUI, in 3dcalc, et cetera. And then
                    use it, instead of the auto-computed default weight, via the
                    '-weight' option.
                   * If you use the '-emask' option, the effects of the exclusion
                     mask are NOT shown in this output dataset!
    
     -blur bb     = Gaussian blur the input images by 'bb' (FWHM) voxels before
                    doing the alignment (the output dataset will not be blurred).
                    The default is 2.345 (for no good reason).
                   * Optionally, you can provide 2 values for 'bb', and then
                     the first one is applied to the base volume, the second
                     to the source volume.
               -->>* e.g., '-blur 0 3' to skip blurring the base image
                     (if the base is a blurry template, for example).
                   * A negative blur radius means to use 3D median filtering,
                     rather than Gaussian blurring. This type of filtering will
                     better preserve edges, which might be important in alignment.
                   * If the base is a template volume that is already blurry,
                     you probably don't want to blur it again, but blurring
                     the source volume a little is probably a good idea, to
                     help the program avoid trying to match tiny features.
                   * Note that -duplo will blur the volumes some extra
                     amount for the initial small-scale warping, to make
                     that phase of the program converge more rapidly.
    
     -pblur       = Use progressive blurring; that is, for larger patch sizes,
                    the amount of blurring is larger. The general idea is to
                    avoid trying to match finer details when the patch size
                    and incremental warps are coarse. When '-blur' is used
                    as well, it sets a minimum amount of blurring that will
                    be used. [06 Aug 2014 -- '-pblur' may be the default someday].
                   * You can optionally give the fraction of the patch size that
                     is used for the progressive blur by providing a value between
                     0 and 0.25 after '-pblur'. If you provide TWO values, the
                     the first fraction is used for progressively blurring the
                     base image and the second for the source image. The default
                     parameters when just '-pblur' is given is the same as giving
                     the options as '-pblur 0.09 0.09'.
                   * '-pblur' is useful when trying to match 2 volumes with high
                     amounts of detail; e.g, warping one subject's brain image to
                     match another's, or trying to match a detailed template.
                   * Note that using negative values with '-blur' means that the
                     progressive blurring will be done with median filters, rather
                     than Gaussian linear blurring.
             -->>*** The combination of the -allineate and -pblur options will make
                     the results of using 3dQwarp to align to a template somewhat
                     less sensitive to initial head position and scaling.
    
     -nopblur     = Don't use '-pblur'; equivalent to '-pblur 0 0'.
    
     -emask ee    = Here, 'ee' is a dataset to specify a mask of voxels
                    to EXCLUDE from the analysis -- all voxels in 'ee'
                    that are NONZERO will not be used in the alignment.
                   * The base image always automasked -- the emask is
                     extra, to indicate voxels you definitely DON'T want
                     included in the matching process, even if they are
                     inside the brain.
               -->>* Note that 3dAllineate has the same option. Since you
                     usually have to use 3dAllineate before 3dQwarp, you
                     will probably want to use -emask in both programs.
                    [ Unless, of course, you are using '-allineate',  which  ]
                    [ will automatically include '-emask' in the 3dAllineate ]
                    [ phase if '-emask' is used here in 3dQwarp.             ]
                   * Applications: exclude a tumor or resected region
                     (e.g., draw a mask in the AFNI Drawing plugin).
               -->>* Note that the emask applies to the base dataset,
                     so if you are registering a pre- and post-surgery
                     volume, you would probably use the post-surgery
                     dataset as the base. If you eventually want the
                     result back in the pre-surgery space, then you
                     would use the inverse warp afterwards (in 3dNwarpApply).
    
     -noXdis      = These options let you specify that the warp should not
     -noYdis      = displace in the given direction. For example, combining
     -noZdis      = -noXdis and -noZdis would mean only warping along the
                    y-direction would be allowed.
                   * Here, 'x' refers to the first coordinate in the dataset,
                     which is usually the Right-to-Left direction. Et cetera.
                   * Note that the output WARP dataset(s) will have sub-bricks
                     for the displacements which are all zero; every WARP dataset
                     has 3 sub-bricks.
    
     -iniwarp ww  = 'ww' is a dataset with an initial nonlinear warp to use.
                   * If this option is not used, the initial warp is the identity.
                   * You can specify a catenation of warps (in quotes) here, as in
                     program 3dNwarpApply.
                   * As a special case, if you just input an affine matrix in a .1D
                     file, that also works -- it is treated as giving the initial
                     warp via the string "IDENT(base_dataset) matrix_file.aff12.1D".
                   * You CANNOT use this option with -duplo !!
                   * -iniwarp is usually used with -inilev to re-start 3dQwarp from
                     a previous stopping point, or from the output of '-allsave'.
                   * In particular, '-iniwarp' and '-resample' will work
                     together if you need to re-start a warp job from the
                     output of '-allsave'.
    
     -inilev lv   = 'lv' is the initial refinement 'level' at which to start.
                   * Usually used with -iniwarp; CANNOT be used with -duplo.
                   * The combination of -inilev and -iniwarp lets you take the
                     results of a previous 3dQwarp run and refine them further:
                       3dQwarp -prefix Q25 -source SS+tlrc -base TEMPLATE+tlrc \
                               -duplo -minpatch 25 -blur 0 3
                       3dQwarp -prefix Q11 -source SS+tlrc -base TEMPLATE+tlrc \
                               -inilev 7 -iniwarp Q25_WARP+tlrc -blur 0 2
                     Note that the source dataset in the second run is the SAME as
                     in the first run. If you don't see why this is necessary,
                     then you probably need to seek help from an AFNI guru.
              -->>** Also see the script @toMNI_Qwarpar for the use of this option
                     in creating a template dataset from a collection of scans from
                     different subjects.
    
     -minpatch mm = Set the minimum patch size for warp searching to 'mm' voxels.
       *OR*        * The value of mm should be an odd integer.
     -patchmin mm  * The default value of mm is 25.
                   * For more accurate results than mm=25, try 19 or 13.
                   * The smallest allowed patch size is 5.
                   * OpenMP parallelization becomes inefficient for patch sizes
                     smaller than about 15x15x15 -- which is why running 3dQwarp
                     down to the minimum patch level of 5 can be very slow.
                   * You may want stop at a larger patch size (say 7 or 9) and use
                     the -Qfinal option to run that final level with quintic warps,
                     which might run faster and provide the same degree of warp detail.
                   * Trying to make two different brain volumes match in fine detail
                     is usually a waste of time, especially in humans. There is too
                     much variability in anatomy to match gyrus to gyrus accurately,
                     especially in the small foldings in the outer cerebral cortex.
                     For this reason, the default minimum patch size is 25 voxels.
                     Using a smaller '-minpatch' might try to force the warp to
                     match features that do not match, and the result can be useless
                     image distortions -- another reason to LOOK AT THE RESULTS.
                                                            -------------------
    
     -maxlev lv   = Here, 'lv' is the maximum refinement 'level' to use. This
                    is an alternate way to specify when the program should stop.
                   * To only do global polynomial warping, use '-maxlev 0'.
                   * If you use both '-minpatch' and '-maxlev', then you are
                     walking on the knife edge of danger.
                   * Of course, I know that you LIVE for such thrills.
    
     -gridlist gl = This option provides an alternate way to specify the patch
                    grid sizes used in the warp optimization process. 'gl' is
                    a 1D file with a list of patches to use -- in most cases,
                    you will want to use it in the following form:
                      -gridlist '1D: 0 151 101 75 51'
                   * Here, a 0 patch size means the global domain. Patch sizes
                     otherwise should be odd integers >= 5.
                   * If you use the '0' patch size again after the first position,
                     you will actually get an iteration at the size of the
                     default patch level 1, where the patch sizes are 75% of
                     the volume dimension. There is no way to force the program
                     to literally repeat the sui generis step of lev=0.
                   * You cannot use -gridlist with -duplo or -plusminus!
    
     -allsave     = This option lets you save the output warps from each level
       *OR*         of the refinement process. Mostly used for experimenting.
     -saveall      * Cannot be used with -nopadWARP, -duplo, or -plusminus.
                   * You could use the saved warps to create different versions
                     of the warped source datasets (using 3dNwarpApply), to help
                     you visualize how the warping process makes progress.
                   * The saved warps are written out at the end of each level,
                     before the next level starts computation. Thus, they could
                     be used to re-start the computation if the program crashed
                     (by using options '-inilev' and '-iniwarp').
    
     -duplo       = Start off with 1/2 scale versions of the volumes,
                    for getting a speedy coarse first alignment.
                   * Then scales back up to register the full volumes.
                     The goal is greater speed, and it seems to help this
                     positively piggish program to be more expeditious.
                   * However, accuracy is somewhat lower with '-duplo',
                     for reasons that currenly elude Zhark; for this reason,
                     the Emperor does not usually use '-duplo'.
    
     -workhard    = Iterate more times, which can help when the volumes are
                    hard to align at all, or when you hope to get a more precise
                    alignment.
                   * Slows the program down (possibly a lot), of course.
                   * When you combine '-workhard'  with '-duplo', only the
                     full size volumes get the extra iterations.
                   * For finer control over which refinement levels work hard,
                     you can use this option in the form (for example)
                         -workhard:4:7
                     which implies the extra iterations will be done at levels
                     4, 5, 6, and 7, but not otherwise.
                   * You can also use '-superhard' to iterate even more, but
                     this extra option will REALLY slow things down.
               -->>* Under most circumstances, you should not need to use either
                     -workhard or -superhard.
               -->>* The fastest way to register to a template image is via the
                     -duplo option, and without the -workhard or -superhard options.
                   * But accuracy may suffer :(
               -->>* If you use this option in the form '-Workhard' (first letter
                     in upper case), then the second iteration at each level is
                     done with quintic polynomial warps.
    
     -Qfinal      = At the finest patch size (the final level), use Hermite
                    quintic polynomials for the warp instead of cubic polynomials.
                   * In a 3D 'patch', there are 2x2x2x3=24 cubic polynomial basis
                     function parameters over which to optimize (2 polynomials
                     dependent on each of the x,y,z directions, and 3 different
                     directions of displacement).
                   * There are 3x3x3x3=81 quintic polynomial parameters per patch.
                   * With -Qfinal, the final level will have more detail in
                     the allowed warps, at the cost of yet more CPU time.
                   * However, no patch below 7x7x7 in size will be done with quintic
                     polynomials.
                   * This option is also not usually needed, and is experimental.
    
     -Qonly       = Use Hermite quintic polynomials at all levels.
                   * Very slow (about 4 times longer). Also experimental.
                   * Will produce a (discrete representation of a) C2 warp.
    
     -plusminus   = Normally, the warp displacements dis(x) are defined to match
                    base(x) to source(x+dis(x)). With this option, the match
                    is between base(x-dis(x)) and source(x+dis(x)) -- the two
                    images 'meet in the middle'.
                   * One goal is to mimic the warping done to MRI EPI data by
                     field inhomogeneities, when registering between a 'blip up'
                     and a 'blip down' down volume, which will have opposite
                     distortions.
                   * Define Wp(x) = x+dis(x) and Wm(x) = x-dis(x). Then since
                     base(Wm(x)) matches source(Wp(x)), by substituting INV(Wm(x))
                     wherever we see x, we have base(x) matches source(Wp(INV(Wm(x))));
                     that is, the warp V(x) that one would get from the 'usual' way
                     of running 3dQwarp is V(x) = Wp(INV(Wm(x))).
                   * Conversely, we can calculate Wp(x) in terms of V(x) as follows:
                       If V(x) = x + dv(x), define Vh(x) = x + dv(x)/2;
                       then Wp(x) = V(INV(Vh(x)))
                   * With the above formulas, it is possible to compute Wp(x) from
                     V(x) and vice-versa, using program 3dNwarpCalc. The requisite
                     commands are left as exercises for aspiring AFNI Jedi Masters.
                   * You can use the semi-secret '-pmBASE' option to get the V(x)
                     warp and the source dataset warped to base space, in addition
                     to the Wp(x) '_PLUS' and Wm(x) '_MINUS' warps.
               -->>* Alas: -plusminus does not work with -duplo or -allineate :-(
                   * However, you can use -iniwarp with -plusminus :-)
               -->>* The outputs have _PLUS (from the source dataset) and _MINUS
                     (from the base dataset) in their filenames, in addition to
                     the {prefix}. The -iwarp option, if present, will be ignored.
    
     -pmNAMES p m = This option lets you change the PLUS and MINUS prefix appendages
                    alluded to directly above to something else that might be more
                    easy for you to grok. For example, if you are warping EPI
                    volumes with phase-encoding in the LR-direction with volumes
                    that had phase-encoding in the RL-direction, you might do
                    something like
       -base EPI_LR+orig -source EPI_RL+orig -plusminus -pmNAMES RL LR -prefix EPIuw
                    recalling the the PLUS name goes with the source (RL) and the
                    MINUS name goes with the base (RL). Then you'd end up with
                    datasets
                      EPIuw_LR+orig and EPIuw_LR_WARP+orig from the base
                      EPIuw_RL+orig and EPIuw_RL_WARP+orig from the source
                    The EPIuw_LR_WARP+orig file could then be used to unwarp (e.g.,
                    using 3dNwarpApply) other LR-encoded EPI datasets from the same
                    scanning session.
    
     -nopad      = Do NOT use zero-padding on the 3D base and source images.
                   [Default == zero-pad as needed]
                  * The underlying model for deformations goes to zero at the
                    edge of the volume being warped. However, if there is
                    significant data near an edge of the volume, then it won't
                    get displaced much, and so the results might not be good.
                  * Zero padding is designed as a way to work around this potential
                    problem. You should NOT need the '-nopad' option for any
                    reason that Zhark can think of, but it is here to be symmetrical
                    with 3dAllineate.
                  * Note that the output (warped from source) dataset will be on the
                    base dataset grid whether or not zero-padding is allowed.,
                    However, unless you use the following option, allowing zero-
                    padding (i.e., the default operation) will make the output WARP
                    dataset(s) be on a larger grid (also see '-expad' below).
    
     -nopadWARP   = If you do NOT use '-nopad' (that is, you DO allow zero-padding
                    during the warp computations), then the computed warp will often
                    be bigger than the base volume. This situation is normally not
                    an issue, but if for some reason you require the warp volume to
                    match the base volume, then use '-nopadWARP' to have the output
                    WARP dataset(s) truncated.
                   * Note that 3dNwarpApply and 3dNwarpAdjust will deal with warps
                     that are defined over grids that are larger than the datasets
                     to which they are applied; this is why Zhark says above that
                     a padded warp 'is normally not an issue'.
    
     -expad EE    = This option instructs the program to pad the warp by an extra
                    'EE' voxels (and then 3dQwarp starts optimizing it).
                   * This option is seldom needed, but can be useful if you
                     might later catenate the nonlinear warp -- via 3dNwarpCat --
                     with an affine transformation that contains a large shift.
                     Under that circumstance, the nonlinear warp might be shifted
                     partially outside its original grid, so expanding that grid
                     can avoid this problem.
                   * Note that this option perforce turns off '-nopadWARP'.
    
     -ballopt     = Normally, the incremental warp parameters are optimized inside
                    a rectangular 'box' (24 dimensional for cubic patches, 81 for
                    quintic patches), whose limits define the amount of distortion
                    allowed at each step. Using '-ballopt' switches these limits
                    to be applied to a 'ball' (interior of a hypersphere), which
                    can allow for larger incremental displacements. Use this
                    option if you think things need to be able to move farther.
    
     -boxopt      = Use the 'box' optimization limits instead of the 'ball'
                    [this is the default at present].
                   * Note that if '-workhard' is used, then ball and box
                     optimization are alternated in the different iterations at
                     each level, so these two options have no effect in that case.
    
     -verb        = Print out very very verbose progress messages (to stderr) :-)
     -quiet       = Cut out most of the fun fun fun progress messages :-(
    
    -----------------------------------
    INTERRUPTING the program gracefully
    -----------------------------------
    If you want to stop the program AND have it write out the results up to
    the current point, you can do so with a command like
      kill -s QUIT processID
    where 'processID' is the process identifier number (pid) for the 3dQwarp
    program you want to terminate. A command like
      ps aux | grep 3dQwarp
    will give you a list of all your processes with the string '3dQwarp' in
    the command line. For example, at the moment I wrote this text, I would
    get the response
     rwcox 62873 693.8 2.3 3274496 755284 p2 RN+ 12:36PM 380:25.26 3dQwarp -prefix ...
     rwcox  6421   0.0 0.0 2423356    184 p0 R+   1:33PM   0:00.00 grep 3dQwarp
     rwcox  6418   0.0 0.0 2563664   7344 p4 S+   1:31PM   0:00.15 vi 3dQwarp.c
    so the processID for the actual run of 3dQwarp was 62873.
    (Also, you can see that Zhark is a 'vi' acolyte, not an 'emacs' heretic.)
    
    The program will 'notice' the QUIT signal at the end of the optimization
    of the next patch, so it may be a moment or two before it actually saves
    the output dataset(s) and exits.
    
    Of course, if you just want to kill the process in a brute force way, with
    nothing left behind to examine, then 'kill processID' will work.
    
    ----------------------------------------------------------------
    CLARIFICATION about the confusing forward and inverse warp issue
    ----------------------------------------------------------------
    An AFNI nonlinear warp dataset stores the displacements (in DICOM mm) from
    the base dataset grid to the source dataset grid. For computing the source
    dataset warped to the base dataset grid, these displacements are needed,
    so that for each grid point in the output (warped) dataset, the corresponding
    location in the source dataset can be found, and then the value of the source
    at that point can be computed (interpolated).
    
    That is, this forward warp is good for finding where a given point in the
    base dataset maps to in the source dataset. However, for finding where a
    given point in the source dataset maps to in the base dataset, the inverse
    warp is needed. Or, if you wish to warp the base dataset to 'look like' the
    source dataset, then you use 3dNwarpApply with the input warp being the
    inverse warp from 3dQwarp.
    
    -----------------------------------
    OUTLINE OF WARP OPTIMIZATION METHOD
    -----------------------------------
    Repeated composition of incremental warps defined by Hermite cubic basis
    functions, first over the entire volume, then over steadily shrinking and
    overlapping patches increasing 'levels': the patches shrink by a factor of
    0.75 at each level).
    
    At 'level 0' (over the entire volume), Hermite quintic basis functions are also
    employed, but these are not used at the more refined levels. All basis functions
    herein are (at least) continuously differentiable, so the discrete warp computed
    will be a representation of an underlying C1 diffeomorphism. The basis functions
    go to zero at the edge of each patch, so the overall warp will decay to the
    identity warp (displacements=0) at the edge of the base volume. (However, use
    of '-allineate' can make the final output warp be nonzero at the edges; the
    programs that apply warps to datasets linearly extrapolate warp displacements
    outside the 3D box over which the warp is defined.)
    
    For this procedure to work, the source and base datasets need to be reasonably
    well aligned already (e.g., via 3dAllineate, if necessary). Multiple warps can
    later be composed and applied via programs 3dNwarpApply and/or 3dNwarpCalc.
    
    Note that it is not correct to say that the resulting warp is a piecewise cubic
    (or quintic) polynomial. The first warp created (at level 0) is such a warp;
    call that W0(x). Then the incremental warp W1(x) applied at the next iteration
    is also a cubic polynomial warp (say), and the result is W0(W1(x)), which is
    more complicated than a cubic polynomial -- and so on. The incremental warps
    aren't added, but composed, so that the mathematical form of the final warp
    would be very unwieldy to express in polynomial form. Of course, the program
    just keeps track of the displacements, not the polynomial coefficients, so it
    doesn't 'care' about the underlying polynomials at all.
    
    One reason for incremental improvement by composition, rather than by addition,
    is the simple fact that if W0(x) is invertible and W1(x) is invertible, then
    W0(W1(x)) is also invertible -- but W0(x)+W1(x) might not be. The incremental
    polynomial warps are kept invertible by simple constraints on the magnitudes
    of their coefficients.
    
    The penalty is a Neo-Hookean elastic energy function, based on a combination of
    bulk and shear distortions: cf. http://en.wikipedia.org/wiki/Neo-Hookean_solid
    The goal is to keep the warps from becoming too 'weird' (doesn't always work).
    
    By perusing the many options above, you can see that the user can control the
    warp optimization in various ways. All these options make using 3dQwarp seem
    pretty complicated. The reason there are so many options is that many different
    cases arise, and we are trying to make the program flexible enough to deal with
    them all. The SAMPLE USAGE section above is a good place to start for guidance.
    Or you can use the @SSwarper and auto_warp.py scripts.
    
    -------------------------------------------------------------------------------
    ***** This program is experimental and subject to sudden horrific change! *****
    -------------------------------------------------------------------------------
    
    ----- AUTHOR = Zhark the Grotesquely Warped -- Fall/Winter/Spring 2012-13 -----
    -----          (but still strangely handsome)                             -----
    
     =========================================================================
    * This binary version of 3dQwarp is compiled using OpenMP, a semi-
       automatic parallelizer software toolkit, which splits the work across
       multiple CPUs/cores on the same shared memory computer.
    * OpenMP is NOT like MPI -- it does not work with CPUs connected only
       by a network (e.g., OpenMP doesn't work with 'cluster' setups).
    * For implementation and compilation details, please see
       https://afni.nimh.nih.gov/pub/dist/doc/misc/OpenMP.html
    * The number of CPU threads used will default to the maximum number on
       your system. You can control this value by setting environment variable
       OMP_NUM_THREADS to some smaller value (including 1).
    * Un-setting OMP_NUM_THREADS resets OpenMP back to its default state of
       using all CPUs available.
       ++ However, on some systems, it seems to be necessary to set variable
          OMP_NUM_THREADS explicitly, or you only get one CPU.
       ++ On other systems with many CPUS, you probably want to limit the CPU
          count, since using more than (say) 16 threads is probably useless.
    * You must set OMP_NUM_THREADS in the shell BEFORE running the program,
       since OpenMP queries this variable BEFORE the program actually starts.
       ++ You can't usefully set this variable in your ~/.afnirc file or on the
          command line with the '-D' option.
    * How many threads are useful? That varies with the program, and how well
       it was coded. You'll have to experiment on your own systems!
    * The number of CPUs on this particular computer system is ...... 16.
    * The maximum number of CPUs that will be used is now set to .... 12.
    * Tests show that using more 10-12 CPUs with 3dQwarp doesn't help much.
      If you have more CPUs on one system, it's faster to run two or three
      separate registration jobs in parallel than to use all the CPUs on
      one 3dQwarp task.
