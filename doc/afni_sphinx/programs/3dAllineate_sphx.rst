***********
3dAllineate
***********

.. _ahelp_3dAllineate:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dAllineate [options] sourcedataset
    
    Program to align one dataset (the 'source') to a base dataset.
    Options are available to control:
     ++ How the matching between the source and the base is computed
        (i.e., the 'cost functional' measuring image mismatch).
     ++ How the resliced source is interpolated to the base space.
     ++ The complexity of the spatial transformation ('warp') used.
     ++ And many many technical options to control the process in detail,
        if you know what you are doing (or just like to fool around).
    
    =====----------------------------------------------------------------------
    NOTES: For most 3D image registration purposes, we now recommend that you
    =====  use Daniel Glen's script align_epi_anat.py (which, despite its name,
           can do many more registration problems than EPI-to-T1-weighted).
      -->> In particular, using 3dAllineate with the 'lpc' cost functional
           (to align EPI and T1-weighted volumes) requires using a '-weight'
           volume to get good results, and the align_epi_anat.py script will
           automagically generate such a weight dataset that works well for
           EPI-to-structural alignment.
      -->> This script can also be used for other alignment purposes, such
           as T1-weighted alignment between field strengths using the
           '-lpa' cost functional.  Investigate align_epi_anat.py to
           see if it will do what you need -- you might make your life
           a little easier and nicer and happier and more tranquil.
      -->> Also, if/when you ask for registration help on the AFNI
           message board, we'll probably start by recommending that you
           try align_epi_anat.py if you haven't already done so.
      -->> For aligning EPI and T1-weighted volumes, we have found that
           using a flip angle of 50-60 degrees for the EPI works better than
           a flip angle of 90 degrees.  The reason is that there is more
           internal contrast in the EPI data when the flip angle is smaller,
           so the registration has some image structure to work with.  With
           the 90 degree flip angle, there is so little internal contrast in
           the EPI dataset that the alignment process ends up being just
           trying to match brain outlines -- which doesn't always give accurate
           results: see http://dx.doi.org/10.1016/j.neuroimage.2008.09.037
      -->> Although the total MRI signal is reduced at a smaller flip angle,
           there is little or no loss in FMRI/BOLD information, since the bulk
           of the time series 'noise' is from physiological fluctuation signals,
           which are also reduced by the lower flip angle -- for more details,
           see http://dx.doi.org/10.1016/j.neuroimage.2010.11.020
    ---------------------------------------------------------------------------
      **** New (Summer 2013) program 3dQwarp is available to do nonlinear  ****
      ***  alignment between a base and source dataset, including the use   ***
      **   of 3dAllineate for the preliminary affine alignment.  If you are  **
      *    interested, see the output of '3dQwarp -help' for the details.     *
    ---------------------------------------------------------------------------
    
    COMMAND LINE OPTIONS:
    ====================
     -base bbb   = Set the base dataset to be the #0 sub-brick of 'bbb'.
                   If no -base option is given, then the base volume is
                   taken to be the #0 sub-brick of the source dataset.
                   (Base must be stored as floats, shorts, or bytes.)
    
     -source ttt = Read the source dataset from 'ttt'.  If no -source
       *OR*        (or -input) option is given, then the source dataset
     -input ttt    is the last argument on the command line.
                   (Source must be stored as floats, shorts, or bytes.)
                ** 3dAllineate can register 2D datasets (single slice),
                   but both the base and source must be 2D -- you cannot
                   use this program to register a 2D slice into a 3D volume!
                ** See the script @2dwarper.Allin for an example of using
                   3dAllineate to do slice-by-slice nonlinear warping to
                   align 3D volumes distorted by time-dependent magnetic
                   field inhomogeneities.
    
     ** NOTA BENE: The base and source dataset do NOT have to be defined **
     ** [that's]   on the same 3D grids; the alignment process uses the  **
     ** [Latin ]   coordinate systems defined in the dataset headers to  **
     ** [  for ]   make the match between spatial locations, rather than **
     ** [ NOTE ]   matching the 2 datasets on a voxel-by-voxel basis     **
     ** [ WELL ]   (as 3dvolreg and 3dWarpDrive do).                     **
     **       -->> However, this coordinate-based matching requires that **
     **            image volumes be defined on roughly the same patch of **
     **            of (x,y,z) space, in order to find a decent starting  **
     **            point for the transformation.  You might need to use  **
     **            the script @Align_Centers to do this, if the 3D       **
     **            spaces occupied by the images do not overlap much.    **
     **       -->> Or the '-cmass' option to this program might be       **
     **            sufficient to solve this problem, maybe, with luck.   **
     **            (Another reason why you should use align_epi_anat.py) **
     **       -->> If the coordinate system in the dataset headers is    **
     **            WRONG, then 3dAllineate will probably not work well!  **
    
     -prefix ppp = Output the resulting dataset to file 'ppp'.  If this
       *OR*        option is NOT given, no dataset will be output!  The
     -out ppp      transformation matrix to align the source to the base will
                   be estimated, but not applied.  You can save the matrix
                   for later use using the '-1Dmatrix_save' option.
            *N.B.: By default, the new dataset is computed on the grid of the
                    base dataset; see the '-master' and/or the '-mast_dxyz'
                    options to change this grid.
            *N.B.: If 'ppp' is 'NULL', then no output dataset will be produced.
                    This option is for compatibility with 3dvolreg.
    
     -floatize   = Write result dataset as floats.  Internal calculations
     -float        are all done on float copies of the input datasets.
                   [Default=convert output dataset to data format of  ]
                   [        source dataset; if the source dataset was ]
                   [        shorts with a scale factor, then the new  ]
                   [        dataset will get a scale factor as well;  ]
                   [        if the source dataset was shorts with no  ]
                   [        scale factor, the result will be unscaled.]
    
     -1Dparam_save ff   = Save the warp parameters in ASCII (.1D) format into
                          file 'ff' (1 row per sub-brick in source).
                        * A historical synonym for this option is '-1Dfile'.
                        * At the top of the saved 1D file is a #comment line
                          listing the names of the parameters; those parameters
                          that are fixed (e.g., via '-parfix') will be marked
                          by having their symbolic names end in the '$' character.
                          You can use '1dcat -nonfixed' to remove these columns
                          from the 1D file if you just want to further process the
                          varying parameters somehow (e.g., 1dsvd).
                        * However, the '-1Dparam_apply' option requires the
                          full list of parameters, including those that were
                          fixed, in order to work properly!
    
     -1Dparam_apply aa  = Read warp parameters from file 'aa', apply them to 
                          the source dataset, and produce a new dataset.
                          (Must also use the '-prefix' option for this to work!  )
                          (In this mode of operation, there is no optimization of)
                          (the cost functional by changing the warp parameters;  )
                          (previously computed parameters are applied directly.  )
                   *N.B.: A historical synonym for this is '-1Dapply'.
                   *N.B.: If you use -1Dparam_apply, you may also want to use
                           -master to control the grid on which the new
                           dataset is written -- the base dataset from the
                           original 3dAllineate run would be a good possibility.
                           Otherwise, the new dataset will be written out on the
                           3D grid coverage of the source dataset, and this
                           might result in clipping off part of the image.
                   *N.B.: Each row in the 'aa' file contains the parameters for
                           transforming one sub-brick in the source dataset.
                           If there are more sub-bricks in the source dataset
                           than there are rows in the 'aa' file, then the last
                           row is used repeatedly.
                   *N.B.: A trick to use 3dAllineate to resample a dataset to
                           a finer grid spacing:
                             3dAllineate -input dataset+orig         \
                                         -master template+orig       \
                                         -prefix newdataset          \
                                         -final wsinc5               \
                                         -1Dparam_apply '1D: 12@0'\'  
                           Here, the identity transformation is specified
                           by giving all 12 affine parameters as 0 (note
                           the extra \' at the end of the '1D: 12@0' input!).
                         ** You can also use the word 'IDENTITY' in place of
                            '1D: 12@0'\' (to indicate the identity transformation).
                  **N.B.: Some expert options for modifying how the wsinc5
                           method works are described far below, if you use
                           '-HELP' instead of '-help'.
                ****N.B.: The interpolation method used to produce a dataset
                           is always given via the '-final' option, NOT via
                           '-interp'.  If you forget this and use '-interp'
                           along with one of the 'apply' options, this program
                           will chastise you (gently) and change '-final'
                           to match what the '-interp' input.
    
     -1Dmatrix_save ff  = Save the transformation matrix for each sub-brick into
                          file 'ff' (1 row per sub-brick in the source dataset).
                          If 'ff' does NOT end in '.1D', then the program will
                          append '.aff12.1D' to 'ff' to make the output filename.
                   *N.B.: This matrix is the coordinate transformation from base
                           to source DICOM coordinates. In other terms:
                              Xin = Xsource = M Xout = M Xbase
                                       or
                              Xout = Xbase = inv(M) Xin = inv(M) Xsource
                           where Xin or Xsource is the 4x1 coordinates of a
                           location in the input volume. Xout is the 
                           coordinate of that same location in the output volume.
                           Xbase is the coordinate of the corresponding location
                           in the base dataset. M is ff augmented by a 4th row of
                           [0 0 0 1], X. is an augmented column vector [x,y,z,1]'
                           To get the inverse matrix inv(M)
                           (source to base), use the cat_matvec program, as in
                             cat_matvec fred.aff12.1D -I
    
     -1Dmatrix_apply aa = Use the matrices in file 'aa' to define the spatial
                          transformations to be applied.  Also see program
                          cat_matvec for ways to manipulate these matrix files.
                   *N.B.: You probably want to use either -base or -master
                          with either *_apply option, so that the coordinate
                          system that the matrix refers to is correctly loaded.
                         ** You can also use the word 'IDENTITY' in place of a
                            filename to indicate the identity transformation --
                            presumably for the purpose of resampling the source
                            dataset to a new grid.
    
      * The -1Dmatrix_* options can be used to save and re-use the transformation *
      * matrices.  In combination with the program cat_matvec, which can multiply *
      * saved transformation matrices, you can also adjust these matrices to      *
      * other alignments.                                                         *
    
      * The script 'align_epi_anat.py' uses 3dAllineate and 3dvolreg to align EPI *
      * datasets to T1-weighted anatomical datasets, using saved matrices between *
      * the two programs.  This script is our currently recommended method for    *
      * doing such intra-subject alignments.                                      *
    
     -cost ccc   = Defines the 'cost' function that defines the matching
                   between the source and the base; 'ccc' is one of
                    ls   *OR*  leastsq         = Least Squares [Pearson Correlation]
                    mi   *OR*  mutualinfo      = Mutual Information [H(b)+H(s)-H(b,s)]
                    crM  *OR*  corratio_mul    = Correlation Ratio (Symmetrized*)
                    nmi  *OR*  norm_mutualinfo = Normalized MI [H(b,s)/(H(b)+H(s))]
                    hel  *OR*  hellinger       = Hellinger metric
                    crA  *OR*  corratio_add    = Correlation Ratio (Symmetrized+)
                    crU  *OR*  corratio_uns    = Correlation Ratio (Unsym)
                   You can also specify the cost functional using an option
                   of the form '-mi' rather than '-cost mi', if you like
                   to keep things terse and cryptic (as I do).
                   [Default == '-hel' (for no good reason, but it sounds nice).]
    
     -interp iii = Defines interpolation method to use during matching
                   process, where 'iii' is one of
                     NN      *OR* nearestneighbour *OR nearestneighbor
                     linear  *OR* trilinear
                     cubic   *OR* tricubic
                     quintic *OR* triquintic
                   Using '-NN' instead of '-interp NN' is allowed (e.g.).
                   Note that using cubic or quintic interpolation during
                   the matching process will slow the program down a lot.
                   Use '-final' to affect the interpolation method used
                   to produce the output dataset, once the final registration
                   parameters are determined.  [Default method == 'linear'.]
                ** N.B.: Linear interpolation is used during the coarse
                         alignment pass; the selection here only affects
                         the interpolation method used during the second
                         (fine) alignment pass.
                ** N.B.: '-interp' does NOT define the final method used
                         to produce the output dataset as warped from the
                         input dataset.  If you want to do that, use '-final'.
    
     -final iii  = Defines the interpolation mode used to create the
                   output dataset.  [Default == 'cubic']
                ** N.B.: For '-final' ONLY, you can use 'wsinc5' to specify
                           that the final interpolation be done using a
                           weighted sinc interpolation method.  This method
                           is so SLOW that you aren't allowed to use it for
                           the registration itself.
                      ++ wsinc5 interpolation is highly accurate and should
                           reduce the smoothing artifacts from lower
                           order interpolation methods (which are most
                           visible if you interpolate an EPI time series
                           to high resolution and then make an image of
                           the voxel-wise variance).
                      ++ On my Intel-based Mac, it takes about 2.5 s to do
                           wsinc5 interpolation, per 1 million voxels output.
                           For comparison, quintic interpolation takes about
                           0.3 s per 1 million voxels: 8 times faster than wsinc5.
                      ++ The '5' refers to the width of the sinc interpolation
                           weights: plus/minus 5 grid points in each direction;
                           this is a tensor product interpolation, for speed.
    
    TECHNICAL OPTIONS (used for fine control of the program):
    =================
     -nmatch nnn = Use at most 'nnn' scattered points to match the
                   datasets.  The smaller nnn is, the faster the matching
                   algorithm will run; however, accuracy may be bad if
                   nnn is too small.  If you end the 'nnn' value with the
                   '%' character, then that percentage of the base's
                   voxels will be used.
                   [Default == 47% of voxels in the weight mask]
    
     -nopad      = Do not use zero-padding on the base image.
                   [Default == zero-pad, if needed; -verb shows how much]
    
     -zclip      = Replace negative values in the input datasets (source & base)
                   with zero.  The intent is to clip off a small set of negative
                   values that may arise when using 3dresample (say) with
                   cubic interpolation.
    
     -conv mmm   = Convergence test is set to 'mmm' millimeters.
                   This doesn't mean that the results will be accurate
                   to 'mmm' millimeters!  It just means that the program
                   stops trying to improve the alignment when the optimizer
                   (NEWUOA) reports it has narrowed the search radius
                   down to this level.  [Default == 0.05 mm]
    
     -verb       = Print out verbose progress reports.
                   [Using '-VERB' will give even more prolix reports.]
     -quiet      = Don't print out verbose stuff.
     -usetemp    = Write intermediate stuff to disk, to economize on RAM.
                   Using this will slow the program down, but may make it
                   possible to register datasets that need lots of space.
           **N.B.: Temporary files are written to the directory given
                   in environment variable TMPDIR, or in /tmp, or in ./
                   (preference in that order).  If the program crashes,
                   these files are named TIM_somethingrandom, and you
                   may have to delete them manually. (TIM=Temporary IMage)
           **N.B.: If the program fails with a 'malloc failure' type of
                   message, then try '-usetemp' (malloc=memory allocator).
           **N.B.: If you use '-verb', then memory usage is printed out
                   at various points along the way.
     -nousetemp  = Don't use temporary workspace on disk [the default].
    
     -check hhh  = After cost functional optimization is done, start at the
                   final parameters and RE-optimize using the new cost
                   function 'hhh'.  If the results are too different, a
                   warning message will be printed.  However, the final
                   parameters from the original optimization will be
                   used to create the output dataset. Using '-check'
                   increases the CPU time, but can help you feel sure
                   that the alignment process did not go wild and crazy.
                   [Default == no check == don't worry, be happy!]
           **N.B.: You can put more than one function after '-check', as in
                     -nmi -check mi hel crU crM
                   to register with Normalized Mutual Information, and
                   then check the results against 4 other cost functionals.
           **N.B.: On the other hand, some cost functionals give better
                   results than others for specific problems, and so
                   a warning that 'mi' was significantly different than
                   'hel' might not actually mean anything useful (e.g.).
    
     ** PARAMETERS THAT AFFECT THE COST OPTIMIZATION STRATEGY **
     -onepass    = Use only the refining pass -- do not try a coarse
                   resolution pass first.  Useful if you know that only
                   small amounts of image alignment are needed.
                   [The default is to use both passes.]
     -twopass    = Use a two pass alignment strategy, first searching for
                   a large rotation+shift and then refining the alignment.
                   [Two passes are used by default for the first sub-brick]
                   [in the source dataset, and then one pass for the others.]
                   ['-twopass' will do two passes for ALL source sub-bricks.]
     -twoblur rr = Set the blurring radius for the first pass to 'rr'
                   millimeters.  [Default == 11 mm]
           **N.B.: You may want to change this from the default if
                   your voxels are unusually small or unusually large
                   (e.g., outside the range 1-4 mm along each axis).
     -twofirst   = Use -twopass on the first image to be registered, and
                   then on all subsequent images from the source dataset,
                   use results from the first image's coarse pass to start
                   the fine pass.
                   (Useful when there may be large motions between the   )
                   (source and the base, but only small motions within   )
                   (the source dataset itself; since the coarse pass can )
                   (be slow, doing it only once makes sense in this case.)
           **N.B.: [-twofirst is on by default; '-twopass' turns it off.]
     -twobest bb = In the coarse pass, use the best 'bb' set of initial
                   points to search for the starting point for the fine
                   pass.  If bb==0, then no search is made for the best
                   starting point, and the identity transformation is
                   used as the starting point.  [Default=5; min=0 max=22]
           **N.B.: Setting bb=0 will make things run faster, but less reliably.
     -fineblur x = Set the blurring radius to use in the fine resolution
                   pass to 'x' mm.  A small amount (1-2 mm?) of blurring at
                   the fine step may help with convergence, if there is
                   some problem, especially if the base volume is very noisy.
                   [Default == 0 mm = no blurring at the final alignment pass]
       **NOTES ON
       **STRATEGY: * If you expect only small-ish (< 2 voxels?) image movement,
                     then using '-onepass' or '-twobest 0' makes sense.
                   * If you expect large-ish image movements, then do not
                     use '-onepass' or '-twobest 0'; the purpose of the
                     '-twobest' parameter is to search for large initial
                     rotations/shifts with which to start the coarse
                     optimization round.
                   * If you have multiple sub-bricks in the source dataset,
                     then the default '-twofirst' makes sense if you don't expect
                     large movements WITHIN the source, but expect large motions
                     between the source and base.
                   * '-twopass' re-starts the alignment process for each sub-brick
                     in the source dataset -- this option can be time consuming,
                     and is really intended to be used when you might expect large
                     movements between sub-bricks; for example, when the different
                     volumes are gathered on different days.  For most purposes,
                     '-twofirst' (the default process) will be adequate and faster,
                     when operating on multi-volume source datasets.
    
     -cmass        = Use the center-of-mass calculation to bracket the shifts.
                       [This option is OFF by default]
                     If given in the form '-cmass+xy' (for example), means to
                     do the CoM calculation in the x- and y-directions, but
                     not the z-direction.
     -nocmass      = Don't use the center-of-mass calculation. [The default]
                      (You would not want to use the C-o-M calculation if the  )
                      (source sub-bricks have very different spatial locations,)
                      (since the source C-o-M is calculated from all sub-bricks)
     **EXAMPLE: You have a limited coverage set of axial EPI slices you want to
                register into a larger head volume (after 3dSkullStrip, of course).
                In this case, '-cmass+xy' makes sense, allowing CoM adjustment
                along the x = R-L and y = A-P directions, but not along the
                z = I-S direction, since the EPI doesn't cover the whole brain
                along that axis.
    
     -autoweight = Compute a weight function using the 3dAutomask
                   algorithm plus some blurring of the base image.
           **N.B.: '-autoweight+100' means to zero out all voxels
                     with values below 100 before computing the weight.
                   '-autoweight**1.5' means to compute the autoweight
                     and then raise it to the 1.5-th power (e.g., to
                     increase the weight of high-intensity regions).
                   These two processing steps can be combined, as in
                     '-autoweight+100**1.5'
                   ** Note that that '**' must be enclosed in quotes;
                      otherwise, the shell will treat it as a wildcard
                      and you will get an error message before 3dAllineate
                      even starts!!
           **N.B.: Some cost functionals do not allow -autoweight, and
                   will use -automask instead.  A warning message
                   will be printed if you run into this situation.
                   If a clip level '+xxx' is appended to '-autoweight',
                   then the conversion into '-automask' will NOT happen.
                   Thus, using a small positive '+xxx' can be used trick
                   -autoweight into working on any cost functional.
     -automask   = Compute a mask function, which is like -autoweight,
                   but the weight for a voxel is set to either 0 or 1.
           **N.B.: '-automask+3' means to compute the mask function, and
                   then dilate it outwards by 3 voxels (e.g.).
                   ** Note that '+' means something very different
                      for '-automask' and '-autoweight'!!
     -autobox    = Expand the -automask function to enclose a rectangular
                   box that holds the irregular mask.
           **N.B.: This is the default mode of operation!
                   For intra-modality registration, '-autoweight' may be better!
                 * If the cost functional is 'ls', then '-autoweight' will be
                   the default, instead of '-autobox'.
     -nomask     = Don't compute the autoweight/mask; if -weight is not
                   also used, then every voxel will be counted equally.
     -weight www = Set the weighting for each voxel in the base dataset;
                   larger weights mean that voxel counts more in the cost
                   function.
           **N.B.: The weight dataset must be defined on the same grid as
                   the base dataset.
           **N.B.: Even if a method does not allow -autoweight, you CAN
                   use a weight dataset that is not 0/1 valued.  The
                   risk is yours, of course (!*! as always in AFNI !*!).
     -wtprefix p = Write the weight volume to disk as a dataset with
                   prefix name 'p'.  Used with '-autoweight/mask', this option
                   lets you see what voxels were important in the algorithm.
     -emask ee   = This option lets you specify a mask of voxels to EXCLUDE from
                   the analysis. The voxels where the dataset 'ee' is nonzero
                   will not be included (i.e., their weights will be set to zero).
                 * Like all the weight options, it applies in the base image
                   coordinate system.
                 * Like all the weight options, it means nothing if you are using
                   one of the 'apply' options.
    
        Method  Allows -autoweight
        ------  ------------------
         ls     YES
         mi     NO
         crM    YES
         nmi    NO
         hel    NO
         crA    YES
         crU    YES
    
     -source_mask sss = Mask the source (input) dataset, using 'sss'.
     -source_automask = Automatically mask the source dataset.
                          [By default, all voxels in the source]
                          [dataset are used in the matching.   ]
                **N.B.: You can also use '-source_automask+3' to dilate
                        the default source automask outward by 3 voxels.
    
     -warp xxx   = Set the warp type to 'xxx', which is one of
                     shift_only         *OR* sho =  3 parameters
                     shift_rotate       *OR* shr =  6 parameters
                     shift_rotate_scale *OR* srs =  9 parameters
                     affine_general     *OR* aff = 12 parameters
                   [Default = affine_general, which includes image]
                   [      shifts, rotations, scaling, and shearing]
    
     -warpfreeze = Freeze the non-rigid body parameters (those past #6)
                   after doing the first sub-brick.  Subsequent volumes
                   will have the same spatial distortions as sub-brick #0,
                   plus rigid body motions only.
    
     -replacebase   = If the source has more than one sub-brick, and this
                      option is turned on, then after the #0 sub-brick is
                      aligned to the base, the aligned #0 sub-brick is used
                      as the base image for subsequent source sub-bricks.
    
     -replacemeth m = After sub-brick #0 is aligned, switch to method 'm'
                      for later sub-bricks.  For use with '-replacebase'.
    
     -EPI        = Treat the source dataset as being composed of warped
                   EPI slices, and the base as comprising anatomically
                   'true' images.  Only phase-encoding direction image
                   shearing and scaling will be allowed with this option.
           **N.B.: For most people, the base dataset will be a 3dSkullStrip-ed
                   T1-weighted anatomy (MPRAGE or SPGR).  If you don't remove
                   the skull first, the EPI images (which have little skull
                   visible due to fat-suppression) might expand to fit EPI
                   brain over T1-weighted skull.
           **N.B.: Usually, EPI datasets don't have as complete slice coverage
                   of the brain as do T1-weighted datasets.  If you don't use
                   some option (like '-EPI') to suppress scaling in the slice-
                   direction, the EPI dataset is likely to stretch the slice
                   thicknesss to better 'match' the T1-weighted brain coverage.
           **N.B.: '-EPI' turns on '-warpfreeze -replacebase'.
                   You can use '-nowarpfreeze' and/or '-noreplacebase' AFTER the
                   '-EPI' on the command line if you do not want these options used.
    
     -parfix n v   = Fix parameter #n to be exactly at value 'v'.
     -parang n b t = Allow parameter #n to range only between 'b' and 't'.
                     If not given, default ranges are used.
     -parini n v   = Initialize parameter #n to value 'v', but then
                     allow the algorithm to adjust it.
             **N.B.: Multiple '-par...' options can be used, to constrain
                     multiple parameters.
             **N.B.: -parini has no effect if -twopass is used, since
                     the -twopass algorithm carries out its own search
                     for initial parameters.
    
     -maxrot dd    = Allow maximum rotation of 'dd' degrees.  Equivalent
                     to '-parang 4 -dd dd -parang 5 -dd dd -parang 6 -dd dd'
                     [Default=30 degrees]
     -maxshf dd    = Allow maximum shift of 'dd' millimeters.  Equivalent
                     to '-parang 1 -dd dd -parang 2 -dd dd -parang 3 -dd dd'
                     [Default=32% of the size of the base image]
             **N.B.: This max shift setting is relative to the center-of-mass
                     shift, if the '-cmass' option is used.
     -maxscl dd    = Allow maximum scaling factor to be 'dd'.  Equivalent
                     to '-parang 7 1/dd dd -parang 8 1/dd dd -paran2 9 1/dd dd'
                     [Default=1.2=image can go up or down 20% in size]
     -maxshr dd    = Allow maximum shearing factor to be 'dd'. Equivalent
                     to '-parang 10 -dd dd -parang 11 -dd dd -parang 12 -dd dd'
                     [Default=0.1111 for no good reason]
    
     NOTE: If the datasets being registered have only 1 slice, 3dAllineate
           will automatically fix the 6 out-of-plane motion parameters to
           their 'do nothing' values, so you don't have to specify '-parfix'.
    
     -master mmm = Write the output dataset on the same grid as dataset
                   'mmm'.  If this option is NOT given, the base dataset
                   is the master.
           **N.B.: 3dAllineate transforms the source dataset to be 'similar'
                   to the base image.  Therefore, the coordinate system
                   of the master dataset is interpreted as being in the
                   reference system of the base image.  It is thus vital
                   that these finite 3D volumes overlap, or you will lose data!
           **N.B.: If 'mmm' is the string 'SOURCE', then the source dataset
                   is used as the master for the output dataset grid.
                   You can also use 'BASE', which is of course the default.
    
     -mast_dxyz del = Write the output dataset using grid spacings of
      *OR*            'del' mm.  If this option is NOT given, then the
     -newgrid del     grid spacings in the master dataset will be used.
                      This option is useful when registering low resolution
                      data (e.g., EPI time series) to high resolution
                      datasets (e.g., MPRAGE) where you don't want to
                      consume vast amounts of disk space interpolating
                      the low resolution data to some artificially fine
                      (and meaningless) spatial grid.
    
    ----------------------------------------------
    DEFINITION OF AFFINE TRANSFORMATION PARAMETERS
    ----------------------------------------------
    The 3x3 spatial transformation matrix is calculated as [S][D][U],
    where [S] is the shear matrix,
          [D] is the scaling matrix, and
          [U] is the rotation (proper orthogonal) matrix.
    Thes matrices are specified in DICOM-ordered (x=-R+L,y=-A+P,z=-I+S)
    coordinates as:
    
      [U] = [Rotate_y(param#6)] [Rotate_x(param#5)] [Rotate_z(param #4)]
            (angles are in degrees)
    
      [D] = diag( param#7 , param#8 , param#9 )
    
            [    1        0     0 ]        [ 1 param#10 param#11 ]
      [S] = [ param#10    1     0 ]   OR   [ 0    1     param#12 ]
            [ param#11 param#12 1 ]        [ 0    0        1     ]
    
    The shift vector comprises parameters #1, #2, and #3.
    
    The goal of the program is to find the warp parameters such that
       I([x]_warped) 'is similar to' J([x]_in)
    as closely as possible in some sense of 'similar', where J(x) is the
    base image, and I(x) is the source image.
    
    Using '-parfix', you can specify that some of these parameters
    are fixed.  For example, '-shift_rotate_scale' is equivalent
    '-affine_general -parfix 10 0 -parfix 11 0 -parfix 12 0'.
    Don't even think of using the '-parfix' option unless you grok
    this example!
    
    ----------- Special Note for the '-EPI' Option's Coordinates -----------
    In this case, the parameters above are with reference to coordinates
      x = frequency encoding direction (by default, first axis of dataset)
      y = phase encoding direction     (by default, second axis of dataset)
      z = slice encoding direction     (by default, third axis of dataset)
    This option lets you freeze some of the warping parameters in ways that
    make physical sense, considering how echo-planar images are acquired.
    The x- and z-scaling parameters are disabled, and shears will only affect
    the y-axis.  Thus, there will be only 9 free parameters when '-EPI' is
    used.  If desired, you can use a '-parang' option to allow the scaling
    fixed parameters to vary (put these after the '-EPI' option):
      -parang 7 0.833 1.20     to allow x-scaling
      -parang 9 0.833 1.20     to allow z-scaling
    You could also fix some of the other parameters, if that makes sense
    in your situation; for example, to disable out-of-slice rotations:
      -parfix 5 0  -parfix 6 0
    and to disable out of slice translation:
      -parfix 3 0
    NOTE WELL: If you use '-EPI', then the output warp parameters (e.g., in
               '-1Dparam_save') apply to the (freq,phase,slice) xyz coordinates,
               NOT to the DICOM xyz coordinates, so equivalent transformations
               will be expressed with different sets of parameters entirely
               than if you don't use '-EPI'!  This comment does NOT apply
               to the output of '-1Dmatrix_save', since that matrix is
               defined relative to the RAI (DICOM) spatial coordinates.
    
    *********** CHANGING THE ORDER OF MATRIX APPLICATION ***********
    
      -SDU or -SUD }= Set the order of the matrix multiplication
      -DSU or -DUS }= for the affine transformations:
      -USD or -UDS }=   S = triangular shear (params #10-12)
                        D = diagonal scaling matrix (params #7-9)
                        U = rotation matrix (params #4-6)
                      Default order is '-SDU', which means that
                      the U matrix is applied first, then the
                      D matrix, then the S matrix.
    
      -Supper      }= Set the S matrix to be upper or lower
      -Slower      }= triangular [Default=lower triangular]
    
      -ashift OR   }= Apply the shift parameters (#1-3) after OR
      -bshift      }= before the matrix transformation. [Default=after]
    
                ==================================================
            ===== RWCox - September 2006 - Live Long and Prosper =====
                ==================================================
    
             ********************************************************
            *** From Webster's Dictionary: Allineate == 'to align' ***
             ********************************************************
    
    ===========================================================================
                           FORMERLY SECRET HIDDEN OPTIONS
    ---------------------------------------------------------------------------
            ** N.B.: Most of these are experimental! [permanent beta] **
    ===========================================================================
    
     -num_rtb n  = At the beginning of the fine pass, the best set of results
                   from the coarse pass are 'refined' a little by further
                   optimization, before the single best one is chosen for
                   for the final fine optimization.
                  * This option sets the maximum number of cost functional
                    evaluations to be used (for each set of parameters)
                    in this step.
                  * The default is 99; a larger value will take more CPU
                    time but may give more robust results.
                  * If you want to skip this step entirely, use '-num_rtb 0'.
                    then, the best of the coarse pass results is taken
                    straight to the final optimization passes.
           **N.B.: If you use '-VERB', you will see that one extra case
                   is involved in this initial fine refinement step; that
                   case is starting with the identity transformation, which
                   helps insure against the chance that the coarse pass
                   optimizations ran totally amok.
     -nocast     = By default, parameter vectors that are too close to the
                   best one are cast out at the end of the coarse pass
                   refinement process. Use this option if you want to keep
                   them all for the fine resolution pass.
     -norefinal  = Do NOT re-start the fine iteration step after it
                   has converged.  The default is to re-start it, which
                   usually results in a small improvement to the result
                   (at the cost of CPU time).  This re-start step is an
                   an attempt to avoid a local minimum trap.  It is usually
                   not necessary, but sometimes helps.
    
     -realaxes   = Use the 'real' axes stored in the dataset headers, if they
                   conflict with the default axes.  [For Jedi AFNI Masters only!]
    
     -savehist sss = Save start and final 2D histograms as PGM
                     files, with prefix 'sss' (cost: cr mi nmi hel).
                    * if filename contains 'FF', floats is written
                    * these are the weighted histograms!
                    * -savehist will also save histogram files when
                      the -allcost evaluations takes place
                    * this option is mostly useless unless '-histbin' is
                      also used
     -median       = Smooth with median filter instead of Gaussian blur.
                     (Somewhat slower, and not obviously useful.)
     -powell m a   = Set the Powell NEWUOA dimensional parameters to
                     'm' and 'a' (cf. source code in powell_int.c).
                     The number of points used for approximating the
                     cost functional is m*N+a, where N is the number
                     of parameters being optimized.  The default values
                     are m=2 and a=3.  Larger values will probably slow
                     the program down for no good reason.  The smallest
                     allowed values are 1.
     -target ttt   = Same as '-source ttt'.  In the earliest versions,
                     what I now call the 'source' dataset was called the
                     'target' dataset:
                        Try to remember the kind of September (2006)
                        When life was slow and oh so mellow
                        Try to remember the kind of September
                        When grass was green and source was target.
     -Xwarp       =} Change the warp/matrix setup so that only the x-, y-, or z-
     -Ywarp       =} axis is stretched & sheared.  Useful for EPI, where 'X',
     -Zwarp       =} 'Y', or 'Z' corresponds to the phase encoding direction.
     -FPS fps      = Generalizes -EPI to arbitrary permutation of directions.
     -histpow pp   = By default, the number of bins in the histogram used
                     for calculating the Hellinger, Mutual Information, and
                     Correlation Ratio statistics is n^(1/3), where n is
                     the number of data points.  You can change that exponent
                     to 'pp' with this option.
     -histbin nn   = Or you can just set the number of bins directly to 'nn'.
     -eqbin   nn   = Use equalized marginal histograms with 'nn' bins.
     -clbin   nn   = Use 'nn' equal-spaced bins except for the bot and top,
                     which will be clipped (thus the 'cl').  If nn is 0, the
                     program will pick the number of bins for you.
                     **N.B.: '-clbin 0' is now the default [25 Jul 2007];
                             if you want the old all-equal-spaced bins, use
                             '-histbin 0'.
                     **N.B.: '-clbin' only works when the datasets are
                             non-negative; any negative voxels in either
                             the input or source volumes will force a switch
                             to all equal-spaced bins.
     -wtmrad  mm   = Set autoweight/mask median filter radius to 'mm' voxels.
     -wtgrad  gg   = Set autoweight/mask Gaussian filter radius to 'gg' voxels.
     -nmsetup nn   = Use 'nn' points for the setup matching [default=98756]
     -ignout       = Ignore voxels outside the warped source dataset.
    
     -blok bbb     = Blok definition for the 'lp?' (Local Pearson) cost
                     functions: 'bbb' is one of
                       'BALL(r)' or 'CUBE(r)' or 'RHDD(r)' or 'TOHD(r)'
                     corresponding to
                       spheres or cubes or rhombic dodecahedra or
                       truncated octahedra
                     where 'r' is the size parameter in mm.
                     [Default is 'RHDD(6.54321)' (rhombic dodecahedron)]
    
     -allcost        = Compute ALL available cost functionals and print them
                       at various points.
     -allcostX       = Compute and print ALL available cost functionals for the
                       un-warped inputs, and then quit.
     -allcostX1D p q = Compute ALL available cost functionals for the set of
                       parameters given in the 1D file 'p' (12 values per row),
                       write them to the 1D file 'q', then exit. (For you, Zman)
                      * N.B.: If -fineblur is used, that amount of smoothing
                              will be applied prior to the -allcostX evaluations.
                              The parameters are the rotation, shift, scale,
                              and shear values, not the affine transformation
                              matrix. An identity matrix could be provided as
                              "0 0 0  0 0 0  1 1 1  0 0 0" for instance or by
                              using the word "IDENTITY"
    
    ===========================================================================
    
    Modifying '-final wsinc5'
    -------------------------
     * The windowed (tapered) sinc function interpolation can be modified
         by several environment variables.  This is expert-level stuff, and
         you should understand what you are doing if you use these options.
         The simplest way to use these would be on the command line, as in
           -DAFNI_WSINC5_RADIUS=9 -DAFNI_WSINC5_TAPERFUN=Hamming
    
     * AFNI_WSINC5_TAPERFUN lets you choose the taper function.
         The default taper function is the minimum sidelobe 3-term cosine:
           0.4243801 + 0.4973406*cos(PI*x) + 0.0782793*cos(2*PI*x)
         If you set this environment variable to 'Hamming', then the
         minimum sidelobe 2-term cosine will be used instead:
           0.53836 + 0.46164*cos(PI*x)
         Here, 'x' is between 0 and 1, where x=0 is the center of the
         interpolation mask and x=1 is the outer edge.
     ++  Unfortunately, the 3-term cosine doesn't have a catchy name; you can
           find it (and many other) taper functions described in the paper
             AH Nuttall, Some Windows with Very Good Sidelobe Behavior.
             IEEE Trans. ASSP, 29:84-91 (1981).
           In particular, see Fig.14 and Eq.36 in this paper.
    
     * AFNI_WSINC5_TAPERCUT lets you choose the start 'x' point for tapering:
         This value should be between 0 and 0.8; for example, 0 means to taper
         all the way from x=0 to x=1 (maximum tapering).  The default value
         is 0.  Setting TAPERCUT to 0.5 (say) means only to taper from x=0.5
         to x=1; thus, a larger value means that fewer points are tapered
         inside the interpolation mask.
    
     * AFNI_WSINC5_RADIUS lets you choose the radius of the tapering window
         (i.e., the interpolation mask region).  This value is an integer
         between 3 and 21.  The default value is 5 (which used to be the
         ONLY value, thus 'wsinc5').  RADIUS is measured in voxels, not mm.
    
     * AFNI_WSINC5_SPHERICAL lets you choose the shape of the mask region.
         If you set this value to 'Yes', then the interpolation mask will be
         spherical; otherwise, it defaults to cubical.
    
     * The Hamming taper function is a little faster than the 3-term function,
         but will have a little more Gibbs phenomenon.
     * A larger TAPERCUT will give a little more Gibbs phenomenon; compute
         speed won't change much with this parameter.
     * Compute time goes up with (at least) the 3rd power of the RADIUS; setting
         RADIUS to 21 will be VERY slow.
     * Visually, RADIUS=3 is similar to quintic interpolation.  Increasing
         RADIUS makes the interpolated images look sharper and more well-
         defined.  However, values of RADIUS greater than or equal to 7 appear
         (to Zhark's eagle eye) to be almost identical.  If you really care,
         you'll have to experiment with this parameter yourself.
     * A spherical mask is also VERY slow, since the cubical mask allows
         evaluation as a tensor product.  There is really no good reason
         to use a spherical mask; I only put it in for experimental purposes.
    ** For most users, there is NO reason to ever use these environment variables
         to modify wsinc5.  You should only do this kind of thing if you have a
         good and articulable reason!  (Or if you really like to screw around.)
    ** The wsinc5 interpolation function is parallelized using OpenMP, which
         makes its usage moderately tolerable.
    
    ===========================================================================
    
    Hidden experimental cost functionals:
    -------------------------------------
       sp   *OR*  spearman        = Spearman [rank] Correlation
       je   *OR*  jointentropy    = Joint Entropy [H(b,s)]
       lss  *OR*  signedPcor      = Signed Pearson Correlation
       lpc  *OR*  localPcorSigned = Local Pearson Correlation Signed
       lpa  *OR*  localPcorAbs    = Local Pearson Correlation Abs
       lpc+ *OR*  localPcor+Others= Local Pearson Signed + Others
       ncd  *OR*  NormCompDist    = Normalized Compression Distance
    
    Notes for the new [Feb 2010] lpc+ cost functional:
    --------------------------------------------------
     * The cost functional named 'lpc+' is a combination of several others:
         lpc + hel*0.4 + crA*0.4 + nmi*0.2 + mi*0.2 + ov*0.4
       ++ 'hel', 'crA', 'nmi', and 'mi' are the histogram-based cost
          functionals also available as standalone options.
       ++ 'ov' is a measure of the overlap of the automasks of the base and
          source volumes; ov is not available as a standalone option.
     * The purpose of lpc+ is to avoid situations where the pure lpc cost
       goes wild; this especially happens if '-source_automask' isn't used.
       ++ Even with lpc+, you should use '-source_automask+2' (say) to be safe.
     * You can alter the weighting of the extra functionals by giving the
       option in the form (for example)
         '-lpc+hel*0.5+nmi*0+mi*0+crA*1.0+ov*0.5'
     * The quotes are needed to prevent the shell from wild-card expanding
       the '*' character.
       --> You can now use ':' in place of '*' to avoid this wildcard problem:
             -lpc+hel:0.5+nmi:0+mi:0+crA:1+ov:0.5+ZZ
     * Notice the weight factors FOLLOW the name of the extra functionals.
       ++ If you want a weight to be 0 or 1, you have to provide for that
          explicitly -- if you leave a weight off, then it will get its
          default value!
       ++ The order of the weight factor names is unimportant here:
            '-lpc+hel*0.5+nmi*0.8' == '-lpc+nmi*0.8+hel*0.5'
     * Only the 5 functionals listed (hel,crA,nmi,mi,ov) can be used in '-lpc+'.
     * In addition, if you want the initial alignments to be with '-lpc+' and
       then finish the Final alignment with pure '-lpc', you can indicate this
       by putting 'ZZ' somewhere in the option string, as in '-lpc+ZZ'.
     * This stuff should be considered really experimental at this moment!
    
    Cost functional descriptions (for use with -allcost output):
    ------------------------------------------------------------
       ls  :: 1 - abs(Pearson correlation coefficient)
       sp  :: 1 - abs(Spearman correlation coefficient)
       mi  :: - Mutual Information = H(base,source)-H(base)-H(source)
       crM :: 1 - abs[ CR(base,source) * CR(source,base) ]
       nmi :: 1/Normalized MI = H(base,source)/[H(base)+H(source)]
       je  :: H(base,source) = joint entropy of image pair
       hel :: - Hellinger distance(base,source)
       crA :: 1 - abs[ CR(base,source) + CR(source,base) ]
       crU :: CR(source,base) = Var(source|base) / Var(source)
       lss :: Pearson correlation coefficient between image pair
       lpc :: nonlinear average of Pearson cc over local neighborhoods
       lpa :: 1 - abs(lpc)
       lpc+:: lpc + hel + mi + nmi + crA + overlap
       ncd :: mutual compressibility (via zlib) -- doesn't work yet
    
     * N.B.: Some cost functional values (as printed out above)
       are negated from their theoretical descriptions (e.g., 'hel')
       so that the best image alignment will be found when the cost
       is minimized.  See the descriptions above and the references
       below for more details for each functional.
    
     * For more information about the 'lpc' functional, see
         ZS Saad, DR Glen, G Chen, MS Beauchamp, R Desai, RW Cox.
           A new method for improving functional-to-structural
           MRI alignment using local Pearson correlation.
           NeuroImage 44: 839-848, 2009.
         http://dx.doi.org/10.1016/j.neuroimage.2008.09.037
         https://afni.nimh.nih.gov/sscc/rwcox/papers/LocalPearson2009.pdf
       The '-blok' option can be used to control the regions
       (size and shape) used to compute the local correlations.
     *** Using the 'lpc' functional wisely requires the use of
         a proper weight volume.  We HIGHLY recommend you use
         the align_epi_anat.py script if you want to use this
         cost functional!  Otherwise, you are likely to get
         less than optimal results (and then swear at us unjustly).
    
     * For more information about the 'cr' functionals, see
         http://en.wikipedia.org/wiki/Correlation_ratio
       Note that CR(x,y) is not the same as CR(y,x), which
       is why there are symmetrized versions of it available.
    
     * For more information about the 'mi', 'nmi', and 'je'
       cost functionals, see
         http://en.wikipedia.org/wiki/Mutual_information
         http://en.wikipedia.org/wiki/Joint_entropy
         http://www.cs.jhu.edu/~cis/cista/746/papers/mutual_info_survey.pdf
    
     * For more information about the 'hel' functional, see
         http://en.wikipedia.org/wiki/Hellinger_distance
    
     * Some cost functionals (e.g., 'mi', 'cr', 'hel') are
       computed by creating a 2D joint histogram of the
       base and source image pair.  Various options above
       (e.g., '-histbin', etc.) can be used to control the
       number of bins used in the histogram on each axis.
       (If you care to control the program in such detail!)
    
     * Minimization of the chosen cost functional is done via
       the NEWUOA software, described in detail in
         MJD Powell. 'The NEWUOA software for unconstrained
           optimization without derivatives.' In: GD Pillo,
           M Roma (Eds), Large-Scale Nonlinear Optimization.
           Springer, 2006.
         http://www.damtp.cam.ac.uk/user/na/NA_papers/NA2004_08.pdf
    
    ===========================================================================
    
     -nwarp type = Experimental nonlinear warping:
    
                  ***** Note that these '-nwarp' options are superseded  *****
                  ***** by the AFNI program 3dQwarp,  which does a more  *****
                  ***** accurate and better and job of nonlinear warping *****
                  ***** ------ Zhark the Warper ------ July 2013 ------- *****
    
                  * At present, the only 'type' is 'bilinear',
                    as in 3dWarpDrive, with 39 parameters.
                  * I plan to implement more complicated nonlinear
                    warps in the future, someday ....
                  * -nwarp can only be applied to a source dataset
                    that has a single sub-brick!
                  * -1Dparam_save and -1Dparam_apply work with
                    bilinear warps; see the Notes for more information.
            ==>>*** Nov 2010: I have now added the following polynomial
                    warps: 'cubic', 'quintic', 'heptic', 'nonic' (using
                    3rd, 5th, 7th, and 9th order Legendre polynomials); e.g.,
                       -nwarp heptic
                  * These are the nonlinear warps that I now am supporting.
                  * Or you can call them 'poly3', 'poly5', 'poly7', and 'poly9',
                      for simplicity and non-Hellenistic clarity.
                  * These names are not case sensitive: 'nonic' == 'Nonic', etc.
                  * Higher and higher order polynomials will take longer and longer
                    to run!
                  * If you wish to apply a nonlinear warp, you have to supply
                    a parameter file with -1Dparam_apply and also specify the
                    warp type with -nwarp.  The number of parameters in the
                    file (per line) must match the warp type:
                       bilinear =  43   [for all nonlinear warps, the final]
                       cubic    =  64   [4 'parameters' are fixed values to]
                       quintic  = 172   [normalize the coordinates to -1..1]
                       heptic   = 364   [for the nonlinear warp functions. ]
                       nonic    = 664
                    In all these cases, the first 12 parameters are the
                    affine parameters (shifts, rotations, etc.), and the
                    remaining parameters define the nonlinear part of the warp
                    (polynomial coefficients); thus, the number of nonlinear
                    parameters over which the optimization takes place is
                    the number in the table above minus 16.
                   * The actual polynomial functions used are products of
                     Legendre polynomials, but the symbolic names used in
                     the header line in the '-1Dparam_save' output just
                     express the polynomial degree involved; for example,
                          quint:x^2*z^3:z
                     is the name given to the polynomial warp basis function
                     whose highest power of x is 2, is independent of y, and
                     whose highest power of z is 3; the 'quint' indicates that
                     this was used in '-nwarp quintic'; the final ':z' signifies
                     that this function was for deformations in the (DICOM)
                     z-direction (+z == Superior).
            ==>>*** You can further control the form of the polynomial warps
                    (but not the bilinear warp!) by restricting their degrees
                    of freedom in 2 different ways.
                    ++ You can remove the freedom to have the nonlinear
                       deformation move along the DICOM x, y, and/or z axes.
                    ++ You can remove the dependence of the nonlinear
                       deformation on the DICOM x, y, and/or z coordinates.
                    ++ To illustrate with the six second order polynomials:
                          p2_xx(x,y,z) = x*x  p2_xy(x,y,z) = x*y
                          p2_xz(x,y,z) = x*z  p2_yy(x,y,z) = y*y
                          p2_yz(x,y,z) = y*z  p2_zz(x,y,z) = z*z
                       Unrestricted, there are 18 parameters associated with
                       these polynomials, one for each direction of motion (x,y,z)
                       * If you remove the freedom of the nonlinear warp to move
                         data in the z-direction (say), then there would be 12
                         parameters left.
                       * If you instead remove the freedom of the nonlinear warp
                         to depend on the z-coordinate, you would be left with
                         3 basis functions (p2_xz, p2_yz, and p2_zz would be
                         eliminated), each of which would have x-motion, y-motion,
                         and z-motion parameters, so there would be 9 parameters.
                    ++ To fix motion along the x-direction, use the option
                       '-nwarp_fixmotX' (and '-nwarp_fixmotY' and '-nwarp_fixmotZ).
                    ++ To fix dependence of the polynomial warp on the x-coordinate,
                       use the option '-nwarp_fixdepX' (et cetera).
                    ++ These coordinate labels in the options (X Y Z) refer to the
                       DICOM directions (X=R-L, Y=A-P, Z=I-S).  If you would rather
                       fix things along the dataset storage axes, you can use
                       the symbols I J K to indicate the fastest to slowest varying
                       array dimensions (e.g., '-nwarp_fixdepK').
                       * Mixing up the X Y Z and I J K forms of parameter freezing
                         (e.g., '-nwarp_fixmotX -nwarp_fixmotJ') may cause trouble!
                    ++ If you input a 2D dataset (a single slice) to be registered
                       with '-nwarp', the program automatically assumes '-nwarp_fixmotK'
                       and '-nwarp_fixdepK' so there are no out-of-plane parameters
                       or dependence.  The number of nonlinear parameters is then:
                         2D: cubic = 14 ; quintic =  36 ; heptic =  66 ; nonic = 104.
                         3D: cubic = 48 ; quintic = 156 ; heptic = 348 ; nonic = 648.
                         [ n-th order: 2D = (n+4)*(n-1) ; 3D = (n*n+7*n+18)*(n-1)/2 ]
                    ++ Note that these '-nwarp_fix' options have no effect on the
                       affine part of the warp -- if you want to constrain that as
                       well, you'll have to use the '-parfix' option.
                       * However, for 2D images, the affine part will automatically
                         be restricted to in-plane (6 parameter) 'motions'.
                    ++ If you save the warp parameters (with '-1Dparam_save') when
                       doing 2D registration, all the parameters will be saved, even
                       the large number of them that are fixed to zero. You can use
                       '1dcat -nonfixed' to remove these columns from the 1D file if
                       you want to further process the varying parameters (e.g., 1dsvd).
                  **++ The mapping from I J K to X Y Z (DICOM coordinates), where the
                       '-nwarp_fix' constraints are actually applied, is very simple:
                       given the command to fix K (say), the coordinate X, or Y, or Z
                       whose direction most closely aligns with the dataset K grid
                       direction is chosen.  Thus, for coronal images, K is in the A-P
                       direction, so '-nwarp_fixmotK' is translated to '-nwarp_fixmotY'.
                       * This simplicity means that using the '-nwarp_fix' commands on
                         oblique datasets is problematic.  Perhaps it would work in
                         combination with the '-EPI' option, but that has not been tested.
    
    -nwarp NOTES:
    -------------
    * -nwarp is slow - reeeaaallll slow - use it with OpenMP!
    * Check the results to make sure the optimizer didn't run amok!
       (You should ALWAYS do this with any registration software.)
    * For the nonlinear warps, the largest coefficient allowed is
       set to 0.10 by default.  If you wish to change this, use an
       option like '-nwarp_parmax 0.05' (to make the allowable amount
       of nonlinear deformation half the default).
      ++ N.B.: Increasing the maximum past 0.10 may give very bad results!!
    * If you use -1Dparam_save, then you can apply the nonlinear
       warp to another dataset using -1Dparam_apply in a later
       3dAllineate run. To do so, use '-nwarp xxx' in both runs
       , so that the program knows what the extra parameters in
       the file are to be used for.
      ++ Bilinear: 43 values are saved in 1 row of the param file.
      ++ The first 12 are the affine parameters
      ++ The next 27 are the D1,D2,D3 matrix parameters (cf. infra).
      ++ The final 'extra' 4 values are used to specify
          the center of coordinates (vector Xc below), and a
          pre-computed scaling factor applied to parameters #13..39.
      ++ For polynomial warps, a similar format is used (mutatis mutandis).
    * The option '-nwarp_save sss' lets you save a 3D dataset of the
      the displacement field used to create the output dataset.  This
      dataset can be used in program 3dNwarpApply to warp other datasets.
      ++ If the warp is symbolized by x -> w(x) [here, x is a DICOM 3-vector],
         then the '-nwarp_save' dataset contains w(x)-x; that is, it contains
         the warp displacement of each grid point from its grid location.
      ++ Also see program 3dNwarpCalc for other things you can do with this file:
           warp inversion, catenation, square root, ...
    
    * Bilinear warp formula:
       Xout = inv[ I + {D1 (Xin-Xc) | D2 (Xin-Xc) | D3 (Xin-Xc)} ] [ A Xin ]
      where Xin  = input vector  (base dataset coordinates)
            Xout = output vector (source dataset coordinates)
            Xc   = center of coordinates used for nonlinearity
                   (will be the center of the base dataset volume)
            A    = matrix representing affine transformation (12 params)
            I    = 3x3 identity matrix
        D1,D2,D3 = three 3x3 matrices (the 27 'new' parameters)
                   * when all 27 parameters == 0, warp is purely affine
         {P|Q|R} = 3x3 matrix formed by adjoining the 3-vectors P,Q,R
        inv[...] = inverse 3x3 matrix of stuff inside '[...]'
    * The inverse of a bilinear transformation is another bilinear
       transformation.  Someday, I may write a program that will let
       you compute that inverse transformation, so you can use it for
       some cunning and devious purpose.
    * If you expand the inv[...] part of the above formula in a 1st
       order Taylor series, you'll see that a bilinear warp is basically
       a quadratic warp, with the additional feature that its inverse
       is directly computable (unlike a pure quadratic warp).
    * 'bilinearD' means the matrices D1, D2, and D3 with be constrained
      to be diagonal (a total of 9 nonzero values), rather than full
      (a total of 27 nonzero values).  This option is much faster.
    * Is '-nwarp bilinear' useful?  Try it and tell me!
    * Unlike a bilinear warp, the polynomial warps cannot be exactly
      inverted.  At some point, I'll write a program to compute an
      approximate inverse, if there is enough clamor for such a toy.
    
    ===========================================================================
    
     =========================================================================
    * This binary version of 3dAllineate is compiled using OpenMP, a semi-
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
    * OpenMP may or may not speed up the program significantly.  Limited
       tests show that it provides some benefit, particularly when using
       the more complicated interpolation methods (e.g., '-cubic' and/or
       '-final wsinc5'), for up to 3-4 CPU threads.
    * But the speedup is definitely not linear in the number of threads, alas.
       Probably because my parallelization efforts were pretty limited.
     =========================================================================
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
