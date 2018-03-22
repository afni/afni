***************
@chauffeur_afni
***************

.. _@chauffeur_afni:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    This was originally a helper function in processing scripts, to take
    quality control (QC) snapshots automatically.  It wraps around a lot
    (but not all) of the veeery useful "driving AFNI" functionality.  You,
    dear user, can still accomplish all the same with those commands, but
    I just wanted to add in some other calculations, as well, to try to
    make the process of generating montages of images easier.
    
    The purpose of this function is to generate images easily and quickly
    while processing-- even if on a remote server (because it uses xvfb to
    make a virtual X11 environment)-- to be able to what is happening in
    data processing at useful stages: for example, alignment of two sets
    without having to click any buttons in the AFNI GUI.  This makes it
    easier to review batch processing, discuss processing with one's boss,
    prepare for a presentation or publication, etc. For example, this
    program is used in most all of FATCAT's fat_proc_* scripts, and even
    TORTOISE includes calls to it for auto-QC imaging if the user has AFNI
    installed (and suuuurely they should??).
    
    Each call to this function will make a set of montages in the axial,
    coronal and sagittal planes, of user-specified dimensionality.
    
    This function can be used on both 3D and 4D data sets, but for the
    latter, probably @djunct_4d_imager would be much more simple to use.
    
    A lot of the help descriptions for command line options, below, will
    refer to the variables in the "AFNI Driver" doc:
    https://afni.nimh.nih.gov/pub/dist/doc/program_help/README.driver.html
    or variables in the "AFNI Environment" doc:
    https://afni.nimh.nih.gov/pub/dist/doc/program_help/README.environment.html
    References to these are sometimes noted explicitely with "see DR" or
    "see ENV", respectively, and potentially with the particular variable.
    For example, "(see ENV: SAVE_AGIF)".
    
    ++ constructed by PA Taylor (NIMH, NIH, USA).
    
    # =========================================================================
    
    -help, -h          :see helpfile (here!)
    -ver               :see version number
    
    -ulay    UUU       :name of underlay dset (required); can be 3D or 4D
                        set, depending on the circumstances.  For 4D, 
                        though, strongly consider using "@djunct_4d_imager".
    
    -olay    OOO       :name of overlay dset (opt).
    
    -olay_off          :explicitly state you are not using olay (opt);
                        if not used and no olay is given, then the user
                        just gets prompted to be sure they haven't 
                        forgotten the olay dset.
    
    -prefix  PPP       :prefix for output files (required).
    
    -cbar    CCC       :specify a new colorbar, where CCC can be any of the
                        cbars in the standard AFNI list (def: Plasma).
    
    -blowup  BB        :enter a "blowup factor", where BB is an integer 
                        in the interval [1, 8].  Increases spatial resolution
                        in the output by a factor of BB (see DR).
    
    -set_xhairs XX     :specify type and/or existence of crosshairs in the
                        image (see DR: SET_XHAIRS).
    
    -opacity   OO      :enter an "opacity factor" for the olay, where OO is 
                        an integer in the interval [0, 9], with the 9 being
                        opaque (see DR).
    
    -delta_slices DS DC DA :when montaging, (DS, DC, DA) is the integer
                         number of slices to use as spacing between views
                         along the (sag, cor, axi) axes, respectively
                         (def: automatically calculate to ~evenly fit the
                         number of selected montage slices along this
                         axis).  (See DR: "mont=PxQ:R"; basically, each D?
                         is the 'R' value along the given axis).  Users
                         can specify a delta_slice value along *some* axis
                         and leave other(s) to be chosen automatically, by
                         specifying a D? value >0 for their own value, and
                         given any other D? value -1.  For example:
                          -delta_slices 40 -1 -1
                         would specify every 40th slice along the sag axis, 
                         while the cor and axi spacing would be automatically 
                         calculated.
    
    -thr_olay THR      :threshold the olay dset at THR (def: 0, or
                        unthreshold). If you are thresholding a statistic
                        brick, then you should see the "-thr_olay_p2stat ..."
                        option, below. (See DR: SET_THRESHNEW)
    -thrflag   'fff'   :further control of how the THR value is interpreted 
                        (def: "*"). (See DR: SET_THRESHNEW)
    -thr_olay_p2stat PP :an alternative way to specify a voxelwise
                        threshold (i.e., instead of "-thr_olay ..."), when
                        thresholding based on a statistic; you can specify
                        the p-value you want, and using internal header
                        information, the appropriate value for whatever
                        statistic is in the statistic brick will be
                        calculated and applied; you likely need to use
                        "-set_subbricks i j k" with this, where 'k' would
                        be the index of the statistic brick (and likely
                        'j' would be the index of the associated
                        coefficient/beta brick; 'i' would be the brick of
                        the underlay volume, and if there is only a single
                        volume there, it could just be either '0' or
                        '-1').
    
    -set_subbricks i j k :specify subbricks being viewed in the ulay, olay
                        and threshold dsets (def: "-1 -1 -1", which means
                        ignore these values).  This is the way to specify 
                        different overlay and threshold subbricks for 
                        displaying, such as using the "beta" or "coefficient"
                        for color and the "statistic" as the threshold level.
                        (See DR: SET_SUBBRICKS)
    
    -func_range FR     :specify upper value FR of the olay dset to be
                        matched to top of colorbar (def: calc 98%ile value
                        of dset and use that).
    
    -func_range_perc FRP :alternative to "-func_range ..."; specify a
                        percentile value FRP to use to make the upper
                        value of the olay dset to be matched to the top of
                        the colorbar (def: calc 98%ile value of dset and
                        use that).
    
    -pbar_posonly      :for color range specification, default is to use
                        both positive and negative values; enter this flag
                        to use only the positive range. (See DR:
                        SET_PBAR_ALL)
    
    -func_resam  RES   :set the resampling mode for dsets; valid values
                        are:   NN  Li  Cu  Bk
                        (def: NN; hey, voxels are voxels!). 
                        (See DR: SET_FUNC_RESAM)
    
    -cbar_ncolors NC   :set colorscale mode (def: 99) (See DR:
                        SET_PBAR_ALL, the 2nd usage case, description
                        about '99').
    
    -cbar_topval  TOPV :set colorscale mode (def: 1) (See DR:
                        SET_PBAR_ALL, the 2nd usage case, description
                        about 'topval').
    
    -save_ftype  FTYPE :type of file as which to save images; key types
                        are listed in the Driver description (def: PNG)
                        (See DR: SAVE_ALLJPEG, SAVE_ALLPNG, SAVE_MPEG,
                        SAVE_AGIF, SAVE_JPEG, SAVE_PNG)
    
    -set_ijk  II JJ KK :Set the controller coordinates to the given
                        triple, which are integer index selectors along
                        the three spatial axes.  This essentially
                        specifies the middle image in the montage (def:
                        for each coordinate, choose middle slice along
                        axis).
    -set_dicom_xyz XX YY ZZ :Set the controller coordinates to the given
                        triple, which are the (x, y, z) coordinates in
                        AFNI's favorite RAI DICOM notation.  (def: for
                        each coordinate, choose middle slice along axis).
    
    -montx  MX         :in creating montage, the number of image panels
                        in a row (def: 3); the total number of panels per 
                        axis is:  MX*MY   (see below "-monty ...").
                        (See DR: "mont=PxQ:R"; basically, MX is the 'P' 
                        value).
    -monty  MY         :in creating montage, the number of image panels
                        in a column (def: 3); the total number of panels
                        per axis is:  MX*MY   (see above "-montx ...").
                        (See DR: "mont=PxQ:R"; basically, MY is the 'Q' 
                        value).
    
    -alpha_par  ALPHP  :In addition to representing olay values as colors
                        with a threshold, one also apply opacity
                        information to 'soften' the effect of
                        thresholding; see DR: SET_FUNC_ALPHA for a
                        description of this behavior (def: "Off", which is
                        just standard thresholding stuff).
    -alpha_floor ALPHF :second parameter for more complicated olay 
                        thresholding behavior.  Too complicated for simple
                        me to explain-- see DR: SET_FUNC_ALPHA (def: ""; 
                        this empty string means use default
                        value, which at time of writing is 0) 
    -alpha_edgize_no   :even *more* control over fancy alpha-based 
                        thresholding; for this, see ENV: 
                        AFNI_EDGIZE_OVERLAY, whose default value is "YES".
                        Using this option changes that value to 'NO' 
                        internally.
    
    -image_zoom_nn_no  :the default zoom is set to NN mode, so no smoothing
                        occurs (see ENV: AFNI_IMAGE_ZOOM_NN).  This option
                        changes behavior internally to set this variable
                        to have the value "NO".
    
    -zerocolor ZC      :Change the default 'background' ulay color of zero
                        values (def: "Black"); ZC can be set to any allowed
                        AFNI value (see ENV: AFNI_IMAGE_ZEROCOLOR).  This 
                        option is mainly for G. Chen, who flaunts convention
                        whenever possible.
    
    -label_mode    LM  :control labels, ON/OFF and location (def: 1);
                        (see ENV: AFNI_IMAGE_LABEL_MODE)
    -label_size    LS  :control labels, size (def: 3);
                        (see ENV: AFNI_IMAGE_LABEL_SIZE)
    -label_color   LC  :control labels, color (def: white);
                        (see ENV: AFNI_IMAGE_LABEL_COLOR)
    -label_setback LB  :control labels, offset from edge (def: 0.01);
                        (see ENV: AFNI_IMAGE_LABEL_SETBACK)
    -label_string LSTR :control labels, string automatically appended
                        to the slice (def: "");
                        (see ENV: AFNI_IMAGE_LABEL_STRING)
    -image_label_ijk LIJK :If this variable is YES, then the image label will
                        be based on the slice index rather than the
                        spatial (mm) coordinate (def: NO); thanks, Bob!
                        (see ENV: AFNI_IMAGE_LABEL_IJK)
    
    -globalrange  GR   :specify how lookup range for matching ulay values
                        is done (def: VOLUME);
                        (see ENV: AFNI_IMAGE_GLOBALRANGE)
    -ulay_range UMIN UMAX :specify min and max range values of ulay dset;
                        if a "%" is on both numbers, then treat the
                        numerical part of UMIN and UMAX as percentiles
                        from which to calculate actual values; otherwise,
                        treat UMIN and UMAX as values directly.
    
    -do_clean          :by default, the temporary directory of copying
                        files and such is not removed;  using this option
                        means that that working directory *is* removed.
    
    # ========================================================================
    
    EXAMPLES:
    
        # 1) Basic vanilla: make a 3x5 montage of just a ulay; there will
        #    be 15 slices shown, evenly spaced along each axis, with some
        #    labels on the corners.
    
        @chauffeur_afni                     \
            -ulay    MY_ULAY.nii.gz         \
            -prefix  PRETTY_PICTURE         \
            -montx 5 -monty 3               \
            -set_xhairs OFF                 \
            -label_mode 1 -label_size 3     \
            -do_clean  
    
    
        # 2) Make a 3x5 montage of an overlayed data set that has an ROI
        #    map, so we want it to be colored-by-integer.  Put the images
        #    into a pre-existing directory, SUBDIR/.
    
        @chauffeur_afni                       \
            -ulay  MY_ULAY.nii.gz             \
            -olay  MY_OLAY.nii.gz             \
            -pbar_posonly                     \
            -cbar "ROI_i256"                  \
            -func_range 256                   \
            -opacity 4                        \
            -prefix   SUBDIR/PRETTY_PICTURE2  \
            -montx 5 -monty 3                 \
            -set_xhairs OFF                   \
            -label_mode 1 -label_size 3       \
            -do_clean 
    
    
        # 3) Make a 3x5 montage of an overlayed data set that shows the
        #    beta coefficients stored in brick [1] while thresholding the
        #    associated statistic stored in brick [2] at voxelwise p=0.001, 
        #    overlayed on the anatomical volume.
        @chauffeur_afni                       \
            -ulay  anat.nii.gz                \
            -olay  stats.nii.gz               \
            -cbar Plasma                      \
            -func_range 3                     \
            -thr_olay_p2stat 0.001            \
            -set_subbricks -1 1 2             \
            -opacity 4                        \
            -prefix   STAT_MAP                \
            -montx 5 -monty 3                 \
            -set_xhairs OFF                   \
            -label_mode 1 -label_size 3       \
            -do_clean 
    
    
    # -------------------------------------------------------------------
