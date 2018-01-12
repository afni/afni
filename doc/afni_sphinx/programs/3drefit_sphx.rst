.. contents:: 
    :depth: 4 

*******
3drefit
*******

.. code-block:: none

    Changes some of the information inside a 3D dataset's header.
    Note that this program does NOT change the .BRIK file at all;
    the main purpose of 3drefit is to fix up errors made when
    using to3d.
    To see the current values stored in a .HEAD file, use the command
    '3dinfo dataset'.  Using 3dinfo both before and after 3drefit is
    a good idea to make sure the changes have been made correctly!
    
    20 Jun 2006: 3drefit will now work on NIfTI datasets (but it will write
                 out the entire dataset, into the current working directory)
    
    Usage: 3drefit [options] dataset ...
    where the options are
      -orient code    Sets the orientation of the 3D volume(s) in the .BRIK.
                      The code must be 3 letters, one each from the
                      pairs {R,L} {A,P} {I,S}.  The first letter gives
                      the orientation of the x-axis, the second the
                      orientation of the y-axis, the third the z-axis:
                         R = right-to-left         L = left-to-right
                         A = anterior-to-posterior P = posterior-to-anterior
                         I = inferior-to-superior  S = superior-to-inferior
                   ** WARNING: when changing the orientation, you must be sure
                      to check the origins as well, to make sure that the volume
                      is positioned correctly in space.
    
      -xorigin distx  Puts the center of the edge voxel off at the given
      -yorigin disty  distance, for the given axis (x,y,z); distances in mm.
      -zorigin distz  (x=first axis, y=second axis, z=third axis).
                      Usually, only -zorigin makes sense.  Note that this
                      distance is in the direction given by the corresponding
                      letter in the -orient code.  For example, '-orient RAI'
                      would mean that '-zorigin 30' sets the center of the
                      first slice at 30 mm Inferior.  See the to3d manual
                      for more explanations of axes origins.
                   ** SPECIAL CASE: you can use the string 'cen' in place of
                      a distance to force that axis to be re-centered.
    
      -xorigin_raw xx Puts the center of the edge voxel at the given COORDINATE
      -yorigin_raw yy rather than the given DISTANCE.  That is, these values
      -zorigin_raw zz directly replace the offsets in the dataset header,
                      without any possible sign changes.
    
      -duporigin cset Copies the xorigin, yorigin, and zorigin values from
                      the header of dataset 'cset'.
    
      -dxorigin dx    Adds distance 'dx' (or 'dy', or 'dz') to the center
      -dyorigin dy    coordinate of the edge voxel.  Can be used with the
      -dzorigin dz    values input to the 'Nudge xyz' plugin.
                   ** WARNING: you can't use these options at the same
                      time you use -orient.
                   ** WARNING: consider -shift_tags if dataset has tags
    
      -xdel dimx      Makes the size of the voxel the given dimension,
      -ydel dimy      for the given axis (x,y,z); dimensions in mm.
      -zdel dimz   ** WARNING: if you change a voxel dimension, you will
                      probably have to change the origin as well.
      -keepcen        When changing a voxel dimension with -xdel (etc.),
                      also change the corresponding origin to keep the
                      center of the dataset at the same coordinate location.
      -xyzscale fac   Scale the size of the dataset voxels by the factor 'fac'.
                      This is equivalent to using -xdel, -ydel, -zdel together.
                      -keepcen is used on the first input dataset, and then
                      any others will be shifted the same amount, to maintain
                      their alignment with the first one.
                   ** WARNING: -xyzscale can't be used with any of the other
                      options that change the dataset grid coordinates!
                   ** N.B.: 'fac' must be positive, and using fac=1.0 is stupid.
    
      -TR time        Changes the TR time to a new value (see 'to3d -help').
                   ** You can also put the name of a dataset in for 'time', in
                      which case the TR for that dataset will be used.
      -notoff         Removes the slice-dependent time-offsets.
      -Torg ttt       Set the time origin of the dataset to value 'ttt'.
                      (Time origins are set to 0 in to3d.)
                   ** WARNING: These 3 options apply only to 3D+time datasets.
                       **N.B.: Using '-TR' on a dataset without a time axis
                               will add a time axis to the dataset.
    
      -newid          Changes the ID code of this dataset as well.
    
      -nowarp         Removes all warping information from dataset.
    
      -apar aset      Set the dataset's anatomy parent dataset to 'aset'
                   ** N.B.: The anatomy parent is the dataset from which the
                      transformation from +orig to +acpc and +tlrc coordinates
                      is taken.  It is appropriate to use -apar when there is
                      more than 1 anatomical dataset in a directory that has
                      been transformed.  In this way, you can be sure that
                      AFNI will choose the correct transformation.  You would
                      use this option on all the +orig dataset that are
                      aligned with 'aset' (i.e., that were acquired in the
                      same scanning session).
                   ** N.B.: Special cases of 'aset'
                       aset = NULL --> remove the anat parent info from the dataset
                       aset = SELF --> set the anat parent to be the dataset itself
    
      -wpar wset      Set the warp parent (the +orig version of a +tlrc dset).
                      This option is used by @auto_tlrc. Do not use it unless
                      you know what you're doing. 
    
      -clear_bstat    Clears the statistics (min and max) stored for each sub-brick
                      in the dataset.  This is useful if you have done something to
                      modify the contents of the .BRIK file associated with this
                      dataset.
      -redo_bstat     Re-computes the statistics for each sub-brick.  Requires
                      reading the .BRIK file, of course.  Also does -clear_bstat
                      before recomputing statistics, so that if the .BRIK read
                      fails for some reason, then you'll be left without stats.
    
      -statpar v ...  Changes the statistical parameters stored in this
                      dataset.  See 'to3d -help' for more details.
    
      -markers        Adds an empty set of AC-PC markers to the dataset,
                      if it can handle them (is anatomical, is in the +orig
                      view, and isn't 3D+time).
                   ** WARNING: this will erase any markers that already exist!
    
      -shift_tags     Apply -dxorigin (and y and z) changes to tags.
    
      -dxtag dx       Add dx to the coordinates of all tags.
      -dytag dy       Add dy to the coordinates of all tags.
      -dztag dz       Add dz to the coordinates of all tags.
    
      -view code      Changes the 'view' to be 'code', where the string 'code'
                      is one of 'orig', 'acpc', or 'tlrc'.
                   ** WARNING: The program will also change the .HEAD and .BRIK
                      filenames to match.  If the dataset filenames already
                      exist in the '+code' view, then this option will fail.
                      You will have to rename the dataset files before trying
                      to use '-view'.  If you COPY the files and then use
                      '-view', don't forget to use '-newid' as well!
                   ** WARNING2: Changing the view without specifying the new 
                      might lead to conflicting information. Consider specifying
                      the space along with -view
      -space spcname  Associates the dataset with a specific template type, e.g.
                      TLRC, MNI, ORIG. The default assumed for +tlrc datasets is
                      'TLRC'. One use for this attribute is to use MNI space
                      coordinates and atlases instead of the default TLRC space.
                   ** See WARNING2 for -view option.
      -cmap cmaptype  Associate colormap type with dataset. Available choices are
                      CONT_CMAP (the default), INT_CMAP (integer colormap display)
                      and SPARSE_CMAP (for sparse integer colormaps). INT_CMAP is
                      appropriate for showing ROI mask datasets or Atlas datasets
                      where the continuous color scales are not useful.
    
      -label2 llll    Set the 'label2' field in a dataset .HEAD file to the
                      string 'llll'.  (Can be used as in AFNI window titlebars.)
      -labeltable TTT Inset the label table TTT in the .HEAD file.
                      The label table format is described in README.environment
                      under the heading: 'Variable: AFNI_VALUE_LABEL_DTABLE'
                  See also -copytables
    
      -denote         Means to remove all possibly-identifying notes from
                      the header.  This includes the History Note, other text
                      Notes, keywords, and labels.
    
      -deoblique      Replace transformation matrix in header with cardinal matrix.
                      This option DOES NOT deoblique the volume. To do so
                      you should use 3dWarp -deoblique. This option is not 
                      to be used unless you really know what you're doing.
    
      -oblique_origin
                      assume origin and orientation from oblique transformation
                      matrix rather than traditional cardinal information
    
      -byteorder bbb  Sets the byte order string in the header.
                      Allowable values for 'bbb' are:
                         LSB_FIRST   MSB_FIRST   NATIVE_ORDER
                      Note that this does not change the .BRIK file!
                      This is done by programs 2swap and 4swap.
    
      -checkaxes      Doesn't alter the input dataset; rather, this just
                      checks the dataset axes orientation codes and the
                      axes matrices for consistency.  (This option was
                      added primarily to check for bugs in various codes.)
    
      -appkey ll      Appends the string 'll' to the keyword list for the
                      whole dataset.
      -repkey ll      Replaces the keyword list for the dataset with the
                      string 'll'.
      -empkey         Destroys the keyword list for the dataset.
    
      -atrcopy dd nn  Copy AFNI header attribute named 'nn' from dataset 'dd'
                      into the header of the dataset(s) being modified.
                      For more information on AFNI header attributes, see
                      documentation file README.attributes. More than one
                      '-atrcopy' option can be used.
              **N.B.: This option is for those who know what they are doing!
                      Without the -saveatr option, this option is
                      meant to be used to alter attributes that are NOT
                      directly mapped into dataset internal structures, since
                      those structures are mapped back into attribute values
                      as the dataset is being written to disk.  If you want
                      to change such an attribute, you have to use the
                      corresponding 3drefit option directly or use the 
                      -saveatr option.
    
                      If you are confused, try to understand this: 
                      Option -atrcopy was never intended to modify AFNI-
                      specific attributes. Rather, it was meant to copy
                      user-specific attributes that had been added to some
                      dataset using -atrstring option. A cursed day came when
                      it was convenient to use -atrcopy to copy an AFNI-specific
                      attribute (BRICK_LABS to be exact) and for that to
                      take effect in the output, the option -saveatr was added.
                      Contact Daniel Glen and/or Rick Reynolds for further 
                      clarification and any other needs you may have.
    
                      Do NOT use -atrcopy or -atrstring with other modification
                      options.
              See also -copyaux
    
      -atrstring n 'x' Copy the string 'x' into the dataset(s) being
                       modified, giving it the attribute name 'n'.
                       To be safe, the 'x' string should be in quotes.
              **N.B.: You can store attributes with almost any name in
                      the .HEAD file.  AFNI will ignore those it doesn't
                      know anything about.  This technique can be a way of
                      communicating information between programs.  However,
                      when most AFNI programs write a new dataset, they will
                      not preserve any such non-standard attributes.
              **N.B.: Special case: if the string 'x' is of the form
                      'file:name', then the contents of the file 'name' will
                      be read in as a single string and stored in the attribute.
      -atrfloat name 'values'
      -atrint name 'values'
                      Create or modify floating point or integer attributes.
                      The input values may be specified as a single string
                      in quotes or as a 1D filename or string. For example,
         3drefit -atrfloat IJK_TO_DICOM_REAL '1 0.2 0 0 -0.2 1 0 0 0 0 1 0' dset+orig
         3drefit -atrfloat IJK_TO_DICOM_REAL flipZ.1D dset+orig
         3drefit -atrfloat IJK_TO_DICOM_REAL \ 
           '1D:1,0.2,2@0,-0.2,1,2@0,2@0,1,0' \ 
           dset+orig
                      Almost all afni attributes can be modified in this way
      -saveatr        (default) Copy the attributes that are known to AFNI into 
                      the dset->dblk structure thereby forcing changes to known
                      attributes to be present in the output.
                      This option only makes sense with -atrcopy
              **N.B.: Don't do something like copy labels of a dataset with 
                      30 sub-bricks to one that has only 10, or vice versa.
                      This option is for those who would deservedly earn a
                      hunting license.
      -nosaveatr      Opposite of -saveatr
         Example: 
         3drefit -saveatr -atrcopy WithLabels+tlrc BRICK_LABS NeedsLabels+tlrc
    
      -'type'         Changes the type of data that is declared for this
                      dataset, where 'type' is chosen from the following:
           ANATOMICAL TYPES
             spgr == Spoiled GRASS             fse == Fast Spin Echo  
             epan == Echo Planar              anat == MRI Anatomy     
               ct == CT Scan                  spct == SPECT Anatomy   
              pet == PET Anatomy               mra == MR Angiography  
             bmap == B-field Map              diff == Diffusion Map   
             omri == Other MRI                abuc == Anat Bucket     
           FUNCTIONAL TYPES
              fim == Intensity                fith == Inten+Thr       
             fico == Inten+Cor                fitt == Inten+Ttest     
             fift == Inten+Ftest              fizt == Inten+Ztest     
             fict == Inten+ChiSq              fibt == Inten+Beta      
             fibn == Inten+Binom              figt == Inten+Gamma     
             fipt == Inten+Poisson            fbuc == Func-Bucket     
    
      -copyaux auxset Copies the 'auxiliary' data from dataset 'auxset'
                      over the auxiliary data for the dataset being
                      modified.  Auxiliary data comprises sub-brick labels,
                      keywords, statistics codes, nodelists, and labeltables
                      AND/OR atlas point lists.
                      '-copyaux' occurs BEFORE the '-sub' operations below,
                      so you can use those to alter the auxiliary data
                      that is copied from auxset.
    
    
      -copytables tabset Copies labeltables AND/OR atlas point lists, if any,
                      from tabset to the input dataset.
                      '-copyaux' occurs BEFORE the '-sub' operations below,
                      so you can use those to alter the auxiliary data
                      that is copied from tabset. 
    
      -relabel_all xx  Reads the file 'xx', breaks it into strings,
                       and puts these strings in as the sub-brick
                       labels.  Basically a batch way of doing
                       '-sublabel' many times, for n=0, 1, ...
                     ** This option is executed BEFORE '-sublabel',
                        so any labels from '-sublabel' will over-ride
                        labels from this file.
                     ** Strings in the 'xx' file are separated by
                        whitespace (blanks, tabs, new lines).
    
      -relabel_all_str 'lab0 lab1 ... lab_p': Just like -relabel_all
                       but with labels all present in one string
    
      -sublabel_prefix PP: Prefix each sub-brick's label with PP
      -sublabel_suffix SS: Suffix each sub-brick's label with SS
    
    The options below allow you to attach auxiliary data to sub-bricks
    in the dataset.  Each option may be used more than once so that
    multiple sub-bricks can be modified in a single run of 3drefit.
    
      -sublabel  n ll  Attach to sub-brick #n the label string 'll'.
      -subappkey n ll  Add to sub-brick #n the keyword string 'll'.
      -subrepkey n ll  Replace sub-brick #n's keyword string with 'll'.
      -subempkey n     Empty out sub-brick #n' keyword string
    
      -substatpar n type v ...
                      Attach to sub-brick #n the statistical type and
                      the auxiliary parameters given by values 'v ...',
                      where 'type' is one of the following:
             type  Description  PARAMETERS
             ----  -----------  ----------------------------------------
             fico  Cor          SAMPLES  FIT-PARAMETERS  ORT-PARAMETERS
             fitt  Ttest        DEGREES-of-FREEDOM
             fift  Ftest        NUMERATOR and DENOMINATOR DEGREES-of-FREEDOM
             fizt  Ztest        N/A
             fict  ChiSq        DEGREES-of-FREEDOM
             fibt  Beta         A (numerator) and B (denominator)
             fibn  Binom        NUMBER-of-TRIALS and PROBABILITY-per-TRIAL
             figt  Gamma        SHAPE and SCALE
             fipt  Poisson      MEAN
    
    You can also use option '-unSTAT' to remove all statistical encodings
    from sub-bricks in the dataset.  This operation would be desirable if
    you modified the values in the dataset (e.g., via 3dcalc).
     ['-unSTAT' is done BEFORE the '-substatpar' operations, so you can  ]
     [combine these options to completely redo the sub-bricks, if needed.]
     [Option '-unSTAT' also implies that '-unFDR' will be carried out.   ]
    
    The following options allow you to modify VOLREG fields:
      -vr_mat val1 ... val12  Use these twelve values for VOLREG_MATVEC_index.
      -vr_mat_ind index       Index of VOLREG_MATVEC_index field to be modified.
                              Optional, default index is 0.
    NB: You can only modify one VOLREG_MATVEC_index at a time
      -vr_center_old x y z    Use these 3 values for VOLREG_CENTER_OLD.
      -vr_center_base x y z   Use these 3 values for VOLREG_CENTER_BASE.
    
    
    The following options let you modify the FDR curves stored in the header:
    
     -addFDR = For each sub-brick marked with a statistical code, (re)compute
               the FDR curve of z(q) vs. statistic, and store in the dataset header
               * '-addFDR' runs as if '-new -pmask' were given to 3dFDR, so that
                  stat values == 0 will be ignored in the FDR algorithm.
    
     -FDRmask mset = load dataset 'mset' and use it as a mask
     -STATmask mset  for the '-addFDR' calculations.
                     * This can be useful if you ran 3dDeconvolve/3dREMLFIT
                        without a mask, and want to apply a mask to improve
                        the FDR estimation procedure.
                     * If '-addFDR' is NOT given, then '-FDRmask' does nothing.
                     * 3drefit does not generate an automask for FDR purposes
                        (unlike 3dREMLfit and 3dDeconvolve), since the input
                        dataset may contain only statistics and no structural
                        information about the brain.
    
     -unFDR  = Remove all FDR curves from the header
               [you will want to do this if you have done something to ]
               [modify the values in the dataset statistical sub-bricks]
    
    ++ Last program update: 27 Mar 2009
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
