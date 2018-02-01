*****************
align_epi_anat.py
*****************

.. _align_epi_anat.py:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    #++ align_epi_anat version: 1.57
    
        ===========================================================================
        align_epi_anat.py     - align EPI to anatomical datasets or vice versa
        
        This Python script computes the alignment between two datasets, typically
        an EPI and an anatomical structural dataset, and applies the resulting
        transformation to one or the other to bring them into alignment.
    
        This script computes the transforms needed to align EPI and  
        anatomical datasets using a cost function designed for this purpose. The  
        script combines multiple transformations, thereby minimizing the amount of 
        interpolation applied to the data.
        
        Basic Usage:
          align_epi_anat.py -anat anat+orig -epi epi+orig -epi_base 5
        
        The user must provide EPI and anatomical datasets and specify the EPI
        sub-brick to use as a base in the alignment.  
    
        Internally, the script always aligns the anatomical to the EPI dataset,
        and the resulting transformation is saved to a 1D file. 
        As a user option, the inverse of this transformation may be applied to the 
        EPI dataset in order to align it to the anatomical data instead.
    
        This program generates several kinds of output in the form of datasets
        and transformation matrices which can be applied to other datasets if
        needed. Time-series volume registration, oblique data transformations and
        Talairach (standard template) transformations will be combined as needed
        and requested (with options to turn on and off each of the steps) in
        order to create the aligned datasets.
        
        **Note the intermediate datasets used to compute the alignment are **not**
        saved unless one of the -save_xxx options is given. This includes
        skull-stripped, slice timing corrected and volume registered datasets
        without alignment. These intermediated datasets are normally deleted.
        See the -save_xxx section below for more information on saving these 
        datasets for future use.
        
        Depending upon selected options, the script's output contains the following:
            Datasets:
              ANAT_al+orig: A version of the anatomy that is aligned to the EPI
              EPI_al+orig: A version of the EPI dataset aligned to the anatomy
              EPI_tlrc_al+tlrc: A version of the EPI dataset aligned to a standard
                           template
            These transformations include slice timing correction and
              time-series registration by default.
    
            Transformation matrices:
              ANAT_al_mat.aff12.1D: matrix to align anatomy to the EPI
              EPI_al_mat.aff12.1D:  matrix to align EPI to anatomy 
                                       (inverse of above)
              EPI_vr_al_mat.aff12.1D: matrix to volume register EPI
              EPI_reg_al_mat.aff12.1D: matrix to volume register and align epi
                                       to anatomy (combination of the two
                                       previous matrices)
              EPI_al_tlrc_mat.aff12.1D: matrix to volume register and align epi
                                       to anatomy and put into standard space
    
            Motion parameters from optional volume registration:
              EPI_tsh_vr_motion.1D: motion parameters from EPI time-series 
                                    registration (tsh included in name if slice
                                    timing correction is also included).
              
        where the uppercase "ANAT" and "EPI" are replaced by the prefix names
        of the input datasets, and the suffix can be changed from "_al" as a user
        option.
              
        You can use these transformation matrices later to align other datasets:
             3dAllineate -cubic -1Dmatrix_apply epi_r1_al_mat.aff12.1D  \
                         -prefix epi_alman epi_r2+orig
    
                 
        The goodness of the alignment should always be assessed visually.
        Superficially, most of 3dAllineate's cost functions, and those
        of registration programs from other packages, will produce a plausible
        alignment based upon a cursory examination but it may not be the best.
        You need to examine the results carefully if alignment quality is crucial
        for your analysis.
    
        In the absence of a gold standard, and given the low contrast of EPI data,
        it is difficult to judge alignment quality by just looking at the two
        volumes. This is the case, even when you toggle quickly between one volume
        and the next, by turning the color overlay off and using the 'u' key in the
        slice viewer window. To aid with the assessment of alignment, you can use
        the -AddEdge option or call the @AddEdge script directly. See the help for
        @AddEdge for more information on that script.
    
        The default options assume the epi and anat datasets start off fairly close,
        as is normally the case when the epi dataset closely precedes or follows an 
        anatomical dataset acquisition. If the two data are acquired over separate
        sessions, or accurate coordinate data is not available in the dataset header
        (as sometimes occurs for oblique data), various options allow for larger
        movement including "-cmass cmass", "-big_move","-giant_move",
        "-ginormous_move", and -align_centers yes". Each of these options
        is described below. If the datasets do not share the same
        coordinate space at all, it may be useful to use the "-ginormous_move", 
        "-align_centers" options or run @Align_Centers script first.
        
        Although this script has been developed primarily for aligning anatomical T1
        data with EPI BOLD data, it has also been successfully applied for aligning
        similar modality data together, including T1-SPGR to T1-SPGR, T1-FLAIR
        to T1-SPGR, EPI to EPI, T1-SPGR at 7T to T1-SPGR at 3T, EPI-rat1 to
        EPI-rat2, .... If this kind of alignment is required, the default cost
        function, the Local Pearson Correlation (lpc), is not appropriate.
        Other cost functions like lpa or nmi have been seen to work well for
        intra-modality alignment, using the option "-cost lpa". Also see the the
        dset1 and dset2 options below for functionally equivalent options to the
        epi and anat options.
            
        ---------------------------------------------
        REQUIRED OPTIONS:
        
        -epi dset   : name of EPI dataset
        -anat dset  : name of structural dataset
        -epi_base   : the epi base used in alignment 
                         (0/mean/median/max/subbrick#)
    
        MAJOR OPTIONS:
        -help       : this help message
    
        -anat2epi   : align anatomical to EPI dataset (default)
        -epi2anat   : align EPI to anatomical dataset
    
        The following options are equivalent to those epi/anat options above
        except it is assumed the datasets will have similar modalities if
        either dset1 or dset2 is specified, and the default cost function is
        changed to 'lpa' instead of 'lpc'. This should reduce confusion when
        aligning other types of datasets. Most other options that also have
        names with anat and epi have corresponding dset1 and dset2 options
        that are exactly equivalent.
    
        -dset1      : name of dataset1
        -dset2      : name of dataset2
        -dset1to2   : align dataset1 to dataset2
        -dset2to1   : align dataset2 to dataset1
        
    
        -suffix ssss: append suffix 'sss' to the original anat/epi dataset to use
                         in the resulting dataset names (default is "_al")
         
        -child_epi dset1 dset2 ... : specify other EPI datasets to align.
            Time series volume registration will be done to the same
            base as the main parent EPI dataset. 
            Note if aligning anat to epi, you can still use the -save_vr option
            to save the volume registered (motion corrected) datasets. See the 
            -save_xxx option section of this help for more information.
        -child_dset2  equivalent to child_epi above
    
        -child_anat dset1 dset2 ... : specify other anatomical datasets to align.
            The same transformation that is computed for the parent anatomical
            dataset is applied to each of the child datasets. This only makes
            sense for anat2epi transformations. Skullstripping is not done for
            the child anatomical dataset.
        -child_dset1  equivalent to child_anat above
    
        -AddEdge    : run @AddEdge script to create composite edge images of
                      the base epi or anat dataset, the pre-aligned dataset and 
                      the aligned dataset. Datasets are placed in a separate
                      directory named AddEdge. The @AddEdge can then be used
                      without options to drive AFNI to show the epi and anat
                      datasets with the edges enhanced. For the -anat2epi case
                      (the default), the anat edges are shown in purple, and the
                      epi edges are shown in cyan (light blue). For the -epi2anat
                      case, the anat edges are shown in cyan, and the epi edges
                      are purple. For both cases, overlapping edges are shown in
                      dark purple.
    
        -big_move   : indicates that large displacement is needed to align the
                      two volumes. This option is off by default.
        -giant_move : even larger movement required - uses cmass, two passes and
                      very large angles and shifts. May miss finding the solution
                      in the vastness of space, so use with caution
        -ginormous_move : adds align_centers to giant_move. Useful for very far
                      apart datasets
    
        Notes on the big_move and giant_move options:
            "big_move" allows for a two pass alignment in 3dAllineate.
            The two-pass method is less likely to find a false minimum 
            cost for alignment because it does a number of coarse (blurred,
            rigid body) alignments first and then follows the best of these
            coarse alignments to the fine alignment stage. The big_move 
            option should be a relatively safe option, but it adds
            processing time.
    
            The giant_move option expands the search parameters in space
            from 6 degrees and 10 mm to 45 degrees and 45 mm and adds in
            a center of mass adjustment. The giant_move option will usually
            work well too, but it adds significant time to the processing
            and allows for the possibility of a very bad alignment.Another cost
            functional is available that has worked well with noisy data, "lpc+ZZ".
            For difficult data, consider that alternative.
    
            If your data starts out fairly close (probably the typical case
            for EPI and anatomical data), you can use the -big_move with 
            little problem. All these methods when used with the default
            lpc cost function require good contrast in the EPI image so that
            the CSF can be roughly identifiable.
            
        -partial_coverage: indicates that the EPI dataset covers only a part of 
                      the brain. Alignment will try to guess which direction should
                      not be shifted If EPI slices are known to be a specific 
                      orientation, use one of these other partial_xxxx options.
        -partial_axial
        -partial_coronal 
        -partial_sagittal
    
        -keep_rm_files : keep all temporary files (default is to remove them)
        -prep_only  : do preprocessing steps only
        -verb nn    : provide verbose messages during processing (default is 0)
        -anat_has_skull yes/no: Anat is assumed to have skull ([yes]/no)
        -epi_strip methodname :  method to mask brain in EPI data 
                       ([3dSkullStrip]/3dAutomask/None)
        -volreg_method methodname: method to do time series volume registration
                       (motion correction) of EPI data 
                       ([3dvolreg],3dWarpDrive,3dAllineate). 
                       3dvolreg is for 6 parameter (rigid-body)
                       3dWarpDrive is for 12 parameter (general affine)
                       3dAllineate - also 12 parameter with LPA cost function
    
                       Note if aligning anat to epi, the volume registered EPI
                       dataset is **not** saved unless you use the -save_vr
                       option. See the -save_xxx option section of this help for
                       more information.
    
        -dset1_strip : skull stripping method for dataset1 
        -dset2_strip : skull stripping method for dataset2 (equivalent to epi_strip)
    
        A template registered anatomical dataset such as a talairach-transformed
           dataset may be additionally specified so that output data are
           in template space. The advantage of specifying this transform here is
           that all transformations are applied simultaneously, thereby minimizing 
           data interpolation.
           
        -tlrc_apar ANAT+tlrc : structural dataset that has been aligned to
                      a master template such as a tlrc dataset. If this option
                      is supplied, then an epi+tlrc dataset will be created. 
                      The @auto_tlrc script may be used to create this 
                      "talairach anatomical parent". This option is only valid
                      if aligning epi to anat.
    
    
        Other options:
        -ex_mode modename : execute mode (echo/dry_run/quiet/[script]).
                         "dry_run" can be used to show the commands that
                         would be executed without actually running them. 
                         "echo" shows the commands as they are executed.
                         "quiet" doesn't display commands at all.
                         "script" is like echo but doesn't show stdout, stderr 
                         header lines and "cd" lines.
                         "dry_run" can be used to generate scripts which can be
                         further customized beyond what may be available through
                         the options of this program.
        -Allineate_opts '-ssss  -sss' : options to use with 3dAllineate. Default
                         options are 
                         "-weight_frac 1.0 -maxrot 6 -maxshf 10 -VERB -warp aff "
        -volreg [on]/off : do volume registration on EPI dataset before alignment
        -volreg_opts  '-ssss -sss' : options to use with 3dvolreg
        -volreg_base basenum/type : the epi base used in time series volume
                         registration.
                         The default is to use the same base as the epi_base.
                         If another subbrick or base type is used, an additional
                         transformation will be computed between the volume
                         registration and the epi_base
                         (0/mean/median/max/subbrick#)
                         
                         Note if aligning anat to epi, the volume registered EPI
                         dataset is **not** saved unless you use the -save_vr
                         option. See the -save_xxx option section of this help for
                         more information.
    
        -tshift [on]/off : do time shifting of EPI dataset before alignment
        -tshift_opts   : options to use with 3dTshift
                         The script will determine if slice timing correction is
                         necessary unless tshift is set to off.
    
        -deoblique [on]/off : deoblique datasets before alignment
        -deoblique_opts '-ssss -sss': options to use with 3dWarp deobliquing
                         The script will try to determine if either EPI or anat data
                         is oblique and do the initial transformation to align anat
                         to epi data using the oblique transformation matrices
                         in the dataset headers.
        
        -master_epi  nnn : master grid resolution for aligned epi output
        -master_tlrc nnn : master grid resolution for epi+tlrc output
        -master_anat nnn : master grid resolution for aligned anatomical data output
        -master_dset1 nnn : equivalent to master_anat above
        -master_dset2 nnn : equivalent to master_epi above
                         (SOURCE/BASE/MIN_DXYZ/dsetname/n.nn)
                         Each of the 'master' options can be set to SOURCE,BASE,
                         a specific master dataset, MIN_DXYZ or a specified cubic 
                         voxel size in mm. 
                         
                         MIN_DXYZ uses the smallest voxel dimension as the basis
                         for cubic output voxel resolution within the bounding box
                         of the BASE dataset.
                         
                         SOURCE and BASE are used as in 3dAllineate help.
                         
                         The default value for master_epi and master_anat is SOURCE,
                         that is the output resolution and coordinates should be
                         the same as the input. This is appropriate for small
                         movements.
                       
                         For cases where either dataset is oblique (and larger
                         rotations can occur), the default becomes MIN_DXYZ.
                         
                         The default value for master_tlrc is MIN_DXYZ.
                         
                         "-master_dset1" and "-master_dset2" may be used as 
                         equivalent expressions for anat and epi output resolutions,
                         respectively.
                         
       -check_flip : check if data may have been left/right flipped by aligning
                         original and flipped versions and then comparing costs
                         between the two. This option produces the L/R flipped
                         and aligned anat/dset1 dataset. A warning is printed
                         if the flipped data has a lower cost function value
                         than the original dataset when both are aligned to the
                         epi/dset2 dataset.
    
                         This issue of left-right confusion can be caused
                         by problems with DICOM files or pipelines
                         that include Analyze format datasets. In these cases,
                         the orientation information is lost, and left-right may
                         be reversed. Other directions can also be confused, but
                         A-P and I-S are usually obvious. Note this problem has
                         appeared on several major publicly available databases.
                         Even if other software packages may proceed without errors
                         despite inconsistent, wrong or even missing coordinate
                         and orientation information, this problem can be easily
                         identified with this option.
    
                         This option does not identify which of the two datasets
                         need to be flipped. It only determines there is likely 
                         to be a problem with one or the other of the two input
                         datasets. Importantly, we recommend properly visualizing
                         the datasets in the afni GUI. Look for asymmetries in the
                         two aligned anat/dset1 datasets, and see how they align
                         with the epi/dset2 dataset. To better determine the left
                         and right of each dataset, we recommend relying on tags
                         like vitamin E or looking for surgical markers.
                          
       -flip_giant : apply giant_move options to flipped dataset alignment
                         even if not using that option for original dataset
                         alignment
    
       -save_xxx options
          Normally all intermediate datasets are deleted at the end of the script.
          If aligning anat to epi, the volume registered EPI dataset, although
          computed, is **not** saved unless you use the -save_vr option.
          Similarly other intermediate datasets are not saved unless explicitly
          requested with one of these options:
          -save_Al_in       : save 3dAllineate input files
          -save_tsh         : save tshifted epi
          -save_vr          : save volume registered epi
          -save_skullstrip  : save skull-stripped (not aligned)
          -save_rep         : save representative tstat epi
          -save_resample    : save resampled epi
          -save_epi_ns      : save skull-stripped epi
          -save_all         : save all the above datasets
    
          Not included with -save_all (since parameters are required):
    
          -save_orig_skullstrip PREFIX : save original skull-stripped dset
          -save_script SCRIPT_NAME     : save shell command script to given file
    
       Alternative cost functions and methods:
         The default method used in this script is the LPC (Localized Pearson 
         Correlation) function. The 'lpc' cost function is computed by the
         3dAllineate program. Other cost functionals are available and are
         described briefly in the help for 3dAllineate. This script allows
         the user to choose any cost function available in that program with
         
         -cost xxx
         
         Some cost functionals have proven to be useful for some situations.
         Briefly, when aligning similar datasets (anat to anat), the 'lpa' method
         usually provides good alignment. Instead of using a negative correlation,
         as the 'lpc' method does, the 'lpa' cost functional uses the absolute value
         of the local correlation, so both positive and negative correlations drive
         the alignment. Occasionally the simplest least squares cost functional
         will be useful (implemented with -ls).
         
         If either of the input datasets has very little structural detail (less
         than typical EPI), the mutual information methods provide a rough
         alignment that gives alignment of mostly the contour of the datasets.
         These are implemented with '-cost nmi' or '-cost mi'. 
         
         The lpa cost function looks for both high positive and negative 
         local Pearson correlation (LPA is an acronym in our program for the
         absolute value of the local Pearson correlation). The LPC method looks
         for negative correlation, essentially matching the dark CSF in T1 images
         with the bright CSF in EPI images. The more negative the correlation the
         more likely the CSF will overlay each other and carry the rest of the 
         volume along with it.
         
         -multi_cost cf1 cf2 ...
         Besides cost from specified cost function or default cost function,
         also compute alignment using other cost functionals. For example, using
         "-cost lpa -multi_cost ls nmi" will compute an alignment for the lpa, ls
         and nmi cost functionals. See 3dAllineate's HELP for a full list of
         available cost functionals. Use the AFNI GUI to view differences among
         cost functionals.
    
         -check_cost cf1 cf2 ...
         Verify alignment against another cost functional. If there is a large
         difference, a warning is printed. This does not mean the alignment is
         bad, only that it is different.
         
         -edge       :  use edge method
         
         The Edge method
         Finally, the "edge" method is a new method that is implemented not as a
         cost functional but as a different algorithm altogether. Based on our
         visualization methods for verifying alignment (as we do in AddEdge),
         it uses a local approach like the LPA/C cost functionals, but it is
         independent of the cost function. 
         
         This method has turned out to be useful in a variety of circumstances. It
         has proven useful for data that changes dramatically over time like
         manganese-enhanced MRI (MEMRI) and for some data that has other large
         non-uniformities issues helping to compensate for those large contrasts.
         
         The edge method prepares the image to be a local spatial variance version
         of the original image. First both input datasets are automasked with the 
         outer voxel layers removed. The spatial variance is computed over that
         mask. The optimal alignment is computed between the edge images. Strictly
         speaking, the datasets are not "edges" but a kind of normalized 2D
         gradient. The original datasets are then aligned using the transformation
         computed by the edge image alignment. Internally within the script,
         the gradient function is accomplished by the 3dLocalstat program using its
         cvar option for coefficient of variation. The coefficient of variation is
         computed as the standard deviation within the local neighborhood divided
         by the mean. The local spatial variance ends up being similar to locally
         normalized images of edges within the image. 
         
         The "-edge" option is relatively insensitive to most of the cost functions
         in 3dAllineate, so "lpa", "mi", "nmi" and even "ls" will usually work well.
         The default is to use the lpa cost functional together with the edge
         method.
    
         The edge image is different in a couple ways from the LPA/C correlation.
         First it is a different function, essentially only a standard deviation
         over a neighborhood, and then normalized by the absolute value of the
         mean - effectively a spatial variance (or square root of the variance).
         The second difference is that while the LPA/C cost functions also operates
         on local neighborhoods, those neighborhoods are 3-dimensional and set by
         a neighborhood size set in mm. The shape of the neighborhoods are
         dodecahedrons (12-side figures) that cover the volume. The edge method
         instead computes the neighborhoods at each voxel, and the neighborhoods
         are only two-dimensional - just the voxel and its 8 neighbors in x and y,
         presumed to be in the same slice rather than across slices. That's for
         both speed in computation and to remove effects of interpolation or false
         edges across the relatively distant slices.
    
         Although not as rigorously tested as the LPC method, this edge method
         seems to give similar results most of the time. The method does have a few
         disadvantages compared to the LPC/LPA methods. First, the AddEdge
         visualization in this script does not support this well (effectively,
         showing edges of edges). Second, the edge method does not provide
         three-dimensional edge detection directly. Many times this is an advantage,
         but if the data has particularly fine slicing in the z-direction, or the
         data has been resampled, this method may not work as well. Also the method
         uses an automask to reduce the data so that outside edges do not drive
         the alignment. The five voxel layer was only empirically found to be
         useful for this, but may, in fact, be problematic for small partial volumes
         or for surface coil data where much of the data may be in the area that
         is masked away.
         
         The edge method makes no assumption about contrasts between images. Only
         that edges of features will overlap - the same feature we use visually to
         verify alignment. This makes it appropriate for both similar and differing
         modality datasets.
         
         Both the LPA/LPC and the edge methods require internal features to be
         present and mostly corresponding in both input datasets. In some cases,
         this correspondence is not available for aligning some kinds of data with
         an anatomical references - low-contrast EPI data, radiopharmaceutical PET
         data targeting specific function, derived parameters from modeling.
         In these cases, fine alignment is not possible, but alternative cost
         functions like mutual information or least squares can provide a rough
         alignment of the contours.
    
         -output_dir dirname : the default output will put the result in
         the current directory even if the anat and epi datasets are in other 
         directories. If a directory is specified here, output data including
         temporary output data will be placed in that directory. If a new directory 
         is specified, that directory will be created first.
         
        Other obscure and experimental options that should only be handled with 
           care, lest they get out, are visible with -option_help.
    
        Examples:
          # align anat to sub-brick 5 of epi+orig. In addition, do slice timing
          # correction on epi+orig and register all sub-bricks to sub-brick 5
          # (Sample data files are in AFNI_data4/sb23 in sample class data)
          # Note the intermediate file, the volume registered EPI dataset,
          # is **not** saved unless the -save_vr option is also used.
          # See the -save_xxx option section of this help for more information.
    
          align_epi_anat.py -anat sb23_mpra+orig -epi epi_r03+orig     \
                            -epi_base 5
          
          # Instead of aligning the anatomy to an epi, transform the epi
          # to match the anatomy. Transform other epi run datasets to be
          # in alignment with the first epi datasets and with the anatomical
          # reference dataset. Note that all epi sub-bricks from all runs
          # are transformed only once in the process, combining volume
          # registration and alignment to the anatomical dataset in a single
          # transformation matrix
    
          align_epi_anat.py -anat sb23_mpra+orig -epi epi_r03+orig      \
                            -epi_base 5 -child_epi epi_r??+orig.HEAD    \
                            -epi2anat -suffix al2anat
          
          # Bells and whistles:
          # - create Talairach transformed epi datasets (still one transform)
          # - do not execute, just show the commands that would be executed.
          #   These commands can be saved in a script or modified.
          # The Talairach transformation requires auto-Talairaching 
          # the anatomical dataset first (cf. @auto_tlrc script)
    
          @auto_tlrc -base ~/abin/TT_N27+tlrc -input sb23_mpra+orig
          align_epi_anat.py -anat sb23_mpra+orig -epi epi_r03+orig      \
                            -epi_base 6 -child_epi epi_r??+orig.HEAD    \
                            -ex_mode dry_run -epi2anat -suffix _altest  \
                            -tlrc_apar sb23_mpra_at+tlrc
    
    
        Our HBM 2008 abstract describing the alignment tools is available here:
          https://afni.nimh.nih.gov/sscc/rwcox/abstracts
        
        Reference:
           If you find the EPI to Anat alignment capability useful, the paper to
           cite is:
           
           ZS Saad, DR Glen, G Chen, MS Beauchamp, R Desai and RW Cox.
           A new method for improving functional-to-structural alignment using
           local Pearson correlation. NeuroImage, 44:839-848, 2009.
           http://dx.doi.org/10.1016/j.neuroimage.2008.09.037
    
    
    A full list of options for align_epi_anat.py:
    
       -epi                
          use:                EPI dataset to align or to which to align
       -dset2              
          use:                dataset to align or to which to align
       -anat               
          use:                Anatomical dataset to align or to which to align
       -dset1              
          use:                Dataset to align or to which to align
       -keep_rm_files      
          use:                Don't delete any of the temporary files created here
       -prep_only          
          use:                Do preprocessing steps only without alignment
       -help               
          use:                The main help describing this program with options
       -limited_help       
          use:                The main help without all available options
       -option_help        
          use:                Help for all available options
       -version            
          use:                Show version number and exit
       -ver                
          use:                Show version number and exit
       -verb               
          use:                Be verbose in messages and options
       -save_script        
          use:                save executed script in given file
       -align_centers      
          use:                align centers of datasets based on spatial
                              extents of the original volume
          allowed:            yes, no, on, off
          default:            no
       -anat_has_skull     
          use:                Do not skullstrip anat dataset
          allowed:            yes, no
       -epi_strip          
          use:                Method to remove skull for EPI data
          allowed:            3dSkullStrip, 3dAutomask, None
       -dset1_strip        
          use:                Method to remove skull for dset1 data
          allowed:            3dSkullStrip, 3dAutomask, None
       -dset2_strip        
          use:                Method to remove skull for dset2 data
          allowed:            3dSkullStrip, 3dAutomask, None
       -volreg_method      
          use:                Time series volume registration method
                              3dvolreg: rigid body least squares
                              3dWarpDrive: 12 parameter least squares
                              3dAllineate: 12 parameter LPA cost function
                              
          allowed:            3dvolreg, 3dWarpDrive, 3dAllineate
          default:            3dvolreg
       -ex_mode            
          use:                Command execution mode.
                              quiet: execute commands quietly
                              echo: echo commands executed
                              dry_run: only echo commands
                              
          allowed:            quiet, echo, dry_run, script
          default:            script
       -overwrite          
          use:                Overwrite existing files
       -big_move           
          use:                Large movement between epi and anat.
                              Uses twopass option for 3dAllineate.
                              Consider cmass options, giant_move,
                              ginormous_move or -align_centers
       -giant_move         
          use:                Even larger movement between epi and anat.
                              Uses twopass option for 3dAllineate.
                              cmass options and wide angles and shifts
       -ginormous_move     
          use:                Adds align_centers to giant_move
       -rigid_body         
          use:                Do only rigid body alignment - shifts and rotates
       -partial_coverage   
          use:                partial_xxxx options control center of mass adjustment
       -partial_axial      
       -partial_coronal    
       -partial_sagittal   
       -AddEdge            
          use:                Run @AddEdge script to create double-edge images
       -Allineate_opts     
          use:                Options passed to 3dAllineate.
          default:            -weight_frac 1.0 -maxrot 6 -maxshf 10 -VERB -warp aff -source_automask+4 
       -perc               
          default:            90
       -suffix             
          default:            _al
       -cost               
       -multi_cost         
          use:                can use multiple cost functionals (lpc,lpa,nmi,....
                              See 3dAllineate -HELP for the full list
                              
       -check_cost         
          use:                Verify alignment against another method
                              Can use multiple cost functionals (lpc,lpa,nmi,....
                              See 3dAllineate -HELP for the full list
                              
       -epi2anat           
          use:                align EPI dataset to anat dataset
       -anat2epi           
          use:                align anat dataset to EPI dataset (default)
       -dset2to1           
          use:                align dset2 dataset to dset1 dataset
       -dset1to2           
          use:                align dset1 dataset to dset2 dataset (default)
       -epi_base           
          use:                Base sub-brick to use for alignment
                              Choose sub-brick number or statistic type
                              Valid choices can be, for example, 0,5,mean
       -dset2_base         
          use:                Base sub-brick to use for alignment
                              Choose sub-brick number or statistic type
                              Valid choices can be, for example, 0,5,mean
       -volreg_base        
          use:                Base to use for volume registration
                              Choose sub-brick number or statistic type
                              Valid choices can be, for example, 0,5,median
       -volreg             
          allowed:            on, off
       -volreg_opts        
          default:            -cubic
       -tshift             
          allowed:            on, off
       -tshift_opts        
       -deoblique          
          allowed:            on, off
       -deoblique_opts     
       -resample           
          allowed:            on, off
       -prep_off           
          use:                turn off all pre-processing steps including
                              deoblique, tshift, volreg and resample
       -cmass              
          use:                center of mass options for 3dAllineate
                              Valid options include cmass+a, cmass+xy, nocmass
                              
       -tlrc_apar          
          use:                If this is set, the results will include +tlrc
                              template transformed datasets for the epi aligned
                              to the anatomical combined with this additional
                              transformation to template of this parent dataset
                              The result will be EPI_al+tlrc.HEAD
                              
       -tlrc_epar          
          use:                Not available yet.
                              If this is set, the results will include +tlrc
                              template transformed datasets for the anatomical
                              aligned to the epi combined with this additional
                              transformation to template of this parent dataset
                              The result will be ANAT_al+tlrc.HEAD
                              
       -auto_tlrc          
          use:                Not available yet.
                              If this is set, the results will also be aligned
                              to a template using the @auto_tlrc script.
                              Transformations computed from that will be combined
                              with the anat to epi transformations and epi to anat
                              (and volreg) transformations
                              0nly one of the -tlrc_apar, -tlrc_epar or the 
                              -auto_tlrc options may be used
                              
       -child_epi          
          use:                Names of child EPI datasets
       -child_dset2        
          use:                Names of children of dset2 datasets
       -child_anat         
          use:                Names of child anatomical datasets
       -child_dset1        
          use:                Names of children of dset1 datasets
       -master_epi         
          use:                -master grid resolution for epi to anat alignment
                              MIN_DXYZ uses the smallest dimension
                              Other options are SOURCE and BASE as in 3dAllineate
                              help. For cases where either dataset is oblique, the
                              default becomes MIN_DXYZ
       -master_dset2       
          use:                -master grid resolution for epi to anat alignment
                              MIN_DXYZ uses the smallest dimension
                              Other options are SOURCE and BASE as in 3dAllineate
                              help. For cases where either dataset is oblique, the
                              default becomes MIN_DXYZ
       -master_tlrc        
          use:                -master grid resolution for epi to tlrc anat
                              alignment
                              MIN_DXYZ uses the smallest dimension
                              Other options are SOURCE and BASE as in 3dAllineate
                              help
       -master_anat        
          use:                -master grid resolution for anat to epi output
                              MIN_DXYZ uses the smallest dimension
                              Other options are SOURCE, BASE, 'n' mm or gridset
       -master_dset1       
          use:                -master grid resolution for dset1 to dset2 output
                              MIN_DXYZ uses the smallest dimension
                              Other options are SOURCE, BASE, 'n' mm or gridset
       -master_anat_dxyz   
          use:                -master grid resolution size (cubic only)
                              
       -master_dset1_dxyz  
          use:                -master grid resolution size (cubic only)
                              
       -master_epi_dxyz    
          use:                -master grid resolution (cubic only)
                              
       -master_dset2_dxyz  
          use:                -master grid resolution (cubic only)
                              
       -master_tlrc_dxyz   
          use:                -master grid resolution (cubic only)
                              
       -pre_matrix         
          use:                Apply an initial transformation from a 1D file.
                              For example, this file may be one generated by 
                              @Align_Centers. The transformation will be applied
                              to the anatomical data before aligning to the EPI
                              instead of using the built-in obliquity matrices,
                              if any
       -post_matrix        
          use:                Apply an additional transformation from a 1D file.
                              This transformation will be applied to the anatomical
                              data after alignment with the EPI. This will be
                              applied similarly to the tlrc transformation and in
                              place of it.
                              Output datasets are kept in the 'orig' view
       -skullstrip_opts    
          use:                Alternate options for 3dSkullstrip.
                              like -rat or -blur_fwhm 2
       -feature_size       
          use:                Minimal size in mm of structures in images to match.
                              Changes options for 3dAllineate for the coarse
                              blurring and lpc/lpa neighborhood sizes.May be useful
                              for rat brains, anat to anat and other
                              'non-standard' alignment
       -rat_align          
          use:                Set options appropriate for rat data - 
                              namely skullstrip and feature size options above.
                              
       -output_dir         
          use:                Set directory for output datasets
                              
       -edge               
          use:                Use internal edges to do alignment
       -edge_erodelevel    
          use:                Number of layers to remove for edge method
       -check_flip         
          use:                Check if L/R flipping gives better results
       -flip_giant         
          use:                use giant_move on flipped data even if not used
                              on original data
       -save_Al_in         
          use:                Save datasets used as input to 3dAllineate
       -save_vr            
          use:                Save motion-corrected epi dataset
       -save_tsh           
          use:                Save time-series corrected dataset
       -save_skullstrip    
          use:                Save unaligned, skullstripped dataset
       -save_orig_skullstrip
          use:                Save simply skullstripped dataset
       -save_epi_ns        
          use:                Save unaligned, skullstripped EPI dataset
       -save_rep           
          use:                Save unaligned representative tstat EPI dataset
       -save_resample      
          use:                Save unaligned EPI dataset resampled to anat grid
       -save_all           
          use:                Save all optional datasets
       -pow_mask           
          use:                power for weighting 1 or 2
          default:            1.0
       -bin_mask           
          use:                convert weighting mask to 0 or 1 - Unused
          allowed:            yes, no
          default:            no
       -box_mask           
          use:                Unused
          allowed:            yes, no
          default:            no
       -mask               
          use:                Not available yet.
                              Mask to apply to data.
          default:            vent
