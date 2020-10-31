#!/usr/bin/env python
# way to run script example
# python ~/afni/src/python_scripts/align_epi_anat.py \
#   -anat anat+orig -epi epi_r1+orig -base_epi median -ex_mode dry_run
#
# more examples

# align_epi_anat.py -anat anat+orig -epi epi+orig -epi_base 5

# align_epi_anat.py -anat sb23_mpra+orig -epi epi_r03+orig \
#   -epi2anat -ex_mode dry_run -epi_base 6 -child_epi epi_r??+orig.HEAD \
#   -anat2epi -epi2anat -tlrc_apar sb23_mpra_at+tlrc -suffix _alx2

import sys
import copy
from time import asctime

# AFNI modules
from afnipy.afni_base import *
from afnipy.afni_util import *
from afnipy.option_list import *
from afnipy.db_mod import *
from afnipy import ask_me

g_help_string = """
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
         3dAllineate -cubic -1Dmatrix_apply epi_r1_al_mat.aff12.1D  \\
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

    -rigid_body   Limit transformation to translation and rotation,
                  no scaling or shearing.
    -rigid_equiv  Compute alignment with full affine 12 parameters, but
                  use only the translation and rotation parameters. Useful
                  for axialization/AC-PC alignment to a template 
   
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

      align_epi_anat.py -anat sb23_mpra+orig -epi epi_r03+orig     \\
                        -epi_base 5
      
      # Instead of aligning the anatomy to an epi, transform the epi
      # to match the anatomy. Transform other epi run datasets to be
      # in alignment with the first epi datasets and with the anatomical
      # reference dataset. Note that all epi sub-bricks from all runs
      # are transformed only once in the process, combining volume
      # registration and alignment to the anatomical dataset in a single
      # transformation matrix

      align_epi_anat.py -anat sb23_mpra+orig -epi epi_r03+orig      \\
                        -epi_base 5 -child_epi epi_r??+orig.HEAD    \\
                        -epi2anat -suffix al2anat
      
      # Bells and whistles:
      # - create Talairach transformed epi datasets (still one transform)
      # - do not execute, just show the commands that would be executed.
      #   These commands can be saved in a script or modified.
      # The Talairach transformation requires auto-Talairaching 
      # the anatomical dataset first (cf. @auto_tlrc script)

      @auto_tlrc -base ~/abin/TT_N27+tlrc -input sb23_mpra+orig
      align_epi_anat.py -anat sb23_mpra+orig -epi epi_r03+orig      \\
                        -epi_base 6 -child_epi epi_r??+orig.HEAD    \\
                        -ex_mode dry_run -epi2anat -suffix _altest  \\
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

"""   

#        Also, because input volumes are preprocessed before using 3dAllineate,
#        the script outputs copies of the preprocessed volumes as they were used
#        in 3dAllineate.
#
#         EPI_epi_in_3dAl_al+orig : EPI volume for 3dAllineate's -base
#         ANAT_epi_in_3dAl_al+orig: ANAT volume for 3dAllineate's -input
#         EPI_wt_3dAl_al+orig     : weight volume for 3dAllineate's -weight
 
#   -cost          : cost function used by 3dAllineate. Default is lpc, anything
#                    else is inferior!
#    -cmass cmass+ss: center of mass option for 3dAllineate 
#                     ('cmass+a','cmass+xy','nocmass',...) Default is cmass+a.
#    -child_anat dset1 dset2 ... : specify other anatomical datasets to align.
#        The anatomical data will be aligned first to the parent
#                 structural dataset. If aligning to EPI data, then the
#                 transformation of the parent anatomical to the EPI data will
#                 be combined with the inter-structural transformation.
#
#     -fresh      : remove any temporary files at start from a previous run
#    Weighting mask options:
#    A weighting mask is used in the alignment. The default weighting mask
#    is the stripped epi dataset normalized from 0 to 1.
#    -pow_mask n.n  : raise the epi masked dataset to a power before normalizing
#                     (default is 1.0)

#    -tlrc_epar epi_template_dset : EPI  dataset that has been aligned to
#                  a master template such as a tlrc dataset If this option
#                  is supplied, then an anat+tlrc dataset will be created.



## BEGIN common functions across scripts (loosely of course)
class RegWrap:
   def __init__(self, label):
      self.align_version = "1.62" # software version (update for changes)
      self.label = label
      self.valid_opts = None
      self.user_opts = None
      self.verb = 1    # a little talkative by default
      self.save_script = '' # save completed script into given file
      self.rewrite = 0 #Do not recreate existing volumes
      self.oexec = "" #dry_run is an option
      self.epi2anat = 0 # align epi to anat optionally
      self.anat2epi = 1 # align anat to epi by default
      self.rmrm = 1   # remove temporary files
      self.prep_only = 0  # do preprocessing only
      self.odir = os.getcwd()    
      self.align_centers = 0    # don't align centers
      self.tshift_flag = 1  # do time shifting on EPI
      self.volreg_flag = 0  # don't do volume registration on EPI by default
      self.resample_flag = 1 # do resample
      self.deoblique_flag = 1  # deoblique datasets first
      self.deoblique_opt = "" # deobliquing/obliquing options
      self.skullstrip_opt = "" # skullstripping options
      self.epistrip_opt = "" # similar stripping/automask options for EPI/dset2
      self.cmass = "nocmass" # no center of mass option for 3dAllineate
      self.epi_base = None  # don't assume representative epi
      self.reg_mat = "" # volume registration matrix 1D file
      self.obl_a2e_mat = ""  # oblique anat to epi matrix
      self.edge = 0        # don't use edges for alignment
      self.edgelevels = 5  # number of outer layers to remove in edge method
      self.cost = ''       # assign cost below
      self.epi_dir = ''    # assign path from EPI (dset2) dataset's path
      self.anat_dir = ''   # assign path from anat (dset1) dataset's path
      self.output_dir = '' # user assigned path for anat and EPI
      self.flip = 0        # don't test for left/right flipping
      self.flip_giant = 0  # don't necessarily use giant_move for L-R flipping test
      self.giant_move = 0  # don't use giant_move
      self.supersize = 0   # don't use supersize
      self.rigid_equiv = 0 # don't do rigid_body equivalent for alignment
            
      # options for saving temporary datasets permanently
      self.save_Al_in = 0  # don't save 3dAllineate input files
      self.save_tsh = 0    # don't save tshifted epi
      self.save_vr = 0     # don't save volume registered epi
      self.save_skullstrip = 0 # don't save skullstripped (not aligned)
      self.save_origstrip  = '' # prefix for simple skullstripped dset
      self.save_rep = 0     # don't save representative tstat epi
      self.save_resample = 0 # don't save resampled epi
      self.save_epi_ns = 0  # don't save skull-stripped epi

      self.master_epi_option = '-master SOURCE'
      self.master_tlrc_option = '-master SOURCE'  # changed to min dimension below
      self.master_anat_option = ''

      self.dset1_generic_name = 'anat'
      self.dset2_generic_name = 'epi'
      
      return

# box, bin and fat mask are not used for now

   def init_opts(self):
      
      self.valid_opts = OptionList('init_opts')
       
      self.valid_opts.add_opt('-epi',  1, [], \
               helpstr="EPI dataset to align or to which to align")
      self.valid_opts.add_opt('-dset2',  1, [], \
               helpstr="dataset to align or to which to align")

      self.valid_opts.add_opt('-anat', 1, [], \
               helpstr="Anatomical dataset to align or to which to align")
      self.valid_opts.add_opt('-dset1', 1, [], \
               helpstr="Dataset to align or to which to align")

      self.valid_opts.add_opt('-keep_rm_files', 0, [], \
               helpstr="Don't delete any of the temporary files created here")
      self.valid_opts.add_opt('-prep_only', 0, [], \
               helpstr="Do preprocessing steps only without alignment")
      self.valid_opts.add_opt('-help', 0, [], \
               helpstr="The main help describing this program with options")
      self.valid_opts.add_opt('-limited_help', 0, [], \
               helpstr="The main help without all available options")
      self.valid_opts.add_opt('-option_help', 0, [], \
               helpstr="Help for all available options")
      self.valid_opts.add_opt('-version', 0, [], \
               helpstr="Show version number and exit")
      self.valid_opts.add_opt('-ver', 0, [], \
               helpstr="Show version number and exit")
      self.valid_opts.add_opt('-verb', 1, [], \
               helpstr="Be verbose in messages and options" )
      # 26 Nov 2012 [rickr]
      self.valid_opts.add_opt('-save_script', 1, [], \
               helpstr="save executed script in given file" )

      self.valid_opts.add_opt('-align_centers', 1, ['no'], ['yes', 'no', 'on', 'off'],  \
               helpstr="align centers of datasets based on spatial\n"      \
                       "extents of the original volume")
      self.valid_opts.add_opt('-anat_has_skull', 1, [], ['yes', 'no'],\
               helpstr="Do not skullstrip anat dataset")
      self.valid_opts.add_opt('-epi_strip', 1, [],           \
                              ['3dSkullStrip', '3dAutomask', 'None'],      \
               helpstr="Method to remove skull for EPI data")
      self.valid_opts.add_opt('-dset1_strip', 1, [],           \
                              ['3dSkullStrip', '3dAutomask', 'None'],      \
               helpstr="Method to remove skull for dset1 data")
      self.valid_opts.add_opt('-dset2_strip', 1, [],           \
                              ['3dSkullStrip', '3dAutomask', 'None'],      \
               helpstr="Method to remove skull for dset2 data")

      self.valid_opts.add_opt('-volreg_method', 1, ['3dvolreg'], \
                              ['3dvolreg', '3dWarpDrive', '3dAllineate'],  \
                      helpstr="Time series volume registration method\n"   \
                              "3dvolreg: rigid body least squares\n"       \
                              "3dWarpDrive: 12 parameter least squares\n"  \
                              "3dAllineate: 12 parameter LPA cost function\n")
      self.valid_opts.add_opt('-ex_mode', 1, ['script'],                   \
                              ['quiet', 'echo', 'dry_run', 'script'],      \
                              helpstr="Command execution mode.\n"          \
                                       "quiet: execute commands quietly\n" \
                                       "echo: echo commands executed\n"    \
                                       "dry_run: only echo commands\n" )

      self.valid_opts.add_opt('-overwrite', 0, [],\
                               helpstr="Overwrite existing files")
      self.valid_opts.add_opt('-big_move', 0, [], \
               helpstr="Large movement between epi and anat.\n"           \
                       "Uses twopass option for 3dAllineate.\n"           \
                       "Consider cmass options, giant_move,\n"            \
                       "ginormous_move or -align_centers")
      self.valid_opts.add_opt('-giant_move', 0, [], \
               helpstr="Even larger movement between epi and anat.\n"     \
                       "Uses twopass option for 3dAllineate.\n"           \
                       "cmass options and wide angles and shifts")
      self.valid_opts.add_opt('-ginormous_move', 0, [], \
               helpstr="Adds align_centers to giant_move")
      self.valid_opts.add_opt('-supersize', 0, [], \
               helpstr="Large scaling difference - up to 50%")


      self.valid_opts.add_opt('-rigid_body', 0, [], \
               helpstr="Do only rigid body alignment - shifts and rotates")
      self.valid_opts.add_opt('-rigid_equiv', 0, [], \
               helpstr="Do only rigid body equivalent alignment - shifts and rotates")

      self.valid_opts.add_opt('-partial_coverage', 0, [],                  \
               helpstr="partial_xxxx options control center of mass adjustment")
      self.valid_opts.add_opt('-partial_axial', 0, [])
      self.valid_opts.add_opt('-partial_coronal', 0, [])
      self.valid_opts.add_opt('-partial_sagittal', 0, [])
      self.valid_opts.add_opt('-AddEdge', 0, [], helpstr=                  \
                              "Run @AddEdge script to create double-edge images")

      self.valid_opts.add_opt('-Allineate_opts', -1,                       \
                             ["-weight_frac 1.0 -maxrot 6 -maxshf 10 -VERB"\
                              " -warp aff -source_automask+4 "],\
                               helpstr="Options passed to 3dAllineate.")


      self.valid_opts.add_opt('-perc', 1, ['90'])
#      self.valid_opts.add_opt('-fresh', 0, [])
      self.valid_opts.add_opt('-suffix', 1,['_al'])
      self.valid_opts.add_opt('-cost', 1,[])
#      self.valid_opts.add_opt('-fat', 1, ['1'])
      self.valid_opts.add_opt('-multi_cost', -1,[], \
           helpstr = "can use multiple cost functionals (lpc,lpa,nmi,....\n" \
               "See 3dAllineate -HELP for the full list\n")
      self.valid_opts.add_opt('-check_cost', -1,[], \
           helpstr = "Verify alignment against another method\n"
               "Can use multiple cost functionals (lpc,lpa,nmi,....\n" \
               "See 3dAllineate -HELP for the full list\n")

      # transform anat to epi by default, but allow the other way
      # the resulting transformation will be done at the end to include
      #  any volreg and oblique transformations
      self.valid_opts.add_opt('-epi2anat', 0, [], \
               helpstr = "align EPI dataset to anat dataset")
      self.valid_opts.add_opt('-anat2epi', 0, [], \
               helpstr = "align anat dataset to EPI dataset (default)")
      self.valid_opts.add_opt('-dset2to1', 0, [], \
               helpstr = "align dset2 dataset to dset1 dataset")
      self.valid_opts.add_opt('-dset1to2', 0, [], \
               helpstr = "align dset1 dataset to dset2 dataset (default)")
      
      # select base EPI dataset type
      self.valid_opts.add_opt('-epi_base', 1, [], [],                  \
              helpstr = "Base sub-brick to use for alignment\n"        \
                        "Choose sub-brick number or statistic type\n"  \
                        "Valid choices can be, for example, 0,5,mean")
#                   ['0', 'sub-brick-n', 'mean', 'median', 'max'])
      self.valid_opts.add_opt('-dset2_base', 1, [], [],                  \
              helpstr = "Base sub-brick to use for alignment\n"        \
                        "Choose sub-brick number or statistic type\n"  \
                        "Valid choices can be, for example, 0,5,mean")

      # select base EPI type for volume registration
      self.valid_opts.add_opt('-volreg_base', 1, [], [],               \
              helpstr = "Base to use for volume registration\n"        \
                        "Choose sub-brick number or statistic type\n"  \
                        "Valid choices can be, for example, 0,5,median")

      # do volume registration of EPI as part of this whole mess
      self.valid_opts.add_opt('-volreg', 1, [], ['on','off'])
      self.valid_opts.add_opt('-volreg_opts', -1, ["-cubic"])
 
      # do time shifting
      self.valid_opts.add_opt('-tshift', 1, [], ['on','off'])
      self.valid_opts.add_opt('-tshift_opts', -1, [])

      # obliquity options
      self.valid_opts.add_opt('-deoblique', 1, [], ['on','off'])
      self.valid_opts.add_opt('-deoblique_opts', -1, [])

      # resampling epi to anat
      self.valid_opts.add_opt('-resample', 1, [], ['on', 'off'])
      
      # turn off all pre-processing steps
      self.valid_opts.add_opt('-prep_off', 0, [], \
              helpstr = "turn off all pre-processing steps including\n" \
                        "deoblique, tshift, volreg and resample")
      # 3dAllineate cmass options
      self.valid_opts.add_opt('-cmass', 1, [], [], \
        helpstr = "choose center of mass options for 3dAllineate\n"
         "Center of mass shifts the center of the datasets to match\n"
         "by computing the weighted centers of each.\n"
         "For partial data, this may be too far in one direction\n"
         "See 3dAllineate help for details\n"
         "Valid options include cmass+a, cmass+xy, nocmass\n"
         "nocmass = no center of mass shift - default\n" 
         "cmass = center of mass shift - used with giant, ginormous_move\n"
         "cmass+a = automatic center of mass for partial data\n"
         "cmass+xy,xz,yz = automatic center of mass for partial\n"
         "                 axial,coronal,sagittal\n"
         "For partial data, it may be easier to select one\n"
         " of the partial_... options above" )

      # talairach transformed anatomical parent dataset
      self.valid_opts.add_opt('-tlrc_apar', 1, [], \
         helpstr="If this is set, the results will include +tlrc\n"
                 "template transformed datasets for the epi aligned\n"
                 "to the anatomical combined with this additional\n"
                 "transformation to template of this parent dataset\n"
                 "The result will be EPI_al+tlrc.HEAD\n")

      # talairach transformed EPI parent dataset
      self.valid_opts.add_opt('-tlrc_epar', 1, [], \
         helpstr="Not available yet.\n"
                 "If this is set, the results will include +tlrc\n"
                 "template transformed datasets for the anatomical\n"
                 "aligned to the epi combined with this additional\n"
                 "transformation to template of this parent dataset\n"
                 "The result will be ANAT_al+tlrc.HEAD\n")

      # auto_talairach results
      self.valid_opts.add_opt('-auto_tlrc', 0, [], \
         helpstr="Not available yet.\n"
                 "If this is set, the results will also be aligned\n"
                 "to a template using the @auto_tlrc script.\n"
                 "Transformations computed from that will be combined\n"
                 "with the anat to epi transformations and epi to anat\n"
                 "(and volreg) transformations\n"
                 "0nly one of the -tlrc_apar, -tlrc_epar or the \n"
                 "-auto_tlrc options may be used\n")
      # child epi datasets
      self.valid_opts.add_opt('-child_epi', -1,[],\
                               helpstr="Names of child EPI datasets")
      self.valid_opts.add_opt('-child_dset2', -1,[],\
                               helpstr="Names of children of dset2 datasets")

      # child anat datasets
      self.valid_opts.add_opt('-child_anat', -1,[],\
                               helpstr="Names of child anatomical datasets")
      self.valid_opts.add_opt('-child_dset1', -1,[],\
                               helpstr="Names of children of dset1 datasets")

      # master resampling options for alignment
      self.valid_opts.add_opt('-master_epi', 1,[],\
             helpstr="-master grid resolution for epi to anat alignment\n"
                    "MIN_DXYZ uses the smallest dimension\n"
                    "Other options are SOURCE and BASE as in 3dAllineate\n"
                    "help. For cases where either dataset is oblique, the\n"
                    "default becomes MIN_DXYZ")
      self.valid_opts.add_opt('-master_dset2', 1,[],\
             helpstr="-master grid resolution for epi to anat alignment\n"
                    "MIN_DXYZ uses the smallest dimension\n"
                    "Other options are SOURCE and BASE as in 3dAllineate\n"
                    "help. For cases where either dataset is oblique, the\n"
                    "default becomes MIN_DXYZ")

      self.valid_opts.add_opt('-master_tlrc', 1,[],\
             helpstr="-master grid resolution for epi to tlrc anat\n"
                    "alignment\n"
                    "MIN_DXYZ uses the smallest dimension\n"
                    "Other options are SOURCE and BASE as in 3dAllineate\n"
                    "help")
                    
      self.valid_opts.add_opt('-master_anat', 1,[],\
             helpstr="-master grid resolution for anat to epi output\n"
                    "MIN_DXYZ uses the smallest dimension\n"
                    "Other options are SOURCE, BASE, 'n' mm or gridset")
      self.valid_opts.add_opt('-master_dset1', 1,[],\
             helpstr="-master grid resolution for dset1 to dset2 output\n"
                    "MIN_DXYZ uses the smallest dimension\n"
                    "Other options are SOURCE, BASE, 'n' mm or gridset")

      self.valid_opts.add_opt('-master_anat_dxyz', -1,[],\
             helpstr="-master grid resolution size (cubic only)\n")
      self.valid_opts.add_opt('-master_dset1_dxyz', -1,[],\
             helpstr="-master grid resolution size (cubic only)\n")

      self.valid_opts.add_opt('-master_epi_dxyz', -1,[],\
             helpstr="-master grid resolution (cubic only)\n")
      self.valid_opts.add_opt('-master_dset2_dxyz', -1,[],\
             helpstr="-master grid resolution (cubic only)\n")

      self.valid_opts.add_opt('-master_tlrc_dxyz', -1,[],\
             helpstr="-master grid resolution (cubic only)\n")


      # apply pre/post-transformation matrice
      self.valid_opts.add_opt('-pre_matrix', 1, [], \
         helpstr="Apply an initial transformation from a 1D file (NB:\n"
                 "not from a *.aff12.1D file); the *.1D file should\n"
                 "contain a 3x4 matrix of numbers.\n"
                 "For example, this file may be one generated by \n"
                 "@Align_Centers, or if inverting a matrix, with:\n"
                 "  cat_matvec mat.aff12.1D -I > mat_INV.1D\n"
                 "The transformation will be applied to the\n"
                 "anatomical data before aligning to the EPI\n"
                 "instead of using the built-in obliquity matrices,\n"
                 "if any")
      self.valid_opts.add_opt('-post_matrix', 1, [], \
         helpstr="Apply an additional transformation from a 1D file.\n"
                 "This transformation will be applied to the anatomical\n"
                 "data after alignment with the EPI. This will be\n"
                 "applied similarly to the tlrc transformation and in\n"
                 "place of it.\n"
                 "Output datasets are kept in the 'orig' view")
      self.valid_opts.add_opt('-skullstrip_opts', -1, [], \
               helpstr="Alternate options for 3dSkullstrip.\n"
                       "like -rat or -blur_fwhm 2")
      self.valid_opts.add_opt('-dset1strip_opts', -1, [], \
               helpstr="Alternate name for skullstrip_opts")
      self.valid_opts.add_opt('-epistrip_opts', -1, [], \
               helpstr="Alternate options for 3dSkullstrip/3dAutomask.\n"
                       "like -rat or -blur_fwhm 2 or -peels 2")
      self.valid_opts.add_opt('-dset2strip_opts', -1, [], \
               helpstr="Alternate name for epistrip_opts")
      self.valid_opts.add_opt('-feature_size', 1, [],\
            helpstr="Minimal size in mm of structures in images to match.\n"\
                    "Changes options for 3dAllineate for the coarse\n" \
                    "blurring and lpc/lpa neighborhood sizes.May be useful\n" \
                    "for rat brains, anat to anat and other\n" \
                    "'non-standard' alignment")
      self.valid_opts.add_opt('-rat_align', 0, [],\
               helpstr="Set options appropriate for rat data - \n"
                       "namely skullstrip and feature size options above.\n")
      self.valid_opts.add_opt('-output_dir', 1,[],\
             helpstr="Set directory for output datasets\n")

      # create edge images
      # do edge-based alignment
      self.valid_opts.add_opt('-edge', 0, [],\
               helpstr="Use internal edges to do alignment")
      self.valid_opts.add_opt('-edge_erodelevel', 1, [],\
               helpstr="Number of layers to remove for edge method")

      self.valid_opts.trailers = 0   # do not allow unknown options
      
      self.valid_opts.add_opt('-check_flip', 0, [], \
               helpstr="Check if L/R flipping gives better results")
      self.valid_opts.add_opt('-flip_giant', 0, [], \
               helpstr="use giant_move on flipped data even if not used\n"\
                       "on original data")

      # saving optional output datasets
      # save datasets used as input to 3dAllineate
      self.valid_opts.add_opt('-save_Al_in', 0, [],    \
               helpstr = "Save datasets used as input to 3dAllineate")
      # save vr dataset
      self.valid_opts.add_opt('-save_vr', 0, [],    \
               helpstr = "Save motion-corrected epi dataset")
      # save timeshifted dataset
      self.valid_opts.add_opt('-save_tsh', 0, [],    \
               helpstr = "Save time-series corrected dataset")
      # save skullstripped anat before alignment
      self.valid_opts.add_opt('-save_skullstrip', 0, [],    \
               helpstr = "Save unaligned, skullstripped dataset")
      # save skullstripped anat before alignment and without oblique xform
      self.valid_opts.add_opt('-save_orig_skullstrip', 1, [],    \
               helpstr = "Save simply skullstripped dataset")
      # save skullstripped epi before alignment
      self.valid_opts.add_opt('-save_epi_ns', 0, [],    \
               helpstr = "Save unaligned, skullstripped EPI dataset")
      # save representative epi tstat epi before alignment
      self.valid_opts.add_opt('-save_rep', 0, [],    \
               helpstr = "Save unaligned representative tstat EPI dataset")
      # save resampled epi dataset before alignment
      self.valid_opts.add_opt('-save_resample', 0, [],    \
               helpstr = "Save unaligned EPI dataset resampled to anat grid")

      # save all the optional datasets
      self.valid_opts.add_opt('-save_all', 0, [],    \
               helpstr = "Save all optional datasets")

      
      # weighting mask options
      self.valid_opts.add_opt('-pow_mask', 1, ['1.0'], \
               helpstr = "power for weighting 1 or 2")
      self.valid_opts.add_opt('-bin_mask', 1, ['no'], ['yes', 'no'], \
               helpstr = "convert weighting mask to 0 or 1 - Unused")
      self.valid_opts.add_opt('-box_mask', 1, ['no'], ['yes', 'no'], \
               helpstr = "Unused")

      self.valid_opts.add_opt('-mask', -1, ['vent'], \
               helpstr="Not available yet.\n"
                       "Mask to apply to data.")

  
  
   def dry_run(self):
      if self.oexec != "dry_run":
         return 0
      else:
         return 1
        
   def apply_initial_opts(self, opt_list):
      opt1 = opt_list.find_opt('-version') # user only wants version
      opt2 = opt_list.find_opt('-ver') 
      if ((opt1 != None) or (opt2 != None)):
         # ps.version()
         ps.ciao(0)   # terminate 
      opt = opt_list.find_opt('-verb')    # set and use verb
      if opt != None: self.verb = int(opt.parlist[0])

      opt = opt_list.find_opt('-save_script') # save executed script
      if opt != None: self.save_script = opt.parlist[0]
            
      opt = opt_list.find_opt('-ex_mode')    # set execute mode
      if opt != None: self.oexec = opt.parlist[0]

      opt = opt_list.find_opt('-keep_rm_files')    # keep temp files
      if opt != None: self.rmrm = 0

      opt = opt_list.find_opt('-prep_only')    # preprocessing only
      if opt != None: self.prep_only = 1
            
      opt = opt_list.find_opt('-help')    # does the user want help?
      if opt != None:
         ps.self_help(2)   # always give full help now by default
         ps.ciao(0)  # terminate

      opt = opt_list.find_opt('-limited_help')  # less help?
      if opt != None:
         ps.self_help()
         ps.ciao(0)  # terminate

      opt = opt_list.find_opt('-option_help')  # help for options only
      if opt != None:
         ps.self_help(1)
         ps.ciao(0)  # terminate
         
      opt = opt_list.find_opt('-perc')    # set and use percentile for weight
      if opt != None: self.perc = float(opt.parlist[0])
      
      opt = opt_list.find_opt('-suffix')    
      if opt != None: 
          self.suffix = opt.parlist[0]
          if((opt=="") or (opt==" ")) :
            self.error_msg("Cannot have blank suffix")
            ps.ciao(1);

      opt = opt_list.find_opt('-cost')    
      if opt != None: self.cost = opt.parlist[0]
      else: self.cost = ''

      opt = opt_list.find_opt('-pow_mask')    
      if opt != None: self.sqmask = opt.parlist[0]
      
      opt = opt_list.find_opt('-box_mask')    
      if opt != None: self.boxmask = opt.parlist[0]
      
      opt = opt_list.find_opt('-bin_mask')    
      if opt != None: self.binmask = opt.parlist[0]

      if (opt_list.find_opt('-epi2anat') or
          opt_list.find_opt('-dset2to1') ):    # align epi to anat
         self.epi2anat = 1
         self.anat2epi = 0     # turn off anat to epi unless requested
         self.volreg_flag = 1  # turn on motion correction/volume registration
         if(opt_list.find_opt('-anat2epi') or
            opt_list.find_opt('-dset1to2')):    # align anat to epi
            self.anat2epi = 1

      opt = opt_list.find_opt('-prep_off')    # turn off all preprocessing steps
      if opt != None: 
          self.deoblique_flag = 0
          self.tshift_flag = 0
          self.volreg_flag = 0
          self.resample_flag = 0
          self.info_msg( \
          "turning off deobliquing tshift, volume registration, resampling")
          # note - individual flags can be turned on, if desired

      opt = opt_list.find_opt('-deoblique')    # deoblique data
      if opt != None: 
          if(opt.parlist[0]=='on'):
              self.deoblique_flag = 1
          elif(opt.parlist[0]=='off'):
              self.deoblique_flag = 0
          else:
              self.error_msg("deoblique option not on/off")
              self.ciao(1)

      opt = opt_list.find_opt('-tshift')    # do time shifting
      if opt != None: 
          if(opt.parlist[0]=='on'):
              self.tshift_flag = 1
          elif(opt.parlist[0]=='off'):
              self.tshift_flag = 0
          else:
              self.error_msg("tshift option not on/off")
              self.ciao(1)

      opt = opt_list.find_opt('-volreg')    # do volume registration
      if opt != None: 
          if(opt.parlist[0]=='on'):
              self.volreg_flag = 1
              self.info_msg("turning on volume registration")
          elif(opt.parlist[0]=='off'):
              self.volreg_flag = 0
              self.info_msg("turning off volume registration")
          else:
              self.error_msg("volreg option not on/off");
              self.ciao(1)

      opt = opt_list.find_opt('-resample')    # resample epi to anat
      if opt != None: 
          if(opt.parlist[0]=='on'):
              self.resample_flag = 1
          elif(opt.parlist[0]=='off'):
              self.resample_flag = 0
              self.info_msg("turning off resampling")
          else:
              self.error_msg("resample option not on/off");
              self.ciao(1)

      opt = opt_list.find_opt('-check_flip')       # check for left/right flipping
      if opt != None: 
         self.flip = 1
         self.save_epi_ns = 1                      # save EPI for QC, even if anat to epi 

      # optional data to save
      opt = opt_list.find_opt('-save_Al_in')       # save 3dAllineate input datasets
      if opt != None: self.save_Al_in = 1
      opt = opt_list.find_opt('-save_tsh')         # save timeshifted data
      if opt != None: self.save_tsh = 1
      opt = opt_list.find_opt('-save_vr')          # save volume registered epi data
      if opt != None: 
         self.save_vr = 1
         opt = opt_list.find_opt('-volreg')        # if volreg has not been set
         if opt == None:  self.volreg_flag = 1     # turn on volreg processing
      opt = opt_list.find_opt('-save_skullstrip')  # save unaligned skullstripped
      if opt != None: self.save_skullstrip = 1
      # save unaligned, unobliqued dataset
      opt = opt_list.find_opt('-save_orig_skullstrip')
      if opt != None:
         val, err = opt_list.get_string_opt('', opt=opt)
         if val == None or err:
            ps.self_help()
            ps.ciao(0)  # terminate
         self.save_origstrip = val
      opt = opt_list.find_opt('-save_epi_ns')      # save unaligned skullstripped epi
      if opt != None: self.save_epi_ns = 1
      opt = opt_list.find_opt('-save_rep')         # save unaligned representative epi
      if opt != None: self.save_rep = 1
      opt = opt_list.find_opt('-save_resample')    # save unaligned resampled epi
      if opt != None: self.save_resample = 1

      opt = opt_list.find_opt('-save_all')  # save all optional output datasets
      if ((opt != None) or self.prep_only):
         self.save_Al_in = 1      # save 3dAllineate input files
         self.save_tsh = 1        # save tshifted epi           
         self.save_vr = 1         # save volume registered epi  
         self.save_skullstrip = 1 # save skullstripped (not aligned)
         self.save_rep = 1        # save representative tstat epi
         self.save_resample = 1   # save resampled epi
         self.save_epi_ns = 1     # save skull-stripped epi


     
   def get_user_opts(self):
      self.valid_opts.check_special_opts(sys.argv) #ZSS March 2014
      self.user_opts = read_options(sys.argv, self.valid_opts)
      if self.user_opts == None: return 1 #bad
      # no options: apply -help
      if ( len(self.user_opts.olist) == 0 or \
           len(sys.argv) <= 1 ) :
         ps.self_help()
         ps.ciao(0)  # terminate
      if self.user_opts.trailers:
         opt = self.user_opts.find_opt('trailers')
         if not opt: 
             print("** ERROR: seem to have trailers, but cannot find them!")
         else:
             print("** ERROR: have invalid trailing args: %s", opt.show())
         return 1  # failure

      # apply the user options
      if self.apply_initial_opts(self.user_opts): return 1

      if self.verb > 3: 
         self.show('------ found options ------ ')    

      return
    
   def show(self, mesg=""):
      print('%s: %s' % (mesg, self.label))
      if self.verb > 2: self.valid_opts.show('valid_opts: ')
      self.user_opts.show('user_opts: ')
   
   def info_msg(self, mesg=""):
       if(self.verb >= 1) :
          print("#++ %s" % mesg)

   def error_msg(self, mesg=""):
       print("#**ERROR %s" % mesg)

   def exists_msg(self, dsetname=""):
       print("** Dataset: %s already exists" % dsetname)
       print("** Not overwriting.")
       if(not ps.dry_run()):
           self.ciao(1)
       
   def ciao(self, i):
      if i > 0:
         print("** ERROR - script failed")
      elif i==0:
         print("")

      os.chdir(self.odir)

      if self.save_script:
         write_afni_com_history(self.save_script)

      # return status code
      sys.exit(i)
      
   # save the script command arguments to the dataset history
   def save_history(self, dset, exec_mode):
      self.info_msg("Saving history")  # sounds dramatic, doesn't it?
      cmdline = args_as_command(sys.argv, \
                 '3dNotes -h "', '" %s' % dset.input())
      com = shell_com(  "%s\n" % cmdline, eo=exec_mode)
      com.run()

   # show help
   # if help_level is 1, then show options help only
   # if help_level is 2, then show main help and options help
   def self_help(self, help_level=0):
      if(help_level!=1) :
         print(g_help_string)
      if(help_level):  
         print("A full list of options for %s:\n" % ps.label)
         for opt in self.valid_opts.olist:
            print("   %-20s" % (opt.name ))
            if (opt.helpstr != ''):
               print("   %-20s   %s" % \
                  ("   use:", opt.helpstr.replace("\n","\n   %-20s   "%' ')))
            if (opt.acceptlist):
               print("   %-20s   %s" % \
                  ("   allowed:" , str.join(', ', opt.acceptlist)))
            if (opt.deflist):
               print("   %-20s   %s" % \
                  ("   default:",str.join(' ', opt.deflist)))
      return 1
   
   # remove all the temporary files for epi and anat base names
   def cleanup(self, rmold=0):
      opt = self.user_opts.find_opt('-epi')
      if opt == None :
         opt = self.user_opts.find_opt('-dset2')
      e = afni_name(opt.parlist[0]) 
      e.to_afni(new_view=dset_view(e.ppve()))
      opt = self.user_opts.find_opt('-anat')
      if opt == None :
         opt = self.user_opts.find_opt('-dset1')
      a = afni_name(opt.parlist[0])
      a.to_afni(new_view=dset_view(a.ppve()))

      self.fresh_start( \
          ("%s" % (e.out_prefix())), \
          ("%s" % (a.out_prefix())), rmold = rmold, \
          epipath = ps.output_dir, anatpath = ps.output_dir)
      return 1

   def version(self):
      self.info_msg("align_epi_anat version: %s" % self.align_version)

   # copy dataset 1 to dataset 2
   # show message and check if dset1 is the same as dset2
   # return non-zero error if can not copy
   def copy_dset(self, dset1, dset2, message, exec_mode):
      self.info_msg(message)
      if(dset1.input()==dset2.input()):
         print("# copy is not necessary")
         return 0
#      if((os.path.islink(dset1.p())) or (os.path.islink(dset2.p()))):
      if(dset1.real_input() == dset2.real_input()):
         print("# copy is not necessary")
         return 0
      ds1 = dset1.real_input()
      ds2 = dset2.real_input()
      ds1s = ds1.replace('/./','/')
      ds2s = ds2.replace('/./','/')
      if(ds1s == ds2s):
          print("# copy is not necessary - both paths are same")
          return 0
      print("copying from dataset %s to %s" % (dset1.input(), dset2.input()))
      dset2.delete(exec_mode)
      com = shell_com(  \
            "3dcopy %s %s" % (dset1.input(), dset2.out_prefix()), eo=exec_mode)
      com.run()
      if ((not dset2.exist())and (exec_mode!='dry_run')):
         print("** ERROR: Could not rename %s\n" % dset1.input())
         return 1
      return 0
   
   

## BEGIN script specific functions   
   def process_input(self):
      #Do the default test on all options entered. 
      #NOTE that default options that take no parameters will not go 
      #through test, but that is no big deal
      for opt in self.user_opts.olist:
         if (opt.test() == None): ps.ciao(1)

      # skull stripping is on by default for anat/dset1
      opt = self.user_opts.find_opt('-anat_has_skull')
      if opt != None and opt.parlist[0] == 'no':
          ps.skullstrip = 0
      else:
          ps.skullstrip = 1
          ps.skullstrip_method = '3dSkullStrip'
          opt = self.user_opts.find_opt('-dset1_strip')
          if(opt!=None):
              ps.skullstrip_method = opt.parlist[0]
              if(ps.skullstrip_method=='None'):
                  ps.skullstrip = 0

      # skull stripping is on by default for epi / dset2
      opt = self.user_opts.find_opt('-epi_strip')
      if opt != None :
          ps.epi_strip_method = opt.parlist[0]
      else:
          ps.epi_strip_method = '3dSkullStrip'
          opt = self.user_opts.find_opt('-dset2_strip')
          if(opt!=None):
              ps.epi_strip_method = opt.parlist[0]

      #Allineate extras
      opt = self.user_opts.find_opt('-Allineate_opts')
      if opt != None: 
         ps.AlOpt = str.join(' ', opt.parlist)
      else:
         ps.AlOpt = ''

      # rigid body alignment
      opt = self.user_opts.find_opt('-rigid_body')
      if opt != None:
         ps.AlOpt = ps.AlOpt.replace('-warp aff', '-warp shift_rotate')

      # rigid body alignment
      opt = self.user_opts.find_opt('-rigid_equiv')
      if opt != None:
         ps.rigid_equiv = 1

      opt = self.user_opts.find_opt('-feature_size')
      if opt != None:
         featuresize = float(opt.parlist[0])
         blursize = 2.0 * featuresize
         aljoin = '-twoblur %f -blok "RHDD(%f)"' % (blursize, featuresize)
         ps.AlOpt = "%s %s" % (ps.AlOpt, aljoin)
      else:
         featuresize = 0.0

      opt = self.user_opts.find_opt('-rat_align')
      if opt != None:
         if featuresize == 0.0 :
            featuresize = 0.5
            blursize = 1.0
            aljoin = '-twoblur %f -blok "RHDD(%f)"' % (blursize, featuresize)
            ps.AlOpt = "%s %s" % (ps.AlOpt, aljoin)
         ps.skullstrip_opt = "-rat"         

      #big_move?
      opt1 = self.user_opts.find_opt('-big_move')
      #giant_move?
      opt2 = self.user_opts.find_opt('-giant_move')
      #ginormous move?
      opt3 = self.user_opts.find_opt('-ginormous_move')
      if(not (opt1 or opt2 or opt3)):
         ps.AlOpt = "%s -onepass " % ps.AlOpt
      else:
         # resampling has the potential to cut off data
         # the cmass flag is used by 3dAllineate to align the center of mass
         # first and then resamples
         self.resample_flag = 0  
         
      if(opt1):
         ps.AlOpt = "%s -twopass " % ps.AlOpt
         
      if(opt2 or opt3):
         if featuresize  == 0.0 :
            fsize = 1
         else:
            fsize = featuresize
            
         ps.AlOpt =  \
         "%s -twobest 11 -twopass -VERB -maxrot 45 -maxshf 40 " \
         "-fineblur %s -source_automask+2" % (ps.AlOpt, fsize)
         ps.cmass = "cmass"
         ps.giant_move = 1
      else :
         ps.giant_move = 0
      if(opt3): # ginormous option
         ps.align_centers = 1

      #supersize - allow for large scale changes
      opt = self.user_opts.find_opt('-supersize')
      if(opt):
         ps.AlOpt =  \
         "%s -maxscl 1.5" % ps.AlOpt
       
      #giant_move?
      opt = self.user_opts.find_opt('-flip_giant')
      if(opt): ps.flip_giant = 1;
         
      #add edges
      opt = self.user_opts.find_opt('-AddEdge')
      if opt == None:
         ps.AddEdge = 0
      else:
         ps.AddEdge = 1

      
      #get anat and epi

      opt = self.user_opts.find_opt('-epi')
      if opt == None: 
         opt = self.user_opts.find_opt('-dset2')
         if opt == None:
            print("** ERROR: Must use -epi or -dset2 option\n")
            return 0
         if self.cost == '':
            self.cost = 'lpa'   # make default cost lpa for dset1/2 terminology
         self.dset_prep_off()
         self.epi_base = "0"      # align to 0th sub-brick
         self.dset1_generic_name = 'dset1'
         self.dset2_generic_name = 'dset2'

      e = afni_name(opt.parlist[0]) 
      ps.epi = e
      opt = self.user_opts.find_opt('-anat')
      if opt == None: 
         opt = self.user_opts.find_opt('-dset1')
         if opt == None:
            print("** ERROR: Must use -anat or -dset1 options\n")
            ps.ciao(1)
         if self.cost == '':
            self.cost = 'lpa'   # make default cost lpa for dset1/2 terminology
         self.dset_prep_off()   # assume no motion correction, slice timing correction,...
         self.epi_base = "0"      # align to 0th sub-brick
         self.dset1_generic_name = 'dset1'
         self.dset2_generic_name = 'dset2'

      a = ps.anat0 = afni_name(opt.parlist[0])
      
      # if cost has not been set by using dset1/2 or cost options
      if self.cost == '' :
         self.cost = 'lpc'      # set default cost to lpc

#      if ps.user_opts.find_opt('-fresh'):
#         ps.fresh_start(e.out_prefix(), a.out_prefix())
         
      #epi input
      if not e.exist():
         print( "** ERROR: Could not find epi / dset2 dataset\n   %s " 
             % e.input())
         ps.ciao(1)
         
      #anat input
      if not a.exist():
         print("** ERROR: Could not find anat / dset1 dataset\n   %s "
             % a.input())
         ps.ciao(1)

      #get 3dTshift options
      opt = self.user_opts.find_opt('-tshift_opts')
      if opt != None: 
         ps.tshift_opt = str.join(' ', opt.parlist)
      else:
         ps.tshift_opt = '-cubic'

      #get 3dvolreg options
      opt = self.user_opts.find_opt('-volreg_opts')
      if opt != None: 
         ps.reg_opt = str.join(' ', opt.parlist)
      else:
         ps.reg_opt = ''

      #get 3dSkullstrip options
      opt = self.user_opts.find_opt('-skullstrip_opts')
      if opt != None: 
         ps.skullstrip_opt = str.join(' ',opt.parlist)

      opt = self.user_opts.find_opt('-dset1strip_opts')
      if opt != None: 
         ps.skullstrip_opt = str.join(' ',opt.parlist)

      opt = self.user_opts.find_opt('-epistrip_opts')
      if opt != None: 
         ps.epistrip_opt = str.join(' ',opt.parlist)

      opt = self.user_opts.find_opt('-dset2strip_opts')
      if opt != None: 
         ps.epistrip_opt = str.join(' ',opt.parlist)

      #get epi base type for alignment (specific sub-brick/median/mean)
      opt = self.user_opts.find_opt('-epi_base')
      if opt == None:
         opt = self.user_opts.find_opt('-dset2_base')
         if opt == None:
            if(self.epi_base==None):
               ps.error_msg("Must use -epi_base or -dset2_base options")
               ps.ciao(1)
         else:
            ps.epi_base = opt.parlist[0]
      else:
         ps.epi_base = opt.parlist[0]
         
      #get volreg_base (matches epi_base by default)
      opt = self.user_opts.find_opt('-volreg_base')
      if opt != None: 
         self.volreg_flag = 1
         ps.volreg_base = opt.parlist[0]
      else:
         ps.volreg_base = ps.epi_base

# may not need this and only the epi_base parameter instead
      #get 3dTstat options
      opt = self.user_opts.find_opt('-tstat_opts')
      if opt != None: 
         ps.tstat_opt = str.join(' ', opt.parlist)
      else:
         ps.tstat_opt = ''

      #check for various center of mass options
      optc = self.user_opts.find_opt('-cmass')
      if optc == None :
         #if no cmass option entered, partial coverage?
         cmass_opts = 0
         opt = self.user_opts.find_opt('-partial_coverage')
         if opt != None:
            ps.cmass = 'cmass+a'
            cmass_opts += 1
         opt = self.user_opts.find_opt('-partial_axial')
         if opt != None:
            ps.cmass = 'cmass+xy'
            cmass_opts += 1
         opt = self.user_opts.find_opt('-partial_coronal')
         if opt != None:
            ps.cmass = 'cmass+xz'
            cmass_opts += 1
         opt = self.user_opts.find_opt('-partial_sagittal')
         if opt != None:
            ps.cmass = 'cmass+yz'
            cmass_opts += 1
         if cmass_opts > 1:
            self.error_msg("Can only use a single partial_xxx coverage option")
      else:
         ps.cmass = optc.parlist[0]

      #get talairached anatomical dataset
      opt = self.user_opts.find_opt('-tlrc_apar')
      if opt != None: 
         anat_tlrc = afni_name(opt.parlist[0]) 
         ps.tlrc_apar = anat_tlrc
         if not anat_tlrc.exist():
            self.error_msg("Could not find anat talairach template dataset\n" \
                  "  %s " %  anat_tlrc.out_prefix())
         else:
            self.info_msg("Talairach transformed anatomical: %s" % \
                        (anat_tlrc.input()))
         if not ps.epi2anat:
            self.error_msg("No talairach transformation performed unless aligning epi2anat")
      else :
         ps.tlrc_apar = ""
      opt = self.user_opts.find_opt('-tlrc_epar')
      if opt != None: 
         at = afni_name(opt.parlist[0]) 
         ps.tlrc_epar = at
         if not at.exist():
            self.error_msg("Could not find epi talairach template dataset\n %s "
                  % at.input())
      else :
         ps.tlrc_epar = ""


      #get pre-transformation matrix
      opt = self.user_opts.find_opt('-pre_matrix')
      if opt != None: 
         ps.pre_matrix = opt.parlist[0]
      else :
         ps.pre_matrix = ""

      #align_centers to get pre-transformation matrix
      opt = self.user_opts.find_opt('-align_centers')
      if opt != None:  # shouldn't happen, defaults to no
         if((opt.parlist[0]=='yes') or (opt.parlist[0]=='on')):
             ps.align_centers = 1
             self.info_msg("turning on align_centers")
             if ps.pre_matrix != "" :
                self.error_msg(
                 "Can not use both align_centers and pre-matrix transformations")

      #get post-transformation matrix
      opt = self.user_opts.find_opt('-post_matrix')
      if opt != None: 
         ps.post_matrix = opt.parlist[0]
      else :
         ps.post_matrix = ""

      # check on the children
      ps.child_epis = self.user_opts.find_opt('-child_epi')
      if (ps.child_epis == None):
          ps.child_epis = self.user_opts.find_opt('-child_dset2')
      
      if ps.child_epis != None: 
         self.info_msg("-child_epi or child_dset2 option given")
         if (not ps.epi2anat) :
            if (self.save_Al_in or self.save_tsh or self.save_vr or self.save_rep or
                self.save_resample or self.save_epi_ns):
                self.info_msg(
                 "Child epis / dset2 will be processed although not aligning\n"
                 "epi to anat datasets with -epi2anat / -dset2to1 because a\n"
                 "save preprocessing option such as -save_vr is selected")
                for child_epi_name in ps.child_epis.parlist:
                   child_epi = afni_name(child_epi_name) 
                   # it's 11:00, do you know where your children are?
                   if not child_epi.exist():
                      self.error_msg("Could not find child epi/dset2 %s\n"
                            % child_epi.input())
                   else:
                      self.info_msg("Found child epi/dset2 %s\n" 
                            % child_epi.input())

            else :
                self.error_msg(
                  "Child epis / dset2 are not processed unless aligning epi\n"
                  "to anat datasets with -epi2anat / -dset2to1 or a save\n"
                  "preprocessing option such as -save_vr is also selected")
                ps.child_epis = None

      ps.child_anats = self.user_opts.find_opt('-child_anat')
      if ps.child_anats == None:
         ps.child_anats = self.user_opts.find_opt('-child_dset1')

      if ps.child_anats != None: 
         self.info_msg("-child_anat option given")
         if (not ps.anat2epi) :
            self.error_msg(
                 "child anat datasets are not processed unless aligning anat\n"
                 "to epi or dset1to2 datasets")
            ps.child_anats = None
         else :
            for child_anat_name in ps.child_anats.parlist:
               child_anat = afni_name(child_anat_name) 
               # it's 11:00, do you know where your children are?
               if not child_anat.exist():
                  self.error_msg("Could not find child anat / dset1\n %s "
                        % child_anat.input())
               else:
                  self.info_msg(
                    "Found child anat / dset1 %s\n" % child_anat.input())

      # output resolution options 
      # EPI output bounding box and voxel size
      min_d =  self.min_dim_dset(ps.epi)
      mast_dxyz = "" # set default output 
      mast_dset = "SOURCE"
      if ps.giant_move == 1 :
         mast_dset = "BASE"
         mast_dxyz = '-mast_dxyz %f' % min_d
         
      opt = self.user_opts.find_opt('-master_epi')  # epi to anat resolution
      if opt == None:
          opt = self.user_opts.find_opt('-master_dset2')  # dset2to1 resolution

      if opt != None: 
          if(opt.parlist[0]!='MIN_DXYZ'):
              if(isFloat(opt.parlist[0])):
                 min_d = float(opt.parlist[0])
                 mast_dxyz = '-mast_dxyz %f' % min_d
              else:
                 mast_dset = opt.parlist[0]
                 if(opt.parlist[0]!='BASE') and (opt.parlist[0]!='SOURCE'):
                    mast_dxyz = ''
                 else:
                    if(opt.parlist[0]=='SOURCE'):
                       mast_dxyz = ''
                    else:   # for BASE, resample by default to min.dimension too
                       mast_dxyz = '-mast_dxyz %f' % min_d
                    

      opt = self.user_opts.find_opt('-master_epi_dxyz')
      if opt == None:
         opt = self.user_opts.find_opt('-master_dset2_dxyz')
      if opt != None:
         if(isFloat(opt.parlist[0])):
            mast_dxyz = '-mast_dxyz %s' % opt.parlist[0]
         else:
            self.info_msg("****Can not use dxyz setting. Use numbers only***")
            self.info_msg("Using default minimum dimension spacing")

      # set up 3dAllineate output grid - master dataset for bounding box and
      #  grid resolution
      if((mast_dset=="SOURCE") and ps.align_centers):
         self.master_epi_option = "%s" % mast_dxyz
         self.master_epi_3dAl_center = 1
      else:
         self.master_epi_option = \
            "-master %s %s" % (mast_dset, mast_dxyz)
         self.master_epi_3dAl_center = 0
      self.master_epi_dset = mast_dset

      # output resolution options 
      # EPI output in TLRC dataset's bounding box
      # voxel size is minimum dimension of EPI by default 
      min_d =  self.min_dim_dset(ps.epi)
      mast_dxyz = '-mast_dxyz %f' % min_d
      mast_dset = "BASE"
      opt = self.user_opts.find_opt('-master_tlrc')  # epi to tlrc resolution

      if opt != None: 
          if(opt.parlist[0]!='MIN_DXYZ'):
              if(isFloat(opt.parlist[0])):
                 min_d = float(opt.parlist[0])
                 mast_dxyz = '-mast_dxyz %f' % min_d
              else:
                 mast_dset = opt.parlist[0]
                 if(opt.parlist[0]!='BASE') and (opt.parlist[0]!='SOURCE'):
                    mast_dxyz = ''
                 else:
                    if(opt.parlist[0]=='SOURCE'):
                       mast_dxyz = ''
                    else:   # for BASE, resample by default to min.dimension too
                       mast_dxyz = '-mast_dxyz %f' % min_d

      opt = self.user_opts.find_opt('-master_tlrc_dxyz')
      if opt != None:
         if(isFloat(opt.parlist[0])):
            mast_dxyz = '-mast_dxyz %s' % opt.parlist[0]
         else:
            self.info_msg("****Can not use dxyz setting. Use numbers only***")
            self.info_msg("Using default minimum dimension spacing")

      # set up 3dAllineate output grid - master dataset for bounding box and
      #  grid resolution
      self.master_tlrc_option = \
          "-master %s %s" % (mast_dset, mast_dxyz) 
      self.master_tlrc_dset = mast_dset
      
      # anat oblique and 3dAllineate output resolution and bounding box
      min_d =  self.min_dim_dset(ps.anat0)
      mast_dxyz = ""    # set default output is same as original
      mast_dset = "SOURCE"
      if ps.giant_move:
         mast_dset = "BASE"
         mast_dxyz = '-mast_dxyz %f' % min_d
      
      self.master_anat_option = "-newgrid %f" % min_d
#      self.info_msg("Spacing for anat to EPI deobliquing is %f mm" % min_d)

      opt = self.user_opts.find_opt('-master_anat') 
      if  opt == None:
         opt = self.user_opts.find_opt('-master_dset1') 

      if opt != None: 
          if(opt.parlist[0]!='MIN_DXYZ'):
              if(isFloat(opt.parlist[0])):
                 min_d = float(opt.parlist[0])
                 self.master_anat_option = "-newgrid %f" % min_d
                 mast_dxyz = '-mast_dxyz %f' % min_d
                 self.info_msg("Spacing for %s to %s obliquing is %f mm" % \
                 (self.dset1_generic_name,self.dset2_generic_name, min_d))
              else: # master is not a voxel size, but a dataset
                 mast_dset = opt.parlist[0]
                 # master is a specific dataset name
                 if(opt.parlist[0]!='BASE') and (opt.parlist[0]!='SOURCE'):
                    self.master_anat_option = "-gridset %s" % opt.parlist[0]
                    mast_dxyz = ''
                 # master is 'SOURCE' or 'BASE'
                 else:
                    self.info_msg("****master anat option is %s" 
                                   % opt.parlist[0])
                    self.info_msg(
                    "****Can not apply BASE or SOURCE as master "
                    "for deobliquing. Using default min dim spacing");
                    if(opt.parlist[0]=='SOURCE'):
                       mast_dxyz = ''
                    else:
                       mast_dxyz = '-mast_dxyz %f' % min_d

      opt = self.user_opts.find_opt('-master_anat_dxyz')
      if opt == None:
         opt = self.user_opts.find_opt('-master_dset1_dxyz')
      if opt != None:
         if(isFloat(opt.parlist[0])):
            mast_dxyz = '-mast_dxyz %s' % opt.parlist[0]
         else:
            self.info_msg("****Can not use dxyz setting. Use numbers only***")
            self.info_msg("Using default min dim spacing")

      # set up 3dAllineate output grid - master dataset for bounding box and
      #  grid resolution
      if((mast_dset=="SOURCE") and ps.align_centers):
         self.master_anat_3dAl_option = "%s" % mast_dxyz
         self.master_anat_3dAl_center = 1
      else:
         self.master_anat_3dAl_option = \
            "-master %s %s" % (mast_dset, mast_dxyz)
         self.master_anat_3dAl_center = 0
      self.master_anat_dset = mast_dset
          
      #get deobliquing options
      opt = self.user_opts.find_opt('-deoblique_opts')
      if opt != None: 
         ps.deoblique_opt = str.join(' ',opt.parlist)
      else:
         ps.deoblique_opt = ''

      # user says it's okay to overwrite existing files 
      opt = self.user_opts.find_opt('-overwrite')
      if opt != None:
         ps.rewrite = 1

      opt = self.user_opts.find_opt('-edge')  # use internal edges to drive alignment
      if (opt != None):
         self.edge = 1
         opt = self.user_opts.find_opt('-edge_erodelevel')
         if opt == None:
            ps.erodelevel = 5
         else:
            ps.erodelevel = opt.parlist[0]
            
         # the cost function is not as important here because the preprocessing
         # steps accomodate for dataset differences - least squares,
         # mutual information or local pearson correlation are all good choices
         opt = self.user_opts.find_opt('-cost') 
         if opt == None: self.cost = 'lpa'  # local Pearson absolute correlation
         if(featuresize==0):            # set feature size if not already set
            featuresize = 0.5
            blursize = 2.0 * featuresize
            aljoin = '-twoblur %f -blok "RHDD(%f)"' % (blursize, featuresize)
            ps.AlOpt = "%s %s" % (ps.AlOpt, aljoin)

      opt = self.user_opts.find_opt('-check_cost')    
      if opt != None:
      
          self.checkcost = str.join(' ', opt.parlist)
      else: 
          self.checkcost = ""

      opt = self.user_opts.find_opt('-multi_cost')    
      if opt != None:
          self.multicost = opt.parlist
      else: 
          self.multicost = []
      self.multicost.append(self.cost)         # only the original cost
      for mcost in self.multicost:
          self.info_msg("Multi-cost is %s" % mcost)


      opt = self.user_opts.find_opt('-output_dir') # set alternative output directory
      if opt != None: 
         self.output_dir = opt.parlist[0]
         # end with a slash
         self.output_dir = "%s/" % os.path.realpath(self.output_dir)
         print("# User has selected a new output directory %s" % self.output_dir)
         com = shell_com(("mkdir %s" % self.output_dir), eo=self.oexec)
         com.run()
         print("cd %s" % self.output_dir)
         if(not self.dry_run()):
            os.chdir(self.output_dir)
      else :
         self.output_dir = "./"  # just the current directory
      self.anat_dir = self.output_dir
      self.epi_dir = self.output_dir

      # all inputs look okay  - this goes after all inputs. ##########
      return 1

   # turn off all preprocessing steps
   def prep_off(self) :
       self.deoblique_flag = 0
       self.tshift_flag = 0
       self.volreg_flag = 0
       self.resample_flag = 0
       self.info_msg("turning off deobliquing tshift, volume registration, resampling")
       return

   # turn off most preprocessing steps - keep deobliquing though
   def dset_prep_off(self) :
       self.tshift_flag = 0
       self.volreg_flag = 0
       self.resample_flag = 0
       self.info_msg("turning off tshift, volume registration, resampling")
       return


   # find smallest dimension of dataset in x,y,z
   def min_dim_dset(self, dset=None) :
       com = shell_com(  \
                "3dAttribute DELTA %s" % dset.input(), eo=ps.oexec,capture=1)
       com.run()
       if  ps.dry_run():
          return (1.234567)

       # new purty python way (donated by rick)
       min_dx = min([abs(float(com.val(0,i))) for i in range(3)])
       
       # old non-pythonesque way
       if 0:
          min_dx = 1000.0       # some high number to start
          i = 0
          while i < 3 :    # find the smallest of the 3 dimensions
             dx = abs(float(com.val(0,i)))
             if (dx<min_dx):
                 min_dx = dx
             i += 1    


       if(min_dx==0.0):
           min_dx = 1.0
       return (min_dx)

   
   # determine if dataset has time shifts in slices
   def tshiftable_dset( self, dset=None) :
       com = shell_com(  \
                "3dAttribute TAXIS_OFFSETS %s" % dset.input(), eo=ps.oexec,capture=1)
       com.run()
       if  ps.dry_run():
           return (1)
       if(len(com.so)): status = 1
       else: status = 0
       return (status)
     
   # determine if dataset is oblique
   def oblique_dset( self, dset=None) :
      com = shell_com(  \
        "3dinfo %s | \grep 'Data Axes Tilt:'|\grep 'Oblique'" % dset.input(),\
          eo=ps.oexec,capture=1)
      com.run()
      if  ps.dry_run():
          return (1)   # default for dry run is to assume oblique
      if(len(com.so)):
#        print("length of so is %d" % len(com.so))
#        oblstr = com.val(len(com.so),4)
#        if(oblstr=="Oblique"):
           self.info_msg( "Dataset %s is ***oblique****" % dset.input())
           return (1)
      self.info_msg( "Dataset %s is not oblique" % dset.input())
      return (0)  # if here, then not oblique


   # parse 1D file of multiple costs for a particular cost
   # the data are arranged like this
   ## 3dAllineate -allcostX1D results:
   ##  ___ ls   ___  ___ sp   ___  ___ mi   ___  ___ crM  ___  ___ nmi  ___  ___ je   ___  ___ hel  ___  ___ crA  ___  ___ crU  ___  ___ lss  ___  ___ lpc  ___  ___ lpa  ___  ___ lpc+ ___  ___ ncd  ___
   #      0.778644     0.410080    -0.108398     0.778821     0.989730    10.462562    -0.018096     0.882749     0.862151     0.221356    -0.009732     0.990268     0.522726     0.999801

   def get_cost(self, costfilename ,costfunction):
      # lpc+ZZ (or lpc+zz) evaluated at end of alignment as lpc
      if costfunction.lower() == "lpc+zz" :
          costfunction = "lpc"

      try:
         costfile = open(costfilename,'r')
         # ignore first line. That should just say "3dAllineate -allcostX1D results"
         costfile.readline()
         # read the list of costnames in the second line
         costnames = costfile.readline()
         # remove the underscores and the leading ##
         costnamelist = [(x) for x in costnames.split() if x != "___" and x != "#" and x != "##" ]
         # read the list of cost values
         costlist = costfile.readline()
         # convert into list structure
         costs = costlist.split()
         costfile.close()
         # make dictionary of names and costs
         costdict = dict(list(zip(costnamelist, costs)))

         # be sure cost function is in the dictionary
         if not costfunction in costnamelist:
            print("** Error processing cost list file %s" % costfilename)
            print("   cost '%s' not found in dict\n" \
                  " : %s" % (costfunction, costdict))

         # get cost value from dictionary (error handling if it doesn't exist in list)
         costvalue = float(costdict[costfunction])
         return (costvalue)
      except:
         print("ERROR: error reading cost list file")
      
      # find the cost name and then the corresponding value
      # for i, name in enumerate(costnamelist):
      #   if name == costfunction:
      #      costvalue = costs[i]
      #      return costvalue
      return (1000)



# align the anatomical data to the epi data using 3dAllineate
# this is the real meat of the program
# note for some of the reasoning:
# the output dataset is the result of the alignment of the anatomical to the EPI
# This is the default and the preferred output for several reasons despite 
# its not being standard to the usual processing in the past
# First, this does not require the EPI data to be resampled other than volume
# registration
# For medium to large differences of alignment, the EPI data resampled to its
# original grid may lose effective resolution. This is caused by the typical
# large difference in slice thickness relative to the EPI x,y voxel size within
# slice resolution.
# Secondly, the anatomical data is usually higher resolution and relatively
# isotropic in voxel dimensions
# Thirdly, the anatomical dataset is typically used for structural reference
# while the EPI voxel values matter for the analysis. Slight blurring of the
# anatomical data is relatively unimportant
# Fourthly, the anatomical dataset's higher resolution allows for finer
# structural alignment (versus downsampling the anatomical to match the EPI)
# One could get around these various issues by using the inverse transform as
# done in the epi2anat method and then resampling the output grid to a finer
# resolution, but this will not usually be necessary.   
  
   def align_anat2epi(  self, e=None, a=None, m=None, \
                        alopt=" -onepass -weight_frac 1.0 -maxrot 6 " \
                               "-maxshf 10 -VERB -warp aff ",\
                        suf = "_alnd_epi", costfunction = "lpc"):
                        #m is the weight brick
      self.info_msg( "Aligning %s data to %s data" % \
           (ps.dset1_generic_name, ps.dset2_generic_name ))
      # for oblique data or pre and post transformed data, save anat to epi transformation
      #   matrix in separate temporary 1D file
      if((ps.obl_a2e_mat!="") or (ps.pre_matrix!="")) or (ps.edge):
         o = a.new("%s_temp%s" % (a.out_prefix(), suf))
         self.anat_mat = "%s%s%s_e2a_only_mat.aff12.1D" %  \
            (o.p(), ps.anat0.out_prefix(), suf)
      else:
         # save transformation matrix with original anatomical name,suf,...
         self.anat_mat = "%s%s%s_mat.aff12.1D" %  (a.p(), ps.anat0.out_prefix(),suf)
         if (ps.anat2epi) or (ps.flip):
            o = a.new("%s%s" % (ps.anat0.out_prefix(), suf)) # save the permanent data
         else:
            o = a.new("__tt_%s%s" % (ps.anat0.out_prefix(), suf)) # save temporary copy
         
      ow = a.new("%s%s_wtal" % (a.out_prefix(), suf))
      of = a.new("%s_flip%s" % (ps.anat0.out_prefix(), suf))
      olr = a.new("__tt_%s_lr%s" % (ps.anat0.out_prefix(), suf))

      if(self.master_anat_dset=='BASE'):
          o.view = e.view
          ow.view = e.view
          of.view = e.view
          olr.view = e.view
      else:
          if(self.master_anat_dset=='SOURCE'):
             o.view = a.view
             ow.view = a.view
             of.view = a.view
             olr.view = a.view
          else:
             manat = afni_name(self.master_anat_dset)
             o.view = manat.view
             ow.view = manat.view
             of.view = manat.view
             olr.view = manat.view

      anatview = o.view

      if (not o.exist() or ps.rewrite or ps.dry_run()):
         o.delete(oexec=ps.oexec)
         ow.delete(oexec=ps.oexec)
         if m:
            wtopt = "-wtprefix %s%s -weight %s" % (ow.p(), ow.out_prefix(), m.input())
         else:
            wtopt = "-wtprefix %s%s " % (ow.p(),ow.out_prefix())
         if(ps.cmass==""):
            cmass = ""
         else:
            cmass = "-%s" % ps.cmass
         if(ps.checkcost==""):
            checkstr = ""
         else:
            checkstr = "-check %s" % ps.checkcost
         com = shell_com(  \
            "3dAllineate -%s "  # costfunction       \
             "%s "              # weighting          \
             "-source %s "        \
             "-prefix %s%s -base %s "                  \
             "%s "  # center of mass options (cmass) \
             "-1Dmatrix_save %s "                    \
             "%s %s %s "  # master grid, other 3dAllineate options \
                       #   (may be user specified)   \
             % (costfunction, wtopt, a.input(), o.p(), o.out_prefix(), \
               e.input(), cmass, self.anat_mat, self.master_anat_3dAl_option, \
               alopt, checkstr ), \
               eo=ps.oexec)
         # if this fails, notify the user
         if com.run():
            print("** 3dAllineate failure")
            return None, None
         e2a_mat = self.anat_mat

         if (ps.flip):
            com = shell_com( \
               "3dLRflip -prefix %s%s -overwrite %s" % \
               (olr.p(), olr.out_prefix(),a.input()), eo=ps.oexec)
            com.run()
            if((ps.flip_giant) and not(ps.giant_move)):
               alopt =  \
               "%s -twobest 11 -twopass -VERB -maxrot 45 -maxshf 40 " \
               "-source_automask+2" % (ps.AlOpt)
               ps.cmass = "cmass"
            # flips don't need weight            
            if m:
               wtopt = "-weight %s" % (m.input())
            else:
               wtopt = "" 

            # save transformation matrix with original anatomical name,suf,...
            self.flip_mat = "%s%s_flip_%s_mat.aff12.1D" % \
                  (a.p(), ps.anat0.out_prefix(),suf)
     
            com = shell_com( \
               "3dAllineate -%s "  # costfunction       \
               "%s "              # weighting          \
               "-source %s "        \
               "-prefix %s%s -base %s "                  \
               "%s "  # center of mass options (cmass) \
               "-1Dmatrix_save %s "                    \
               "%s %s %s "  # master grid, other 3dAllineate options \
                         #   (may be user specified)   \
               % (costfunction, wtopt, olr.input(), of.p(), of.out_prefix(), \
                 e.input(), cmass, self.flip_mat, self.master_anat_3dAl_option, \
                 alopt, checkstr ), \
                 eo=ps.oexec)
            com.run()

            com = shell_com( \
               "3dAllineate -allcostX1D IDENTITY __tt_lr_noflipcosts.1D "       \
               "%s "              # weighting          \
               "-source %s "        \
               "-base %s "                  \
               "%s "  # center of mass options (cmass) \
               "%s %s %s "  # master grid, other 3dAllineate options \
               % (wtopt, o.input(), e.input(), cmass, \
                  self.master_anat_3dAl_option, alopt, checkstr ), \
                 eo=ps.oexec)
            com.run()
            com = shell_com( \
               "3dAllineate -allcostX1D IDENTITY __tt_lr_flipcosts.1D "       \
               "%s "              # weighting          \
               "-source %s "                           \
               "-base %s "                             \
               "%s "  # center of mass options (cmass) \
               "%s %s %s "  # master grid, other 3dAllineate options \
               % (wtopt, of.input(), e.input(), cmass, \
                  self.master_anat_3dAl_option, alopt, checkstr ), \
                 eo=ps.oexec)
            com.run()

            noflipcost = self.get_cost("__tt_lr_noflipcosts.1D",costfunction)
            print("No flip cost is %f for %s cost function" % (noflipcost, costfunction))
            flipcost = self.get_cost("__tt_lr_flipcosts.1D",costfunction)
            print("Flip cost is %f for %s cost function" % (flipcost, costfunction))
            outname = 'aea_checkflip_results.txt'
            try: 
                f = open(outname, 'w')
                f.write("flip_cost_orig : %f\n" % noflipcost)
                f.write("flip_cost_flipped : %f\n" % flipcost)
                f.write("flip_cost_func : %s\n" % costfunction)
                f.write("flip_base: %s\n" %  e.pv())
                f.write("flip_dset_orig : %s\n" % o.pv())
                f.write("flip_dset_flipped : %s\n" % of.pv())
                if(flipcost < noflipcost):
                   print("WARNING: ************ flipped data aligns better than original data\n" \
                         "Check for left - right flipping in the GUI ************************")
                   f.write("flip_guess : DO_FLIP\n")
                else:
                   f.write("flip_guess : NO_FLIP\n")
                   print("Data does not need flipping")
                f.close()
            except:
                print("WARNING: Could not write to flip text file")

         # if rigid_equiv, then save only rigid equivalent alignment
         # extract with cat_matvec
         if(ps.rigid_equiv):
            self.info_msg( \
                "Reducing alignment transformation to rigid equivalent")
            com = shell_com(  \
                  "cat_matvec -ONELINE %s -P > __tt_temp.1D ; sleep 1; \
                  mv __tt_temp.1D %s" % \
                  (self.anat_mat,self.anat_mat), eo=ps.oexec)
            com.run();
 
         # if not doing alignment for anat2epi, just return now,
         # and use the xform matrix later
         if (not(ps.anat2epi)):
            return o, ow

         if((ps.obl_a2e_mat!="")  or ps.edge ) :
            # save the permanent data
            o = afni_name("%s%s%s%s" % \
               (ps.anat_dir, ps.anat0.out_prefix(), suf,ps.anat0.view)) 
            if (not o.exist() or ps.rewrite or ps.dry_run()):
               o.delete(oexec=ps.oexec)
            else:
               self.exists_msg(o.input())

            # for oblique and edge data, 
            # need to apply matrix to skullstripped anat
            if(ps.obl_a2e_mat!=""): 
               # overall transformation A to E is (E2A^-1 PreShift/Oblique)
               # 1Dmatrix_apply takes E to A as input so inverse
               #    E to A = Obl^-1 E2A  
               obl_mat = "%s -I" % ps.obl_a2e_mat
               e2a_mat = "%s%s%s_mat.aff12.1D" %  (a.p(), ps.anat0.out_prefix(),suf)
               # combine transformations
               # earliest transformation is last as input to cat_matvec
               com = shell_com(  \
                     "cat_matvec -ONELINE %s %s > %s" % \
                     (self.anat_mat, obl_mat, e2a_mat), eo=ps.oexec)
               com.run();
               self.info_msg( \
                   "Combining %s to %s and oblique transformations" %  \
                   (ps.dset1_generic_name, ps.dset2_generic_name ))

            else:   # just apply the matrix to the original data (edges)
               e2a_mat = self.anat_mat
               self.info_msg( "Applying transformation to skullstripped %s" % \
                          ps.dset1_generic_name)

            com = shell_com(  \
                  "3dAllineate -base %s -1Dmatrix_apply %s " \
                  "-prefix %s%s -input %s  %s %s"   %  \
                  ( e.input(), e2a_mat, o.p(),o.out_prefix(),\
                    ps.anat_ns0.input(),\
                    self.master_anat_3dAl_option, alopt ), eo=ps.oexec)

            com.run()
      else:
         self.exists_msg(o.input())
      # process the children
      if ps.child_anats != None: 
         if (ps.anat2epi):
            for child_anat_name in ps.child_anats.parlist:
               child_anat = afni_name(child_anat_name) 

               # skip the parent if it's included
               if(child_anat.input()==ps.anat0.input()) :
                  child_anat_out = afni_name("%s%s_child%s%s" % \
                    (child_anat.p(),child_anat.prefix,suf,child_anat.view))
               else:
                  child_anat_out=afni_name("%s%s%s%s" % \
                    (child_anat.p(),child_anat.prefix,suf,child_anat.view))
               
               child_anat_out.view = anatview     # child_anat.view
               self.info_msg("Processing child %s: %s" % \
                  (ps.dset1_generic_name, child_anat.ppv()))
               if (ps.rewrite) :
                  overwritestr = "-overwrite"
                  child_anat_out.delete(oexec=ps.oexec)
               else :
                  overwritestr = ""
               com = shell_com(  \
                     "3dAllineate -base %s -1Dmatrix_apply %s "          \
                     "-prefix %s%s -input %s  %s %s %s"   %              \
                     ( e.input(), e2a_mat,                          \
                       child_anat_out.p(), child_anat_out.out_prefix(),    \
                       child_anat.input(),                 \
                       self.master_anat_3dAl_option, alopt, overwritestr), \
                       eo=ps.oexec)

               com.run()

      return o, ow

# Some notes on this alignment matrix mechanics are sorely needed
#  because it gets somewhat involved and takes a good deal of time to explain
#  and to understand all the possible combinations of cases.
#  Let's first define a series of "spaces" or coordinate systems where 
#  datasets are in alignment with that particular space's original dataset.
#
#  ac = AnatCard (the usual original anatomical dataset without any obliquity
#                     transformation applied, in what we call cardinal or cardinalized space )
#  ao = AnatReal (the deobliqued version of the above transformed by its
#                     IJK_TO_DICOM_REAL transformation matrix)
#  ec = EPICard  (similar to AnatCard but the EPI cardinalized dataset, our starting dset)
#  eo = EPIReal  (similar to AnatReal but the deobliqued EPI dataset)
#  
# The usual case we calculate is the transformation from A_ac to A_ec for the anatomical dataset
#   or the reverse E_ec to E_ac for the epi dataset where the "subscript" signifies the space
# Each transformation is computed for a specific space:
#  A2E = A_ec2E_ec_base = the transformation computed by 3dAllineate to transform the anatomical in 
#                     the EPI's cardinal space to be in alignment with the EPI base sub-brick
#  E_ec_base = the EPI in its own cardinalized space. This is the sub-brick used by 3dAllineate 
#               and the one used for 3dAllineate and the one used as a reference for 3dvolreg
#
# The "oblique" transformation of the anatomical dataset that transforms the original cardinalized
#  anatomical dataset to the EPI's cardinalized space is done inside 3dWarp as
#  Obl = A_ac2A_ec = (T_eo T_ec^-1)^-1 (T_ao T_ac^-1)
# 

# The time-series motion correction volume registration can be described as a transformation
#  of the EPI sub-bricks to the EPI base sub-brick transformation
#  VR = E_ec_i2E_ec_base

# An additional initial shift or pre-transformation on the original anatomical dataset 
#  (such as produced by @Align_Centers)
#  In this implementation, we'll treat this transformation as a substitute for the oblique
#  transformation A_ac2A_ec

# One may also choose an additional post-transformation matrix like the one produced by
# 3dTagalign usually instead of a tlrc transformation. In this case, the tlrc transformation
# would be defined by the post transformation matrix instead of the Warp transformation
# in the tlrc_apar dataset's header. 

# Both the pre and  post transformation matrices could potentially be applied
# many different ways, but until there's a need for something different, let's use it only as
# a substitute for the obliquity and tlrc warp transformation matrices,respectively.

# There are the complications that the 1Dmatrix_save and 1Dmatrix_apply use the reverse of what
# you might expect, but at least they're consistent. The matrices that come from those options
# (used by both 3dvolreg and 3dAllineate) are the transformation of base to source 
# (output to input). So each of these transformations is the inverse of the transformation
# performed or how to get back to where you were. In this program, there are two usages
#   3dvolreg saves the VR^-1 transformation (Ebase->Eiec)
#   3dAllineate saves the E2A transformation (A2E^-1 or E_ec2A_ac)
# Applying also uses the same format of base to source,
#  so needs to be inverted from source to base
# A linear algebra reminder here is the inverse of the product of matrices is equal to 
#  the reverse order with each matrix inverted
# (A B)^-1 = B^-1 A^-1
# Also the order of operations is first transformation is on right, then proceeds to left
#   before inverting for 1Dmatrix_apply

# So the usual case (-anat2epi without any extra shift) is A_ac to A_ec
#   and may include oblique transformation
# Case 1: A_ac to E_ec
#  A_ac  (A_ac2A_ec) A_ec (A_ec2E_ec_base) A_ec_base = A_ec2E_ec A_ac2A_ec
#        = A2E Obl
# matrix_apply (inverse) = Obl^-1 E2A
#    E2A is the output of 3dAllineate of A2E

# Case 2: E_ec to A_ac (-epi2anat)
#  may also include obliquity and volume registration
#  E_ec_i _E_ec_i2E_ec_base_ E_ec_base _A_ec2E_ec^-1_ A_ec _A_ac2A_ec^-1_ A_ac 
#    = A_ac2A_ec^-1  A_ec2E_ec^-1 E_ec_i2E_ec_base
#    = Obl^-1 A2E^-1 VR
# matrix_apply (inverse) = VR^-1 A2E Obl = VR^-1 E2A^-1 Obl
#    E2A is the output of 3dAllineate of A2E, VR^-1 is output of 3dvolreg

#
# Case 3: E_ec_i to A_at (-epi2anat -tlrc_apar ...)
# talairach transformation of follower EPI to talairach anat)
#  E_ec_i (E_ec_i2E_ec_base) E_ec_base (A_ec2E_ec^-1) A_ec (A_ac2A_ec^-1) A_ac (A_ac2A_at) A_at
#    = A_ac2A_at A_ac2A_ec^-1  A_ec2E_ec^-1 E_ec_i2E_ec_base
#    = Tlrc Obl^-1 A2E^-1 VR
# matrix_apply (inverse) = VR^-1 A2E Obl Tlrc^-1 = VR^-1 E2A^-1 Obl Tlrc^-1
#    E2A is the output of 3dAllineate of A2E, VR^-1 is output of 3dvolreg
# 


   # align the epi to the anatomical but do it using the inverse 
   # transformation of the alignment of anat to epi
   def align_epi2anat(  self, e=None, a=None, \
        alopt="",\
        suf = "_alnd_anat"):
      # tlrc space output null by default 
      t = []
      self.info_msg(" Applying alignment for %s to %s" % (ps.dset2_generic_name, ps.dset1_generic_name))
 
      o = e.new("%s%s" % (self.epi_afniformat.out_prefix(), suf))
      o.path = ps.output_dir
#      o = afni_name("%s%s" % (self.epi.out_prefix(), suf)) # was e.out_prefix() here
      if(self.master_epi_dset == 'SOURCE'):
          o.view = "%s" % e.view
          if(not(o.view)) :
                 o.view = dset_view(self.master_epi_dset)   
#          self.info_msg("o.view is SOURCE VIEW %s\n" % o.view)
      else:
          if(self.master_epi_dset == 'BASE'):
             o.view = "%s" % a.view
             if(not(o.view)) :
                 o.view = dset_view(a.ppve())   
#             self.info_msg("o.view is BASE VIEW %s\n" % o.view)

          else:
             mepi = afni_name(self.master_epi_dset)
             o.view = mepi.view
             if(not(o.view)) :
                 o.view = dset_view(self.master_epi_dset)   
#             self.info_msg("o.view is OTHER VIEW %s\n" % o.view)

#      self.info_msg("o.view is %s\n" % o.view)
      if(not(o.view)) :
          o.view = '+orig'
      eview = "%s" % o.view
#      o.view = '+orig'

      # allow overwrite in AFNI commands
      if (ps.rewrite) :
         owrite = "-overwrite"
      else:
         owrite = ""
     
      if (not o.exist() or ps.rewrite or ps.dry_run()):
         o.delete(oexec=ps.oexec)
         o.view = eview
         # anat_mat = "%s%s_mat.aff12.1D" %  (ps.anat0.out_prefix(),suf)
         epi_mat = "%s%s%s_mat.aff12.1D" %  (o.p(), self.epi_afniformat.out_prefix(),suf)
         self.info_msg("Inverting %s to %s matrix" % \
                    (ps.dset1_generic_name, ps.dset2_generic_name ))

         if(ps.obl_a2e_mat!="") :
            oblique_mat = "%s" % ps.obl_a2e_mat
         else :
            oblique_mat = ""
         
         com = shell_com(  \
                  "cat_matvec -ONELINE %s %s -I > %s" % \
                  ( oblique_mat, ps.anat_mat,  epi_mat), eo=ps.oexec)
         com.run();
         
         # concatenate volume registration from epi data
         if(ps.volreg_flag):
            self.info_msg("Concatenating volreg and %s " \
                          "to %s transformations" % \
                         (ps.dset2_generic_name, ps.dset1_generic_name ))

            epi_mat = "%s%s%s_reg_mat.aff12.1D" % (o.p(), self.epi_afniformat.out_prefix(), suf)
            com = shell_com(  \
                     "cat_matvec -ONELINE %s %s -I %s > %s" % \
                     (oblique_mat, ps.anat_mat, self.reg_mat, epi_mat), eo=ps.oexec)
            com.run();

 
         self.info_msg( "Applying transformation of %s to %s" % \
                        (ps.dset2_generic_name, ps.dset1_generic_name ))
         com = shell_com(  \
               "3dAllineate -base %s -1Dmatrix_apply %s " \
               "-prefix %s%s -input %s  %s %s %s"   %  \
               ( a.input(), epi_mat, o.p(), o.out_prefix(), e.input(),\
                 self.master_epi_option, alopt, owrite), eo=ps.oexec)
         
         com.run()

         # mark as not oblique if deobliqued
         if(oblique_mat!="") :
            o.view = eview
            com = shell_com ("3drefit -deoblique %s" % (o.input()), eo=ps.oexec)
            com.run()


#         if (not o.exist() and not ps.dry_run()):
#            self.error_msg("Could not apply transformation to epi data")
#            return None

         # concatenate talairach or post transformation
         if((ps.tlrc_apar != "") or (ps.post_matrix !="")):

            self.info_msg( "Concatenating talairach/post, volume registration," \
                           " %s to %s transformations" % \
                           (ps.dset2_generic_name, ps.dset1_generic_name ))

            
            if(ps.post_matrix != ""):
                anat_tlrc_mat = ps.post_matrix
                epi_mat = "%s%s%s_post_mat.aff12.1D" % (o.p(),self.epi_afniformat.out_prefix(), suf)

            if(ps.tlrc_apar != ""): # tlrc parent trumps post_matrix
               com = shell_com(  \
                        "3dAttribute WARP_TYPE %s" % \
                         ps.tlrc_apar.input(), eo=ps.oexec, capture=1)
               com.run();
               if(com.status != 0) :
                  self.error_msg("Warp type not defined for this dataset: %s" % \
                            ps.tlrc_apar.input())
                  return o,t

               tlrc_type = int(com.val(0,0))
               if(tlrc_type != 0) :
                  self.error_msg("Can not compute transformations for manually"
                            " talairached data")
                  return o,t

               anat_tlrc_mat = "%s::WARP_DATA" % (ps.tlrc_apar.input())
               epi_mat = "%s%s%s_tlrc_mat.aff12.1D" % (o.p(),self.epi_afniformat.out_prefix(), suf)

            # note registration matrix, reg_mat, can be blank and ignored
            com = shell_com(  \
                   "cat_matvec -ONELINE %s -I %s %s -I %s  > %s" % \
                   (anat_tlrc_mat, oblique_mat, ps.anat_mat, self.reg_mat, \
                     epi_mat), eo=ps.oexec)
            com.run();

            if(ps.tlrc_apar!=""):
               tlrc_dset = afni_name("%s_tlrc%s+tlrc" % (self.epi_afniformat.prefix, suf))
               tlrc_dset.path = o.p()
               # tlrc_dset.view = ps.tlrc_apar.view  '+tlrc'
               if(self.master_tlrc_dset=='SOURCE'):
                   tlrc_dset.view = e.view
               else:
                   if(self.master_tlrc_dset=='BASE'):
                      tlrc_dset.view = ps.tlrc_apar.view
                   else:
                      mtlrc = afni_name(self.master_tlrc_dset)
                      tlrc_dset.view = mtlrc.view

               if tlrc_dset.exist():
                  tlrc_dset.delete(oexec=ps.oexec)
               atlrcpost = tlrc_dset
               self.info_msg(  \
                  "Applying transformation of %s to %s tlrc parent" % \
                   (ps.dset2_generic_name, ps.dset1_generic_name ))

               com = shell_com( \
                 "3dAllineate -base %s -1Dmatrix_apply %s " \
                 "-prefix %s%s -input %s -verb %s %s %s" % \
                 ( ps.tlrc_apar.input(), epi_mat, atlrcpost.p(), atlrcpost.prefix,e.input(),\
                   ps.master_tlrc_option, alopt, owrite), eo=ps.oexec)

            else:
               tlrc_orig_dset = afni_name("%s%s_post%s" % (self.epi_afniformat.out_prefix(), suf))
               tlrc_orig_dset = o.p()
               tlrc_orig_dset.view = '+orig'
               base_dset = a
               if(self.master_tlrc_dset=='SOURCE'):
                   tlrc_orig_dset.view = e.view
               else:
                   if(self.master_tlrc_dset=='BASE'):
                      tlrc_orig_dset.view = a.view
                   else:
                      mtlrc = afni_name(self.master_tlrc_dset)
                      tlrc_orig_dset.view = mtlrc.view
                      base_dset = mtlrc

               if tlrc_orig_dset.exist():
                  tlrc_orig_dset.delete(oexec=ps.oexec)
               atlrcpost = tlrc_orig_dset
               self.info_msg("Applying post transformation matrix to %s" % \
                              ps.dset2_generic_name)

               com = shell_com( \
                 "3dAllineate -base %s -1Dmatrix_apply %s " \
                 "-prefix %s%s -input %s -verb %s %s %s" % \
                 ( base_dset.input(), epi_mat, atlrcpost.p(), atlrcpost.input(), e.input(),\
                   ps.master_tlrc_option, alopt, owrite), eo=ps.oexec)

            com.run()

            # remove obliquity from output
            if(atlrcpost.type == 'BRIK'):
              # force +tlrc output for master SOURCE option - 3dAllineate saves this as +orig
              if((ps.master_tlrc_dset=="SOURCE") and (ps.tlrc_apar!="")):
                 com = shell_com ("3drefit -deoblique -view tlrc %s%s+orig" %     \
                        ( atlrcpost.p(), atlrcpost.prefix), eo=ps.oexec)
                 com.run()
              else:
                 if(oblique_mat!=""):
                    com = shell_com ("3drefit -deoblique %s%s+tlrc" %  \
                      (atlrcpost.p(), atlrcpost.prefix), eo=ps.oexec)
                    com.run()
            t = atlrcpost
      else:
         self.exists_msg(o.input())
            
#          if (not o.exist() and not ps.dry_run()):
#             self.error_msg("Could not apply tlrc transformation to epi data")
#             return None


      return o,t

   # reduce EPI dataset to a single representative sub-brick
   def tstat_epi(self, e=None, tstat_opt="", prefix = "temp_ts"  ):
      o = afni_name(prefix)
      o.to_afni(new_view=dset_view(o.ppve()))
      if (not o.exist() or ps.rewrite or ps.dry_run()):
         o.delete(oexec=ps.oexec)
         # if more than 1 sub-brick
         if (ps.dry_run() or \
            ((not ps.dry_run() and dset_dims(e.input())[3] > 1))):
         # could be: if number choose bucket else use that as stat
         # choose a statistic as representative
            self.info_msg("Creating representative %s sub-brick" % \
              ps.dset2_generic_name)

            # if an integer, choose a single sub-brick
            if(ps.epi_base.isdigit()): 
            # if an integer, choose a single sub-brick
               com = shell_com(  \
               "3dbucket -prefix %s %s'[%s]'" % \
               (o.pp(), e.input(), ps.epi_base) , eo=ps.oexec)
            else:          
               if((ps.epi_base=='median') or (ps.epi_base=='max') or \
               (ps.epi_base=='mean')):   
                  com = shell_com(  \
                  "3dTstat -%s -prefix %s %s" % \
                  (ps.epi_base, o.pp(), e.input()), eo=ps.oexec)
               else:
                  self.info_msg(
                    "using 0th sub-brick - assuming epi_base is dataset name" )
                  com = shell_com( "3dbucket -prefix %s %s'[0]'" %  \
                    (o.pp(), e.input()), eo=ps.oexec)
         else:   # choose a single sub-brick (sub-brick 0)
            self.info_msg("using 0th sub-brick because only one found")
            com = shell_com( "3dbucket -prefix %s %s'[0]'" %  \
              (o.pp(), e.input()), eo=ps.oexec)
         com.run();
         if (not o.exist() and not ps.dry_run()):
            o.show
            print("** ERROR: Could not 3dTstat epi")
            return None
      else:
         self.exists_msg(o.input())

      return o

   # create edge dataset of internal brain structure edges
   def edge_dset(self, e=None, prefix = "temp_edge", binarize=0,erodelevel=2):
      o = afni_name(prefix)
      o.view = e.view
      m = afni_name("%s_edge_mask" % o.prefix)
      m.view = e.view
      
      if (not m.exist() or ps.rewrite or ps.dry_run()):
         m.delete(oexec=ps.oexec)
         self.info_msg("Creating edge dataset")
         com = shell_com("3dAutomask -overwrite -erode %s -prefix %s %s" \
                         % (erodelevel, m.prefix, e.input()), eo=ps.oexec)
         com.run()

         if(binarize):
            lprefix = "%s_edge_cvar" % o.prefix
         else:
            lprefix = o.prefix

         com = shell_com(                                                  \
                         "3dLocalstat -overwrite -mask %s"                 \
                         " -nbhd 'RECT(-2,-2,-1)'"                          \
                         " -stat cvar -prefix %s %s" %                     \
                         (m.ppv(), lprefix, e.input()), eo=ps.oexec)
         com.run();

         if(binarize):
            com = shell_com( \
              "3dhistog -omit 0 -max 1 -nbins 1000 %s_edge_cvar%s "     \
                            " > %s_edge_histo.1D" % (prefix, o.view, prefix), eo=ps.oexec)
            com.run()

            com = shell_com("3dTstat -argmax -prefix %s_edge_histomax "    \
                            "%s_edge_histo.1D'[1]'\\' " %                  \
                            (prefix, prefix), eo=ps.oexec)
            com.run()

            com = shell_com("1dcat %s_edge_histomax.1D" %                  \
                             prefix, eo=ps.oexec, capture=1)
            com.run()

            if(ps.dry_run()): edgeindex = 123
            else:  edgeindex = int(com.val(0,0))

            com = shell_com("1dcat %s_edge_histo.1D'[0]{%d}'" %            \
                  (prefix, edgeindex), eo=ps.oexec, capture=1)
            com.run()

            if(ps.dry_run()): edgevalue = 0.0123
            else:  edgevalue = float(com.val(0,0))

            # threshold anatomical and EPI edge data
            self.info_msg("Thresholding edges at %f" % edgevalue)
            com = shell_com( \
                '3dcalc -a %s_edge_cvar%s -overwrite -expr "step(a-%f)"' \
                ' -prefix %s' % (prefix, o.view, edgevalue, o.out_prefix()), eo=ps.oexec)
            com.run()

         if (not o.exist() and not ps.dry_run()):
            print("** ERROR: Could not create edge dataset %s" % o.ppv())
            return None
      else:
         self.exists_msg(o.input())

            
      return o

   
   # deoblique epi dataset
   def deoblique_epi(self, e=None, deoblique_opt="", prefix="temp_deob"):
      o = e.new(prefix)  
      if (not o.exist() or ps.rewrite or ps.dry_run()):
         o.delete(oexec=ps.oexec)
         self.info_msg( "Deobliquing")
         com = shell_com(  \
               "3dWarp -deoblique -prefix %s%s %s %s "   %  \
               ( o.p(),o.out_prefix(), deoblique_opt, e.input()), eo=ps.oexec)
         com.run();
         if (not o.exist() and not ps.dry_run()):
            print("** ERROR: Could not deoblique epi data\n")
            return None
      else:
         self.exists_msg(o.input())

      return o

   # oblique anat to epi dataset
   # even if neither is really oblique, this shouldn't hurt.
   # if a pre-transformation matrix is defined, apply it here
   def oblique_anat2epi(self,a=None,e=None,oblique_opt="",suffix="_ob"):
      o = a.new("%s%s" % (a.out_prefix(), suffix))

      if(self.pre_matrix!=""):
         # use pre-shift transformation matrix instead
         self.obl_a2e_mat = self.pre_matrix   
         warp_str = "3dWarp -verb -matvec_in2out %s -prefix %s%s %s %s %s " \
                  % (self.pre_matrix, o.p(), o.out_prefix(),                \
                  self.master_anat_option, oblique_opt,                     \
                  a.input())
      else:
         if(ps.align_centers):
            # use shift transformation of centers between grids as initial
            # transformation. @Align_Centers (3drefit) instead of 3dWarp
            copy_cmd = "3dcopy %s %s%s" % (a.input(), o.p(), o.out_prefix())
            warp_str = "%s; @Align_Centers -base %s -dset %s -no_cp" %     \
              (copy_cmd, e.input(), o.input())         
         else:
            # get obliquity matrix from 3dWarp output and oblique anat to epi
            # tempmat = "__tt_%s_obla2e_mat.1D" % a.out_prefix()
            self.obl_a2e_mat = "%s%s_obla2e_mat.1D" % (a.p(), a.out_prefix())
            self.info_msg( "Matching obliquity of %s to %s" %                 \
                       (ps.dset1_generic_name, ps.dset2_generic_name ))

            warp_str = "3dWarp -verb -card2oblique %s -prefix %s%s %s %s %s " \
                     "  | \grep  -A 4 '# mat44 Obliquity Transformation ::'"  \
                     "  > %s"                                                 \
                    % (e.input(), o.p(), o.out_prefix(),                      \
                     self.master_anat_option, oblique_opt,                    \
                     a.input(), self.obl_a2e_mat)
 
      if (not o.exist() or ps.rewrite or ps.dry_run()):
         o.delete(oexec=ps.oexec)
         com = shell_com( warp_str, eo=ps.oexec)
         com.run();
         if (not o.exist() and not ps.dry_run()):
            print("** ERROR: Could not oblique/shift anat to epi data\n")
            return None
      else:
         self.exists_msg(o.input())
       
      if(ps.align_centers):
         # align_centers saves the shift transformation, but
         # we use the opposite direction for obliquity, so invert
         # the shift transformation too and use that like the
         # obliquity matrix in later concatenations
         self.obl_a2e_mat = "%s%s_shft_I.1D" % (a.p(), a.out_prefix())
         catcom = "cat_matvec %s%s_shft.1D -I > %s" %  \
             (o.p(),o.out_prefix(), self.obl_a2e_mat)
         com = shell_com( catcom, eo=ps.oexec)
         com.run();
         if(self.master_anat_3dAl_center == 1):
             self.info_msg("Using align_centered %s as anatomical master" % \
                 o.prefix)
             self.master_anat_3dAl_option = "-master %s %s" %    \
                 (o.input(), self.master_anat_3dAl_option)
         if(self.master_epi_3dAl_center == 1):
            tempshft = e.new("%s_shft%s" % (e.out_prefix(), suffix))
           
            copy_cmd = "3dcopy %s %s%s" % \
                 (e.input(), tempshft.p(), tempshft.out_prefix())
            warp_str = "%s; @Align_Centers -base %s -dset %s -no_cp" %     \
                 (copy_cmd, e.input(), tempshft.input())         

            if (not tempshft.exist() or ps.rewrite or ps.dry_run()):
               tempshft.delete(oexec=ps.oexec)
               com = shell_com( warp_str, eo=ps.oexec)
               com.run();
               if (not tempshft.exist() and not ps.dry_run()):
                  print("** ERROR: Could not oblique/shift anat to epi data\n")
                  return None
            else:
               self.exists_msg(tempshft.input())
            self.info_msg("Using align_centered %s as epi master" % \
                 tempshft.prefix)
            self.master_epi_option = "-master %s %s" %    \
                 (tempshft.input(), self.master_epi_option)
         
      return o


   # do time shifting of EPI dataset
   def tshift_epi(  self, e=None, tshift_opt="-cubic", prefix="temp_tsh"):
      o = afni_name(prefix)  

      if (not o.exist() or ps.rewrite or ps.dry_run()):
         o.delete(oexec=ps.oexec)
         self.info_msg( "Correcting for slice timing")
         com = shell_com(  \
               "3dTshift -prefix %s %s %s "   %  \
               ( o.pp(), tshift_opt, e.input()), eo=ps.oexec)
         com.run();
         if (not o.exist() and not ps.dry_run()):
            print("** ERROR: Could not do time shifting of epi data\n")
            return e
      else:
         self.exists_msg(o.input())

      return o

 
   # do volume registration of EPI dataset
   def register_epi(self, e=None, reg_opt="-quintic", prefix="temp_vr", \
                      motion_prefix = "temp_vr", childflag=0):
      o = afni_name(prefix)

      if (not o.exist() or ps.rewrite or ps.dry_run()):
         o.delete(oexec=ps.oexec)
         # save the volreg output to file names based on original epi name
         #  (not temporary __tt_ names)
         self.mot_1D = "%s_motion.1D" % (motion_prefix)    # motion parameters output
         self.reg_mat = "%s%s_mat.aff12.1D" % (o.p(), o.out_prefix())  # volreg transformation matrix
         self.info_msg( "Volume registration for %s data" % \
                    ps.dset2_generic_name)

         # user option for which registration program (3dvolreg,3dWarpDrive,...)
         opt = self.user_opts.find_opt('-volreg_method')
         if opt != None: 
            vrcom = opt.parlist[0]
         else:
            vrcom = '3dvolreg'

         if (vrcom == '3dWarpDrive'):
            vrcom = '3dWarpDrive -affine_general'
         if (vrcom == '3dAllineate'):
            vrcom = '3dAllineate -cost lpa -automask -source_automask'
         # find base for registration
         # could be: if number just use that as base
         # if((ps.volreg_base=='median') or (ps.volreg_base=='max') 
         #   or (ps.volreg_base=='mean')):   
         # choose a statistic as representative
         # if an integer, choose a single sub-brick
         if(childflag) :   # or (vrcom != "3dvolreg") :
            base = "%s'[%s]'"  %  (ps.epi.input(), ps.volreg_base)
         elif(ps.volreg_base.isdigit()):
            base = "%s" % ps.volreg_base
            if(vrcom != '3dvolreg'):
                 base = "%s'[%s]'"  %  (ps.epi.input(), ps.volreg_base)

         # otherwise median, mean or max
         else:          
           # if more than 1 sub-brick, compute stat, otherwise use 0th
           if 0:  # (dset_dims(e.ppve())[3] < 2 ):
              self.info_msg("Not enough sub-bricks to compute %s" % \
                             ps.volreg_base)
              base = "0"
           else:
              ots = e.new("%s_ts_tempalpha" % o.out_prefix())
              base = "%s'[0]'" % ots.input()
              if (not ots.exist() or ps.rewrite):
                 ots.delete(oexec=ps.oexec)

              # compute stats, volreg, then recompute stats
              com = shell_com(  \
                "3dTstat -%s -prefix %s%s %s" % \
                (ps.volreg_base, ots.p(), ots.out_prefix(), e.input()), eo=ps.oexec)
              com.run()

              if(not ots.exist() and not ps.dry_run()):
                 self.error_msg("Could not create intermediate data" \
                                "for time series registration")
                 ps.ciao(1)

              ovr_alpha = e.new("%s_vr_tempalpha" % o.out_prefix())

              if((vrcom != '3dvolreg') and (ps.volreg_base.isdigit())):
                 base = "%s'[%s]'"  %  (ps.epi.input(), ps.volreg_base)
              com = shell_com(                                       \
                    "%s -prefix %s%s -base %s %s %s "  %               \
                ( vrcom, ovr_alpha.p(), ovr_alpha.out_prefix(), base,               \
                  reg_opt, e.input()), eo=ps.oexec)
              com.run()

              ots = e.new("%s_vrt" % o.out_prefix())
              base = "%s'[0]'" % ots.input()
              if (not ots.exist() or ps.rewrite):
                 ots.delete(oexec=ps.oexec)

                 com = shell_com(  \
                   "3dTstat -%s -prefix %s%s %s" % \
                   (ps.volreg_base, ots.p(), ots.out_prefix(), ovr_alpha.input()), \
                   eo=ps.oexec)
                 com.run()

              if(not ots.exist() and not ps.dry_run()):
                 self.error_msg("Could not create intermediate data" \
                                "for time series registration")
                 ps.ciao(1)

         com = shell_com(                                      \
               "%s -1Dfile %s -1Dmatrix_save %s "              \
               "-prefix %s%s -base %s %s %s "  %                 \
           ( vrcom, self.mot_1D, self.reg_mat, o.p(), o.out_prefix(), base, \
             reg_opt, e.input()), eo=ps.oexec)
         com.run()

         if (not o.exist() and not ps.dry_run()):
            self.error_msg( "Could not do volume registration")
            return None
      else:
         self.exists_msg(o.input())

      return o

   # resample EPI data to match higher resolution anatomical data
   def resample_epi(  self, e=None, resample_opt="", prefix="temp_rs", \
        subbrick=""):
      o = afni_name(prefix)
      if (not o.exist() or ps.rewrite or ps.dry_run()):
         o.delete(oexec=ps.oexec)
         self.info_msg( "resampling %s to match %s data" % \
           (ps.dset2_generic_name, ps.dset1_generic_name ))

         if (subbrick == ""):
             sb = ""
         else:
             if(subbrick.isdigit()): 
                sb = "[%s]" % subbrick
             else:
                sb = "[0]"
                             
         com = shell_com(  \
               "3dresample -master %s -prefix %s -inset %s'%s' -rmode Cu" \
                % (ps.anat_ns.ppv(), o.pp(), e.input(),sb), eo=ps.oexec)
         com.run()
         if (not o.exist() and not ps.dry_run()):
            print("** ERROR: Could not resample\n")
            return None          
      else:
         self.exists_msg(o.input())

      return o
      
   # remove skull or outside brain area
   def skullstrip_data(self, e=None, use_ss='3dSkullStrip', \
       skullstrip_opt="", prefix = "temp_ns"):
      self.info_msg( "removing skull or area outside brain")
      if (use_ss == '3dSkullStrip'):     #skullstrip epi
         n = afni_name(prefix)
         if (not n.exist() or ps.rewrite or ps.dry_run()):
            n.delete(oexec=ps.oexec)
            com = shell_com(  \
                  "3dSkullStrip -orig_vol %s -input %s -prefix %s" \
                  % (skullstrip_opt, e.input(), n.pp()) , eo=ps.oexec)
            com.run()
            if (not n.exist() and not ps.dry_run()):
               print("** ERROR: Could not strip skull\n")
               return None
         else:
            self.exists_msg(n.input())
      elif use_ss == '3dAutomask': #Automask epi
         n = afni_name(prefix)
         j = afni_name("%s__tt_am_%s" % (n.p(),n.pve()))
         if (not n.exist() or ps.rewrite or ps.dry_run()):
            n.delete(oexec=ps.oexec)
            com = shell_com(  \
                  "3dAutomask %s -apply_prefix %s %s" \
                  % ( skullstrip_opt,  n.pp(), e.input()), eo=ps.oexec)
            com.run()
            if (not n.exist() and not ps.dry_run()):
               print("** ERROR: Could not strip skull with automask\n")
               return None
            j.delete(oexec=ps.oexec)
         else:
            self.exists_msg(n.input())
      else:
         n = e;
      return n

   # create weighting volume for matching (upper 10% by default)
   def create_weight(self, e, sq=1.0, box='no', \
                     binit = 'no', perci = -1.0, \
                     fati = -1, suf="_wt"):
   #e is a preprocessed epi, no skull
   # box, bin and fat mask are not used
      a = ps.anat_ns
      
      o = afni_name("%s%s%s%s" % (ps.output_dir,e.out_prefix(), suf,e.view))            
      if perci < 0:
         perci = 90.0;
      self.info_msg( "Computing weight mask")
      com_str = "3dBrickStat -automask -percentile %f 1 %f %s" \
                        % (perci, perci, e.input())
      if(not ps.dry_run()):
         com = shell_com( com_str, eo=ps.oexec, capture=1)
         com.run()
         th = float(com.val(0,1))
         self.info_msg( "Applying threshold of %f on %s" % (th, e.input()))
      else:
         com = shell_com( com_str, "dry_run")
         com.run()
         th = -999
         self.info_msg( "Would be applying threshold for real run here")
            
      if sq == 1.0:
         com = shell_com( 
               "3dcalc -datum float -prefix %s%s "\
               "-a %s -expr 'min(1,(a/%f))'" \
               % (o.p(), o.out_prefix(), e.input(), th), eo=ps.oexec)
      else:
         com = shell_com( 
               "3dcalc -datum float -prefix %s%s -a "\
               "%s -expr 'min(1,(a/%f))**%f'" \
               % (o.p(), o.out_prefix(), e.input(), th, sq), eo=ps.oexec)

      if (not o.exist() or ps.rewrite or ps.dry_run()):
         o.delete(oexec=ps.oexec)
         com.run()
      else:
         self.exists_msg(o.input())
         
      if (not o.exist() and not ps.dry_run()):
         print("** ERROR: Could not create weight dset\n")
         return None
      
      return o


   # Create double edge images for later viewing
   def add_edges(self, d1, d2, d3, d1name, d2name, d3name, listlog = ""):
      # specified examinelist for output name or use default in @AddEdge
      if (listlog == ""):
         aelistlog = ""
      else :
         aelistlog = "-examinelist %s" % listlog
         
      if (d1name==""):
         d1AE = afni_name("%s%s" % (d1.prefix,d1.view))
      else:
         d1AE = afni_name("%s%s" % (d1name,d1.view))
         
      if (d2name==""):
         d2AE = afni_name("%s%s" % (d2.prefix,d2.view))
      else:
         d2AE = afni_name("%s%s" % (d2name,d2.view))

      if (d3name==""):
         d3AE = afni_name("%s%s" % (d3.prefix,d3.view))
      else:
         d3AE = afni_name("%s%s" % (d3name,d3.view))
      
      com = shell_com(("mkdir %sAddEdge"% ps.output_dir), eo=ps.oexec)
      com.run()

      com = shell_com( \
        "3dcopy -overwrite %s %sAddEdge/%s" % \
        (d1.input(), ps.output_dir, d1AE.prefix), eo=ps.oexec)
      com.run()
      com = shell_com( \
        "3dcopy -overwrite %s %sAddEdge/%s" % \
        (d2.input(), ps.output_dir, d2AE.prefix), eo=ps.oexec)
      com.run()
      com = shell_com( \
        "3dcopy -overwrite %s %sAddEdge/%s" % \
        (d3.input(), ps.output_dir, d3AE.prefix), eo=ps.oexec)
      com.run()

      # changed .input to .shortinput           3 Feb 2012 [rickr,dglen]
      com = shell_com( 
            "cd %sAddEdge; @AddEdge -no_deoblique %s %s %s %s; cd - " \
            % (ps.output_dir,aelistlog, d1AE.shortinput(), d2AE.shortinput(),\
            d3AE.shortinput()), eo=ps.oexec)
      com.run()

   # do the preprocessing of the EPI data    
   def process_epi(self, use_ss='3dSkullStrip', childflag=0):
      basesuff = ""
      if self.epi_dir == '':
         basepath = self.epi.p()
      else:
         basepath = self.epi_dir

      if(self.epi.type == 'NIFTI'):
         #copy original epi to a temporary file
         o = afni_name("%s__tt_%s%s" % \
                     (basepath, self.epi.out_prefix(),self.epi.view))
         o.to_afni(new_view=dset_view(self.epi.ppve()))

         self.info_msg("Copying NIFTI EPI input to AFNI format")
         if (not o.exist() or ps.rewrite or ps.dry_run()):
            com = shell_com( "3dcopy %s %s%s" % \
              (self.epi.input(), o.p(), o.pv()), eo=ps.oexec)
            com.run();
         basename = o.out_prefix()
         baseviewext = "%s%s" % (o.view, o.extension)
         basepathname = "%s%s" % (basepath, basename) 
         tempbasepathname = basepathname
         #make filename output without .nii.gz
         self.epi_afniformat = afni_name("%s%s%s" % \
                     (basepath, self.epi.out_prefix(),self.epi.view))
         self.epi_afniformat.to_afni(new_view=dset_view(self.epi.ppve()))
      else:
         o = self.epi;
         basename = o.out_prefix()
         baseviewext = "%s%s" % (o.view, o.extension)
         basepathname = "%s%s" % (basepath, basename) 
         tempbasepathname = "%s__tt_%s" % (basepath, basename)
         self.epi_afniformat = self.epi

#      if Childflag:    # no temporary files for children with exceptions below
#         prepre = ""
#      else:
#         prepre = "__tt_"
#      suff = ps.suffix
 
      # time shift epi data, prepend a prefix
      if(self.tshift_flag):
         if(self.tshiftable_dset(o)) :
            basesuff = "%s_tsh" % basesuff
            if(ps.save_tsh):
                prefix = "%s%s%s" % (basepathname,basesuff,baseviewext)
            else:
                prefix = "%s%s%s" \
                % (tempbasepathname,basesuff,baseviewext)
            o = self.tshift_epi( o, ps.tshift_opt, prefix=prefix)
         else:
            self.info_msg("Can not do time shifting of slices. "
                          "Data is already time shifted")
      # if timeshifting was done, this will be the final epi dataset
      # and then the concatenated volreg, 3dAllineate transformations will be
      # applied
      tshift_o = o         
      # do volume registration
      if(self.volreg_flag):
         if(ps.dry_run() or \
           (not ps.dry_run() and (dset_dims(o.input())[3] > 1))) :
             basesuff = "%s_vr" % basesuff
             if(ps.save_vr):
                prefix = "%s%s%s%s" % (basepath,self.epi_afniformat.out_prefix(),basesuff,baseviewext)
#                prefix = "%s%s%s" % (basepathname,basesuff,baseviewext)
             else:
                prefix = "%s%s%s" \
                % (tempbasepathname,basesuff,baseviewext)
             # if aligning epi to anat or saving volreg output, save motion parameters
             # if(ps.epi2anat):
             motion_prefix = "%s%s%s" % (basepath,self.epi_afniformat.out_prefix(),basesuff)
#             motion_prefix = "%s%s%s" % (basepath,basename,basesuff)
             # else:
             #   motion_prefix = prefix
             o = self.register_epi( o, ps.reg_opt, prefix, motion_prefix,
                                    childflag=childflag)
         else:
            self.info_msg("Skipping time series volume registration. "
                          "Must have more than a single sub-brick.")
            self.reg_mat = "" # children can be skipped too

      volreg_o = o

      # if just processing child epi datasets, just go home
      #   and skip reduction, resampling, skullstripping
      if(childflag):
         return tshift_o, volreg_o, volreg_o
 
      # reduce epi to a single representative sub-brick
      basesuff = "%s_ts" % basesuff
      if(ps.save_rep):
          prefix = "%s%s%s%s" % (basepath,basename,basesuff,baseviewext)
      else:
          prefix = "%s%s%s" \
          % (tempbasepathname,basesuff,baseviewext)

      o = self.tstat_epi(o, ps.tstat_opt, prefix)

      # resample epi to match anat
      if(self.resample_flag) :
         basesuff = "%s_rs" % basesuff
         if(ps.save_resample):
            prefix = "%s%s%s%s" % (basepath,basename,basesuff,baseviewext)
         else:
            prefix = "%s%s%s" \
            % (tempbasepathname,basesuff,baseviewext)
         
         e = self.resample_epi( o,"", prefix)
      else:
         e = o  # no need to resample

      # remove outside brain or skull
      basesuff = "%s_ns" % basesuff
      if(self.save_epi_ns) :
         prefix = "%s%s%s" % (basepathname,basesuff,baseviewext)
      else:
         prefix = "%s%s%s" \
         % (tempbasepathname,basesuff,baseviewext)
       
      skullstrip_o = self.skullstrip_data( e, use_ss, ps.epistrip_opt, prefix)

      # use edges to align optionally
      if(ps.edge) :
         basesuff = "%s_edge" % (basesuff)
         if(ps.save_rep):
            prefix = "%s%s%s" % (basepathname,basesuff,baseviewext)
         else:
            prefix = "%s%s%s" % (tempbasepathname,basesuff,baseviewext)

         skullstrip_o = self.edge_dset(skullstrip_o, prefix, binarize=0,
            erodelevel=ps.erodelevel)

      return  tshift_o, volreg_o, skullstrip_o

   # do the preprocessing of the anatomical data
   def process_anat(self):
      if self.anat_dir == '':
         basepath = self.anat0.p()
      else:
         basepath = self.anat_dir
         
      self.anat0_master = self.anat0

      #copy original anat to a temporary file
      if(not ps.flip):
         self.anat = afni_name("%s__tt_%s%s" % \
                  (basepath, self.anat0.out_prefix(),self.anat0.view))
      else:
         self.anat = afni_name("%s%s_unflipped%s" % \
                  (basepath, self.anat0.out_prefix(),self.anat0.view))
		  
      self.anat.to_afni(new_view=dset_view(self.anat.ppve()))

      if (not self.anat.exist() or ps.rewrite or ps.dry_run()):
         com = shell_com( "3dcopy %s %s%s" % \
           (self.anat0.input(), self.anat.p(), self.anat.pv()), eo=ps.oexec)
         com.run();
         if (not self.anat.exist() and not ps.dry_run()):
            print("** ERROR: Could not copy anat (%d)" % self.anat.exist())
            ps.ciao(1)
      else:
         self.exists_msg(self.anat.input())

      # now that we have the data in AFNI format, use the AFNI format copy names
      self.anat0 = afni_name("%s%s%s" % \
                  (basepath, self.anat0.out_prefix(),self.anat0.view))
		  
      self.anat0.to_afni(new_view=dset_view(self.anat.ppve()))

      a = self.anat;

      #do we need to strip ?
      if(ps.skullstrip):
         #don't use same type of input.
         n = afni_name("%s%s_ns%s" % (a.p(), a.out_prefix(),self.anat.view))
         if (not n.exist() or ps.rewrite or ps.dry_run()):
            n.delete(oexec=ps.oexec)
            self.info_msg( "Removing skull from %s data" % \
                       ps.dset1_generic_name)

            if(ps.skullstrip_method=="3dSkullStrip"):
               com = shell_com(  \
                  "%s -orig_vol %s -input %s -prefix %s%s" \
                  % (ps.skullstrip_method, ps.skullstrip_opt, a.input(), \
                  n.p(),n.out_prefix()), eo=ps.oexec)
            else:
               com = shell_com(  \
                  "%s %s -apply_prefix %s%s %s" \
                  % (ps.skullstrip_method, ps.skullstrip_opt, n.p(), \
                  n.out_prefix(), a.input()),eo=ps.oexec)

            com.run()
            if (not n.exist() and not ps.dry_run()):
               print("** ERROR: Could not strip skull\n")
               return None
         else:
            self.exists_msg(o.input())
      else:
         n = a
         
      ps.anat_ns0 = n    # pre-obliquing or edging skullstripped anat

      # match obliquity of anat to epi data
      if(self.deoblique_flag or ps.align_centers):
         # if either anat or epi is oblique or 
         #   there is a pre-transformation matrix,
         #   move anat to match epi
         if((self.oblique_dset(n)) or (self.oblique_dset(ps.epi)) \
             or (self.pre_matrix!="") or ps.align_centers) :
            # set default output spacing if not already set with user options
            opt = self.user_opts.find_opt('-master_anat') 
            if opt == None: 
                min_d =  self.min_dim_dset(ps.anat_ns0)
                self.master_3dAl_option = "-mast_dxyz %f" % min_d
                self.info_msg(
                  "Spacing for %s to oblique %s alignment is %f" %
                   (self.dset1_generic_name, self.dset2_generic_name, min_d))
               
            n = self.oblique_anat2epi(n, ps.epi, ps.deoblique_opt)

      # use edges to match optionally
      if(self.edge):
         prefix = "%s%s_edge%s" % (a.p(),a.out_prefix(), a.view)
         n = self.edge_dset(n, prefix,binarize=0, erodelevel=ps.erodelevel)

      #do we need to shift?
#      optc = self.user_opts.find_opt('-align_centers')
#      if optc != None and optc.parlist[0] == 'yes': 

#         com = shell_com(  \
#               "@Align_Centers -cm -base %s -dset %s" \
#               % (ps.epi.ppve(), n.ppve() ) , eo=ps.oexec)
#         com.run() 
#         a_shft = n.new("%s_shft" % n.out_prefix(),"+orig")
#         if (not a_shft.exist() and not ps.dry_run()):
#            print "** ERROR: Could not shift anat (%d)" % a_shft.exist()
#            return None
#         ps.anat_ns = a_shft
#      else:
      ps.anat_ns = n; 
      return 1

   # create final output files (non-temporary names)
   def create_output(self, aae, w, eaa, eaat, suf, epi_in=None, anat_in=None):

      #Create a properly named version of anatomy aligned to  EPI
      opt = ps.user_opts.find_opt('-anat')
      if opt == None :
         opt = self.user_opts.find_opt('-dset1')
      ain = afni_name(opt.parlist[0])
      opt = ps.user_opts.find_opt('-epi')
      if opt == None:
         opt = ps.user_opts.find_opt('-dset2')

      ein = afni_name(opt.parlist[0])

      # save skull stripped anat before alignment
      # Yikes! Rick noticed the saved skullstrip was the obliqued one
      # and not the original. Should be original ps.anat_ns0, not ps.anat_ns
      if(ps.skullstrip and ps.save_skullstrip):
         ao_ns = afni_name("%s%s_ns%s" % (ps.anat_ns0.p(),ain.prefix,ain.view))
         self.copy_dset( ps.anat_ns0, ao_ns, 
          "Creating final output: skullstripped %s data" % \
                          self.dset1_generic_name, ps.oexec)

      # maybe save pre-obliqued skull stripped anat before alignment
      # 3 Aug 2011 [rickr]
      if(ps.skullstrip and ps.save_origstrip):
         ao_ons = afni_name("%s%s%s%s" % (ps.anat_ns0.p(), ain.prefix, ps.save_origstrip,ain.view))
         self.copy_dset( ps.anat_ns0, ao_ons, 
          "Creating final output: skullstripped original %s data" % \
                          self.dset1_generic_name, ps.oexec)

      # save anatomy aligned to epi 
      if (aae):
         # save aligned anatomy
         o = afni_name("%s%s%s%s" % (aae.p(),ain.prefix, suf,aae.view))
         o.view = aae.view
         self.copy_dset( aae, o, 
          "Creating final output: %s data aligned to %s" % \
               (self.dset1_generic_name, self.dset2_generic_name) , ps.oexec)
         self.save_history(o,ps.oexec)

         if (ps.save_tsh):
            # save the timeshifted EPI data
            if (self.epi_ts and self.tshift_flag) :
               eo = afni_name("%s%s_tshft%s" % \
                  (self.epi_ts.p(),ein.prefix,self.epi_ts.view))
               self.copy_dset( self.epi_ts, eo,
                "Creating final output: time-shifted %s" % \
                          self.dset2_generic_name, ps.oexec)

         # save the volume registered EPI data
         if (ps.save_vr):
            if (self.epi_vr and self.volreg_flag):
               eo = afni_name("%s%s_vr%s" %
                    (self.epi_vr.p(),ein.prefix,self.epi_vr.view))
               self.copy_dset( self.epi_vr, eo, 
                 "Creating final output: time series volume-registered %s" % \
                          self.dset2_generic_name, ps.oexec)

      # save Allineate input datasets
      if(ps.save_Al_in):
         # save weight used in 3dAllineate
         if w:
            ow = afni_name("%s%s_wt_in_3dAl%s%s" % \
            (w.p(),ein.prefix,suf,w.view))
            self.copy_dset( w, ow, 
             "Creating final output: weighting data", ps.oexec)

         #save a version of the epi as it went into 3dAllineate         
         if epi_in:
            eo = afni_name("%s%s_epi_in_3dAl%s%s" % \
            (epi_in.p(),ein.prefix, suf,epi_in.view))
            self.copy_dset( epi_in, eo,     \
             "Creating final output: "
             "%s representative data as used by 3dAllineate" % \
                          self.dset2_generic_name, ps.oexec)

         #save a version of the anat as it went into 3dAllineate
         if anat_in:  
            ao = afni_name("%s%s_anat_in_3dAl%s%s" % \
            (ps.anat_ns0.p(), ain.prefix, suf, anat_in.view))
            self.copy_dset( anat_in, ao, 
            "Creating final output: %s data as used by 3dAllineate" % \
                          self.dset1_generic_name, ps.oexec)

      #Now create a version of the epi that is aligned to the anatomy
      if (eaa):
         #save the epi aligned to anat
         o = afni_name("%s%s%s%s" % \
         (eaa.p(), ein.prefix, suf,eaa.view))
         self.copy_dset( eaa, o,
          "Creating final output: %s data aligned to %s" % \
                   (self.dset2_generic_name, self.dset1_generic_name), ps.oexec)
         self.save_history(o,ps.oexec)
      #And a version of the epi that is aligned to the anatomy in standard space
      if (eaat):
         #save the epi aligned to anat in standard space
         #put note in header with command line
         self.save_history(eaat,ps.oexec)
       
      return

   # remove all the temporary files from a previous run
   # remove old results too optionally
   def fresh_start(self, epref="", apref="", rmold = 0, epipath="", anatpath="" ):
      self.info_msg("Removing all the temporary files")
      epref = epref.strip()
      apref = apref.strip()
      if epref == "" and apref == "":
         com = shell_com(  "\\rm -f %s__tt_*" % ps.output_dir, eo=ps.oexec)
         com.run()  
      else:
         if epref != "":
            com = shell_com(  "\\rm -f %s__tt_%s*" % (epipath, epref), eo=ps.oexec)
            com.run() 
            if(rmold):
               com = shell_com( "\\rm -f %s%s*%s*" % (epipath, epref, ps.suffix), \
                eo=ps.oexec)
               com.run() 

         if apref != "":
            com = shell_com(  "\\rm -f %s__tt_%s*" % (anatpath, apref), eo=ps.oexec)
            com.run()  
            if(rmold):
               com = shell_com( "\\rm -f %s%s*%s*" % (anatpath, apref, ps.suffix), \
                  eo=ps.oexec)
               com.run() 

      return


   # process children EPI as for parent but 
   #   no alignment of anat to EPI and no representative EPI
   # Do time shifting, time series volume registration
   def process_child_epi(self, childepi) :
      # do preprocessing of epi
      # do time shifting, volume registration of child epi data
      #   if requested; otherwise, just keep the input epi
      if(childepi.input()==ps.epi.input()) : # skip the parent if it's included
         return                              #   in child list
      child = copy.copy(ps)      
      
      child.epi = childepi
      
      self.info_msg("Parent %s:  Child: %s" % 
          (ps.epi.input(), child.epi.input()))
            
      child.epi_ts, child.epi_vr, child.epi_ns = \
          child.process_epi(childflag=1)
      if (not child.epi_ns):
         child.ciao(1)

      e = child.epi_ns
      a = ps.anat0_master       # use parent anat

      if(ps.prep_only):  # if preprocessing only, exit now
         return

      if (ps.epi2anat) :   # does the user want the epi aligned to the anat
         # compute transformation just from applying inverse
         child.epi_alnd = \
            child.align_epi2anat(child.epi_ts, a, ps.AlOpt, suf=ps.suffix)
         if (not child.epi_alnd):
            ps.ciao(1)
      else:
         child.epi_alnd = ''

      
# Main:
if __name__ == '__main__':


   ps = RegWrap('align_epi_anat.py')
   ps.init_opts()
   ps.version()
   rv = ps.get_user_opts()
   if (rv != None): ps.ciao(1) 
   
   #process and check input params
   if(not (ps.process_input())):
      ps.ciao(1)

   # get rid of any previous temporary data
   ps.cleanup()

   
   #Now process anatomy and epi
   if (not ps.process_anat()):
      ps.ciao(1)
   # do preprocessing of epi
   # final epi2anat option may use timeshifted data as basis
   ps.epi_ts, ps.epi_vr, ps.epi_ns = \
      ps.process_epi(use_ss=ps.epi_strip_method)
      # (ps.user_opts.find_opt('-epi_strip').parlist[0]))
   if (not ps.epi_ns):
      ps.ciao(1)
   
   e = ps.epi_ns
   ps.epi_alnd_tlrc = None
      
   #Create a weight for final pass
   if(not(ps.edge)):
      ps.epi_wt = \
         ps.create_weight( e, float(ps.sqmask), ps.boxmask, \
                        ps.binmask, ps.perc, -1, suf = "_wt")
   else: ps.epi_wt = e  # use binary edge image for weight too
   
   if(ps.prep_only):  # if preprocessing only, exit now
      ps.ciao(0)
      
   #Do alignment to that pesky little epi
   for mcost in ps.multicost:
      if (mcost==ps.cost):
         suff = ps.suffix
      else:
         suff = "%s_%s" % (ps.suffix, mcost)

      a = ps.anat_ns

      ps.anat_alnd, ps.anat_alndwt = \
         ps.align_anat2epi(e, a, ps.epi_wt, ps.AlOpt, suff, mcost)
      if (not ps.anat_alnd):
         ps.ciao(1)
      if (ps.epi2anat) :   # does the user want the epi aligned to the anat
         # compute transformation just from applying inverse
         a = ps.anat0_master      # especially important for oblique datasets
         ps.epi_alnd, ps.epi_alnd_tlrc = \
            ps.align_epi2anat(ps.epi_ts, a, ps.AlOpt, suf=suff)
         if (not ps.epi_alnd):
            ps.ciao(1)
      else:
         ps.epi_alnd = ''

      if (not ps.anat2epi):
         ps.anat_alnd = ''

      if(ps.AddEdge):
         if(ps.output_dir=="./"):
            com = shell_com("\\rm -f AddEdge/*", eo=ps.oexec)
         else:
            com = shell_com("cd %s; \\rm -f AddEdge/*" % ps.output_dir, eo=ps.oexec)

         com.run()
         # @AddEdge requires single sub-brick, resampled data (grids must match)
         #  and skullstripped data to avoid extracranial and cranial edges
         #  so using data as it went into 3dAllineate here
         # check if resampling is turned off, and do that here
         # for epi2anat, resample epi_alnd to anat
         # if skullstripping is off, we'll have to assume it's already done

         # Note 3dAllineate does not require resampling. This script
         # provides an option to avoid resampling.
         # If @AddEdge does require resampling though, so do it now if it hasn't been done yet
         if(ps.resample_flag):
            ein_rs = e
         else:
            baseviewext = "%s%s" % (ps.epi.view, ps.epi.extension)
            ein_rs = ps.resample_epi(e, "","%s__tt_%s_rs_in%s" % \
              (ps.output_dir, ps.epi.out_prefix(),baseviewext))

         if (ps.epi2anat and ps.anat2epi):
            listlog_a2e = "a2e_examine_list.log"
            listlog_e2a = "e2a_examine_list.log"
         else :
            listlog_a2e = ""
            listlog_e2a = ""


         if (ps.epi2anat):    # if aligning epi to anatomical
            # for the edge image, we need a dataset at the resolution of the
            # anatomical and the specific epi sub-brick we used as the epi_base
            # Use 3dAllineate to create the resampled, skullstripped EPI
            # from the same representative sub-brick used to align anat to epi
            
            if((ps.output_dir == "./") or (ps.output_dir == " ")):
               outdir = ps.epi.p()
            else:
               outdir = ps.output_dir
               
            ps.epi_addedge = e.new("__tt_%s_addedge" % ps.epi.out_prefix())
            epi_mat = "%s%s%s_mat.aff12.1D" % \
                      (outdir, ps.epi.out_prefix(), ps.suffix)
            if((ps.volreg_flag) and (ps.epi_base.isdigit())):
               epi_mat = "%s%s%s_reg_mat.aff12.1D{%s}" % \
                (outdir, ps.epi.out_prefix(), suff, ps.epi_base)

              # epi_mat  = "%s{%s}" % (epi_mat, ps.epi_base) 

            ps.info_msg( "Applying transformation for %s to %s for @AddEdge" % \
              (ps.dset2_generic_name, ps.dset1_generic_name ))

            com = shell_com(  \
               "3dAllineate -base %s -1Dmatrix_apply '%s' " \
               "-prefix %s%s -input %s  -master BASE"   %   \
               ( ps.anat0.input(), epi_mat, outdir,  \
                 ps.epi_addedge.out_prefix(),               \
                 ein_rs.input()), eo=ps.oexec)
            com.run()
            ps.add_edges(ps.anat_ns, ein_rs, ps.epi_addedge,\
                     "%s_ns" % (ps.anat0.out_prefix()),     \
                     "%s_ns" % (ps.epi.out_prefix()),       \
                     "%s%s" % (ps.epi.out_prefix(), suff ), \
                      listlog = listlog_e2a) 

         if (ps.anat2epi):
            ps.add_edges(ein_rs, ps.anat_ns, ps.anat_alnd,  \
                         "%s_ns" % ps.epi.out_prefix(),     \
                         "%s_ns" % ps.anat0.out_prefix(),   \
                         "%s%s" % (ps.anat0.out_prefix(), suff), \
                         listlog = listlog_a2e)


      #Create final results
      ps.create_output(ps.anat_alnd, ps.anat_alndwt, ps.epi_alnd, ps.epi_alnd_tlrc, \
           "%s" % suff, e, a)

      # process the children
      if ps.child_epis != None: 
         for child_epi_name in ps.child_epis.parlist:
            child_epi = afni_name(child_epi_name) 
            ps.process_child_epi(child_epi)
            if (ps.rmrm):  # cleanup after the children, remove any extra files
               ps.fresh_start(child_epi.out_prefix(), apref="", rmold=0,\
                  epipath=("%s" % ps.output_dir) )

   #cleanup after the parents too?
   if (ps.rmrm):
      ps.cleanup()


   print("\n# Finished alignment successfully")
   if(ps.AddEdge):
      print("To view edges produced by @AddEdge, type:")
      print("cd AddEdge")
      if (0):  #ZSS  @AddEdge now launches AFNI
         print("afni -niml -yesplugouts &")
         
      if (ps.epi2anat and ps.anat2epi):
         print("@AddEdge -examinelist %s" % listlog_a2e)
         print("@AddEdge -examinelist %s" % listlog_e2a)
      else:
         print("@AddEdge")
            
   ps.ciao(0)
