********
whereami
********

.. _ahelp_whereami:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: whereami [x y z [output_format]] [-lpi/-spm] [-atlas ATLAS] 
       ++ Reports brain areas located at x y z mm in some template space
       ++ according to atlases present with your AFNI installation.
       ++ Show the contents of available atlases
       ++ Extract ROIs for certain atlas regions using symbolic notation
       ++ Report on the overlap of ROIs with Atlas-defined regions.
    
    Options (all options are optional):
    -----------------------------------
        x y z [output_format] : Specifies the x y z coordinates of the 
                                location probed. Coordinate are in mm and 
                                assumed to be in RAI or DICOM format, unless
                                otherwise specified (see -lpi/-spm below)
                                In the AFNI viewer, coordinate format is
                                specified above the coordinates in the top-left
                                of the AFNI controller. Right click in that spot
                                to change between RAI/DICOM and LPI/SPM.
                         NOTE I:In the output, the coordinates are reported
                                in LPI, in keeping with the convention used
                                in most publications.
                        NOTE II:To go between LPI and RAI, simply flip the 
                                sign of the X and Y coordinates.
    
                                Output_format is an optional flag where:
                                0 is for standard AFNI 'Where am I?' format.
                                1 is for Tab separated list, meant to be 
                                friendly for use in spreadsheets. 
                                The default output flag is 0. You can use
                                options -tab/-classic instead of the 0/1 flag.
     -coord_file XYZ.1D: Input coordinates are stored in file XYZ.1D
                         Use the '[ ]' column selectors to specify the
                         X,Y, and Z columns in XYZ.1D.
                         Say you ran the following 3dclust command:
               3dclust -1Dformat -1clip 0.3  5 3000 func+orig'[1]' > out.1D
                         You can run whereami on each cluster's center
                         of mass with:
               whereami -coord_file out.1D'[1,2,3]' -tab
                   NOTE: You cannot use -coord_file AND specify x,y,z on
                         command line.
     -linkrbrain: get report from linkRbrain from list of coordinates
               only with -coord_file and -space or -dset_space
     -linkr_type tasks/genes: report for correlation with tasks or genes
               Default is tasks
     -lpi/-spm: Input coordinates' orientation is in LPI or SPM format. 
     -rai/-dicom: Input coordinates' orientation is in RAI or DICOM format.
     NOTE: The default format for input coordinates' orientation is set by 
           AFNI_ORIENT environment variable. If it is not set, then the default 
           is RAI/DICOM
     -space SPC: Space of input coordinates.
           SPC can be any template space name. Without a NIML table definition,
           the space name is limited to MNI, MNI_ANAT or TLRC (the default).
     -classic: Classic output format (output_format = 0).
     -tab: Tab delimited output (output_format = 1). 
           Useful for spreadsheeting.
     -atlas ATLAS: Use atlas ATLAS for the query.
                   You can use this option repeatedly to specify
                   more than one atlas. Default is all available atlases.
                  ATLAS is one of:
     -dset: Determine the template space to use from this reference dataset
            Space for human data is usually TLRC, MNI, MNI_ANAT.
            If the space is known and a reference atlas can be found, the
            regions will be based on the coordinates from this template space.
     -atlas_sort: Sort results by atlas (default)
     -zone_sort | -radius_sort: Sort by radius of search
     -old : Run whereami in the olde (Pre Feb. 06) way.
     -show_atlas_code: Shows integer code to area label map of the atlases
                       in use. The output is not too pretty because
                       the option is for debugging use.
     -show_atlas_region REGION_CODE: You can now use symbolic notation to
                                     select atlas regions. REGION_CODE has 
                                     three colon-separated elements forming it:
                Atlas_Name:Side:Area.
          Atlas_Name: one of the atlas names listed above.
                      If you do not have a particular atlas in your AFNI
                      installation, you'll need to download it (see below).
          Side      : Either left, right or nothing(::) for bilateral.
          Area      : A string identifying an area. The string cannot contain
                      blanks. Replace blanks by '_' for example Cerebellar Vermis
                      is Cerebellar_Vermis. You can also use the abbreviated 
                      version cereb_ver and the program will try to guess at 
                      what you want and offer suggestions if it can't find the
                      area or if there is ambiguity. Abbreviations are formed
                      by truncating the components (chunks) of an area's name 
                      (label). For example:
                   1- TT_Daemon::ant_cing specifies the bilateral
                      anterior cingulate in the TT_Daemon atlas.
                   2- CA_N27_ML:left:hippo specifies the left
                      hippocampus in the CA_N27_ML atlas.
                   3- CA_N27_MPM:right:124 specifies the right
                      ROI with integer code 124 in the CA_N27_MPM atlas
                   4- CA_N27_ML::cereb_ver seeks the Cerebellar
                      Vermis in the CA_N27_ML atlas. However there
                      many distinct areas with this name so the program
                      will return with 'potential matches' or suggestions.
                      Use the suggestions to refine your query. For example:
                      CA_N27_ML::cereb_vermis_8
     -mask_atlas_region REGION_CODE: Same as -show_atlas_region, plus
                                     write out a mask dataset of the region.
     -prefix PREFIX: Prefix for the output mask dataset
     -max_areas MAX_N: Set a limit on the number of distinct areas to report.
                 This option will override the value set by the environment
                 variable AFNI_WHEREAMI_MAX_FIND, which is now set to 5
                 The variable  AFNI_WHEREAMI_MAX_FIND should be set in your
                 .afnirc file.
     -max_search_radius MAX_RAD: Set a limit on the maximum searching radius when
                         reporting results. This option will override the 
                         value set by the environment variable 
                         AFNI_WHEREAMI_MAX_SEARCH_RAD,
                         which is now set to 7.500000 .
     -min_prob MIN_PROB: set minimum probability to consider in probabilistic
                 atlas output. This option will overrid the value set by the
                 environment variable AFNI_WHEREAMI_PROB_MIN (default is 1E-10)
     NOTE: You can turn off some of the whining by setting the environment 
           variable  AFNI_WHEREAMI_NO_WARN
     -debug DEBUG: Debug flag
     -verb VERB: Same as -debug DEBUG
    
    Options for determining the percent overlap of ROIs with Atlas-defined areas:
    ---------------------------------------------------------------------------
     -bmask MASK: Report on the overlap of all non-zero voxels in MASK dataset
                  with various atlas regions. NOTE: The mask itself is not binary,
                  the masking operation results in a binary mask.
     -omask ORDERED_MASK:Report on the overlap of each ROI formed by an integral 
                         value in ORDERED_MASK. For example, if ORDERED_MASK has 
                         ROIs with values 1, 2, and 3, then you'll get three 
                         reports, one for each ROI value. Note that -omask and
                         -bmask are mutually exclusive.
     -cmask MASK_COMMAND: command for masking values in BINARY_MASK, 
                          or ORDERED_MASK on the fly.
            e.g. whereami -bmask JoeROIs+tlrc \
                          -cmask '-a JoeROIs+tlrc -expr equals(a,2)'
                  Would set to 0, all voxels in JoeROIs that are not
                  equal to 2.
            Note that this mask should form a single sub-brick,
            and must be at the same resolution as the bmask (binary mask) or
            the omask (the ordered mask) datasets.
            This option follows the style of 3dmaskdump (since the
            code for it was, uh, borrowed from there (thanks Bob!, thanks Rick!)).
            See '3dmaskdump -help' for more information.
    
    Note on the reported coordinates of the Focus Point:
    ----------------------------------------------------
    Coordinates of the Focus Point are reported in available template spaces in
    LPI coordinate order. The three principal spaces reported are Talairach 
     (TLRC), MNI, MNI Anatomical (MNI_ANAT).
      The TLRC coordinates follow the convention specified by the Talairach and 
         Tournoux Atlas.
      The MNI coordinates are derived from the TLRC ones using an approximation 
         equation.
      The MNI Anat. coordinates are a shifted version of the MNI coordinates 
         (see Eickhoff et al. 05).
    
     For users who do not use the NIML table method of specifying template 
     and transformations, the MNI coordinates reported here are derived from TLRC
     by an approximate function (the Brett transform). For transformations
     between MNI_ANAT and TLRC coordinates, the 12 piece-wise linear transformation
     that was used to transform the MNI_ANAT N27 brain to TLRC space is also
     used to compute the coordinates in either direction.
     For users who do use the NIML table method, the transformations among
     the various Talairach, MNI and MNI_ANAT spaces may be performed a variety
     of ways. The default method uses the Brett transform for TLRC to MNI, and
     a simple shift for MNI to MNI_ANAT.
    
    How To See Atlas Data In AFNI as datasets:
    ------------------------------------------
       If you want to view the atlases in the same session
       that you are working with, choose one of options below.
       For the sake of illustrations, I will assume that atlases
       reside in directory: /user/abin/
     1-Load the session where atlases reside on afni's command
       line: afni ./ /user/abin
     2-Set AFNI's environment variable AFNI_GLOBAL_SESSION
       to the directory where the atlases reside.
       You can add the following to you .afnirc file:
       AFNI_GLOBAL_SESSION = /user/abin
       Or, for a less permanent solution, you can set this environment
       variable in the shell you are working in with (for csh and tcsh):
       setenv AFNI_GLOBAL_SESSION /user/abin 
       ***********
       BE CAREFUL: Do not use the AFNI_GLOBAL_SESSION approach
       *********** if the data in your session is not already 
       written in +tlrc space. To be safe, you must have
       both +tlrc.HEAD and +tlrc.BRIK for all datasets
       in that session (directory). Otherwise, if the anat parents are
       not properly set, you can end up applying the +tlrc transform
       from one of the atlases instead of the proper anatomical 
       parent for that session.
    
       Note: You can safely ignore the:
                  ** Can't find anat parent ....  
             messages for the Atlas datasets.
    
    Convenient Color maps For Atlas Datasets:
    ----------------------------------------
       Color maps (color scales) for atlas dataset should automatically be used
       when these datasets are viewed in the overlay. To manually select a
       a specific color scale in the AFNI GUI's overlay panel:
         o set the color map number chooser to '**' 
         o right-click on the color map's color bar and select 
           'Choose Colorscale'
         o pick one of: CytoArch_ROI_256, CytoArch_ROI_256_gap, ROI_32. etc.
         o set autorange off and set the range to the number of colors 
           in the chosen map (256, 32, etc.). 
           Color map CytoArch_ROI_256_gap was created for the proper viewing
           of the Maximum Probability Maps of the Anatomy Toolbox.
    
    How To See Atlas regions overlaid in the AFNI GUI:
    --------------------------------------------------
       To see specific atlas regions overlaid on underlay and other overlay data,
         1. In Overlay control panel, check "See TT Atlas Regions" 
         2. Switch view to Talairach in View Panel
         3. Right-click on image and select "-Atlas colors". In the Atlas colors
            menu, select the colors you would like and then choose Done.
         The images need to be redrawn to see the atlas regions, for instance,
            by changing slices. Additional help is available in the Atlas colors
            menu.
       For the renderer plug-in, the underlay and overlay datasets should both
         have Talairach view datasets actually written out to disk
       The whereami and "Talairach to" functions are also available by right-
         clicking in an image window.
    
    Example 1:
    ----------
       To find a cluster center close to the top of the brain at -12,-26, 76 (LPI),
       whereami, assuming the coordinates are in Talairach space, would report:
        whereami -12 -26 76 -lpi
         ++ Input coordinates orientation set by user to LPI
         +++++++ nearby Atlas structures +++++++
    
         Original input data coordinates in TLRC space
    
         Focus point (LPI)=
            -12 mm [L], -26 mm [P],  76 mm [S] {TLRC}
            -12 mm [L], -31 mm [P],  81 mm [S] {MNI}
            -13 mm [L], -26 mm [P],  89 mm [S] {MNI_ANAT}
    
         Atlas CA_N27_MPM: Cytoarch. Max. Prob. Maps (N27)
           Within 4 mm: Area 6
           Within 7 mm: Area 4a
    
         Atlas CA_N27_ML: Macro Labels (N27)
           Within 1 mm: Left Paracentral Lobule
           Within 6 mm: Left Precentral Gyrus
              -AND- Left Postcentral Gyrus
    
    Example 2:
    ----------
       To create a mask dataset of both  left and right amygdala, you can do:
        whereami -prefix amymask -mask_atlas_region 'TT_Daemon::amygdala'
    
    
       Note masks based on atlas regions can be specified "on the fly" in 
       the same way with other afni commands as a dataset name (like 3dcalc,
       for instance), so a mask, very often, is not needed as a separate,
       explicit dataset on the disk.
    
    
    Example 3:
    ----------
       To create a mask from a FreeSurfer 'aparc' volume parcellation:
       (This assumes you have already run @SUMA_Make_Spec_FS, and your
        afni distribution is recent. Otherwise update afni then run:
        @MakeLabelTable -atlasize_labeled_dset aparc.a2009s+aseg_rank.nii
        from the SUMA/ directory for that subject.)
       To find the region's name, try something like:
        whereami -atlas aparc.a2009s+aseg_rank -show_atlas_code | grep -i insula
       Or you can try this search, assuming you screwed up the spelling:
       whereami -atlas aparc+aseg_rank -show_atlas_code | \
                                      apsearch -word insola -stdin
       If you really screw up the spelling try:
       whereami -atlas aparc+aseg_rank -show_atlas_code | \
                                      sed 's/[-_]/ /g'  | \
                                      apsearch -word insolent -stdin
       Pick one area then run:
        whereami -atlas aparc.a2009s+aseg_rank \
                   -mask_atlas_region   \
                         aparc.a2009s+aseg_rank::ctx_rh_S_circular_insula_sup
    
    
     
    ---------------
     Atlas NIML tables:
     Atlas, templates, template spaces and transforms may all now be specified
     in a text file that follows an XML-like format, NIML. The specifications
     for the NIML table files will be described more fully elsewhere, but an
     overview is presented here. By default, and soon to be included with the
     AFNI distributions, the file AFNI_atlas_spaces.niml contains entries for
     each of the available atlases, template spaces, templates and 
     transformations. Two other additional files may be specified and changed
     using the environment variables, AFNI_SUPP_ATLAS and AFNI_LOCAL_ATLAS.
     It is best to examine the provided NIML table as an example for extending
     and modifying the various atlas definitions.
    
     Show atlas NIML table options:
     -show_atlases          : show all available atlases
     -show_templates        : show all available templates
     -show_spaces           : show all available template spaces
     -show_xforms           : show all available xforms
     -show_atlas_all        : show all the above
    
     -show_available_spaces srcspace : show spaces that are available from
                 the source space
     -show_chain srcspace destspace : show the chain of transformations
                 needed to go from one space to another
     -calc_chain srcspace destspace : compute the chain of transformations
                 combining and inverting transformations where possible
         examples: convert coordinates from TT_N27 to MNI or MNI anat space
                 whereami -calc_chain TT_N27 MNI  -xform_xyz_quiet 10 20 30
                 whereami -calc_chain TT_N27 MNI  -xform_xyz_quiet 0 0 0
                 whereami -calc_chain TT_N27 MNIA -xform_xyz_quiet 0 0 0
     -xform_xyz : used with calc_chain, takes the x,y,z coordinates and 
                 applies the combined chain of transformations to compute
                 a new x,y,z coordinate
     -xform_xyz_quiet : Same as -xform_xyz but only ouputs the final result
     -coord_out  outfile : with -xform_xyz, -coord_file and -calc_chain, 
                 specifies an output file for transformed coordinates
                 If not specified, coord_files will be transformed and printed
                 to stdout 
    Note setting the environment variable AFNI_WAMI_DEBUG will show detailed
     progress throughout the various functions called within whereami.
     For spaces defined using a NIML table, a Dijkstra search is used to find
     the shortest path between spaces. Each transformation carries with it a
     distance attribute that is used for this computation. By modifying this
     field, the user can control which transformations are preferred.
    
     -web_atlas_type XML/browser/struct : report results from web-based atlases
                using XML output to screen, open a browser for output or just
                return the name of the structure at the coordinate
     -html   :  put whereami output in html format for display in a browser
     
    ---------------
     More information about Atlases in AFNI can be found here:
          https://afni.nimh.nih.gov/sscc/dglen/AFNIAtlases
     Class document illustrating whereami usage:
          https://afni.nimh.nih.gov/pub/dist/edu/latest/afni11_roi/afni11_roi.pdf
    ---------------
    Global Options (available to all AFNI/SUMA programs)
      -h: Mini help, at time, same as -help in many cases.
      -help: The entire help output
      -HELP: Extreme help, same as -help in majority of cases.
      -h_view: Open help in text editor. AFNI will try to find a GUI editor
      -hview : on your machine. You can control which it should use by
               setting environment variable AFNI_GUI_EDITOR.
      -h_web: Open help in web browser. AFNI will try to find a browser.
      -hweb : on your machine. You can control which it should use by
              setting environment variable AFNI_GUI_EDITOR. 
      -h_find WORD: Look for lines in this programs's -help output that match
                    (approximately) WORD.
      -h_raw: Help string unedited
      -h_spx: Help string in sphinx loveliness, but do not try to autoformat
      -h_aspx: Help string in sphinx with autoformatting of options, etc.
      -all_opts: Try to identify all options for the program from the
                 output of its -help option. Some options might be missed
                 and others misidentified. Use this output for hints only.
      
       -overwrite: Overwrite existing output dataset.
                   Equivalent to setting env. AFNI_DECONFLICT=OVERWRITE
       -ok_1D_text: Zero out uncommented text in 1D file.
                    Equivalent to setting env. AFNI_1D_ZERO_TEXT=YES
       -Dname=val: Set environment variable 'name' to value 'val'
                 For example: -DAFNI_1D_ZERO_TEXT=YES
       -Vname=: Print value of environment variable 'name' to stdout and quit.
                This is more reliable that the shell's env query because it would
                include envs set in .afnirc files and .sumarc files for SUMA
                programs.
                 For example: -VAFNI_1D_ZERO_TEXT=
       -skip_afnirc: Do not read the afni resource (like ~/.afnirc) file.
       -pad_to_node NODE: Output a full dset from node 0 to MAX_NODE-1
                       ** Instead of directly setting NODE to an integer you 
                          can set NODE to something like:
                       ld120 (or rd17) which sets NODE to be the maximum 
                          node index on an Icosahedron with -ld 120. See 
                          CreateIcosahedron for details.
                       d:DSET.niml.dset which sets NODE to the maximum node found
                          in dataset DSET.niml.dset.
                       ** This option is for surface-based datasets only.
                          Some programs may not heed it, so check the output if
                          you are not sure.
       -pif SOMETHING: Does absolutely nothing but provide for a convenient
                       way to tag a process and find it in the output of ps -a
       -echo_edu: Echos the entire command line to stdout (without -echo_edu)
                  for edification purposes
    
    Thanks to Kristina Simonyan for feedback and testing.
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
