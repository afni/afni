
/** cf. afni_history.h **/

#include "afni_history.h"

/*  (for starting a new file, search for CHANGE)

    basic format: 3-field date, user, program_name, impact_level,
                  short description without newline
                  (optional) long descrption with intermediate newlines
  
    copy entire section: { ... } ,
  
    Notes: - months are JAN ... DEC (see afni_history.h)

           - levels are :
                    MICRO           - users don't see
                    MINOR           - small affect on users
                    MAJOR           - larger affect on users
                    SUPER           - important changes, like new programs
                    SUPERDUPER      - we expect users to know

           - types are:
                    TYPE_GENERAL    - unspecified update type
                    TYPE_NEW_PROG   - new program
                    TYPE_NEW_OPT    - new program option
                    TYPE_NEW_ENV    - new environment variable or change
                    TYPE_BUG_FIX    - bug fix
                    TYPE_MODIFY     - a change (not new, not a fix)

           - PLEASE, stick to what fits on an 80 column terminal
           - it may be nice to put the newest entires at the top
           - leave the last "99, NULL" entry as it is

 -- example --

 { 26 , FEB , 2008 , ZSS , "my_program" , MAJOR ,
   "short description of change" ,
   "(optional) description of where to get more information\n"
   "   - with newlines, if you babble for multiple lines\n"
   "   (but none at the end)"
 } ,

*/

/* Ponderosos:
   -----------
   - Every bone of my body shivers with excitement at the
   thought of adding a type variable :
   { NA, NEW_PROG, NEW_OPT, NEW_ENV, BUG_FIX }
   It would allow users to check for new things easily.
   And it might help eradicate misery from this world.
   Did I also mention that it can fit on the first line
   beautfully, right after the short string? WOW!
   - We should probably start building a program list. To make sure 
   that we have no typos there. We can have users add to that list 
   whenever they introduce a program for the first time. This way 
   nobody has to do it at once. 
   - Also, we need to have special 'program names' like: 
   "afni-general", "suma-general", "general"
   or perhaps
   "AFNI", "SUMA", "ALL"
   these would be used to refer to package-wide changes.
*/ 

afni_history_struct ziad_history[] = {
/*=====BELOW THIS LINE=====*/
 { 4, Oct , 2012 , ZSS , "MapIcosahedron" , MICRO , TYPE_BUG_FIX,
   "Dset Mapping failed on sparse datasets, at times. ",
   NULL
 },

 { 4, Oct , 2012 , ZSS , "3dedge3" , MICRO , TYPE_MODIFY,
   "Updated 3DEdge library to GPL version: 2012-02-22",
   NULL
 },

 { 4, Oct , 2012 , ZSS , "afni-matlab" , MINOR , TYPE_NEW_OPT,
   "Minor tweaks to WriteBrik and BrikInfo",
   "WriteBrik can automatically set some header fields (AdjustHeader option)\n"
   "BrikInfo now loads IJK_TO_DICOM_REAL"
 },

 { 28, Sep , 2012 , ZSS , "@T1scale" , MINOR , TYPE_NEW_PROG,
   "A mini script to scale T1s by PD volumes",
   "Script uses method borrowed from @CalculateSignatures\n"
   "to reduce bias field in T1 using a PD volume. Script also\n"
   "works well without PD."
 },

 { 28, Sep , 2012 , ZSS , "@auto_tlrc" , MINOR , TYPE_NEW_OPT,
   "Enabled automatic centering via -init_xform",
   "You can use -init_xform AUTO_CENTER or -init_xform CENTER\n"
   "to perform center alignment during registration."
 },

 { 28, Sep , 2012 , ZSS , "afni-general" , MINOR , TYPE_NEW_PROG,
   "Added Paul Taylor's map_TrackID to warp tracks by an affine transform",
   NULL
 },

 { 28, Sep , 2012 , ZSS , "3dTrackID" , MICRO , TYPE_NEW_OPT,
   "Option -rec_orig to record dataset origin in tractography output",
   NULL
 },

 { 22, Sep , 2012 , ZSS , "@SUMA_Make_Spec_Caret" , MICRO , TYPE_MODIFY,
   "Updated script to work with current Caret release",
   NULL
 },

 { 22, Sep , 2012 , ZSS , "3dkmeans" , MICRO , TYPE_BUG_FIX,
   "Fixed default prefix which was overwriting input.",
   "Also added default selection when input has single value"
 },

 { 21, Sep , 2012 , ZSS , "afni-general" , MICRO , TYPE_MODIFY,
   "pkundu update",
   NULL
 },


 { 18, Sep , 2012 , ZSS , "afni-general" , MINOR , TYPE_NEW_PROG,
   "Checked in Prantik Kundu's MEICA tools.",
   "Try meica.py -help, also try @Install_MEICA_Demo"
 },

 { 13, Sep , 2012 , ZSS , "3dhistog" , MINOR , TYPE_NEW_OPT,
   "Added -pdf to 3dhistog to make area = 1",
   NULL
 },

 { 12, Sep , 2012 , ZSS , "3dttest++" , MICRO , TYPE_BUG_FIX,
   "Fixed incorrect warning in 3dttest++ about labels and filenames",
   "The warning is intented to be sure users don't mess up the\n"
   "-setA option when using the long form. The warning message\n"
   "was being triggered incorrectly, this should no longer be the case.\n"
   "Warning did not affect results."
 },

 { 12, Sep , 2012 , ZSS , "afni-general" , MICRO , TYPE_BUG_FIX,
   "Stupid null character termination missing in THD_filepath()",
   NULL
 },

 { 11, Sep , 2012 , ZSS , "suma" , MICRO , TYPE_BUG_FIX,
   "Fixed potential corruption in macro SUMA_NEL_REPLACE_STRING",
   "Not sure to make of this problem, but bad reads/writes\n"
   "are happening in m_rc[(row)] in the macro, and to cs[n0++]\n"
   "in cs[n0++] = lbl[i]; of SUMA_AddColAtt_CompString(). In fact\n"
   "the first problem happens in cs[n0++], so the problem might\n"
   "start there. In any case, the fix was to increase the length\n"
   "of allocated segment by +10 instead of +1 in SUMA_NEL_REPLACE_STRING\n"
   "That seems to do the trick.\n"
 },

 { 11, Sep , 2012 , ZSS , "suma" , MINOR , TYPE_MODIFY,
   "Began merger of surface controllers",
   "The idea is to have all surface controllers in one window\n"
   "in order to save on screen space. Looks promising but must be\n"
   "rolled out carefully because surface controllers permeate everything."
   "Use environment variable SUMA_SameSurfCont to turn feature on.\n"
   "Make sure it is debugged with valgrind too.\n"
 },

 { 11, Sep , 2012 , ZSS , "suma" , MINOR , TYPE_BUG_FIX,
   "Fixed 'potential' crash in SUMA_SetScaleThr_one()",
   NULL
 },

 { 10, Sep , 2012 , ZSS , "suma" , MINOR , TYPE_BUG_FIX,
   "Typing in sub-brick arrow fields was ignored on linux",
   "Problem due to incorrect error checking from strtod()"
 },

 { 7, Sep , 2012 , ZSS , "suma" , MICRO , TYPE_NEW_OPT,
   "SUMA now highlights cluster |max|, rather than max",
   NULL
 },

 { 6, Sep , 2012 , ZSS , "@RetinoProc" , MINOR , TYPE_NEW_OPT,
   "Added -no_volreg for time series already registered",
   NULL
 },

 { 6, Sep , 2012 , ZSS , "suma" , MICRO , TYPE_BUG_FIX,
   "SUMA's sub-brick arrowfields were not updating properly",
   NULL
 },

 { 5, Sep , 2012 , ZSS , "@CalculateSignatures" , MINOR , TYPE_NEW_OPT,
   "Added possiblity to scale by top percentiles with -FATscl",
   "This would produce .sc9 outputs. It looks like the scaling is\n"
   "more promising than by MEDIAN(20) or MAD(20)."
 },

 { 5, Sep , 2012 , ZSS , "1dRplot" , MINOR , TYPE_NEW_OPT,
   "Made program take in histograms produced by 3dGenFeatureDist",
   "This required changes to AFNIio.R so that NIML groups are not\n"
   "automatically sent to the distbin. Instead, the first element\n"
   "in the ni_group is returned."
 },

 { 5, Sep , 2012 , ZSS , "3dGenFeatureDist" , MICRO , TYPE_NEW_OPT,
   "Program now creates the feature correlation matrix per class.",
   "Both histograms and correlation matrices are stored in NIML\n"
   "format under a directory named by the user."
 },

 { 4, Sep , 2012 , ZSS , "3ddot" , MINOR , TYPE_NEW_OPT,
   "Made program output niml 1D format, makes it easy to plot with 1dRplot",
   NULL
 },

 { 30, Aug , 2012 , ZSS , "3ddot" , MINOR , TYPE_NEW_OPT,
   "Made program create corr. matrix as opposed to just one pair of sub-bricks",
   "Output is also beautified with option -show_labels\n"
 },

 { 29, Aug , 2012 , ZSS , "afni-general" , MINOR , TYPE_BUG_FIX,
   "AFNI build was failing on machines where DONT_USE_MCW_MALLOC was defined",
   "The cause of failure was a missing #define NI_calloc() when \n"
   " DONT_USE_MCW_MALLOC is defined. The one line needed is now in niml.h.\n"
   "Builds likely affected are solaris and macosx_10.7_Intel_64\n"
   "Affected build dates from about Aug. 24th  to Aug. 29th.\n"
 },

 { 29, Aug , 2012 , ZSS , "suma" , MINOR , TYPE_NEW_OPT,
   "Allowing yoking of intensity to node selection.",
   "Ctrl+Button-3 would select a node and switch intensity sub-brick\n"
   "to that node's index * K. This is only done if the dataset currently\n"
   "viewed has as K times as many sub-bricks as the surface has nodes.\n"
   "K being an integer, naturally.\n Nick Oosterhoff instigated this business\n"
 },

 { 28, Aug , 2012 , ZSS , "suma" , MICRO , TYPE_BUG_FIX,
   "Fixed interaction with L/R yoking asynchrony.",
   "Problems included interaction with clustering setting and with\n"
   "Intensity/Threshold/Brightness selectors."
 },

 { 28, Aug , 2012 , ZSS , "suma" , MICRO , TYPE_NEW_OPT,
   "Preserved controller settings for newly loaded or reloaded dsets",
   "The preserved settings include clustering options."
 },

 { 28, Aug , 2012 , ZSS , "suma" , MICRO , TYPE_BUG_FIX,
   "Fixed some L/R yoking problems and a crash source in drive mode.",
   "The crash was caused when calling SUMA_Set_Menu_Widget() on a controller\n"
   "not fully realized.\n"
 },

 { 27, Aug , 2012 , ZSS , "afni-general" , MICRO , TYPE_MODIFY,
   "Modifed THD_load_tcat() to include subbrick labels in auto catenation",
   "Also, started checking for datatype mixing and issuing warning when needed."
 },

 { 24, Aug , 2012 , ZSS , "suma" , MICRO , TYPE_BUG_FIX,
   "Fixed SUMA<-->AFNI cross hair linkage with multiple anat. correct surfaces",
   "It used to be that a coordinate always got attached to the Local Domain\n"
   "Parent surface. Even if you clicked closest to a node on the Pial surface\n"
   "SUMA would jump to the correspoding node on the smoothwm surface. This was\n"
   "way uncool."
 },

 { 24, Aug , 2012 , ZSS , "@ScaleVolume" , MICRO , TYPE_BUG_FIX,
   "step(a) was used instead of bool(a) for masking operations",
   "This caused zeros in the output where input values were negative.\n"
   "Also added a modifier to the labels to highlight that features were\n"
   "scaled."
 },

 { 24, Aug , 2012 , ZSS , "@CalculateSignatures" , MINOR , TYPE_NEW_OPT,
   "Added coordinate feature generation, if TLRC xform is provided",
   "Note that the TLRC xform need not be too precise. The coordinate\n"
   "features are not intended to make sure of segmentation templates."
 },

 { 24, Aug , 2012 , ZSS , "suma" , MAJOR , TYPE_NEW_OPT,
   "Added interactive clustering to surface controller",
   "The state of affairs:\n"
   "  -Clustering tables are only output to the terminal \n"
   "     and the log window.\n"
   "  -Clustering is yoked between hemispheres\n"
   "  -Can control connectivity radius and min area or min nodes\n"
   "  -When you click on a node inside a cluster, the node in that\n"
   "     cluster with the highest value is highlighted with a black\n"
   "     sphere." 
 },

 { 24, Aug , 2012 , ZSS , "niprobe" , MICRO , TYPE_NEW_PROG,
   "A variant on niccc to handle certain NIML file probing operations",
   NULL
 },

 { 24, Aug , 2012 , ZSS , "@SUMA_Make_Spec_FS" , MICRO , TYPE_MODIFY,
   "Made script port more datasets from FreeSurfer (depth, curvature)",
   NULL
 },

 { 24, Aug , 2012 , ZSS , "SurfClust" , MICRO , TYPE_NEW_OPT,
   "Allow -i* form of surface input",
   NULL
 },

 { 24, Aug , 2012 , ZSS , "afni-general" , MINOR , TYPE_NEW_PROG,
   "Added Paul Taylor's 3dRSFC to the distribution",
   NULL
 },

 { 20, Aug , 2012 , ZSS , "SurfExtrema" , MICRO , TYPE_NEW_OPT,
   "Made default input the convexity of the surface",
   NULL
 },

 { 15, Aug , 2012 , ZSS , "3drefit" , MINOR , TYPE_NEW_OPT,
   "Added -sublabel_prefix and -sublabel_suffix",
   NULL
 },

 { 8, Aug , 2012 , ZSS , "suma" , MINOR , TYPE_MODIFY,
   "Improved handling of coords in 'cm' units and better axis text layout",
   "Surfaces with coords in cm were poorly displayed and without warning.\n"
   "Now you are urged to make use of the SUMA_NodeCoordsUnits env . \n"
   "Axis text labels were also improved to reduce clobbering."
 },

 { 19, Jul , 2012 , ZSS , "suma" , MAJOR , TYPE_NEW_OPT,
   "Allow for L/R hemi yoking for many operations",
   "These include:"
   "Threshold setting. I,T,B sub-brick selection. Range \n"
   "setting. Dset loading. Cmap changing. Dset switching.\n"
   "Order changing."
 },

 { 19, Jul , 2012 , ZSS , "suma" , MICRO , TYPE_NEW_ENV,
   "New variables to initialize range setting and symmetric range",
   "See SUMA_Auto_I_Range, SUMA_Auto_B_Range, and SUMA_Sym_I_Range\n"
   "in ~/.sumarc . If you don't see them, time to run: suma -update_env\n"
 },

 { 18, Jul , 2012 , ZSS , "afni-general" , MINOR , TYPE_NEW_PROG,
   "Added Paul Taylor's 3dReHo to the distribution",
   NULL
 },

 { 10, Jul , 2012 , ZSS , "afni-general" , MINOR , TYPE_BUG_FIX,
   "Made default prefix path be ./ instead of path of input",
   "The change was done to function EDIT_empty_copy() which assigned\n"
   "to a pathless prefix the path of the input dataset if the latter\n"
   "was specified. The problem was that something like:\n"
   "3dMean -prefix mmm P1/joe+orig P2/jane+orig \n"
   "would have written mmm+orig under P1/\n"
   "To make matters less palatable other programs like 3dcalc behaved \n"
   "differently: 3dcalc -prefix ccc -a P1/joe+orig -expr 'a' \n"
   "would produce ./ccc+orig \n" 
 },
 
 { 6, Jul , 2012 , ZSS , "suma" , MINOR , TYPE_NEW_OPT,
   "Use ArrowFields when datasets have large numbers of sub-bricks",
   "The switch is done automatically and is controlled with env:\n"
   "SUMA_ArrowFieldSelectorTrigger"
 },
 
 { 6, Jul , 2012 , ZSS , "suma" , MINOR , TYPE_NEW_ENV,
   "SUMA_ArrowFieldSelectorTrigger set threshold for using ArrowFields",
   "For datasets with sub-bricks >= SUMA_ArrowFieldSelectorTrigger\n"
   "and arrow field is used to switch between sub-bricks"
 },
 
 { 28, Jun , 2012 , ZSS , "ROIgrow" , MINOR , TYPE_NEW_OPT,
   "Added -insphere and -inbox options",
   "These options grow ROIs by finding nodes that fall inside a box or sphere\n"
   "of preset size around nodes in the original ROIs."
 },
 
 { 19, Jun , 2012 , ZSS , "DriveSuma" , MINOR , TYPE_NEW_OPT,
   "Added -switch_cmode to allow switching how values map to colors",
   "This controls the 'Col' menu in SUMA's surface controller.\n" 
 },
 
 { 19, Jun , 2012 , ZSS , "suma" , MINOR , TYPE_BUG_FIX,
   "Ctrl+W was not saving datasets when filenames contained a path",
   "Problem had to do with clumsy prefix forming.\n" 
 },
 
 { 4, Jun , 2012 , ZSS , "@Install_AfniRetinoDemo" , MICRO , TYPE_NEW_PROG,
   "Demo script to download and run Retinotopy pipeline demo",
   NULL
 },
 
 { 4, Jun , 2012 , ZSS , "@Install_TSrestMovieDemo" , MICRO , TYPE_NEW_PROG,
   "Demo script to make a resting state movie a la Daniel Margulies'",
   NULL
 },
 
 { 1, Jun , 2012 , ZSS , "3dROIstats" , MICRO , TYPE_BUG_FIX,
   "3dROIstats crashed on float dsets with scaling. Not anymore",
   "Problem was scaling brick pointer without mallocizing first."
 },
 
 { 31, May , 2012 , ZSS , "afni-general" , MINOR , TYPE_MODIFY,
   "Merged ptaylor's code changes into the distribution",
   NULL
 },
 
 { 25, May , 2012 , ZSS , "SurfToSurf" , MICRO , TYPE_BUG_FIX,
   "Improve auto-name generation and enabled output format specification",
   NULL
 },
 
 { 24, May , 2012 , ZSS , "afni-general" , MINOR , TYPE_MODIFY,
   "Merger of changes in P. Taylor's code",
   NULL
 },
 
 { 23, May , 2012 , ZSS , "afni-general" , MICRO , TYPE_NEW_OPT,
   "Allow label-based selection for annotation files",
   "See function: process_NSD_labeltable(), now you can do something like:\n"
   "3dcalc -a std.60.lh.aparc.a2009s.annot.niml.dset'<wm_lh_G_precentral>' \\\n"
   "       -expr 'step(a)' -prefix wm_lh_G_precentral.niml.dset \n"
 },
 
 { 21, May , 2012 , ZSS , "ROIgrow" , MICRO , TYPE_BUG_FIX,
   "ROIgrow was not taking -spec surface definition",
   NULL
 },
 
 { 21, May , 2012 , ZSS , "@SUMA_Make_Spec_FS" , MINOR , TYPE_NEW_OPT,
   "Made the script also port thickness data and take them to std. space",
   NULL
 },
 
 { 21, May , 2012 , ZSS , "3dTcorrelate" , MINOR , TYPE_NEW_OPT,
   "Added -covariance option, turned off DOF setting for anything but pearson.",
   "See function THD_covariance()\n"
 },
 
 { 21, May , 2012 , ZSS , "MapIcosahedron" , MINOR , TYPE_NEW_OPT,
   "Made program output mapping info file for use with SurfToSurf",
   "Also made microscopic improvement to automatic dset name generation for"
   " -dset_map"
 },
 
 { 21, May , 2012 , ZSS , "SurfToSurf" , MICRO , TYPE_BUG_FIX,
   "Microscopic improvement to automatic dset name generation for output dsets",
   NULL
 },
 
 { 15, May , 2012 , ZSS , "1dRplot" , MICRO , TYPE_NEW_OPT,
   "Added -load.Rdat to regenerate previous plot on command line",
   NULL
 },
 
 { 15, May , 2012 , ZSS , "afni-general" , MINOR , TYPE_NEW_OPT,
   "Added '[1dcat FF.1D]' syntax for sub-brick selections in 1D file",
   "This was added for the same reason '[count ...]' was added, and \n"
   "that is to allow for lengthy selection values. Dealing with selections\n"
   "of 100+ sub-bricks virtually guarantees some string limit is reached,\n"
   "somewhere. See 3dTcat -help for details"
 },
 
 { 8, May , 2012 , ZSS , "afni-general" , MINOR , TYPE_MODIFY,
   "Code updates for P. Taylor's tractography",
   NULL
 },
 
 { 8, May , 2012 , ZSS , "3dSeg" , MINOR , TYPE_BUG_FIX,
   "Fixed memory leak problem",
   NULL
 },
 
 { 8, May , 2012 , ZSS , "count" , MINOR , TYPE_NEW_OPT,
   "Added -form to count: count -form %c 49 130",
   NULL
 },
 
 { 4, May , 2012 , ZSS , "afni" , MINOR , TYPE_NEW_OPT,
   "Added inverse distance measures to InstaCorr",
   "For the moment, those options are only accessible to usernames \n"
   " rwcox and ziad. Relevant functions are:\n"
   "  THD_vectim_distance(), and THD_distance(). THD_distance() is not\n"
   "used at the moment, it would be from 3dLocalBistat once I get around\n"
   "to testing it. For now, its access from 3dLocalBistat is #if-ed out.\n"
 },
 
 { 27, APR , 2012 , ZSS , "afni" , MINOR , TYPE_NEW_OPT,
   "Added percentile thresholding to AFNI's interface",
   "Relevant functions:\n"
   "  flush_vinfo_sort();\n"
   "  flush_3Dview_sort();\n"
   "  get_3Dview_sort();\n"
   "  AFNI_thresh_from_percentile()\n"
   "Relevant structure variables:\n"
   "  cont_perc_thr in Three_D_View;\n"
   "  th_sort, N_th_sort, th_sortid in AFNI_view_info;\n"
   "Feature also accessible from plugout_drive with: SET_FUNC_PERCENTILE +/-\n"
 },
 
 { 26, APR , 2012 , ZSS , "afni-general" , MINOR , TYPE_BUG_FIX,
   "Made header_name be same as brik_name for NIFTI dsets",
   "Changes were in EDIT_dset_items(), search for April 26"
 },
 
 { 24, APR , 2012 , ZSS , "suma" , MINOR , TYPE_NEW_OPT,
   "Can read .niml.tract files as DOs. Nothing too fancy yet",
   "Tractography files are loaded and handled like any DO.\n"
   "See function SUMA_DrawTractDO().\n"
 },
 
 { 24, APR , 2012 , ZSS , "3dTrackID" , MINOR , TYPE_MODIFY,
   "Modified 3dTrackID to make use of new track I/O functions.",
   NULL
 },
 
 { 24, APR , 2012 , ZSS , "afni-general" , MINOR , TYPE_MODIFY,
   "Wrote TrackIO.[ch] to handle tractography data I/O.",
   "All changes are under ptaylor/ . New tract format is in NIML.\n"
   "See functions in TrackIO.h for details.\n"
 },
 

#if 0 /* not yet */
 { 16, APR , 2012 , ZSS , "BrainSkin" , MINOR , TYPE_NEW_PROG,
   "Early version to create a brain envelope surface isotopic with pial surf",
   "Many functions added/modified for surface/volume intersections\n"
   "See SUMA_SurfaceInterectionVolume(), SUMA_Push_Nodes_To_Hull(),\n"
   "SUMA_VoxelToSurfDistances(), SUMA_GetOffset2bytemask(),\n"
   "SUMA_NodeNeighborMask(), SUMA_NN_GeomSmooth[23],\n"
   "SUMA_SmoothAttr_Neighb_wght(), SUMA_CurvatureToDset(), \n"
   "SUMA_mri_volume_infill_solid(), etc.\n"
   "New convenience functions: SUMA_*_Smooth_SO, SUMA_OrientSOTriangles(),\n"
   "SUMA_THD_IsoSurface(), and SUMA_Mesh_Resample_nodes(). \n"
   "Macros SUMA_RECOMPUTE*, SUMA_CLOSEST_NODE_VEC.\n"
   "New triangle intersection function: SUMA_MT_count_intersect_triangle().\n"
 },

 { 16, APR , 2012 , ZSS , "SurfSmooth" , MINOR , TYPE_NEW_OPT,
   "Added -NN_geom[23] for controlled inflation.",
   NULL
 },
 
#endif

 { 16, APR , 2012 , ZSS , "SurfaceMetrics" , MINOR , TYPE_MODIFY,
   "Improved output format and added Displayable Objects to output.",
   NULL
 },

 { 16, APR , 2012 , ZSS , "SampBias" , MINOR , TYPE_MODIFY,
   "Improved I/O",
   NULL
 },

 { 16, APR , 2012 , ZSS , "SurfToSurf" , MINOR , TYPE_NEW_OPT,
   "Added -closest_possible\n",
   "For allowing the substitution of the projection result with \n"
   "the closest node that could be found along any direction.\n"
   "See changes to SUMA_GetM2M_NN()"
 },

 { 16, APR , 2012 , ZSS , "DriveSuma" , MINOR , TYPE_NEW_OPT,
   "Added -do_draw_mask to restrict where node-based DOs are shown",
   NULL
 },

 { 16, APR , 2012 , ZSS , "suma" , MINOR , TYPE_NEW_OPT,
   "More I/T selector linkage modes.",
   NULL
 },

 { 16, APR , 2012 , ZSS , "suma" , MINOR , TYPE_NEW_OPT,
   "Show data value at crosshair directly in display",
   "See SUMA_UpdateCrossHairNodeLabelFieldForSO(),\n"
   "SUMA_FormNodeValFieldStrings(), and SUMA_UpdateNodeValField()\n"
 },

 { 16, APR , 2012 , ZSS , "suma" , MINOR , TYPE_NEW_OPT,
   "Better handling on multiline centering for display in viewer window.",
   NULL
 },

 { 16, APR , 2012 , ZSS , "suma" , MINOR , TYPE_NEW_OPT,
   "New option to control which node-based DOs are displayed.",
   "See SUMA's interactive help on 'Ctrl+p' for details.\n"
 },

 { 16, APR , 2012 , ZSS , "suma-general" , MINOR , TYPE_MODIFY,
   "New convenience functions for I/O",
   "SUMA_AddDsetIndexCol(), SUMA_CreateFullDsetPointer, and\n"
   "macro SUMA_DSET_NAME_CHECK "
   "Improved name parsing and format guessing.\n"
 },

 { 16, APR , 2012 , ZSS , "afni-general" , MINOR , TYPE_MODIFY,
   "Created ptaylor/ under src/ to include Paul Taylor's code contributions",
   "To compile all of Paul's programs: cd src/ptaylor; make all \n"
   "or from src/ make ptaylor_all\n"
   "Added macros AFNI_3D_to_1D_index and AFNI_1D_to_3D_index in 3ddata.h"
 },

 { 16, APR , 2012 , ZSS , "plugout_drive" , MICRO , TYPE_MODIFY,
   "Cosmetic error message handling to help debugging SLaconte problems",
   NULL
 },

 { 16, APR , 2012 , ZSS , "afni-general" , MICRO , TYPE_MODIFY,
   "Added new help features to bunch of programs",
   "1dCorrelate, 1dTsort, 1ddot, 3dANOVA*, 1dplot, 3dAutobox, cat_matvec,\n"
   "waver\n"
 },

 { 16, APR , 2012 , ZSS , "afni" , MICRO , TYPE_BUG_FIX,
   "Increased allocated space for bigmap variables in display.c",
   "This is to stop a a MCW_malloc post-corruption which happens\n"
   "under certain compiler/OS combinations. No big deal.\n"
 },

 { 16, APR , 2012 , ZSS , "1dTsort" , MICRO , TYPE_NEW_OPT,
   "Added -imode to return the mode of 1D file content.",
   NULL
 },

 { 22, MAR , 2012 , ZSS , "@Spharm.examples" , MINOR , TYPE_MODIFY,
   "Improvements to the script to make it work with new programe versions.",
   "It will now download its own data for demo purporses.\n"
 },

 { 22, MAR , 2012 , ZSS , "suma" , MINOR , TYPE_NEW_OPT,
   "Add values at node to the display.",
   "This involved an improvement to the centering of text so that\n"
   "one can center each line in a paragraph",
 },

 { 22, MAR , 2012 , ZSS , "SurfaceMetrics" , MICRO , TYPE_NEW_OPT,
   "Now outputs NIML dsets and Displayable objects for certain metrics",
   NULL,
 },

 { 22, MAR , 2012 , ZSS , "suma-general" , MINOR , TYPE_NEW_OPT,
   "Improve display of node-based DOs. ctrl+p for showing subset of objects.",
   "Node-based DOs overwhelm the display. Ctrl+p allows one to show DOs\n"
   "only around the selected node, or just at it. \n"
   "See also DriveSuma's -do_mask_mode option.",
 },

 { 6, MAR , 2012 , ZSS , "afni-general" , MICRO , TYPE_BUG_FIX,
   "Option completion killed filename completion. Not anymore.",
   NULL,
 },

 { 5, MAR , 2012 , ZSS , "suma" , MINOR , TYPE_BUG_FIX,
   "Recorder saving was crashing for very long filenames. Not anymore.",
   "The shell however will not like very long names and will complain."
 },

 { 5, MAR , 2012 , ZSS , "afni-general" , MINOR , TYPE_NEW_OPT,
   "Added option auto-completion for AFNI's programs for csh users",
   "The implementation of this feature is via apsearch. In particular,\n"
   "apsearch -update_all_afni_help now creates the necessary commands\n"
   "for csh's complete program to know about available options.\n"
   "See apsearch -help for details.\n"
   "This was done in conjunction with Brian Pittman.\n"
 },

 { 1, MAR , 2012 , ZSS , "suma" , MINOR , TYPE_BUG_FIX,
   "Fixed suma crash after a plot a certain plot freeze sequence",
   "The sequence was: create plot, freeze it, get another open\n"
   "then close the frozen one. At this point, suma lost control\n"
   "of the still unfrozen one, so it is practically frozen.\n"
   "Pressing Freeze twice on that remaining plot cause suma to crash.\n"
   "None of that occurs anymore.\n"
 },

 { 1, MAR , 2012 , ZSS , "suma" , MINOR , TYPE_BUG_FIX,
   "Fixed cutting plane motion while volume rendering under linux",
   "On Macs, the scroll wheel gives out button 6 and 7, on linux\n"
   "it is Buttons 4 and 5.\n"
 },

 { 1, MAR , 2012 , ZSS , "afni" , MINOR , TYPE_BUG_FIX,
   "Some cleanups and tweaks of XmHTML library.",
   "Refresh was not working well when scrolling so I ended up adding a mini\n"
   "refresh routine that seems to do the trick. XmHTMLRefresh(Widget w)\n"
   "It is called each time there is a repositioning. Perhaps that is overkill,\n"
   "but no need to sweat this for now.\n"
 },

 { 29, FEB , 2012 , ZSS , "suma" , MINOR , TYPE_NEW_OPT,
   "Added option to use html viewer for WAMI results.",
   "This is to allow users to click on a URL if one is available for a certain\n"
   "atlas result. For now the display is very crude and only accessible with\n"
   "-DWEBBY_WAMI=YES.\n"
 },

 { 29, FEB , 2012 , ZSS , "suma" , MINOR , TYPE_NEW_OPT,
   "Added 'ctrl+W' that allows saving results from interactive correlations.",
   "This allows for convenient saving of interactively created datasets\n"
   "such as those from single-subject, or group correlation maps in resting\n"
   "state.\n"
 },

 { 27, FEB , 2012 , ZSS , "afni-general" , MINOR , TYPE_BUG_FIX,
   "Fixed bug in read_niml_file() that incorrectly set a read_header_only flag",
   NULL
 },

 { 27, FEB , 2012 , ZSS , "suma-general" , MINOR , TYPE_NEW_OPT,
   "Allow on the fly definition of standard meshes with -i ld120, for example.",
   NULL
 },

 { 24, FEB , 2012 , ZSS , "@RetinoProc" , MINOR , TYPE_BUG_FIX,
   "Added number of volume registration regressors to -ort_adjust",
   NULL
 },

 { 24, FEB , 2012 , ZSS , "general" , MAJOR , TYPE_MODIFY,
   "I/O library for R now can use AFNI's c library.",
   "This new functionality allows the use a variety of formats\n"
   "both surface and volume-based for reading to and writing from R.\n"
   "3dMEMA can use this new functionality with option -cio\n"
 },

 { 24, FEB , 2012 , ZSS , "3dMEMA" , MINOR , TYPE_NEW_OPT,
   "3dMEMA can now handle surface-based data",
   "It makes use of the new I/O functions that use AFNI's\n"
   "C-library.\n"
 },

 { 24, FEB , 2012 , ZSS , "3dinfo" , MINOR , TYPE_NEW_OPT,
   "Options -val_diff and -sval_diff to compare values in 2 dsets.",
   NULL
 },

 { 22, FEB , 2012 , ZSS , "3dROIstats" , MINOR , TYPE_NEW_OPT,
   "Compute the mode of ROI voxels, see -mode and -nzmode",
   NULL
 },

 { 13, FEB , 2012 , ZSS , "3dSkullStrip" , MINOR , TYPE_BUG_FIX,
   "-orig_vol now forces datum type to be same as that of input.",
   "This fixed a problem with anatomicals ranging in the million(!) to\n"
   "come up the equivalent of a binary mask with -orig_vol.\n"
 },
 
 { 10, FEB , 2012 , ZSS , "suma" , MINOR , TYPE_BUG_FIX,
   "Improved snapshot making. Previous bugs were caused by buffer swap problems",
   "This fixed (I hope for good), problems of recording ('r'), continuous \n"
   "recording (OS X and Linux), and the oversampling.\n"
 },
 
 { 9, FEB , 2012 , ZSS , "3dSurf2Vol" , MINOR , TYPE_NEW_OPT,
   "Added -sdata, making 3dSurf2Vol compatible with multitude of surface dsets",
   NULL
 },
 
 { 7, FEB , 2012 , ZSS , "3dcopy" , MINOR , TYPE_BUG_FIX,
   "Fixed problem with 3dcopy HHH.nii TTT+orig not producing output",
   "Problem was caused by new change to EDIT_empty_copy() which\n"
   "assigned storage mode to that of input dset instead of STORAGE_BY_BRICK\n"
   "Fix involved setting storage mode per the prefix or view in \n"
   "EDIT_dset_items()\n"
 },
 
 { 7, FEB , 2012 , ZSS , "DriveSuma" , MINOR , TYPE_NEW_OPT,
   "Added -bkg_col and -autorecord options",
   NULL
 },
 
 { 7, FEB , 2012 , ZSS , "SurfMeasures" , MINOR , TYPE_NEW_OPT,
   "Added -out option to handle output in various formats.",
   "Also added -func ALL option.\n"
 },

 { 6, FEB , 2012 , ZSS , "suma" , MINOR , TYPE_NEW_OPT,
   "Added automatic sub-brick selection matching IxT in interface",
   "Also see corresponding env var: SUMA_IxT_LinkMode\n"
 },
 
 { 6, FEB , 2012 , ZSS , "3dANOVA3" , MINOR , TYPE_BUG_FIX,
   "Improved (I hope) sub-brick labels. Added FDR too.",
   "Same improvements to all 3dANOVA programs.\n"
 },
 
 { 6, FEB , 2012 , ZSS , "3dANOVA3" , MINOR , TYPE_BUG_FIX,
   "Fixed comaptibility with .niml.dset data",
   "This required more improvements (I hope) to THD_init_diskptr_names() and\n"
   "EDIT_empty_copy() for surface-based datasets.\n"
 },
 
 { 6, FEB , 2012 , ZSS , "3drefit" , MINOR , TYPE_BUG_FIX,
   "3drefit was failing on the view change when target dset is under a path.",
   "Prolem was with default catenation of path to DSET_BRIKNAME and \n"
   "DSET_HEADNAME. Those now contain the path automatically.\n"
 },
 
 { 26, JAN , 2012 , ZSS , "3dinfill" , MINOR , TYPE_NEW_PROG,
   "Fills holes in a volume based on neighboring values",
   "This program is a wrapper to function SUMA_VolumeInFill.\n"
   "Written in response to a query by Mike Beauchamp.\n"
 },
 
 { 24, JAN , 2012 , ZSS , "SurfExtrema" , MINOR , TYPE_NEW_PROG,
   "Finds nodes that are local extrema on the surface.",
   "Program in response to request by Daniel Margulies\n"
 },
 
 { 23, JAN , 2012 , ZSS , "@SUMA_Make_Spec_FS" , MINOR , TYPE_NEW_OPT,
   "Added -ld and -no_ld options",
   "@SUMA_Make_Spec_FS by default now runs MapIcosahedron at two ld values.\n"
 },
 
 { 23, JAN , 2012 , ZSS , "@SUMA_AlignToExperiment" , MINOR , TYPE_NEW_OPT,
   "Added -atlas_followers",
   "Automatically bring along atlas datasets under -surf_anat's directory.\n"
 },
 
 { 23, JAN , 2012 , ZSS , "suma-general" , MINOR , TYPE_NEW_OPT,
   "Allowed addition of 'R' or 'L' when jumping to a node.",
   "This make DriveSuma work well with node indices that are specified\n"
   "for two hemispheres as is done in the batch mode of 3dGroupInCorr.\n"
   "This applies to both suma and DriveSuma",
 },
 
 { 23, JAN , 2012 , ZSS , "suma" , MICRO , TYPE_BUG_FIX,
   "Improved logic for assigning ROIs when parent is not present",
   "ROI parent assignment now takes into account last visited surface"
   "in any of the open viewer, and hemisphere side when appropriate.",
 },
 
 { 23, JAN , 2012 , ZSS , "suma" , MICRO , TYPE_BUG_FIX,
   "Fixed bug in display of FRAME bound textures.",
   "Looks like it had to do with depth testing.",
 },
 
 { 20, JAN , 2012 , ZSS , "afni" , MICRO , TYPE_MODIFY,
   "Trim dset names to keep them readable in plugin dataset selector buttons",
   "See function TrimString() and how it is used in PLUG_finalize_dataset_CB()",
 },
 
 { 20, JAN , 2012 , ZSS , "afni-general" , MICRO , TYPE_NEW_OPT,
   "Added -h_web, and -Vname= to all C command-line programs.",
   "See also apsearch -web_prog_help option",
 },
 
 { 19, JAN , 2012 , ZSS , "DriveSuma" , MINOR , TYPE_NEW_OPT,
   "Allow definition of DO on the fly with -fixed_do, -mobile_do.",
   "See DriveAfni -help and suma -help_nido",
 },
    
 { 18, JAN , 2012 , ZSS , "suma-general" , MICRO , TYPE_NEW_ENV,
   "SUMA_Position_Original for controlling initial window position.",
   "Use suma -update_env ; to update env file, then look for\n"
   " env SUMA_Position_Original for help and default value.",
 },
    
 { 18, JAN , 2012 , ZSS , "niccc" , MINOR , TYPE_NEW_OPT,
   "-s option to test string NI elements",
   NULL,
 },
    
 { 18, JAN , 2012 , ZSS , "Surf2VolCoord" , MINOR , TYPE_NEW_OPT,
   "-closest_* options to create find nodes that fall closest to XYZ locations",
   NULL,
 },
    
 { 17, JAN , 2012 , ZSS , "3dGroupInCorr" , MINOR , TYPE_NEW_OPT,
   "Allowed BATCH mode to work on surfaces.",
   NULL,
 },
    
 { 13, JAN , 2012 , ZSS , "ParseName" , MINOR , TYPE_NEW_OPT,
   "Options -pre and -app help in creating filenames regarding of type",
   NULL,
 },
    
 { 11, JAN , 2012 , ZSS , "3dROIstats" , MICRO , TYPE_MODIFY,
   "Use ROI labels in output if label tables or atlas point lists are present",
   NULL,
 },
    
 { 11, JAN , 2012 , ZSS , "3drefit" , MICRO , TYPE_NEW_OPT,
   "Added -copytables to copy label tables and atlas point lists",
   NULL,
 },
    
 { 11, JAN , 2012 , ZSS , "afni-general" , MICRO , TYPE_MODIFY,
   "Added copying of label table and atlas point lists to aux copying function",
   "See functions THD_copy_labeltable_atr and THD_copy_datablock_auxdata\n"
   "for details." 
 },
    
 { 10, JAN , 2012 , ZSS , "3dTagalign" , MICRO , TYPE_NEW_OPT,
   "Added interpolation options.",
   NULL,
 },
    
 { 9, JAN , 2012 , ZSS , "DriveSuma" , MICRO , TYPE_NEW_OPT,
   "Added -setSUMAenv,-N_fore_smooth, and -N_final_smooth",
   NULL,
 },
    
 { 9, JAN , 2012 , ZSS , "suma-general" , MICRO , TYPE_NEW_OPT,
   "Added -setenv to all SUMA programs",
   NULL,
 },
    
 { 6, JAN , 2012 , ZSS , "ROI2dataset" , MICRO , TYPE_NEW_OPT,
   "Added -keep_separate to allow for another way to split ROIs",
   NULL,
 },
    
 { 4, JAN , 2012 , ZSS , "ROI2dataset" , MICRO , TYPE_NEW_OPT,
   "Added -nodelist_with_ROIval to facilitate splitting ROIs",
   NULL,
 },
    
 { 4, JAN , 2012 , ZSS , "@Atlasize" , MICRO , TYPE_NEW_OPT,
   "Allow for delimiters in keys file and handle names with spaces",
   "Similar additions are made to @MakeLabelTable",
 },
    
 { 4, JAN , 2012 , ZSS , "afni" , MINOR , TYPE_NEW_OPT,
   "Automatically add an Atlas to whereami list if encountered in the session",
   NULL,
 },
    
 { 30, DEC , 2011 , ZSS , "afni" , MINOR , TYPE_NEW_OPT,
   "Simplified how afni handles custom group or single-subject atlases",
   "By default, SessionAtlases and AFNI_SUPP_ATLAS_DIR/CustomAtlases.niml\n"
   "are added to the atlas list for whereami queries.",
 },
    
 { 30, DEC , 2011 , ZSS , "@AfniEnv" , MINOR , TYPE_NEW_PROG,
   "A convenience script to automatically edit .afnirc",
   NULL,
 },
    
 { 30, DEC , 2011 , ZSS , "@Atlasize" , MINOR , TYPE_NEW_PROG,
   "A convenience script to turn a dataset to an atlas",
   "The script makes it easy to create a group atlas or a single-subject\n"
   "atlas and place them automatically in a location that AFNI\n"
   "recognizes making them available to the user without bothering with\n"
   "too many environment variable settings.\n",
 },
    
 { 30, DEC , 2011 , ZSS , "@MakeLabelTable" , MINOR , TYPE_NEW_OPT,
   "Improved @MakeLabelTable's creation of ATLAS niml files",
   NULL,
 },
    
 { 30, DEC , 2011 , ZSS , "3dinfo" , MINOR , TYPE_NEW_OPT,
   "Numerous new options to 3dinfo, including checks for dset presence on disk",
   NULL,
 },
    
 { 30, DEC , 2011 , ZSS , "afni-general" , MINOR , TYPE_BUG_FIX,
   "NIFTI datasets used to loose their path information upon loading.",
   "This is problematic for a few reasons. For instance, afni ~/tmp/joe.niml\n"
   "will display the dataset as ./joe.niml. Some internal functions for\n"
   "conveniently dealing with atlases fail when the path is messed up in this\n"
   "way. Also, this problem occurred for NIFTI but NOT AFNI native formats \n"
   "wich resulted in different behaviour for programs like 3drefit.\n"
   "For example: 3drefit -space MNI ~/tmp/joe.nii is not comparable to\n"
   "3drefit -space MNI ~/tmp/joe+tlrc. (Actually, that behaviour was patched\n"
   "on Dec 27 but explicitly setting the path at output. But the fix needed\n"
   "to happen for other programs so that is now done at the io level.)\n",
 },
    
 { 27, DEC , 2011 , ZSS , "3drefit" , MINOR , TYPE_BUG_FIX,
   "3drefit failed when refitting a NIFTI dset in another directory.",
   "If you did something like 3drefit -space MNI joe/jim/dset.nii\n"
   "you ended up with a new dataset ./dset.nii as opposed to modifying\n"
   "the one under joe/jim/ directory",
 },
    
 { 20, DEC , 2011 , ZSS , "3dclust" , MINOR , TYPE_BUG_FIX,
   "-prefix failed when input dset was mastered.",
   "Better use macro PREP_LOADED_DSET_4_REWRITE whenever modifying\n"
   "a dataset loaded from disk for the purporse of rewriting it.",
 },
    
 { 20, DEC , 2011 , ZSS , "3dUpsample" , MINOR , TYPE_NEW_OPT,
   "Allow for upsampling to go to 11 (actually 320) and control output datum",
   NULL,
 },
 
 { 20, DEC , 2011 , ZSS , "suma" , MINOR , TYPE_NEW_OPT,
   "SUMA now allows for direct recording to disk.",
   "See SUMA's ctrl+h for key ctrl+R for details.\n"
   "Related environment variable is SUMA_AutoRecordPrefix.",
 },
 
 { 20, DEC , 2011 , ZSS , "suma" , MINOR , TYPE_NEW_OPT,
   "SUMA now displays area labels in the viewer, F9 key toggles it",
   "See SUMA's ctrl+h output in the section for F9\n"
   "Related environment variable is SUMA_ShowLabelsAtCrossHair.",
 },
 
 { 13, DEC , 2011 , ZSS , "3dinfo" , MINOR , TYPE_NEW_OPT,
   "-same_* options return information about dset pairs",
   NULL,
 },
 
 { 13, DEC , 2011 , ZSS , "@SUMA_AlignToExperiment" , MINOR , TYPE_NEW_OPT,
   "-init_xform to allow for an initial transformation of -surf_anat",
   NULL,
 },
 
 { 13, DEC , 2011 , ZSS , "plugout_drive" , MICRO , TYPE_BUG_FIX,
   "AFNI_drive_switch_* functions discriminated against short prefixes.",
   NULL,
 },
 
 { 12, DEC , 2011 , ZSS , "apsearch" , MINOR , TYPE_NEW_OPT,
   "Small improvements and addition of logging option of search results",
   "See -apsearch_log_file option for details. Logging is only enabled\n"
   "if AFNI_LOG_BEST_PROG_OPTION is set to YES.",
 },
 
 { 12, DEC , 2011 , ZSS , "@global_parse" , MINOR , TYPE_NEW_PROG,
   "Script to simplify support of global options such as h_find from scripts",
   NULL ,
 },
 
 { 12, DEC , 2011 , ZSS , "afni" , MINOR , TYPE_NEW_OPT,
   "Slice displays can now show atlas labels in addition to slice location ",
   "The displays are turned on by right-clicking on the slice viewer's color\n"
   "bar and then setting the 'Label' to something other than 'OFF'\n"
   "See isqCR_getlabel, ISQ_getlabel, and AFNI_get_dset_val_label in code\n",
 },
 
 { 9, DEC , 2011 , ZSS , "suma" , MICRO , TYPE_NEW_ENV,
   "SUMA_DoNotSendStates restricts certain surfaces from getting sent to suma.",
   NULL,
 },
 
 { 7, DEC , 2011 , ZSS , "@SUMA_Make_Spec_FS" , MINOR , TYPE_NEW_OPT,
   "Added atlas creation of aseg, aparc+aseg, in addition to aparc.a200*",
   "See whereami's example 3 for how to extract ROIs from FreeSurfer atlases.",
 },
 
 { 7, DEC , 2011 , ZSS , "@FS_roi_label" , MICRO , TYPE_BUG_FIX,
   "Fixed bug caused by not cleaning tmp files under /tmp/ with -rankmap",
   "This bug should not have affected anyone. It showed up as I\n"
   "was making @SUMA_Make_Spec_FS process more than one type of\n"
   "parcellation volume.\n",
 },
 
 { 6, DEC , 2011 , ZSS , "apsearch" , MICRO , TYPE_NEW_OPT,
   "-stdin for input",
   "More tweaking of search distances and a battery of tests\n"
   "to help decide if tweak is in the right direction.\n",
 },
 
 { 5, DEC , 2011 , ZSS , "afni-general" , MICRO , TYPE_NEW_OPT,
   "Added -h_view to all C command-line programs.",
   "See also apsearch -view_prog_help option",
 },
 
 { 5, DEC , 2011 , ZSS , "afni-general" , MICRO , TYPE_NEW_ENV,
   "AFNI_GUI_EDITOR controls user's preferred text editor.",
   "See apsearch -afni_text_editor.",
 },
 
 { 2, DEC , 2011 , ZSS , "suma" , MICRO , TYPE_NEW_OPT,
   "Added 'Save' button to suma text info windows",
   NULL,
 },
 
 { 2, DEC , 2011 , ZSS , "suma" , MICRO , TYPE_NEW_ENV,
   "SUMA_NodeCoordsUnits allows the specification of node coord. units",
   NULL,
 },
 
 { 2, DEC , 2011 , ZSS , "afni-general" , MICRO , TYPE_BUG_FIX,
   "Wrote SOLARIS_strcasestr to replace missing strcasestr on SOLARIS",
   NULL,
 },
 
 { 2, DEC , 2011 , ZSS , "ConvertSurface" , MINOR , TYPE_NEW_OPT,
   "-XYZscale is added to scale the coordinates of a surface",
   "Surface that are not in mm coordinates can look pretty bad in SUMA.\n"
   "This scaling option can be used to easily change coordinates from cm to mm\n"
   "for example.",
 },
 
 { 30, NOV , 2011 , ZSS , "ConvertDset" , MICRO , TYPE_NEW_OPT,
   "-pad_to_node 0 now padds output dset to max node in input dset",
   NULL,
 },
 
 { 29, NOV , 2011 , ZSS , "@clean_help_dir" , MICRO , TYPE_NEW_PROG,
   "Removes redundant help files from the afni help directory.",
   NULL,
 },
 
 { 29, NOV , 2011 , ZSS , "@DeblankFileNames" , MICRO , TYPE_NEW_PROG,
   "Replaces spaces in filenames with_something_less_annoying.",
   NULL,
 },
 
 { 28, NOV , 2011 , ZSS , "afni-general" , MICRO , TYPE_BUG_FIX,
   "Fixed function THD_deplus_prefix to improve prefix cleanup",
   "Previous version cleaned +orig but not +orig.HEAD or +orig. for example.\n"
   "Current one removes +orig +orig. +orig.HEAD +orig.BRIK +orig.BRIK.gz and\n"
   "corresponding +acpc and +tlrc brethren.\n"
 },
 
 { 25, NOV , 2011 , ZSS , "Dimon" , MICRO , TYPE_NEW_OPT,
   "Added -gert_quit_on_err",
   "Passes -quit_on_err option to to3d."
 },
 
 { 25, NOV , 2011 , ZSS , "to3d" , MICRO , TYPE_NEW_OPT,
   "Added -quit_on_err",
   "Keeps to3d from going in interactive mode in case of error."
 },
 
 { 22, NOV , 2011 , ZSS , "afni" , MICRO , TYPE_NEW_OPT,
   "Added -h_find as a global option",
   "afni -help now outputs information about 'Global options'"
 },
 
 { 21, NOV , 2011 , ZSS , "afni" , MICRO , TYPE_NEW_OPT,
   "Added -all_opts as a global option",
   "The option is the equivalent of running apsearch -all_opts \n"
   "for a certain program."
 },
 
 { 21, NOV , 2011 , ZSS , "apsearch" , MICRO , TYPE_NEW_OPT,
   "Added -all_opts to show all options for a program",
   NULL
 },
 

 { 17, NOV , 2011 , ZSS , "apsearch" , MINOR , TYPE_NEW_PROG,
   "This is a program to test approximate string matching functions",
   "The program is best described by its help output.\n"
   "It is meant to quickly test the behavior of the approx* functions\n"
   "in thd_ttatlas_query.c where approximate string matching will\n"
   "be useful is easily specifying an atlas areas by name.\n"
   "Another useful offshoot is the function: suggest_best_prog_option\n"
   "which can easily be called right after a command-line parsing error\n"
   "message is displayed. For example, in apsearch I have the following:\n"
   "\n"
   "   { /* bad news in tennis shoes */\n"
"      fprintf(stderr,\"** Error %s: bad option %s\\n\", argv[0], argv[iarg]);\n"
   "      suggest_best_prog_option(argv[0], argv[iarg]);\n"
   "      return 1;\n"
   "   }\n"
   "So all is needed to retrofit an old program is the suggest function.\n"
   "That function will do nothing if environment variable AFNI_NO_OPTION_HINT\n"
   "is set.\n"
   "To use suggest_best_prog_option from a script, use:\n"
   "     apsearch -popt `basnemane $0` -word ARG\n"
   "\n"
 },

 { 17, NOV , 2011 , ZSS , "afni" , MICRO , TYPE_NEW_ENV,
   "Added threshold locking capability under the 'Lock' menu",
   NULL
 },
 
 { 17, NOV , 2011 , ZSS , "afni" , MINOR , TYPE_NEW_OPT,
   "If AFNI_NO_OPTION_HINT, don't bother with helpful suggestions",
   NULL
 },
 
 { 15, NOV , 2011 , ZSS , "afni_general" , MINOR , TYPE_BUG_FIX,
   "Fixed bug introduced by cleaning up header_name and brick_name",
   NULL
 },
 
 { 15, NOV , 2011 , ZSS , "afni_general" , MINOR , TYPE_BUG_FIX,
   "More changes regarding obliquity warnings.",
   "   AFNI:\n"
   "   Popups only appear when user is viewing dsets of different obliquity.\n"
   "     See dset_obliquity_angle_diff()\n"
   "      and THD_report_obliquity()\n"
   "   \n"
   "   The warnings only appear ONCE per pair of misfits, per afni process. \n"
   "      See AFNI_check_obliquity() for implementation details.\n"
   "   \n"
   "   Oblique notices no longer appear on the command line each time you \n"
   "   read a session or switch dsets. To tell you that you are viewing an \n"
   "   oblique dset and that the coords are not quite what they should be,\n"
   "   I added a '*' next to the 'order:' descriptor in the top left corner. \n"
   "\n"
   "Command Line programs:\n"
   "   In most programs that should not care about obliquity, I add\n"
   "   early under main():   \n"
   "      set_obliquity_report(0); /* silence obliquity */ \n"
   "   \n"
   "  Also, 3dcalc, 3dTcat, 3dbucket, and function THD_open_tcat() \n"
   "   Only complain when obliquity between dset pair differs. \n"
   "   The check is done right after checking for grid matching with\n"
   "   EQUIV_DATAXES\n"
   "   \n"
  } ,


 { 10, NOV , 2011 , ZSS , "afni" , MINOR , TYPE_BUG_FIX,
   "Turned off obliquity popup warning when Ulay and Olay angles are similar",
   "AFNI issues warning when obliquity differs for a pair of viewed sets.\n"
   "It does not repeat warnings for a certain pair. It only output one set\n"
   "of warnings per dataset when 'switch ulay' is set, although I'd love to\n"
   "get rid of that habit as soon as possible.\n"   
  } ,

 { 10, NOV , 2011 , ZSS , "3dinfo" , MICRO , TYPE_BUG_FIX,
   "Many new options for selective information.",
   NULL   
  } ,

 { 2, NOV , 2011 , ZSS , "3dfim+" , MICRO , TYPE_BUG_FIX,
   "Turned off scaling for Best Index in output dset",
   NULL   
  } ,

 { 26, OCT , 2011 , ZSS , "suma" , MICRO , TYPE_NEW_OPT,
   "Made SUMA hide overlay from SUMA whenever 'See Overlay' is off in AFNI",
   NULL   
  } ,

 { 26, OCT , 2011 , ZSS , "afni" , MICRO , TYPE_BUG_FIX,
   "Fixed bug where AFNI sent the max voxel to suma despite threshold",
   "In BYTE and SHORT datasets the maximum voxel always got sent to\n"
   "suma, even if the threshold was higher than the maximum value in\n"
   "the dataset.\n"   
  } ,

 { 18, OCT , 2011 , ZSS , "afni" , MICRO , TYPE_NEW_OPT,
   "Added -available_npb* options to suggest available port blocks",
   NULL  
  } ,

 { 14, OCT , 2011 , ZSS , "afni-general" , MINOR , TYPE_GENERAL,
   "Improvements of atlas handling in whereami and afni",
   "1- In addition to loading atlas specs from environment variable\n"
   "AFNI_SUPP_ATLAS, AFNI will automatically search for a file\n"
   "named  SessionAtlases.niml which is created with @MakeLabelTable.\n"
   "2- AFNI will check if a viewed dataset is an atlas and will\n"
   "automatically add it to the atlas list. It will also show the label at \n"
   "the cross-hair location.\n"
   "New functions of interest:\n"
   "  get_Atlas_ByDsetID, is_Dset_Atlasy, \n"
   "  label_table_to_atlas_point_list, is_identity_xform_chain\n"
  } ,

 { 13, OCT , 2011 , ZSS , "whereami" , MINOR , TYPE_BUG_FIX,
   "-omask failed if atlas was stored in float type.",
   NULL
  } ,

 { 13, OCT , 2011 , ZSS , "3drefit" , MINOR , TYPE_BUG_FIX,
   "Added support for -atrstring operation on NIFTI datasets ",
   "This required making changes to THD_init_diskptr_names functions\n"
   "and a few more of its colleagues."
  } ,

 { 11, OCT , 2011 , ZSS , "@auto_tlrc" , MINOR , TYPE_NEW_OPT,
   "Added support for NIFTI datasets",
   NULL
  } ,

 { 11, OCT , 2011 , ZSS , "3dinfo" , MINOR , TYPE_NEW_OPT,
   "Added new options for extracting field values in scripts",
   NULL
  } ,

 { 4, OCT , 2011 , ZSS , "CreateIcosahedron" , MICRO , TYPE_NEW_OPT,
   "Added -min_nodes option",
   NULL
  } ,

 { 4, OCT , 2011 , ZSS , "GroupAna" , MINOR , TYPE_NEW_OPT,
   "Added support for writing NIML files as output",
   "This also required writing a few new matlab functions such as\n"
   " BrikInfo_2_niml_writesimple."
 } ,

 { 1, AUG , 2011 , ZSS , "Draw Dataset" , MINOR , TYPE_MODIFY,
   "Changed the gap selection to arrows to allow for much larger gaps",
   NULL
  } ,

 { 1, AUG , 2011 , ZSS , "whereami" , MINOR , TYPE_BUG_FIX,
   "Whereami's symbolic notation failed with float valued atlases",
   "This is now fixed. Code also checks for scale factors.\n"
   "There was another bug waiting to happen with:\n"
   "!is_Atlas_Named(atlas, \"CA_N27_PM\"), which is now:\n"
   "!is_probabilistic_atlas(atlas)\n"
  } ,

 { 22, JUL , 2011 , ZSS , "afni" , MICRO , TYPE_NEW_ENV,
   "Added AFNI_TIME_LOCK to turn on AFNI's Time Lock by default",
   NULL
  } ,

 { 20, JUL , 2011 , ZSS , "1dcat" , MICRO , TYPE_NEW_OPT,
   "added -sel option to 1dcat",
   "with -sel one can apply the same selection of columns and rows\n"
   "to all files on the command line, allowing the use of wildcards\n"
   "when specifying input 1D files.\n"
  } ,

 { 24, JUN , 2011 , ZSS , "afni-general" , MICRO , TYPE_NEW_OPT,
   "added global option -pif which is used to flag certain commands.",
   "-pif PROCESS_ID_FLAG is used to flag a particular AFNI command\n"
   "so that you can identify from the shell that command's process id\n"
   "by grepping for PROCESS_ID_FLAG on the output of 'ps -a'.\n"
  } ,

 { 20, JUN , 2011 , ZSS , "@ROI_Corr_Mat" , MICRO , TYPE_BUG_FIX,
   "added support for +tlrc input, more debugging messages, -echo option.",
   NULL
  } ,

 { 20, JUN , 2011 , ZSS , "3drename" , MICRO , TYPE_NEW_OPT,
   "added support for -overwrite",
   NULL
  } ,

 { 16, JUN , 2011 , ZSS , "3dSurfMask" , MAJOR , TYPE_NEW_OPT,
   "Major improvements for mask creation and voxel distance computations",
   "Option -fill_method SLOW produces more accurate masks for voxels\n"
   "inside the closed surface. In addition, the program outputs a dataset\n"
   "with the shortest distance of each voxel to the surface.\n"
   "See examples under 3dSurfMask -help."
  } ,
 
 { 16, JUN , 2011 , ZSS , "SurfPatch" , MINOR , TYPE_NEW_OPT,
   "-flip_orientation allows for reversing triangle orientation.",
   "See new examples under SurfPatch -help"
 } ,

 { 16, JUN , 2011 , ZSS , "SurfPatch" , MINOR , TYPE_BUG_FIX,
   "No longer complains about not finding seed in cases where it should.",
   NULL
  } ,

 { 8, JUN , 2011 , ZSS , "@FSlabel2dset" , MINOR , TYPE_NEW_PROG,
   "Script to change FreeSurfer ascii label file to SUMA dset",
   NULL
  } ,

 { 8, JUN , 2011 , ZSS , "afni-general" , MAJOR , TYPE_NEW_OPT,
   "Added -npb and modified title bar to reflect bloc of ports",
   "You can have multiple instances of programs talking to each other.\n"
   "See afni's help for details on the -npb option."
  } ,

 { 7, JUN , 2011 , ZSS , "afni" , MINOR , TYPE_NEW_OPT,
   "Added 'w' key for writing out colormap to disk.",
   "See suma's help for the colormap.\n"
   "(press ctrl+h with cursor over the colormap in the surface controller)"  
  } ,

 { 7, JUN , 2011 , ZSS , "afni-general" , MAJOR , TYPE_NEW_OPT,
   "Allow multiple instances of communicating programs per machine.",
   "This was done by generalizing option -np PORT_OFFSET which \n"
   "allows users to use a different set of ports for different AFNI/SUMA/etc.\n"
   "sessions."
   "Port numbers should no longer be hard coded anywhere. New dedicated\n"
   "ports should be added to then new init_ports_list() function."  
  } ,

 { 7, JUN , 2011 , ZSS , "afni" , MICRO , TYPE_NEW_OPT,
   "-list_ports, -port_number* give port assignment information",
   "See afni -help for details."  
  } ,

 { 7, JUN , 2011 , ZSS , "suma" , MICRO , TYPE_NEW_OPT,
   "-np, -list_ports, -port_number* control and report port assignments",
   "See suma -help for details."  
  } ,

 { 19, MAY , 2011 , ZSS , "@RetinoProc" , MINOR , TYPE_NEW_OPT,
   "Added support for multiple reference time series.",
   "Search for -var* options in @RetinoProc -help.\n" 
  } ,

 { 19, MAY , 2011 , ZSS , "3dRetinoPhase" , MINOR , TYPE_NEW_OPT,
   "Added option to use best of multiple reference time series.",
   "Search for -multi_ref_ts in 3dRetinoPhase -help\n" 
 } ,

 { 18, MAY , 2011 , ZSS , "suma" , MICRO , TYPE_BUG_FIX,
   "Node value now updates when switching dsets while viewing surf patches.\n",
   NULL,
  } ,

 { 18, MAY , 2011 , ZSS , "@RetinoProc" , MAJOR , TYPE_NEW_OPT,
   "Added options for mapping to specific layers, and the use of delay",
   "The options -wm, -pial, etc. allow for tight control of mapping\n"
   "onto specific layers in the cortex.\n"
   "Option -delay improves latency estimation compare to using the phase\n"
   "of the fundamental frequency.\n" 
  } ,

  { 18, MAY , 2011 , ZSS , "SurfRetinoMap" , MINOR , TYPE_MODIFY,
   "Added a correlation coefficient with VFR output. \n",
   NULL,
  } ,

 { 18, MAY , 2011 , ZSS , "3dRetinoPhase" , MAJOR , TYPE_NEW_OPT,
   "Added options to 3dRetinoPhase to estimate latency as in 3ddelay \n",
   "The two options for computing delays, instead of phase, are \n"
   "-phase_estimate DELAY, and -ref_ts REF_TS . See program's -help for \n"
   "details.\n"
   "Also added -ort_adjust which is needed to account for lost DOF in \n"
   "external detrending when estimating the significance of correlation \n"
   "coefficients with delay estimation.\n"
   "The program now output a correlation coefficient with the visual field\n"
   "angles datasets. \n"
   "And speaking of correlation coefficients, the significance levels should\n"
   "be taken with a grain of salt, especially in derived datasets such as \n"
   "field angle, and VFR in SurfRetinoMap.\n" 
  } ,

 { 18, MAY , 2011 , ZSS , "3ddelay" , MINOR , TYPE_NEW_OPT,
   "Added options to 3ddelay to improve its utility for retinotopy analysis",
   "The new options are -phzreverse and -phzscale. Though useful, you are\n"
   "better off using 3dRetinoPhase -phase_estimate DELAY option. It is much\n"
   "more convenient for retinotopy analysis and fits better in @RetinoProc\n"
  } ,

 { 25, APR , 2011 , ZSS , "afni-general" , MAJOR , TYPE_MODIFY,
   "Major reorganization of 'whereami' functionality.",
   "The code changes affect anything related to atlas datasets and whereami\n"
   "functionality. The changes were made take advantage of Daniel Glen's new\n"
   "API to handle atlas, space, and template definitions.\n"
   "There is now very little reliance on hard coded atlas information in the\n"
   "source code. Whatever is left is needed to ensure backward compatibility.\n"
  } ,

 { 8, APR , 2011 , ZSS , "3dLocalstat" , MINOR , TYPE_NEW_OPT,
   "Added options -reduce* to compute results on reduced grid",
   "This would help in speeding up the computing of stats over large regions\n"
   "without paying too high a premium on processor time. \n"
   "Changes were made in 3dLocalstat.c and mri_nstats.c. \n"
   "Micro modification in r_new_resam.c's r_new_resam_dset."
  } ,

 { 29, MAR , 2011 , ZSS , "suma" , MINOR , TYPE_BUG_FIX,
   "Fixed bug in default view of flat surfaces.",
   "The problem manifested itself when large rotations were present\n"
   "in the transform of the sv volume, resulting in flat meshes\n"
   "being incorrectly labeled as spherical."
 } ,

 { 29, MAR , 2011 , ZSS , "ConvexHull" , MINOR , TYPE_BUG_FIX,
   "Fixed error with proj_xy option.",
   NULL
 } ,

 { 29, MAR , 2011 , ZSS , "afni-general" , MINOR , TYPE_MODIFY,
   "Basic support for HTTP/1.1",
   "See functions read_URL_http11, and page_* in thd_http.c."
 } ,

 { 22, MAR , 2011 , ZSS , "afni-general" , MICRO , TYPE_NEW_ENV,
   "AFNI_NIFTI_TYPE_WARN controls frequency of NIFTI type conversion warnings",
   "Default is to warn once per session. \n"
   "Search for AFNI_NIFTI_TYPE_WARN in README.environment for details."
 } ,

 { 22, MAR , 2011 , ZSS , "3dhistog" , MINOR , TYPE_NEW_OPT,
   "-roi_mask allows creation of separate histogram for each ROI in mask.",
   NULL
 } ,

 { 27, JAN , 2011 , ZSS , "afni" , MICRO , TYPE_BUG_FIX,
   "Fixed problem in THD_add_bricks when dset in AFNI is not malloc-ed.",
   NULL
 } ,

 { 26, JAN , 2011 , ZSS , "afni" , MICRO , TYPE_MODIFY,
   "Made THD_add_bricks add labels to new bricks ",
   NULL
 } ,

 { 25, JAN , 2011 , ZSS , "suma" , MICRO , TYPE_MODIFY,
   "Improved logic for assigning ROI parent.",
   NULL
 } ,

 { 4, JAN , 2011 , ZSS , "SurfFWHM" , MINOR , TYPE_BUG_FIX,
   "Fixed SurfFWHM which had the same masking problem as SurfSmooth.",
   NULL
 } ,

 { 4, JAN , 2011 , ZSS , "SurfSmooth" , MINOR , TYPE_BUG_FIX,
   "Fixed SurfSmooth to work with the combination HEAT07+Sparse Dsets+cmask ",
   "The problem was caused by a bad masking operation at the detrending\n"
   "function when sparse datasets with cmask option are used. The detrending\n"
   "is used to estimate the FWHM in the blurmaster. As a result, SurfSmooth \n"
   "would not converge under such circumstances.\n"
   "In addition there was an optimizer related bug in the macro SUMA_COL_FILL\n"
   "Thanks to Christopher Ackerman from JHMI for reporting the bug."
 } ,

 { 3, JAN , 2011 , ZSS , "afni-general" , MICRO , TYPE_BUG_FIX,
   "Modified decode_*linebuf to better treat the 'i' character in 1D files",
   NULL
 } ,

 { 3, JAN , 2011 , ZSS , "3dUniformize" , MINOR , TYPE_NEW_OPT,
   "Changed 3dUniformize to accept byte, short, or float input.",
   "These changes also avoid data clipping that was necessary\n"
   "when output data was handled as shorts.\n"
   "The output format is similar to that of the input.\n"
   "-auto_clip is now the default.\n"
 } ,

 { 21, DEC , 2010 , ZSS , "@help.AFNI" , MINOR , TYPE_NEW_PROG,
   "A simple script to look at AFNI's all help page",
   NULL
 } ,

 { 10, DEC , 2010 , ZSS , "3BrickStat" , MINOR , TYPE_NEW_OPT,
   "Added -mrange and -mvalue options to 3dBrickStat",
   NULL
 } ,

 { 9, DEC , 2010 , ZSS , "3dhistog" , MINOR , TYPE_NEW_OPT,
   "Made 3dhistog output NIML 1D format with -prefix option",
   "With -prefix's output users can get properly labeled \n"
   "output with a simple command like 1dRplot -input hist.1D"
 } ,

 { 2, DEC , 2010 , ZSS , "afni" , MICRO , TYPE_MODIFY,
   "Made afni startup with two windows when no layout is present",
   NULL
 } ,

 { 24, NOV , 2010 , ZSS , "3dTcat" , MICRO , TYPE_BUG_FIX,
   "3dTcat was forcing sub-brick selection at times",
   "Say each of dset1 and dset2 has 10 subbricks. \n"
   "A command like this:\n"
   " 3dTcat dset1+orig[0..8] dset2+orig \n"
   "used to produce a dataset that is 18 sub-bricks, not\n"
   "19. That is because the [0..8] selection was applied\n"
   "to dset1 and all subsequent dsets on the command that\n"
   "did not explicitly have selectors in their name."
 } ,

 { 22, NOV , 2010 , ZSS , "3dRowFillin" , MINOR , TYPE_NEW_OPT,
   "Added XYZ.OR, and XYZ.AND to 3dRowFillin",
   NULL
 } ,

 { 16, NOV , 2010 , ZSS , "ExamineXmat" , MINOR , TYPE_NEW_OPT,
   "A major rewrite of ExamineXmat",
   "see ExamineXmat -help for details"
 } ,

 { 5, NOV , 2010 , ZSS , "niccc" , MINOR , TYPE_NEW_OPT,
   "Added -attribute, -match, and -f options to niccc",
   "See niccc -help for details."
 } ,

 { 5, NOV , 2010 , ZSS , "3dkmeans" , MAJOR , TYPE_NEW_PROG,
   "3dkmeans is a program for performing kmeans clustering",
   "Program was written by A. Vovk and Z. Saad, based on \n"
   "  The C clustering library.\n"
   "Copyright (C) 2002 Michiel Jan Laurens de Hoon.\n"
   "See suma -sources for copyright details\n"
   "\n"
   "See 3dkmeans -help for usage details.\n"
 } ,

 { 1, NOV , 2010 , ZSS , "3dBrickStat" , MINOR , TYPE_BUG_FIX,
   "3dBrickStat's precentile option did not work with byte datasets",
   NULL
 } ,

 { 29, OCT , 2010 , ZSS , "@ElectroGrid" , MINOR , TYPE_NEW_PROG,
   "A script for facilitating ECOG grid creation",
   NULL
 } ,

 { 28, OCT , 2010 , ZSS , "@RegroupLabels" , MINOR , TYPE_NEW_PROG,
   "A script for regrouping label datasets",
   NULL
 } ,

 { 28, OCT , 2010 , ZSS , "3dcopy" , MICRO , TYPE_NEW_OPT,
   "Support for -overwrite",
   NULL
 } ,

 { 22, OCT , 2010 , ZSS , "suma" , MINOR , TYPE_BUG_FIX,
   "Fixed rendering of spheres, which were affected by ambient light.",
   "Thanks to MSB for the complaint."
 } ,

 { 22, OCT , 2010 , ZSS , "DriveSuma" , MINOR , TYPE_NEW_OPT,
   "Added -RenderMode option to control how a surface is rendered",
   "This option is the same as Surface Controller-->RenderMode menu"
 } ,

 { 22, OCT , 2010 , ZSS , "ConvexHull" , MINOR , TYPE_NEW_OPT,
   "Added -q_opt option to allow for delaunay triangulation of 2D coordinates",
   "This new option makes it easy to create a surface from a set\n"
   "of ECOG electrodes.\n"
 } ,

 { 22, OCT , 2010 , ZSS , "suma" , MINOR , TYPE_NEW_OPT,
   "Added NodeMarker field to the spec file",
   "The NodeMarker is a NIDO object that gets replicated at all nodes.\n"
   "Replicated markers inherit the color of the nodes IF the NodeMarker\n"
   "has no color attribute.\n"
 } ,

 { 20, OCT , 2010 , ZSS , "niccc" , MICRO , TYPE_NEW_PROG,
   "Started making niccc as part of the default compilation",
   "The program, originally written by Bob, \n"
   "is useful for testing NIML functions"
 } ,

 { 20, OCT , 2010 , ZSS , "afni-general" , MINOR , TYPE_MODIFY,
   "Added NI_duplicate* functions to niml",
   NULL
 } ,

 { 19, OCT , 2010 , ZSS , "3dcopy" , MINOR , TYPE_MODIFY,
   "Allowed 3dcopy to take . or ./ as output options",
   "Other changes make the error message a little more\n"
   "informative\n"
 } ,

 { 27, SEP , 2010 , ZSS , "suma" , MINOR , TYPE_NEW_OPT,
   "Began code changes to allow for volume rendering",
   "Changes mostly involve SUMA_volume_render.c and were\n"
   "made with Joachim Bottger's help. Of note is the addition"
   "of the Volume Object structure SUMA_VolumeObject.\n"
 } ,

 { 18, SEP , 2010 , ZSS , "DriveSuma" , MICRO , TYPE_NEW_OPT,
   "Added -Dsp option to allow control of Dset display mode",
   NULL
 } ,

 { 7, SEP , 2010 , ZSS , "3dRetinoPhase" , MINOR , TYPE_BUG_FIX,
   "Fixed crash caused by bad function prototype.",
   "Crash only occurred on linux. Thanks to P. J. Kohler for \n"
   "tests and bug report.\n"
 } ,

 { 3, SEP , 2010 , ZSS , "@SUMA_Make_Spec_FS" , MINOR , TYPE_NEW_OPT,
   "Script now process v1.label files from FreeSurfer",
   "The output is two datasets per hemisphere, one for the ??.v1.prob.label\n"
   "and one for the ??.v1.predict.label.\n"
 } ,

 { 3, SEP , 2010 , ZSS , "suma" , MINOR , TYPE_BUG_FIX,
   "Fixed inappropriate read in SUMA_Draw_SO_Dset_Contours",
   "This bug had caused a crash on one machine, but had the potential\n"
   "for bad surprises in the future.\n"
 } ,

 { 26, AUG , 2010 , ZSS , "@auto_tlrc" , MAJOR , TYPE_BUG_FIX,
   "Fixed bug in @auto_tlrc in usage 2 mode AND with no suffix",
"There was a bug in @auto_tlrc  for the last 2 months: \n"
"   from June 30, 2010 until  August 26, 2010. \n"
"\n"
"It occurs only when using auto_tlrc in: \n"
"  usage 2 mode \n"
"        AND \n"
"  with -suffix omitted, or set to NONE.\n"
"\n"
"This bug does not affect your data if you had used adwarp -apar, \n"
"  or if you put your data in TLRC space via afni_proc.py, \n"
"  or align_epi_anat.py.\n"
"\n"
"The bug essentially results in a renaming of your follower dataset, \n"
" without the spatial transformation. So, if you were applying the \n"
" transform to epi+orig, it practically got renamed to epi+tlrc. \n"
" Despite the +tlrc in the name, epi+tlrc would still be in +orig \n"
" view and you no longer have epi+orig on disk. \n"
"   \n"
"Examples of affected commands would be:\n"
"	@auto_tlrc  -apar anat+tlrc 	               -input epi+orig \n"
"or\n"
"	@auto_tlrc  -apar anat+tlrc 	-suffix NONE   -input epi+orig  \n"
"\n"
"The script did produce Error messages but it failed to stop.\n"
"\n"
"If you think you ran the buggy command (a clear symptom would be \n"
"missing +orig datasets AND bad alignment in TLRC of course), you \n"
"must download the latest binaries and rerun @auto_tlrc after you \n"
"have recreated the +orig files. You can also just get @auto_tlrc \n"
"at the link below.\n"
"\n"
"Sorry for this, I had tested complicated option combinations on \n"
"the last release, but all tests had used the -suffix option.\n"
"(<http://afni.nimh.nih.gov/afni/community/board/read.php?f=1&i=34139&t=34139>)"
"\n"
"\n"
"Binaries postdating this message will contain the new script. \n"
"You can also get a corrected version of @auto_tlrc at this link:\n"
"<http://afni.nimh.nih.gov/sscc/staff/ziad/Misc_Download/tmp/@auto_tlrc>\n"
"\n"
"Thanks To James Blair for finding the bug.\n"
 } ,

 { 25, AUG , 2010 , ZSS , "ConvertDset" , MICRO , TYPE_NEW_OPT,
   "Added -no_history option",
   NULL
 } ,

 { 24, AUG , 2010 , ZSS , "suma" , MINOR , TYPE_MODIFY,
   "Improved SUMA's DotXform (InstaCorr) and GroupInCorr interface",
   "Changed interface so that shft+ctrl+right click is needed to initiate \n"
   "callback. This makes it consistent with AFNI's interface.\n"
   "shft+ctrl+right click and drag now a little faster.\n"
 } ,

 { 23, AUG , 2010 , ZSS , "suma" , MINOR , TYPE_MODIFY,
   "Fixed bug in sub-brick selection lists",
   "Before the bug fix, once a sub-brick selection list was open\n"
   "(right-click on 'I', 'T', or 'B') for one dataset, it never got\n"
   "updated after switching to another dataset, rendering it quite useless.\n"
   "Thanks to Adam Greenberg for reporting the error.\n"
 } ,

 { 23, AUG , 2010 , ZSS , "suma" , MINOR , TYPE_MODIFY,
   "SUMA now detects retinotopy results and displays them appropriately",
   NULL
 } ,
 
 { 23, AUG , 2010 , ZSS , "suma" , MINOR , TYPE_NEW_OPT,
   "Shift+Ctrl left, right rotates surface about Z axis",
   "Useful for rotating flat surfaces"
 } ,
 
  { 23, AUG , 2010 , ZSS , "suma" , MINOR , TYPE_NEW_ENV,
   "Allow users to specify colormaps for retinotopy results",
   "See help section for variables:\n"
   "  SUMA_RetinoAngle_DsetColorMap and SUMA_VFR_DsetColorMap\n"
   "in your ~/.sumarc, after running suma -update_env.\n"
 } ,
 
  { 23, AUG , 2010 , ZSS , "@RetinoProc" , MAJOR , TYPE_NEW_PROG,
   "Packages processing step for phase-based retinotopic mapping.",
   "See @RetinoProc -help for details\n"
 } ,
 
  { 23, AUG , 2010 , ZSS , "3dRetinoPhase" , MAJOR , TYPE_NEW_PROG,
   "Calculate visual field angle from phase-based retinotopy data.",
   NULL
 } ,

  { 23, AUG , 2010 , ZSS , "SurfRetinoMap" , MAJOR , TYPE_NEW_PROG,
   "Calculates Visual Field Signs from visual field angle data",
   NULL
  } ,
 
  { 23, AUG , 2010 , ZSS , "@SUMA_Make_Spec_FS" , MICRO , TYPE_MODIFY,
   "Script now looks for brain envelope surface from FreeSurfer",
   "Thanks to Mike Beauchamp for the modification."
  } ,
 
 { 18, AUG , 2010 , ZSS , "@DriveSuma" , MINOR , TYPE_MODIFY,
   "Improvements to @DriveSuma to make it more didactic",
   NULL
 } ,

 { 18, AUG , 2010 , ZSS , "DriveSuma" , MICRO , TYPE_MODIFY,
   "Added -echo_edu option for edification purposes",
   NULL
 } ,

 { 9, AUG , 2010 , ZSS , "suma-general" , MICRO , TYPE_NEW_OPT,
   "Allows reading 5-column colormaps",
   NULL
 } ,

 { 9, AUG , 2010 , ZSS , "MapIcosahedron" , MICRO , TYPE_BUG_FIX,
   "Changed 2 sprintf lines writing and reading from same address.",
   NULL
 } ,

 { 6, AUG , 2010 , ZSS , "3dSetupGroupInCorr" , MINOR , TYPE_BUG_FIX,
   "Fixed bug with LRpairs when time series had different lengths.",
   "Also fixed minor bug with error message about data size"
 } ,

 { 6, AUG , 2010 , ZSS , "afni" , MICRO , TYPE_BUG_FIX,
   "Tiny changes to functions prettyfying numeric output"
   "Particularly functions approximate_number_string, and \n"
   "commaized_integer_string, and macro MEMORY_CHECK in 3dREMLfit"
 } ,

 { 5, AUG , 2010 , ZSS , "suma" , MICRO , TYPE_BUG_FIX,
   "Fixed crash when using group instant correlation on 1 surface.",
   NULL
 } ,

 { 2, AUG , 2010 , ZSS , "suma" , MICRO , TYPE_BUG_FIX,
   "Fixed default naming for interactive dsets.",
   "The older naming, based on label, rather than filename\n"
   "created some conflicts under certain cases.\n"
   "Repair job in SUMA_dot_product."
 } ,

 { 28, JUL , 2010 , ZSS , "plugout_drive" , MINOR , TYPE_NEW_OPT,
   "Added SET_INDEX to plugout_drive",
   NULL 
 } ,

 { 26, JUL , 2010 , ZSS , "3dclust" , MINOR , TYPE_BUG_FIX,
   "Mean calculations were off for large clusters with large values.",
   NULL 
 } ,

 { 8, JUL , 2010 , ZSS , "suma" , MINOR , TYPE_BUG_FIX,
   "Fixed striping with contour objects",
   NULL 
 } ,

 { 6, JUL , 2010 , ZSS , "suma" , MINOR , TYPE_NEW_ENV,
   "Added SUMA_Cmaps_Dir to point to directory with colormaps",
   "With this environment variable, users can point to a \n"
   "directory containing SUMA colormaps (*.cmap) that should\n"
   "be made available to SUMA. For help on colormap file formats,\n"
   "open a surface controller ('View'-->'Surface Controller'), \n"
   "click on BHelp (bottom left) and then click on 'New' button\n"
   "which is to the right of the colormap selector." 
 } ,

 { 1, JUL , 2010 , ZSS , "SurfToSurf" , MINOR , TYPE_NEW_OPT,
   "Added -mapfile option, allowing SurfToSurf to reuse an existing mapping.",
   NULL
 } ,

 { 30 , JUN , 2010 , ZSS , "@auto_tlrc" , MINOR , TYPE_MODIFY,
   "Added -onewarp, and -init_xform to @auto_tlrc",
"I have made some small improvements to @auto_tlrc, but avoided changing \n"
"its default operation, except in one case.\n"
"\n"
"In 'Usage 2', the old version performed two resampling operations. One in \n" "3dWarp with the default quintic kernel, the other in 3dresample with the \n"
"Bk kernel. The new version can perform only one resampling thereby  \n"
"reducing the smoothness of the final result. To change from the default\n" 
"behavior,use the new option -onewarp. "
"\n"
"The help output of the old version stated that -rmode controlled the \n"
"interpolation kernel in 'Usage 1'. That was not the case. In Usage 1,\n"
"interpolation was always linear. It remains so in the current version,\n"
"but the -rmode option can now be used to change the kernel.\n"
"\n"
"The change in default operation between this version and the previous \n"
"concerns 'Usage 1'. In the old version, the brain was skull stripped, AND\n"
"its intensities adjusted by 3dSkullStrip. It was this adjusted brain that\n"
"was then output in TLRC space. In other terms, the output was with \n"
"no skull, but its values differed from those of the input.\n"
"This is no longer the case. In the current version, you will \n"
"get a skull-stripped version of the input in TLRC space \n"
"(no intensity adjustment).\n"
"Behavior of the -warp_orig_vol is unchanged.\n" 
"This change in 'Usage 1' does not affect the registration transform, \n"
"nor 'Usage 2'.\n" 
"If you insist on the old mode of operation, then contact me and I will \n"
"send you an older version of @auto_tlrc."
 } ,

 { 25 , MAY , 2010 , ZSS , "suma" , MINOR , TYPE_NEW_OPT,
   "Added click and drag for third mouse button",
   NULL
 } ,

 { 21 , MAY , 2010 , ZSS , "SurfToSurf" , MINOR , TYPE_NEW_OPT,
   "Added -dset option to take in niml dsets",
   NULL
 } ,

 { 4 , MAY , 2010 , ZSS , "3dROIstats" , MINOR , TYPE_NEW_OPT,
   "Added -nzsigma to 3dROIstats",
   NULL
 } ,

 { 4 , MAY , 2010 , ZSS , "1dtranspose" , MICRO , TYPE_NEW_OPT,
   "Allowed 1dtranspose to accept -overwrite",
   "The main reason for this is to allow such a command:\n"
   "  1dtranspose -overwrite FILE.1D FILE.1D\n"
   "without having to create temporary files."
 } ,

 { 2 , MAY , 2010 , ZSS , "afni" , MICRO , TYPE_BUG_FIX,
   "Fixed Instacorr update failure when A_ICOR dset is present on disk.",
   NULL
 } ,

 { 29 , APR , 2010 , ZSS , "3dMean" , MICRO , TYPE_MODIFY,
   "Allowed program to work with only one dset for input.",
   NULL
 } ,

 { 21 , APR , 2010 , ZSS , "3ddelay" , MINOR , TYPE_BUG_FIX,
   "-nodtrnd option was setting polort to 1, as opposed to 0.",
   NULL
 } ,

 { 16 , APR , 2010 , ZSS , "3dbucket" , MINOR , TYPE_NEW_OPT,
   "-agluto option = amalgamated -prefix and -glueto",
   NULL
 } ,

 { 12 , APR , 2010 , ZSS , "afni" , MINOR , TYPE_BUG_FIX,
   "Added AFNI_GUI_WRITE_AS_DECONFLICT to modify interactive 'Write' overwrite.",
   "The default is to have the interactive 'Write' buttons overwrite exisiting\n"
   "volumes. If this env variable is set to YES, the 'Write' behaviour follows\n"
   "env AFNI_DECONFLICT\n"
 } ,

 { 12 , APR , 2010 , ZSS , "afni" , MINOR , TYPE_BUG_FIX,
   "Made InstaCorr SeedJump work with GroupInCorr",
   NULL
 } ,

 { 23 , MAR , 2010 , ZSS , "1dplot" , MINOR , TYPE_NEW_OPT,
   "Added -jpgs, and -pngs to allow easier size setup.",
   NULL
 } ,

 { 23 , MAR , 2010 , ZSS , "1dplot" , MICRO , TYPE_BUG_FIX,
   "Fixed unreported bug related to wintitle variable.",
   NULL
 } ,

 { 19 , MAR , 2010 , ZSS , "SurfDist" , MINOR , TYPE_NEW_OPT,
   "Added option to calculate Euclidian distance",
   NULL
 } ,

 { 18 , MAR , 2010 , ZSS , "RetroTS" , MINOR , TYPE_BUG_FIX ,
   "Fixed crash in function remove_PNduplicates",
   NULL
 } ,

 { 16 , MAR , 2010 , ZSS , "3dcalc" , MINOR , TYPE_NEW_OPT ,
   "-within option to test Min <= X <= Max",
   NULL
 } ,

 { 9 , MAR , 2010 , ZSS , "3dROIstats" , MINOR , TYPE_NEW_OPT ,
   "-nomeanout to get rid of mean in output",
   NULL
 } ,

 { 9 , MAR , 2010 , ZSS , "3dclust" , MINOR , TYPE_BUG_FIX ,
   "-isomerge and -isovalue options were being ignored",
   NULL
 } ,

 { 8 , MAR , 2010 , ZSS , "3dTcat" , MINOR , TYPE_BUG_FIX ,
   "Program was not working with string label sub-brick selection",
   NULL
 } ,

 { 5 , MAR , 2010 , ZSS , "3dLocalstat" , MINOR , TYPE_NEW_OPT ,
   "Added -stat P2skew to calculate Pearson's second skewness coefficient",
   NULL
 } ,

 { 3 , MAR , 2010 , ZSS , "3dAutomask" , MINOR , TYPE_NEW_OPT ,
   "-depth option to determine how deep voxel is in mask",
   NULL
 } ,

 { 3 , MAR , 2010 , ZSS , "3dmerge" , MINOR , TYPE_NEW_OPT ,
   "-isomerge and -isovalue options that mimick 3dclust's options",
   NULL
 } ,

 { 3 , MAR , 2010 , ZSS , "3dmerge" , MINOR , TYPE_NEW_OPT ,
   "-1clust_depth option to determine how deep voxel is in cluster",
   NULL
 } ,

 { 1 , MAR , 2010 , ZSS , "@SUMA_Make_Spec_FS" , MINOR , TYPE_MODIFY ,
   "Script now deals with 2009, and 2005 parcellations.",
   NULL
 } ,

 { 1 , MAR , 2010 , ZSS , "FSread_annot" , MINOR , TYPE_MODIFY ,
   "Allow FSread_annot to work with 2009 parcellation results.",
   NULL
 } ,

 { 1 , MAR , 2010 , ZSS , "FSread_annot" , MINOR , TYPE_NEW_OPT ,
   "Added -FSversoin, -hemi, and -FScmap* options for 2009 parcellations",
   NULL
 } ,

 { 1 , MAR , 2010 , ZSS , "@FS_roi_label" , MINOR , TYPE_NEW_OPT ,
   "Modified -name to accept 'ALL' ",
   NULL
 } ,

 { 26 , FEB , 2010 , ZSS , "afni" , MINOR , TYPE_MODIFY ,
   "Insert Dtable structure (Label_Dtable) in dset",
   "Inserted Dtable structure (dset->Label_Dtable) in THD_3dim_dataset.\n"
   "The hash table is used to report on the label corresponding to a voxel's\n"
   "integer value. Inserting a label table into the header can be done\n"
   "with 3drefit.\n"
   "Labels are reported in the ULay and OLay value fields in the bottom right\n"
   "corner of AFNI's Define Overlay panel. The hint at that location also \n"
   "shows the labels, which could be quite long."
 } ,

 { 26 , FEB , 2010 , ZSS , "3drefit" , MINOR , TYPE_NEW_OPT ,
   "-labeltable option to add a label table to a dataset",
   NULL
 } ,

 { 15 , FEB , 2010 , ZSS , "afni" , MINOR , TYPE_MODIFY ,
   "Automatically setup range and sign for ROI colorbars",
   NULL
 } ,

 { 15 , FEB , 2010 , ZSS , "3dTstat" , MINOR , TYPE_NEW_OPT ,
   "-arg*1 options to keep from getting 0 in arg* output",
   NULL
 } ,

 { 15 , FEB , 2010 , ZSS , "3dTstat" , MINOR , TYPE_NEW_OPT ,
   "-*mask options to allow masking",
   NULL
 } ,

 { 12 , FEB , 2010 , ZSS , "MapIcosahedron" , MINOR , TYPE_NEW_OPT ,
   "-*_cut_surfaces to deal with bad triangles on standard flat surfaces",
   NULL
 } ,

 { 12 , FEB , 2010 , ZSS , "3dTstat" , MINOR , TYPE_NEW_OPT ,
   "Added -argmin1, -argmax1, -argabsmax1 options to increment argument by 1",
   NULL
 } ,

 { 12 , FEB , 2010 , ZSS , "suma" , MINOR , TYPE_MODIFY ,
   "Better setup of left and right flat surfaces.",
   NULL
 } ,

 { 14 , JAN , 2010 , ZSS , "3dLocalstat" , MINOR , TYPE_NEW_OPT ,
   "Added -rank and -frank options to 3dLocalstat",
   NULL
 } ,

 { 15 , DEC , 2009 , ZSS , "afni" , MINOR , TYPE_NEW_OPT ,
   "Allow label based sub-brick selection in AFNI and SUMA",
   NULL
 } ,

 { 1 , DEC , 2009 , ZSS , "suma-general" , MINOR , TYPE_BUG_FIX ,
   "Ignore triangles from Caret with nodes that have all zero coords",
   "Not doing so results in bad display of some flat meshes because\n"
   "the .topo file contains triangles with nodes that appear masked by\n"
   "0.0 0.0 0.0 in the .coord file"
 } ,

 { 1 , DEC , 2009 , ZSS , "@SUMA_Make_Spec_Caret" , MINOR , TYPE_BUG_FIX ,
   "Improved script to make it pick up new naming convention.",
   NULL
 } ,
 
 { 25 , NOV , 2009 , ZSS , "MapIcosahedron" , MAJOR , TYPE_NEW_OPT ,
   "Added -NN_dset_map and -dset_map options to map dsets onto new meshes",
   "The program now automatically warps LabelDsets specified in the spec\n"
   "file, or any dataset specified on the command line."
 } ,
 
 { 24 , NOV , 2009 , ZSS , "1dmatcalc" , MINOR , TYPE_NEW_OPT ,
   "Added &read4x4Xform to read in spatial affine transformations.",
   "1dmatcalc can now take in spatial affine transforms in vector\n"
   "or matrix form"
 } ,
 
 { 24 , NOV , 2009 , ZSS , "afni-matlab" , MINOR , TYPE_BUG_FIX ,
   "Stopped writing empty attributes which caused trouble in AFNI.",
   "Empty attributes make AFNI halt the parsing of the header."
 } ,
 
 { 23 , NOV , 2009 , ZSS , "afni" , MICRO , TYPE_BUG_FIX ,
   "A couple of small fixes to drive_switch functions.",
   "The problem with with sscanf reading past string end\n"
   "in certain cases."
 } ,
 
 { 23 , NOV , 2009 , ZSS , "suma" , MINOR , TYPE_BUG_FIX ,
   "Workaround for connection drop between AFNI and SUMA in Shared Memory Mode.",
   "Env SUMA_AttemptTalkRecover allows SUMA to recover from drop.\n"
   "This ENV is set by default to No."
 } ,
 
 { 19 , NOV , 2009 , ZSS , "suma" , MINOR , TYPE_BUG_FIX ,
   "Fixed recorder lag on OS X machines.",
   "On OS X platforms, the 'r' or 'R' recording modes\n"
   "used to record the previous image it seems. "
 } ,
 
 { 17 , NOV , 2009 , ZSS , "@SUMA_Make_Spec_FS" , MINOR , TYPE_NEW_OPT ,
   "Script now automatically sets up the spec file with labeled datasets",
   "Labeled datasets are created from annotation files,\n"
   "and are now rendered in a special manner in SUMA"
 } ,
 
 { 17 , NOV , 2009 , ZSS , "suma_general" , MINOR , TYPE_NEW_ENV ,
   "SUMA_AlwaysAssignSurface (see suma -environment for details)",
   NULL
 } ,  
 
 { 17 , NOV , 2009 , ZSS , "suma_general" , MINOR , TYPE_NEW_ENV ,
   "SUMA_LabelDsetOpacity (see suma -environment for details)",
   NULL
 } ,  
 
 { 17 , NOV , 2009 , ZSS , "suma_general" , MINOR , TYPE_NEW_ENV ,
   "SUMA_ConvexityDsetOpacity (see suma -environment for details)",
   NULL
 } ,  
 
 { 17 , NOV , 2009 , ZSS , "suma_general" , MINOR , TYPE_NEW_ENV ,
   "SUMA_ShowLabelDsetAtStartup (see suma -environment for details)",
   NULL
 } ,  
 
 { 17 , NOV , 2009 , ZSS , "ROI2dataset" , MINOR , TYPE_NEW_OPT ,
   "Option -label_dset creates a labeled dataset from ROIs.",
   "Labeled datasets are now rendered in a special manner in SUMA"
 } ,

 { 17 , NOV , 2009 , ZSS , "FSread_annot" , MINOR , TYPE_NEW_OPT ,
   "Option -dset creates a labeled dataset from annotation file.",
   "Labeled datasets are now rendered in a special manner in SUMA"
 } ,

 { 17 , NOV , 2009 , ZSS , "suma" , MAJOR , TYPE_NEW_OPT ,
   "SUMA now handles labeled datatets in a special manner",
   "An example of a labeled dataset would be FreeSurfer's annotation files.\n"
   "If an annotation file is present in the spec file (@SUMA_Make_Spec_FS),\n"
   "SUMA will display it as part of the background.\n"
   "You can chose to display the labeled dataset in color, contours, or both.\n"
   "Data from labeled datasets is now shown under the 'Lbl' field in the\n"
   "surface controller's Xhair block.\n"
 } ,


 { 9 , NOV , 2009 , ZSS , "SurfMeasures" , MINOR , TYPE_NEW_OPT ,
   "Added option for improved node volume estimation.",
   "The new measure, called node_volg, uses Gauss' Theorem to estimate\n"
   "the volume associated with a node in two isotopic surfaces. This option\n"
   "is more robust to surface curvature variations.\n"
 } ,

 { 9 , NOV , 2009 , ZSS , "SurfPatch" , MINOR , TYPE_NEW_OPT ,
   "Added options to check for, and correct 'bowties' in pathches.",
   "Bowties in patches result in non 2-manifold stiched surfaces whose\n"
   "volume cannot be caluclated. The *bowtie option deal with such cases.\n"
 } ,

 { 9 , NOV , 2009 , ZSS , "SurfPatch" , MINOR , TYPE_NEW_OPT ,
   "Added options to shrink patch contours at nodes not in selected nodes.",
   "See options -adjust_contour for details."
 } ,

 { 3 , NOV , 2009 , ZSS , "MapIcosahedron" , MICRO , TYPE_MODIFY ,
   "Minor code change, EdgeList was computed twice for no reason.",
   NULL
 } ,

 { 16 , OCT , 2009 , ZSS , "afni" , MICRO , TYPE_MODIFY ,
   "Turned off zeroing of uncommented text in .1D files",
   "You can turn this behaviour back on by setting env\n"
   "AFNI_1D_ZERO_TEXT to YES"
 } ,

 { 16 , OCT , 2009 , ZSS , "1dSEM" , MICRO , TYPE_MODIFY ,
   "Setenv AFNI_1D_ZERO_TEXT to YES  ",
   NULL
 } ,

 { 15 , OCT , 2009 , ZSS , "afni" , MICRO , TYPE_MODIFY ,
   "Manual graph scaling forces AUTOSCALE [A] off",
   NULL
 } ,

 { 14 , OCT , 2009 , ZSS , "suma" , MINOR , TYPE_MODIFY ,
   "Direct viewing of pre-processed datasets in Dot xform",
   NULL
 } ,

 { 14 , OCT , 2009 , ZSS , "suma" , MINOR , TYPE_MODIFY ,
   "New help window for Dot xform",
   NULL
 } ,

 { 13 , OCT , 2009 , ZSS , "ConvertSurface" , MICRO , TYPE_NEW_OPT ,
   "-xmat_1D allows for single row transform definition",
   NULL
 } ,

 { 13 , OCT , 2009 , ZSS , "quickspec" , MICRO , TYPE_NEW_OPT ,
   "-tsnad to allow for setting anatomical flag and local domain parent",
   NULL
 } ,

 { 7 , OCT , 2009 , ZSS , "1dplot" , MICRO , TYPE_NEW_OPT ,
   "1dplot's window frame now shows a title. See 1dplot -wintitle for details.",
   NULL
 } ,

 { 6 , OCT , 2009 , ZSS , "suma" , MAJOR , TYPE_BUG_FIX ,
   "More bug fixes related to ROI loading, and OSX's GLX problem",
   NULL
 } ,

 { 24 , SEP , 2009 , ZSS , "suma" , MINOR , TYPE_NEW_OPT ,
   "Improved the 'star' blurring interface.",
   NULL
 } ,

 { 24 , SEP , 2009 , ZSS , "suma" , MINOR , TYPE_NEW_ENV ,
   "Setup final color blurring level.",
   NULL
 } ,

 { 24 , SEP , 2009 , ZSS , "suma" , MAJOR , TYPE_BUG_FIX ,
   "Fixed crashes on OS X 10.5 caused by OS X's buggy GLX implementation",
   NULL
 } ,

 { 18 , SEP , 2009 , ZSS , "FSread_annot" , MINOR , TYPE_NEW_OPT ,
   "Allow specifying external FreeSurfer color table.",
   NULL
 } ,

 { 18 , SEP , 2009 , ZSS , "SurfClust" , MINOR , TYPE_NEW_OPT ,
   "Added option -n minnodes",
   NULL
 } ,

 { 14 , SEP , 2009 , ZSS , "SurfClust" , MINOR , TYPE_NEW_OPT ,
   "Allow specifying rmm in number of edges connecting nodes.",
   NULL
 } ,

 { 9 , SEP , 2009 , ZSS , "@fast_roi" , MINOR , TYPE_NEW_OPT ,
   "Allow @fast_roi to accept existing TLRC transformed anatomy",
   NULL
 } ,

 { 16 , JUL , 2009 , ZSS , "afni" , MINOR , TYPE_NEW_OPT ,
   "Added option to force autoscale on the graphing window",
   "The option is accessible by pressing 'A' in the graph window\n"
   "or under 'Graph->Opt->Scale->AUTO'. "
 } ,

 { 22 , MAY , 2009 , ZSS , "@FS_roi_label" , MINOR , TYPE_NEW_PROG ,
   "A script to get FreeSurfer parcellation and annotation labels",
   "The script is used to return a label associated with a FreeSurfer\n"
   "annotation or parcellation integer label."
 } ,

 { 21 , MAY , 2009 , ZSS , "suma" , MINOR , TYPE_BUG_FIX ,
   "Additional check for caret-version string",
   "It seems that the syntax for caret-version has changed\n"
   "slightly and was being missed by SUMA. As a result, Caret\n"
   "surfaces were not being properly aligned with the volumes."
 } ,

 { 21 , MAY , 2009 , ZSS , "imcat" , MICRO , TYPE_NEW_OPT ,
   "Now output a grayscale 1D version of an image",
   "That is done by adding a .1D to the -prefix parameter."
 } ,

 { 19 , MAY , 2009 , ZSS , "prompt_user" , MINOR , TYPE_NEW_PROG ,
   "Prompts user for input with an X window",
   NULL
 } ,

 { 19 , MAY , 2009 , ZSS , "DriveSuma" , MICRO , TYPE_NEW_OPT ,
   "Modified time out for DriveSuma to 5 minutes",
   "Time out can be controlled by environment variable SUMA_DriveSumaMaxWait"
 } ,

 { 19 , MAY , 2009 , ZSS , "@ROI_Corr_Mat" , MAJOR , TYPE_NEW_PROG ,
   "A script to calculate correlation matrices between ROI time series",
   "Calculates the correlation matrices between average time series from ROIs\n"
   "defined in a mask volume. Script was written with Hang Joon Jo."
 } ,

 { 19 , MAY , 2009 , ZSS , "3dROIstats" , MINOR , TYPE_NEW_OPT ,
   "Added -zerofill and -roisel options",
   "Added options to force output for empty ROIs."
 } ,

 { 19 , MAY , 2009 , ZSS , "1ddot" , MINOR , TYPE_NEW_OPT ,
   "Added -okzero to keep program from exiting with all zero input",
   "Added options to force output for empty ROIs."
 } ,
 
 { 8 , MAY , 2009 , ZSS , "@SUMA_Make_Spec_FS" , MINOR , TYPE_NEW_OPT ,
   "The script now automatically turns FS annot files to SUMA ROI files",
   NULL
 } ,
 
 { 8 , MAY , 2009 , ZSS , "suma" , MINOR , TYPE_BUG_FIX ,
   "Better display of ROI contours on patches",
   "Suma now correctly handles ROI display when the ROIs\n"
   "contain nodes that are not in the patch. Also, on flat surfaces,\n"
   "ROI contours are drawn raised so that they float over flat surfaces."
 } ,

 { 5 , MAY , 2009 , ZSS , "DriveSuma" , MINOR , TYPE_BUG_FIX ,
   "Added 'd' and 'j' keys",
   NULL
 } ,

 { 5 , MAY , 2009 , ZSS , "3dRank" , MINOR , TYPE_NEW_PROG ,
   "A program that substitutes a voxel's value by that value's rank",
   NULL
 } ,
 
 { 29 , APR , 2009 , ZSS , "SurfSmooth" , MINOR , TYPE_BUG_FIX ,
   "-detprefix_* options now do something!",
   NULL
 } ,
 
 { 29 , APR , 2009 , ZSS , "3dTcorrMap" , MINOR , TYPE_NEW_OPT ,
   "-VarThresh* options for obtaining counts at different thresholds",
   NULL
 } ,
 
 { 29 , APR , 2009 , ZSS , "ConvertDset" , MINOR , TYPE_NEW_OPT ,
   "-node_select_1D now respects node ordering",
   NULL
 } ,
 
 { 29 , APR , 2009 , ZSS , "ROI2dataset" , MINOR , TYPE_NEW_OPT ,
   "-nodelist* options to output node sequence forming ROI",
   NULL
 } ,
 
 { 29 , APR , 2009 , ZSS , "MapIcosahedron" , MINOR , TYPE_BUG_FIX ,
   "Fixed crash when using -it option for geometry smoothing",
   NULL
 } ,
 
 { 14 , APR , 2009 , ZSS , "suma-general" , MINOR , TYPE_BUG_FIX ,
   "niml ROI files with empty ROI no longer crash suma/ROI2dataset",
   NULL
 } ,
 
 { 6 , APR , 2009 , ZSS , "DriveSuma" , MINOR , TYPE_NEW_OPT ,
   "Added support for '.', ',', and space keys, and 'shw_0'",
   NULL
 } ,
 
 { 1 , APR , 2009 , ZSS , "suma" , MINOR , TYPE_NEW_OPT ,
   "Arrow keys based navigation along surface.",
   "See 'Alt+U-D' section in SUMA's interactive usage help.\n" 
 } ,
 
 { 1 , APR , 2009 , ZSS , "suma" , MICRO , TYPE_NEW_ENV ,
   "Added SUMA_KeyNodeJump variable \n",
   "This variable controls number of nodes to jump with arrow keys navigation."
   "See suma -environment for complete list and defaults.\n"
 } ,
 
 { 31 , MAR , 2009 , ZSS , "suma" , MINOR , TYPE_NEW_OPT ,
   "Added 'on the fly' dot product computation",
   "This feature is only available with SUMA's -dev\n"
   "option. It is still in testing mode and its \n"
   "interface may change. If intrigued, see 'D' key's\n"
   "help in SUMA's interactive usage help.\n" 
 } ,
 
 { 31 , MAR , 2009 , ZSS , "suma-general" , MINOR , TYPE_MODIFY ,
   "Handling of double precision datatypes.",
   NULL
 } ,
 
 { 31 , MAR , 2009 , ZSS , "DriveSuma" , MINOR , TYPE_NEW_OPT ,
   "Added support for '[' and ']' keys and -view_surf",
   NULL
 } ,
 
 { 26 , MAR , 2009 , ZSS , "suma-general" , MINOR , TYPE_NEW_OPT ,
   "Support for reading/writing MNI .obj surfaces",
   NULL
 } ,
 
 { 12 , MAR , 2009 , ZSS , "suma-general" , MINOR , TYPE_NEW_OPT ,
   "SUMA talks to matlab.",
   NULL
 } ,
 

 { 10 , FEB , 2009 , ZSS , "DriveSuma" , MICRO , TYPE_NEW_OPT ,
   "Added -anim_dup to slow down movies",
   NULL
 } ,

 { 3 , FEB , 2009 , ZSS , "suma" , MICRO , TYPE_BUG_FIX ,
   "No longer crashes with old format ROIs",
   NULL
 } ,

 { 3 , FEB , 2009 , ZSS , "suma" , MICRO , TYPE_BUG_FIX ,
   "Fixed free-related error messages on OS X",
   "These errors were generated because some pointers\n"
   "were allocated with MCW_MALLOC and freed without it.\n"
   "solution was to include mcw_malloc.h in coxplot and\n"
   "gifti_choice.c"
 } ,

 { 2 , FEB , 2009 , ZSS , "DriveSuma" , MINOR , TYPE_NEW_OPT ,
   "Added load_col option to DriveSuma",
   NULL
 } ,

 { 2 , FEB , 2009 , ZSS , "SurfDist" , MINOR , TYPE_NEW_OPT ,
   "SurfDist now outputs distances from one node to a set of other nodes",
   NULL
 } ,

 { 13 , JAN , 2009 , ZSS , "afni-matlab" , MINOR , TYPE_NEW_PROG ,
   "Added GS_orth_1D.m to perform Gram-Schmidt orthogonalization",
   NULL
 } ,

 { 13, JAN , 2009 , ZSS , "afni-general" , MINOR ,  TYPE_BUG_FIX,
   "AFNI should now be Lesstif compliant",
   "Made numerous changes with Rick Reynolds. The biggest modification\n"
   "regards buttons inside popup menus. Those buttons are no longer\n"
   "inside rowcolumn widgets and had to be directly managed by afni."
 },

 { 5, JAN , 2009 , ZSS , "MapIcosahedron" , MINOR ,  TYPE_NEW_OPT,
   "Program no longer confined to FreeSurfer surfaces",
   "The program now allows the user to specify any morph sphere,\n"
   "not just sphere.reg or sphere. This required rewriting much of\n"
   "of the program's main section but the algorithm for the mapping\n"
   "remains unchanged."
 },

 { 5, JAN , 2009 , ZSS , "CreateIcosahedron" , MICRO ,  TYPE_NEW_OPT,
   "Program uses new Spec writing function and writes different surface formats",
   NULL
 },

 { 13 , DEC , 2008 , ZSS , "afni-matlab" , MAJOR , TYPE_NEW_PROG ,
   "RetroTS creates regressors for cardiac, respiratory, and RVT filtering",
   "RetroTS.m and accompanying functions create slice-based regressors\n"
   "for regressing out the effects of cardiac, respiratory, and RVT effects\n"
   "from FMRI time series. The regressors generated are meant to be used \n"
   "with 3dREMLfit."
 } ,

 { 9, DEC , 2008 , ZSS , "@Align_Centers" , MINOR ,  TYPE_NEW_OPT,
   "Added -cm option to allow centering based on the center of mass",
   NULL
 },

 { 9, DEC , 2008 , ZSS , "suma" , MINOR ,  TYPE_BUG_FIX,
   "Fixed (potential) SUMA crash when Draw ROI interface is first opened",
   NULL
 },

 { 9, DEC , 2008 , ZSS , "3dCM" , MICRO ,  TYPE_MODIFY,
   "Added history note to 3dCM",
   NULL
 },
 
 { 8, DEC , 2008 , ZSS , "suma" , MICRO ,  TYPE_BUG_FIX,
   "SUMA works now with LESSTIF, interface is almost the same as in MOTIF",
   NULL
 },

 { 2, DEC , 2008 , ZSS , "general" , MICRO ,  TYPE_BUG_FIX,
   "Environment vars. set in the shell override those in .afnirc or .sumarc",
   NULL
 },

 { 2, DEC , 2008 , ZSS , "@NoisySkullStrip" , MAJOR ,  TYPE_NEW_PROG,
   "A script to improve skull stripping in noisy of heavily shaded data",
   NULL
 },

 { 2, DEC , 2008 , ZSS , "@Spharm.examples" , MAJOR ,  TYPE_NEW_PROG,
   "A script to demonstrate the usage of SpharmDeco and SpharmReco",
   NULL
 },

 { 2, DEC , 2008 , ZSS , "SpharmReco" , MAJOR ,  TYPE_NEW_PROG,
   "Reconstructs data from spherical harmonics decomposition.",
   "See SpharmDeco -help and the demo script @Spharm.examples\n"
   "for details."
 },

 { 2, DEC , 2008 , ZSS , "SpharmDeco" , MAJOR ,  TYPE_NEW_PROG,
   "Performs spherical harmonics decomposition.",
   "This program performs spherical harmonics decomposition\n"
   "for surfaces' geometry and/or surface-based data\n"
   "See SpharmReco -help and the demo script @Spharm.examples\n"
   "for details."
 },
 
 { 21 , NOV , 2008 , ZSS , "@fast_roi" , MAJOR ,  TYPE_NEW_PROG,
   "Creates Atlas-based ROIs in original space for real-time experiments ",
   NULL
 },
 
 { 13 , NOV , 2008 , ZSS , "suma" , MINOR ,  TYPE_NEW_OPT,
   "Added a NIDO sphere as Displayable Object ",
   "See suma -help_nido for details."
 },
 
 { 13 , NOV , 2008 , ZSS , "@SUMA_AlignToExperiment" , MINOR ,  TYPE_NEW_OPT,
   "Added -al option to use 3dAllineate -lpa ",
   NULL
 },
 
 { 03 , NOV , 2008 , ZSS , "3dNLfim" , MICRO ,  TYPE_NEW_OPT,
   "Added Exp (single exponential) model ",
   NULL
 },
 
 { 17 , OCT , 2008 , ZSS , "imcat" , MICRO ,  TYPE_NEW_OPT,
   "-crop is a new option for cropping an image ",
   NULL
 },
 
 { 03 , OCT , 2008 , ZSS , "suma" , MICRO ,  TYPE_NEW_ENV,
   "SUMA_StartUpLocation to control initial window placement ",
   "See output for suma -environment for details."
 },
 
 { 03 , OCT , 2008 , ZSS , "@DO.examples" , MINOR ,  TYPE_NEW_PROG,
   "A script that demonstrates the use of Displayable Objects",
   "See SUMA's interactive help for ctrl+alt+s for more information\n"
   "on Displayable Objects (DOs). "
 },

 { 03 , OCT , 2008 , ZSS , "suma" , MINOR ,  TYPE_NEW_OPT,
   "SUMA accepts text, images, and textures as Displayable Objects",
   "See SUMA's interactive help for ctrl+alt+s for more information\n"
   "on Displayable Objects (DOs). The new DOs are in a simple \n"
   "NIML format. It is simplest to look at the script @DO.examples\n"
   "for illustrations of the various forms of DOs that SUMA supports.\n"
   "Sample NIML DOs (called NIDOs) are now provided with the distribution\n"
   "They are called demo.*.niml.do."
 },

 { 03 , OCT , 2008 , ZSS , "DriveSuma" , MINOR ,  TYPE_NEW_OPT,
   "-viewer_position/_width/_height/_size to control window placement",
   "See DriveSuma -help for details."
 },


 { 16 , SEP , 2008 , ZSS , "DriveSuma" , MINOR ,  TYPE_NEW_OPT,
   "-load_do sends SUMA Displayable Objects to be rendered",
   "See SUMA's interactive help for ctrl+alt+s for more information\n"
   "on Displayable Objects.\n"
 },

 { 16 , SEP , 2008 , ZSS , "SurfDist" , MINOR ,  TYPE_NEW_OPT,
   "-node_path_do outputs the shortest path between two nodes",
   "The shortest path(s) are stored as a SUMA Displayable Object\n"
   "which can be loaded into SUMA with ctrl+alt+s or using DriveSuma.\n"
 },

 { 2 , SEP , 2008 , ZSS , "3dSkullStrip" , MINOR ,  TYPE_BUG_FIX,
   "Starting sphere center was incorrectly initialized",
   NULL
 },
 
 { 29 , AUG , 2008 , ZSS , "afni-general" , MICRO,  TYPE_GENERAL,
   "Added toy programs 3dTsmoothR.c and toyR.c to test C<-->R interface",
   "The programs demonstrate how to call R functions from C.\n"
   "To build them one needs to run tcsh ./@RmakeOpts\n"
   "Add 'include Makefile.R.INCLUDE' to Makefile\n"
   "Then make 3dTsmoothR toyR\n"
   "The programs demonstrate time series processing and plotting in R."
 },
  
 { 29 , AUG , 2008 , ZSS , "ExamineXmat" , MAJOR,  TYPE_NEW_PROG,
   "An interactive tool to examine a design matrix",
   NULL
 },
 
 { 29 , AUG , 2008 , ZSS , "3ddelay" , MINOR ,  TYPE_BUG_FIX,
   "Unitialized pointer in 3ddelay",
   NULL
 },

 { 29 , JUL , 2008 , ZSS , "afni" , MINOR , TYPE_MODIFY ,
   "Changed transform used to crete mni coord. in interactive whereami",
   "The transform from TLRC to MNI used to be via the manually TLRCed\n"
   "N27 brain. However this created inconsistency in the second line of the \n"
   "Focus point output (MNI Brain) on the order of a couple of mm\n"
   "with the command-line whereami program. \n"
   "Now both interactive and command-line whereami produce the same \n"
   "Focus Point output. Note that the rest of the whereami\n"
   "output remains unchanged."
 },
  
 { 29 , MAY , 2008 , ZSS , "3dfim+" , MINOR , TYPE_MODIFY ,
   "allowed for -polort -1 and for sub-brick selection with dataset names",
   NULL
 } ,

 { 15 , MAY , 2008 , ZSS , "3dmaskdump" , MINOR , TYPE_NEW_OPT ,
   "added -n_rand and -n_randseed",
   NULL
 } ,

 { 15 , MAY , 2008 , ZSS , "3dfim+" , MINOR , TYPE_BUG_FIX ,
   "Fixed memory corruption when using more than 20 regressors",
   NULL
 } ,

 { 9 , MAY , 2008 , ZSS , "3dROIstats" , MAJOR , TYPE_NEW_OPT ,
   "Added option -1Dformat to output results in 1D format",
   NULL
 } ,

 { 8 , MAY , 2008 , ZSS , "3dsvm" , MAJOR , TYPE_BUG_FIX ,
   "Fixed memory corruption caused by improper declaration of combName",
   NULL
 } ,

 { 24 , APR , 2008 , ZSS , "SurfSmooth" , MICRO , TYPE_BUG_FIX ,
   "Fixed leaky SurfSmooth.",
   "Leak was from one of fin_float pointers in Head07\n"
   "Also found leak in THD_extract_detrended_array and a few\n"
   "more small leaks in other SUMA function. Valgrind is good."
 } ,

 { 23 , APR , 2008 , ZSS , "afni-python" , MICRO , TYPE_MODIFY ,
   "Changed methods in afni_name class",
   ".path is now absolute\n"
   ".inp() and .out() should be used to specify input and output volumes\n"
   "Updated lpc_align.py and align_epi_anat.py to reflect changes."
 } ,

 { 2 , APR , 2008 , ZSS , "suma" , MINOR , TYPE_MODIFY ,
   "Changed suma -environment's output to reflect user's current environment.",
   "With this change, users can always replace their .sumarc with\n"
   "the one output by suma -environment without worrying about loosing\n"
   "their preferred settings."
 } ,

 { 27 , MAR , 2008 , ZSS , "suma-general" , MICRO , TYPE_MODIFY ,
   "Changed internal handling of various MATVEC sources.",
   "Simplified handling of different sources of MATVECs from\n"
   "AFNI's headers. Added handling of ALLINEATE_MATVEC to the\n"
   "mix."
 } ,

 { 27 , MAR , 2008 , ZSS , "afni-matlab" , MINOR , TYPE_MODIFY ,
   "Added other return options for Read_1D.m and BrikLoad.m",
   NULL
 } ,

 { 26 , MAR , 2008 , ZSS , "3dvolreg" , MINOR , TYPE_BUG_FIX ,
   "3dvolreg's -1Dmatrix_save was not always accounting for centers of rotation"
   ,"Now the output of -1Dmatrix_save always matches what one would get for\n"
   "the first sub-brick from\n"
   "   cat_matvec FRED+orig::VOLREG_MATVEC_000000 -I "
 } ,

 { 26 , MAR , 2008 , ZSS , "cat_matvec" , MINOR , TYPE_MODIFY ,
   "cat_matvec can now output IJK_TO_CARD_DICOM and IJK_TO_DICOM_REAL" ,
   NULL
 } ,

 { 26 , MAR , 2008 , ZSS , "@Align_Centers" , MINOR , TYPE_MODIFY ,
   "Script now outputs a matrix that represents the shift" ,
   "This matrix can be used with 3dAllineate to perform the shift."
 } ,

 { 26 , MAR , 2008 , ZSS , "@SUMA_AlignToExperiment" , MINOR , TYPE_NEW_OPT ,
   "Added -align_centers as an option to deal with very large moves" ,
   "It used to be that users had to run @Align_Centers on all their\n"
   "data when big moves were needed to align the surface's antaomy\n"
   "to that of the experiment. This is no longer needed."
 } ,

 { 25 , MAR , 2008 , ZSS , "suma" , MINOR , TYPE_BUG_FIX ,
   "ROIs drawn on different surfaces now handled properly" ,
   NULL
 } ,

 { 25 , MAR , 2008 , ZSS , "suma-general" , MINOR , TYPE_MODIFY ,
   "Instead of SAME, one can use surface's name as LocalDomainParent" ,
   NULL
 } ,

 { 25 , MAR , 2008 , ZSS , "ConvertSurface" , MICRO , TYPE_NEW_OPT ,
   "Added option -ixmat_1D to apply inverse of affine transform" ,
   NULL
 } ,

 { 25 , MAR , 2008 , ZSS , "@auto_tlrc" , MICRO , TYPE_MODIFY ,
   "Improvements on -rigid_equiv output and .1D transform output" ,
   NULL
 } ,

 { 25 , MAR , 2008 , ZSS , "@SUMA_AlignToExperiment" , MICRO , TYPE_MODIFY ,
   "View is now changed even if rigid-body registration is used." ,
   NULL
 } ,

 { 21 , MAR , 2008 , ZSS , "3dnvals" , MICRO , TYPE_NEW_OPT ,
   "Added -all to 3dnvals to output all 4 dimensions. " ,
   NULL
 } ,

 { 17 , MAR , 2008 , ZSS , "lpc_align.py" , MICRO , TYPE_NEW_OPT ,
   "Added -big_move and -partial_coverage." ,
   "-big_move is for when large displacements are needed for alignment.\n"
   "-partial_coverage is for when EPI covers a portion of the brain."
 } ,

 { 12 , MAR , 2008 , ZSS , "suma" , MICRO , TYPE_BUG_FIX ,
   "Changed crash in SurfaceMetrics when -spec and -i options are mixed. " ,
   NULL
 } ,

 { 12 , MAR , 2008 , ZSS , "suma" , MINOR , TYPE_MODIFY ,
   "Changed surface controller font to 8. " ,
   "You can get the old font size with environment\n"
   "variable SUMA_SurfContFontSize BIG.\n"
   "The default is now SMALL.\n"
 } ,

 { 7 , MAR , 2008 , ZSS , "suma" , MINOR , TYPE_NEW_OPT ,
   "Color map in surface controller can be flipped and rotated" ,
   "* f key for flipping colormap\n"
   "* Up/Down keys for rotating color map\n"
   "* Home for home\n"
   "Hit ctrl+h on Surface Controller's colormap for more help."
 } ,

 { 6 , MAR , 2008 , ZSS , "suma" , MINOR , TYPE_NEW_ENV ,
   "Added three variables affecting the surface controller" ,
   "* SUMA_ShowOneOnly: Sets '1 Only' on or off. On by default\n"
   "* SUMA_GraphHidden: Update open graphs even if corresponding dset\n"
   "                    is hidden."
   "* SUMA_ColorMapRotationFraction: Fraction of colormap to rotate\n"
   "                    up or down with arrow keys.\n"
   "See suma -environment for a complete list."
 } ,

 { 5 , MAR , 2008 , ZSS , "suma-general" , MAJOR , TYPE_GENERAL ,
   "Support for GIFTI surface format writing" ,
   "GIFTI writing can now be done with SUMA programs.\n"
   "For example, see options -o_gii and -xml* in ConvertSurface program."
 } ,
 
 { 28 , FEB , 2008 , ZSS , "suma-general" , MAJOR , TYPE_NEW_ENV ,
   "Support for GIFTI surface format reading" ,
   NULL 
 } ,
 
 { 27 , FEB , 2008 , ZSS , "suma" , MAJOR , TYPE_GENERAL ,
   "Addition of p and q values under threshold bar" ,
   "Use BHelp on p and q text in suma to get more info."
 } ,
 
 { 27 , FEB , 2008 , ZSS , "suma-general" , MICRO , TYPE_BUG_FIX ,
   "Another bout of initialization and leak fixes based on valgrind's output" ,
   "The hope is that some weird X behavior is caused by uninitialized\n"
   "variables."
 } ,
 
 { 14 , FEB , 2008 , ZSS , "suma" , MAJOR , TYPE_GENERAL ,
   "Graphing of dset content with 'g'" ,
   "A graph of the column content at a node can be plotted\n"
   "This would be the surface equivalent to AFNI's graphing function.\n"
   "See suma's interactive help 'ctrl+h' for more info."
 } ,

 { 12 , FEB , 2008 , ZSS , "suma" , MINOR , TYPE_NEW_OPT ,
   "An option to show all of suma's environment variables" ,
   "See help for -environment in suma -help."
 } ,
 
 { 6 , FEB , 2008 , ZSS , "SurfDist" , MAJOR , TYPE_NEW_PROG ,
   "Program to calculate geodesic internodal distances" ,
   NULL
 } ,
 
 { 12 , JAN , 2008 , ZSS , "suma" , MAJOR , TYPE_GENERAL ,
   "Scroll lists for I T and B selectors in suma" ,
   "Right click on pulldown menu titles to get\n"
   "a scroll list instead. This makes selecting from\n"
   "a long list of options, or columns, much easier.\n"
   "Right click on 'I' to the left of suma's Intensity Selection\n"
   "button for an illustration."
 } ,
 
 { 19 , DEC , 2007 , ZSS , "suma-general" , MAJOR , TYPE_GENERAL ,
   "Use of '[i]' to select node index from surface dset" ,
   "Square bracket '[]' selection works for surface-based\n"
   "datasets much like it does for volume-based ones.\n"
   "In addition, one can use '[i]' to select the indices\n"
   "of nodes for which data are defined in a particular \n"
   "surface-based dataset.\n"
   "For more information, see 'SUMA dataset input options:' \n"
   "section in the output of ConvertDset -help ."
 } ,

 { 18 , DEC , 2007 , ZSS , "ROIgrow" , MINOR , TYPE_NEW_PROG ,
   "Grows regions separately, depending on labels" ,
   NULL
 } ,
 
 { 18 , DEC , 2007 , ZSS , "ROI2dataset" , MINOR , TYPE_GENERAL ,
   "Output full datasets if needed" ,
   NULL
 } ,
 
 { 17 , DEC , 2007 , ZSS , "ConvertDset" , MINOR , TYPE_GENERAL ,
   "Output of full dsets if needed" ,
   "This can be used to force a dataset with data\n"
   "defined on a few nodes to be written out with a\n"
   "complete list of nodes, using 0 where no data are defined."
 } ,
 
 { 19 , NOV , 2007 , ZSS , "Surf2VolCoord" , MINOR , TYPE_NEW_PROG ,
   "Program to show surface-node to voxel correspondence" ,
   "This can be used to understand how surface coordinates\n"
   "relate to voxel coordinates."
 } ,

 { 26 , SEP , 2007 , ZSS , "SurfSmooth" , MAJOR , TYPE_MODIFY ,
   "Big changes to data smoothing functions" ,
   "* HEAT_07 method does controlled blurring, with options\n"
   "to blur 'to' a particular FWHM. No guessing needed for\n"
   "iterative kernel bandwidth or number of iterations.\n"
   "* HEAT_05 method improved to reduce numerical precision\n"
   "problems."
 } ,
 
 { 26 , SEP , 2007 , ZSS , "SurfFWHM" , SUPER , TYPE_NEW_PROG ,
   "Program to estimate FWHM of data on surface" ,
   NULL
 } ,
 
 { 10 , MAR , 2007 , ZSS , "MapIcosahedron" , MINOR , TYPE_MODIFY ,
   "Better handling of surface centers" ,
   NULL
 } ,
 
 { 15 , FEB , 2007 , ZSS , "suma" , MINOR , TYPE_NEW_OPT ,
   "High resolution image saving with ctrl+r" ,
   "Very high resolution images can be created.\n"
   "See suma's interactive help 'ctrl+h' for more info."
 } ,

 { 5 , FEB , 2007 , ZSS , "SurfDsetInfo" , MINOR , TYPE_NEW_PROG ,
   "Program to display surface dataset information" ,
   "Output is crude at the moment."
 } ,

 { 15 , JAN , 2007 , ZSS , "suma" , MAJOR , TYPE_GENERAL ,
   "Allow replacement of pre-loaded DO and Dsets" ,
   "When a dataset is reloaded, it replaces the one\n"
   "already loaded in suma."
 } ,
 
 { 5 , JAN , 2007 , ZSS , "imcat" , MINOR , TYPE_NEW_PROG ,
   "Program to stitch images." ,
   NULL
 } ,

 { 30 , NOV , 2006 , ZSS , "suma" , MINOR , TYPE_GENERAL ,
   "Addition of new Displayable Objects (DO)(ctrl+Alt+s)" ,
   "Allows display of segments, spheres and other markers\n"
   "See suma's interactive help 'ctrl+h' for more info."
 } ,
 
 { 20 , SEP , 2006 , ZSS , "DriveSuma" , MAJOR , TYPE_NEW_PROG ,
   "Program to control SUMA from the command line" ,
   NULL
 } ,

 { 31 , AUG , 2006 , ZSS , "AnalyzeTrace" , MICRO , TYPE_NEW_PROG ,
   "Program to analyze the output of -trace option." ,
   NULL
 } ,

 
 { 99,99,99, NULL,NULL, 99,99, NULL,NULL}  /** the end (do not delete) **/
} ;
