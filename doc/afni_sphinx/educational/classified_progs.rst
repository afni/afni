.. _edu_class_prog:

***********************
Classified Program list
***********************

All AFNI programs, great and small, are listed here and classified
based on functionality.  That is, they are grouped into some general
categories that we made up and given short bios.

The ranking of each program is to highlight ones that we think are
particularly useful in general processing ('5' being the most directly
useful, and '1' being something that might just be a low-level,
supplementary tool).  Note that a given program may appear in more
than one group.

This page might be most useful by using your browser to search through
the text for keywords of interest, such as "ROI", "mask", "diffusion",
"align", "model", etc.  Clicking on the name of the program will bring
you its online help documentation, referenced from :ref:`this page of
all AFNI "helps"<programs_main>`.

.. contents:: :local:

|



**Interactive viewer GUIs**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 5
     - :ref:`afni <ahelp_afni>`
     - GUI-based viewer for exploring data primarily as slices
   * - 5
     - :ref:`suma <ahelp_suma>`
     - GUI-based viewer for exploring data in 3D: surfaces, tracts, graph nodes, and volume slices
   * - 2
     - :ref:`aiv <ahelp_aiv>`
     - AFNI Image Viewer program


**Voxelwise calcs, esp. stats and tests**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 5
     - :ref:`3dttest++ <ahelp_3dttest++>`
     - Compute voxelwise t-tests (and GLMs) across collection of datasets (see 3dMEMA for generalizing to include within-subjects variance in model)
   * - 5
     - :ref:`3dTstat <ahelp_3dTstat>`
     - Compute voxelwise statistics of time series datasets (e.g., mean, variance)
   * - 5
     - :ref:`3dMVM <ahelp_3dMVM>`
     - Group-analysis program that performs traditional ANOVA- and ANCOVA style computations, and multivariate modeling
   * - 5
     - :ref:`3dLME <ahelp_3dLME>`
     - Group-analysis program that performs linear mixed-effects (LME) modeling analysis
   * - 5
     - :ref:`3dMEMA <ahelp_3dMEMA>`
     - Group-analysis program that performs Mixed Effects Meta Analysis, modeling both within- and across-subjects variability
   * - 4
     - :ref:`3dMean <ahelp_3dMean>`
     - Compute the average of a number of datasets
   * - 3
     - :ref:`3dGroupInCorr <ahelp_3dGroupInCorr>`
     - With a group of dsets, calc voxelwise t-tests on group of corr maps wherever user clicks
   * - 3
     - :ref:`3dTcorr1D <ahelp_3dTcorr1D>`
     - Correlation coefficient between 3D+time dataset and 1D time series
   * - 3
     - :ref:`3dTcorrMap <ahelp_3dTcorrMap>`
     - Compute average correlation of every voxel with every other
   * - 1
     - :ref:`3dSetupGroupInCorr <ahelp_3dSetupGroupInCorr>`
     - Preliminary program to run when using 3dGroupInCorr
   * - 1
     - :ref:`3dClustSim <ahelp_3dClustSim>`
     - 3Monte Carlo simulation for multiple comparison correction
   * - 1
     - :ref:`3dTcorrelate <ahelp_3dTcorrelate>`
     - Compute correlation between two 3D+time datasets
   * - 1
     - :ref:`3dTfitter <ahelp_3dTfitter>`
     - Fit a linear model to each voxel time series, with various methods (L1, L2, LASSO)
   * - 1
     - :ref:`3dTfilter <ahelp_3dTfilter>`
     - Apply a linear filter to each voxel time series (archaic)
   * - 1
     - :ref:`3dTsort <ahelp_3dTsort>`
     - Sort each voxel’s time series in various ways
   * - 1
     - :ref:`3dRank <ahelp_3dRank>`
     - 
   * - 1
     - :ref:`3dRankizer <ahelp_3dRankizer>`
     - 
   * - 1
     - :ref:`3dANOVA <ahelp_3dANOVA>`
     - 1-way ANOVA (fixed effects)
   * - 1
     - :ref:`3dANOVA2 <ahelp_3dANOVA2>`
     - 2-way ANOVA (fixed, random, mixed effects)
   * - 1
     - :ref:`3dANOVA3 <ahelp_3dANOVA3>`
     - 3-way ANOVA (fixed, random, mixed effects)
   * - 1
     - :ref:`3dExtractGroupInCorr <ahelp_3dExtractGroupInCorr>`
     - 
   * - 1
     - :ref:`3dFDR <ahelp_3dFDR>`
     - False Discovery Rate analysis
   * - 1
     - :ref:`AlphaSim <ahelp_AlphaSim>`
     - (obsolete -> use 3dClustSim)
   * - 1
     - :ref:`1dSEM <ahelp_1dSEM>`
     - Structural equation modeling (path analysis)  (why here???)
   * - 1
     - :ref:`3dWilcoxon <ahelp_3dWilcoxon>`
     - Nonparametric Wilcoxon test
   * - 1
     - :ref:`3dKruskalWallis <ahelp_3dKruskalWallis>`
     - Nonparametric Kruskal-Wallis test
   * - 1
     - :ref:`3dFriedman <ahelp_3dFriedman>`
     - Nonparametric Friedman test
   * - 1
     - :ref:`3dMannWhitney <ahelp_3dMannWhitney>`
     - Nonparametric 3dMannWhitney test
   * - 1
     - :ref:`3dRegAna <ahelp_3dRegAna>`
     - Voxel-wise linear regression analyses
   * - 1
     - :ref:`3dttest <ahelp_3dttest>`
     - (obsolete -> use 3dttest++)
   * - 1
     - :ref:`3dPval <ahelp_3dPval>`
     - Convert each statistical value in a dataset (e.g., t-statistic) to a p-value
   * - 1
     - :ref:`3dNormalityTest <ahelp_3dNormalityTest>`
     - Tests the input values at each voxel for normality, using the Anderson-Darling method


**Get info/stats within ROIs**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 5
     - :ref:`whereami <ahelp_whereami>`
     - Get atlas region name for coordinates
   * - 4
     - :ref:`3dROIstats <ahelp_3dROIstats>`
     - Calculate dataset values from multiple ROIs
   * - 4
     - :ref:`3dmaskave <ahelp_3dmaskave>`
     - Calculate dataset values averaged over a ROI
   * - 4
     - :ref:`3dBrickStat <ahelp_3dBrickStat>`
     - Calculate percentile values within dsets
   * - 3
     - :ref:`3dUndump <ahelp_3dUndump>`
     - Create dataset from text (inverse of 3dmaskdump)
   * - 1
     - :ref:`3dmaskdump <ahelp_3dmaskdump>`
     - Output all dataset values in a ROI
   * - 1
     - :ref:`3dMax <ahelp_3dMax>`
     - (obsolete -> use 3dBrickStat)
   * - 1
     - :ref:`3dOverlap <ahelp_3dOverlap>`
     - Create mask that is overlap of nonzero voxels from multiple datasets
   * - 1
     - :ref:`3dfractionize <ahelp_3dfractionize>`
     - Resample a mask dataset to a different resolution
   * - 1
     - :ref:`3dTto1D <ahelp_3dTto1D>`
     - Collapse 4D data to 1D in various ways
   * - 1
     - :ref:`@Atlasize <ahelp_@Atlasize>`
     - Make an atlas from a dataset and label text files
   * - 1
     - :ref:`@MakeLabelTable <ahelp_@MakeLabelTable>`
     - Assign labels to values
   * - 1
     - :ref:`AFNI_atlas_spaces.niml <ahelp_AFNI_atlas_spaces.niml>`
     - space, atlas, transformations definitions file


**Build FMRI pipelines**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 5
     - :ref:`afni_proc.py <ahelp_afni_proc.py>`
     - Generate tcsh script for processing single subject FMRI data
   * - 4
     - :ref:`gen_ss_review_scripts.py <ahelp_gen_ss_review_scripts.py>`
     - Generate QC review scripts
   * - 1
     - :ref:`uber_proc.py <ahelp_uber_proc.py>`
     - 
   * - 1
     - :ref:`uber_subject.py <ahelp_uber_subject.py>`
     - Graphical interface to help set up an afni_proc.py command
   * - 1
     - :ref:`gen_group_command.py <ahelp_gen_group_command.py>`
     - Generate group analysis scripts
   * - 1
     - :ref:`afni_restproc.py <ahelp_afni_restproc.py>`
     - (obsolete -> use afni_proc.py)


**Align/register/warp/axialize spatially**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 5
     - :ref:`3dvolreg <ahelp_3dvolreg>`
     - Volumetric registration (rigid body in 3D, 6DOF linear)
   * - 5
     - :ref:`align_epi_anat.py <ahelp_align_epi_anat.py>`
     - Align 2 volumes (e.g. anat and EPI)
   * - 5
     - :ref:`3dQwarp <ahelp_3dQwarp>`
     - Align two datasets using nonlinear warping (relatedly, see @SSwarper, auto_warp.py, @toMNI_Qwarpar)
   * - 5
     - :ref:`3dAllineate <ahelp_3dAllineate>`
     - Cross-modality affine volume registration
   * - 5
     - :ref:`3dresample <ahelp_3dresample>`
     - Rewrite dataset in new orientation, with new voxel size
   * - 4
     - :ref:`fat_proc_align_anat_pair <ahelp_fat_proc_align_anat_pair>`
     - Align a T1w dset to a T2w dset, esp. as part of DWI processing and if incorporating FreeSurfer after
   * - 4
     - :ref:`fat_proc_axialize_anat <ahelp_fat_proc_axialize_anat>`
     - Attempt to align major viewing planes of anatomical with FOV, based on WB alignment to a reference vol
   * - 4
     - :ref:`3dNwarpApply <ahelp_3dNwarpApply>`
     - Apply a nonlinear warp to transform a dataset
   * - 3
     - :ref:`@Align_Centers <ahelp_@Align_Centers>`
     - Align the center of a dataset to another
   * - 3
     - :ref:`@AddEdge <ahelp_@AddEdge>`
     - Show two or more datasets with edges for alignment visualization
   * - 1
     - :ref:`auto_warp.py <ahelp_auto_warp.py>`
     - wrapper for nonlinear warping with 3dQwarp
   * - 1
     - :ref:`@auto_tlrc <ahelp_@auto_tlrc>`
     - Automatic transformation of dataset to match Talairach template (rigid/12dof???)
   * - 1
     - :ref:`afni_proc.py <ahelp_afni_proc.py>`
     - Can wrap many registration operations
   * - 1
     - :ref:`3dWarp <ahelp_3dWarp>`
     - Non-rigid transformation of 3D coordinates
   * - 1
     - :ref:`3dWarpDrive <ahelp_3dWarpDrive>`
     - Volumetric registration, includes warping (12DOF, linear affine); prob use 3dAllineate or align_epi_anat.py (???)
   * - 1
     - :ref:`@align_partial_oblique <ahelp_@align_partial_oblique>`
     - Align (non-oblique) full- and partial-coverage T1w datasets; consider 3dQwarp instead.
   * - 1
     - :ref:`@auto_align <ahelp_@auto_align>`
     - (obsolete -> use align_epi_anat.py)
   * - 1
     - :ref:`@SSwarper <ahelp_@SSwarper>`
     - Skull-stripping program that uses a reference anatomical
   * - 1
     - :ref:`@SUMA_AlignToExperiment <ahelp_@SUMA_AlignToExperiment>`
     - Align volume from FreeSurfer analysis to a different session's anatomical volume in order to warp surfaces similarly
   * - 1
     - :ref:`3dNwarpAdjust <ahelp_3dNwarpAdjust>`
     - Adjust a collection of nonlinear warps for template building (@toMNI_Qwarpar)
   * - 1
     - :ref:`3dNwarpCalc <ahelp_3dNwarpCalc>`
     - Carry out calculations on nonlinear warps
   * - 1
     - :ref:`3dNwarpCat <ahelp_3dNwarpCat>`
     - Combine linear and nonlinear warps (spatial transformations)
   * - 1
     - :ref:`3dNwarpFuncs <ahelp_3dNwarpFuncs>`
     - Compute various voxelwise information about a nonlinear warp (e.g., Jacobian)
   * - 1
     - :ref:`3dNwarpXYZ <ahelp_3dNwarpXYZ>`
     - Apply a nonlinear warp to a set of (x,y,z) triples
   * - 1
     - :ref:`3dTagalign <ahelp_3dTagalign>`
     - Align datasets by matching manually placed 'tags'
   * - 1
     - :ref:`plugin(Edit Tagset) <ahelp_plugin(Edit Tagset)>`
     - Place 'tags' in a dataset interactively
   * - 1
     - :ref:`3drotate <ahelp_3drotate>`
     - Rigid body rotation of dataset in 3D
   * - 1
     - :ref:`3dAnatNudge <ahelp_3dAnatNudge>`
     - (obsolete -> use align_epi_anat.py); try to align EPI and structural volumes automatically
   * - 1
     - :ref:`cat_matvec <ahelp_cat_matvec>`
     - Utility for combining linear affine transformation matrices (e.g., from 3dAllineate)
   * - 1
     - :ref:`adwarp <ahelp_adwarp>`
     - Transform dataset using warp from dataset header
   * - 1
     - :ref:`Vecwarp <ahelp_Vecwarp>`
     - Transform 3-vectors using warp from dataset header
   * - 1
     - :ref:`2dImReg <ahelp_2dImReg>`
     - Slice-by-slice registration (rigid body in 2D)
   * - 1
     - :ref:`3daxialize <ahelp_3daxialize>`
     - (obsolete -> use 3dresample)
   * - 1
     - :ref:`lpc_align.py <ahelp_lpc_align.py>`
     - (obsolete -> use align_epi_anat.py)
   * - 1
     - :ref:`@toMNI_Awarp <ahelp_@toMNI_Awarp>`
     - Make a group template - affine alignment
   * - 1
     - :ref:`@toMNI_Qwarpar <ahelp_@toMNI_Qwarpar>`
     - Make a group template - iterative nonlinear alignment
   * - 1
     - :ref:`uber_align_test.py <ahelp_uber_align_test.py>`
     - GUI for affine alignment with align_epi_anat.py
   * - 1
     - :ref:`unWarpEPI.py <ahelp_unWarpEPI.py>`
     - Blip-up/down unwarping nonlinear alignment
   * - 1
     - :ref:`@Shift_Volume <ahelp_@Shift_Volume>`
     - Move origin of dataset by specified amount or shift between MNI and MNI_ANAT


**SUMA surface calculations, formats and viewing**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 5
     - :ref:`@SUMA_Make_Spec_FS <ahelp_@SUMA_Make_Spec_FS>`
     - Convert Freesurfer surfaces to SUMA spec files
   * - 4
     - :ref:`IsoSurface <ahelp_IsoSurface>`
     - Extract isosurface from a volume
   * - 4
     - :ref:`3dSurf2Vol <ahelp_3dSurf2Vol>`
     - Compute volume equivalent from surface or pair of surfaces
   * - 4
     - :ref:`3dVol2Surf <ahelp_3dVol2Surf>`
     - Assign values to surface nodes from volumetric data
   * - 4
     - :ref:`DriveSuma <ahelp_DriveSuma>`
     - Send commands to SUMA program from script
   * - 3
     - :ref:`SurfaceMetrics <ahelp_SurfaceMetrics>`
     - Provides information on surface mesh
   * - 3
     - :ref:`SurfMeasures <ahelp_SurfMeasures>`
     - Compute various measurements for surface or pair of surfaces
   * - 1
     - :ref:`Surf2VolCoord <ahelp_Surf2VolCoord>`
     - 
   * - 1
     - :ref:`SurfClust <ahelp_SurfClust>`
     - Find clusters on surfaces
   * - 1
     - :ref:`SurfDist <ahelp_SurfDist>`
     - Output shortest distance between two nodes on a surface (along surface or Euclidean)
   * - 1
     - :ref:`SurfDsetInfo <ahelp_SurfDsetInfo>`
     - Display information about surface dataset
   * - 1
     - :ref:`SurfExtrema <ahelp_SurfExtrema>`
     - Find local extrema in a (surface) dataset
   * - 1
     - :ref:`SurfFWHM <ahelp_SurfFWHM>`
     - 
   * - 1
     - :ref:`SurfInfo <ahelp_SurfInfo>`
     - Show information on surface
   * - 1
     - :ref:`SurfMesh <ahelp_SurfMesh>`
     - Reduce number of points in surface mesh
   * - 1
     - :ref:`SurfPatch <ahelp_SurfPatch>`
     - Extract patch of surface or compute volume from specified nodes
   * - 1
     - :ref:`SurfQual <ahelp_SurfQual>`
     - Quality check for surfaces
   * - 1
     - :ref:`SurfRetinoMap <ahelp_SurfRetinoMap>`
     - 
   * - 1
     - :ref:`SurfSmooth <ahelp_SurfSmooth>`
     - Smooth surfaces
   * - 1
     - :ref:`@SurfSmooth.HEAT_07.examples <ahelp_@SurfSmooth.HEAT_07.examples>`
     - 
   * - 1
     - :ref:`SurfToSurf <ahelp_SurfToSurf>`
     - Interpolate data from one surface onto mesh of another surface
   * - 1
     - :ref:`suma_change_spec <ahelp_suma_change_spec>`
     - 
   * - 1
     - :ref:`SUMA_glxdino <ahelp_SUMA_glxdino>`
     - 
   * - 1
     - :ref:`SUMA_paperplane <ahelp_SUMA_paperplane>`
     - 
   * - 1
     - :ref:`SUMA_pixmap2eps <ahelp_SUMA_pixmap2eps>`
     - 
   * - 1
     - :ref:`quickspec <ahelp_quickspec>`
     - Generate (basic) specification file for running suma
   * - 1
     - :ref:`ROI2dataset <ahelp_ROI2dataset>`
     - Convert ROI (e.g., after drawing) to SUMA-type dset
   * - 1
     - :ref:`3dSurfMask <ahelp_3dSurfMask>`
     - Generate volumetric mask for inside of surface
   * - 1
     - :ref:`ConvertDset <ahelp_ConvertDset>`
     - Converts a surface dataset from one format to another
   * - 1
     - :ref:`ConvertSurface <ahelp_ConvertSurface>`
     - Convert surface files among various formats
   * - 1
     - :ref:`CompareSurfaces <ahelp_CompareSurfaces>`
     - Compute distances between two surfaces at each node
   * - 1
     - :ref:`CreateIcosahedron <ahelp_CreateIcosahedron>`
     - 
   * - 1
     - :ref:`MapIcosahedron <ahelp_MapIcosahedron>`
     - Create new version of surface mesh using mesh of icosahedron
   * - 1
     - :ref:`@IsoMasks <ahelp_@IsoMasks>`
     - 
   * - 1
     - :ref:`@SUMA_Make_Spec_SF <ahelp_@SUMA_Make_Spec_SF>`
     - Convert SureFit surfaces to SUMA spec files
   * - 1
     - :ref:`MakeColorMap <ahelp_MakeColorMap>`
     - Make afni and suma colormaps


**Mask/skull-strip/segment**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 5
     - :ref:`3dAutomask <ahelp_3dAutomask>`
     - Generate a brain and skull-only mask
   * - 5
     - :ref:`3dSkullStrip <ahelp_3dSkullStrip>`
     - Enhanced skull stripping
   * - 4
     - :ref:`3dmask_tool <ahelp_3dmask_tool>`
     - for combining/dilating/eroding/filling masks
   * - 3
     - :ref:`@NoisySkullStrip <ahelp_@NoisySkullStrip>`
     - Strips the skull of anatomical datasets with low SNR
   * - 3
     - :ref:`3dSeg <ahelp_3dSeg>`
     - Segment anatomical (t1w) volume into major brain tissue types
   * - 1
     - :ref:`plugin(Draw Dataset) <ahelp_plugin(Draw Dataset)>`
     - Manually draw ROI mask datasets
   * - 1
     - :ref:`3dinfill <ahelp_3dinfill>`
     - Edit masks by filling in holes
   * - 1
     - :ref:`3dIntracranial <ahelp_3dIntracranial>`
     - Strip off outside-the-brain voxels
   * - 1
     - :ref:`plugin(Gyrus Finder) <ahelp_plugin(Gyrus Finder)>`
     - Interactively segment gray and white matter
   * - 1
     - :ref:`3dClipLevel <ahelp_3dClipLevel>`
     - Find value to threshold off outside-the-brain voxels


**Make/edit/evaluate stimulus timing files**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 4
     - :ref:`make_random_timing.py <ahelp_make_random_timing.py>`
     - Generate random stimulus times files
   * - 4
     - :ref:`timing_tool.py <ahelp_timing_tool.py>`
     - Edit stimulus timing files
   * - 1
     - :ref:`1dMarry <ahelp_1dMarry>`
     - Combine ragged 1D files for use with 3dDeconvolve's -stim_times_AM2 option
   * - 1
     - :ref:`make_stim_times.py <ahelp_make_stim_times.py>`
     - Convert 0/1 stim file format to stim times format
   * - 1
     - :ref:`RSFgen <ahelp_RSFgen>`
     - (obsolete -> use make_random_timing.py)
   * - 1
     - :ref:`@make_stim_file <ahelp_@make_stim_file>`
     - (obsolete/esoteric/do not use; use what???); make stim files for 3dDeconvolve
   * - 1
     - :ref:`stimband <ahelp_stimband>`
     - 


**Edit dset headers**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 5
     - :ref:`3dinfo <ahelp_3dinfo>`
     - Print out information from the header
   * - 5
     - :ref:`nifti_tool <ahelp_nifti_tool>`
     - Displays, modifies, copies nifti structures in datasets
   * - 4
     - :ref:`3drefit <ahelp_3drefit>`
     - Lets you change attributes in a dataset header
   * - 1
     - :ref:`3dAttribute <ahelp_3dAttribute>`
     - Print out a single header attribute
   * - 1
     - :ref:`3dnvals <ahelp_3dnvals>`
     - Print out the number of sub-bricks (3D volumes) in a dataset
   * - 1
     - :ref:`3dnewid <ahelp_3dnewid>`
     - Assign a new ID code to a dataset (also, generate a random string for filenames)
   * - 1
     - :ref:`3dNotes <ahelp_3dNotes>`
     - Lets you put text notes into a dataset header
   * - 1
     - :ref:`plugin(Dataset NOTES) <ahelp_plugin(Dataset NOTES)>`
     - Interactive header notes editor
   * - 1
     - :ref:`gifti_tool <ahelp_gifti_tool>`
     - Displays, modifies, copies nifti structures in datasets
   * - 1
     - :ref:`cifti_tool <ahelp_cifti_tool>`
     - Displays, modifies, copies nifti structures in datasets
   * - 1
     - :ref:`nifti1_tool <ahelp_nifti1_tool>`
     - (how diff than nifti_tool???)
   * - 1
     - :ref:`3dCM <ahelp_3dCM>`
     - Estimate dset's center of mass, and allow recentering
   * - 1
     - :ref:`@AfniOrient2RAImap <ahelp_@AfniOrient2RAImap>`
     - Convert orientation code into signed code used in AFNI header
   * - 1
     - :ref:`@AfniOrientSign <ahelp_@AfniOrientSign>`
     - Convert orientation code into signed +/-1 code relative to RAI and permutations
   * - 1
     - :ref:`@FromRAI <ahelp_@FromRAI>`
     - Convert RAI coordinates into another coordinate order
   * - 1
     - :ref:`@ToRAI <ahelp_@ToRAI>`
     - Convert coordinates to RAI order
   * - 1
     - :ref:`@FullPath <ahelp_@FullPath>`
     - Get absolute path of a file
   * - 1
     - :ref:`@GetAfniBin <ahelp_@GetAfniBin>`
     - Returns path of afni executables
   * - 1
     - :ref:`@GetAfniDims <ahelp_@GetAfniDims>`
     - Get dimensions of dataset
   * - 1
     - :ref:`@GetAfniID <ahelp_@GetAfniID>`
     - Get AFNI ID of dataset
   * - 1
     - :ref:`@GetAfniOrient <ahelp_@GetAfniOrient>`
     - Get orientation code of dataset
   * - 1
     - :ref:`@GetAfniPrefix <ahelp_@GetAfniPrefix>`
     - Get prefix part of dataset name
   * - 1
     - :ref:`@GetAfniRes <ahelp_@GetAfniRes>`
     - Get voxel resolution of dataset
   * - 1
     - :ref:`@GetAfniView <ahelp_@GetAfniView>`
     - Get afni view equivalent of dataset (+orig,+tlrc)
   * - 1
     - :ref:`@parse_name <ahelp_@parse_name>`
     - Return parts of an AFNI or NIFTI dataset name
   * - 1
     - :ref:`@parse_afni_name <ahelp_@parse_afni_name>`
     - Return parts of an AFNI dataset name
   * - 1
     - :ref:`ParseName <ahelp_ParseName>`
     - Return parts of a dataset name including AFNI specifiers
   * - 1
     - :ref:`@FindAfniDsetPath <ahelp_@FindAfniDsetPath>`
     - Find a path to dataset
   * - 1
     - :ref:`@isOblique <ahelp_@isOblique>`
     - Flag if dataset is marked as oblique
   * - 1
     - :ref:`@Shift_Volume <ahelp_@Shift_Volume>`
     - Move origin of dataset by specified amount or shift between MNI and MNI_ANAT


**Compute various numbers from datasets**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 5
     - :ref:`3dFWHMx <ahelp_3dFWHMx>`
     - Estimate FWHM for all sub-bricks of dataset
   * - 3
     - :ref:`3dBrickStat <ahelp_3dBrickStat>`
     - Simple statistics (max, min, mean) for scripts
   * - 2
     - :ref:`3dExtrema <ahelp_3dExtrema>`
     - Find local maxima (or minima) of datasets
   * - 1
     - :ref:`3ddot <ahelp_3ddot>`
     - Dot product (correlation coefficient) of 2 sub-bricks
   * - 1
     - :ref:`3dStatClust <ahelp_3dStatClust>`
     - Find statistically connected clusters
   * - 1
     - :ref:`3dGetrow <ahelp_3dGetrow>`
     - Output voxel values for a row/column in x,y,z space
   * - 1
     - :ref:`3dFWHM <ahelp_3dFWHM>`
     - (obsolete -> use 3dFWHMx)


**Blur and smooth dsets**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 5
     - :ref:`3dmerge <ahelp_3dmerge>`
     - Process (e.g., blur) and optionally combine datasets
   * - 1
     - :ref:`3dBlurInMask <ahelp_3dBlurInMask>`
     - Blur a dataset, but only inside a mask (or masks)
   * - 1
     - :ref:`3dBlurToFWHM <ahelp_3dBlurToFWHM>`
     - Blur a dataset to a given level of smoothness (for inter-site studies)
   * - 1
     - :ref:`3danisosmooth <ahelp_3danisosmooth>`
     - Anisotropic blurring of a dataset (e.g., to clean up structural images)
   * - 1
     - :ref:`3dMedianFilter <ahelp_3dMedianFilter>`
     - Smooth a 3D volume using a median filter


**Volume editing/image processing**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 4
     - :ref:`3dedge3 <ahelp_3dedge3>`
     - Calculate edges in 3D
   * - 4
     - :ref:`3danisosmooth <ahelp_3danisosmooth>`
     - Smooth a dataset using an anisotropic technique to preserve edges
   * - 4
     - :ref:`3dUnifize <ahelp_3dUnifize>`
     - Correct T1-weighted dataset for non-uniform histogram
   * - 2
     - :ref:`3dSharpen <ahelp_3dSharpen>`
     - 3D sharpening filter applied to a dataset (to clean up a template)
   * - 1
     - :ref:`3dUniformize <ahelp_3dUniformize>`
     - (obsolete -> use 3dUnifize)


**Update AFNI, install software (not demos)**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 5
     - :ref:`@update.afni.binaries <ahelp_@update.afni.binaries>`
     - Update current AFNI binaries
   * - 5
     - :ref:`afni_system_check.py <ahelp_afni_system_check.py>`
     - Evaluate present setup
   * - 4
     - :ref:`rPkgsInstall <ahelp_rPkgsInstall>`
     - Get+install all necessary R packages
   * - 1
     - :ref:`@UpdateAfni <ahelp_@UpdateAfni>`
     - (obsolete -> use @update.afni.binaries)
   * - 1
     - :ref:`@get.afni.version <ahelp_@get.afni.version>`
     - Download an archived version of AFNI source code using github
   * - 1
     - :ref:`afni_vcheck <ahelp_afni_vcheck>`
     - Check if update needed (compare present and available version numbers)


**Simple dset calcs (-> make new dsets)**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 5
     - :ref:`3dcalc <ahelp_3dcalc>`
     - Voxel-by-voxel general purpose calculator
   * - 5
     - :ref:`3dmerge <ahelp_3dmerge>`
     - Various spatial filters, thresholds, and averaging
   * - 5
     - :ref:`3dTstat <ahelp_3dTstat>`
     - Various statistics of multi-brick datasets, voxel-by-voxel
   * - 4
     - :ref:`3dMean <ahelp_3dMean>`
     - Average datasets together, voxel-by-voxel, for each timept
   * - 4
     - :ref:`3danisosmooth <ahelp_3danisosmooth>`
     - Edge preserving filter for spatial smoothing
   * - 1
     - :ref:`3dWinsor <ahelp_3dWinsor>`
     - Nonlinear order statistics filter for spatial smoothing
   * - 1
     - :ref:`3dLocalstat <ahelp_3dLocalstat>`
     - Find simple statistical values for neighborhoods around each voxel
   * - 1
     - :ref:`3dLocalBistat <ahelp_3dLocalBistat>`
     - Compute various bivariate statistics for neighborhoods around each voxel
   * - 1
     - :ref:`3dLocalstat <ahelp_3dLocalstat>`
     - Compute some local statistics in a neighborhood around each voxel
   * - 1
     - :ref:`3dLocalACF <ahelp_3dLocalACF>`
     - Compute mixed model ACF parameters in a neighborhood around each voxel
   * - 1
     - :ref:`3dLocalPV <ahelp_3dLocalPV>`
     - Compute the 'principal vector' from a time series dataset, in a neighborhood around each voxel
   * - 1
     - :ref:`3dLocalSVD <ahelp_3dLocalSVD>`
     - Compute the SVD from a time series dataset, in a neighborhood around each voxel
   * - 1
     - :ref:`3dLocalHistog <ahelp_3dLocalHistog>`
     - Compute the count of how many times each unique value occurs, in a neighborhood around each voxel
   * - 1
     - :ref:`3dTto1D <ahelp_3dTto1D>`
     - Collapse 4D data to 1D in various ways
   * - 1
     - :ref:`3dmatcalc <ahelp_3dmatcalc>`
     - Applies matrix to datasets
   * - 1
     - :ref:`3dmatmult <ahelp_3dmatmult>`
     - Multiply datasets as matrices


**Resting state FMRI parameters**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 3
     - :ref:`3dRSFC <ahelp_3dRSFC>`
     - Calculate RSFC parameters (ALFF, fALFF, RSFA, etc.) for uncensored time series
   * - 3
     - :ref:`3dReHo <ahelp_3dReHo>`
     - Calculate ReHo (Kendall's coefficient of concordance) for time series
   * - 3
     - :ref:`3dLombScargle <ahelp_3dLombScargle>`
     - Calculate amp/pow spectrum (like FFT) along time axis with missing time points
   * - 3
     - :ref:`3dAmpToRSFC <ahelp_3dAmpToRSFC>`
     - Calculate RSFC parameters (ALFF, fALFF, RSFA, etc.) from 3dLombScargle output


**Make/edit correlation matrices**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 4
     - :ref:`3dNetCorr <ahelp_3dNetCorr>`
     - Calculate correlation matrix of a set of ROIs, as well as WB maps of each
   * - 1
     - :ref:`@ROI_Corr_Mat <ahelp_@ROI_Corr_Mat>`
     - Make an NxN ROI correlation matrix of N ROIs (consider 3dNetCorr instead)
   * - 1
     - :ref:`fat_mat_sel.py <ahelp_fat_mat_sel.py>`
     - Visualize functional correlation (*.netcc files) or tracted-WM property (*.grid file) matrices
   * - 1
     - :ref:`3dErrtsCormat <ahelp_3dErrtsCormat>`
     - Compute the correlation matrix for the residual (or error) time series in a dataset


**Make/edit ROIs and clusters, resample**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 5
     - :ref:`3dresample <ahelp_3dresample>`
     - Rewrite dataset, possibly in new orientation, with new voxel size
   * - 5
     - :ref:`3dclust <ahelp_3dclust>`
     - Find clusters of voxels in a dataset and print out a table about the clusters
   * - 4
     - :ref:`3dmerge <ahelp_3dmerge>`
     - Edit datasets (e.g., blur, cluster), and optionally combine them
   * - 3
     - :ref:`3dUndump <ahelp_3dUndump>`
     - Create a 3D dataset from text data
   * - 3
     - :ref:`3dROIMaker <ahelp_3dROIMaker>`
     - Threshold and clusterize dataset, as well as inflate (esp. for tractography prep)
   * - 2
     - :ref:`3dfractionize <ahelp_3dfractionize>`
     - Resample a mask to a different grid size
   * - 1
     - :ref:`3dExtrema <ahelp_3dExtrema>`
     - Find local extrema within volumes
   * - 1
     - :ref:`3dmaxima <ahelp_3dmaxima>`
     - Find local extrema within volumes
   * - 1
     - :ref:`3dClustCount <ahelp_3dClustCount>`
     - 


**Edit dsets: concatenate, split, add/remove slices**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 3
     - :ref:`3dZeropad <ahelp_3dZeropad>`
     - Add zero slices around the edges of a dataset
   * - 3
     - :ref:`3dZcat <ahelp_3dZcat>`
     - Assemble a 3D+time dataset from multiple input sub-bricks
   * - 3
     - :ref:`3dZcutup <ahelp_3dZcutup>`
     - Cut slices out of a dataset to make a 'thinner' dataset
   * - 1
     - :ref:`3dbucket <ahelp_3dbucket>`
     - Assemble a bucket dataset from multiple input sub-bricks3dTcat
   * - 1
     - :ref:`3dTsplit4D <ahelp_3dTsplit4D>`
     - Convert a 3D+time dataset into multiple 3D single-brick files
   * - 1
     - :ref:`3dAutobox <ahelp_3dAutobox>`
     - Automatically crop a dataset to remove empty space
   * - 1
     - :ref:`3dXYZcat <ahelp_3dXYZcat>`
     - Glue multiple sub-bricks together along the {x|y|z}-axis
   * - 1
     - :ref:`3dZregrid <ahelp_3dZregrid>`
     - Interpolate a dataset to a different slice thickness


**Drive AFNI/SUMA, make images/snapshots/montages**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 5
     - :ref:`@chauffeur_afni <ahelp_@chauffeur_afni>`
     - Wrapper to combine environment+driving functionality to save image files of 3D dataset (nice in conjunction with imcat to form arrays of images)
   * - 5
     - :ref:`imcat <ahelp_imcat>`
     - Very useful program for making grids of images and things (nice in conjunction with @chauffeur_afni or generally driving AFNI)
   * - 5
     - :ref:`@snapshot_volreg <ahelp_@snapshot_volreg>`
     - Drive AFNI to save QC images of EPI-anatomical alignment
   * - 4
     - :ref:`DriveSuma <ahelp_DriveSuma>`
     - Drive suma from external program
   * - 3
     - :ref:`@Quiet_Talkers <ahelp_@Quiet_Talkers>`
     - Close all network talking afni and suma instances (often used at end of "talking" scripts)
   * - 3
     - :ref:`@djunct_4d_imager <ahelp_@djunct_4d_imager>`
     - Wrapper to combine environment+driving functionality to save image/movies files of 4D dataset
   * - 3
     - :ref:`plugout_drive <ahelp_plugout_drive>`
     - Drive afni GUI from external program
   * - 2
     - :ref:`@DriveAfni <ahelp_@DriveAfni>`
     - Example script to drive afni GUI with class data
   * - 2
     - :ref:`@DriveSuma <ahelp_@DriveSuma>`
     - Example script to drive suma with class data
   * - 1
     - :ref:`HalloSuma <ahelp_HalloSuma>`
     - 
   * - 1
     - :ref:`@djunct_calc_mont_dims.py <ahelp_@djunct_calc_mont_dims.py>`
     - Sub-functionality of @djunct_dwi_selector.bash
   * - 1
     - :ref:`@djunct_dwi_selector.bash <ahelp_@djunct_dwi_selector.bash>`
     - Helper/intermediate function for fat_proc_select_vols
   * - 1
     - :ref:`@djunct_select_str.py <ahelp_@djunct_select_str.py>`
     - Sub-functionality of @djunct_dwi_selector.bash
   * - 1
     - :ref:`plugout_ijk <ahelp_plugout_ijk>`
     - 
   * - 1
     - :ref:`plugout_tt <ahelp_plugout_tt>`
     - 
   * - 1
     - :ref:`plugout_tta <ahelp_plugout_tta>`
     - 
   * - 1
     - :ref:`@snapshot_volreg3 <ahelp_@snapshot_volreg3>`
     - (obsolete -> use @snapshot_volreg)
   * - 1
     - :ref:`@CommandGlobb <ahelp_@CommandGlobb>`
     - Execute AFNI commands for multiple datasets
   * - 1
     - :ref:`prompt_popup <ahelp_prompt_popup>`
     - Popup a dialog box with a message and buttons
   * - 1
     - :ref:`prompt_user <ahelp_prompt_user>`
     - (obsolete -> use prompt_popup)
   * - 1
     - :ref:`@AfniEnv <ahelp_@AfniEnv>`
     - Get and set AFNI environment variables


**Deal with 1D time series**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 4
     - :ref:`1d_tool.py <ahelp_1d_tool.py>`
     - Perform various manipulations of 1D data
   * - 4
     - :ref:`1dplot <ahelp_1dplot>`
     - Graph values from columns in a file
   * - 3
     - :ref:`1dtranspose <ahelp_1dtranspose>`
     - Transpose 1D files (interchange rows and columns)
   * - 1
     - :ref:`1dCorrelate <ahelp_1dCorrelate>`
     - Calculate correlation coefficients between 1D columns, with confidence intervals
   * - 1
     - :ref:`1deval <ahelp_1deval>`
     - 1D calculator (like 3dcalc for 1D files)
   * - 1
     - :ref:`1dcat <ahelp_1dcat>`
     - Catenate 1D files horizontally (use system program cat for vertical combining)
   * - 1
     - :ref:`1dgrayplot <ahelp_1dgrayplot>`
     - Show values from columns in a file as bands of gray levels
   * - 1
     - :ref:`1dmatcalc <ahelp_1dmatcalc>`
     - Matrix calculator for 1D files
   * - 1
     - :ref:`1dsum <ahelp_1dsum>`
     - Add up all numbers in columns of a 1D file (can also do means)
   * - 1
     - :ref:`1dTsort <ahelp_1dTsort>`
     - Sort each column of the input 1D file (separately)
   * - 1
     - :ref:`1dsvd <ahelp_1dsvd>`
     - Compute the Singular Value Decomposition of a matrix (including PCA)
   * - 1
     - :ref:`1dUpsample <ahelp_1dUpsample>`
     - Interpolate columns of a 1D file to a finer grid
   * - 1
     - :ref:`column_cat <ahelp_column_cat>`
     - Catenate data horizontally


**DWI/DTI/diffusion-related**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 5
     - :ref:`3dDWItoDT <ahelp_3dDWItoDT>`
     - Estimate diffusion tensor and parameters from DWIs (and see fat_proc_dwi_to_dt)
   * - 5
     - :ref:`3dTrackID <ahelp_3dTrackID>`
     - Perform deterministic, mini- or fully-probabilistic tracking for DTI or HARDI data
   * - 5
     - :ref:`fat_proc_convert_dcm_anat <ahelp_fat_proc_convert_dcm_anat>`
     - Wrapper to convert 3D dataset from DICOMs, with additional nice features.
   * - 5
     - :ref:`fat_proc_convert_dcm_dwis <ahelp_fat_proc_convert_dcm_dwis>`
     - Wrapper to convert 4D dataset from DICOMs, with additional nice features.
   * - 5
     - :ref:`fat_proc_decmap <ahelp_fat_proc_decmap>`
     - Make a directionally-encoded color map of DTI data.
   * - 5
     - :ref:`fat_proc_dwi_to_dt <ahelp_fat_proc_dwi_to_dt>`
     - Wrapper to estimate DT and parameters, and align datasets.
   * - 5
     - :ref:`fat_proc_filter_dwis <ahelp_fat_proc_filter_dwis>`
     - Graphical interface to help user select out bad volumes (esp. from DWI dataset)
   * - 5
     - :ref:`fat_proc_grad_plot <ahelp_fat_proc_grad_plot>`
     - In progress...
   * - 5
     - :ref:`fat_proc_imit2w_from_t1w <ahelp_fat_proc_imit2w_from_t1w>`
     - Invert a T1w dataset to imitate a T2w-type contrast dset (as a backup ref for DWI processing with TORTOISE)
   * - 5
     - :ref:`fat_proc_map_to_dti <ahelp_fat_proc_map_to_dti>`
     - Wrapper to bring data (esp. FS surfaces and parcels) into DTI space
   * - 5
     - :ref:`fat_proc_select_vols <ahelp_fat_proc_select_vols>`
     - Select out good valumes in a DWI dataset + associated text files
   * - 5
     - :ref:`@GradFlipTest <ahelp_@GradFlipTest>`
     - Test what 'flip', if any, is necessary for gradients in a DWI set
   * - 5
     - :ref:`1dDW_Grad_o_Mat++ <ahelp_1dDW_Grad_o_Mat++>`
     - Perform calculations and conversions of DWI gradients and matrices
   * - 4
     - :ref:`3dDWUncert <ahelp_3dDWUncert>`
     - Estimate uncertainty of FA and V1 of diffusion tensor dataset, for tracking purposes (and see fat_proc_dwi_to_dt)
   * - 4
     - :ref:`@fat_tract_colorize <ahelp_@fat_tract_colorize>`
     - Visualize volumetric output maps from 3dTrackID tracking
   * - 3
     - :ref:`fat_roi_row.py <ahelp_fat_roi_row.py>`
     - Select a single ROI's row out of a connectivity matrix file (*.grid or *.netcc)
   * - 1
     - :ref:`3dDTeig <ahelp_3dDTeig>`
     - Computes eigenvalues and eigenvectors for an input DT set
   * - 1
     - :ref:`3dDTtoDWI <ahelp_3dDTtoDWI>`
     - Calculate 'ideal' DWIs for each grad, from DT+b0+gradient files
   * - 1
     - :ref:`3dDTtoNoisyDWI <ahelp_3dDTtoNoisyDWI>`
     - Make a simulated DWI set with random noise, from DT+gradient information
   * - 1
     - :ref:`3dEigsToDT <ahelp_3dEigsToDT>`
     - Calculate diffusion tensor dataset from eigenvalues and eigenvectors
   * - 1
     - :ref:`3dTORTOISEtoHere <ahelp_3dTORTOISEtoHere>`
     - Convert standard TORTOISE-format DTs to AFNI-format DTs
   * - 1
     - :ref:`DTIStudioFibertoSegments <ahelp_DTIStudioFibertoSegments>`
     - Convert a DTIStudio Fiber file to a SUMA segment file
   * - 1
     - :ref:`@DTI_studio_reposition <ahelp_@DTI_studio_reposition>`
     - (probably obsolete)
   * - 1
     - :ref:`InstaTract <ahelp_InstaTract>`
     - (intermediate function only)
   * - 1
     - :ref:`3dProbTrackID <ahelp_3dProbTrackID>`
     - (obsolete -> use 3dTrackID)
   * - 1
     - :ref:`map_TrackID <ahelp_map_TrackID>`
     - Apply linear affine transform to track file (*.trk format only)
   * - 1
     - :ref:`1dDW_Grad_o_Mat <ahelp_1dDW_Grad_o_Mat>`
     - (obsolete -> use 1dDW_Grad_o_Mat++)


**Convert surfaces from other software**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 5
     - :ref:`@SUMA_Make_Spec_FS <ahelp_@SUMA_Make_Spec_FS>`
     - Convert output from standard FreeSurfer 'recon-all' processing to AFNI+SUMAland
   * - 1
     - :ref:`@SUMA_Make_Spec_Caret <ahelp_@SUMA_Make_Spec_Caret>`
     - Convert output from standard Caret processing to AFNI+SUMAland
   * - 1
     - :ref:`@SUMA_Make_Spec_SF <ahelp_@SUMA_Make_Spec_SF>`
     - Convert output from standard SureFit processing to AFNI+SUMAland
   * - 1
     - :ref:`@SUMA_FSvolToBRIK <ahelp_@SUMA_FSvolToBRIK>`
     - 
   * - 1
     - :ref:`@SUMA_renumber_FS <ahelp_@SUMA_renumber_FS>`
     - Renumber standard FS-'recon-all' seg+parc values; make tissue-grouped maps (part of @SUMA_Make_Spec_FS)
   * - 1
     - :ref:`@suma_reprefixize_spec <ahelp_@suma_reprefixize_spec>`
     - 
   * - 1
     - :ref:`@FSlabel2dset <ahelp_@FSlabel2dset>`
     - 
   * - 1
     - :ref:`FSread_annot <ahelp_FSread_annot>`
     - 
   * - 1
     - :ref:`@FS_roi_label <ahelp_@FS_roi_label>`
     - 
   * - 1
     - :ref:`parse_fs_lt_log.py <ahelp_parse_fs_lt_log.py>`
     - Parse FreeSurfer region labels to get indices


**Convert statistics and p-values**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 4
     - :ref:`ccalc <ahelp_ccalc>`
     - A command line calculator (like 3dcalc)
   * - 4
     - :ref:`cdf <ahelp_cdf>`
     - Compute probabilities, thresholds for standard distributions
   * - 4
     - :ref:`p2dsetstat <ahelp_p2dsetstat>`
     - Convert a p-value to a stat, using parameters stored in a dset header


**Compare dset volumes (masks or valued)**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 5
     - :ref:`3dABoverlap <ahelp_3dABoverlap>`
     - Count overlaps between 2 datasets (union, intersection, etc.)
   * - 5
     - :ref:`3dSliceNDice <ahelp_3dSliceNDice>`
     - Calculate Dice coefficients slice-by-slice (for all three FOV planes) between mask dsets.
   * - 4
     - :ref:`3dMatch <ahelp_3dMatch>`
     - Find pairs of similar-looking subbricks between two groups of dsets
   * - 1
     - :ref:`@DiceMetric <ahelp_@DiceMetric>`
     - Computes Dice Coefficient between two datasets
   * - 1
     - :ref:`3ddot <ahelp_3ddot>`
     - Calculate correlation coefficients between sub-brick pairs in a 4D dset
   * - 1
     - :ref:`3ddot_beta <ahelp_3ddot_beta>`
     - Faster version of 3ddot, though currently just for calculating eta-squared
   * - 1
     - :ref:`3dOverlap <ahelp_3dOverlap>`
     - Count of number of voxels that are nonzero in ALL of the input dataset sub-bricks


**Time series pre-processing**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 4
     - :ref:`3dTshift <ahelp_3dTshift>`
     - Shift slices to a common time origin (temporal interpolation)
   * - 3
     - :ref:`3dBrainSync <ahelp_3dBrainSync>`
     - Alter one dataset’s time series to be maximally correlated with another dataset’s time series
   * - 1
     - :ref:`3dDespike <ahelp_3dDespike>`
     - Remove spikes from voxel time series
   * - 1
     - :ref:`3dDetrend <ahelp_3dDetrend>`
     - Remove trends from voxel time series
   * - 1
     - :ref:`3dTproject <ahelp_3dTproject>`
     - Project out time series (like -errts from 3dDeconvolve)
   * - 1
     - :ref:`3dFourier <ahelp_3dFourier>`
     - FFT-based lowpass and highpass filtering
   * - 1
     - :ref:`3dTsmooth <ahelp_3dTsmooth>`
     - Smooth time series in the time domain
   * - 1
     - :ref:`3dTRfix <ahelp_3dTRfix>`
     - Resample a dataset in time from an irregular grid to a regular grid
   * - 1
     - :ref:`RetroTS.py <ahelp_RetroTS.py>`
     - Generate slicewise physiological regressors


**Time series analysis**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 5
     - :ref:`3dREMLfit <ahelp_3dREMLfit>`
     - Multiple linear regression (generalized least squares)
   * - 4
     - :ref:`3dDeconvolve <ahelp_3dDeconvolve>`
     - Multiple linear regression and deconvolution (ordinary least squares)
   * - 1
     - :ref:`3dNLfim <ahelp_3dNLfim>`
     - Nonlinear regression
   * - 1
     - :ref:`3dLSS <ahelp_3dLSS>`
     - Ad hoc version of IM regression, giving amplitudes for each stimulus event
   * - 1
     - :ref:`3dTcorrelate <ahelp_3dTcorrelate>`
     - Correlate two input datasets, voxel-by-voxel
   * - 1
     - :ref:`3dAutoTcorrelate <ahelp_3dAutoTcorrelate>`
     - Correlate each voxel with every other voxel
   * - 1
     - :ref:`3dpc <ahelp_3dpc>`
     - Principal component analysis
   * - 1
     - :ref:`3dDeconvolve_f <ahelp_3dDeconvolve_f>`
     - (obsolete -> use 3dDeconvolve)
   * - 1
     - :ref:`3dSynthesize <ahelp_3dSynthesize>`
     - Compute 3d+time dataset from partial model
   * - 1
     - :ref:`plugin(Deconvolution) <ahelp_plugin(Deconvolution)>`
     - Interactive deconvolution
   * - 1
     - :ref:`3ddelay <ahelp_3ddelay>`
     - Single regressor linear analysis with time shifting
   * - 1
     - :ref:`plugins(Nlfit and Nlerr) <ahelp_plugins(Nlfit and Nlerr)>`
     - Interactive nonlinear regression
   * - 1
     - :ref:`3dfim <ahelp_3dfim>`
     - Linear regression (obsolete -> use 3dDeconvolve)
   * - 1
     - :ref:`3dfim+ <ahelp_3dfim+>`
     - Linear regression (obsolete -> use 3dDeconvolve)
   * - 1
     - :ref:`1dNLfit <ahelp_1dNLfit>`
     - Fit a general model to a vector of data


**Quality checks (for 3D+time datasets or results)**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 4
     - :ref:`3dToutcount <ahelp_3dToutcount>`
     - Check voxel time series for quality (temporal outliers)
   * - 4
     - :ref:`@radial_correlate <ahelp_@radial_correlate>`
     - Check datasets for correlation artifact
   * - 4
     - :ref:`gen_ss_review_scripts.py <ahelp_gen_ss_review_scripts.py>`
     - Generate QC review scripts
   * - 1
     - :ref:`3dTqual <ahelp_3dTqual>`
     - Check dataset sub-bricks for quality (spatial outliers)
   * - 1
     - :ref:`@compute_gcor <ahelp_@compute_gcor>`
     - Compute average pairwise correlation (GCOR), one number
   * - 1
     - :ref:`gen_ss_review_table.py <ahelp_gen_ss_review_table.py>`
     - Generate spread-sheet of review_basic results
   * - 1
     - :ref:`3dCountSpikes <ahelp_3dCountSpikes>`
     - (obsolete -> use 3dToutcount)


**Miscellaneous file manipulations**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 4
     - :ref:`file_tool <ahelp_file_tool>`
     - Display or edit data in arbitrary files
   * - 4
     - :ref:`1d_tool.py <ahelp_1d_tool.py>`
     - For manipulating and evaluating 1D files
   * - 1
     - :ref:`@diff.files <ahelp_@diff.files>`
     - Compare (diff) a set of files to those in another location
   * - 1
     - :ref:`@diff.tree <ahelp_@diff.tree>`
     - Compare (diff) 2 directory trees of files
   * - 1
     - :ref:`2swap <ahelp_2swap>`
     - Byte pair swap, e.g., ab ba
   * - 1
     - :ref:`4swap <ahelp_4swap>`
     - Byte quad swap, e.g., abc dcba
   * - 1
     - :ref:`24swap <ahelp_24swap>`
     - Mixed 2 and 4 byte swaps in same file
   * - 1
     - :ref:`strblast <ahelp_strblast>`
     - Find a string in a file and replace it with junk
   * - 1
     - :ref:`@NoExt <ahelp_@NoExt>`
     - Remove specified file extensions from file name
   * - 1
     - :ref:`@NoPound <ahelp_@NoPound>`
     - Change name of file or dataset to avoid pound (#) symbols
   * - 1
     - :ref:`@np <ahelp_@np>`
     - Generate new prefix given some base prefix


**MVM modelling of (correlational or structural) matrices**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 3
     - :ref:`fat_mvm_prep.py <ahelp_fat_mvm_prep.py>`
     - Combine *.grid/*.netcc files with subject data in CSV files; for fat_mvm* modeling
   * - 3
     - :ref:`fat_mvm_review.py <ahelp_fat_mvm_review.py>`
     - (only beta)
   * - 1
     - :ref:`fat_lat_csv.py <ahelp_fat_lat_csv.py>`
     - Make latent variables for CSV file data using factor analysis; esp for fat_mvm* usage
   * - 1
     - :ref:`fat_mat_sel.py <ahelp_fat_mat_sel.py>`
     - Plot matrices from 3dNetcorr (*.netcc) or 3dTrackID (*.grid) files
   * - 1
     - :ref:`fat_mvm_gridconv.py <ahelp_fat_mvm_gridconv.py>`
     - Convert ooold 3dTrackID output *.grid files; should be unnecessary now
   * - 1
     - :ref:`fat_mvm_scripter.py <ahelp_fat_mvm_scripter.py>`
     - Read in a data table file (esp. from fat_mvm_prep.py) and build 3dMVM command


**Generate model 1D time series**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 1
     - :ref:`3dDeconvolve <ahelp_3dDeconvolve>`
     - Generate hemodynamic responses for stimulus timing files
   * - 1
     - :ref:`1dBport <ahelp_1dBport>`
     - Generate columns of sines and cosines for bandpassing
   * - 1
     - :ref:`sqwave <ahelp_sqwave>`
     - Generate a square wave (a very old program)
   * - 1
     - :ref:`waver <ahelp_waver>`
     - Generate hemodynamic responses to stimulus time series


**Fourier related, time series/freq**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 3
     - :ref:`3dLombScargle <ahelp_3dLombScargle>`
     - Calculate amp/pow spectrum (like FFT) along time axis with missing time points
   * - 3
     - :ref:`3dAmpToRSFC <ahelp_3dAmpToRSFC>`
     - Calculate RSFC parameters (ALFF, fALFF, RSFA, etc.) from 3dLombScargle output
   * - 3
     - :ref:`3dBandpass <ahelp_3dBandpass>`
     - 
   * - 3
     - :ref:`3dRSFC <ahelp_3dRSFC>`
     - Calculate RSFC parameters (ALFF, fALFF, RSFA, etc.) for uncensored time series
   * - 1
     - :ref:`3dFourier <ahelp_3dFourier>`
     - 
   * - 1
     - :ref:`3dDFT <ahelp_3dDFT>`
     - FFT along time axis
   * - 1
     - :ref:`3dFFT <ahelp_3dFFT>`
     - FFT along spatial axis
   * - 1
     - :ref:`1dBandpass <ahelp_1dBandpass>`
     - 
   * - 1
     - :ref:`3dPeriodogram <ahelp_3dPeriodogram>`
     - 
   * - 1
     - :ref:`3dWavelets <ahelp_3dWavelets>`
     - 
   * - 1
     - :ref:`stimband <ahelp_stimband>`
     - 


**Dset histograms**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 3
     - :ref:`3dHist <ahelp_3dHist>`
     - Compute histograms using functions for generating priors
   * - 1
     - :ref:`3dAnhist <ahelp_3dAnhist>`
     - Create and plot histogram of dataset, print peaks
   * - 1
     - :ref:`3dhistog <ahelp_3dhistog>`
     - Create histogram of dataset to a file
   * - 1
     - :ref:`plugin(Histogram) <ahelp_plugin(Histogram)>`
     - Interactively graphs histogram of a dataset (or ROI)
   * - 1
     - :ref:`plugin(ScatterPlot) <ahelp_plugin(ScatterPlot)>`
     - Interactively graphs 1 sub-brick vs. another (or ROI)


**Download/install demos**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 4
     - :ref:`@Install_D99_macaque <ahelp_@Install_D99_macaque>`
     - Install Saleem D99 macaque atlas and template
   * - 4
     - :ref:`@Install_DBSproc <ahelp_@Install_DBSproc>`
     - Install DBS processing pipeline script
   * - 4
     - :ref:`@Install_FATCAT_DEMO2 <ahelp_@Install_FATCAT_DEMO2>`
     - Install newer FATCAT Demo for DTI processing with fat_proc programs (and including TORTOISE and FreeSurfer processing)
   * - 3
     - :ref:`@Install_FATCAT_DEMO <ahelp_@Install_FATCAT_DEMO>`
     - Install original FATCAT Demo for DTI + some FMRI processing
   * - 3
     - :ref:`@Install_FATMVM_DEMO <ahelp_@Install_FATMVM_DEMO>`
     - Install FATCAT+MVM statistical modeling demo, multivariate modeling in conjuction with tractography (also applies to correlation matrices such as from 3dNetCorr)
   * - 1
     - :ref:`@Install_3dPFM_Demo <ahelp_@Install_3dPFM_Demo>`
     - 
   * - 1
     - :ref:`@Install_AfniRetinoDemo <ahelp_@Install_AfniRetinoDemo>`
     - 
   * - 1
     - :ref:`@Install_ClustScat_Demo <ahelp_@Install_ClustScat_Demo>`
     - 
   * - 1
     - :ref:`@Install_InstaCorr_Demo <ahelp_@Install_InstaCorr_Demo>`
     - Install demo data for InstaCorr, instant correlation
   * - 1
     - :ref:`@Install_MEICA_Demo <ahelp_@Install_MEICA_Demo>`
     - 
   * - 1
     - :ref:`@Install_NIH_Marmoset <ahelp_@Install_NIH_Marmoset>`
     - Install NIH Marmoset atlas and template
   * - 1
     - :ref:`@Install_RSFMRI_Motion_Group_Demo <ahelp_@Install_RSFMRI_Motion_Group_Demo>`
     - 
   * - 1
     - :ref:`@Install_TSrestMovieDemo <ahelp_@Install_TSrestMovieDemo>`
     - 


**DICOM info and conversion**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 5
     - :ref:`Dimon <ahelp_Dimon>`
     - Read DICOM files on disk or as they are created
   * - 5
     - :ref:`dcm2niix_afni <ahelp_dcm2niix_afni>`
     - Primary choice for converting DCM files of DWI dsets (and possibly FMRI)
   * - 3
     - :ref:`dicom_hinfo <ahelp_dicom_hinfo>`
     - Print out selected information from a number of DICOM headers
   * - 2
     - :ref:`to3d <ahelp_to3d>`
     - Read image files, write AFNI format datasets (not usually directly used)
   * - 2
     - :ref:`from3d <ahelp_from3d>`
     - Write dataset slices into image files
   * - 1
     - :ref:`dicom_hdr <ahelp_dicom_hdr>`
     - Print out information from one DICOM header
   * - 1
     - :ref:`dicom_to_raw <ahelp_dicom_to_raw>`
     - For extracting only the binary image data from a DICOM file
   * - 1
     - :ref:`@move.to.series.dirs <ahelp_@move.to.series.dirs>`
     - Partition DICOM images into run directectories
   * - 1
     - :ref:`Ifile <ahelp_Ifile>`
     - Read GE realtime EPI files and runs to3d
   * - 1
     - :ref:`Imon <ahelp_Imon>`
     - (obs - use Dimon); Read GE realtime EPI files as they are created
   * - 1
     - :ref:`rtfeedme <ahelp_rtfeedme>`
     - Dissect one dataset, sends images to AFNI realtime plugin
   * - 1
     - :ref:`plugin(RT Options) <ahelp_plugin(RT Options)>`
     - Control options for AFNI realtime image input
   * - 1
     - :ref:`abut <ahelp_abut>`
     - Create zero-filled slices to put into dataset gaps
   * - 1
     - :ref:`dicom_hdr <ahelp_dicom_hdr>`
     - Print information from a DICOM file
   * - 1
     - :ref:`ge_header <ahelp_ge_header>`
     - Print information from a GE I. file
   * - 1
     - :ref:`mayo_analyze <ahelp_mayo_analyze>`
     - Print information froman ANALYZE .hdr file
   * - 1
     - :ref:`siemens_vision <ahelp_siemens_vision>`
     - Print information from a Siemens Vision .ima file
   * - 1
     - :ref:`Dimon1 <ahelp_Dimon1>`
     - (obsolete -> use Dimon)


**Copy/convert/manipulate dsets**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 5
     - :ref:`3dcopy <ahelp_3dcopy>`
     - Copy a dataset to make new files
   * - 1
     - :ref:`3dBRAIN_VOYAGERtoAFNI <ahelp_3dBRAIN_VOYAGERtoAFNI>`
     - 
   * - 1
     - :ref:`3dAFNIto3D <ahelp_3dAFNIto3D>`
     - Read image files, write AFNI format datasets
   * - 1
     - :ref:`3dAFNItoANALYZE <ahelp_3dAFNItoANALYZE>`
     - Convert AFNI format dataset to ANALYZE format
   * - 1
     - :ref:`3dAFNItoMINC <ahelp_3dAFNItoMINC>`
     - Convert AFNI format dataset to MINC format
   * - 1
     - :ref:`3dMINCtoAFNI <ahelp_3dMINCtoAFNI>`
     - Convert MINC format dataset to AFNI format
   * - 1
     - :ref:`3dAFNItoNIFTI <ahelp_3dAFNItoNIFTI>`
     - 
   * - 1
     - :ref:`3dAFNItoNIML <ahelp_3dAFNItoNIML>`
     - 
   * - 1
     - :ref:`3dAFNItoRaw <ahelp_3dAFNItoRaw>`
     - 
   * - 1
     - :ref:`3dANALYZEtoAFNI <ahelp_3dANALYZEtoAFNI>`
     - (obsolete -> use 3dcopy or to3d)
   * - 1
     - :ref:`3dCRUISEtoAFNI <ahelp_3dCRUISEtoAFNI>`
     - 
   * - 1
     - :ref:`3dThreetoRGB <ahelp_3dThreetoRGB>`
     - Convert 3 scalar datasets to 1 RGB AFNI format dataset
   * - 1
     - :ref:`3dPAR2AFNI.pl <ahelp_3dPAR2AFNI.pl>`
     - (almost useless helpfile... obsolete???)
   * - 1
     - :ref:`3drename <ahelp_3drename>`
     - Rename dataset files
   * - 1
     - :ref:`3ddup <ahelp_3ddup>`
     - Make an 'empty' duplicate (warp-on-demand) of a dataset
   * - 1
     - :ref:`3dTwotoComplex <ahelp_3dTwotoComplex>`
     - Create complex dataset from two sub-bricks
   * - 1
     - :ref:`3dEmpty <ahelp_3dEmpty>`
     - Create header file only for specified dimensions
   * - 1
     - :ref:`3dVecRGB_to_HSL <ahelp_3dVecRGB_to_HSL>`
     - Convert RGB coloration to HSL values; typically intermed step in viewing prob. tracking results.
   * - 1
     - :ref:`3dMaskToASCII <ahelp_3dMaskToASCII>`
     - 


**Changing dset spatial structure**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 3
     - :ref:`3dLRflip <ahelp_3dLRflip>`
     - Flip dataset contents Left <-> Right
   * - 3
     - :ref:`fat_proc_axialize_anat <ahelp_fat_proc_axialize_anat>`
     - Rotate brain to have standard viewing planes along slices
   * - 1
     - :ref:`3daxialize <ahelp_3daxialize>`
     - Rewrite dataset with slices in different direction
   * - 1
     - :ref:`3dresample <ahelp_3dresample>`
     - Rewrite dataset in new orientation, with new voxel size


**Change dset temporal structure**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 1
     - :ref:`3dUpsample <ahelp_3dUpsample>`
     - Upsample in time (to a shorter TR)


**Supplementary/open programs included in AFNI**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 1
     - :ref:`cjpeg <ahelp_cjpeg>`
     - Compress an image file to a JPEG file
   * - 1
     - :ref:`djpeg <ahelp_djpeg>`
     - Decompress a JPEG file to an image file
   * - 1
     - :ref:`mpeg_encode <ahelp_mpeg_encode>`
     - Convert sequence of images into MPEG movie
   * - 1
     - :ref:`whirlgif <ahelp_whirlgif>`
     - Concatenate series of GIFs into a single one


**Supplemental/underlying programs (no desc needed)**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 1
     - :ref:`@global_parse <ahelp_@global_parse>`
     - 
   * - 1
     - :ref:`lib_afni1D.py <ahelp_lib_afni1D.py>`
     - 
   * - 1
     - :ref:`afni_util.py <ahelp_afni_util.py>`
     - 
   * - 1
     - :ref:`3dToyProg <ahelp_3dToyProg>`
     - 


**Simulate/generate dsets**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 1
     - :ref:`3dTSgen <ahelp_3dTSgen>`
     - Generate 3D+time dataset from 1D model and noise
   * - 1
     - :ref:`3dClustSim <ahelp_3dClustSim>`
     - Simulate datasets and estimate statistical power
   * - 1
     - :ref:`slow_surf_clustsim.py <ahelp_slow_surf_clustsim.py>`
     - Like 3dClustSim, but for surface data.
   * - 1
     - :ref:`quick.alpha.vals.py <ahelp_quick.alpha.vals.py>`
     - Companion to slow_surf_clustsim.py
   * - 1
     - :ref:`3dConvolve <ahelp_3dConvolve>`
     - Simulate datasets via convolution (for testing 3dDeconvolve)
   * - 1
     - :ref:`3dInvFMRI <ahelp_3dInvFMRI>`
     - Compute stimulus time series given activation map and 3D+time dataset
   * - 1
     - :ref:`3dDTtoNoisyDWI <ahelp_3dDTtoNoisyDWI>`
     - Make a simulated DWI set with random noise, from DT+gradient information
   * - 1
     - :ref:`1dgenARMA11 <ahelp_1dgenARMA11>`
     - Generate synthetic ARMA(1,1) correlated time series, to test 3dREMLfit


**Miscellaneous visualization tools**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 3
     - :ref:`aiv <ahelp_aiv>`
     - AFNI Image Viewer program
   * - 3
     - :ref:`3dGrayplot <ahelp_3dGrayplot>`
     - Take a 3D+time dataset and make a summary 2D image for data quality review
   * - 1
     - :ref:`plugin(Render[new]) <ahelp_plugin(Render[new])>`
     - Interactive volume rendering
   * - 1
     - :ref:`plugin(Dataset#N) <ahelp_plugin(Dataset#N)>`
     - Graph extra dataset time series in AFNI graph viewer
   * - 1
     - :ref:`afni_open <ahelp_afni_open>`
     - Open various AFNI/SUMA files (*.xmat, *.pdf, etc.)


**Miscellaneous utilities**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 5
     - :ref:`apsearch <ahelp_apsearch>`
     - Simple+approx string searching; used in atlases and helps
   * - 4
     - :ref:`count <ahelp_count>`
     - Generate numbered strings for command line scripts
   * - 4
     - :ref:`afni_history <ahelp_afni_history>`
     - Display a log of updates to AFNI code
   * - 3
     - :ref:`ccalc <ahelp_ccalc>`
     - A command line calculator (like 3dcalc)
   * - 3
     - :ref:`cdf <ahelp_cdf>`
     - Compute probabilities, thresholds for standard distributions
   * - 1
     - :ref:`byteorder <ahelp_byteorder>`
     - Report the byteorder of the current CPU


**Unclassed**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 1
     - :ref:`1dAstrip <ahelp_1dAstrip>`
     - 
   * - 1
     - :ref:`@1dDiffMag <ahelp_@1dDiffMag>`
     - 
   * - 1
     - :ref:`1ddot <ahelp_1ddot>`
     - 
   * - 1
     - :ref:`1dfft <ahelp_1dfft>`
     - 
   * - 1
     - :ref:`1dFlagMotion <ahelp_1dFlagMotion>`
     - 
   * - 1
     - :ref:`1dgrayplot <ahelp_1dgrayplot>`
     - 
   * - 1
     - :ref:`1dnorm <ahelp_1dnorm>`
     - 
   * - 1
     - :ref:`@2dwarper <ahelp_@2dwarper>`
     - 
   * - 1
     - :ref:`@2dwarper.Allin <ahelp_@2dwarper.Allin>`
     - 
   * - 1
     - :ref:`2perm <ahelp_2perm>`
     - 
   * - 1
     - :ref:`3dAcost <ahelp_3dAcost>`
     - 
   * - 1
     - :ref:`3dbuc2fim <ahelp_3dbuc2fim>`
     - 
   * - 1
     - :ref:`3dClipLevel <ahelp_3dClipLevel>`
     - 
   * - 1
     - :ref:`3dClustCount <ahelp_3dClustCount>`
     - 
   * - 1
     - :ref:`3dConformist <ahelp_3dConformist>`
     - 
   * - 1
     - :ref:`3dDegreeCentrality <ahelp_3dDegreeCentrality>`
     - 
   * - 1
     - :ref:`3dECM <ahelp_3dECM>`
     - 
   * - 1
     - :ref:`3dEntropy <ahelp_3dEntropy>`
     - 
   * - 1
     - :ref:`3dGenFeatureDist <ahelp_3dGenFeatureDist>`
     - 
   * - 1
     - :ref:`3dGenPriors <ahelp_3dGenPriors>`
     - 
   * - 1
     - :ref:`3dkmeans <ahelp_3dkmeans>`
     - 
   * - 1
     - :ref:`3dLFCD <ahelp_3dLFCD>`
     - 
   * - 1
     - :ref:`3dmaskSVD <ahelp_3dmaskSVD>`
     - 
   * - 1
     - :ref:`3dMSE <ahelp_3dMSE>`
     - 
   * - 1
     - :ref:`3dMultiThresh <ahelp_3dMultiThresh>`
     - 
   * - 1
     - :ref:`3dnoise <ahelp_3dnoise>`
     - 
   * - 1
     - :ref:`3dnvals <ahelp_3dnvals>`
     - 
   * - 1
     - :ref:`3dPFM <ahelp_3dPFM>`
     - 
   * - 1
     - :ref:`3dPolyfit <ahelp_3dPolyfit>`
     - 
   * - 1
     - :ref:`3dproject <ahelp_3dproject>`
     - 
   * - 1
     - :ref:`3dretroicor <ahelp_3dretroicor>`
     - 
   * - 1
     - :ref:`3dSignatures <ahelp_3dSignatures>`
     - 
   * - 1
     - :ref:`3dSpaceTimeCorr <ahelp_3dSpaceTimeCorr>`
     - 
   * - 1
     - :ref:`3dSpatNorm <ahelp_3dSpatNorm>`
     - 
   * - 1
     - :ref:`3dStatClust <ahelp_3dStatClust>`
     - 
   * - 1
     - :ref:`3dsvm <ahelp_3dsvm>`
     - 
   * - 1
     - :ref:`3dsvm_linpredict <ahelp_3dsvm_linpredict>`
     - 
   * - 1
     - :ref:`3dTnorm <ahelp_3dTnorm>`
     - 
   * - 1
     - :ref:`3dtoXdataset <ahelp_3dtoXdataset>`
     - 
   * - 1
     - :ref:`3dXClustSim <ahelp_3dXClustSim>`
     - 
   * - 1
     - :ref:`@4Daverage <ahelp_@4Daverage>`
     - 
   * - 1
     - :ref:`@afni.run.me <ahelp_@afni.run.me>`
     - 
   * - 1
     - :ref:`afni_skeleton.py <ahelp_afni_skeleton.py>`
     - 
   * - 1
     - :ref:`AnalyzeTrace <ahelp_AnalyzeTrace>`
     - 
   * - 1
     - :ref:`balloon <ahelp_balloon>`
     - 
   * - 1
     - :ref:`BrainSkin <ahelp_BrainSkin>`
     - 
   * - 1
     - :ref:`@build_afni_Xlib <ahelp_@build_afni_Xlib>`
     - 
   * - 1
     - :ref:`@Center_Distance <ahelp_@Center_Distance>`
     - 
   * - 1
     - :ref:`@CheckForAfniDset <ahelp_@CheckForAfniDset>`
     - 
   * - 1
     - :ref:`@clean_help_dir <ahelp_@clean_help_dir>`
     - 
   * - 1
     - :ref:`@clip_volume <ahelp_@clip_volume>`
     - 
   * - 1
     - :ref:`ConvexHull <ahelp_ConvexHull>`
     - 
   * - 1
     - :ref:`@DeblankFileNames <ahelp_@DeblankFileNames>`
     - 
   * - 1
     - :ref:`demo.fixed.niml.do <ahelp_demo.fixed.niml.do>`
     - 
   * - 1
     - :ref:`demo.mobile.niml.do <ahelp_demo.mobile.niml.do>`
     - 
   * - 1
     - :ref:`@demo_prompt <ahelp_@demo_prompt>`
     - 
   * - 1
     - :ref:`@DO.examples <ahelp_@DO.examples>`
     - 
   * - 1
     - :ref:`eg_main_chrono.py <ahelp_eg_main_chrono.py>`
     - 
   * - 1
     - :ref:`@ElectroGrid <ahelp_@ElectroGrid>`
     - 
   * - 1
     - :ref:`ent16 <ahelp_ent16>`
     - 
   * - 1
     - :ref:`@escape- <ahelp_@escape->`
     - 
   * - 1
     - :ref:`@ExamineGenFeatDists <ahelp_@ExamineGenFeatDists>`
     - 
   * - 1
     - :ref:`ExamineXmat <ahelp_ExamineXmat>`
     - 
   * - 1
     - :ref:`@fast_roi <ahelp_@fast_roi>`
     - 
   * - 1
     - :ref:`FD2 <ahelp_FD2>`
     - 
   * - 1
     - :ref:`fdrval <ahelp_fdrval>`
     - 
   * - 1
     - :ref:`fftest <ahelp_fftest>`
     - 
   * - 1
     - :ref:`fim2 <ahelp_fim2>`
     - 
   * - 1
     - :ref:`@fix_FSsphere <ahelp_@fix_FSsphere>`
     - 
   * - 1
     - :ref:`@float_fix <ahelp_@float_fix>`
     - 
   * - 1
     - :ref:`float_scan <ahelp_float_scan>`
     - 
   * - 1
     - :ref:`ftosh <ahelp_ftosh>`
     - 
   * - 1
     - :ref:`gen_epi_review.py <ahelp_gen_epi_review.py>`
     - 
   * - 1
     - :ref:`GLTsymtest <ahelp_GLTsymtest>`
     - 
   * - 1
     - :ref:`gui_uber_align_test.py <ahelp_gui_uber_align_test.py>`
     - 
   * - 1
     - :ref:`gui_uber_skel.py <ahelp_gui_uber_skel.py>`
     - 
   * - 1
     - :ref:`gui_uber_subj.py <ahelp_gui_uber_subj.py>`
     - 
   * - 1
     - :ref:`gui_uber_ttest.py <ahelp_gui_uber_ttest.py>`
     - 
   * - 1
     - :ref:`gui_xmat.py <ahelp_gui_xmat.py>`
     - 
   * - 1
     - :ref:`@help.AFNI <ahelp_@help.AFNI>`
     - 
   * - 1
     - :ref:`help_format <ahelp_help_format>`
     - 
   * - 1
     - :ref:`im2niml <ahelp_im2niml>`
     - 
   * - 1
     - :ref:`images_equal <ahelp_images_equal>`
     - 
   * - 1
     - :ref:`imand <ahelp_imand>`
     - 
   * - 1
     - :ref:`imaver <ahelp_imaver>`
     - 
   * - 1
     - :ref:`imcalc <ahelp_imcalc>`
     - 
   * - 1
     - :ref:`imcutup <ahelp_imcutup>`
     - 
   * - 1
     - :ref:`imdump <ahelp_imdump>`
     - 
   * - 1
     - :ref:`immask <ahelp_immask>`
     - 
   * - 1
     - :ref:`imreg <ahelp_imreg>`
     - 
   * - 1
     - :ref:`imrotate <ahelp_imrotate>`
     - 
   * - 1
     - :ref:`imstack <ahelp_imstack>`
     - 
   * - 1
     - :ref:`imstat <ahelp_imstat>`
     - 
   * - 1
     - :ref:`imupsam <ahelp_imupsam>`
     - 
   * - 1
     - :ref:`inspec <ahelp_inspec>`
     - 
   * - 1
     - :ref:`@make_plug_diff <ahelp_@make_plug_diff>`
     - 
   * - 1
     - :ref:`make_pq_script.py <ahelp_make_pq_script.py>`
     - 
   * - 1
     - :ref:`meica.py <ahelp_meica.py>`
     - 
   * - 1
     - :ref:`module_test_lib.py <ahelp_module_test_lib.py>`
     - 
   * - 1
     - :ref:`mritopgm <ahelp_mritopgm>`
     - 
   * - 1
     - :ref:`mycat <ahelp_mycat>`
     - 
   * - 1
     - :ref:`myget <ahelp_myget>`
     - 
   * - 1
     - :ref:`neuro_deconvolve.py <ahelp_neuro_deconvolve.py>`
     - 
   * - 1
     - :ref:`nicat <ahelp_nicat>`
     - 
   * - 1
     - :ref:`niccc <ahelp_niccc>`
     - 
   * - 1
     - :ref:`nifti1_test <ahelp_nifti1_test>`
     - 
   * - 1
     - :ref:`niml_feedme <ahelp_niml_feedme>`
     - 
   * - 1
     - :ref:`niprobe <ahelp_niprobe>`
     - 
   * - 1
     - :ref:`nsize <ahelp_nsize>`
     - 
   * - 1
     - :ref:`option_list.py <ahelp_option_list.py>`
     - 
   * - 1
     - :ref:`@Purify_1D <ahelp_@Purify_1D>`
     - 
   * - 1
     - :ref:`python_module_test.py <ahelp_python_module_test.py>`
     - 
   * - 1
     - :ref:`qdelaunay <ahelp_qdelaunay>`
     - 
   * - 1
     - :ref:`qhull <ahelp_qhull>`
     - 
   * - 1
     - :ref:`quotize <ahelp_quotize>`
     - 
   * - 1
     - :ref:`rbox <ahelp_rbox>`
     - 
   * - 1
     - :ref:`read_matlab_files.py <ahelp_read_matlab_files.py>`
     - 
   * - 1
     - :ref:`realtime_receiver.py <ahelp_realtime_receiver.py>`
     - 
   * - 1
     - :ref:`rmz <ahelp_rmz>`
     - 
   * - 1
     - :ref:`ROIgrow <ahelp_ROIgrow>`
     - 
   * - 1
     - :ref:`rotcom <ahelp_rotcom>`
     - 
   * - 1
     - :ref:`SampBias <ahelp_SampBias>`
     - 
   * - 1
     - :ref:`ScaleToMap <ahelp_ScaleToMap>`
     - 
   * - 1
     - :ref:`@ScaleVolume <ahelp_@ScaleVolume>`
     - 
   * - 1
     - :ref:`@ScriptCheck <ahelp_@ScriptCheck>`
     - 
   * - 1
     - :ref:`serial_helper <ahelp_serial_helper>`
     - 
   * - 1
     - :ref:`sfim <ahelp_sfim>`
     - 
   * - 1
     - :ref:`@simulate_motion <ahelp_@simulate_motion>`
     - 
   * - 1
     - :ref:`@SkullStrip_TouchUp <ahelp_@SkullStrip_TouchUp>`
     - 
   * - 1
     - :ref:`SpharmDeco <ahelp_SpharmDeco>`
     - 
   * - 1
     - :ref:`@Spharm.examples <ahelp_@Spharm.examples>`
     - 
   * - 1
     - :ref:`@statauxcode <ahelp_@statauxcode>`
     - 
   * - 1
     - :ref:`@T1scale <ahelp_@T1scale>`
     - 
   * - 1
     - :ref:`tfim <ahelp_tfim>`
     - 
   * - 1
     - :ref:`@TimeDiff <ahelp_@TimeDiff>`
     - 
   * - 1
     - :ref:`tokens <ahelp_tokens>`
     - 
   * - 1
     - :ref:`uber_skel.py <ahelp_uber_skel.py>`
     - 
   * - 1
     - :ref:`uber_ttest.py <ahelp_uber_ttest.py>`
     - 
   * - 1
     - :ref:`ui_xmat.py <ahelp_ui_xmat.py>`
     - 
   * - 1
     - :ref:`uniq_images <ahelp_uniq_images>`
     - 
   * - 1
     - :ref:`@VolCenter <ahelp_@VolCenter>`
     - 
   * - 1
     - :ref:`xmat_tool.py <ahelp_xmat_tool.py>`
     - 
   * - 1
     - :ref:`Xphace <ahelp_Xphace>`
     - 


**R programs (in R; called by others)**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

   * - 1
     - :ref:`1dGC.R <ahelp_1dGC.R>`
     - 
   * - 1
     - :ref:`1dRplot <ahelp_1dRplot>`
     - 
   * - 1
     - :ref:`1dRplot.R <ahelp_1dRplot.R>`
     - 
   * - 1
     - :ref:`1dSVAR.R <ahelp_1dSVAR.R>`
     - 
   * - 1
     - :ref:`3dAOV.R <ahelp_3dAOV.R>`
     - 
   * - 1
     - :ref:`3dGC.R <ahelp_3dGC.R>`
     - 
   * - 1
     - :ref:`3dICA.R <ahelp_3dICA.R>`
     - 
   * - 1
     - :ref:`3dICC.R <ahelp_3dICC.R>`
     - 
   * - 1
     - :ref:`3dICC_REML.R <ahelp_3dICC_REML.R>`
     - 
   * - 1
     - :ref:`3dKS.R <ahelp_3dKS.R>`
     - 
   * - 1
     - :ref:`3dMEMA.R <ahelp_3dMEMA.R>`
     - 
   * - 1
     - :ref:`3dPFM.R <ahelp_3dPFM.R>`
     - 
   * - 1
     - :ref:`3dRetinoPhase <ahelp_3dRetinoPhase>`
     - 
   * - 1
     - :ref:`3dRowFillin <ahelp_3dRowFillin>`
     - 
   * - 1
     - :ref:`3dRprogDemo <ahelp_3dRprogDemo>`
     - 
   * - 1
     - :ref:`3dRprogDemo.R <ahelp_3dRprogDemo.R>`
     - 
   * - 1
     - :ref:`3dSignatures.R <ahelp_3dSignatures.R>`
     - 
   * - 1
     - :ref:`AFNI_Batch_R <ahelp_AFNI_Batch_R>`
     - 
   * - 1
     - :ref:`AFNIplot.R <ahelp_AFNIplot.R>`
     - 
   * - 1
     - :ref:`afni_run_R <ahelp_afni_run_R>`
     - 
   * - 1
     - :ref:`@ANATICOR <ahelp_@ANATICOR>`
     - 
   * - 1
     - :ref:`@DoPerRoi.py <ahelp_@DoPerRoi.py>`
     - 
   * - 1
     - :ref:`ExamineXmat.R <ahelp_ExamineXmat.R>`
     - 
   * - 1
     - :ref:`FIRdesign <ahelp_FIRdesign>`
     - 
   * - 1
     - :ref:`Level2.R <ahelp_Level2.R>`
     - 
   * - 1
     - :ref:`@RenamePanga <ahelp_@RenamePanga>`
     - 
   * - 1
     - :ref:`@Reorder <ahelp_@Reorder>`
     - 
   * - 1
     - :ref:`@RetinoProc <ahelp_@RetinoProc>`
     - 
   * - 1
     - :ref:`@R_funclist <ahelp_@R_funclist>`
     - 
   * - 1
     - :ref:`@ShowDynamicRange <ahelp_@ShowDynamicRange>`
     - 
   * - 1
     - :ref:`Signatures.R <ahelp_Signatures.R>`
     - 
   * - 1
     - :ref:`smooth.R <ahelp_smooth.R>`
     - 
   * - 1
     - :ref:`SpharmReco <ahelp_SpharmReco>`
     - 
