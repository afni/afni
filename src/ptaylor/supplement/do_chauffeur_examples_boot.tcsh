#!/bin/tcsh

# This script will partially generate help pages, but not fully.  Wah.

set oscript = auto_@chauffeur_afni.rst
printf "" > $oscript

# -------------------------------------------------------------------------
# FUNNY_OSTRICH_01

   set here       = $PWD                                  
   set idir       = $here                                 # dir with data
   set odir       = $here/QC

   # anatomical volumes: some present already, and some derived here
   set vanat      = $idir/anat+orig                       # anatomical vol
   set preanat    = `3dinfo -prefix_noext "${vanat}"`     # vol prefix 
   set vanats     = $idir/strip+orig                      # anat. no skull
   set preanats   = `3dinfo -prefix_noext "${vanats}"`    # vol prefix 
   set preanatm   = anat_mask                             # vol prefix 
   set vanatm     = $idir/${preanatm}.nii.gz              # anat. ss + msk
   set preanatsu  = anat_ss_uni                           # vol prefix 
   set vanatsu    = $idir/${preanatsu}.nii.gz             # anat. unifized
   set preanatsub = anat_ss_uni_box                       # vol prefix 
   set vanatsub   = $idir/${preanatsub}.nii.gz            # anat. uni + box

   # stat/model output vol 
   set vmod       = $idir/func_slim+orig                  # model results
   set premod     = `3dinfo -prefix_noext "${vmod}"`      # vol prefix 

   # EPI volumes: some present already, others derived here
   set vepi       = $idir/epi_r1+orig                       # EPI vol, 4D
   set preepi     = `3dinfo -prefix_noext "${vepi}"`        # vol prefix 
   set preepie    = epi_edge0                               # vol prefix 
   set vepie      = $idir/${preepie}.nii.gz                 # EPI edgey [0]
   set preepip    = epi_part                                # vol prefix 
   set vepip      = $idir/${preepip}.nii.gz                 # part of EPI

   # selecting coef/stat bricks and labels
   set indcoef    = 3                                     # effect estimate
   set indstat    = 4                                     # stat of ee
   set labcoef    = `3dinfo -label "${vmod}[${indcoef}]"` # str label of ee
   set labstat    = `3dinfo -label "${vmod}[${indstat}]"` # str label of stat
   set labstatf   = "${labstat:gas/#/_/}"                 # str: no '#'
   set labcoeff   = "${labcoef:gas/#/_/}"                 # str: no '#'

   # info for thresholding/clustering
   set pthr       = 0.001                                 # voxelwise thresh
   set ttype      = "bisided"                             # {1,2,bi}sided

   \mkdir -p $odir

grep -B 100 WOOLY_MAMMOTH_01 do_chauff*tcsh | \
    grep -A 100 FUNNY_OSTRICH_01 > file_MAMMOTH_01.txt
# -------------------------------------------------------------------------

# ========================================================================
# ========================================================================
# ========================================================================

cat <<EOF >> $oscript
.. _tut_auto_@chauffeur_afni:

*********************
Using @chauffeur_afni
*********************

.. contents::
   :depth: 3

.. highlight:: Tcsh

\`\`@chauffeur_afni\`\` is an omnibus program that wraps around a lot
of the environment variables and "driving" capabilities in AFNI, with
the convenience of command line options.  This program can be used to
view either:

* multiple slices within a single 3D volume, or the same slice across

* a 4D dataset (underlay with or without overlay).  

In either case image files are always output in sets of 3: one each of
axial, coronal and sagittal slices.  Movies can be made as well.

The \`README.driver
<https://afni.nimh.nih.gov/pub/dist/doc/program_help/README.driver.html>\`_
and \`README.environment
<https://afni.nimh.nih.gov/pub/dist/doc/program_help/README.environment.html>\`_
pages are quite useful reading in accompaniment for The Curious
amongst you.

This program was originally written for use within :ref:\`FATCAT
fat_proc\* tools <FATCAT_prep>\` tools (generalizing the visualization
of \`\`@snapshot_volreg\`\`).  The \`\`fat_proc\*\`\` programs call
\`\`@chauffeur_afni\`\` internally, generating images at each step of
the pipeline that can be reviewed later s a first pass evaluation of
the data processing.  That, in fact, is the primary intention of the
program: help users look at their data as and after they process it.


Examples
========

We present some examples of auto-image making with
\`\`@chauffeur_afni\`\` using publicly available data, distributed as
part of the freely downloadable AFNI Bootcamp demo sets (see
:ref:\`here <Bootcamping>\`).  Specifically, these commands refer to
files in the \`\`AFNI_data6/afni/\`\` directory.

The code snippets could all be run in a single (\`\`tcsh\`\`) script
file-- the variable are consistent throughout the page, starting from
the initial definitions.  Such a file could be saved and run in the
same \`\`AFNI_data6/afni/\`\` directory.

Some supplementary data sets are generated and put into the "present"
input directory, and the images themselves are put into a subdirectory
that is created called \`\`QC/\`\`. Several of the examples include
thresholding statistical output data sets, and therefore include run
additional AFNI programs like \`\`3dClusterize\`\`, etc.  And at no
extra charge!

For space considerations on this webpage:

* only axial and sagittal images are displayed here (not the coronal,
  even though they are made automatically from the same
  \`\`@chauffeur_afni\`\` command)

* most of the montages are just :math:\`3\times3\`; for real practical
  usage, sometimes, it might useful to make them, say,
  :math:\`5\times3\`.

**To toggle between images**: open each of the images you want to move
between/among in a separate browser tab (e.g., middle mouse click on
it), and then move between tabs (e.g., \`\`Ctrl + Tab\`\` and \`\`Ctrl
+ Shift + Tab\`\`).

**Variable definitions**: The following are relevant variables for the
below commands (\`\`tcsh\`\` syntax), such as defining the directory
where the data is sitting, dset names, etc.:

.. hidden-code-block:: Tcsh
   :starthidden: False
   :label: - show code y/n -

EOF

sed '1d;$d' file_MAMMOTH_01.txt >> $oscript










# ========================================================================

# FUNNY_OSTRICH_06

   set idx   = 0
   set iiidx = `printf "%03d" $idx`
   set xmp   = "ca${iiidx}"

   set opref = ${xmp}_${preanat}
   @chauffeur_afni                             \
       -ulay  ${vanat}                         \
       -prefix   "$odir/${opref}"              \
       -montx 3 -monty 3                       \
       -set_xhairs ON                          \
       -label_mode 1 -label_size 3             \
       -do_clean 

grep -B 100 WOOLY_MAMMOTH_06 do_chauff*tcsh | \
    grep -A 100 FUNNY_OSTRICH_06 > file_MAMMOTH_06.txt

cat <<EOF >> $oscript

|

**Ex. ${idx}**: 3D anatomical volume
-------------------------------------------------------

Simply view the anatomical volume as an underlay by itself.  Might be
useful to check for artifact, coverage, etc.  The full crosshair grid
shows where slices are taken from, and might be useful for seeing the
relative alignment/axialization of the brain.

Unless specified otherwise, the ulay black/white mapping is to 0%/98%
of voxels in the whole volume. The AFNI GUI uses 2%/98% of slicewise
percentiles by default, but since default viewing here is
montage-based, volumewise is implemented by default for consistency
across both the individual view-plane montage as well as across three
view-planes that are created per command execution.


.. hidden-code-block:: Tcsh
   :starthidden: False
   :label: - show code y/n -

EOF

sed '1d;$d' file_MAMMOTH_06.txt >> $oscript

cat <<EOF >> $oscript

.. list-table:: 
   :header-rows: 1
   :widths: 40 40 

   * - Example ${idx}
     -
   * - .. image:: media/${opref}.axi.png
          :width: 100%   
          :align: center
     - .. image:: media/${opref}.sag.png
          :width: 100%   
          :align: center

|

EOF










# ========================================================================

# FUNNY_OSTRICH_09

   @   idx  += 1
   set iiidx = `printf "%03d" $idx`
   set xmp   = "ca${iiidx}"

   set opref = ${xmp}_${preanat}_000
   @chauffeur_afni                             \
       -ulay  ${vanat}                         \
       -prefix   "$odir/${opref}"              \
       -montx 3 -monty 3                       \
       -set_dicom_xyz 0 0 0                    \
       -delta_slices  5 15 10                  \
       -set_xhairs ON                          \
       -label_mode 1 -label_size 3             \
       -do_clean 

grep -B 100 WOOLY_MAMMOTH_09 do_chauff*tcsh | \
    grep -A 100 FUNNY_OSTRICH_09 > file_MAMMOTH_09.txt

cat <<EOF >> $oscript

|

**Ex. ${idx}**: 3D anatomical volume
-------------------------------------------------------

By default, the image slices are set as follows: if there are N total
images in the montage, place N along each axis spaced as evenly as
possible (as done in the previous example).  

However, users can specify either the (x, y, z) or (i, j, k) location
of the central slice, as well as spacing between each of the N slices
(the "delta" number of rows/columns between image slices).  In this
example the central image is placed at the location (x, y, z) = (0, 0,
0), and different slice spacing is specified along different axes.


.. hidden-code-block:: Tcsh
   :starthidden: False
   :label: - show code y/n -

EOF

sed '1d;$d' file_MAMMOTH_09.txt >> $oscript

cat <<EOF >> $oscript

.. list-table:: 
   :header-rows: 1
   :widths: 40 40 

   * - Example ${idx}
     -
   * - .. image:: media/${opref}.axi.png
          :width: 100%   
          :align: center
     - .. image:: media/${opref}.sag.png
          :width: 100%   
          :align: center

|

EOF














# ========================================================================

# FUNNY_OSTRICH_07

   @   idx  += 1
   set iiidx = `printf "%03d" $idx`
   set xmp   = "ca${iiidx}"

   # binarize the skullstripped anatomical, if not already done
   if ( ! -e ${vanatm} ) then
       3dcalc                  \
           -a $vanats          \
           -expr 'step(a)'     \
           -prefix $vanatm
   endif

   set opref = ${xmp}_${preanatm}
   @chauffeur_afni                             \
       -ulay  ${vanat}                         \
       -olay  ${vanatm}                        \
       -opacity 4                              \
       -prefix   "$odir/${opref}"              \
       -montx 3 -monty 3                       \
       -set_xhairs OFF                         \
       -label_mode 1 -label_size 3             \
       -do_clean 

grep -B 100 WOOLY_MAMMOTH_07 do_chauff*tcsh | \
    grep -A 100 FUNNY_OSTRICH_07 > file_MAMMOTH_07.txt

cat <<EOF >> $oscript

|

**Ex. ${idx}**: 3D anatomical volume, olay mask
-------------------------------------------------------

(Going back to evenly spread slices...) Add an overlay with some
transparency to the previous anatomical-- here, a binary mask of the
skullstripped volume to check the quality of the skullstripping
results. The olay color comes from the max of the default colorbar
('Plasma').  The crosshairs have been turned off.


.. hidden-code-block:: Tcsh
   :starthidden: False
   :label: - show code y/n -

EOF

sed '1d;$d' file_MAMMOTH_07.txt >> $oscript

cat <<EOF >> $oscript

.. list-table:: 
   :header-rows: 1
   :widths: 40 40 

   * - Example ${idx}
     -
   * - .. image:: media/${opref}.axi.png
          :width: 100%   
          :align: center
     - .. image:: media/${opref}.sag.png
          :width: 100%   
          :align: center

|

EOF
















# ========================================================================

# FUNNY_OSTRICH_02

   @   idx  += 1
   set iiidx = `printf "%03d" $idx`
   set xmp   = "ca${iiidx}"

   # determine voxelwise stat threshold, using p-to-statistic
   # calculation
   set sthr = `p2dsetstat                             \
                   -inset "${vmod}[${indstat}]"       \
                   -pval $pthr                        \
                   -$ttype                            \
                   -quiet`

   echo "++ The p-value ${pthr} was convert to a stat value of: ${sthr}."

   set opref = ${xmp}_${premod}_${labcoeff}
   @chauffeur_afni                             \
       -ulay  ${vanats}                        \
       -olay  ${vmod}                          \
       -func_range 3                           \
       -cbar Spectrum:red_to_blue              \
       -thr_olay ${sthr}                       \
       -set_subbricks -1 $indcoef $indstat     \
       -opacity 5                              \
       -prefix   "$odir/${opref}"              \
       -montx 3 -monty 3                       \
       -set_xhairs OFF                         \
       -label_mode 1 -label_size 3             \
       -do_clean 

grep -B 100 WOOLY_MAMMOTH_02 do_chauff*tcsh | \
    grep -A 100 FUNNY_OSTRICH_02 > file_MAMMOTH_02.txt


# ---------------------------------------------------------------------

cat <<EOF >> $oscript

|

**Ex. ${idx}**: threshold stats voxelwise, view effects
-------------------------------------------------------

Pretty standard "vanilla mode" of seeing thresholded statistic results
of (task) FMRI modeling.  In AFNI we strongly recommend viewing the
effect estimate ("coef", like the beta in a GLM, for example) as the
olay, and using its associated statistic for voxelwise
thresholding. The range of the functional data is "3", since that
might be a reasonable max/upper response value for this FMRI data that
has been scaled to meaningful BOLD %signal change units; the colorbar
is just the one that is default in AFNI GUI. 

Here, the underlay is just the skullstripped anatomical volume.  Note
that there is a lot of empty space: this might be a reason to use the
\`\`-delta_slices ..\`\` option from above.  Another option would be
to "autobox" the ulay volume, as shown below.

The threshold appropriate for this statistic was generated by
specifying a p-value, and then using the program \`\`p2dsetstat\`\` to
read the header info for that volume and do the p-to-stat conversion.

Note that the slice location is shown in each panel (in a manner
agnostic to the dset's orientation like RAI, LPI, SRA, etc.).

.. hidden-code-block:: Tcsh
   :starthidden: False
   :label: - show code y/n -

EOF

sed '1d;$d' file_MAMMOTH_02.txt >> $oscript

cat <<EOF >> $oscript

.. list-table:: 
   :header-rows: 1
   :widths: 40 40 

   * - Example ${idx}
     -
   * - .. image:: media/${opref}.axi.png
          :width: 100%   
          :align: center
     - .. image:: media/${opref}.sag.png
          :width: 100%   
          :align: center

|

EOF










# =====================================================================
# ========================================================================

# FUNNY_OSTRICH_03

   @   idx  += 1
   set iiidx = `printf "%03d" $idx`
   set xmp   = "ca${iiidx}"

   # Make a nicer looking underlay: unifized and skullstripped
   # anatomical
   if ( ! -e $vanatsu ) then
       3dUnifize -GM -prefix $vanatsu -input $vanats
   endif

   set opref = ${xmp}_${premod}_${labcoeff}
   @chauffeur_afni                             \
       -ulay  ${vanatsu}                       \
       -olay  ${vmod}                          \
       -cbar Reds_and_Blues_Inv                \
       -ulay_range 0% 150%                     \
       -func_range 3                           \
       -thr_olay ${sthr}                       \
       -set_subbricks -1 $indcoef $indstat     \
       -opacity 5                              \
       -prefix   "$odir/${opref}"              \
       -montx 3 -monty 3                       \
       -set_xhairs OFF                         \
       -label_mode 1 -label_size 3             \
       -do_clean 

grep -B 100 WOOLY_MAMMOTH_03 do_chauff*tcsh | \
    grep -A 100 FUNNY_OSTRICH_03 > file_MAMMOTH_03.txt


# ---------------------------------------------------------------------

cat <<EOF >> $oscript

**Ex. ${idx}**: threshold stats voxelwise, view effects, II
-----------------------------------------------------------

Quite similar to the above command and output, with a couple changes:

* the colorbar has been changed, to one that shows pos and neg effects
  separately

* the ulay range has been specified in a way to make it darker-- this
  might be useful to allow more olay colors to stick out; in
  particular, yellows/light colors don't get lost in a white/light
  ulay coloration.

.. hidden-code-block:: Tcsh
   :starthidden: False
   :label: - show code y/n -

EOF

sed '1d;$d' file_MAMMOTH_03.txt >> $oscript

cat <<EOF >> $oscript

.. list-table:: 
   :header-rows: 1
   :widths: 40 40 

   * - Example ${idx}
     -
   * - .. image:: media/${opref}.axi.png
          :width: 100%   
          :align: center
     - .. image:: media/${opref}.sag.png
          :width: 100%   
          :align: center

|

EOF






# =====================================================================
# ========================================================================

# FUNNY_OSTRICH_11

   @   idx  += 1
   set iiidx = `printf "%03d" $idx`
   set xmp   = "ca${iiidx}"

   set opref = ${xmp}_${premod}_${labcoeff}_alpha
   @chauffeur_afni                             \
       -ulay  ${vanatsu}                       \
       -olay  ${vmod}                          \
       -cbar Reds_and_Blues_Inv                \
       -ulay_range 0% 150%                     \
       -func_range 3                           \
       -thr_olay ${sthr}                       \
       -alpha_par Quadratic                    \
       -set_subbricks -1 $indcoef $indstat     \
       -opacity 5                              \
       -prefix   "$odir/${opref}"              \
       -montx 3 -monty 3                       \
       -set_xhairs OFF                         \
       -label_mode 1 -label_size 3             \
       -do_clean 

grep -B 100 WOOLY_MAMMOTH_11 do_chauff*tcsh | \
    grep -A 100 FUNNY_OSTRICH_11 > file_MAMMOTH_11.txt


# ---------------------------------------------------------------------

cat <<EOF >> $oscript

**Ex. ${idx}**: threshold stats voxelwise, view effects, III
------------------------------------------------------------

Another take on thresholding: one without being so strict, and showing
more of the data.  For example, it might be quite informative to still
see some of the "near misses" in the data.  

One can soften the ON/OFF binarization of thresholding, by decreasing
the "alpha" level (or opacity) of sub-threshold voxels in a continuous
manner: either quadratically (used here) or linearly (less steep
decline in visibility).  The black outline still highlights the
suprathreshold locations nicely.



.. hidden-code-block:: Tcsh
   :starthidden: False
   :label: - show code y/n -

EOF

sed '1d;$d' file_MAMMOTH_11.txt >> $oscript

cat <<EOF >> $oscript

.. list-table:: 
   :header-rows: 1
   :widths: 40 40 

   * - Example ${idx}
     -
   * - .. image:: media/${opref}.axi.png
          :width: 100%   
          :align: center
     - .. image:: media/${opref}.sag.png
          :width: 100%   
          :align: center

|

EOF














# =====================================================================
# =====================================================================

# FUNNY_OSTRICH_04

   @   idx  += 1
   set iiidx = `printf "%03d" $idx`
   set xmp   = "ca${iiidx}"

   set opref = ${xmp}_${premod}
   3dClusterize                                \
       -overwrite                              \
       -echo_edu                               \
       -inset   ${vmod}                        \
       -ithr    $indstat                       \
       -idat    $indcoef                       \
       -$ttype  "p=$pthr"                      \
       -NN             1                       \
       -clust_nvox     200                     \
       -pref_map       ${premod}_map.nii.gz    \
       -pref_dat       ${premod}_EE.nii.gz     \
     > ${premod}_report.txt

   @chauffeur_afni                             \
       -ulay  $vanatsu                         \
       -olay  ${premod}_EE.nii.gz              \
       -cbar Reds_and_Blues_Inv                \
       -ulay_range 0% 150%                     \
       -func_range 3                           \
       -opacity 5                              \
       -prefix   "$odir/${opref}"              \
       -montx 3 -monty 3                       \
       -set_xhairs OFF                         \
       -label_mode 1 -label_size 3             \
       -do_clean 

grep -B 100 WOOLY_MAMMOTH_04 do_chauff*tcsh | \
    grep -A 100 FUNNY_OSTRICH_04 > file_MAMMOTH_04.txt

# ---------------------------------------------------------------------

cat <<EOF >> $oscript

**Ex. ${idx}**: threshold stats voxelwise + clusterize, view effects
------------------------------------------------------------------------

The previous examples were just thresholded voxelwise. This used
\`\`3dClusterize\`\` to add in cluster-volume thresholding to this;
the program generates both the effect estimate volume ("EE") as well
as a map of the clusters ("map", has a different integer per ROI,
sorted by size) produced by the dual thresholding.  The clustersize of
200 voxels was just chosen arbitrarily (but could be calculated for
real data with \`\`3dClustSim\`\`, for example).

Comment on \`\`3dClusterize\`\` usage: if you have a mask in the
header of the stats file, then you can add an opt "-mask_from_hdr" to
this command to read it directly from the header, similar to usage in
the GUI.

The rest of the visualization aspects of the EE volume here are pretty
similar to the preceding.

.. hidden-code-block:: Tcsh
   :starthidden: False
   :label: - show code y/n -

EOF

sed '1d;$d' file_MAMMOTH_04.txt >> $oscript

cat <<EOF >> $oscript

.. list-table:: 
   :header-rows: 1
   :widths: 40 40 

   * - Example ${idx}
     -
   * - .. image:: media/${opref}.axi.png
          :width: 100%   
          :align: center
     - .. image:: media/${opref}.sag.png
          :width: 100%   
          :align: center

|

EOF











# =====================================================================

# FUNNY_OSTRICH_08

   @   idx  += 1
   set iiidx = `printf "%03d" $idx`
   set xmp   = "ca${iiidx}"

   # Save space: autobox
   if ( ! -e $vanatsub ) then
       3dAutobox -prefix $vanatsub -npad 7 -input $vanatsu
   endif


   set opref = ${xmp}_${premod}
   3dClusterize                                \
       -overwrite                              \
       -echo_edu                               \
       -inset   ${vmod}                        \
       -ithr    $indstat                       \
       -idat    $indcoef                       \
       -$ttype  "p=$pthr"                      \
       -NN             1                       \
       -clust_nvox     200                     \
       -pref_map       ${premod}_map.nii.gz    \
       -pref_dat       ${premod}_EE.nii.gz     \
     > ${premod}_report.txt

   @chauffeur_afni                             \
       -ulay  $vanatsub                        \
       -olay  ${premod}_EE.nii.gz              \
       -cbar Reds_and_Blues_Inv                \
       -ulay_range 0% 150%                     \
       -func_range 3                           \
       -opacity 5                              \
       -prefix   "$odir/${opref}"              \
       -montx 3 -monty 3                       \
       -set_xhairs OFF                         \
       -label_mode 1 -label_size 3             \
       -do_clean 

grep -B 100 WOOLY_MAMMOTH_08 do_chauff*tcsh | \
    grep -A 100 FUNNY_OSTRICH_08 > file_MAMMOTH_08.txt

# ---------------------------------------------------------------------

cat <<EOF >> $oscript

**Ex. ${idx}**: threshold stats voxelwise + clusterize, view effects, II
------------------------------------------------------------------------

Same olay as above, but just autobox the ulay for a smaller FOV
that has less empty space ("autoboxed" with a wee bit of padding).

.. hidden-code-block:: Tcsh
   :starthidden: False
   :label: - show code y/n -

EOF

sed '1d;$d' file_MAMMOTH_08.txt >> $oscript

cat <<EOF >> $oscript

.. list-table:: 
   :header-rows: 1
   :widths: 40 40 

   * - Example ${idx}
     -
   * - .. image:: media/${opref}.axi.png
          :width: 100%   
          :align: center
     - .. image:: media/${opref}.sag.png
          :width: 100%   
          :align: center

|

EOF













# =====================================================================

# FUNNY_OSTRICH_05

   @   idx  += 1
   set iiidx = `printf "%03d" $idx`
   set xmp   = "ca${iiidx}"

   set opref = ${xmp}_${premod}
   @chauffeur_afni                             \
       -ulay  ${vanatsub}                      \
       -olay  ${premod}_map.nii.gz             \
       -ulay_range 0% 150%                     \
       -cbar ROI_i64                           \
       -pbar_posonly                           \
       -opacity 6                              \
       -zerocolor white                        \
       -label_color "blue"                     \
       -blowup 1                               \
       -prefix   "$odir/${opref}"              \
       -montx 3 -monty 3                       \
       -set_xhairs OFF                         \
       -label_mode 1 -label_size 3             \
       -do_clean 

grep -B 100 WOOLY_MAMMOTH_05 do_chauff*tcsh | \
    grep -A 100 FUNNY_OSTRICH_05 > file_MAMMOTH_05.txt

# ---------------------------------------------------------------------

cat <<EOF >> $oscript

**Ex. ${idx}**: view ROIs (here, cluster maps)
---------------------------------

Here we view the cluster map of the clusterized data. Each ROI is
"labelled" in the data by having a different integer volume, and the
colorbar used now could accommodate the visualization of up to 64
clusters (there are other integer-appropriate colorbars that go up
higher).

Oh, and the background color of zero-valued ulay voxels can be
changed, along with the labelcolor.  

The resolution at which the images are saved is controlled by the
"blowup factor".  By default, the resampling mode of the dsets is just
NN, so that datasets aren't blurred, and as the olay is resampled to
match the ulay resolution the results are not distorted or smoothed
artificially (and integers would stay integers).  This also has a bit
of interaction with how the labels look.  Larger blow-up factors might
not affect how the brain images appear, but they will affect how the
labels look: higher blowup factors leading to finer labels (which may
be harder to read on some screens, depending on settings/programs,
though on paper they would look nicer).  Larger blowup factors might
be necessary for making images to submit as journal figures.  Lots of
things to consider.

.. hidden-code-block:: Tcsh
   :starthidden: False
   :label: - show code y/n -

EOF

sed '1d;$d' file_MAMMOTH_05.txt >> $oscript

cat <<EOF >> $oscript

.. list-table:: 
   :header-rows: 1
   :widths: 40 40 

   * - Example ${idx}
     -
   * - .. image:: media/${opref}.axi.png
          :width: 100%   
          :align: center
     - .. image:: media/${opref}.sag.png
          :width: 100%   
          :align: center

|

EOF








# =====================================================================

# FUNNY_OSTRICH_10

   @   idx  += 1
   set iiidx = `printf "%03d" $idx`
   set xmp   = "ca${iiidx}"

   if ( ! -e ${vepie} ) then
        3dedge3 -prefix ${vepie} -input ${vepi}'[0]'
   endif

   set opref = ${xmp}_${premod}
   @chauffeur_afni                             \
       -ulay  ${vanatsub}                      \
       -olay  ${vepie}                         \
       -ulay_range 0% 150%                     \
       -func_range_perc 50                     \
       -pbar_posonly                           \
       -cbar "red_monochrome"                  \
       -opacity 6                              \
       -prefix   "$odir/${opref}"              \
       -montx 3 -monty 3                       \
       -set_xhairs OFF                         \
       -label_mode 1 -label_size 3             \
       -do_clean 

grep -B 100 WOOLY_MAMMOTH_10 do_chauff*tcsh | \
    grep -A 100 FUNNY_OSTRICH_10 > file_MAMMOTH_10.txt

# ---------------------------------------------------------------------

cat <<EOF >> $oscript

**Ex. ${idx}**: check alignment with edge view
----------------------------------------------

Check out the alignment between two volumes by making and "edge-ified"
version of one and overlaying it on the other.  This is *quite* useful
in many occasions.  (Note that this is also the purpose of
\`\`@snapshot_volreg\`\`, which is also discussed
:ref:\`in this tutorial section here <tut_auto_@snapshot_volreg>\`.)

Users can then check the alignment of pertinent things: tissue
boundaries, matching structures, etc.  

Note that in the present case the EPI **hadn't** been aligned to the
anatomical yet, so we might not expect great alignment in the present
scenario (it's basically just a question of how much the subject might
have moved betwixt scans).  The EPI has also relatively low contrast
and spatial resolution, so that the lines are fairly course-- much
more so than if two anatomicals were viewed in this way.  There are
tricks that one can play to enhance the features of the EPI for such
viewing, but that is a larger sidenote (and most readers have likely
rightfully given up detailed reading by this point in the webpage).


.. hidden-code-block:: Tcsh
   :starthidden: False
   :label: - show code y/n -

EOF

sed '1d;$d' file_MAMMOTH_10.txt >> $oscript

cat <<EOF >> $oscript

.. list-table:: 
   :header-rows: 1
   :widths: 40 40 

   * - Example ${idx}
     -
   * - .. image:: media/${opref}.axi.png
          :width: 100%   
          :align: center
     - .. image:: media/${opref}.sag.png
          :width: 100%   
          :align: center

|

EOF











# =====================================================================

# FUNNY_OSTRICH_12

   @   idx  += 1
   set iiidx = `printf "%03d" $idx`
   set xmp   = "ca${iiidx}"

   # just taking a subset of the time series for this example
   if ( ! -e ${vepip} ) then
        3dcalc -a ${vepi}'[0..16]' -expr 'a' -prefix ${vepip}
   endif

   set opref = ${xmp}_${preepip}
   @chauffeur_afni                             \
       -ulay  ${vepip}                         \
       -mode_4D                                \
       -image_label_ijk                        \
       -prefix   "$odir/${opref}"              \
       -blowup 4                               \
       -set_xhairs OFF                         \
       -label_mode 1 -label_size 3             \
       -do_clean 


grep -B 100 WOOLY_MAMMOTH_12 do_chauff*tcsh | \
    grep -A 100 FUNNY_OSTRICH_12 > file_MAMMOTH_12.txt

# ---------------------------------------------------------------------

cat <<EOF >> $oscript

**Ex. ${idx}**: 4D mode
----------------------------------------------

This program can also look at one slice across time, using the
\`\`-mode_4D\`\`\ flag-- in the present example, looking at one slice
across the first 17 time points.  This might be useful, for example,
to look for distortions across time (e.g., dropout slices, severe
motion or EPI distortion). 

By default, a slice is chosen hear the center of the volume's FOV, but
users may specify the location.

Here, the per-slice "xyz" label would not represent the location in
space; instead, we use the \`\`-image_label_ijk\`\` option to specify
which [n]th volume we are viewing in the time series, starting with
[0]. 

.. hidden-code-block:: Tcsh
   :starthidden: False
   :label: - show code y/n -

EOF

sed '1d;$d' file_MAMMOTH_12.txt >> $oscript

cat <<EOF >> $oscript

.. list-table:: 
   :header-rows: 1
   :widths: 40 40 

   * - Example ${idx}
     -
   * - .. image:: media/${opref}.axi.png
          :width: 100%   
          :align: center
     - .. image:: media/${opref}.sag.png
          :width: 100%   
          :align: center

|

EOF








# =====================================================================
# =====================================================================
# =====================================================================

# clean up intermediate files
\rm file_MAMMOTH*txt

echo "\n\n++ Done!\n\n"




