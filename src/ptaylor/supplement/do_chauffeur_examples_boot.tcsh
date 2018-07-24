#!/bin/tcsh

# This script will partially generate help pages, but not fully.  Wah.

set oscript = auto_@chauffeur_afni.rst
printf "" > $oscript

# -------------------------------------------------------------------------

# FUNNY_OSTRICH_01

   set here     = $PWD                                  
   set idir     = $here                                 # dir with data

   set vanat    = $idir/anat+orig                       # anatomical vol
   set vanatuni = $idir/anat_uni.nii.gz                 # anat. unifized
   set vmod     = $idir/func_slim+orig                  # output model results
   set premod   = `3dinfo -prefix_noext "${vmod}"`      # vol prefix (for fname)
   set odir     = QC

   set indcoef  = 3                                     # effect estimate
   set indstat  = 4                                     # stat of ee
   set labstat  = `3dinfo -label "${vmod}[${indstat}]"` # str label of ee
   set labcoef  = `3dinfo -label "${vmod}[${indcoef}]"` # str label of ee-stat
   set labstatf = "${labstat:gas/#/_/}"                 # str: safe for fname
   set labcoeff = "${labcoef:gas/#/_/}"                 # str: safe for fname

   set pthr     = 0.001                                 # voxelwise threshold
   set ttype    = "bisided"                             # {1,2,bi}sided

   \mkdir -p $odir

grep -B 100 WOOLY_MAMMOTH_01 do_chauff*tcsh | \
    grep -A 100 FUNNY_OSTRICH_01 > file_MAMMOTH_01.txt

# ========================================================================
# ========================================================================
# ========================================================================

cat <<EOF >> $oscript
.. _tut_auto_@chauffeur_afni:

Using @chauffeur_afni
=====================

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
--------

We present some examples of auto-image making with
\`\`@chauffeur_afni\`\` using publicly available data, distributed as
part of the freely downloadable AFNI Bootcamp demo sets (see
:ref:\`here <Bootcamping>\`).  Specifically, these commands refer to
files in the \`\`AFNI_data6/afni/\`\` directory therein.

The code snippets could all be run in a single script file-- the
variable names go together.  Such a file could be saved and run in the
same \`\`AFNI_data6/afni/\`\` directory. 

Some supplementary data sets are generated and put into the "present"
input directory, and the images themselves are put into a subdirectory
that is created called \`\`QC/\`\`. Several of the examples include
thresholding statistical output data sets, and therefore include run
additional AFNI programs like \`\`3dClusterize\`\`, etc.  And at no
extra charge!

For space considerations here:

* only axial and sagittal images are displayed here (not the coronal,
  even though they are made automatically from the same
  \`\`@chauffeur_afni\`\` command)

* most of the montages are just :math:\`3\times3\`; for real practical
  usage, sometimes, it might useful to make them, say,
  :math:\`5\times3\`.

The following are some variables relevant for the below commands
(\`\`tcsh\`\` syntax):

.. code-block:: Tcsh

EOF

sed '1d;$d' file_MAMMOTH_01.txt >> $oscript

# ========================================================================

# FUNNY_OSTRICH_02

      set idx   = 0
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
          -ulay  $vanat                           \
          -olay  $vmod                            \
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

0. **Example ${idx}**: threshold stats voxelwise, view effects

   Pretty standard "vanilla mode" of seeing thresholded statistic
   results of (task) FMRI modeling.  In AFNI we strongly recommend
   viewing the effect estimate ("coef", like the beta in a GLM, for
   example) as the olay, and using its associated statistic for
   voxelwise thresholding. The range of the functional data is "3",
   since that might be a reasonable max/upper response value for this
   FMRI data that has been scaled to meaningful BOLD %signal change
   units; the colorbar is just the one that is default in AFNI
   GUI. Here, the underlay is just an anatomical volume.

   The threshold appropriate for this statistic was generated by
   specifying a p-value, and then using the program \`\`p2dsetstat\`\`
   to read the header info for that volume and do the p-to-stat
   conversion.

   Note that the slice location is shown in each panel (in a manner
   agnostic to the dset's orientation like RAI, LPI, SRA, etc.).

   .. code-block:: Tcsh
 
EOF

sed '1d;$d' file_MAMMOTH_02.txt >> $oscript

cat <<EOF >> $oscript

   .. list-table:: 
      :header-rows: 1
      :widths: 50 50 

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

      # Make a nicer looking underlay: unified anatomical
      if ( ! -e $vanatuni ) then
          3dUnifize -GM -prefix $vanatuni -input $vanat
      endif

      set opref = ${xmp}_${premod}_${labcoeff}
      @chauffeur_afni                             \
          -ulay  $vanatuni                        \
          -olay  $vmod                            \
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

#. **Example ${idx}**: threshold stats voxelwise, view effects, II

   Quite similar to the above command and output, with a couple changes:

   * the colorbar has been changed, to one that shows pos and neg
     effects separately

   * the ulay range has been specified in a way to make it darker--
     this might be useful to allow more olay colors to stick out; in
     particular, yellows/light colors don't get lost in a white/light
     ulay coloration.

   .. code-block:: Tcsh

EOF

sed '1d;$d' file_MAMMOTH_03.txt >> $oscript

cat <<EOF >> $oscript

   .. list-table:: 
      :header-rows: 1
      :widths: 50 50 

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
          -ulay  $vanatuni                        \
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

#. **Example ${idx}**: threshold stats voxelwise + clusterize, view effects

   The previous examples were just thresholded voxelwise. This used
   \`\`3dClusterize\`\` to add in cluster-volume thresholding to this;
   the program generates both the effect estimate volume ("EE") as
   well as a map of the clusters ("map", has a different integer per
   ROI, sorted by size) produced by the dual thresholding.  The
   clustersize of 200 voxels was just chosen arbitrarily (but could be
   calculated for real data with \`\`3dClustSim\`\`, for example).

   Comment on \`\`3dClusterize\`\` usage: if you have a mask in the
   header of the stats file, then you can add an opt "-mask_from_hdr"
   to this command to read it directly from the header, similar to
   usage in the GUI.

   The rest of the visualization aspects of the EE volume here are
   pretty similar to the preceding.

   .. code-block:: Tcsh

EOF

sed '1d;$d' file_MAMMOTH_04.txt >> $oscript

cat <<EOF >> $oscript

   .. list-table:: 
      :header-rows: 1
      :widths: 50 50 

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
          -ulay  $vanatuni                        \
          -olay  ${premod}_map.nii.gz             \
          -ulay_range 0% 150%                     \
          -cbar ROI_i64                           \
          -pbar_posonly                           \
          -opacity 6                              \
          -prefix   "$odir/${opref}"              \
          -montx 3 -monty 3                       \
          -set_xhairs OFF                         \
          -label_mode 1 -label_size 3             \
          -do_clean 

grep -B 100 WOOLY_MAMMOTH_05 do_chauff*tcsh | \
    grep -A 100 FUNNY_OSTRICH_05 > file_MAMMOTH_05.txt

# ---------------------------------------------------------------------

cat <<EOF >> $oscript

#. **Example ${idx}**: view cluster maps

   Here we view the cluster map of the clusterized data. Each ROI is
   "labelled" in the data by having a different integer volume, and
   the colorbar used now could accommodate the visualization of up to
   64 clusters (there are other integer-appropriate colorbars that go
   up higher).

   .. code-block:: Tcsh

EOF

sed '1d;$d' file_MAMMOTH_05.txt >> $oscript

cat <<EOF >> $oscript

   .. list-table:: 
      :header-rows: 1
      :widths: 50 50 

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

echo "\n\n++ Done!\n\n"




