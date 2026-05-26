#!/bin/tcsh

# A script for comparison mask overlaps properties
# 
# ============================================================================

@global_parse `basename $0` "$*" ; if ($status) exit 0

#set version   = "0.1";  set rev_dat   = "May 24, 2026"
# + start of program
#
set version   = "1.0";  set rev_dat   = "May 25, 2026"
# + add in many options, more complete behavior and outputs, and
#   help description
#
# ----------------------------------------------------------------

set this_prog = "compare_mask_overlap.tcsh"
set prog_abbr = "CMO"
set here      = $PWD

# ----------------------- set defaults --------------------------

set inputA    = ""
set inputB    = ""
set ulay      = ""

set outdir    = ""
set prefix    = ""
set opref     = "overlap"
set orep      = "${opref}_report.txt"

set rim_dep   = 5.0                      # mm value for rim depth
set ADTO      = 0                        # add data into outdir?

set overwrite = ""
set wdir      = ""
set DO_CLEAN  = 1           

# ------------------- process options, a la rr ----------------------

if ( $#argv == 0 ) goto SHOW_HELP

set ac = 1
while ( $ac <= $#argv )
    # terminal options
    if ( ("$argv[$ac]" == "-h" ) || ("$argv[$ac]" == "-help" )) then
        goto SHOW_HELP
    endif
    if ( "$argv[$ac]" == "-ver" ) then
        goto SHOW_VERSION
    endif

    if ( "$argv[$ac]" == '-echo' ) then
        set echo

    # --------- required

    else if ( "$argv[$ac]" == "-inputA" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set inputA = "$argv[$ac]"

    else if ( "$argv[$ac]" == "-inputB" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set inputB = "$argv[$ac]"

    else if ( "$argv[$ac]" == "-outdir" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set outdir   = "$argv[$ac]"

    else if ( "$argv[$ac]" == "-prefix" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set prefix = "$argv[$ac]"
        set opref  = `basename "${prefix}"`

        # prefix should have no path
        if ( "${prefix}" != "${opref}" ) then
            echo "+* WARNING: -prefix should have _no_ path info in it,"
            echo "   bc it is output into the -outdir location."
            echo "   We will just use '${opref}' instead."
        endif

    # --------- opt

    else if ( "$argv[$ac]" == "-ulay" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set ulay = "$argv[$ac]"

    else if ( "$argv[$ac]" == "-rim_dep" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set rim_dep = "$argv[$ac]"

    else if ( "$argv[$ac]" == "-workdir" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set wdir = "$argv[$ac]"

        set tf = `python -c "print('/' in '${wdir}')"`
        if ( "${tf}" == "True" ) then
            echo "** ERROR: '-workdir ..' is a name only, no '/' allowed\n"
            goto BAD_EXIT
        endif

    else if ( "$argv[$ac]" == "-add_data_to_outdir" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set tmp = "$argv[$ac]"
        if ( "${tmp}" == "Yes" || "${tmp}" == "1" ) then
            set ADTO = 1
        else if ( "${tmp}" == "No" || "${tmp}" == "0" ) then
            set ADTO = 0
        else
            echo "** ERROR: bad value after -data_above_outdir: ${tmp}"
            echo "   Must be one of: Yes, 1, No, 0"
            goto BAD_EXIT
        endif

    else if ( "$argv[$ac]" == "-overwrite" ) then
        set overwrite = "-overwrite"
        
    else if ( "$argv[$ac]" == "-no_clean" ) then
        set DO_CLEAN = 0
        
    else
        echo "\n\n** ERROR: unexpected option #$ac = '$argv[$ac]'\n\n"
        goto BAD_EXIT
        
    endif
    @ ac += 1
end

# =======================================================================
# ======================== ** Verify + setup ** =========================
# =======================================================================

if ( "${outdir}" == "" ) then
    echo "** ERROR: need to provide output dir name with '-outdir ..'"
    goto BAD_EXIT
endif

if ( "${inputA}" == "" ) then
    echo "** ERROR: need to provide input dataset with '-inputA ..'"
    goto BAD_EXIT
endif

if ( "${inputB}" == "" ) then
    echo "** ERROR: need to provide input dataset with '-inputB ..'"
    goto BAD_EXIT
endif

# make workdir name, if nec
if ( "${wdir}" == "" ) then
    set tmp_code = `3dnewid -fun11`  # should be essentially unique hash
    set wdir     = __workdir_${prog_abbr}_${tmp_code}
endif

# check output directory, use input one if nothing given
if ( ! -e "${outdir}" ) then
    echo "++ Making new output directory: $outdir"
    \mkdir -p "${outdir}"
else if ( -e "${outdir}" && "${overwrite}" != "-overwrite" ) then
    echo "** ERROR: outdir exists already: ${outdir}"
    echo "   Either remove it, or add '-overwrite'"
    goto BAD_EXIT
endif

# make the working directory
if ( ! -e "${outdir}/${wdir}" ) then
    echo "++ Making working directory: ${outdir}/${wdir}"
    \mkdir -p "${outdir}/${wdir}"
else
    echo "+* WARNING:  Somehow found a premade working directory (?):"
    echo "      ${outdir}/${wdir}"
endif

# this needs to be (re)set, in case user entered -prefix
set orep = "${opref}_report.txt"

# =======================================================================
# =========================== ** Main work ** ===========================
# =======================================================================

# ----- copy over input* to wdir, maskifying each

echo "++ copy inputs to wdir"

3dcalc                                                                       \
    -overwrite                                                               \
    -a          "${inputA}"                                                  \
    -expr       "bool(a)"                                                    \
    -prefix     ${outdir}/${wdir}/dset_00_maskA.nii.gz                       \
    -datum      short                                                        \
    -nscale

if ( $status ) then
    goto BAD_EXIT
endif

3dcalc                                                                       \
    -overwrite                                                               \
    -a          "${inputB}"                                                  \
    -expr       "bool(a)"                                                    \
    -prefix     ${outdir}/${wdir}/dset_00_maskB.nii.gz                       \
    -datum      short                                                        \
    -nscale

if ( $status ) then
    goto BAD_EXIT
endif

if ( "${ulay}" != "" ) then
    3dcalc                                                                   \
        -overwrite                                                           \
        -a          "${ulay}"                                                \
        -expr       "a"                                                      \
        -prefix     ${outdir}/${wdir}/dset_00_ulay.nii.gz                    \
        -datum      short                                                    \
        -nscale

    if ( $status ) then
        goto BAD_EXIT
    endif
endif

# move into wdir
cd ${outdir}/${wdir}

# ----- make core+rim masks

echo "++ Make core+rim masks"

# from maskA, make inner and outer rim...
3dDepthMap                                                                   \
    -overwrite                                                               \
    -bounds_are_not_zero                                                     \
    -rimify              ${rim_dep}                                          \
    -input               "3dcalc( -a dset_00_maskA.nii.gz -expr a+3*not(a) )" \
    -prefix              dset_01_rim.nii.gz

if ( $status ) then
    goto BAD_EXIT
endif

# ... and finalize by adding core
3dcalc                                                                       \
    -overwrite                                                               \
    -a          dset_00_maskA.nii.gz                                         \
    -b          dset_01_rim.nii.gz                                           \
    -expr       "a+b"                                                        \
    -prefix     dset_02_rimcore.nii.gz

if ( $status ) then
    goto BAD_EXIT
endif

# at this point, dset_02_rimcoreA.nii.gz has 3 values to be used in masking:
#     1 : core
#     2 : inner rim
#     3 : outer rim

# ----- make the report of overlaps/dice in core and rim

# the 'outrim' uses a different func, because we invert the check
# there---there should be nothing, so we 'count' the errors by
# inverting the masks and treating it like dice calc in the same way
set all_label = ( "core" "inrim" "outrim" "overall" )
set all_func  = ( "step" "step"  "not" "step" )

# ... and prepare to output a report
printf "" > ${orep}

# put input filenames at the top
set bnameA = `basename "${inputA}"`
set bnameB = `basename "${inputB}"`
set ulname = `basename "${ulay}"`

if ( "${ulname}" == "" ) then
    set ulname = "None"
endif

printf "%-30s : %-s \n"     "dset inputA" "${bnameA}" >> ${orep}
printf "%-30s : %-s \n"     "dset inputB" "${bnameB}" >> ${orep}
printf "%-30s : %-s \n"     "dset ulay"   "${ulname}" >> ${orep}
printf "\n"   >> ${orep}

foreach ii ( `seq 1 1 ${#all_label}` )
    # NB: the value of ii also selects the region within the mask
    set label  = "${all_label[${ii}]}"
    set func   = "${all_func[${ii}]}"
    
    if ( "${label}" == "overall" ) then
        # overall calc : calc everywhere
        set use_b = "(bool(b)+not(bool(b)))"
    else
        # use one of core/rim regions
        set use_b = "step(b)"
    endif

    # limit each mask to region of interest
    3dcalc                                                                   \
        -overwrite                                                           \
        -a          dset_00_maskA.nii.gz                                     \
        -b          dset_02_rimcore.nii.gz"<${ii}>"                          \
        -expr       "${func}(a) * ${use_b}"                                  \
        -prefix     dset_03_maskA_${label}.nii.gz

    if ( $status ) then
        goto BAD_EXIT
    endif

    3dcalc                                                                   \
        -overwrite                                                           \
        -a          dset_00_maskB.nii.gz                                     \
        -b          dset_02_rimcore.nii.gz"<${ii}>"                          \
        -expr       "${func}(a) * ${use_b}"                                  \
        -prefix     dset_03_maskB_${label}.nii.gz

    if ( $status ) then
        goto BAD_EXIT
    endif

    # do calcs
    3dABoverlap                                                              \
        -no_automask                                                         \
        dset_03_maskA_${label}.nii.gz                                        \
        dset_03_maskB_${label}.nii.gz                                        \
        > __tmp_olap_${ii}_${label}.txt

    # ... and interpret outputs
    set all_val   = `tail -n 1 __tmp_olap_${ii}_${label}.txt`
    set nvoxA     = ${all_val[1]}   # #A, total nvox in A  
    set nvoxB     = ${all_val[2]}   # #B, total nvox in B
    set union     = ${all_val[3]}   # #(A uni B), set union (A or B)
    set inter     = ${all_val[4]}   # #(A int B), set intersection (A and B)
    set AnotB     = ${all_val[5]}   # #(A \ B), nvox in A that aren't in B
    set BnotA     = ${all_val[6]}   # #(B \ A), nvox in B that aren't in A
    set AnotBperc = ${all_val[7]}   # %(A \ B), nvox in A that aren't in B
    set BnotAperc = ${all_val[8]}   # %(B \ A), nvox in B that aren't in A
    # ... and we ignore the radii of gyration estimates

    # And roll with the Dice:  2*intersection/(sum of individual masks)
    if ( "${nvoxA}" != "0" || "${nvoxB}" != "0" ) then
        set dice = `ccalc -expr "2.0*${inter} / (${nvoxA}+${nvoxB})"`
    else 
        set dice = "0.0"
    endif

    if ( "${label}" == "outrim" ) then
        set key = "outrim inv"
    else
        set key = "${label}"
    endif

    printf "%-30s : %d \n"     "${key} nvoxA" "${nvoxA}" >> ${orep}
    printf "%-30s : %d \n"     "${key} nvoxB" "${nvoxB}" >> ${orep}
    printf "%-30s : %d \n"     "${key} union" "${union}" >> ${orep}
    printf "%-30s : %d \n"     "${key} intersection" "${inter}" >> ${orep}
    printf "%-30s : %d \n"     "${key} AnotB" "${AnotB}" >> ${orep}
    printf "%-30s : %d \n"     "${key} BnotA" "${BnotA}" >> ${orep}
    printf "%-30s : %-0.5f \n" "${key} AnotB perc" "${AnotBperc}" >> ${orep}
    printf "%-30s : %-0.5f \n" "${key} BnotA perc" "${BnotAperc}" >> ${orep}
    printf "%-30s : %-0.5f \n" "${key} Dice" "${dice}"   >> ${orep}
    printf "\n"   >> ${orep}

end

\cp ${orep} ../.

# ----- do QC images

# ... if we have a ulay provided
if ( "${ulay}" == "" ) then
    echo "+* No '-ulay ..' dset provided for QC images, so skipping them."
else
    # Make a convenient (if weirdly enumerated) dset for overlap and
    # display:
    #      18 -> blue : overlap of masks
    #      24 -> red   : yes inputA, no  inputB
    #      26 -> green : no  inputA, yes inputB
    3dcalc                                                                   \
        -overwrite                                                           \
        -a          dset_00_maskA.nii.gz                                     \
        -b          dset_00_maskB.nii.gz                                     \
        -expr       "18*a*b + 26*not(a)*b + 24*a*not(b)"                     \
        -prefix     dset_10_olapAB.nii.gz

    if ( $status ) then
        exit goto BAD_EXIT
    endif

    # convenient, in case dset gets opened separately
    3drefit -cmap INT_CMAP dset_10_olapAB.nii.gz

    # ========================================================================

    # Make a montage of the zeroth brick of each dset

    # bc @chauffeur_afni can output a run script that refers to the
    # ulay/olay dsets and allow the AFNI GUI to load them with the
    # specified settings, we do a bit of fanciness for the special
    # case that the provided dsets are just above the outdir, if the
    # users chooses

    if ( ${ADTO} ) then
        # cp dsets to outdir, which is just above wdir
        set dset_ulay = ${opref}_dset_ulay.nii.gz
        set dset_olay = ${opref}_dset_olapAB.nii.gz

        3dcopy -overwrite dset_00_ulay.nii.gz   ../${dset_ulay}
        3dcopy -overwrite dset_10_olapAB.nii.gz ../${dset_olay}

        if ( $status ) then
            echo "** ERROR: failed to cp data to outdir"
            goto BAD_EXIT
        endif

        # move out of wdir to outdir
        cd ..
    else 
        set dset_ulay = dset_00_ulay.nii.gz  
        set dset_olay = dset_10_olapAB.nii.gz  
    endif

    set chauff_pref = IMG                     # temp/remove
    set imrun_pref  = "${opref}_run.tcsh"     # keep
    set imcat_pref  = "${opref}_QC.jpg"       # keep

    @chauffeur_afni                                                          \
        -ulay              "${dset_ulay}"                                    \
        -olay              "${dset_olay}"                                    \
        -box_focus_slices  "${dset_olay}"                                    \
        -thr_olay          40                                                \
        -olay_alpha        Linear                                            \
        -cbar_ncolors      3                                                 \
        -cbar_topval       ""                                                \
        -cbar              "40=green 25=rbgyr20_20 20=lt-blue2 0=none"       \
        -prefix            "${chauff_pref}"                                  \
        -pbar_posonly                                                        \
        -func_range        40                                                \
        -set_xhairs        OFF                                               \
        -montx             8                                                 \
        -monty             1                                                 \
        -opacity           9                                                 \
        -label_mode        1                                                 \
        -label_size        3                                                 \
        -cmd2script        "${imrun_pref}"                                   \
        -c2s_text          'View mask comparison'                            \
        -c2s_mont_1x1

    if ( $status ) then
        goto BAD_EXIT
    endif

    # Concatenate images across all 3 sliceviews.

    set pref_cat = ALL_IMG.jpg

    2dcat                                                                    \
        -gap      5                                                          \
        -gap_col  150 180 150                                                \
        -nx       1                                                          \
        -ny       3                                                          \
        -prefix   "${imcat_pref}"                                            \
        "${chauff_pref}"*{axi,cor,sag}*png

    if ( $status ) then
        goto BAD_EXIT
    endif

    # clean up chauff_pref
    \rm "${chauff_pref}"*.png

    if ( ${ADTO} ) then
        # return to wdir for any further steps
        cd -
    else
        # cp the final image up
        cp "${imcat_pref}" ../.
    endif

endif

# ---------------------------------------------------------------------

# move out of wdir to the outdir
cd ..
set whereout = $PWD

if ( $DO_CLEAN == 1 ) then
    echo "++ Clean working dir"
    \rm -rf ${wdir}
else
    echo "++ NOT removing temporary working dir: ${wdir}"
endif

cat <<EOF

++ DONE. See the output:
   $whereout/${opref}"

EOF

goto GOOD_EXIT

# ========================================================================
# ========================================================================

SHOW_HELP:
cat << EOF
-------------------------------------------------------------------------
Overview ~1~

This program facilitates comparing two different mask datasets, with a
particular focus on whole brain masks. It makes a report of relevant
overlap-related quantities, as well as a montage image comparing the
masks.

auth    : PA Taylor (SSCC, NIMH, NIH, USA)
          RC Reynolds (SSCC, NIMH, NIH, USA)
          Y Narayana Swamy (SSCC, NIMH, NIH, USA)
ver     : ${version}
revdate : ${rev_dat}

-------------------------------------------------------------------------
Options ~1~

-inputA      IA    :(req) input dataset, the primary one

-inputB      IB    :(req) input dataset, the secondary one

-outdir      000   :(req) output dir name 

-ulay        UU    :if this option is used, then a QC image is made of the
                    masks and their overlap as overlay, and this dset UU
                    is the underlay
                    NB: it is highly recommended to include this dset, to
                    have the QC images made, too

-rim_dep     RD    :the depth (in units of mm) of the inner and outer rim 
                    regions
                    (def: ${rim_dep})

-add_data_to_outdir ADTO
                   :should copies of the underlay and mask-overlap dsets be
                    added to the outdir? If so, a driver script for AFNI
                    to load the dsets for interactive viewing is also
                    added there.  Valid values for ADTO include:
                        Yes, 1, No, 0
                    (def: ${ADTO})

-prefix      PPP   :a prefix for helping to name files in the outdir.
                    The value of PPP should _not_ include path info,
                    because that is determined/provide by the outdir.

-workdir     WWW   :specify the name of the temporary working directory
                    (which is created as a new subdirectory of the output
                    file location---do not include path info here, just a
                    simple name)

-overwrite         :by default, this program will not overwrite a
                    pre-existing 'outdir'. Add this option to allow it to
                    do so

-no_clean          :do not remove working directory (def: remove it)

-echo              :very verbose output when running (for troubleshooting)

-help, -h          :display this meager help info

-ver               :display this program version

-------------------------------------------------------------------------
Notes ~1~

Outputs, 1: overlap report file ~2~

  A quantitative report of overlap information is provided in a text
  file called:  PREFIX_report.txt.

  All values within this file are either voxel counts or
  fractions/ratios.

  This report is essentially a dictionary of useful quantities, with
  the key or label in the first column, and the value given in the
  second text column. Because this file follows the simple
  colon-separate column formatting style, the results across one or
  more runs can be quickly and helpfully pooled together in a
  spreadsheet XLS or TSV file using gen_ss_review_table.py.

  The report contains the voxel count and overlap information for the
  input masks, divided into 4 sections for specificity:

    inrim   : The "inner rim" boundary region of the inputA mask,
              defined as being _inside_ the mask and within 'rim_dep' mm 
              from the boundary.

    outrim  : The "outer rim" boundary region of the inputA mask,
              defined as being _outside_ the mask and within 'rim_dep' mm 
              from the boundary.

              The additional work "inv" appears in this section,
              because the expectation is generally that few/no voxels
              should appear here, so the reported counts are
              _inverted_, to remain on a similar scale of
              comparability with other regions. For example, by
              definition no voxels from inputA appear in the outrim;
              by inverting the count, the value of 'outrim nvoxA' is
              therefore the total volume of the outrim, and the value
              of 'outrim nvoxB' is how many voxels in inputB do _not_
              actually appear in that band.

    core    : The "inner core" of the inputA mask, defined as being 
              everything _inside_ the mask that is not part of the inner rim.

    overall : All voxels within the FOV, that is just the total masks.

  These sections of interest exist because most often, the interesting
  differences in masks appear at the edges/boundaries, and so we
  quantify these specifically on their own. In general, we expect the
  core regions to overlap quite strongly, and so that isn't so
  interesting. Similarly, only looking at overall overlap likely
  washes out important comparisons around the edge.

  The values for each section are labelled as follows:

    nvoxA        :  #A, total nvox in A                    
    nvoxB        :  #B, total nvox in B                    
    union        :  nvox in mask union (A or B)         
    intersection :  nvox in mask intersection (A and B) 
    AnotB        :  nvox in A that aren't in B   
    BnotA        :  nvox in B that aren't in A   
    AnotB perc   :  percent of vox in A that aren't in B   
    BnotA perc   :  percent of vox in B that aren't in A   
    Dice         :  Dice coefficient, 2*intersection/(nvoxA + nvoxB)

Outputs, 2: overlap QC image ~2~

  A QC image is output, showing the shared overlap and distinct
  regions of the two masks, inputA and inputB. The image file is
  called PREFIX_QC.jpg.

  It is a montage of 8 slices from each of the axial, sagittal and
  coronal planes. It has the following color scheme:

    blue    : yes inputA, yes inputB -> overlap (dset val = 18)

    red     : yes inputA, no  inputB -> A not B (dset val = 24)

    green   : no  inputA, yes inputB -> B not A (dset val = 26)

  We provide the numerical values of the voxels within the visualized
  dataset because if users want, they can add this option to their
  command to have the underlay and overlap dsets copied into the
  outdir, as well as a driver script to view the datasets interactively: 

    -add_data_to_outdir Yes 

  The name of the created driver script will be PREFIX_run.tcsh, and 
  the dsets are called PREFIX_dset_ulay.nii.gz (for the underlay) and 
  PREFIX_dset_olapAB.nii.gz (for the mask overlap overlay).

  The number values might seem weird (why not simply use 1,2,3? and
  why is the overlap the lowest value?), but there is method to this
  madness.  The overlap is expected to be large, and the differences
  typically small/scattered.  Therefore, the bright colors were saved
  for the differences.  Also, the opacity is not uniform across types,
  but lower for the big, overlap region and higher for the
  differences, to keep those brighter against the grayscale
  background.  Therefore, AFNI's famous "alpha fading" was used to
  manage relative transparency (rather than global overlay
  opacity). That is why the number values for the differences are
  larger than the overlap. 

  Additionally, if any folks want to focus on the differences, they
  can simply: set the olay threshold to something like 20, probably
  turn off alpha thresholding ("A" key above colorbar in GUI), and
  clusterize the remaining voxels, and hop around the dset that way.

  Note that if users don't want to '-add_data_to_outdir ..', they can
  leave the workdir around (that is, not clean it up), and they can
  use the driver script that is stored _there_ with the same name to
  look at the equivalent datasets (that just have slightly different
  names, because they weren't copied to the outdir; but the driver
  script knows what to do).

-------------------------------------------------------------------------
Examples ~1~

  1. Make output QC image and report of two masks:

     compare_mask_overlap.tcsh                                     \
         -inputA              mask_01.nii.gz                       \
         -inputB              mask_02.nii.gz                       \
         -ulay                sub-001_T1w.nii.gz                   \
         -outdir              compare_mask_01_02


  2. Same as #1, but add overlap dsets and the driver script for 
     interactive viewing of overlap results:

     compare_mask_overlap.tcsh                                     \
         -inputA              mask_01.nii.gz                       \
         -inputB              mask_02.nii.gz                       \
         -ulay                sub-001_T1w.nii.gz                   \
         -outdir              compare_mask_01_02                   \
         -add_data_to_outdir  Yes


  3. Same as #1, but no QC images because no ulay is provided :(
     so only the text report is output:

     compare_mask_overlap.tcsh                                     \
         -inputA              mask_01.nii.gz                       \
         -inputB              mask_02.nii.gz                       \
         -outdir              compare_mask_01_02


  4. Same as #1, but change the depth/thinkness of the rim region:

     compare_mask_overlap.tcsh                                     \
         -inputA              mask_01.nii.gz                       \
         -inputB              mask_02.nii.gz                       \
         -ulay                sub-001_T1w.nii.gz                   \
         -rim_dep             4.321                                \ 
         -outdir              compare_mask_01_02

EOF

# ----------------------------------------------------------------------

    goto GOOD_EXIT

SHOW_VERSION:
   echo "version  $version (${rev_dat})"
   goto GOOD_EXIT

FAIL_MISSING_ARG:
    echo "** ERROR: Missing an argument after option flag: '$argv[$ac]'"
    goto BAD_EXIT

BAD_EXIT:
    exit 1

GOOD_EXIT:
    exit 0
