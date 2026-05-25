#!/bin/tcsh

# A script for comparison mask properties
# 
# ============================================================================

@global_parse `basename $0` "$*" ; if ($status) exit 0

set version   = "0.0";  set rev_dat   = "Feb 1, 2023"
# + better template for scripting
#
# ----------------------------------------------------------------

set this_prog = "adjunct_mask_compare.tcsh"
set prog_abbr = "AMC"
set here      = $PWD

# ----------------------- set defaults --------------------------

set inputA    = ""
set inputB    = ""
set ulay      = ""

set outdir    = ""
set opref     = "prefix"
set prefix    = ""

set rim_dep   = 5.0                      # mm value for rim depth

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
        #set prefix = "$argv[$ac]"
        #set opref  = `basename "$argv[$ac]"`
        #set outdir   = `dirname  "$argv[$ac]"`
        set outdir   = "$argv[$ac]"

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
endif

# make the working directory
if ( ! -e "${outdir}/${wdir}" ) then
    echo "++ Making working directory: ${outdir}/${wdir}"
    \mkdir -p "${outdir}/${wdir}"
else
    echo "+* WARNING:  Somehow found a premade working directory (?):"
    echo "      ${outdir}/${wdir}"
endif

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
set orep = report_dice.txt
printf "" > ${orep}

foreach ii ( `seq 1 1 ${#all_label}` )
    # NB: the value of ii also selects the region within the mask
    set label  = "${all_label[${ii}]}"
    set func   = "${all_func[${ii}]}"
    
    if ( "${label}" == "overall" ) then
c        # overall calc : calc everywhere
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

    printf "%-20s : %d \n"     "${label} nvoxA" "${nvoxA}" >> ${orep}
    printf "%-20s : %d \n"     "${label} nvoxB" "${nvoxB}" >> ${orep}
    printf "%-20s : %d \n"     "${label} union" "${union}" >> ${orep}
    printf "%-20s : %d \n"     "${label} intersection" "${inter}" >> ${orep}
    printf "%-20s : %d \n"     "${label} AnotB" "${AnotB}" >> ${orep}
    printf "%-20s : %d \n"     "${label} BnotA" "${BnotA}" >> ${orep}
    printf "%-20s : %-0.5f \n" "${label} AnotB perc" "${AnotBperc}" >> ${orep}
    printf "%-20s : %-0.5f \n" "${label} BnotA perc" "${BnotAperc}" >> ${orep}
    printf "%-20s : %-0.5f \n" "${label} Dice" "${dice}"   >> ${orep}
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

    # ============================================================================

    # Make a montage of the zeroth brick of each dset
    
    set img_pref = IMG

    @chauffeur_afni                                                              \
        -ulay              dset_00_ulay.nii.gz                                   \
        -olay              dset_10_olapAB.nii.gz                                 \
        -box_focus_slices  dset_10_olapAB.nii.gz                                 \
        -thr_olay          40                                                    \
        -olay_alpha        Linear                                                \
        -cbar_ncolors      3                                                     \
        -cbar_topval       ""                                                    \
        -cbar              "40=green 25=rbgyr20_20 20=lt-blue2 0=none"           \
        -prefix            ${img_pref}                                           \
        -pbar_saveim       ${img_pref}_pbar.jpg                                  \
        -pbar_posonly                                                            \
        -func_range        40                                                    \
        -set_xhairs        OFF                                                   \
        -montx             8                                                     \
        -monty             1                                                     \
        -opacity           9                                                     \
        -label_mode        1                                                     \
        -label_size        3                                                     \
        -cmd2script        ${img_pref}_run.tcsh                                  \
        -c2s_text          'View mask comparison'                                \
        -c2s_mont_1x1

    if ( $status ) then
        goto BAD_EXIT
    endif

    # Concatenate images across all 3 sliceviews.

    set pref_cat = ALL_IMG.jpg

    2dcat                                                                        \
        -gap      5                                                              \
        -gap_col  150 180 150                                                    \
        -nx       1                                                              \
        -ny       3                                                              \
        -prefix   ../${pref_cat}                                                 \
        ${img_pref}*{axi,cor,sag}*png

    if ( $status ) then
        goto BAD_EXIT
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

This program facilitates comparing two different mask datasets.  It
makes a report of relevant quantities 

auth    : PA Taylor (SSCC, NIMH, NIH, USA)
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

-rim_dep     RD    :the depth (in units of mm) of the inner and outer rim 
                    regions
                    (def: ${rim_dep})

-workdir     WWW   :specify the name of the temporary working directory
                    (which is created as a new subdirectory of the output
                    file location---do not include path info here, just a
                    simple name)

-no_clean          :do not remove working directory (def: remove it)

-echo              :very verbose output when running (for troubleshooting)

-help, -h          :display this meager help info

-ver               :display this program version

-------------------------------------------------------------------------
Notes ~1~

*****describe outputs:

QC image

report

-------------------------------------------------------------------------
Examples ~1~

*****

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
