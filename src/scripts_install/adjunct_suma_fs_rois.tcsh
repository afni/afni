#!/bin/tcsh


@global_parse `basename $0` "$*" ; if ($status) exit 0

set version   = "0.1";  set rev_dat   = "Feb 8, 2021"
# [PT] start of this program in program form.  Programmatically speaking.
#
# ----------------------------------------------------------------

set this_prog = "adjunct_suma_fs_rois.tcsh"
set tpname    = "${this_prog:gas/@djunct_//}"
set here      = $PWD

set subj      = ""
set odir      = ""

set DO_CLEAN       = 1

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

    #  ---------- inputs: required ---------------

    if ( "$argv[$ac]" == "-sid" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set subj = "$argv[$ac]"

    else if ( "$argv[$ac]" == "-suma_dir" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set odir = "$argv[$ac]"

    else if ( "$argv[$ac]" == "-no_clean" ) then
        set DO_CLEAN = 0

    else
        echo "\n\n** ERROR: unexpected option #$ac = '$argv[$ac]'\n\n"
        goto BAD_EXIT
        
    endif
    @ ac += 1
end

# =======================================================================
# ============================ ** SETUP ** ==============================
# =======================================================================

echo "++ Prepare for running ${this_prog} (ver = ${version})"

if ( "${subj}" == "" ) then
    echo "** ERROR: missing subj ID. Use '-sid ..'"
    goto BAD_EXIT
endif

if ( "${odir}" == "" ) then
    echo "** ERROR: missing SUMA/ directory location. Use '-suma_dir ..'"
    goto BAD_EXIT
endif

# ===================== output dir + wdir =======================
 
set wa        = `which afni`
set list_2000 = ${wa:h}/afni_fs_aparc+aseg_2000.txt
set list_2009 = ${wa:h}/afni_fs_aparc+aseg_2009.txt
set otxt_2000 = "stats_fs_rois_2000_${subj}.1D"
set otxt_2009 = "stats_fs_rois_2009_${subj}.1D"
set oseg_2000 = "stats_fs_segs_2000_${subj}.1D"
set oseg_2009 = "stats_fs_segs_2009_${subj}.1D"

cd ${odir}           # now all files we need are just "here"

# =========================== Actual Plots ==============================

set root_2000 = aparc+aseg
set root_2009 = aparc.a2009s+aseg

set all_rens  = ( all gm gmrois wmat vent csf othr unkn )
set Nren      = ${#all_rens}

# ---------------------------------

# each of these lists must have the same number of items (probably 2)
set all_lists = ( ${list_2000}    ${list_2009} )
set all_otxts = ( ${otxt_2000}    ${otxt_2009} )
set all_osegs = ( ${oseg_2000}    ${oseg_2009} )
set all_roots = ( ${root_2000}    ${root_2009} )
set Niter     = ${#all_lists}

foreach nn ( `seq 1 1 ${Niter}` ) 

    set the_list = ${all_lists[$nn]}
    set the_otxt = ${all_otxts[$nn]}
    set the_oseg = ${all_osegs[$nn]}
    set the_root = ${all_roots[$nn]}
    set the_parc = ( ${the_root}*REN_all.nii* )

    echo "++ ------ Process ${the_list:t} for: ${the_parc} ---------"

    set nlines  = `cat ${the_list} | wc -l`

    printf "# %8s  #   %4s   %10s   %s\n"                  \
        "Nvox" "VAL" "TISS__TYPE" "STRING_LABEL"           \
        |& tee  ${the_otxt}

    foreach ii ( `seq 2 1 ${nlines}` )

        set line      = `awk "{if (NR == ${ii}) print}" ${the_list}`
        set fs_num    = ${line[1]}
        set str_lab   = ${line[2]}
        set tiss_type = ${line[3]}
        set ren_val   = ${line[4]}

        set ooo = `3dROIstats                       \
                    -quiet                          \
                    -DAFNI_ATLAS_NAME_TYPE=name     \
                    -nzvoxels                       \
                    -nobriklab                      \
                    -mask "${the_parc}<${str_lab}>" \
                    "${the_parc}"`

        if ( "${ooo}" != "" ) then
            set nv = ${ooo[2]}
        else
            set nv = 0
        endif

        printf "%10d  #   %4d   %10s   %s\n"                  \
            ${nv} ${ren_val} ${tiss_type} ${str_lab}         \
            |& tee -a ${the_otxt}
    end

    echo "++ Done making: ${the_otxt}"

    echo "++ ------ Process tissues for: ${the_root} ---------"

    printf "# %8s  #   %4s   %s\n"                \
        "Nvox" "SEG__TYPE"  "FILE_NAME"           \
        |& tee  ${the_oseg}

    # ---------- first, the brainmask ----------------

    set dset_mask = ( brainmask.nii* )

    set ooo = `3dROIstats                       \
                -quiet                          \
                -nzvoxels                       \
                -nobriklab                      \
                -mask ${dset_mask[1]}           \
                ${dset_mask[1]}`

    if ( "${ooo}" != "" ) then
        set nv = ${ooo[2]}
    else
        set nv = 0
    endif

    printf "%10s  #   %10s   %s\n"                \
        "${nv}" "wb_mask"  "${dset_mask[1]}"      \
        |& tee -a ${the_oseg}

    # ---------- then, the tissue types ----------------

    foreach jj ( `seq 1 1 ${Nren}` )

        set seg      = ${all_rens[${jj}]}
        set dset_seg = ( ${the_root}_REN_${seg}.nii* )
        set ooo = `3dROIstats                       \
                    -quiet                          \
                    -nzvoxels                       \
                    -nobriklab                      \
                    -mask "3dcalc( -a ${dset_seg[1]} -expr step(a) )"   \
                    "${dset_seg[1]}"`

        if ( "${ooo}" != "" ) then
            set nv = ${ooo[2]}
        else
            set nv = 0
        endif

        printf "%10s  #   %10s   %s\n"                \
            "${nv}" "${seg}"  "${dset_seg}"           \
            |& tee -a ${the_oseg}
    end
end

# ---------------------------------


goto GOOD_EXIT

# ========================================================================
# ========================================================================

SHOW_HELP:
cat << EOF
-------------------------------------------------------------------------

OVERVIEW ~1~

In brief, this script is for quickly making some ROI stats (counts)
for the SUMA/ directory created by @SUMA_Make_Spec_FS after running
FreeSurfer's recon-all.  

written by PA Taylor.

# --------------------------------------------------------------------

USAGE ~1~

This program has the following options:

    -sid          SUBJ_ID    :(req) subject ID

    -suma_dir     SUMA_DIR   :(req) SUMA/ directory output by AFNI's
                              @SUMA_Make_Spec_FS

    -help                    :(opt) show help

    -hview                   :(opt) show help in text editor

    -ver                     :(opt) show version

OUTPUT ~1~

This script makes four *.1D files in the specified SUMA/ directory.
Column labels are present in each file.  Note that the ROI string
labels are provided for each ROI, but behind a comment symbol in each
line (so you can use them as regular *.1D files, with 1dcat,
1dtranspose, etc.):

  stats_fs_rois_2000_FT.1D  : voxel counts of the "2000" parcellation 
                              (from the file: aparc+aseg_REN_all.nii*)

  stats_fs_rois_2009_FT.1D  : voxel counts of the "2009" parcellation 
                              (from the file: aparc+aseg_REN_all.nii*)

  stats_fs_segs_2000_FT.1D  : voxel counts of the "2000" parc brain mask
                              and tissue/segmentations (from the
                              brainmask.nii* and aparc+aseg_REN_* files)

  stats_fs_segs_2009_FT.1D  : voxel counts of the "2009" parc brain mask
                              and tissue/segmentations (from the
                              brainmask.nii* and aparc.a2009s+aseg_REN_* 
                              files)

EXAMPLE ~1~

  adjunct_suma_fs_rois.tcsh               \
      -sid       sub-001                  \
      -suma_dir  group/sub-001/SUMA

EOF

# ----------------------------------------------------------------------

    goto GOOD_EXIT

SHOW_VERSION:
   echo "version  $version (${rev_dat})"
   goto GOOD_EXIT

FAIL_MISSING_ARG:
    echo "** ERROR! Missing an argument after option flag: '$argv[$ac]'"
    goto BAD_EXIT

BAD_EXIT:
    exit 1

GOOD_EXIT:
    exit 0
