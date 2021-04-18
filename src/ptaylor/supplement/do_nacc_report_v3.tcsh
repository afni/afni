#!/bin/tcsh

#set echo

# Make a table of NAcc properties

# NB: this script is just for *single* echo data at the moment

### ver = 2.0
# [PT: Mar 30, 2021] Rewrote to put in loops over ROIs and dsets: more
#                    generalizable. Added mean volreg dset info, too.
### ver = 2.1
# [PT: Mar 30, 2021] Fixed a bug in the winf* calcs
### ver = 2.2
# [PT: Mar 30, 2021] add in Harv-Ox atlas ROIs for stats
### ver = 3.0
# [PT: Apr 16, 2021] add the ROI snapshotting
### ver = 3.1
# [PT: Apr 18, 2021] updated ROI snapshotting; no new inputs needed
### ver = 3.2
# [PT: Apr 18, 2021] the all_vols members should be local, not with path
#                    info

# ------------------------------------------------------------------------
# user specifies inputs and parameters

set narg_good = 3     # just the num of args needed, for checking
set subj      = $1    # subj ID
set dir_ap_ss = $2    # something/${subj}.results, or ${tempdir} (for SLURM)
set dir_fs_ss = $3    # something/SUMA

# ------------------------------------------------------------------------
# define other variables of interest: dsets, intermediate prefixes and
# final outputs; note that we will jump to AP results directory for
# most processing, so most names are local to there.

set here        = $PWD

# these should be the follower dset names used in AP
set foll_fs2000 = follow_ROI_FSall00+tlrc.HEAD
set foll_fs2009 = follow_ROI_FSall09+tlrc.HEAD

# this is the Harv-Ox atl, resampled to final EPI grid, with just some
# particular ROIs present+labeled
set hox_atl     = /data/NIMH_Haskins/dylan/template_hox/HOX_rois_333.nii.gz
#set hox_atl     = /data/PROJECTS/HOX_3.5mm_iso.nii.gz

# tmp prefix for intermediates; cleaned up at end
set tpref       = __rep_tmp 

# output name of report
set nacc_rep    = report_nacc.txt

# ------------------------------------------------------------------------

set narg = ${#argv}
if ( ${narg} == ${narg_good} ) then
    echo ""
    echo "++ Good number of input args (${narg}) for subj: ${subj}"
    echo ""
else
    echo "** ERROR: Should have ${narg_good} args input, but have: ${narg}"
cat <<EOF
   -----------------------------------------------------------
   set subj      = $1    # subj ID
   set dir_ap_ss = $2    # something/\${subj}.results or \${tempdir} (for SLURM)
   set dir_fs_ss = $3    # something/SUMA
   -----------------------------------------------------------
EOF
    exit 1
endif

# -------------------------------------------------------------------------
# before jumping to AP results, finalize ROI dataset names and properties

# the dsets output by @SUMA_Make_Spec_FS, i.e., ones with labels
set orig_2000 = ( ${dir_fs_ss}/aparc+aseg_REN_all.nii* )
set orig_2009 = ( ${dir_fs_ss}/aparc.a2009s+aseg_REN_all.nii* )

# reattach labeltables, if they don't have them yet
if ( ! `3dinfo -is_atlas_or_labeltable ${dir_ap_ss}/${foll_fs2000}` ) then
    3drefit -copytables ${orig_2000}  ${dir_ap_ss}/${foll_fs2000}
    3drefit -cmap INT_CMAP            ${dir_ap_ss}/${foll_fs2000}
endif

if ( ! `3dinfo -is_atlas_or_labeltable ${dir_ap_ss}/${foll_fs2009}` ) then
    3drefit -copytables ${orig_2009}  ${dir_ap_ss}/${foll_fs2009}
    3drefit -cmap INT_CMAP            ${dir_ap_ss}/${foll_fs2009}
endif

# -------------------------------------------------------------------------
# from here on, everything is local to the AP results directory, so
# move there, for simpler naming

cd ${dir_ap_ss}

# -------------------------------------------------------------------------
# define all ROIs of interest.  Each dset is on final EPI grid already.

# corresponding dsets (ROIs within which to get stats) and labels

set all_rois = ( ${foll_fs2000}"<Left-Accumbens-area>"  \
                 ${foll_fs2000}"<Right-Accumbens-area>" \
                 ${hox_atl}"<lh_hox_nacc>"              \
                 ${hox_atl}"<rh_hox_nacc>"              )

set lab_rois = ( lh_nacc_fs rh_nacc_fs lh_nacc_hox rh_nacc_hox )

# -------------------------------------------------------------------------
# finalize dsets from within which we will get stats

# the specific volreg dset to grab. just the run one one at
# present. Would also need to adjust for multi-echo data, too.
set dset_vr_4d   = ( pb*${subj}*.r01*volreg+*.HEAD )

echo "++ Make intermediate dset: mean across time of volreg data"
3dTstat                               \
    -overwrite                        \
    -prefix ${tpref}_vr.nii.gz        \
    "${dset_vr_4d}"

# corresponding dsets (all from which we will get stats) and labels;
# each is in the AP results dir

set all_vols     = ( TSNR.vreg.r01.${subj}+tlrc.HEAD              \
                     TSNR.${subj}+tlrc.HEAD                       \
                     ${tpref}_vr.nii.gz                           )

set lab_vols     = ( tsnr_vr  \
                     tsnr_fin \
                     mean_vr  )

# for images, hot-color ranges for cbars for each type of data;
# strings to be split later in echoing (doing this for 1-to-1
# correspondence)
set ran_vols    = (  "40  100" \
                    "100  240" \
                    "800 1200" )

# -------------------------------------------------------------------------
# start report, and populate with values

echo ""
echo "Number of ROI     : $#all_rois"
echo "Number of volumes : $#all_vols"
echo ""

printf '' > ${nacc_rep}

printf  "%-30s : %10s \n"             \
        "subject ID" "${subj}"        \
        >> ${nacc_rep}

printf "\n" >>  ${nacc_rep}

# Value of interest: size of each ROI

foreach ii ( `seq 1 1 ${#all_rois}` )
    set rdset = "${all_rois[$ii]}"
    set rlab  = "${lab_rois[$ii]}"

    set val   = `3dROIstats -quiet    \
                    -nzvoxels         \
                    -nobriklab        \
                    -nomeanout        \
                    -mask "${rdset}"  \
                    "${rdset}"`

    printf  "%-30s : %10s \n"          \
            "${rlab} nvox" "${val}"    \
            >> ${nacc_rep}
end

printf "\n" >>  ${nacc_rep}

# Value of interest: stats and percentiles of data within each ROI.
# Loop over each volume of interest

foreach hh ( `seq 1 1 ${#all_vols}` )
    set vdset = "${all_vols[$hh]}"
    set vlab  = "${lab_vols[$hh]}"

    foreach ii ( `seq 1 1 ${#all_rois}` )
        set rdset = "${all_rois[$ii]}"
        set rlab  = "${lab_rois[$ii]}"

        # calculate all stats of interest, then reorder for presenting
        set sss = `3dBrickStat                 \
                        -slow                  \
                        -perc_quiet            \
                        -percentile 0 25 100   \
                        -mean                  \
                        -stdev                 \
                        -mask "${rdset}"       \
                        "${vdset}"`
        set all_stat = ( $sss[6-7] $sss[1-5] ) 
        set lab_stat = ( mean std min p25 med p75 max )

        # write out each stat of interest into the report
        foreach jj ( `seq 1 1 ${#all_stat}` )
            set stat  = "${all_stat[$jj]}"
            set slab  = "${lab_stat[$jj]}"

            # full label for table row
            set flab  = "${rlab} ${vlab} ${slab}"

            printf  "%-30s : %10.1f \n"      \
                    "${flab}" "${stat}"      \
                    >> ${nacc_rep}
        end

        # Extra 'stat' of interest: 5-95%ile window scaled by median
        # this gives a dimensionless value of the range of values in
        # the ROI.  That is, large values have a wide range, small
        # values have a small one.  Also do this for a 25-75%ile
        # window, in case that is more elucidating.  We call these
        # "window fractions" (winf)
        
        set www = `3dBrickStat                 \
                        -slow                  \
                        -perc_quiet            \
                        -perclist 5 5 25 50 75 95   \
                        -mask "${rdset}"       \
                        "${vdset}"`

        set w1 = `echo "scale=4; (${www[5]}-${www[1]})/${www[3]}" | bc`
        set w2 = `echo "scale=4; (${www[4]}-${www[2]})/${www[3]}" | bc`

        set all_win = ( $w1     $w2    )
        set lab_win = ( winf90  winf50 )

        # write out each stat of interest into the report
        foreach jj ( `seq 1 1 ${#all_win}` )
            set wval  = "${all_win[$jj]}"
            set wlab  = "${lab_win[$jj]}"

            # full label for table row
            set flab  = "${rlab} ${vlab} ${wlab}"

            printf  "%-30s : %13.4f \n"      \
                    "${flab}" "${wval}"      \
                    >> ${nacc_rep}
        end

        printf "\n" >>  ${nacc_rep}
    end
end

# end of first work: making txt report
# ------------------------------------------------------------------------
# now add in imagizing, too

# we are still in AP results dir

foreach hh ( `seq 1 1 ${#all_vols}` )
    set vdset = "${all_vols[$hh]}"
    set vlab  = "${lab_vols[$hh]}"
    set vran  = `echo "${ran_vols[$hh]}"`  # split string into 2 vals

    # we have different ways of scaling for tsnr* and mean*; above is
    # default, for TSNR; we use this to change ${vran} for mean_* dsets
    set vlab_pre = `echo ${vlab} | awk '{print substr($1,1,4)}'`

    if ( "${vlab_pre}" == "mean" ) then 
        # get values from within mask for signal mean---too hard to
        # scale generally
        set mask = ( mask_epi_anat*${subj}*.HEAD )

        set vran  = `3dBrickStat -slow                    \
                        -non-zero                         \
                        -perc_quiet                       \
                        -mask "${mask}"                   \
                        -perclist 2 5 95                  \
                        ${vdset}`
    endif

    foreach ii ( `seq 1 1 ${#all_rois}` )
        set rdset = "${all_rois[$ii]}"
        set rlab  = "${lab_rois[$ii]}"

        tcsh ${here}/do_make_roi_zoom_imgs.tcsh                 \
                -full_dset_name_roi "${rdset}"                  \
                -cbar_hot_range     "${vran[1]}" "${vran[2]}"   \
                -dset_olay          "${vdset}"                  \
                -prefix             "img__${vlab}__${rlab}__${subj}"
    end
end

# ---------------------------------------------------------------------

# clean up
if ( 1 ) then
    echo "++ Clean up intermediate file(s) from ROI stats"
    \rm  ${tpref}_*
endif

# jump back
cd ${here}


echo ""
echo "++ Done with report.  Check out file:"
echo "   ${dir_ap_ss}/${nacc_rep}"
echo ""

exit 0





    else if ( "${vlab_pre}" == "mean" ) then 
        # get values from within mask for signal mean---too hard to
        # scale generally
        set mask = ( mask_epi_anat*${subj}*.HEAD )

        set vval  = `3dBrickStat -slow                    \
                        -non-zero                         \
                        -perc_quiet                       \
                        -mask "${mask}"                   \
                        -perclist 2 5 95                  \
                        ${vdset}`

        foreach ii ( `seq 1 1 ${#all_rois}` )
            set rdset = "${all_rois[$ii]}"
            set rlab  = "${lab_rois[$ii]}"

            tcsh ${here}/do_make_roi_zoom_imgs.tcsh                 \
                    -full_dset_name_roi "${rdset}"                  \
                    -cbar_hot_range     "${vval[1]}" "${vval[2]}"   \
                    -dset_olay          "${vdset}"                  \
                    -prefix             "img__${vlab}__${rlab}__${subj}"
        end
    else
        echo "+* WARNING: THIS OCCURRENCE SHOULD NEVER HAPPEN."
        echo "   Weirdly, vlab = ${vlab}, so vlab_pre = ${vlab_pre}"
    endif

