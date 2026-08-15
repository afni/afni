#!/bin/tcsh

# file for testing old/new 3dClustSim

# 2026-08-15 : [pt] 3dClustSim got an update under the hood, now
#              using a different/faster version of the Ziggurat
#              algorithm. This script tests the older and new versions
#              for similarity.
# 
# ===========================================================================

# set locations of old and new program versions, for comparisons

set path_old = ${HOME}/afni_build_GOOD_2026_07_02_09_07_1783000295
set path_old = ${path_old}/src/linux_ubuntu_16_64_glw_local_shared/
set path_new = ${HOME}/afni_build/src/linux_ubuntu_16_64_glw_local_shared

set prog_old = ${path_old}/3dClustSim
set prog_new = ${path_new}/3dClustSim

# make output dir for test results (and init/clear a text file of diffs)

set dir_test = testing-3dClustSim
set txt_diff = ${dir_test}/all_diffs.txt

\mkdir -p ${dir_test}
printf '' > ${txt_diff}

# input test data created (if need be), via these cmds
if ( ! -f mask.auto.nii.gz ) then
    echo "** ERROR: cannot find output mask"
    echo "   Is this being run in: ~/AFNI_data6/afni/ ? It should be..."
else
    echo "++ Found input mask"
    echo "   ... just proceeding with comparison"
endif

# ===========================================================================

# all tests are basically the same, just need to index by 2-digit code
set all_ii = `count_afni -digits 2 1 1`

foreach ii ( ${all_ii} )

    set bname   = ${dir_test}/test-${ii}

    set out_old = ${bname}-old_csim 
    set out_new = ${bname}-new_csim
    set out_log = ${bname}-plot-log.txt

    # special things done when running here:
    # + want to record time of each, so this records time values as
    #   integers that count milliseconds: date +%s%3N
    # + the acf values used here come from the Bootcamp processed data,
    #   just to be realistic examples

    set time0 = `date +%s%3N`
    ${prog_old}                              \
        -prefix  ${out_old}                  \
        -mask    mask.auto.nii.gz            \
        -acf     0.738202  3.2154  11.9615   \
        -iter    10000
    set time1 = `date +%s%3N`
    @ time_ms_old = ${time1} - ${time0}

    set time0 = `date +%s%3N`
    ${prog_new}                              \
        -prefix  ${out_new}                  \
        -mask    mask.auto.nii.gz            \
        -acf     0.738202  3.2154  11.9615   \
        -iter    10000
    set time1 = `date +%s%3N`
    @ time_ms_new = ${time1} - ${time0}

    @ time_diff_ms = ${time_ms_new} - ${time_ms_old}

    set time_diff_frac = `echo "scale = 1; 100.0*(${time_ms_new} - ${time_ms_old})/(1.0*${time_ms_old}) " | bc`

cat <<EOF

++ Time info (number of ms) for run: ${ii} 
   old  : ${time_ms_old}
   new  : ${time_ms_new}
   diff : ${time_diff_ms}
   perc : ${time_diff_frac} %

EOF

cat <<EOF
---------------------------
++ And check out these dsets:
    
   meld ${out_old}.NN2_bisided.1D \
        ${out_new}.NN2_bisided.1D

   Hopefully:
    + integer part of values is similar, esp. as p-values decrease

EOF


end

# ===========================================================================

echo "++ DONE.  Check diffs file:"
echo "-------------------------------------------------"
cat ${txt_diff}
echo "-------------------------------------------------"

exit 0



