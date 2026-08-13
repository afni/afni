#!/bin/tcsh

# file for testing old/new FIRdesign

# 2026-08-11 : [pt] FIRdesign got an update under the hood, now using
#              a more modern version of remez.c. This script tests the
#              older and new versions for similarity.  Interestingly, in 
#              many of the test cases, the tolerance for 3dDiff could be 0.
# 
# ===========================================================================

# set locations of old and new program versions, for comparisons

set path_old = ${HOME}/afni_build_GOOD_2026_08_11_09_08_1786455134
set path_old = ${path_old}/src/linux_ubuntu_16_64_glw_local_shared/
set path_new = ${HOME}/afni_build/src/linux_ubuntu_16_64_glw_local_shared

set prog_old = ${path_old}/FIRdesign
set prog_new = ${path_new}/FIRdesign

# make output dir for test results (and init/clear a text file of diffs)

set dir_test = testing-FIRdesign
set txt_diff = ${dir_test}/all_diffs.txt

\mkdir -p ${dir_test}
printf '' > ${txt_diff}

# ===========================================================================

set bname   = ${dir_test}/test-01

set out_old = ${bname}-old.1D
set out_new = ${bname}-new.1D
set out_log = ${bname}-plot-log.txt

${prog_old} 0.01 0.10 180 > ${out_old}
${prog_new} 0.01 0.10 180 > ${out_new}

echo "---- test: ${bname} ----" |& tee -a ${txt_diff}
3dDiff -a ${out_old} -b ${out_new} -tol 0.0001 |& tee -a ${txt_diff}

1dplot -one -ynames "old" "new" -dashed 1:3 -title "${bname}" \
    ${out_old} ${out_new} >& ${out_log} &

# ----------------------------------------------------------------------------

set bname   = ${dir_test}/test-02

set out_old = ${bname}-old.1D
set out_new = ${bname}-new.1D
set out_log = ${bname}-plot-log.txt

${prog_old} 0.01 0.10 181 > ${out_old}
${prog_new} 0.01 0.10 181 > ${out_new}

echo "---- test: ${bname} ----" |& tee -a ${txt_diff}
3dDiff -a ${out_old} -b ${out_new} -tol 0.0001 |& tee -a ${txt_diff}

1dplot -one -ynames "old" "new" -dashed 1:3 -title "${bname}" \
    ${out_old} ${out_new} >& ${out_log} &

# ----------------------------------------------------------------------------

set bname   = ${dir_test}/test-03

set out_old = ${bname}-old.1D
set out_new = ${bname}-new.1D
set out_log = ${bname}-plot-log.txt

${prog_old} 0.0 0.20 250 > ${out_old}
${prog_new} 0.0 0.20 250 > ${out_new}

echo "---- test: ${bname} ----" |& tee -a ${txt_diff}
3dDiff -a ${out_old} -b ${out_new} -tol 0.0001 |& tee -a ${txt_diff}

1dplot -one -ynames "old" "new" -dashed 1:3 -title "${bname}" \
    ${out_old} ${out_new} >& ${out_log} &


# ----------------------------------------------------------------------------

set bname   = ${dir_test}/test-04

set out_old = ${bname}-old.1D
set out_new = ${bname}-new.1D
set out_log = ${bname}-plot-log.txt

# NB: special case using "grep .." bc old prog spits warning text into stdout
${prog_old} 0.1 0.50 2000 | grep -v "Re" > ${out_old}
${prog_new} 0.1 0.50 2000 > ${out_new}

echo "---- test: ${bname} ----" |& tee -a ${txt_diff}
3dDiff -a ${out_old} -b ${out_new} -tol 0.0001 |& tee -a ${txt_diff}

1dplot -one -ynames "old" "new" -dashed 1:3 -title "${bname}" \
    ${out_old} ${out_new} >& ${out_log} &

# ----------------------------------------------------------------------------

echo "++ DONE.  Check diffs file:"
echo "-------------------------------------------------"
cat ${txt_diff}
echo "-------------------------------------------------"

exit 0
