#!/bin/tcsh

# file for testing old/new 3dClustSim

# 2026-08-15 : [pt] 3dClustSim got an update under the hood, now
#              using a different/faster version of the Ziggurat
#              algorithm. This script tests the older and new versions
#              for similarity.
# 
# ===========================================================================

set prog = 3dClustSim
set idx  = 00

# ---------------------------------------------------------------------------
# set locations of old and new program versions, for comparisons

set path_old = ${HOME}/afni_build_GOOD_2026_07_02_09_07_1783000295
set path_old = ${path_old}/src/linux_ubuntu_16_64_glw_local_shared/
set path_new = ${HOME}/afni_build/src/linux_ubuntu_16_64_glw_local_shared

set prog_old = ${path_old}/${prog}
set prog_new = ${path_new}/${prog}

# output dir for results and a text file

set dir_test = odir_test-${idx}-${prog}
set txt_diff = ${dir_test}/all_diffs.txt
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# generic checks to be able to run testing

if ( ! -f ${prog_old} ) then
    echo "** ERROR: cannot find prog old:"
    echo "   ${prog_old}"
    exit -1
endif

if ( ! -f ${prog_new} ) then
    echo "** ERROR: cannot find prog new:"
    echo "   ${prog_new}"
    exit -1
endif

if ( -d ${dir_test} ) then
    echo ""
    echo "** ERROR: already have output testing dir."
    echo "   Consider running the following to remove it:"
    echo ""
    echo "     \\rm -rf ${dir_test}"
    echo ""
    exit -1
endif

echo "++++ Passed first checks to be able to run test. Continuing."
# ---------------------------------------------------------------------------

# ===========================================================================
# extra checks, specific to this dset

# test data 1: findable?

if ( ! -f mask.auto.nii.gz ) then
    echo "** ERROR: cannot find output mask"
    echo "   Is this being run in: ~/AFNI_data6/afni/ ? It should be..."
else
    echo "++ Found input mask"
    echo "   ... just proceeding with comparison"
endif

### in case you want to test single
# setenv OMP_NUM_THREADS 1

# ===========================================================================

# ---------------------------------------------------------------------------
# make output dir for test results (and init/clear a text file of diffs)

\mkdir -p ${dir_test}
printf '' > ${txt_diff}
# ---------------------------------------------------------------------------

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



