#!/bin/tcsh

# file for testing old/new 3dDWItoDT

# 2026-10-26 : [pt] 3dDWItoDT has some updated estimation in very
#              special cases of extremely prolate/oblate spheroids. We
#              are testing versions for similarity.
# 
# ===========================================================================

set prog = 3dDWItoDT
set idx  = 00

# ---------------------------------------------------------------------------
# set locations of old and new program versions, for comparisons

set path_old = ${HOME}/afni_build_ORIG
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

set all_fname = ( mask_DWI+orig.HEAD \
                  AVEB0_DWI.nii.gz   \
                  GRADS_bmatA.dat )

foreach fname ( ${all_fname} ) 

    if ( ! -f ${fname} ) then
        echo "** ERROR: cannot find input fname: ${fname}"
        echo "   Is this being run in: ~/AFNI_demos/FATCAT_DEMO/ ?"
        echo "   It should be..."
        exit -1
    else
        echo "++ Found input fname: ${fname}"
    endif
end

echo "   ... just proceeding with comparison"

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
###set all_ii = `count_afni -digits 2 1 1`

set ii = 00
if ( 1 ) then   ### instead of for-loop

    set bname   = ${dir_test}/test-${ii}

    set out_old = ${bname}-old_dwi2dt 
    set out_new = ${bname}-new_dwi2dt
    set out_log = ${bname}-plot-log.txt

    # special things done when running here:
    # + want to record time of each, so this records time values as
    #   integers that count milliseconds: date +%s%3N

    set time0 = `date +%s%3N`
    ${prog_old}                              \
        -prefix          ${out_old}          \
        -mask            mask_DWI+orig.HEAD  \
        -eigs -sep_dsets -nonlinear          \
        -scale_out_1000                      \
        -bmatrix_FULL    GRADS_bmatA.dat     \
        AVEB0_DWI.nii.gz 
    set time1 = `date +%s%3N`
    @ time_ms_old = ${time1} - ${time0}

    set time0 = `date +%s%3N`
    ${prog_new}                              \
        -prefix          ${out_new}          \
        -mask            mask_DWI+orig.HEAD  \
        -eigs -sep_dsets -nonlinear          \
        -scale_out_1000                      \
        -bmatrix_FULL    GRADS_bmatA.dat     \
        AVEB0_DWI.nii.gz 
    set time1 = `date +%s%3N`
    @ time_ms_new = ${time1} - ${time0}

    # NB: the out_old and out_new here are just prefixes; we actually
    # test several scalar output files
    echo "---- test: ${bname} ----" |& tee -a ${txt_diff}
    3dDiff -a ${out_old}_FA+orig.HEAD -b ${out_new}_FA+orig.HEAD -tol 0.00001 \
        |& tee -a ${txt_diff}
    3dDiff -a ${out_old}_MD+orig.HEAD -b ${out_new}_MD+orig.HEAD -tol 0.00001 \
        |& tee -a ${txt_diff}
    3dDiff -a ${out_old}_L1+orig.HEAD -b ${out_new}_L1+orig.HEAD -tol 0.00001 \
        |& tee -a ${txt_diff}
    3dDiff -a ${out_old}_RD+orig.HEAD -b ${out_new}_RD+orig.HEAD -tol 0.00001 \
        |& tee -a ${txt_diff}

    @ time_diff_ms = ${time_ms_new} - ${time_ms_old}

    set time_diff_frac = `echo "scale = 1; 100.0*(${time_ms_new} - ${time_ms_old})/(1.0*${time_ms_old}) " | bc`

cat <<EOF

++ Time info (number of ms) for run: ${ii} 
   old  : ${time_ms_old}
   new  : ${time_ms_new}
   diff : ${time_diff_ms}
   perc : ${time_diff_frac} %

EOF

endif  ### closing

# ===========================================================================

echo "++ DONE.  Check diffs file:"
echo "-------------------------------------------------"
cat ${txt_diff}
echo "-------------------------------------------------"

exit 0



