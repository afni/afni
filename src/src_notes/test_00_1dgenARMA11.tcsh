#!/bin/tcsh

# file for testing old/new 1dgenARMA11

# 2026-08-13 : [pt] 1dgenARMA11 got an update under the hood, now using
#              a more modern version of zgaussian(). This script tests the
#              older and new versions for similarity.
# 
# ===========================================================================

# set locations of old and new program versions, for comparisons

set path_old = ${HOME}/afni_build_GOOD_2026_08_13_07_08_1786622358
set path_old = ${path_old}/src/linux_ubuntu_16_64_glw_local_shared/
set path_new = ${HOME}/afni_build/src/linux_ubuntu_16_64_glw_local_shared

set prog_old = ${path_old}/1dgenARMA11
set prog_new = ${path_new}/1dgenARMA11

# make output dir for test results (and init/clear a text file of diffs)

set dir_test = testing-1dgenARMA11
set txt_diff = ${dir_test}/all_diffs.txt

\mkdir -p ${dir_test}
printf '' > ${txt_diff}

# ===========================================================================

# all we need to do is change num of points generated
set all_num  = ( 100 500 1000 5000 10000 50000 100000 500000 )
set all_nbin = ( 10  50  100  100  100   100   100    100    )
set nnum     = ${#all_num}

# all tests are basically the same, just need to index by 2-digit code
set all_ii = `count_afni -digits 2 1 ${nnum}`

foreach hh ( `seq 1 1 ${nnum}` )
    set ii   = ${all_ii[$hh]}
    set num  = ${all_num[$hh]}
    set nbin = ${all_nbin[$hh]}

    set bname   = ${dir_test}/test-${ii}

    set out_old = ${bname}-old.1D
    set out_new = ${bname}-new.1D
    set out_log = ${bname}-plot-log.txt

    # special things done when running here:
    # + want to record time of each, so this records time values as
    #   integers that count milliseconds: date +%s%3N
    # + the -prefix of 3dhistog always adds .1D, so hack around it

    set time0 = `date +%s%3N`
    ${prog_old} -num ${num} -a 0.8 -lam 0.7 > ${bname}_ARMAgen11_old.1D
    set time1 = `date +%s%3N`
    @ time_ms_old = ${time1} - ${time0}
    3dhistog -nbin ${nbin} -min -5 -max 5 -pdf \
        -prefix ${bname}-old ${bname}_ARMAgen11_old.1D

    set time0 = `date +%s%3N`
    ${prog_old} -num ${num} -a 0.8 -lam 0.7 > ${bname}_ARMAgen11_new.1D
    set time1 = `date +%s%3N`
    @ time_ms_new = ${time1} - ${time0}
    3dhistog -nbin ${nbin} -min -5 -max 5 -pdf \
        -prefix ${bname}-new ${bname}_ARMAgen11_new.1D

    echo "---- test: ${bname} ----" |& tee -a ${txt_diff}
    3dDiff -a "${out_old}[1]" -b "${out_new}[1]" -tol 0.01 |& tee -a ${txt_diff}
    3dDiff -a "${out_old}[2]" -b "${out_new}[2]" -tol 0.01 |& tee -a ${txt_diff}

    @ time_diff_ms = ${time_ms_new} - ${time_ms_old}

    set time_diff_frac = `echo "scale = 1; 100.0*(${time_ms_new} - ${time_ms_old})/(1.0*${time_ms_old}) " | bc`

cat <<EOF

++ Time info (number of ms) for run: ${ii} 
   old  : ${time_ms_old}
   new  : ${time_ms_new}
   diff : ${time_diff_ms}
   perc : ${time_diff_frac} %

EOF

    1dplot \
        -one -xmulti "${out_old}[0]" "${out_new}[0]" \
        -ynames "old" "new" -dashed 1:3 -title "${bname}" \
        "${out_old}[1]" "${out_new}[1]" >& ${out_log} &

end

# ... and another set of images 
foreach hh ( `seq 1 1 ${nnum}` )
    set ii   = ${all_ii[$hh]}
    set num  = ${all_num[$hh]}
    set nbin = ${all_nbin[$hh]}

    set bname   = ${dir_test}/test-${ii}

    set out_old = ${bname}-old.1D
    set out_new = ${bname}-new.1D
    set out_log = ${bname}-plot-log.txt

    1dplot \
        -one -xmulti "${out_old}[0]" "${out_new}[0]" \
        -ynames "old" "new" -dashed 1:3 -title "${bname}" \
        "${out_old}[2]" "${out_new}[2]" >& ${out_log} &
end

# ===========================================================================

echo "++ DONE.  Check diffs file:"
echo "-------------------------------------------------"
cat ${txt_diff}
echo "-------------------------------------------------"

exit 0
































exit 0


set bname   = ${dir_test}/test-01

set out_old = ${bname}-old.1D
set out_new = ${bname}-new.1D
set out_log = ${bname}-plot-log.txt

set time0 = `date +%s%3N`
${prog_old} 0.01 0.10 180 > ${out_old}
${prog_new} 0.01 0.10 180 > ${out_new}

echo "---- test: ${bname} ----" |& tee -a ${txt_diff}
3dDiff -a ${out_old} -b ${out_new} -tol 0.0001 |& tee -a ${txt_diff}

1dplot -one -ynames "old" "new" -dashed 1:3 -title "${bname}" \
    ${out_old} ${out_new} >& ${out_log} &





exit 0

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
