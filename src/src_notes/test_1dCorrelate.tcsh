#!/bin/tcsh

# file for testing old/new 1dCorrelate

# 2026-08-12 : [pt] 1dCorrelate got an update under the hood, now
#              using a different/faster version of the Ziggurat
#              algorithm. This script tests the older and new versions
#              for similarity.
# 
# ===========================================================================

# set locations of old and new program versions, for comparisons

set path_old = ${HOME}/afni_build_GOOD_2026_08_12_11_08_1786548635
set path_old = ${path_old}/src/linux_ubuntu_16_64_glw_local_shared/
set path_new = ${HOME}/afni_build/src/linux_ubuntu_16_64_glw_local_shared

set prog_old = ${path_old}/1dCorrelate
set prog_new = ${path_new}/1dCorrelate

# make output dir for test results (and init/clear a text file of diffs)

set dir_test = testing-1dCorrelate
set txt_diff = ${dir_test}/all_diffs.txt

\mkdir -p ${dir_test}
printf '' > ${txt_diff}

# input test scripts were created once, via these cmds
# 1dcat jrandom1D:20,2    | column -t -R 0 > data_input_01.1D
# 1dcat jrandom1D:50,2    | column -t -R 0 > data_input_02.1D
# 1dcat jrandom1D:200,2   | column -t -R 0 > data_input_03.1D
# 1dcat jrandom1D:500,2   | column -t -R 0 > data_input_04.1D
# 1dcat jrandom1D:5000,2  | column -t -R 0 > data_input_05.1D
# 1dcat jrandom1D:10000,2 | column -t -R 0 > data_input_06.1D

# ===========================================================================

# all tests are basically the same, just need to index by 2-digit code
set all_ii = `count_afni -digits 2 1 6`

foreach ii ( ${all_ii} )

    set bname   = ${dir_test}/test-${ii}

    set out_old = ${bname}-old.1D
    set out_new = ${bname}-new.1D
    set out_log = ${bname}-plot-log.txt

    # a couple special things done when running here:
    # + have to select just the 4 quantities of interest from outputs
    # + want to record time of each, so this records time values as
    #   integers that count milliseconds: date +%s%3N

    set time0 = `date +%s%3N`
    ${prog_old} -Spearman -alpha 5.0 -nboot 10000 data_input_${ii}.1D \
        | tail -n 1 | cut -d' ' -f7-10 > ${out_old}
    set time1 = `date +%s%3N`
    @ time_ms_old = ${time1} - ${time0}

    set time0 = `date +%s%3N`
    ${prog_new} -Spearman -alpha 5.0 -nboot 10000 data_input_${ii}.1D \
        | tail -n 1 | cut -d' ' -f7-10 > ${out_new}
    set time1 = `date +%s%3N`
    @ time_ms_new = ${time1} - ${time0}

    echo "---- test: ${bname} ----" |& tee -a ${txt_diff}
    3dDiff -a ${out_old} -b ${out_new} -tol 0.01 |& tee -a ${txt_diff}

    @ time_diff_ms = ${time_ms_new} - ${time_ms_old}

cat <<EOF

++ Time info (number of ms) for run: ${ii} 
   old  : ${time_ms_old}
   new  : ${time_ms_new}
   diff : ${time_diff_ms}

EOF

    1dplot -one -ynames "old" "new" -dashed 1:3 -title "${bname}" \
        ${out_old} ${out_new} >& ${out_log} &

end

# ===========================================================================

echo "++ DONE.  Check diffs file:"
echo "-------------------------------------------------"
cat ${txt_diff}
echo "-------------------------------------------------"

exit 0



