#!/bin/tcsh

# file for testing old/new 1dCorrelate

# 2026-08-12 : [pt] 1dCorrelate got an update under the hood, now
#              using a different/faster version of the Ziggurat
#              algorithm. This script tests the older and new versions
#              for speed.
#              + in this case, using '-vsig ..' adds a lot of calls to the 
#                Ziggurat-Gaussian func, so we see time differences
# 
# ===========================================================================

set prog = 1dCorrelate
set idx  = 01

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

set all_dat  = `find . -maxdepth 1 -name "data_input_??.1D" | cut -b3- | sort`
set nfind    = 6
set ndat     = ${#all_dat}

if ( ${ndat} != ${nfind} ) then
cat <<EOF

** ERROR: could only find ${ndat} out of ${nfind} input dsets

   Consider copy+pasting these lines these to generate test data:

     1dcat jrandom1D:20,2    | column -t > data_input_01.1D
     1dcat jrandom1D:50,2    | column -t > data_input_02.1D
     1dcat jrandom1D:200,2   | column -t > data_input_03.1D
     1dcat jrandom1D:500,2   | column -t > data_input_04.1D
     1dcat jrandom1D:5000,2  | column -t > data_input_05.1D
     1dcat jrandom1D:10000,2 | column -t > data_input_06.1D

EOF
    exit -1
else
    echo "++ Seem to have found enough input dsets to run."
    echo "   Here we go..."
endif

# test data 2: findable?

set all_dat  = `find . -maxdepth 1 -name "data_vsig.1D" | cut -b3- | sort`
set nfind    = 1
set ndat     = ${#all_dat}

if ( ${ndat} != ${nfind} ) then
cat <<EOF

** ERROR: could only find ${ndat} out of ${nfind} input dsets

   Consider copy+pasting these lines these to generate test data:

     echo "0.5 0.75" > data_vsig.1D

EOF
    exit -1
else
    echo "++ Seem to have found enough input dsets to run."
    echo "   Here we go..."
endif

# ===========================================================================

# ---------------------------------------------------------------------------
# make output dir for test results (and init/clear a text file of diffs)

\mkdir -p ${dir_test}
printf '' > ${txt_diff}
# ---------------------------------------------------------------------------

# ===========================================================================
# run tests

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
    ${prog_old} -Pearson -alpha 5.0 -nboot 10000 -vsig data_vsig.1D \
        data_input_${ii}.1D \
        | tail -n 1 | cut -d' ' -f7-10 > ${out_old}
    set time1 = `date +%s%3N`
    @ time_ms_old = ${time1} - ${time0}

    set time0 = `date +%s%3N`
    ${prog_new} -Pearson -alpha 5.0 -nboot 10000 -vsig data_vsig.1D \
        data_input_${ii}.1D \
        | tail -n 1 | cut -d' ' -f7-10 > ${out_new}
    set time1 = `date +%s%3N`
    @ time_ms_new = ${time1} - ${time0}

    echo "---- test: ${bname} ----" |& tee -a ${txt_diff}
    3dDiff -a ${out_old} -b ${out_new} -tol 0.01 |& tee -a ${txt_diff}

    @ time_diff_ms = ${time_ms_new} - ${time_ms_old}

    set time_diff_frac = `echo "scale = 1; 100.0*(${time_ms_new} - ${time_ms_old})/(1.0*${time_ms_old}) " | bc`

cat <<EOF

++ Time info (number of ms) for run: ${ii} 
   old  : ${time_ms_old}
   new  : ${time_ms_new}
   diff : ${time_diff_ms}
   perc : ${time_diff_frac} %

   In the plots that pop up, the thing to check is that the black
   (old) and red (new) lines look similar between old and new (the
   points used to generate them are from distribution values that we
   are checking).

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



