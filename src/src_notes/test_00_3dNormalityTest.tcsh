#!/bin/tcsh

# file for testing old/new 3dNormalityTest

# 2026-08-13 : [pt] 3dNormalityTest got an update under the hood, now
#              using a different/faster version of the Ziggurat
#              algorithm. This script tests the older and new versions
#              for similarity.
# 
# ===========================================================================

# set locations of old and new program versions, for comparisons

set path_old = ${HOME}/afni_build_GOOD_2026_07_02_09_07_1783000295
set path_old = ${path_old}/src/linux_ubuntu_16_64_glw_local_shared/
set path_new = ${HOME}/afni_build/src/linux_ubuntu_16_64_glw_local_shared

set prog_old = ${path_old}/3dNormalityTest
set prog_new = ${path_new}/3dNormalityTest

# make output dir for test results (and init/clear a text file of diffs)

set dir_test = testing-3dNormalityTest
set txt_diff = ${dir_test}/all_diffs.txt

\mkdir -p ${dir_test}
printf '' > ${txt_diff}

# input test data created (if need be), via these cmds
if ( ! -f data_input_01.nii.gz ) then
    3dUndump -overwrite -dimen 100 100 100 -prefix __tmp.nii.gz
    3dcalc                                                                   \
        -overwrite                                                           \
        -a       __tmp.nii.gz                                                \
        -b       '1D: 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0'               \
        -expr    'i*gran(0,1.4)+(100-i)*eran(4)'                             \
        -prefix  data_input_01.nii.gz                                        \
        -datum   float
    \rm -f __tmp.nii.gz
else
    echo "++ Already have input data file: data_input_01.nii.gz"
    echo "   ... just proceding with comparison"
endif

# ===========================================================================

# all tests are basically the same, just need to index by 2-digit code
set all_ii = `count_afni -digits 2 1 1`

foreach ii ( ${all_ii} )

    set bname   = ${dir_test}/test-${ii}

    set out_old = ${bname}-old.nii.gz
    set out_new = ${bname}-new.nii.gz
    set out_log = ${bname}-plot-log.txt

    # special things done when running here:
    # + want to record time of each, so this records time values as
    #   integers that count milliseconds: date +%s%3N

    set time0 = `date +%s%3N`
    ${prog_old} -prefix ${out_old} -input data_input_${ii}.nii.gz
    set time1 = `date +%s%3N`
    @ time_ms_old = ${time1} - ${time0}

    set time0 = `date +%s%3N`
    ${prog_new} -prefix ${out_new} -input data_input_${ii}.nii.gz
    set time1 = `date +%s%3N`
    @ time_ms_new = ${time1} - ${time0}

    echo "---- test: ${bname} ----" |& tee -a ${txt_diff}
    3dDiff -a ${out_old} -b ${out_new} -tol 0.1 |& tee -a ${txt_diff}

    @ time_diff_ms = ${time_ms_new} - ${time_ms_old}

    set time_diff_frac = `echo "scale = 1; 100.0*(${time_ms_new} - ${time_ms_old})/(1.0*${time_ms_old}) " | bc`

    # diff vol
    3dcalc                                 \
        -a       ${out_new}                \
        -b       ${out_old}                \
        -expr    "a-b"                     \
        -prefix  ${bname}_DIFF.nii.gz

    echo "++ generate image: old"
    @chauffeur_afni                                                       \
        -ulay             data_input_${ii}.nii.gz                         \
        -ulay_range       -150 1000                                       \
        -olay             ${out_old}                                      \
        -func_range       2                                               \
        -cbar             GoogleTurbo                                     \
        -thr_olay_p2stat  0.01                                            \
        -thr_olay_pside   bisided                                         \
        -olay_alpha       No                                              \
        -olay_boxed       No                                              \
        -set_subbricks    0 0 0                                           \
        -opacity          9                                               \
        -prefix           ${bname}_old                                    \
        -set_xhairs       OFF                                             \
        -montx            8                                               \
        -monty            1                                               \
        -no_sag -no_cor                                                   \
        -blowup           4                                               \
        -label_mode       1                                               \
        -label_size       4                                               \
        >& /dev/null
    echo "++ generate image: new"
    @chauffeur_afni                                                       \
        -ulay             data_input_${ii}.nii.gz                         \
        -ulay_range       -150 1000                                       \
        -olay             ${out_new}                                      \
        -func_range       2                                               \
        -cbar             GoogleTurbo                                     \
        -thr_olay_p2stat  0.01                                            \
        -thr_olay_pside   bisided                                         \
        -olay_alpha       No                                              \
        -olay_boxed       No                                              \
        -set_subbricks    0 0 0                                           \
        -opacity          9                                               \
        -prefix           ${bname}_new                                    \
        -set_xhairs       OFF                                             \
        -montx            8                                               \
        -monty            1                                               \
        -no_sag -no_cor                                                   \
        -blowup           4                                               \
        -label_mode       1                                               \
        -label_size       4                                               \
        >& /dev/null
    echo "++ generate image: diff"
    @chauffeur_afni                                                       \
        -ulay             data_input_${ii}.nii.gz                         \
        -ulay_range       -150 1000                                       \
        -olay             ${bname}_DIFF.nii.gz                            \
        -func_range       2                                               \
        -cbar             GoogleTurbo                                     \
        -thr_olay         0.1                                             \
        -set_subbricks    0 0 0                                           \
        -olay_alpha       Yes                                             \
        -olay_boxed       No                                              \
        -opacity          9                                               \
        -prefix           ${bname}_DIFF                                   \
        -set_xhairs       OFF                                             \
        -montx            8                                               \
        -monty            1                                               \
        -no_sag -no_cor                                                   \
        -blowup           4                                               \
        -label_mode       1                                               \
        -label_size       4                                               \
        >& /dev/null
    echo "++ image: concat"
    2dcat                                                                 \
        -gap      5                                                       \
        -gap_col  70 70 70                                                \
        -nx       1                                                       \
        -ny       3                                                       \
        -prefix   ${bname}_final_OLDNEW.jpg                               \
        ${bname}_old.axi.png ${bname}_new.axi.png ${bname}_DIFF.axi.png

cat <<EOF

++ Time info (number of ms) for run: ${ii} 
   old  : ${time_ms_old}
   new  : ${time_ms_new}
   diff : ${time_diff_ms}
   perc : ${time_diff_frac} %

EOF

cat <<EOF
---------------------------
++ And check out this image:
    
     ${bname}_final_OLDNEW.jpg 

   top row : old
   mid row : new
   bot row : diffs

   Hopefully:
    + the first two rows look similar, 
    + the bottom row is sparse or with zero-ish colors (bc there is
      randomness involved, we expect some nonzero, float-ish diffs)

EOF


end

# ===========================================================================

echo "++ DONE.  Check diffs file:"
echo "-------------------------------------------------"
cat ${txt_diff}
echo "-------------------------------------------------"

exit 0



