#!/bin/tcsh

set dset_00 = anat_ASL_345.nii.gz
### to create the above
# 3dresample -input anat+orig. -prefix anat_RAI_333.nii.gz \
#            -orient RAI -dxyz 3 3 3
# 3dresample -orient ASL -prefix anat_ASL_333.nii.gz       \
#            -input anat_RAI_333.nii.gz
# 3dcopy     anat_ASL_333.nii.gz anat_ASL_345.nii.gz
# 3drefit    -xdel 3 -ydel 4 -zdel 5 anat_ASL_345.nii.gz

set pref_refit = test_aform_refit
set pref_resam = test_aform_resam

set all_ornt = `3dinfo -orient "${dset_00}"`
set all_ornt = ( ${all_ornt} RAI LPI ASL RPS SPR AIR )

#if ( 0 ) then    # make an option to turn this part on/off later, as nec
    foreach ooo ( ${all_ornt} )

        echo "++ create new test dsets, for orient:  ${ooo}"

        # ---------- make dsets for refit

        set dset_refit = ${pref_refit}_${ooo}.nii.gz

        3dcopy                              \
            -overwrite                      \
            "${dset_00}"                    \
            ${dset_refit}

        3drefit                             \
            -orient ${ooo}                  \
            ${dset_refit}

        # ---------- make dsets for resample

        set dset_resam = ${pref_resam}_${ooo}.nii.gz

        3dresample                          \
            -overwrite                      \
            -orient  ${ooo}                 \
            -prefix  ${dset_resam}          \
            -input  "${dset_00}"

    end
#endif

# -----------------------------------------------------------------------

# define+initialize "answer" outputs
set file_refit = results_refit.1D
set file_resam = results_resam.1D
printf "" > ${file_refit} ; printf "" > ${file_resam} # clear

# define+initialize "test" outputs
set aform_refit = new_aform_refit.1D
set aform_resam = new_aform_resam.1D
printf "" > ${aform_refit} ; printf "" > ${aform_resam} # clear

foreach ooo ( ${all_ornt} )

    echo "++ ORIENT: ${ooo}"

    set dset_refit = ${pref_refit}_${ooo}.nii.gz
    set dset_resam = ${pref_resam}_${ooo}.nii.gz

    # the "correct" answers for each kind of behavior
    3dinfo -aform_real_oneline ${dset_refit} >> ${file_refit}
    3dinfo -aform_real_oneline ${dset_resam} >> ${file_resam}

    # at somepoint, will have diff opts for these
    3dinfo -aform_real_refit_ori ${ooo} "${dset_00}" > tmp.1D
    cat_matvec -ONELINE tmp.1D >> ${aform_refit}
    3dinfo -aform_real_refit_ori ${ooo} "${dset_00}" > tmp.1D
    cat_matvec -ONELINE tmp.1D >> ${aform_resam}
end

# -----------------------------------------------------------------------

# -------------- test refit
set diff_refit_mat = diff_refit_mat.1D
set diff_refit_row = diff_refit_row.1D
set diff_refit_sca = diff_refit_sca.1D
3dcalc                                                             \
    -a ${file_refit}\'                                             \
    -b ${aform_refit}\'                                            \
    -expr 'a-b'                                                    \
    -prefix -                                                      \
    | 1dtranspose stdin > ${diff_refit_mat}

3dTstat -abssum -prefix ${diff_refit_row} ${diff_refit_mat}
3dTstat -abssum -prefix ${diff_refit_sca} ${diff_refit_row}\' 

1dcat ${diff_refit_sca}

# -------------- test resam
set diff_resam_mat = diff_resam_mat.1D
set diff_resam_row = diff_resam_row.1D
set diff_resam_sca = diff_resam_sca.1D
3dcalc                                                             \
    -a ${file_resam}\'                                             \
    -b ${aform_resam}\'                                            \
    -expr 'a-b'                                                    \
    -prefix -                                                      \
    | 1dtranspose stdin > ${diff_resam_mat}

3dTstat -abssum -prefix ${diff_resam_row} ${diff_resam_mat}
3dTstat -abssum -prefix ${diff_resam_sca} ${diff_resam_row}\' 

1dcat ${diff_resam_sca}

# -----------------------------------------------------------------------

echo ""
echo "-------------------------------------------"
echo "++ check resam diffs"
cat ${diff_resam_mat}

echo ""
echo "-------------------------------------------"
echo "++ check refit diffs"
cat ${diff_refit_mat}
