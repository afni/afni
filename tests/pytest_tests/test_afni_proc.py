#! /usr/bin/env python

"""Automated tests for afni_proc.py

To run:
    python3 afni_proc.py
"""

import os
import shutil
import subprocess

here = os.path.abspath(os.path.dirname(__file__))
AFNI_TEST_DATA_PATH = os.path.join(here, 'afni_test_data')
FT_PATH = os.path.join(AFNI_TEST_DATA_PATH, "AFNI_data6", "FT_analysis", "FT")

_subj = "FT"

def _get_afni_proc_path():
    loc = shutil.which('afni_proc.py')
    if loc is None:
        raise Exception("Cannot find afni_proc.py")
    return loc


def test_handout_realcase2():
    """Test the command in the afni_proc.py handout (real case 2)."""

    cmd = """coverage run {afni_proc} -subj_id {subj} \
        -script proc.{subj} -scr_overwrite \
        -blocks tshift align tlrc volreg blur mask scale regress \
        -copy_anat {data}/FT_anat+orig \
        -dsets \
        {data}/FT_epi_r1+orig.HEAD \
        {data}/FT_epi_r2+orig.HEAD \
        {data}/FT_epi_r3+orig.HEAD \
        -volreg_align_to MIN_OUTLIER \
        -volreg_align_e2a \
        -volreg_tlrc_warp \
        -blur_size 4.0 \
        -tcat_remove_first_trs 2 \
        -regress_stim_times \
        {data}/AV1_vis.txt \
        {data}/AV2_aud.txt \
        -regress_stim_labels \
        vis aud \
        -regress_basis 'BLOCK(20,1)' \
        -regress_censor_motion 0.3 \
        -regress_opts_3dD \
        -jobs 2 \
        -gltsym 'SYM: vis -aud' -glt_label 1 V-A \
        -gltsym 'SYM: 0.5*vis +0.5*aud' -glt_label 2 mean.VA \
        -regress_compute_fitts \
        -regress_make_ideal_sum sum_ideal.1D \
        -regress_est_blur_epits \
        -regress_est_blur_errts \
        -regress_run_clustsim yes"""
    cmd = cmd.format(
        afni_proc=_get_afni_proc_path(),
        subj=_subj,
        data=FT_PATH)
    # Raises error on failure.
    subprocess.run(cmd.split(), check=True)

    procfile = "proc.{}".format(_subj)
    assert os.path.isfile(procfile)


def test_handout_realcase3():
    cmd = """coverage run {afni_proc} -subj_id {subj} \
        -script proc.{subj} -scr_overwrite \
        -blocks tshift align tlrc volreg blur mask scale regress \
        -copy_anat {data}/FT_anat+orig \
        -dsets \
        {data}/FT_epi_r1+orig.HEAD \
        {data}/FT_epi_r2+orig.HEAD \
        {data}/FT_epi_r3+orig.HEAD \
        -volreg_align_to MIN_OUTLIER \
        -volreg_align_e2a \
        -volreg_tlrc_warp \
        -blur_size 4.0"""
    cmd = cmd.format(
        afni_proc=_get_afni_proc_path(),
        subj=_subj,
        data=FT_PATH)
    # Raises error on failure.
    subprocess.run(cmd.split(), check=True)

    procfile = "proc.{}".format(_subj)
    assert os.path.isfile(procfile)


def main():
    test_handout_realcase2()
    test_handout_realcase3()


if __name__ == '__main__':
    main()
