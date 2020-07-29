from .utils import tools
from pathlib import Path
import pytest
import os


fatdir = Path("AFNI_demos/FATCAT_DEMO")
fatdir2 = fatdir / "test_extra"
# Define Data
data_paths = {
    "SOME_ICA_NETS_in_DWI": fatdir / "SOME_ICA_NETS_in_DWI+orig.BRIK.gz",
    "MULTISITE_in_DWI": fatdir / "MULTISITE_in_DWI+orig.BRIK",
    "mask_DWI": fatdir / "mask_DWI+orig.BRIK.gz",
    "grad_BMAT": fatdir2 / "grad_BMAT.txt",
    "AVEB0_DWI": fatdir / "AVEB0_DWI.nii.gz",
    "bvec": fatdir / "bvec",
    "bval": fatdir / "bval",
    "DT_FA": fatdir2 / "DT_FA+orig.BRIK.gz",
    "DT_DT": fatdir2 / "DT_DT+orig.BRIK.gz",
    "DT_V1": fatdir2 / "DT_V1+orig.BRIK.gz",
    "ROI_ICMAP_GMI": fatdir2 / "ROI_ICMAP_GMI+orig.BRIK.gz",
    "ROI_ICMAP_GM": fatdir2 / "ROI_ICMAP_GM+orig.BRIK.gz",
    "REST_in_DWI": fatdir / "REST_in_DWI.nii.gz",
    "all_dt_files": fatdir2 / "DT_*",
    "grad_cvec_n_one": fatdir2 / "grad_cvec_n-1.txt",
    "OLSAMP": fatdir2 / "o.3dLombScargle_amp.nii.gz",
    "odtT": fatdir2 / "DT_TORTOISE_PREP.nii.gz",
    "uncert": fatdir2 / "o.UNCERT+orig.BRIK.gz",
    "mask_minisphere_dwi": fatdir2 / "mask_minisphere_dwi.nii.gz",
    "mini_REST_proc_unfilt": fatdir2 / "mini_REST_proc_unfilt.nii.gz",
    "mask_eroded_dwi": fatdir2 / "mask_eroded_dwi.nii.gz",
}


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
def test_3ddot_beta(data, ptaylor_env):
    opref = data.outdir / "o.3ddot_beta"
    cmd = f"""
    3ddot_beta
        -echo_edu
        -input {data.SOME_ICA_NETS_in_DWI}
        -mask {data.mask_DWI}
        -doeta2
        -prefix {opref}
    """
    cmd = " ".join(cmd.split())

    differ = tools.OutputDiffer(data, cmd)
    differ.run()


# -------------------------------------------------------------------------


def test_1dDW_Grad_o_Mat_plus_plus_colmat(data, ptaylor_env):
    outfile = data.outdir / "o.1dDW_Grad_o_Mat++_BMAT.txt"
    cmd = f"""
    1dDW_Grad_o_Mat++
        -echo_edu
        -in_row_vec {data.bvec}'[2..32]'
        -in_bvals {data.bval}'[2..32]'
        -flip_y
        -out_col_matA {outfile}
    """
    cmd = " ".join(cmd.split())

    differ = tools.OutputDiffer(data, cmd)
    differ.run()


def test_1dDW_Grad_o_Mat_plus_plus_colvec(data, ptaylor_env):
    outfile = data.outdir / "o.1dDW_Grad_o_Mat++_cvec_n-1.txt"
    cmd = f"""
    1dDW_Grad_o_Mat++
        -echo_edu
        -in_row_vec {data.bvec}'[3..32]'
        -flip_y
        -out_col_vec {outfile}

    """
    cmd = " ".join(cmd.split())
    differ = tools.OutputDiffer(data, cmd)
    differ.run()


# -------------------------------------------------------------------------
@pytest.mark.veryslow
def test_3dDWItoDT(data, ptaylor_env):
    opref = data.outdir / "3dDWItoDT"
    # just using '-linear' for speed!
    cmd = f"""
    3dDWItoDT
        -echo_edu
        -prefix {opref}
        -mask {data.mask_DWI}
        -eigs
        -scale_out_1000
        -sep_dsets
        -linear
        -bmatrix_FULL {data.grad_BMAT}
        {data.AVEB0_DWI}
    """
    cmd = " ".join(cmd.split())

    differ = tools.OutputDiffer(data, cmd, kwargs_scans={"data_kwargs": {"rtol": 0.01}})
    differ.run()


# -------------------------------------------------------------------------


def test_3dROIMaker(data, ptaylor_env):
    opref = data.outdir / "o.3dROIMaker"
    cmd = f"""
    3dROIMaker
        -echo_edu
        -nifti
        -inset {data.SOME_ICA_NETS_in_DWI}
        -thresh 3.0
        -volthr 130
        -inflate 2
        -wm_skel {data.DT_FA}
        -skel_thr 0.2
        -skel_stop
        -mask {data.mask_DWI}
        -prefix {opref}
        -overwrite
    """
    cmd = " ".join(cmd.split())

    differ = tools.OutputDiffer(data, cmd)
    differ.run()


# -------------------------------------------------------------------------


@pytest.mark.slow
def test_3dDTtoNoisyDWI(data, ptaylor_env):
    opref = data.outdir / "o.3dDTtoNoisyDWI"
    cmd = f"""
    3dDTtoNoisyDWI
        -echo_edu
            -grads {data.grad_cvec_n_one}
        -choose_seed 7
        -dt_in {data.DT_DT}
        -mask {data.mask_minisphere_dwi}
        -noise_DWI 0.05
        -noise_B0 0
        -prefix {opref}
    """
    cmd = " ".join(cmd.split())

    differ = tools.OutputDiffer(data, cmd)
    differ.run()


# -------------------------------------------------------------------------


def test_3dEigsToDT(data, ptaylor_env):
    opref = data.outdir / "o.3dEigsToDT"
    cmd = f"""
    3dEigsToDT
    -echo_edu
    -eig_vals '{data.tests_data_dir / fatdir2}/DT_L*'
    -eig_vecs '{data.tests_data_dir / fatdir2}/DT_V*'
    -prefix {opref}
    """
    cmd = " ".join(cmd.split())

    differ = tools.OutputDiffer(data, cmd)
    differ.run()


# -------------------------------------------------------------------------


def test_3dVecRGB_to_HSL(data, ptaylor_env):
    opref = data.outdir / "o.3dVecRGB_to_HSL"
    cmd = f"""
    3dVecRGB_to_HSL
        -echo_edu
        -in_vec {data.DT_V1}
        -in_scal {data.DT_FA}
        -mask {data.mask_DWI}
        -prefix {opref}
    """
    cmd = " ".join(cmd.split())

    differ = tools.OutputDiffer(data, cmd)
    differ.run()


# ------------------------------------------------------------------------


def test_3dTORTOISEtoHere(data, ptaylor_env):
    opref = data.outdir / "o.3dTORTOISEtoHere"

    cmd = f"""
    3dTORTOISEtoHere
        -echo_edu
        -dt_tort {data.odtT}
        -prefix {opref}
    """
    cmd = " ".join(cmd.split())

    differ = tools.OutputDiffer(data, cmd)
    differ.run()


# -------------------------------------------------------------------------


@pytest.mark.veryslow
def test_3dDWUncert(data, ptaylor_env):
    opref = data.outdir / "o.3dDWUncert"
    cmd = f"""
    3dDWUncert
        -echo_edu
        -inset {data.AVEB0_DWI}
        -mask {data.mask_DWI}
        -pt_choose_seed 5
        -prefix {opref}
        -input {data.tests_data_dir / fatdir2}/DT
        -bmatrix_FULL {data.grad_BMAT}
        -iters 10

    """
    cmd = " ".join(cmd.split())

    differ = tools.OutputDiffer(
        data,
        cmd,
        kwargs_log={"append_to_ignored": [" min", "Nvox progress proxy count"]},
    )

    differ.run()


# -------------------------------------------------------------------------


@pytest.mark.slow
@pytest.mark.xfail(raises=NotImplementedError)
def test_3dTrackID_DET(data, ptaylor_env):
    opref = data.outdir / "o.3dTrackID_DET"

    cmd = f"""
    3dTrackID
        -echo_edu
        -mode DET
        -dti_in {data.tests_data_dir / fatdir2}/DT
        -netrois {data.mask_eroded_dwi}
        -logic OR
        -alg_Thresh_Len 30
        -alg_Nseed_X 1
        -alg_Nseed_Y 1
        -alg_Nseed_Z 1
        -prefix {opref}
    """
    cmd = " ".join(cmd.split())

    differ = tools.OutputDiffer(data, cmd)
    differ.run()


@pytest.mark.veryslow
@pytest.mark.xfail(raises=NotImplementedError)
def test_3dTrackID_PROB(data, ptaylor_env):
    opref = data.outdir / "o.3dTrackID_PROB"
    cmd = f"""
    3dTrackID
        -echo_edu
        -mode PROB
        -choose_seed 5
        -no_indipair_out
        -nifti
        -dti_in {data.tests_data_dir / fatdir2}/DT
        -uncert {data.uncert}
        -netrois {data.ROI_ICMAP_GMI}
        -mask {data.mask_DWI}
        -alg_Thresh_Frac 0.2
        -alg_Nseed_Vox 5
        -alg_Nmonte 5
        -dump_rois AFNI
        -prefix {opref}

    """
    cmd = " ".join(cmd.split())

    differ = tools.OutputDiffer(data, cmd)
    differ.run()


# -------------------------------------------------------------------------


@pytest.mark.slow
@pytest.mark.xfail(raises=NotImplementedError)
def test_3dNetCorr(data, ptaylor_env):
    opref = data.outdir / "o.3dNetCorr"
    cmd = f"""
    3dNetCorr
        -echo_edu
        -inset {data.REST_in_DWI}'[5..50]'
        -in_rois {data.ROI_ICMAP_GM}
        -prefix {opref}
        -fish_z
        -ts_wb_Z
        -nifti
        -ts_out
    """
    cmd = " ".join(cmd.split())
    differ = tools.OutputDiffer(data, cmd)
    differ.run()


# -------------------------------------------------------------------------


@pytest.mark.slow
def test_3dMatch(data, ptaylor_env):
    opref = data.outdir / "o.3dMatch"
    cmd = f"""
    3dMatch
        -echo_edu
        -only_dice_thr
        -inset {data.SOME_ICA_NETS_in_DWI}
        -refset {data.MULTISITE_in_DWI}
        -mask {data.mask_DWI}
        -in_min 1.0
        -ref_min 3
        -prefix {opref}
    """
    cmd = " ".join(cmd.split())
    differ = tools.OutputDiffer(data, cmd)
    differ.run()


# -------------------------------------------------------------------------


def test_3dRSFC(data, ptaylor_env):
    opref = data.outdir / "o.3dRSFC"
    cmd = f"""
    3dRSFC
        -echo_edu
        -input {data.mini_REST_proc_unfilt}
        -prefix {opref}
        -band 0.01 0.1
    """
    cmd = " ".join(cmd.split())
    differ = tools.OutputDiffer(data, cmd)
    differ.run()


# -------------------------------------------------------------------------


def test_3dLombScargle(data, ptaylor_env):
    olsamp = data.outdir / "o.3dLombScargle"
    cmd = f"""
    3dLombScargle
        -echo_edu
        -nifti
        -inset {data.mini_REST_proc_unfilt}
        -prefix {olsamp}
        -censor_str '[0..3,5,6..18,22..34,36..47,49,51..60,63..84,86..88,90,92..$]'
    """
    cmd = " ".join(cmd.split())
    differ = tools.OutputDiffer(data, cmd)
    differ.run()


# -------------------------------------------------------------------------


def test_3dAmpToRSFC(data, ptaylor_env):

    opref = data.outdir / "o.3dAmpToRSFC"
    cmd = f"""
    3dAmpToRSFC
        -nifti
        -band 0.01 0.1
        -mask {data.mask_DWI}
        -in_amp {data.OLSAMP}
        -prefix {opref}
    """
    cmd = " ".join(cmd.split())
    differ = tools.OutputDiffer(data, cmd)
    differ.run()


# -------------------------------------------------------------------------


def test_3dClusterize(data, ptaylor_env):
    opref = data.outdir / "o.3dClusterize"
    cmd = f"""
    3dClusterize
        -echo_edu
        -mask {data.mask_DWI}
        -inset {data.SOME_ICA_NETS_in_DWI}
        -ithr 0
        -idat 0
        -bisided -3 3
        -NN 2
        -clust_nvox 200
        -pref_map {opref}_MAP.nii.gz
        -pref_dat {opref}_DAT.nii.gz
    """
    # > {opref}_REP.txt
    cmd = " ".join(cmd.split())
    differ = tools.OutputDiffer(data, cmd)
    differ.run()


def test_3dClusterize_new(data, ptaylor_env):
    opref = data.outdir / "o.3dClusterize"
    cmd = f"""
    3dClusterize
        -echo_edu
        -mask {data.mask_DWI}
        -inset {data.SOME_ICA_NETS_in_DWI}
        -ithr 0
        -idat 0
        -bisided -3 3
        -NN 2
        -clust_nvox 200
        -pref_map {opref}_MAP.nii.gz
        -pref_dat {opref}_DAT.nii.gz
    """
    # > {opref}_REP.txt
    cmd = " ".join(cmd.split())
    differ = tools.OutputDiffer(data, cmd)
    differ.run()


# -------------------------------------------------------------------------


@pytest.mark.slow
def test_3dReHo(data, ptaylor_env):
    opref = data.outdir / "o.3dReHo"

    cmd = f"""
    3dReHo
        -echo_edu
        -inset {data.mini_REST_proc_unfilt}'[0..45]'
        -nneigh 7
        -prefix {opref}
    """
    cmd = " ".join(cmd.split())
    differ = tools.OutputDiffer(data, cmd)
    differ.run()


def test_3dReHo_with_box(data, ptaylor_env):
    # check with ROIs (and different kind of neighborhood opts
    opref = data.outdir / "o.3dReHo_ROI"
    cmd = f"""
        3dReHo
            -echo_edu
            -mask {data.mask_DWI}
            -inset {data.REST_in_DWI}'[5..50]'
            -box_X 2
            -box_Y 2
            -box_Z 3
            -prefix {opref}
            -in_rois {data.ROI_ICMAP_GM}
        """
    cmd = " ".join(cmd.split())
    differ = tools.OutputDiffer(data, cmd)
    differ.run()


# -------------------------------------------------------------------------
def test_3dSliceNDice(data, ptaylor_env):
    opref = data.outdir / "o.3dSliceNDice"
    # make masks first
    cmd = f"""
        3dcalc
            -echo_edu
            -a {data.SOME_ICA_NETS_in_DWI}'[0]'
            -b {data.mask_DWI}
            -expr 'step(b)*(step(a-3)+step(-3-a))'
            -prefix {opref}_preA.nii.gz;
        3dcalc
            -echo_edu
            -a {data.SOME_ICA_NETS_in_DWI}'[1]'
            -b {data.mask_DWI}
            -expr 'step(b)*(step(a-3)+step(-3-a))'
            -prefix {opref}_preB.nii.gz;

        3dSliceNDice
            -echo_edu
            -out_domain all
            -insetA {opref}_preA.nii.gz
            -insetB {opref}_preB.nii.gz
            -prefix {opref}
        """
    cmd = " ".join(cmd.split())
    differ = tools.OutputDiffer(data, cmd)
    differ.run()


# -------------------------------------------------------------------------


def test_3dSpaceTimeCorr(data, ptaylor_env):
    opref = data.outdir / "o.3dSpaceTimeCorr"
    cmd = f"""
    3dSpaceTimeCorr
        -echo_edu
        -insetA {data.REST_in_DWI}'[5..15]'
        -insetB {data.REST_in_DWI}'[25..35]'
        -mask {data.mask_minisphere_dwi}
        -prefix {opref}
    """
    cmd = " ".join(cmd.split())
    differ = tools.OutputDiffer(
        data,
        cmd,
        kwargs_log={"append_to_ignored": [" min"]},
        kwargs_scans={"data_kwargs": {"rtol": 0.01}},
    )
    differ.run()


# -------------------------------------------------------------------------


def test_3dZipperZapper(data, ptaylor_env):
    opref = data.outdir / "o.3dZipperZapper"
    cmd = f"""
    3dZipperZapper
        -echo_edu
        -input {data.AVEB0_DWI}'[0..10]'
        -mask {data.mask_minisphere_dwi}
        -prefix {opref}
        -do_out_slice_param
    """
    cmd = " ".join(cmd.split())
    differ = tools.OutputDiffer(data, cmd)
    differ.run()
