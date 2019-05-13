from .utils import tools

# Define Data
data_paths = {
    "mask": "mini_data/mask_3mm.nii.gz",
    "epi": "sample_test_output/3dAllineate/test_3dAllineate_basic/aligned_with_omp.nii.gz",
}


def test_3dmaskave_basic(data, run_cmd):

    outfile_prefix = data.outdir / ("anat_roi_resam.nii.gz")
    out_1d = data.outdir / ("epi_avg.1D")
    cmd = """
    3dmaskave
        -mask {data.mask}
        -quiet {data.epi}
        > {out_1d}
    """

    proc = run_cmd(cmd, locals())
    # Test all outputs match
    tools.assert_all_files_equal(data)
