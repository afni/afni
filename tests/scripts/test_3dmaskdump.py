from .utils import tools

# Define Data
data_paths = {
    "mask": "mini_data/mask_3mm.nii.gz",
    "epi": "sample_test_output/3dAllineate/test_3dAllineate_basic/aligned_with_omp.nii.gz",
}


def test_3dmaskdump_basic(data, run_cmd):

    outfile_path = data.outdir / ("Vrel_tstats.txt")
    cmd = """
    3dmaskdump
        -noijk
        -mask {data.mask} {data.epi}
        > {outfile_path}
    """

    proc = run_cmd(cmd, locals())
    # Test all outputs match
    tools.assert_all_files_equal(data)
