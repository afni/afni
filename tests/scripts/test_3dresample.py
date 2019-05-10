from .utils import tools

# Define Data
data_paths = {
    "anat1": "mini_data/anat_3mm_no_skull.nii.gz",
    "epi": "AFNI_data6/afni/epi_r1+orig.BRIK",
}


def test_3dresample_basic(data, run_cmd):

    outfile_prefix = data.outdir / ("anat_roi_resam.nii.gz")
    cmd = """
    3dresample
        -master {data.epi}
        -prefix {outfile_prefix}
        -inset {data.anat1}
        -rmode NN
        -verbose
    """

    proc = run_cmd(cmd, locals())
    # Test all outputs match
    tools.assert_all_files_equal(data)
