from afni_test_utils import tools

# Define Data
data_paths = {
    "anat1": "mini_data/anat_3mm_no_skull.nii.gz",
    "epi": "AFNI_data6/afni/epi_r1+orig.BRIK",
}


def test_3dresample_basic(data):

    outfile_prefix = data.outdir / ("anat_roi_resam.nii.gz")
    cmd = """
    3dresample
        -master {data.epi}
        -prefix {outfile_prefix}
        -inset {data.anat1}
        -rmode NN
        -verbose
    """
    cmd = " ".join(cmd.format(**locals()).split())

    # Run command and test all outputs match
    differ = tools.OutputDiffer(data, cmd)
    differ.run()
