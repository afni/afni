from afni_test_utils import tools

# Define Data
data_paths = {"anatomical": "mini_data/anat_3mm.nii.gz"}


def test_3dcopy_basic(data):
    outfile = data.outdir / "copied.nii.gz"
    cmd = """
    3dcopy {data.anatomical} {outfile}
    """
    cmd = " ".join(cmd.format(**locals()).split())
    # Run command and test all outputs match
    differ = tools.OutputDiffer(data, cmd)
    differ.run()
