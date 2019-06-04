from .utils import tools

# Define Data
data_paths = {"mask": "mini_data/mask_3mm.nii.gz", "epi": "mini_data/aligned.nii.gz"}


def test_3dmaskave_basic(data):

    outfile_prefix = data.outdir / ("anat_roi_resam.nii.gz")
    out_1d = data.outdir / ("epi_avg.1D")
    cmd = """
    3dmaskave
        -mask {data.mask}
        -quiet {data.epi}
        > {out_1d}
    """
    cmd = " ".join(cmd.format(**locals()).split())

    # Run command and test all outputs match
    differ = tools.OutputDiffer(data, cmd)
    differ.run()
