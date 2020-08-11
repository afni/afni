from afni_test_utils import tools
import filecmp

# Define Data
data_paths = {
    "anatomical": "mini_data/anat_3mm.nii.gz",
    "no_skull": "mini_data/anat_3mm_no_skull_zero_padded.nii.gz",
}


def test_3dcalc_basic(data):
    outfile = data.outdir / "outside_brain.nii.gz"
    cmd = """
    3dcalc -a {data.anatomical} -b {data.no_skull} -expr 'a*not(b)' -prefix {outfile}
    """
    cmd = " ".join(cmd.format(**locals()).split())

    # Run command and test all outputs match
    differ = tools.OutputDiffer(
        data, cmd, kwargs_log={"append_to_ignored": ["Output dataset"]}
    )
    differ.run()
