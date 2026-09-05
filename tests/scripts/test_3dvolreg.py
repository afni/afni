from pathlib import Path
import subprocess as sp

import nibabel as nb
import numpy as np
import pytest
from afni_test_utils import tools

# No external data: the input is synthesised in the test's output directory.
data_paths = {}

NX, NY, NZ, NT = 12, 12, 6, 8


def make_epi_with_stale_afni_extension(outdir):
    """Return a 4D NIfTI whose AFNI extension says NT volumes while its header
    (and data) hold only NT-1.

    This is what a file looks like after a nibabel/nilearn based step drops
    volumes from an AFNI-written NIfTI but keeps the header extensions, as in
    the MRIQC pipeline of issue #73."""
    rng = np.random.default_rng(0)
    arr = (1000 + 100 * rng.standard_normal((NX, NY, NZ, NT))).round()
    img = nb.Nifti1Image(arr.astype(np.int16), np.diag([3.0, 3.0, 3.0, 1.0]))
    img.header.set_xyzt_units("mm", "sec")
    img.header["pixdim"][4] = 2.0
    synth = outdir / "synth.nii"
    nb.save(img, synth)

    # Let AFNI write the file, so it carries an AFNI extension (TAXIS_NUMS etc.)
    withext = outdir / "withext.nii"
    sp.run(
        ["3dcalc", "-a", str(synth), "-expr", "a", "-prefix", str(withext)],
        check=True,
        stdout=sp.DEVNULL,
        stderr=sp.DEVNULL,
    )

    # Drop the last volume with nibabel; extensions are carried over unchanged.
    src = nb.load(withext)
    dropped = nb.Nifti1Image(
        np.asarray(src.dataobj)[..., :-1], src.affine, header=src.header
    )
    assert len(dropped.header.extensions) > 0
    out = outdir / "dropped.nii"
    nb.save(dropped, out)
    return out


def test_3dvolreg_nifti_stale_afni_extension(data):
    """3dvolreg must write a complete output (and not exit 0 with an empty
    file) when the input's AFNI extension disagrees with the NIfTI header
    about the number of volumes (issue #73)."""
    infile = make_epi_with_stale_afni_extension(data.outdir)

    # the length of the time axis must agree with the number of sub-bricks
    ntimes, nv = sp.run(
        ["3dinfo", "-ntimes", "-nv", str(infile)],
        capture_output=True,
        text=True,
        check=True,
    ).stdout.split()[-2:]
    assert int(ntimes) == int(nv) == NT - 1

    outfile = data.outdir / "vr.nii.gz"
    cmd = """
    3dvolreg
        -prefix {outfile}
        -1Dfile {data.outdir}/vr.1D
        -base 0
        {infile}
    """
    cmd = " ".join(cmd.format(**locals()).split())
    # run_cmd raises if 3dvolreg exits non-zero
    tools.run_cmd(data, cmd)

    assert outfile.exists() and outfile.stat().st_size > 0
    assert nb.load(outfile).shape == (NX, NY, NZ, NT - 1)
