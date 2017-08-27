#! /usr/bin/env python

"""Show the help messages of all AFNI programs. Fail test if showing help
message fails or if program is not found.

To run:
    python3 test_help_messages.py
"""

import os
from pathlib import Path
import shutil
import subprocess

here = os.path.realpath(os.path.dirname(__file__))

# This assumes we are in afni_root/tests/pytest_tests directory.
AFNI_ROOT = os.path.join(here, '..', '..')

SHOULD_NOT_BE_EXECUTABLE = [
    'afni_fs_aparc+aseg_2000.txt',
    'afni_fs_aparc+aseg_2009.txt',
    'demo.fixed.niml.do',
    'demo.mobile.niml.do',
        'CMakeLists.txt']

KNOWN_BROKEN_HELP = [
    # basemap not installed: https://matplotlib.org/basemap/users/installing.html
    'fat_proc_grad_plot', 
     # requires dcm2niix_afni
    'fat_proc_convert_dcm_anat',
    'fat_proc_convert_dcm_dwis'
    ]

NOT_INSTALLED = [
    '3dBRAIN_VOYAGERtoAFNI',
    '3dBrainSync',
    '3dCRUISEtoAFNI',
    '3dClusterize',
    '3dGenFeatureDist',
    '3dGenPriors',
    '3dGrayplot',
    '3dHist',
    '3dMaskToASCII',
    '3dProbTrackID',
    '3dSeg',
    '3dSkullStrip',
    '3dSliceNDice',
    '3dSurf2Vol',
    '3dSurfMask',
    '3dTto1D',
    '3dVol2Surf',
    '3dZipperZapper',
    '3dinfill',
    'AnalyzeTrace',
    'BrainSkin',
    'CompareSurfaces',
    'ConvertDset',
    'ConvertSurface',
    'ConvexHull',
    'CreateIcosahedron',
    'DriveSuma',
    'FSread_annot',
    'HalloSuma',
    'InstaTract',
    'IsoSurface',
    'MakeColorMap',
    'MapIcosahedron',
    'ParseName',
    'ROI2dataset',
    'ROIgrow',
    'RetroTS.py',
    'SUMA_glxdino',
    'SUMA_paperplane',
    'SUMA_pixmap2eps',
    'SampBias',
    'ScaleToMap',
    'SpharmDeco',
    'SpharmReco',
    'Surf2VolCoord',
    'SurfClust',
    'SurfDist',
    'SurfDsetInfo',
    'SurfExtrema',
    'SurfFWHM',
    'SurfInfo',
    'SurfMeasures',
    'SurfMesh',
    'SurfPatch',
    'SurfQual',
    'SurfRetinoMap',
    'SurfSmooth',
    'SurfToSurf',
    'SurfaceMetrics',
    'Xphace',
    'afni_history',
    'afni_open',
    'aiv',
    'cjpeg',
    'dcm2niix_afni',
    'djpeg',
    'get_afni_model_PRF',
    'get_afni_model_PRF_6',
    'get_afni_model_PRF_6_BAD',
    'inspec',
    'meica.py',
    'mpeg_encode',
    'nifti1_tool',
    'niprobe',
    'prompt_popup',
    'prompt_user',
    'qdelaunay',
    'qhull',
    'quickspec',
    'suma',
    '1dsound']


def _get_programs(afni_root):
    """Return list of AFNI programs, given path to AFNI's root directory,
    right above src.
    """
    src = Path(afni_root).absolute() / 'src'
    makefile =  'Makefile.INCLUDE'
    # Get list of AFNI programs.
    subprocess.run(
        ['make', '-f', makefile, 'prog_list'],
         stdout=subprocess.DEVNULL,
         cwd = str(src))
    prog_list = (src /'prog_list.txt').read_text().splitlines()
    prog_list = [j.strip() for j in prog_list if j.strip()]
    (src /'prog_list.txt').unlink()
    # Parse and return list of AFNI programs.
    return [j for j in prog_list if not j.startswith('#')]


def test_prog_list_helps():
    programs = _get_programs(AFNI_ROOT)
    not_found = []
    no_success = []
    timedout = []

    for prog in programs:
        if prog in set([ *SHOULD_NOT_BE_EXECUTABLE, *KNOWN_BROKEN_HELP, *NOT_INSTALLED]):
            continue
        if shutil.which(prog) is None:  # Program does not exist.
            not_found.append(prog)
            continue

        # Run program's help.
        try:
            process = subprocess.run( 
                [prog, '-help'], 
                stderr=subprocess.PIPE,
                stdout=subprocess.DEVNULL,
                timeout=1)

            if process.returncode != 0:
                msg = "return code {}".format(process.returncode)
                if process.stderr:
                    msg = process.stderr.splitlines()[-1].decode()
                no_success.append(
                    "{} ({})".format(prog, msg))
        except subprocess.TimeoutExpired:
            timedout.append("{}".format(prog))
            

    if not_found:
        print("PROGRAMS NOT FOUND:")
        print("    " + "\n    ".join(not_found))

    if no_success:
        print("PROGRAMS THAT FAILED:")
        print("    " + "\n    ".join(no_success))
    if timedout:
        print("PROGRAMS THAT Timed-out:")
        print("    " + "\n    ".join(timedout))

    if not_found or no_success:
        raise ValueError("Some help messages not working.")


def main():
    test_prog_list_helps()


if __name__ == '__main__':
    main()
