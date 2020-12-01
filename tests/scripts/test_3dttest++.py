from pathlib import Path
import shutil
from afni_test_utils import tools
import pytest

# pre-defs for data_paths
ad6_idstr = "FP FR FT FV FX GF GG GI GK GM"
ad6_idlist = ad6_idstr.split()
ad6_olsq_dsets = ["OLSQ.%s.betas+tlrc.HEAD" % sid for sid in ad6_idlist]
ad6_olsq_wpath = ["AFNI_data6/group_results/%s" % d for d in ad6_olsq_dsets]

# main data_paths def
base_path = Path("old_test_data") / "3dttest++"
data_paths = {
    # "Zovar": base_path / "Zovar",
    # "Tovar": base_path / "Tovar",
    # "cov_files_u": [(str(base_path / "U{:02d}+orig.HEAD")).format(n) for n in range(8)],
    # "cov_files_G": [(str(base_path / "G{:02d}+orig.HEAD")).format(n) for n in range(8)],
    "set_a": [(str(base_path / "R{:02d}+orig.HEAD")).format(n) for n in range(4)],
    "set_b": [(str(base_path / "R{:02d}+orig.HEAD")).format(n) for n in range(4, 8)],
    "ad6_olsq": ad6_olsq_wpath,
}


def test_3dttest_pp_AD6_gr_s5_paired(data):

    seta_l = [
        '%s "%s[Vrel#0_Coef]"' % (ad6_idlist[i], data.ad6_olsq[i])
        for i in range(len(ad6_idlist))
    ]
    setb_l = [
        '%s "%s[Arel#0_Coef]"' % (ad6_idlist[i], data.ad6_olsq[i])
        for i in range(len(ad6_idlist))
    ]
    seta = " ".join(seta_l)
    setb = " ".join(setb_l)
    cmd = """
         3dttest++
             -prefix {data.outdir}/stat.5.ttest
             -AminusB
             -setA Vrel {seta}
             -setB Arel {setb}
             -paired
         """
    # not using global vars here, just locals
    cmd = " ".join(cmd.format(**locals()).split())

    rc = tools.OutputDiffer(data, cmd, merge_error_with_output=True)
