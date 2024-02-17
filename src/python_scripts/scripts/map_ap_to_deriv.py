#!/usr/bin/env python

# This is a program to map AP results directory contents
# (specifically, some in the out.ss_review_uvars.json) to a separate
# derivatives directory.
#
#
#
# ===========================================================================

import sys
from   afnipy import lib_ap_derivs as lader

# ===========================================================================
# ===========================================================================

if __name__ == "__main__":

    # !!! TEMP
    x = lader.ap_deriv_obj(
        "/home/ptaylor/AFNI_data6/FT_analysis/FT.results",
        deriv_dir="banana",
        overwrite=True,
        verb=2,
    )

    x.map_all()

    y = lader.ap_deriv_obj(
        "/home/ptaylor/AFNI_data6/FT_analysis/FT.surf.results",
        deriv_dir="banana2",
        overwrite=True,
        verb=2,
    )

    y.map_all()
