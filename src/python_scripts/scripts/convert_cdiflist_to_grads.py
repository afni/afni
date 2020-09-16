#!/usr/bin/env python

# python3 status: compatible

# Take in a GE cdiflist and write out a gradient file+ set of bvals

# -----------------------------------------------------------------------
#ver='0.0' ; date='June 8, 2020'
# + [PT] birth
#
ver = '0.1' ; date = 'June 10, 2020'
# [PT] debug/test
#
##########################################################################

import sys, os, copy

from afnipy import afni_base          as ab
from afnipy import afni_util          as UTIL
from afnipy import lib_cdiflist       as LCDI


# =============================================================================

if __name__ == "__main__" : 

    iopts = LCDI.parse_args(sys.argv)

    ab.IP("ver : {} (libver : {})"
          "".format( ver, LCDI.ver ))


    # read in inputs
    ngrad, cdiflist_list = LCDI.read_in_cdiflist(iopts.cdiflist)

    # calc bvals and (unscaled) bvecs
    list_bval, list_bvec \
        = LCDI.cdiflist_vals_to_grad_bval( cdiflist_list, 
                                           bval=iopts.bval_max )

    # write outputs
    LCDI.write_files_from_bval_bvec( list_bval, 
                                     list_bvec, 
                                     iopts.prefix )

    sys.exit(0)
