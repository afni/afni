#!/usr/bin/env python

# this program runs the newer/test command line interface for retroicor.

import sys
import lib_retro_opts    as lro
import lib_retro_reading as lrr


# ================================ main =====================================

if __name__ == "__main__":

    # Process the command line options. Leads to one of:
    # + a quick but OK exit (disp help, ver, opt list, etc.)
    # + getting a dict of checked opts for the program
    # + error exit :(
    args_dict = lro.main_option_processing( sys.argv )

    test_retro_obj = lrr.retro_obj(args_dict)

    print("++ DONE.  Goodbye.")
