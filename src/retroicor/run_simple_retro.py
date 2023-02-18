#!/usr/bin/env python

# this program runs the newer/test command line interface for retroicor.

import sys
import lib_retro_opts as lro


# ================================ main =====================================

if __name__ == "__main__":

    # Process the command line options. Leads to one of:
    # + a quick but OK exit (disp help, ver, opt list, etc.)
    # + getting a dict of checked opts for the program
    # + error exit :(
    args_dict = lro.main_option_processing( sys.argv )

    print("++ DONE.  Goodbye.")
