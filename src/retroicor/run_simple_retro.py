#!/usr/bin/env python

# this program runs the newer/test command line interface for retroicor.

import sys
import lib_retro_opts as lro


# ================================ main =====================================

if __name__ == "__main__":

    # process the command line options, and get a dictionary of them
    # to use (or, if a quick-exit option was used, text might just be
    # displayed in the terminal, followed by a 0-exit).
    args_dict = lro.main_option_processing( sys.argv[1:] )

    print("++ DONE.  Goodbye.")
