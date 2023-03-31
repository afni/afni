#!/usr/bin/env python

# this program runs the newer/test command line interface for retroicor.

import sys
import lib_retro_opts    as lro
import lib_retro_reading as lrr
import lib_retro_proc as lrp
import lib_retro_out as RET
import lib_retro_outObj as RETO


# ================================ main =====================================

if __name__ == "__main__":

    # Process the command line options. Leads to one of:
    # + a quick but OK exit (disp help, ver, opt list, etc.)
    # + getting a dict of checked opts for the program
    # + error exit :(
    args_dict = lro.main_option_processing( sys.argv )

    test_retro_obj = lrr.retro_obj(args_dict)
    
    physiologicalNoiseComponents = lrp.getPhysiologicalNoiseComponents(test_retro_obj)
    if len(physiologicalNoiseComponents) == 0:
        print('*** Error in retro_ts.  Failure to get physiological ' + \
              'noise components')
        sys.exit()

    if RETO.outputInNimlFormat(physiologicalNoiseComponents, test_retro_obj):
        print('** ERROR outputting SliBase file')
        sys.exit()

    # if RET.outputInNimlFormat(physiologicalNoiseComponents, test_retro_obj):
    #     print('** ERROR outputting SliBase file')
    #     sys.exit()
    
    # if len(physiologicalNoiseComponents['resp_phases']) > 0 and\
    #     (test_retro_obj.save_graph_level > 1 or 
    #      test_retro_obj.show_graph_level > 1):
    #     status = RET.show_rvt_peak(physiologicalNoiseComponents, test_retro_obj)
    #     if status == 1:
    #         print('*** Error in retro_ts')
    #         print('Failure to show RVT peak')
    #         sys.exit()

    print("++ DONE.  Goodbye.")
