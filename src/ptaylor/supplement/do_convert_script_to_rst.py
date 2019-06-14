#!/usr/bin/env python

import sys
import os

import lib_DCSTR as ldcstr

# =============================================================================

if __name__ == "__main__" : 

    iopts     = ldcstr.parse_DCSTR_args(sys.argv[1:])

    text_list = ldcstr.read_text_to_list( iopts.infile )

    oscript_txt, orst_txt, oscript, orst = \
                ldcstr.interpret_DCSTR_list( text_list, iopts.prefix )

    # write output files

    fff = open(oscript, 'w')
    fff.write(oscript_txt)
    fff.close()

    fff = open(orst, 'w')
    fff.write(orst_txt)
    fff.close()

    # deal with python 2/3 [rickr]
    try:    code = eval('0o755')
    except: code = eval('0755')
    try:
        os.chmod(oscript, code)
    except:
        omsg = "failed: chmod {} {}".format(code, oscript)
        print(omsg)

    # finish

    bye_msg = '''
    ++ Done making (executable) script to generate HTML QC:
    {}
    '''.format('SOME_FILE')

    print( bye_msg )

    sys.exit(0)



