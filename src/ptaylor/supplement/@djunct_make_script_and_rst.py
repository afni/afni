#!/usr/bin/env python

import sys 
import os 

import afni_base as ab
import lib_msar as lmsar

# =============================================================================

if __name__ == "__main__" : 

    iopts     = lmsar.parse_MSAR_args(sys.argv[1:])

    text_list = lmsar.read_text_to_list( iopts.infile )

    oscript_txt, orst_txt = \
                lmsar.interpret_MSAR_list( text_list, iopts )
    
    # --------- write output files ---------

    # Make dir to hold script/any images
    a, b, c = ab.simple_shell_exec("\mkdir -p {}".format(iopts.subdir))
    if not(a) :
        print("++ Wrote directory to hold scripts")
    else:
        print("** Badness writing dir to hold scripts {}".format(iopts.subdir))

    # write script to local dir
    fff = open(iopts.oname_script, 'w')
    fff.write(oscript_txt)
    fff.close()

    # execute script, if desired
    if iopts.do_execute :
        a, b, c = ab.simple_shell_exec("tcsh -ef {}".format(iopts.oname_script))
        if not(a) :
            print("++ Success running script {}".format(iopts.oname_script))
        else:
            print("** Badness running script {}".format(iopts.oname_script))

    # copy images over, if any exist
    if iopts.nmedia :
        nfail, list_fail = lmsar.copy_images_to_subdir(iopts)
        if nfail :
            print("*+ These {} images failed to copy:".format(nfail))
            print("   {}".format(list_fail))
        else:
            print("*+ These {} images copied OK:".format(iopts.nmedia))
            for x in iopts.list_media :
                print("   {}".format(x))

    # write script to final dir
    fff = open(iopts.prefix_script, 'w')
    fff.write(oscript_txt)
    fff.close()

    # write RST to final dir
    fff = open(iopts.prefix_rst, 'w')
    fff.write(orst_txt)
    fff.close()

    # deal with python 2/3 [rickr]
    try:    code = eval('0o755')
    except: code = eval('0755')
    try:
        os.chmod(iopts.prefix_script, code)
        os.chmod(iopts.oname_script, code)
    except:
        omsg = "failed: chmod {} {}".format(code, iopts.prefix_script)
        omsg = "failed: chmod {} {}".format(code, iopts.oname_script)
        print(omsg)


    # ------------ finish --------------

    bye_msg = '''
    ++ Done making (executable) script to generate HTML QC:
    {}
    {}
    '''.format(iopts.prefix_script, iopts.prefix_rst)

    print( bye_msg )

    sys.exit(0)



