#!/usr/bin/env python

#ver = '1.7'; date = 'July 8, 2019'
# + [PT] better generalization of -execute_script
# + [PT] add in SUBSECTION
# + [PT] allow wildcard chars in IMAGE names
# + [PT] fix help output disp
#
#ver = '1.8'; date = 'July 9, 2019'
# + [PT] now can have multiple MARK files input, and multiple script/reflinks
#        ... but still creating a single RST file
# + [PT] tarball also created, if >1 script
#
ver = '1.9'; date = 'July 10, 2019'
# + [PT] can have text in the image tables now
#
##########################################################################

import sys 
import os 

import afni_base as ab
import lib_msar as lmsar

# =============================================================================

if __name__ == "__main__" : 

    iopts     = lmsar.parse_MSAR_args(sys.argv[1:])

    ainfo = lmsar.all_info_MSAR()

    # might have several scripts to parse
    for ii in range (iopts.ninfile) :
        ainfo.add_text( lmsar.read_text_to_list( iopts.infile_list[ii] ) )

        # only include TOC for the first file
        oscript_txt, orst_txt = \
            lmsar.interpret_MSAR_list( ainfo.text_list[ii], iopts, ii,
                                       DO_TOC=not(ii) )
        ainfo.add_oscript_txt( oscript_txt )
        ainfo.add_orst_txt( orst_txt )

    # ------------------ write output files ------------------

    # Make dir to hold script/any images
    a, b, c = ab.simple_shell_exec("\mkdir -p {}".format(iopts.subdir))
    if not(a) :
        print("++ Wrote directory to hold scripts")
    else:
        print("** Badness writing dir to hold scripts {}".format(iopts.subdir))
        sys.exit(12)

    # write script to local dir
    for ii in range (iopts.nscript) :
        fff = open(iopts.oname_script_list[ii], 'w')
        fff.write(ainfo.oscript_txt_list[ii])
        fff.close()

    if iopts.tarball_name :
        all_script = ' '.join(iopts.oname_script_list)
        # \\ needed before the t, because \t is a TAB!
        a,b,c = ab.simple_shell_exec("\\tar -zcf {} {} "
                                     "".format(iopts.tarball_name_path, 
                                               all_script))
        if not(a) :
            print( "++ Wrote scripty tarball: {}"
                   "".format(iopts.tarball_name_path) )
        else:
            print( "** Badness making script tarball: {}"
                   "".format(iopts.tarball_name_path) )
            sys.exit(12)

    # execute script, if desired
    if iopts.do_execute :

        # do this clearance because globbing occurs in making of RSTs,
        # and results of that might (likely) depend on the results of
        # executing the script
        ainfo.clear_orst_txt()
        iopts.clear_media()

        for ii in range (iopts.nscript) :
            a, b, c = ab.simple_shell_exec( "tcsh -ef {}"
                                    "".format(iopts.oname_script_list[ii]) )
            if not(a) :
                print( "++ Success running script {}"
                       "".format(iopts.oname_script_list[ii]) )

                # [PT: July 18, 2019] Have to redo this part, done
                # above, so that the RST gets the real file names of
                # things to copy, if globbing was involved; media list
                # to copy is also regenerated here
                print("++ Regenerating RST, in case new files were made.")
                oscript_txt, orst_txt = \
                    lmsar.interpret_MSAR_list( ainfo.text_list[ii], iopts, ii,
                                               DO_TOC=not(ii) )
                ainfo.add_orst_txt( orst_txt ) # only need new RSTs

            else:
                print( "** Badness running script {}"
                       "".format(iopts.oname_script_list[ii]) )
                sys.exit(8)

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

    for ii in range (iopts.nscript) :
        # write script to final dir
        print( "++ Writing output script: {}"
               "".format(iopts.prefix_script_list[ii]) )
        fff = open(iopts.prefix_script_list[ii], 'w')
        fff.write(ainfo.oscript_txt_list[ii])
        fff.close()

    # write RST to final dir: simply the concatenation of the
    # individual MARK files.
    fff = open(iopts.prefix_rst, 'w')
    for ii in range (ainfo.ntext) :
        fff.write(ainfo.orst_txt_list[ii])
        fff.write("\n\n")
    fff.close()

    # deal with python 2/3 [rickr]
    try:    code = eval('0o755')
    except: code = eval('0755')
    for ii in range (iopts.nscript) :
        try:
            os.chmod(iopts.prefix_script_list[ii], code)
            os.chmod(iopts.oname_script_list[ii], code)
        except:
            omsg = "failed: chmod {} {}".format( code, 
                                                 iopts.prefix_script_list[ii] )
            omsg = "failed: chmod {} {}".format( code, 
                                                 iopts.oname_script_list[ii] )
            print(omsg)


    # ------------ finish --------------

    bye_msg = '''
    ++ Done making (executable) script to generate HTML QC:
       RST     : {}'''.format(iopts.prefix_rst)
    for ii in range (iopts.nscript) :
        bye_msg+= '''
       SCRIPT  : {}'''.format(iopts.prefix_script_list[ii])
    if iopts.tarball_name :
        bye_msg+= '''
       TARBALL : {}'''.format(iopts.tarball_name_path)
    bye_msg+= '''\n'''
    print( bye_msg )

    sys.exit(0)



