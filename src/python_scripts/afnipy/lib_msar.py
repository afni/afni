
import sys, os, glob
from afnipy import lib_apqc_tcsh as lat
from afnipy import afni_base as ab


#ver = '1.2'; date = 'June 14, 2019'
# + [PT] revamped: have new features like title and reflink
#
#ver = '1.3'; date = 'June 17, 2019'
# + [PT] work on textblock and images therein
#
#ver = '1.4'; date = 'June 19, 2019'
# + [PT] no more REFLINK block, is just put in on command line
# + [PT] also introducing a special case of SECTION+TEXTBLOCK called
#        INTRO (bc the shebang is always at top of script)
#
#ver = '1.5'; date = 'June 19, 2019'
# + [PT] lots of tiny things updated/fixed/added.  Now in pretty good
#        working order
#
#ver = '1.6'; date = 'June 20, 2019'
# + [PT] Fix imcaption part of TEXTBLOCK->IMAGE, as well as help file disp
#
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
#ver = '1.9'; date = 'July 10, 2019'
# + [PT] can have text in the image tables now
#
#ver = '1.91'; date = 'July 18, 2019'
# + [PT] fix if '-prefix ..' dir is the present one
#
ver = '1.92'; date = 'Feb 24, 2020'
# + [PT] fix imcaption processing
#
##########################################################################

# Default "mode" (or environs) is CODE; this is a list of *other*
# special modes, each of which is either a single line, or has a pair
# of start/end keywords; we go through finding the start of each mode
# based on keyword (or by being default CODE), and then know what to
# look for for its ending mode.  SPACE is now also a mode, because it
# is useful to have as filler.
OTHER_MODES = [ 'TITLE',       # title of script/demo
                'SECTION',     # can breakup demo into parts
                'SUBSECTION',  # can breakup demo into subparts
                'TEXTBLOCK',   # basic description, just for RST
                'TEXTINTRO',   # special case of SECTION+TEXTBLOCK
                'SHEBANG',     # should always be first line of code
                'HCODE'        # script: normal code; RST: hidden+expandable
               ]

# --------------------------------------------------------------------------

def get_line_start_mode(x) :
    '''Input a string; if it is the start of a field, get the name of that
field; other, returns empty string'''

    ss = x.split()

    if not(ss):
        return 'SPACE'

    elif ss[0].__contains__("#:TITLE") :
        return 'TITLE'

    elif ss[0].__contains__("#:SECTION") :
        return 'SECTION'

    elif ss[0].__contains__("#:SUBSECTION") :
        return 'SUBSECTION'

    elif ss[0].__contains__('cat') and \
         x.__contains__('<<')  and \
         x.__contains__('TEXTBLOCK') :
        return 'TEXTBLOCK'
    
    elif ss[0].__contains__('cat') and \
         x.__contains__('<<')  and \
         x.__contains__('TEXTINTRO') :
        return 'TEXTINTRO'

    elif ss[0].__contains__("#!") :
        return 'SHEBANG'
    
    elif ss[0].__contains__('#:HIDE_ON') :
        return 'HCODE'

    else:
        return 'CODE'

# --------------------------------------------------------------------------

def find_mode_and_span(W, lstart, prev_mode=''):
    '''W is list of strings.

    lstart is the index of the line we start with.

    prev_mode is last/existing type of mode/block we were in (this
    affects how an empty space is interpreted, for example)

    '''

    X = W[lstart:]

    Nx    = len(X)
    lline = X[0]
    ss    = lline.split()

    start_mode = get_line_start_mode(lline)

    if start_mode == 'SPACE':
        tmode = start_mode
        tspan = 1

    elif (start_mode == 'CODE') or (start_mode == 'SHEBANG') :
        # empty lines not already in one of the OTHER_MODES should
        # just be code
        tmode = start_mode

        tspan = 0
        for jj in range(1, Nx):
            tt    = get_line_start_mode(X[jj])
            if OTHER_MODES.__contains__(tt) :
                tspan = jj
                break
        if not(tspan):
            # everything is code
            tspan = Nx

    elif start_mode == 'SECTION' or start_mode == 'SUBSECTION' :
        tmode = start_mode

        # we have to count empty spaces after this as part of the
        # mode, for accounting purposes
        tspan = 1
        for jj in range(1, Nx):
            tt = X[jj].split()
            if tt :
                tspan = jj
                break

    elif start_mode == 'TITLE' :
        tmode = start_mode

        # we have to count empty spaces after this as part of the
        # mode, for accounting purposes
        tspan = 1
        for jj in range(1, Nx):
            tt = X[jj].split()
            if tt :
                tspan = jj
                break

    elif (start_mode == 'TEXTBLOCK') or (start_mode == 'TEXTINTRO') :
        # these basically obey the same rules
        tmode = start_mode

        tspan = 0
        for jj in range(1, Nx):
            tt    = X[jj].split()
            if tt:
                # [PT: July 8, 2019] Have to allow for single quotes
                # to be around key word:  cat << 'KW'
                if tt[0] == tmode or tt[0] == "'" + tmode + "'" :
                    tspan = jj+1 # bc ends with keyword
                    break
        if not(tspan) :
            print('''** Parse error:  can't find end of "cat << {kw}"\n'''
                  '''   started at line {lnum}'''
                  '''   Need a "{kw}" somewhere.'''
                  ''.format(lnum=lstart, kw=tmode))
            sys.exit(2)

    elif start_mode == 'HCODE' :
        tmode = start_mode

        tspan = 0
        for jj in range(1, Nx):
            tt    = X[jj].split()
            if tt:
                if tt[0].__contains__("#:HIDE_OFF"):
                    tspan = jj+1 # +1 bc of keyword ending
                    break
        if not(tspan) :
            print('''** Parse error:  can't find end of "#:HIDE_ON"\n'''
                  '''   started at line {}.'''
                  '''   Need a "#:HIDE_OFF" somewhere.'''
                  ''.format(lstart))
            sys.exit(3)

    else:
        print(start_mode)

    return tmode, tspan

# -----------------------------------------------------------------------

def add_in_space( X, 
                  tstart,
                  tspan ):
    '''X is a list of strings.

    '''

    tscript = ''
    trst    = ''

    # Note the '-1', because there is a closing keyword
    Nx = tstart + tspan - 1 

    for ii in range(tstart+1, Nx):
        trst    += X[ii]
        tscript += X[ii] 

    return tscript, trst

# -----------------------------------------------------------------------

def add_in_textblock( X, 
                      tstart,
                      tspan,
                      iopts,
                      tmode = 'TEXTBLOCK' ):
    '''X is a list of strings.

    This is only for the RST file.

    tmode can also be TEXTINTRO, which is the same except for
    prepending a section heading for TEXTINTRO (and the output jumps
    to the top o' the queue in the RST page).

    '''

    tscript = ''
    trst    = ''
    
    Nx = tstart + tspan - 1

    ii = tstart+1
    while ii < Nx :

        x  = X[ii]
        ss = x.split()

        if not(ss):
            # empty case: just add line.
            trst += x
            ii   += 1
            
        elif ss[0].__contains__("#:IMAGE") :
            # IMAGE subblock must be contiguous-- empty line declares
            # end of it
            im_span = 1
            for jj in range(ii+1, Nx): # search for end (next empty line)
                y  = X[jj]
                uu = y.split()
                if not(uu) :
                    break
                else:
                    im_span+= 1
            imtxt = add_in_textblock_image( X,
                                            ii,
                                            im_span,
                                            iopts )
            #print(imtxt)
            trst+= imtxt
            ii+= im_span

        elif ss[0].__contains__("#:INCLUDE") :
            # INCLUDE subblock must be contiguous-- empty line declares
            # end of it
            im_span = 1
            for jj in range(ii+1, Nx): # search for end (next empty line)
                y  = X[jj]
                uu = y.split()
                if not(uu) :
                    break
                else:
                    im_span+= 1
            imtxt = add_in_textblock_include( X,
                                              ii,
                                              im_span,
                                              iopts )
            #print(imtxt)
            trst+= imtxt
            ii+= im_span
            
        else:
            # simple case, just add line
            trst += x
            ii   += 1

    return tscript, trst

# -----------------------------------------------------------------------

def add_in_textblock_include( X, 
                              tstart,
                              tspan,
                              iopts ):
    '''X is a list of strings.

    This is only for the RST file.  It can include any of the
    "include" directive phrases (start line, end line, etc.) from here:
    http://docutils.sourceforge.net/docs/ref/rst/directives.html#including-an-external-document-fragment

    The general format is:
    
    #:INCLUDE:  filename
        :lines:    1-10
        :language: none

    '''

    trst    = ''
    
    Nx = tstart + tspan

    fname     = ''
    oplist    = []

    # (opt) title is everything else include in this first line
    ss = X[tstart].split()
    if len(ss) == 2 :
        fname = ss[1]
    else:
        print('''** ERROR in TEXTBLOCK_INCLUDE: wrong number ({}) '''
              '''of entries'''.format(len(ss)))
        print('''   Main line must look like this, with just 1 filename:'''
              '''     #:INCLUDE:  filename''')
        sys.exit(5)

    for ii in range(tstart+1, Nx):
        ss = X[ii].strip()
        oplist.append( ss )

    # Now build table; some formatting necessary-- calc basename for
    # RST, bc name might include path
    inc_bname = os.path.basename( fname )
    trst += '''
.. literalinclude:: {}/{}
'''.format( iopts.subdir_rst, inc_bname )

    for op in oplist:
        trst+= 3*' ' + op + '\n'

    iopts.add_media( fname )

    return trst
    
# -----------------------------------------------------------------------

def add_in_textblock_image( X, 
                            tstart,
                            tspan,
                            iopts ):
    '''X is a list of strings.

    This is only for the RST file.

    The general format is:
    
    #:IMAGE:  Optional Title || and other col title!
        IMAGE1 IMAGE2
        IMAGE3 
    #:IMCAPTION: optional caption for any image(s)

    '''

    trst    = ''
    
    Nx = tstart + tspan

    imtitle   = []
    Nheader   = 0
    imcaption = []
    imlist    = []

    # (opt) title is everything else include in this first line
    ss = X[tstart].split()
    if len(ss) > 1 :
        tt =  ' '.join(ss[1:])
        imtitle = tt.split('||') # can have more than one title
        Nheader = 1
        Ntitle  = len(imtitle)

    # Now check rest for images, and/or captions; by construct, no
    # line can be empty.  We read everything as images until we get to
    # the IMCAPTION keyword, after which all is CAPTION
    lmode = 'IMAGES'
    for ii in range(tstart+1, Nx):
        ss = X[ii].split()
        if ss[0].__contains__("#:IMCAPTION") :
            lmode = 'CAPTION'
            if len(ss) > 1:
                imcaption.append(' '.join(ss[1:]))
        elif lmode == 'IMAGES' :
            # [PT: July 8, 2019] The structure in this 'elif' allows
            # for the combination of wildcard chars in image names and
            # the '-execute_script' functionality, which has created a
            # need for two main passes through interpreting the RST:
            # in the first main pass of the program, images might not
            # be found, but in the second pass, they might/should.
      
            minilist = parse_image_list( X[ii] ) 
            if minilist :
                imlist.append(minilist) 

        elif lmode == 'CAPTION' :
            # [PT: Feb 24, 2020] fixed a couple index/length things here
            if len(ss) > 0:
                imcaption.append((' '.join(ss[0:])))

    # Calc max dims of image matrix
    Ncol = 0
    Nrow = len( imlist )
    for rr in imlist:
        if len(rr) > Ncol:
            Ncol = len(rr)

    # [PT: July 8, 2019] If Ncol is 0, then that means no items were
    # found-- likely/hopefully that is a case of using
    # '-execute_script', and this being the first pass
    if not(Ncol) :
        #print("++ No images found on this pass.")
        return trst

    allwid = str(100 // Ncol) + ' '
    
    # Now build table;  some formatting necessary
    
    cap_text = ''
    if imcaption :
        cap_text = ' '.join(imcaption)

    trst += '''
.. list-table:: {}
   :header-rows: {}
   :widths: {}

'''.format(cap_text, Nheader, Ncol * allwid)

    if imtitle :
        for cc in range(Ncol) :
            if cc :
                symb = ' '
            else:
                symb = '*'

            if cc >= Ntitle :
                trst+= '   {} - {}\n'.format(symb, ' ')
            else:
                
                trst+= '   {} - {}\n'.format(symb, imtitle[cc].strip())
            
    for rr in range(Nrow):
        for cc in range(Ncol) :
            if cc :
                symb = ' '
            else:
                symb = '*'

            if cc >= len(imlist[rr]) :
                trst+= '   {} -\n'.format(symb)
            elif imlist[rr][cc] == 'NULL':
                trst+= '   {} -\n'.format(symb)
            else:
                if imlist[rr][cc][:2] == '[[' :
                    # if we have a string block, and ignore the [[ and
                    # ]] symbols wrapping the text
                    trst+= '''   {} - {}\n'''.format( symb, 
                                                      imlist[rr][cc][2:-2])
                else:
                    # Image attaching mode: make a list of these to
                    # copy later
                    iopts.add_media( imlist[rr][cc] )
                    # calc basename for RST, bc img name might include path
                    img_bname = os.path.basename( imlist[rr][cc] )
                    trst+= '''   {} - .. image:: {}/{}
          :width: 100%   
          :align: center\n'''.format( symb, iopts.subdir_rst,
                                      img_bname )
                 
                
    return trst

# -----------------------------------------------------------------------

def parse_image_list( uss ):
    '''This function exists because we now allow for text to be placed
    into the table, wrapped in [[ pairs o' square brackets ]].

    Input
    -----

    uss : an unsplit string

    Output
    ------
    
    minilist : list of string of files found, as well as any [[text]] blocks

    '''

    minilist = []

    N     = len (uss)
    ii    = 0
    smode = 'image'   # alternative is 'text'
    s0    = 0
    s1    = -1

    while ii < N-1 :

        if (uss[ii:ii+2] == '[[') :
            if smode == 'image' :
                smode = 'text'
                s1 = ii 

                # close out 'image' part of string, and get all images there
                ss = uss[s0:s1].split()
                #print(ss)
                for imstr in ss:
                    glimstr = glob.glob(imstr)
                    if glimstr :
                        minilist.extend(glimstr)

                # reset s0
                s0 = ii
                ii+= 1

            elif smode == 'text' :
                point = ' '*(ii) + '^'
                print("** ERROR: (char idx ={}) either nested [[s or forgotten "
                      "]] in line:\n\t{}\n\t{}".format(ii, uss, point))
                sys.exit(2)

        elif uss[ii:ii+2] == ']]' :
            if smode == 'text' :
                smode = 'image'  # reset for next
                s1 = ii+2
                
                # strip away leading whitespace, because spacing will
                # matter in attaching the string into the text.
                # removing the trailing whitespace, too, for no good
                # reason.
                clean_str = '[[' + uss[s0+2:s1-2].strip() + ']]'
                minilist.extend([clean_str])

                # reset s0
                s0 = ii+2
                ii+= 2
                
            elif smode == 'image' :
                point = ' '*(ii) + '^'
                print("** ERROR: (char idx={}) have a ]] without matched [[ "
                      "in line:\n\t{}\n\t{}".format(ii, uss, point))
                sys.exit(2)

        else:
            ii+=1

    # and one last check at the end
    if smode == 'image' :
        s1 = ii 

        # close out 'image' part of string, and get all images there
        ss = uss[s0:s1].split()
        #print(ss)
        for imstr in ss:
            glimstr = glob.glob(imstr)
            if glimstr :
                minilist.extend(glimstr)

    elif smode == 'text' :
        point = ' '*(ii) + '^'
        print("** ERROR: (char idx={}) have a ]] without matched [[ "
              "in line:\n\t{}\n\t{}".format(ii, uss, point))
        sys.exit(2)



    return minilist

# -----------------------------------------------------------------------

def copy_images_to_subdir(iopts):

    count_fail = 0
    list_fail = []
    
    for ii in range(iopts.nmedia):
        iname = iopts.list_media[ii]
        oname = iopts.subdir + '/' + os.path.basename(iopts.list_media[ii])
        a, b, c = ab.simple_shell_exec("\cp {} {}".format(iname, oname))
        if a :
            count_fail+= 1
            list_fail.append( iname )

    return count_fail, list_fail

# --------------------------------------------------------------------------

def add_in_code ( X, 
                  tstart,
                  tspan, 
                  tmode='CODE',
                  cmode='Tcsh',
                  add_shebang_rst='' ):
    '''X is a list of strings.

    At the moment, just these modes: cmode = {none|Tcsh}

    '''

    tscript = ''
    trst    = ''

    Nx = tstart + tspan
    N0 = tstart 

    if tmode == 'HCODE' :
        Nx-= 1 # because there is a closing keyword
        N0+= 1 # because there is an opening keyword

        trst+= '''

.. hidden-code-block:: {}
   :starthidden: True
   :label: - show code y/n -

'''.format(cmode)

    elif tmode == 'SHEBANG' :
        trst+= '' 

    else: # should just be code
        trst+= '''

.. code-block:: {}

'''.format(cmode)

    if add_shebang_rst :
        trst    += add_shebang_rst # should already be indented
                
    for ii in range(N0, Nx):

        # try to format ending \s nicely
        text = X[ii]
        if len(text) >= 2:
            if text[-2:] == '\\\n' :
                text = lineup_eol(text)
        
        tscript += text
        trst    += 3*' ' + text

    return tscript, trst

# -----------------------------------------------------------------------

def add_in_section( X, 
                    tstart,
                    tspan,
                    is_sub=False ):
    '''X should just be a list of a single string, but may generalize...

    Also now includes SUBSECTION; basically same as SECTION, but uses
    '-' instead of '=' for the underline.  No change in indentation.

    '''

    tscript = ''
    trst    = ''

    lline = X[tstart]
    ss    = lline.split()
    text  = ' '.join(ss[1:])
    ltext = len(text)
        
    trst   += text + "\n"
    if is_sub :
        trst   += (ltext  + 2 ) * '-' 
        tscript+= lat.bannerize( text, fullwid=72, padsymb='-' )
    else:
        trst   += (ltext  + 2 ) * '=' 
        tscript+= lat.bannerize( text, fullwid=72, padsymb='=' )

    # might have empty lines/padding
    for ii in range(1, tspan):
        jj = tstart + ii
        tscript += X[jj]
        trst    += X[jj]

    return tscript, trst 

# --------------------------------------------------------------------------

def create_text_title( X, 
                       tstart,
                       tspan,
                       MAKE_SECTION=False):
    '''X should just be a list of a single string, but may generalize...

    The MAKE_SECTION switch is for when we have multiple scripts: the
title still gets placed up above in the RST (and in the script), but
it will be formatted as a section instead of as a title.  

    '''

    tscript = ''
    trst    = ''

    lline = X[tstart]
    ss    = lline.split()
    text  = ' '.join(ss[1:])
    ltext = len(text)

    tscript += '# '
    tscript += text + '\n'

    if MAKE_SECTION :
        trst    += text + "\n"
        trst    += ltext * '=' 

    else:
        trst    += ltext * '*'  + "\n" 
        trst    += text + "\n"
        trst    += ltext * '*' 

    # might have empty lines/padding
    for ii in range(1, tspan):
        jj = tstart + ii
        tscript += X[jj]
        trst    += X[jj]

    return tscript, trst

# --------------------------------------------------------------------------

def interpret_MSAR_list( X, iopts, idx, DO_TOC=True ):

    N = len(X)

    USED_A_CODE_BLOCK = 0 # this lets us know where the shebang/top of
                          # orig code can be placed
    
    oscript_txt = ''
    orst_txt    = '\n\n'

    # Here and below, have to be careful with space

    if iopts.reflink_list[idx] :
        orst_txt+= '''.. _{}:'''.format(iopts.reflink_list[idx])
        orst_txt+= '''\n\n'''

    orst_txt+= '''TO_BE_THE_TITLE'''
    orst_txt+= '''\n\n'''

    if DO_TOC :
        orst_txt+= '''.. contents:: :local:'''
        orst_txt+= '''\n\n'''
        orst_txt+= '''Introduction\n============'''
        orst_txt+= '''\n\n'''

    # only put the tarball at the top by the TOC
    if DO_TOC and iopts.tarball_name :
        include_tarball = '**Download script tarball:** '
        include_tarball+= ':download:`{tball} <{spath}/{tball}>`'.format(
            tball=iopts.tarball_name, spath=iopts.subdir_rst)
        orst_txt+= include_tarball
        orst_txt+= '''\n\n'''

        all_script = ', '.join(iopts.oname_script_list)
        orst_txt+= '''Open the tarball using '''
        orst_txt+= '''``tar -xf {}`` '''.format(iopts.tarball_name)
        orst_txt+= '''to get the following scripts in your directory: '''
        orst_txt+= '''{}.'''.format(all_script)
        orst_txt+= '''\n\n'''

    # header for RST
    orst_txt+= '''**Download script:** :download:`{script} <{spath}/{script}>`

TO_BE_THE_INTRO

'''.format( script=iopts.oname_script_list[idx],
            spath=iopts.subdir_rst )

    # header for script
    oscript_txt+= '''TO_BE_THE_SHEBANG
'''
    
    ii      = 0
    tmode   = ''
    
    while ii < N :

        tmode, tspan = find_mode_and_span(X, ii, prev_mode=tmode)
        print("++ {:10s} range: [{:4d}, {:4d})".format(tmode,ii+1,ii+tspan+1))

        if tmode == 'SHEBANG' :
            # special case: calculate and save to prepend later to
            # the first usage of CODE or HIDDEN CODE laterz
            tscript_sheb, trst_sheb = add_in_code( X,
                                                   ii,
                                                   tspan,
                                                   tmode=tmode )
            oscript_txt = oscript_txt.replace( "TO_BE_THE_SHEBANG",
                                               tscript_sheb )
            ii         += tspan
        
        elif (tmode == 'CODE') or (tmode == 'HCODE') :

            # This silliness is because we may have to prepend the
            # shebang to a code block, and the hidden-code-block has
            # some prepending+indenting of its own.
            add_shebang_rst    = ''
            if not(USED_A_CODE_BLOCK) :
                add_shebang_rst    = trst_sheb
                USED_A_CODE_BLOCK  = 1
                
            tscript, trst = add_in_code( X,
                                         ii,
                                         tspan,
                                         tmode=tmode,
                                         add_shebang_rst=add_shebang_rst )
            oscript_txt+= tscript
            orst_txt   += trst 
            ii         += tspan

        elif tmode == 'SECTION' or tmode == 'SUBSECTION' :

            IS_SUB = False
            if tmode == 'SUBSECTION' :
                IS_SUB = True

            # SECTIONs and SUBSECTIONs are one line of text, but can
            # span several lines of whitespace
            tscript, trst = add_in_section( X,
                                            ii,
                                            tspan,
                                            is_sub=IS_SUB )
            oscript_txt+= tscript
            orst_txt   += trst 
            ii         += tspan

        elif tmode == 'TITLE' :
            # TITLEs are one line of text, but can span several
            # lines of whitespace
            # NOTE the special output here
            tscript, trst = create_text_title( X,
                                               ii,
                                               tspan,
                                               MAKE_SECTION=not(DO_TOC) )
            
            oscript_txt+= tscript
            orst_txt    = orst_txt.replace( "TO_BE_THE_TITLE", trst )
            ii         += tspan

        elif tmode == 'TEXTBLOCK' :
            tscript, trst = add_in_textblock( X,
                                              ii, 
                                              tspan,
                                              iopts )
            #oscript_txt+= tscript
            orst_txt   += trst 
            ii         += tspan

        elif tmode == 'TEXTINTRO' :
            tscript, trst = add_in_textblock( X,
                                              ii, 
                                              tspan,
                                              iopts,
                                              tmode = 'TEXTINTRO' )
            #oscript_txt+= tscript
            orst_txt    = orst_txt.replace( "TO_BE_THE_INTRO", trst )
            ii         += tspan
            
        elif tmode == 'SPACE' :
            tscript, trst = add_in_space( X,
                                          ii, 
                                          tspan )
            oscript_txt+= tscript
            orst_txt   += trst 
            ii         += tspan

        else:
            print("** ERROR. Unrecognized:")
            print(X[ii])
            sys.exit(2)

    # this goes here on the off chance that either Intro or Title is
    # not provided-- replace the place holder with emptiness.
    orst_txt    = orst_txt.replace( "TO_BE_THE_INTRO", '' )
    orst_txt    = orst_txt.replace( "TO_BE_THE_TITLE", '' )

    return oscript_txt, orst_txt

# ----------------------------------------------------------------------

def lineup_eol( x, llen = 72 ) :

    Nx = len(x)
    diff = llen - Nx

    if diff > 0 :
        y = x[:-2] + diff * ' ' + x[-2:]
    else:
        y = x

    return y

# --------------------------------------------------------------------------

def get_path_dirname( fff ):
    '''Take in potential file name.

    If not found, return ''.
    
    Otherwise, return relative path to there ('.' for local dir).
    '''

    fff_isfile = os.path.isfile( fff )

    if fff_isfile :
        fff_pathdir = os.path.dirname(fff)
        if not(fff_pathdir) :
            # even if found, current directory pathname is ''; change
            # to this so that we can attach '/' to it later
            fff_pathdir = '.' 
    else:  
        # complete, utter failure
        fff_pathdir = ''

    return fff_pathdir

# ----------------------------------------------------------------------

def read_text_to_list( fname ): 

    fff = open(fname, 'r')
    x = fff.readlines()
    fff.close()

    return x

# -------------------------------------------------------------------

def ARG_missing_arg(arg):
    print("** ERROR: missing argument after option flag: {}".format(arg))
    sys.exit(1)

# --------------------------------------------------------------------------

class all_info_MSAR:

    text_list        = []
    oscript_txt_list = []
    orst_txt_list    = []

    ntext            = 0

    def add_text(self, tt ):
        self.text_list.append(tt)
        self.ntext+= 1

    def add_oscript_txt(self, tt ):
        self.oscript_txt_list.append(tt)

    def add_orst_txt(self, tt ):
        self.orst_txt_list.append(tt)

    # needed because of re-parsing with '-execute script'
    def clear_oscript_txt(self):
        self.oscript_txt_list = []

    def clear_orst_txt(self):
        self.orst_txt_list = []

# --------------------------------------------------------------------------

# at the moment, obj.infile_path_list is created, but it won't be
# used-- we assume all scripts and all generated files are in the
# present working directory.  In the future, that might be relaxed
# since now we allow for multiple scripts in a single run.

class init_opts_MSAR:

    script_type   = 'tcsh' # can generalize to other types later
    
    # req input
    prefix_rst         = ""    # can/should include path (a/b/FILE.rst)
    prefix_rst_noext   = ""    # useful name, neither path nor ext
    infile_list        = []
    infile_path_list   = []    # keep track of where it came from, for exec_scr
    oname_script_list  = []    # should just be name of file (SCRIPT.tcsh)
    reflink_list       = []    # name of subdir in media/, and RST link name

    ninfile            = 0
    nscript            = 0
    nreflink           = 0

    # opt input
    do_execute    = 0     # opt to run created script (e.g., to gen imgs)
    
    # made by finish_defs()
    outdir        = ""    # where everything will go
    meddir        = ""    # inside outdir, where media/ dir is
    subdir        = ""    # inside meddir, where images/script/tarball will go
    subdir_rst    = ""    # local dir for RST path
    prefix_script_list = []  # list of full names for outputting scripts

    tarball_name  = ""        # for scripts, if >1
    tarball_name_path = ""    # for scripts, if >1, name with path

    # made later during parsing, potentially
    list_media    = []    # list of images to copy into media/reflink/.
    nmedia        = 0
    
    # ----------- req -----------------

    def set_prefix_rst(self, ppp):
        # file name must in '.rst'; useful also to have value without
        # ext, for other file names/paths
        if ppp[-4:] == '.rst' :
            self.prefix_rst       = ppp
            self.prefix_rst_noext = os.path.basename(ppp)[:-4]
        else:
            print("+* WARNING (minor): You didn't have '.rst' on the "
                  "'-prefix_rst ..' entry, so I am adding one for you.")
            self.prefix_rst       = ppp + '.rst'
            self.prefix_rst_noext = os.path.basename(ppp)

    def add_prefix_script(self, ppp):
        self.prefix_script_list.append( ppp )

    def add_infile(self, fff):
        path_fff = get_path_dirname( fff )
        self.infile_list.append( fff )
        self.infile_path_list.append( path_fff )
        self.ninfile+= 1

    def add_oname_script(self, oname):
        self.oname_script_list.append( oname )
        self.nscript+= 1

    def add_reflink(self, reflink):
        self.reflink_list.append( reflink )
        self.nreflink+= 1

    # ----------- opt -----------------
    
    def set_execute(self):
        self.do_execute = 1 

    # ------- complete some var defs/names --------

    def add_media(self, X):
        self.list_media.append( X )
        self.nmedia+= 1
        
    # needed because of re-running RST generation in case of
    # '-execute_script'
    def clear_media(self):
        self.list_media = []
        self.nmedia     = 0

    def finish_defs(self):

        # use this function here, because this file won't exist yet!
        pp = os.path.dirname(self.prefix_rst)
        if not(pp) :
            pp = '.' # the above returns '' for local dir-- not cool

        # these are all potentially full/relative paths
        self.outdir = pp
        self.meddir = '/'.join([self.outdir, 'media'])
        self.subdir = '/'.join([self.meddir, self.prefix_rst_noext])
        for i in range(self.nscript) :
            ps = '/'.join([self.subdir, self.oname_script_list[i]])
            self.add_prefix_script( ps )
        
        # just local/partial path within RST dir
        self.subdir_rst = '/'.join(['media', self.prefix_rst_noext])

        # tarball, if necessary; will also go into self.subdir_rst
        if self.ninfile > 1 :
            self.tarball_name = self.prefix_rst_noext + '.tgz'
            self.tarball_name_path = '{}/{}'.format( self.subdir, 
                                                     self.tarball_name )

    # ---------- check ----------------

    def check_req(self):
        MISS = 0
        if not(self.ninfile) :
            print("** ERROR: missing infile(s)")
            MISS+=1
            
        if self.prefix_rst == "" :
            print("** ERROR: missing prefix_rst")
            MISS+=1
            
        if not(self.nreflink) :
            print("** ERROR: missing reflink(s)")
            MISS+=1
        else:
            for i in range(self.nreflink):
                if self.reflink_list[i][0] == "_" :
                    print("+* You can't start the reflink name with "
                          "an underscore '_'.")
                    print("   {} does NOT abide!".format(self.reflink_list[i]))
                    MISS+=1

        if not(self.nscript) :
            print("** ERROR: missing prefix_script(s)")
            MISS+=1
        else:
            for i in range(self.nscript):
                if self.oname_script_list[i].__contains__("/") :
                    print("** ERROR: don't include path in the " 
                          "'-oname_script ..' entry.")
                    print("   {} does NOT abide!"
                          "".format(self.oname_script_list[i]))
                    MISS+=1

        if self.ninfile != self.nscript :
            print("** ERROR: number of '-input ..' values must match "
                  "the number of '-oname_script ..' values.")
            MISS+=1

        if self.ninfile == self.nreflink :
            pass
        elif self.nreflink == 1 :
            # in this case, pad upwards with blank ones, so indexing
            # works later
            for i in range(1, self.ninfile):
                self.add_reflink( '' )
        else: 
            print("** ERROR: must have either just one '-reflink ..' value,\n"
                  "   OR the number of '-reflink ..' values must match\n"
                  "   the number of '-input ..' values (here, {})."
                  "".format(self.ninfile) )
            MISS+=1

        return MISS


# --------------------------------------------------------------------------

help_string_MSAR = '''

PURPOSE ~1~ 

Program to take a script with some special (~simple) markup and turn
it into both an RST page and a script for the online Sphinx
documentation.


INPUTS ~1~ 

-prefix_rst AA    :(req) output filename, including any path, of the
                   RST/Sphinx file.  AA must include file extension 
                   '.rst'.  E.g.:  tutorial/fun_3dcalc.rst

-prefix_script BB :(req) output filename, *without* any path, of the
                   script file.  BB probably should include file extension, 
                   such as '.tcsh'.  E.g.:  fun_3dcalc.tcsh


-reflink       CC :(req) a string tag that will be 1) subdirectory name
                   holding images for the given demo, and 2) the RST 
                   internal reference label, as '.. _CC:'.  First character
                   of CC must be alphabetic.

-execute_script   :(req/opt) flag to not just create the RST+script, but
                   to execute the script as well.  IF the script
                   generates images that will be copied to the
                   media/CC/. directory, then this flag should be used
                   at least the first time the script is run (so the
                   files can be copied); it may not be necessary to
                   execute on later runs.
 

OUTPUTS ~1~ 

+ an RST file, which is basically a Sphinx-formatted page, that can be
  placed in a separate directory

+ an output directory to put into the Sphinx tree, called
  [rst-path]/media/CC, where [rst-path] is the location of the output
  RST file and CC is the reflink name.

+ a script file, both locally (where the script is run, so that it can
  be executed) and in [rst-path]/media/CC (which will be shown in the
  RST pages).

+ images made by the script which are flagged to be show in the RST
  pages will be copied to [rst-path]/media/CC/. 


EXAMPLES ~1~

   1) First time through, execute script to make images:
   adjunct_make_script_and_rst.py                                          \\
       -input          ex_afni11_roi_cmds.tcsh                             \\
       -reflink        afni11_roi_cmds                                     \\
       -prefix_script  afni11_roi_cmds.tcsh                                \\
       -prefix_rst ~/afni_doc/tutorials/rois_corr_vis/afni11_roi_cmds.rst  \\
       -execute_script

   2) Second time through, if "only" text changes/formatting:
   adjunct_make_script_and_rst.py                                          \\
       -input          ex_afni11_roi_cmds.tcsh                             \\
       -reflink        afni11_roi_cmds                                     \\
       -prefix_script  afni11_roi_cmds.tcsh                                \\
       -prefix_rst ~/afni_doc/tutorials/rois_corr_vis/afni11_roi_cmds.rst 

'''

def parse_MSAR_args(argv):

    Narg = len(argv)

    if not(Narg):
        print(help_string_MSAR)
        sys.exit(0)

    # initialize objs
    iopts  = init_opts_MSAR()    # input-specific

    # check through inputs
    i = 0
    while i < Narg:
        if argv[i] == "-ver":
            print(ver)
            sys.exit(0)

        elif argv[i] == "-help" or argv[i] == "-h":
            print(help_string_MSAR)
            sys.exit(0)

        # ---------- req ---------------

        # only single RST file output
        elif argv[i] == "-prefix_rst":
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_prefix_rst(argv[i])
            
        elif argv[i] == "-input":
            count = 0
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            while i < Narg:
                aaa = argv[i][0]
                if aaa != "-" :
                    iopts.add_infile(argv[i])
                    count+=1
                    i+=1
                else:
                    i-=1
                    break
            if not(count):
                ARG_missing_arg(argv[i])

        elif argv[i] == "-prefix_script":
            count = 0
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            while i < Narg:
                aaa = argv[i][0]
                if aaa != "-" :
                    iopts.add_oname_script(argv[i])
                    count+=1
                    i+=1
                else:
                    i-=1
                    break
            if not(count):
                ARG_missing_arg(argv[i])

        elif argv[i] == "-reflink":
            count = 0
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            while i < Narg:
                aaa = argv[i][0]
                if aaa != "-" :
                    iopts.add_reflink(argv[i])
                    count+=1
                    i+=1
                else:
                    i-=1
                    break
            if not(count):
                ARG_missing_arg(argv[i])

        # ---------- req ---------------

        elif argv[i] == "-execute_script":
            iopts.set_execute()

        # --------- finish -------------

        else:
            print("** ERROR: unknown opt: '{}'".format(argv[i]))
            sys.exit(2)
        i+=1

               
    if iopts.check_req():
        print("** ERROR with input arguments (see detailed whining above).")
        sys.exit(1)

    iopts.finish_defs()  # make some paths we need later

    return iopts

