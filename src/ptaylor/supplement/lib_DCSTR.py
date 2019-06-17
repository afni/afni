
import sys
import lib_apqc_tcsh as lat


#ver = '1.2'; date = 'Jne 14, 2019'
# [PT] revamped: have new features like title and reflink
#
ver = '1.3'; date = 'Jne 17, 2019'
# [PT] work on textblock and images therein
#
##########################################################################

# Default "mode" (or environs) is CODE; this is a list of *other*
# special modes, each of which is either a single line, or has a pair
# of start/end keywords; we go through finding the start of each mode
# based on keyword (or by being default CODE), and then know what to
# look for for its ending mode.  SPACE is now also a mode, because it
# is useful to have as filler.
OTHER_MODES = [ 'TITLE',       # title of script/demo
                'REFLINK',     # abbrev for RST hyperlink reference
                'SECTION',     # can breakup demo into parts
                'TEXTBLOCK',   # basic description, just for RST
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

    elif ss[0].__contains__("#:REFLINK") :
        return 'REFLINK'

    elif ss[0].__contains__("#:SECTION") :
        return 'SECTION'

    elif ss[0].__contains__('cat') and \
         x.__contains__('<<')  and \
         x.__contains__('TEXTBLOCK') :
        return 'TEXTBLOCK'

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

    elif start_mode == 'CODE' :
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

    elif start_mode == 'SECTION' :
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

    elif start_mode == 'REFLINK' :
        tmode = start_mode

        # we have to count empty spaces after this as part of the
        # mode, for accounting purposes
        tspan = 1
        for jj in range(1, Nx):
            tt = X[jj].split()
            if tt :
                tspan = jj
                break

    elif start_mode == 'TEXTBLOCK' :
        tmode = start_mode

        tspan = 0
        for jj in range(1, Nx):
            tt    = X[jj].split()
            if tt:
                if tt[0] == 'TEXTBLOCK':
                    tspan = jj+1 # bc ends with keyword
                    break
        if not(tspan) :
            print('''** Parse error:  can't find end of "cat <<TEXTBLOCK"\n'''
                  '''   started at line {}'''
                  '''   Need a "TEXTBLOCK" somewhere.'''
                  ''.format(lstart))
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
                      subdir = '' ):
    '''X is a list of strings.

    This is only for the RST file.

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
                                            subdir=subdir )
            print(imtxt)
            trst+= imtxt
            ii+= im_span
                                    
        else:
            # simple case, just add line
            trst += x
            ii   += 1

    return tscript, trst

# -----------------------------------------------------------------------

def add_in_textblock_image( X, 
                            tstart,
                            tspan,
                            subdir = ''):
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
    impath    = 'media'
    
    if subdir :
        impath+= '/' + subdir

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
            imcaption.append(ss[1:])
        elif lmode == 'IMAGES' :
            imlist.append(ss)
        elif lmode == 'CAPTION' :
            imcaption.append(ss[1:])

    # Calc max dims of image matrix
    Ncol = 0
    Nrow = len( imlist )
    for rr in imlist:
        if len(rr) > Ncol:
            Ncol = len(rr)
    allwid = str(100 // Ncol) + ' '
    
    # Now build table;  some formatting necessary
    
    trst += '''
.. list-table:: 
   :header-rows: {}
   :widths: {}

'''.format(Nheader, Ncol * allwid)

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
                trst+= '''   {} - .. image:: {}/{}
          :width: 100%   
          :align: center\n'''.format(symb, impath, imlist[rr][cc])
                
    return trst

# -----------------------------------------------------------------------

def add_in_code ( X, 
                  tstart,
                  tspan, 
                  hide=False,
                  cmode='Tcsh' ):
    '''X is a list of strings.

    At the moment, just these modes: cmode = {none|Tcsh}

    '''

    tscript = ''
    trst    = ''

    Nx = tstart + tspan
    N0 = tstart 

    if hide:
        Nx-= 1 # because there is a closing keyword
        N0+= 1 # because there is an opening keyword

        trst+= '''

.. hidden-code-block:: {}
   :starthidden: False
   :label: - show code y/n -

'''.format(cmode)

    else:
        trst+= '''

.. code-block:: {}

'''.format(cmode)

    for ii in range(N0, Nx):

        # try to format ending \s nicely
        text = X[ii]
        if len(text) >= 2:
            if text[-2:] == '\\\n' :
                text = lineup_eol(text)
        
        trst    += 3*' ' + text
        tscript += text

    return tscript, trst

# -----------------------------------------------------------------------

def add_in_section( X, 
                    tstart,
                    tspan ):
    '''X should just be a list of a single string, but may generalize...'''

    tscript = ''
    trst    = ''

    lline = X[tstart]
    ss    = lline.split()
    text  = ' '.join(ss[1:])
    ltext = len(text)
    
    tscript+= lat.bannerize( text )
    
    trst   += text + "\n"
    trst   += (ltext  + 2 ) * '-' 

    # might have empty lines/padding
    for ii in range(1, tspan):
        jj = tstart + ii
        tscript += X[jj]
        trst    += X[jj]

    return tscript, trst 

# --------------------------------------------------------------------------

def create_text_title( X, 
                       tstart,
                       tspan ):
    '''X should just be a list of a single string, but may generalize...'''

    tscript = ''
    trst    = ''

    lline = X[tstart]
    ss    = lline.split()
    text  = ' '.join(ss[1:])
    ltext = len(text)

    tscript += '# TUTORIAL: '
    tscript += text + '\n'
    
    trst    += (ltext  + 2 ) * '*'  + "\n" 
    trst    += text + "\n"
    trst    += (ltext  + 2 ) * '*' 

    # might have empty lines/padding
    for ii in range(1, tspan):
        jj = tstart + ii
        tscript += X[jj]
        trst    += X[jj]

    return tscript, trst

# --------------------------------------------------------------------------

def create_text_reflink( X, 
                         tstart,
                         tspan ):
    '''X should just be a list of a single string, but may generalize...'''

    tscript = ''
    trst    = ''
    reflink = ''

    lline = X[tstart]
    ss    = lline.split()
    text  = '_'.join(ss[1:])
    if text[0] != '_' :
        text = '_' + text

    reflink = text[1:]   # use this later as a subdir for any images
        
    trst+= '.. ' + text + ':\n'

    # might have empty lines/padding
    for ii in range(1, tspan):
        jj = tstart + ii
        tscript += X[jj]
        trst    += X[jj]

    return tscript, trst, reflink

# --------------------------------------------------------------------------

def interpret_DCSTR_list( X, prefix ):

    N = len(X)

    oscript_txt = ''
    orst_txt    = ''

    # header for RST
    orst_txt+= '''TO_BE_THE_REFLINK

TO_BE_THE_TITLE

.. contents:: :local:

|

'''

    oscript = prefix + '.tcsh'
    orst    = prefix + '.rst'

    ii      = 0
    tmode   = ''
    reflink = '' # also used as subdir of images

    while ii < N :

        tmode, tspan = find_mode_and_span(X, ii, prev_mode=tmode)
        print("++ {:10s} range: [{:4d}, {:4d})".format(tmode,ii+1,ii+tspan+1))

        if tmode == 'CODE' :
            tscript, trst = add_in_code( X,
                                         ii,
                                         tspan,
                                         hide=False )
            oscript_txt+= tscript
            orst_txt   += trst 
            ii         += tspan

        elif tmode == 'HCODE' :
            tscript, trst = add_in_code( X,
                                         ii, 
                                         tspan,
                                         hide=True )
            oscript_txt+= tscript
            orst_txt   += trst 
            ii         += tspan

        elif tmode == 'SECTION' :
            # SECTIONs are one line of text, but can span several
            # lines of whitespace
            tscript, trst = add_in_section( X,
                                            ii,
                                            tspan )
            oscript_txt+= tscript
            orst_txt   += trst 
            ii         += tspan

        elif tmode == 'TITLE' :
            # TITLEs are one line of text, but can span several
            # lines of whitespace
            # NOTE the special output here
            tscript, trst = create_text_title( X,
                                               ii,
                                               tspan )
            
            oscript_txt+= tscript
            orst_txt    = orst_txt.replace( "TO_BE_THE_TITLE", trst )
            ii         += tspan

        elif tmode == 'REFLINK' :
            # TITLEs are one line of text, but can span several
            # lines of whitespace
            # NOTE the special output here
            tscript, trst, reflink = create_text_reflink( X,
                                                          ii,
                                                          tspan )
            oscript_txt+= tscript
            orst_txt    = orst_txt.replace( "TO_BE_THE_REFLINK", trst )
            ii         += tspan

        elif tmode == 'TEXTBLOCK' :
            tscript, trst = add_in_textblock( X,
                                              ii, 
                                              tspan,
                                              subdir = reflink)
            oscript_txt+= tscript
            orst_txt   += trst 
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

    return oscript_txt, orst_txt, oscript, orst

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

class init_opts_DCSTR:

    # req input
    infile   = ""
    prefix   = ""

    # ----------- req -----------------

    def set_infile(self, fff):
        self.infile = fff

    def set_prefix(self, prefix):
        self.prefix = prefix

    # ---------- check ----------------

    def check_req(self):
        MISS = 0
        if self.infile == []:
            print("missing: infiles")
            MISS+=1
        if self.prefix == "":
            print("missing: prefix")
            MISS+=1
        return MISS





# --------------------------------------------------------------------------

help_string_DCSTR = '''
Get help!
'''

def parse_DCSTR_args(argv):

    Narg = len(argv)

    if not(Narg):
        print(help_string_DCSTR)
        sys.exit(0)

    # initialize objs
    iopts  = init_opts_DCSTR()    # input-specific

    # check through inputs
    i = 0
    while i < Narg:
        if argv[i] == "-ver":
            print(ver)
            sys.exit(0)

        elif argv[i] == "-help" or argv[i] == "-h":
            print(help_string_apqc_1dplot)
            sys.exit(0)

        # ---------- req ---------------

        elif argv[i] == "-input":
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_infile(argv[i])
        
        elif argv[i] == "-prefix":
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_prefix(argv[i])
        
        # --------- finish -------------

        else:
            print("** ERROR: unknown opt: '{}'".format(argv[i]))
            sys.exit(2)
        i+=1

    if iopts.check_req():
        print("** ERROR: Missing input arguments")
        sys.exit(1)
        
    return iopts

