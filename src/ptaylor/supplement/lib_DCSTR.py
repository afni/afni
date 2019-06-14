
import sys
import lib_apqc_tcsh as lat


# default environment is CODE; this is a list of special
# environments/lines, the start of which end a CODE line; we should
# only need to find start of environ.  SPACE is also one now, because
# it can follow sections of well-defined length
OTHER_MODES = [ 'SECTION', 
                'DESCRIPT',
                'HID_CODE' ]

# --------------------------------------------------------------------------

def get_line_start_mode(x) :
    '''Input a string; if it is the start of a field, get the name of that
field; other, returns empty string'''

    ss = x.split()

    if not(ss):
        return 'SPACE'

    elif ss[0].__contains__("#:SECTION") :
        return 'SECTION'

    elif ss[0].__contains__('cat') and \
         x.__contains__('<<')  and \
         x.__contains__('EOF') :
        return 'DESCRIPT'

    elif ss[0].__contains__('#:HIDE_ON') :
        return 'HID_CODE'

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

    elif start_mode == 'DESCRIPT' :
        tmode = start_mode

        tspan = 0
        for jj in range(1, Nx):
            tt    = X[jj].split()
            if tt:
                if tt[0] == 'EOF':
                    tspan = jj+1 # bc ends with keyword
                    break
        if not(tspan) :
            print('''** Parsing error:  can't find end of "cat <<EOF"\n'''
                  '''   started at line {}'''
                  '''   Need a "EOF" somewhere.'''
                  ''.format(lstart))
            sys.exit(2)

    elif start_mode == 'HID_CODE' :
        tmode = start_mode

        tspan = 0
        for jj in range(1, Nx):
            tt    = X[jj].split()
            if tt:
                if tt[0].__contains__("#:HIDE_OFF"):
                    tspan = jj+1 # +1 bc of keyword ending
                    break
        if not(tspan) :
            print('''** Parsing error:  can't find end of "#:HIDE_ON"\n'''
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
                  tspan, 
                  oscript_txt, 
                  orst_txt ):
    '''X is a list of strings.

    '''

    # Note the '-1', because there is a closing keyword
    Nx = tstart + tspan - 1 

    for ii in range(tstart+1, Nx):
        orst_txt    += X[ii]
        oscript_txt += X[ii] 

    return oscript_txt, orst_txt

# -----------------------------------------------------------------------

def add_in_descript( X, 
                     tstart,
                     tspan, 
                     oscript_txt, 
                     orst_txt ):
    '''X is a list of strings.

    This is only for RST, and we don't include the end stuff.

    '''

    Nx = tstart + tspan - 1
    for ii in range(tstart+1, Nx):
        orst_txt    += X[ii]
    return oscript_txt, orst_txt

# -----------------------------------------------------------------------

def add_in_code ( X, 
                  tstart,
                  tspan, 
                  oscript_txt, 
                  orst_txt,
                  hide=False,
                  cmode='Tcsh' ):
    '''X is a list of strings.

    At the moment, just these modes: cmode = {none|Tcsh}

    '''

    Nx = tstart + tspan
    N0 = tstart 

    if hide:
        Nx-= 1 # because there is a closing keyword
        N0+= 1 # because there is an opening keyword

        orst_txt+= '''

.. hidden-code-block:: {}
   :starthidden: False
   :label: - show code y/n -

'''.format(cmode)

    else:
        orst_txt+= '''

.. code-block:: {}

'''.format(cmode)

    for ii in range(N0, Nx):
        orst_txt    += 3*' ' + X[ii]
        oscript_txt += X[ii]

    return oscript_txt, orst_txt

# -----------------------------------------------------------------------

def add_in_section( X, 
                    tstart,
                    tspan, 
                    oscript_txt, 
                    orst_txt ):
    '''X should just be a list of a single string, but may generalize...'''

    lline = X[tstart]
    ss = lline.split()
    
    text  = ' '.join(ss[1:])
    ltext = len(text)
    
    oscript_txt += lat.bannerize( text )
    
    orst_txt    += text + "\n"
    orst_txt    += (ltext  + 2 ) * '*' 

    # might have empty lines/padding
    for ii in range(1, tspan):
        jj = tstart + ii
        oscript_txt += X[jj]
        orst_txt    += X[jj]

    return oscript_txt, orst_txt

# --------------------------------------------------------------------------

def interpret_DCSTR_list( X, prefix ):

    N = len(X)

    oscript_txt = ''
    orst_txt    = ''

    oscript     = prefix + '.tcsh'
    orst        = prefix + '.rst'

    ii = 0
    tmode = ''

    while ii < N :

        tmode, tspan = find_mode_and_span(X, ii, prev_mode=tmode)
        print("++ {:10s} range: [{:4d}, {:4d})".format(tmode,ii+1,ii+tspan+1))

        if tmode == 'CODE' :
            oscript_txt, orst_txt = add_in_code( X,
                                                 ii,
                                                 tspan, 
                                                 oscript_txt, 
                                                 orst_txt,
                                                 hide=False )
            ii+= tspan

        elif tmode == 'HID_CODE' :
            oscript_txt, orst_txt = add_in_code( X,
                                                 ii, 
                                                 tspan, 
                                                 oscript_txt, 
                                                 orst_txt,
                                                 hide=True )
            ii+= tspan

        elif tmode == 'SECTION' :
            # SECTIONs are one line of text, but can span several
            # lines of whitespace
            oscript_txt, orst_txt = add_in_section( X,
                                                    ii,
                                                    tspan,
                                                    oscript_txt, 
                                                    orst_txt )
            ii+= tspan


        elif tmode == 'DESCRIPT' :
            oscript_txt, orst_txt = add_in_descript( X,
                                                     ii, 
                                                     tspan, 
                                                     oscript_txt, 
                                                     orst_txt)
            ii+= tspan

        elif tmode == 'SPACE' :
            oscript_txt, orst_txt = add_in_space( X,
                                                  ii, 
                                                  tspan, 
                                                  oscript_txt, 
                                                  orst_txt)
            ii+= tspan

        else:
            print("** UNRECOGNIZED")
            print(X[ii])

#        else:
#
#            text = lline
#            if len(text) >= 2:
#                if text[-2:] == '\\\n' :
#                    text = lineup_eol(lline)
#
#            oscript_txt += text
#
#
#            ii+= 1


    return oscript_txt, orst_txt, oscript, orst

# ----------------------------------------------------------------------

def lineup_eol( x, llen = 70 ) :

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

