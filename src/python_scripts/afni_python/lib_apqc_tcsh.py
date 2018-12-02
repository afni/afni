#
auth = 'PA Taylor'
# ver : 1.32 || date: Oct 15, 2018
# + some level of written-ness
#
# ver : 1.33 || date: Oct 16, 2018
# + new uvars
# + new checks
# + new QC dir and subdir defs
#
# ver : 1.34 || date: Oct 17, 2018
# + new uvars
# + new text, new output strings
# + WARN type introduced
#
#ver = '1.4' ; date = 'Oct 23, 2018'
# + do stats differently: separate olay and thr
# + also start a json for the pbar info
#
#ver = '1.5' ; date = 'Nov 1, 2018'
# + update regression warning 
#
#ver = '1.51' ; date = 'Nov 19, 2018'
# + update 1dplot string labels (add in "enorm" and "outliers")
#
ver = '1.6' ; date = 'Dec 2, 2018'
# + check_dep() now has more conditions, because of some specific
#   vars+values that may or may not be there
#   - first one: for stats_dsets, check about "NO_STATS" value
#
#########################################################################

import sys
import lib_ss_review as lssr

# ----------------------------------------------------------------------

scriptname = '@ss_review_html'
qcbase     = 'QC'                    # full odir has subj ID concatenated
dir_img    = 'media_img'
dir_dat    = 'media_dat'

page_title_json = '_page_title'

# ----------------------------------------------------------------------

def check_dep(D, lcheck):
    '''Does dictionary 'D' contain each of the elements of 'lcheck'?'''

    HAS_ALL = 1
    for x in lcheck:
        # general check: based on existence
        if not(D.__contains__(x)) :
            HAS_ALL = 0
            break

        # specific check(s), based on known values:
        elif x == "stats_dset" :
            if D[x] == "NO_STATS" :
                HAS_ALL = 0
                break

    return HAS_ALL

# ----------------------------------------------------------------------

def bannerize( x, fullwid=76, indent=0, padpre=1, padpost=2):
    '''Make the banner for each QC image block.  

Inputs
------
    x : a string that is the message to put, and it gets wrapped with a
        comment symbol and '='.

    fullwid : can be any number of width; default is 76 because there
        are 2 other characters put in: ' ' before/after the string.

    indent : number of spaces to prepend to the line, if desired

    padpre : number of empty lines to affix beforehand

    padpost : number of empty lines to affix after text

Returns
-------
    string : x padded with comment/lines/spaces.

    '''

    out = indent*' ' + "# "

    x = x.strip()
    freespace = fullwid - len(out) - len(x)

    if freespace < 0:
        freespace = 0

    lban = freespace // 2
    rban = freespace - lban
        
    out+= lban*"="
    out+= ' '+x+' '
    out+= rban*"="

    if padpre:
        out = padpre*'\n'+out
    if padpost:
        out+= padpost*'\n'

    return out

# ----------------------------------------------------------------------

def padassign(x, L):
    '''Move an assignment operator '=' rightward to a new index L, if it
exists to the left; if it exists to the right of that spot, or if it
doesn't appear in that string, just return the original string.  If
multiple occurences of '=' occur in a string, just return original.

    '''
    
    # check if string contains it
    if not('=' in x):
        return x

    # check if multiple occurences
    if x.count('=') > 1:
        return x

    # check if we are already there/rightward
    K = x.index('=')
    if K >= L:
        return x

    out = x.replace('=', (L-K)*' '+'=')

    return out

# ----------------------------------------------------------------------

def commandize( x, fullwid=76, indent=0, padpre=0, padpost=0,
                ALIGNASSIGN=False, ALLEOL=True, cmdindent=4,
                REP_TIL=True):
    '''Make the readable/spaced/EOL-char'ed version for each QC image
cmd.

Inputs
------

    x : a string that is the cmd; should arranged into multiple lines
        (for long commands). Don't need/shouldn't have EOL chars.

    fullwid : can be any number of width; default is 76, since ' \'
        gets added on to most lines.

    indent : number of spaces to prepend to each line, if desired

    padpre : number of empty lines to affix beforehand

    padpost : number of empty lines to affix after text

    ALIGNASSIGN : flag to line up assignment operators, using longest in
        list of commands

    ALLEOL : flag to use the EOL char at the end of all (but the last)
        lines;  if entering separate cmds, turn off.

    cmdindent : number of spaces to prepend to lines after the [0]th,
        i.e., just indenting within a single command.  Wouldn't be
        used if there are several sep lines, for example.

    REP_TIL : replace any tilde '~' with a space at the end (may be
        useful because the lines are stripped of surrounding white
        space to start)

Returns
-------

    string : will have the same number of lines as x, but all lines
    after [0]th will have indentation, and each line except [-1]th
    will have EOL char at end

    '''

    y = x.split('\n')
    Nlines = len(y)
    count = 0
    new = []
    maxia = 0    # will be loc of assign ops if using ALIGNASSIGN

    # go through, strip whitespace from ends, and find out where the
    # '=' are in each line
    z = []
    for line in y:
        ll = line.strip()
        if ALIGNASSIGN :
            if '=' in ll:
                iassign = ll.index('=')
                if iassign > maxia :
                    maxia = iassign
        z.append(ll)

    # line up assign ops if asked; also, need nonzero maxia
    if ALIGNASSIGN and maxia:
        for i in range(len(z)):
            z[i] = padassign(z[i], maxia)

    # go through last time and space everything appropriately
    for line in z:
        ll = line.strip()
        if len(ll) :
            ll = indent*' ' +ll
            if count:
                ll = cmdindent*' ' + ll
            if ALLEOL :
                lenll = len(ll)
                pad = fullwid - lenll
                if pad > 0:
                    ll+= pad*' '
            if REP_TIL:
                ll = ll.replace('~', ' ')
            new.append( ll )
            count+=1

    # put newlines at ends of each, or not
    if ALLEOL :
        out = ' \\\n'.join(new)
    else:
        out = '\n'.join(new)

    if padpre:
        out = padpre*'\n'+out
    if padpost:
        out+= padpost*'\n'

    return out

# ---------------------------------------------------------------------

def commentize( x, fullwid=76, indent=0, padpre=0, padpost=0,
                REP_TIL=True):
    '''Take a string and make it into a comment; indent uniformly, if
necessary.  Having '||' in a string (surrounded by whitespace) will
translate to starting a new line.

Inputs
------

    x : a string that is the cmd; should arranged into multiple lines
        (for long commands). Don't need/shouldn't have EOL chars.

    fullwid : can be any number of width; default is 76, since ' \'
        gets added on to most lines.

    indent : number of spaces to prepend to each line, if desired

    padpre : number of empty lines to affix beforehand

    padpost : number of empty lines to affix after text

    REP_TIL : replace any tilde '~' with a space at the end (may be
        useful because the lines are stripped of surrounding white
        space to start)

Returns
-------

    string : output >=1 line string version of the original, but
        spaced to not overrun the fullwid line width.  Can be
        uniformly indented.  If the input x is only whitespace or
        empty, then a null string is returned.

    '''

    y = x.split()

    new = []
    line = indent*' ' + '#'
    lenstart = len(line)
    for word in y:
        if word == "||":
            new.append(line)
            line = indent*' ' + '#'
        elif len(line) + len(word) < fullwid:
            line+= ' ' + word
        else:
            new.append(line)
            line = indent*' ' + '#'
            line+= ' ' + word
    # and get the last line, if there is any text still there beyond
    # just the starting stuff
    if len(line) > lenstart :
        new.append(line)

    out = '\n'.join(new)

    if padpre:
        out = padpre*'\n'+out
    if padpost:
        out+= padpost*'\n'

    if REP_TIL :
        out = out.replace('~', ' ')

    return out

# ---------------------------------------------------------------------

def echoize( x, efile='', indent=0, padpre=0, padpost=0,
             quote='''"''', REP_TIL=True):
    '''Take a string and make it into a series of echo statements that
will be sent into a text file; indent uniformly, if necessary.  Each
line is stripped of white space to start, but the '~' character will
be replaced by a space at the end, by default.  The 

Inputs
------

    x : a string that is the cmd; should arranged into multiple lines
        (for long commands). Don't need/shouldn't have EOL chars.

    efile : redirect echo to a file; otherwise, just sent to screen

    indent : number of spaces to prepend to each line, if desired

    padpre : number of empty lines to affix beforehand

    padpost : number of empty lines to affix after text

    quote : select kind of quote to use for echo; default is ".

    REP_TIL : replace any tilde '~' with a space at the end (may be
        useful because the lines are stripped of surrounding white
        space to start)

Returns
-------

    string : output >=1 line string version of the original, but
        spaced to not overrun the fullwid line width.  Can be
        uniformly indented.  If the input x is only whitespace or
        empty, then a null string is returned.

    '''

    multix = x.split('\n')
    N   = len(multix)
    new = []

    # set variable to contain output text file name
    if efile :
        new.append( '# text for output section in html'  )
        new.append( 'set otxt = {}'.format(efile) )

    for i in range(N):
        line = multix[i]
        ll = line.strip()
        y = indent * ' '
        y+= '''echo {}'''.format(quote) + ll + '''{}'''.format(quote)
        if REP_TIL:
            y = y.replace('~', ' ')
        if efile :
            # choose how to redirect each line
            red = '> ${otxt}' if ( i == 0 ) else '>> ${otxt}'
            y+= '''   {}'''.format(red)
        new.append(y)
        

    out = '\n'.join(new)

    if padpre:
        out = padpre*'\n'+out
    if padpost:
        out+= padpost*'\n'

    return out

# ======================================================================
# ======================================================================
# ===================== specifics for the dsets ========================
# ======================================================================
# ======================================================================

def make_apqc_top_vars(ssdict, fulllist):
    '''Count how many variables for QC imaging exist in the list searched
    for by gen_ss_review.py.  Build a tcsh-variable setting string of
    them.

    '''

    Nap = len(ssdict)

    if Nap:
        print('++ Found {} files for QCing.'.format(Nap))
    else:
        print('*+ Warning! Found *0* files for QCing???')
        sys.exit(0)

    out = ''
    for x in fulllist:
        if ssdict.__contains__(x):
            out+= 'set {} = {}\n'.format(x, ssdict[x])
    out = commandize( out, cmdindent=0, 
                      ALIGNASSIGN=True, ALLEOL=False,
                      padpre=1, padpost=2 )

    return out

# ----------------------------------------------------------------------

def make_apqc_dirs():
    '''Commands to make the QC/ and subdirs for images, etc. These are
fixed names, basically.  Should be run near start of prog.

    '''

    comm  = ''' pretty self explanatory'''

    # a little fun with curly brackets and Python's s.format()
    pre = '''
    set odir_qc = {}_${{subj}}
    set odir_img = ${{odir_qc}}/{}
    set odir_dat = ${{odir_qc}}/{}
    '''.format( qcbase, dir_img, dir_dat )

    cmd = '''
    \\mkdir -p ${odir_img}
    \\mkdir -p ${odir_dat}
    '''

    comm = commentize( comm )
    pre  = commandize( pre, cmdindent=0, 
                       ALIGNASSIGN=True, ALLEOL=False )
    cmd  = commandize( cmd, cmdindent=0, 
                       ALIGNASSIGN=True, ALLEOL=False,
                       padpost=2 )

    lout = [comm, pre, cmd]
    return '\n\n'.join(lout)

# ========================== other top vars ==============================

# need to *find* the template...
# ['template']
def apqc_find_template( ):

    comm  = '''Find the template'''

    pre = '''
    set btemp = `basename ${template}`
    '''

    pre2 = '''# try to locate the template
set templ_path = `@FindAfniDsetPath ${template}`

if ( ${#templ_path} ) then
    set templ_vol = "${templ_path}/${btemp}"
    echo "*+ Found ${templ_vol}"
else
    echo "** ERROR: Cannot find template, even though one was specified."
    echo "   Please place the template in a findable spot, and try again."
    exit 1
endif

'''

    pre  = commandize( pre, cmdindent=0, 
                       ALIGNASSIGN=True, ALLEOL=False )

    lout = [pre, pre2]
    return '\n\n'.join(lout)

# ========================== 1D files/plots ==============================

# ['motion_dset', 'nt_orig']
def apqc_1D_volreg(jpgsize, opref, run_mode):

    comm  = ''' review plots: 3dvolreg motion regressors'''

    pre = '''
    set jpgsize = {} 
    set opref = {}
    set tjson  = _tmp.txt
    set ojson  = ${{odir_img}}/${{opref}}.json
    @ imax = ${{nt_orig}} - 1
    '''.format(jpgsize, opref)
    
    otxt  = "${odir_img}/${opref}" + ".txt"

    if run_mode == 'basic' :
        cmd = '''
        1dplot                                                     
        -sepscl 
        -volreg 
        -xlabel   "vol"
        -title    "Estimated motion parameters (volreg)"
        -jpgs     ${jpgsize} "${odir_img}/${opref}" 
        "${motion_dset}" 
        '''
    elif run_mode == 'pythonic' :
        cmd = '''
        1dplot.py                                                     
        -sepscl 
        -boxplot_on    
        -reverse_order 
        -infiles  "${motion_dset}"
        -ylabels   VOLREG
        -xlabel   "vol index"
        -title    "Estimated motion parameters (volreg)" 
        -prefix   "${odir_img}/${opref}.jpg" 
        '''

    # text shown above image in the final HTML
    jsontxt = '''
    cat << EOF >! ${tjson}
    title ::  Check: volume registration motion parameters
    text  ::  ""
    linkid :: VR6
    linkid_hov :: volume registration motion parameters
    EOF
    '''

    jsontxt_cmd = '''
    abids_json_tool.py   
    -overwrite       
    -txt2json              
    -delimiter_major '::'    
    -delimiter_minor ',,'     
    -input  ${tjson}
    -prefix ${ojson}
    '''

    comm  = commentize( comm )
    pre   = commandize( pre, cmdindent=0, 
                        ALIGNASSIGN=True, ALLEOL=False )
    cmd   = commandize( cmd )
    jsontxt = commandize( jsontxt, cmdindent=0, ALLEOL=False )
    jsontxt_cmd  = commandize( jsontxt_cmd, padpost=2  )

    lout  = [comm, pre, cmd, jsontxt, jsontxt_cmd]
    return '\n\n'.join(lout)

# ----------------------------------------------------------------------

# **NB**: it is likely that any changes in this function should mirror
# those of apqc_1D_motenorm_cen

# ['outlier_dset', 'nt_orig'], also use 'censor_dset' and 'out_limit'
def apqc_1D_cen_out( jpgsize, opref, run_mode,
                     has_cen_dset=False,
                     has_lim=False ):

    STR_CEN_pre   = '''set cstr = "(no censoring applied)"'''
    STR_CL_title  = ''
    STR_CEN_json2 = ''
    if has_cen_dset : 
        STR_CEN_pre = '''set cstr = `1d_tool.py -show_trs_censored encoded -infile ${censor_dset}`
        set Ncstr = `1d_tool.py -verb 0 -show_censor_count -infile ${censor_dset}`
        set Pcstr = `echo "scale=0; ${Ncstr} * 100 / ${nt_orig}" | bc`'''
        STR_CEN_json2+= ''' (${Pcstr}%)'''
        if has_lim : 
            STR_CL_title+= ''' (with limit)'''
        STR_CL_title+= ''' and all censored points'''

    comm  = ''' review plots (colored TRs are censored); outliers with 
    fraction limit'''

    pre = '''
    set jpgsize = {} 
    set opref = {}
    set tjson  = _tmp.txt
    set ojson  = ${{odir_img}}/${{opref}}.json
    @ imax = ${{nt_orig}} - 1
    {}
    '''.format(jpgsize, opref, STR_CEN_pre)

    if run_mode == 'basic' :

        # extra uvars may or may not be used
        STR_CEN_cmd = ''
        STR_LIM_cmd = ''
        STR_CEN_json = ''
        STR_LIM_json = ''
        if has_cen_dset : 
            STR_CEN_cmd+= '''-censor_RGB green 
                             -censor   ${censor_dset}'''
            STR_CEN_json+=''', total censoring (green)'''
            # think one only has a censor limit if one has a censor file??
            if has_lim : 
                STR_LIM_cmd+= '''"1D: ${nt_orig}@${out_limit}"'''
                STR_LIM_json+=''', limit (red)'''

        cmd = '''
        1dplot
        -one 
        {}
        -jpgs     ${{jpgsize}} "${{odir_img}}/${{opref}}"
        -aspect   2
        -xlabel   "vol"
        -title    "Outlier frac{}"
        ${{outlier_dset}}
        {}
        '''.format( STR_CEN_cmd, STR_CL_title, STR_LIM_cmd )

    elif run_mode == 'pythonic' :

        # extra uvars may or may not be used
        STR_CEN_cmd  = ''
        STR_LIM_cmd  = ''
        STR_CEN_json = ''
        STR_LIM_json = ''
        if has_cen_dset : 
            STR_CEN_cmd+= '''-censor_files ${censor_dset}'''
            STR_CEN_json+=''', total censoring (red)'''
            # think one only has a censor limit if one has a censor file??
            if has_lim : 
                STR_LIM_cmd+= '''-censor_hline ${out_limit}'''
                STR_LIM_json+=''', limit (cyan)'''

        cmd = '''
        1dplot.py                                                     
        -boxplot_on    
        -reverse_order 
        -infiles  "${{outlier_dset}}"
        -ylabels   "frac"
        {}
        {}
        -xlabel   "vol index"
        -title    "Outlier frac{}"
        -prefix   "${{odir_img}}/${{opref}}.jpg" 
        '''.format( STR_CEN_cmd, STR_LIM_cmd, STR_CL_title)

    # text shown above image in the final HTML; same for basic and
    # pythonic, b/c just the format strings hold differences now
    jsontxt = '''
    cat << EOF >! ${{tjson}}
    title ::  Check: outliers
    text  ::  volume fraction (black){}{}
    subtext :: "censored vols{}: ${{cstr}}"
    linkid :: out
    linkid_hov :: outliers 
    EOF
    '''.format( STR_LIM_json, STR_CEN_json, STR_CEN_json2 )

    jsontxt_cmd = '''
    abids_json_tool.py   
    -overwrite       
    -txt2json              
    -delimiter_major '::'    
    -delimiter_minor ',,'     
    -input  ${tjson}
    -prefix ${ojson}
    '''

    comm = commentize( comm )
    pre  = commandize( pre, cmdindent=0, 
                       ALIGNASSIGN=True, ALLEOL=False)
    cmd  = commandize( cmd )
    jsontxt = commandize( jsontxt, cmdindent=0, ALLEOL=False )
    jsontxt_cmd  = commandize( jsontxt_cmd, padpost=2  )

    lout = [comm, pre, cmd, jsontxt, jsontxt_cmd]
    return '\n\n'.join(lout)

# ----------------------------------------------------------------------

# **NB**: it is likely that any changes in this function should mirror
# those of apqc_1D_cen_out

# ['censor_dset', 'enorm_dset', 'mot_limit', 'nt_orig']
def apqc_1D_motenorm_cen( jpgsize, opref, run_mode,
                          has_cen_dset=False,
                          has_lim=False ):

    STR_CEN_pre   = '''set cstr = "(no censoring applied)"'''
    STR_CL_title  = ''
    STR_CEN_json2 = ''
    if has_cen_dset : 
        STR_CEN_pre = '''set cstr = `1d_tool.py -show_trs_censored encoded -infile ${censor_dset}`
        set Ncstr = `1d_tool.py -verb 0 -show_censor_count -infile ${censor_dset}`
        set Pcstr = `echo "scale=0; ${Ncstr} * 100 / ${nt_orig}" | bc`'''
        STR_CEN_json2+= ''' (${Pcstr}%)'''
        if has_lim : 
            STR_CL_title+= ''' (with limit)'''
        STR_CL_title+= ''' and all censored points'''

    comm  = ''' review plots (colored TRs are censored); outliers with 
    enorm motion limit'''

    pre = '''
    set jpgsize = {} 
    set opref = {}
    set tjson  = _tmp.txt
    set ojson  = ${{odir_img}}/${{opref}}.json
    @ imax = ${{nt_orig}} - 1
    {}
    '''.format( jpgsize, opref, STR_CEN_pre )

    otxt     = "${odir_img}/${opref}" + ".txt"
    osubtxt  = "${odir_img}/${opref}" + "_SUB.txt"

    if run_mode == 'basic' :

        # extra uvars may or may not be used
        STR_CEN_cmd  = ''
        STR_LIM_cmd  = ''
        STR_CEN_json = ''
        STR_LIM_json = ''
        if has_cen_dset : 
            STR_CEN_cmd+= '''-censor_RGB green 
                             -censor   ${censor_dset}'''
            STR_CEN_json+=''', total censoring (green)'''
            # think one only has a censor limit if one has a censor file??
            if has_lim : 
                STR_LIM_cmd+= '''"1D: ${nt_orig}@${mot_limit}"'''
                STR_LIM_json+=''', mot limit (red)'''

        cmd = '''
        1dplot 
        -one 
        {}
        -jpgs     ${{jpgsize}} "${{odir_img}}/${{opref}}"
        -aspect   2
        -xlabel   "vol"
        -title    "Mot enorm{}"
        ${{enorm_dset}}
        {}
        '''.format( STR_CEN_cmd, STR_CL_title, STR_LIM_cmd )

    elif run_mode == 'pythonic' :

        # extra uvars may or may not be used
        STR_CEN_cmd = ''
        STR_LIM_cmd = ''
        STR_CEN_json = ''
        STR_LIM_json = ''
        if has_cen_dset : 
            STR_CEN_cmd+= '''-censor_files ${censor_dset}'''
            STR_CEN_json+=''', total censoring (red)'''
            # think one only has a censor limit if one has a censor file??
            if has_lim : 
                STR_LIM_cmd+= '''-censor_hline ${mot_limit}'''
                STR_LIM_json+=''', mot limit (cyan)'''

        cmd = '''
        1dplot.py                                                     
        -boxplot_on    
        -reverse_order 
        -infiles  "${{enorm_dset}}"
        -ylabels   "enorm"
        {}
        {}
        -xlabel   "vol index"
        -title    "Mot enorm{}"
        -prefix   "${{odir_img}}/${{opref}}.jpg" 
        '''.format( STR_CEN_cmd, STR_LIM_cmd, STR_CL_title)

    jsontxt = '''
    cat << EOF >! ${{tjson}}
    title ::  Check: motion
    text  ::  "enorm (black){}{}"
    subtext :: "censored vols{}: ${{cstr}}"
    linkid :: mot
    linkid_hov :: motion (enorm)
    EOF
    '''.format( STR_LIM_json, STR_CEN_json, STR_CEN_json2 )

    jsontxt_cmd = '''
    abids_json_tool.py   
    -overwrite       
    -txt2json              
    -delimiter_major '::'    
    -delimiter_minor ',,'     
    -input  ${tjson}
    -prefix ${ojson}
    '''

    comm = commentize( comm )
    pre  = commandize( pre, cmdindent=0, 
                       ALIGNASSIGN=True, ALLEOL=False )
    cmd  = commandize( cmd )
    jsontxt = commandize( jsontxt, cmdindent=0, ALLEOL=False )
    jsontxt_cmd  = commandize( jsontxt_cmd, padpost=2  )

    lout = [comm, pre, cmd, jsontxt, jsontxt_cmd]
    return '\n\n'.join(lout)

# ['xmat_stim']
def apqc_1D_xmat_stim(jpgsize, opref, run_mode ):

    comm  = ''' view xmatrix'''

    pre = '''
    set jpgsize = {} 
    set opref = {}
    set tjson  = _tmp.txt
    set ojson  = ${{odir_img}}/${{opref}}.json
    #@ imax = ${{nt_orig}} - 1
    '''.format(jpgsize, opref)

    if run_mode == 'basic' :
        cmd = '''
        1dplot 
        -sepscl 
        -jpgs     $jpgsize "${odir_img}/${opref}"
        -aspect   2
        -xlabel   "vol"
        -title    "Non-baseline regressors in X-matrix"
        ${xmat_stim}
        '''
    elif run_mode == 'pythonic' :
        cmd = '''
        1dplot.py 
        -sepscl 
        -boxplot_on
        -reverse_order 
        -infiles  ${xmat_stim}
        -xlabel   "vol"
        -title    "Non-baseline regressors in X-matrix"
        -prefix   "${odir_img}/${opref}.jpg"
        '''

    jsontxt = '''
cat << EOF >! ${tjson}
    title ::  Check: regressors (per stimulus)
    text  ::  "non-baseline regressors (in ${xmat_regress})"
    linkid :: regps
    linkid_hov :: regressors (per stimulus)
EOF
'''

    jsontxt_cmd = '''
abids_json_tool.py   
    -overwrite       
    -txt2json              
    -delimiter_major '::'    
    -delimiter_minor ',,'     
    -input  ${tjson}
    -prefix ${ojson}
'''

    comm = commentize( comm )
    pre  = commandize( pre, cmdindent=0, 
                       ALIGNASSIGN=True, ALLEOL=False )
    cmd  = commandize( cmd )
    jsontxt = commandize( jsontxt, cmdindent=0, ALLEOL=False )
    jsontxt_cmd  = commandize( jsontxt_cmd, padpost=2  )

    lout = [comm, pre, cmd, jsontxt, jsontxt_cmd]
    return '\n\n'.join(lout)


# ['sum_ideal']
def apqc_1D_sum_ideal(jpgsize, opref, run_mode):

    comm  = ''' view xmatrix'''

    pre = '''
    set jpgsize = {} 
    set opref = {}
    set tjson  = _tmp.txt
    set ojson  = ${{odir_img}}/${{opref}}.json
    #@ imax = ${{nt_orig}} - 1
    '''.format(jpgsize, opref)

    #otxt  = "${odir_img}/${opref}" + ".txt"

    if run_mode == 'basic' :
        cmd = '''
        1dplot 
        -sepscl 
        -jpgs     $jpgsize "${odir_img}/${opref}"
        -aspect   2
        -xlabel   "vol"
        -title    "Sum of non-baseline regressors in X-matrix"
        ${sum_ideal}
        '''
    elif run_mode == 'pythonic' :
        cmd = '''
        1dplot.py 
        -boxplot_on
        -sepscl 
        -boxplot_on
        -reverse_order 
        -infiles  ${sum_ideal}
        -xlabel   "vol"
        -title    "Sum of non-baseline regressors in X-matrix"
        -prefix   "${odir_img}/${opref}.jpg"
        '''

    jsontxt = '''
cat << EOF >! ${tjson}
    title ::  Check: regressors (combined stimulus)
    text  ::  "sum of non-baseline regressors (in ${sum_ideal})"
    linkid :: regcs
    linkid_hov :: regressors (combined stimulus)
EOF
'''

    jsontxt_cmd = '''
abids_json_tool.py   
    -overwrite       
    -txt2json              
    -delimiter_major '::'    
    -delimiter_minor ',,'     
    -input  ${tjson}
    -prefix ${ojson}
'''

    comm = commentize( comm )
    pre  = commandize( pre, cmdindent=0, 
                       ALIGNASSIGN=True, ALLEOL=False )
    cmd  = commandize( cmd )
    jsontxt = commandize( jsontxt, cmdindent=0, ALLEOL=False )
    jsontxt_cmd  = commandize( jsontxt_cmd, padpost=2  )

    lout = [comm, pre, cmd, jsontxt, jsontxt_cmd]
    return '\n\n'.join(lout)


# ========================== images ================================

# ['stats_dset', 'mask_dset', 'final_anat']
# ['template'] # secondary consideration
def apqc_vol_check_stats_anat( opref, focusbox, iolay, ithr, 
                               olay_posonly=True ):

    perc_thr_thr  = 90                    # %ile for thresholding thr vol
    perc_olay_top = 99                    # %ile for top of pbar for olay

    # what will minval of pbar be? 0, or -max?
    if olay_posonly :
        olay_minval_str = "-pbar_posonly"
        pbar_min = "0"
    else:
        olay_minval_str = "-pass"
        pbar_min = "-${olay_topval}" # $olay_topval gets defined below

    comm  = '''peruse statistical results: 
    {} %ile threshold of thr vol [{}]
    {} %ile topval for olay vol [{}] for pbar'''.format( perc_thr_thr,
                                                         ithr,
                                                         perc_olay_top,
                                                         iolay )

    # NB: below, note the '.axi.json', because of how @chauffeur_afni
    # appends slice plane in the name of each output image
    pre = '''
    set opref = {}
    set focus_box = {}
    set ulay_name = `3dinfo -prefix_noext ${{final_anat}}`
    set olay_name = `3dinfo -prefix_noext ${{stats_dset}}`
    set olaybrick = {}
    set olaylabel = `3dinfo -label ${{stats_dset}}"[${{olaybrick}}]"`
    set thrbrick = {}
    set thrlabel = `3dinfo -label ${{stats_dset}}"[${{thrbrick}}]"`
    set tjson  = _tmp.txt
    set ojson  = ${{odir_img}}/${{opref}}.axi.json
    set tpbarjson  = _tmppbar.txt
    set opbarjson  = ${{odir_img}}/${{opref}}.pbar.json
    '''.format( opref, focusbox, iolay, ithr )

    # threshold for stat dset, from %ile in mask
    cmd0 = '''
    set tt = `3dBrickStat 
    -slow 
    -percentile {0} 1 {0}
    -mask "${{mask_dset}}" 
    ${{stats_dset}}"[${{thrbrick}}]"`
    '''.format( perc_thr_thr )

    cmd1 = '''
    set thr_thresh = ${tt[2]}
    '''

    # top value for colorbar of olay, from %ile in mask
    cmd2 = '''
    set pp = `3dBrickStat 
    -slow 
    -percentile {0} 1 {0}
    -mask "${{mask_dset}}" 
    ${{stats_dset}}"[${{olaybrick}}]"`
    '''.format( perc_olay_top )

    cmd3 = '''
    set olay_topval = ${{pp[2]}}
    set olay_botval = {}
    '''.format( pbar_min )

    cmd4 = '''
    @chauffeur_afni    
    -ulay  ${{final_anat}}
    -box_focus_slices ${{focus_box}}
    -olay  ${{stats_dset}}  
    -cbar Plasma 
    {}
    -ulay_range 0% 120%  
    -func_range ${{olay_topval}}
    -thr_olay ${{thr_thresh}}    
    -olay_alpha Yes
    -olay_boxed Yes
    -set_subbricks -1 ${{olaybrick}} ${{thrbrick}}
    -opacity 9  
    -pbar_saveim   "${{odir_img}}/${{opref}}.pbar.jpg"
    -prefix        "${{odir_img}}/${{opref}}"
    -save_ftype JPEG
    -montx 7 -monty 1  
    -set_xhairs OFF 
    -label_mode 1 -label_size 3  
    -do_clean
    '''.format( olay_minval_str )

    # As default, use :: and ,, as major and minor delimiters,
    # respectively, because those should be useful in general.  
    # NB: because we want the single apostrophe to appear as a text
    # character, we have to wrap this with the double quotes
    jsontxt = '''
cat << EOF >! ${tjson}
    title ::  Check: statistics vol
    text  ::  "olay: ${olay_name} (stat '${olaylabel}')",, "thr : ${olay_name} (stat '${thrlabel}')"
    linkid :: vstat
    linkid_hov :: statistics vol
EOF
'''

    jsontxt_cmd = '''
abids_json_tool.py   
    -overwrite       
    -txt2json              
    -delimiter_major '::'    
    -delimiter_minor ',,'     
    -input  ${tjson}
    -prefix ${ojson}
'''

    pbarjsontxt = '''
cat << EOF >! ${{tpbarjson}}
    pbar_bot :: ${{olay_botval}}
    pbar_top :: ${{olay_topval}}
    pbar_reason :: {}{}
    vthr :: ${{thr_thresh}}
    vthr_reason :: {}{}
EOF
'''.format( perc_olay_top, '''%ile in mask''', 
            perc_thr_thr,  '''%ile in mask''' )

    pbarjsontxt_cmd = '''
abids_json_tool.py   
    -overwrite       
    -txt2json              
    -delimiter_major '::'    
    -delimiter_minor ',,'     
    -input  ${tpbarjson}
    -prefix ${opbarjson}
'''

    comm = commentize( comm )
    pre  = commandize( pre, cmdindent=0, 
                       ALIGNASSIGN=True, ALLEOL=False )
    cmd0  = commandize( cmd0 )
    cmd1  = commandize( cmd1 )
    cmd2  = commandize( cmd2 )
    cmd3  = commandize( cmd3, cmdindent=0, 
                        ALIGNASSIGN=True, ALLEOL=False )
    cmd4  = commandize( cmd4 )

    # NB: for commandizing the *jsontxt commands, one NEEDS
    # 'cmdindent=0', bc 'EOF' cannot be indented to be detected
    pbarjsontxt = commandize( pbarjsontxt, cmdindent=0, ALLEOL=False )
    pbarjsontxt_cmd  = commandize( pbarjsontxt_cmd )
    jsontxt = commandize( jsontxt, cmdindent=0, ALLEOL=False )
    jsontxt_cmd  = commandize( jsontxt_cmd, padpost=2 )

    lout = [comm, pre, cmd0, cmd1, cmd2, cmd3, cmd4, 
            pbarjsontxt, pbarjsontxt_cmd, jsontxt, jsontxt_cmd]
    return '\n\n'.join(lout)

# ----------------------------------------------------------------------

# ['final_anat', 'final_epi_dset']
def apqc_vol_align_epi_anat( opref, focusbox ):

    comm  = '''Compare the quality of alignment between the anatomical 
    (ulay) and edge-ified EPI (olay): || look at gross alignment || 
    follow ventricles and gyral patterns'''

    pre = '''
    set opref = {}
    set focus_box = {}
    set ulay_name = `3dinfo -prefix_noext ${{final_anat}}`
    set olay_name = `3dinfo -prefix_noext ${{final_epi_dset}}`
    set tjson  = _tmp.txt
    set ojson  = ${{odir_img}}/${{opref}}.axi.json
    '''.format( opref, focusbox )

    cmd = '''
    @djunct_edgy_align_check
    -ulay    ${final_anat}
    -box_focus_slices ${focus_box}
    -olay    ${final_epi_dset}
    -prefix  ${odir_img}/${opref}

    '''

    jsontxt = '''
cat << EOF >! ${tjson}
    title ::  Check: vol alignment (EPI-anat)
    text  ::  "ulay: ${ulay_name} (anat)",, "olay: ${olay_name} (EPI edges)"
    linkid :: ve2a
    linkid_hov :: vol alignment (EPI-anat)
EOF
'''

    jsontxt_cmd = '''
abids_json_tool.py   
    -overwrite       
    -txt2json              
    -delimiter_major '::'    
    -delimiter_minor ',,'     
    -input  ${tjson}
    -prefix ${ojson}
'''

    comm = commentize( comm )
    pre  = commandize( pre, cmdindent=0, 
                       ALIGNASSIGN=True, ALLEOL=False )
    cmd  = commandize( cmd )
    jsontxt = commandize( jsontxt, cmdindent=0, ALLEOL=False )
    jsontxt_cmd  = commandize( jsontxt_cmd, padpost=2 )

    lout = [comm, pre, cmd, jsontxt, jsontxt_cmd]
    return '\n\n'.join(lout)

# ----------------------------------------------------------------------

# ['final_anat', 'template']
def apqc_vol_align_anat_tlrc( opref, focusbox ):

    comm  = '''Compare the quality of alignment between the template 
    (ulay) and edge-ified anatomical (olay): || look at gross alignment || 
    follow ventricles and gyral patterns'''

    pre = '''
    set opref = {}
    set focus_box = {}
    set ulay_name = `3dinfo -prefix_noext ${{templ_vol}}`
    set olay_name = `3dinfo -prefix_noext ${{final_anat}}`
    set tjson  = _tmp.txt
    set ojson  = ${{odir_img}}/${{opref}}.axi.json
    '''.format( opref, focusbox )

    jsontxt = '''
cat << EOF >! ${tjson}
    title ::  Check: vol alignment (anat-template) 
    text  ::  "ulay: ${ulay_name} (template)",, "olay: ${olay_name} (anat edges)"
    linkid :: va2t
    linkid_hov :: vol alignment (anat-template) 
EOF
'''

    jsontxt_cmd = '''
abids_json_tool.py   
    -overwrite       
    -txt2json              
    -delimiter_major '::'    
    -delimiter_minor ',,'     
    -input  ${tjson}
    -prefix ${ojson}
'''

    # !!!! ? need '[0]' selector here because (multibrick) SSW reference
    # templates could be reference template 
    cmd = '''
    @djunct_edgy_align_check
    -ulay    ${templ_vol}
    -box_focus_slices ${focus_box}
    -olay    ${final_anat}
    -prefix  ${odir_img}/${opref}

    '''

    comm = commentize( comm )
    pre  = commandize( pre, cmdindent=0, 
                       ALIGNASSIGN=True, ALLEOL=False )
    cmd  = commandize( cmd )
    jsontxt = commandize( jsontxt, cmdindent=0, ALLEOL=False )
    jsontxt_cmd  = commandize( jsontxt_cmd, padpost=2 )

    lout = [comm, pre, cmd, jsontxt, jsontxt_cmd]
    return '\n\n'.join(lout)

# ========================== dat/txt ================================

# @ss_review_basic dumped to text file
# 1
def apqc_dat_ss_review_basic( opref ):

    comm  = '''basic information from processing'''

    pre = '''
    set opref = {}
    set tjson  = _tmp.txt
    set ojson  = ${{odir_dat}}/${{opref}}.json
    '''.format( opref )

    cmd0 = '''
    cat out.ss_review.${subj}.txt
    > ${odir_dat}/${opref}.dat
    '''

    cmd1 = '''
echo "++ Check basic proc info in: ${odir_dat}/${opref}.dat"
    '''

    jsontxt = '''
cat << EOF >! ${tjson}
    title ::  Check: basic processing information
    text  ::  ""
    linkid :: info
    linkid_hov :: basic processing information
EOF
'''

    jsontxt_cmd = '''
abids_json_tool.py   
    -overwrite       
    -txt2json              
    -delimiter_major '::'    
    -delimiter_minor ',,'     
    -input  ${tjson}
    -prefix ${ojson}
'''

    comm  = commentize( comm )
    pre   = commandize( pre, cmdindent=0, 
                       ALIGNASSIGN=True, ALLEOL=False )
    cmd0  = commandize( cmd0 )
    cmd1  = commandize( cmd1, ALLEOL=False )
    jsontxt = commandize( jsontxt, cmdindent=0, ALLEOL=False )
    jsontxt_cmd  = commandize( jsontxt_cmd, padpost=2 )

    lout = [comm, pre, cmd0, cmd1, jsontxt, jsontxt_cmd]
    return '\n\n'.join(lout)

# ========================== warn/txt ================================

# Text warning, goes to dir_dat output
# ['xmat_regress']
def apqc_dat_cormat_warn( opref ):

    comm  = '''review: check for correlation matrix warnings'''

    pre = '''
    set opref = {}
    set tjson  = _tmp.txt
    set ojson  = ${{odir_dat}}/${{opref}}.json
    '''.format( opref )

    cmd0 = '''
    1d_tool.py 
    -show_cormat_warnings 
    -infile ${xmat_regress}
    > ${odir_dat}/${opref}.dat
    '''

    cmd1 = '''
echo "++ Check for corr matrix warnings in: ${odir_dat}/${opref}.dat"
    '''

    jsontxt = '''
cat << EOF >! ${tjson}
    title ::  Check: regression matrix correlation warnings
    text  ::  ""
    linkid :: wmat
    linkid_hov :: reg matrix corr warnings
EOF
'''

    jsontxt_cmd = '''
abids_json_tool.py   
    -overwrite       
    -txt2json              
    -delimiter_major '::'    
    -delimiter_minor ',,'     
    -input  ${tjson}
    -prefix ${ojson}
'''


    comm  = commentize( comm )
    pre   = commandize( pre, cmdindent=0, 
                       ALIGNASSIGN=True, ALLEOL=False )
    cmd0  = commandize( cmd0 )
    cmd1  = commandize( cmd1, ALLEOL=False )
    #imtxt = echoize(imtxt, efile=otxt, padpost=2)
    jsontxt = commandize( jsontxt, cmdindent=0, ALLEOL=False )
    jsontxt_cmd  = commandize( jsontxt_cmd, padpost=2 )

    lout = [comm, pre, cmd0, cmd1, jsontxt, jsontxt_cmd]
    return '\n\n'.join(lout)

# ----------------------------------------------------------------------

# Text warning, goes to dir_dat output
# ['pre_ss_warn_dset']
def apqc_dat_pre_ss_warn( opref ):

    comm  = '''review: check for pre-steady state warnings'''

    pre = '''
    set opref = {}
    set tjson  = _tmp.txt
    set ojson  = ${{odir_dat}}/${{opref}}.json
    '''.format( opref )

    cmd = '''
if ( -f ${pre_ss_warn_dset} && ! -z ${pre_ss_warn_dset} ) then
~~~~cat ${pre_ss_warn_dset} > ${odir_dat}/${opref}.dat
else
~~~~echo "\\nnone\\n"  > ${odir_dat}/${opref}.dat
endif
    '''

    jsontxt = '''
cat << EOF >! ${tjson}
    title ::  Check: pre-steady state warnings
    text  ::  ""
    linkid :: wpss
    linkid_hov :: pre-steady state warnings
EOF
'''

    jsontxt_cmd = '''
abids_json_tool.py   
    -overwrite       
    -txt2json              
    -delimiter_major '::'    
    -delimiter_minor ',,'     
    -input  ${tjson}
    -prefix ${ojson}
'''

    comm = commentize( comm )
    pre  = commandize( pre, cmdindent=0, 
                       ALIGNASSIGN=True, ALLEOL=False )
    cmd  = commandize( cmd, cmdindent=0, ALLEOL=False )
    jsontxt = commandize( jsontxt, cmdindent=0, ALLEOL=False )
    jsontxt_cmd  = commandize( jsontxt_cmd, padpost=2 )

    lout = [comm, pre, cmd, jsontxt, jsontxt_cmd]
    return '\n\n'.join(lout)

# ----------------------------------------------------------------------

# Text warning, goes to dir_dat output
# ['tent_warn_dset']
def apqc_dat_tent_warn( opref ):

    comm  = '''show any TENT warnings from timing_tool.py'''

    pre = '''
    set opref = {}
    set tjson  = _tmp.txt
    set ojson  = ${{odir_dat}}/${{opref}}.json
    '''.format( opref )

    cmd = '''
if ( -f ${tent_warn_dset} ) then
~~~~cat ${tent_warn_dset} > ${odir_dat}/${opref}.dat
else
~~~~echo "\\nnone\\n"  > ${odir_dat}/${opref}.dat
endif
    '''

    jsontxt = '''
cat << EOF >! ${tjson}
    title ::  Check: TENT warnings from timing_tool.py
    text  ::  ""
    linkid :: wTENT
    linkid_hov :: TENT warnings from timing_tool.py
EOF
'''

    jsontxt_cmd = '''
abids_json_tool.py   
    -overwrite       
    -txt2json              
    -delimiter_major '::'    
    -delimiter_minor ',,'     
    -input  ${tjson}
    -prefix ${ojson}
'''

    comm = commentize( comm )
    pre  = commandize( pre, cmdindent=0, 
                       ALIGNASSIGN=True, ALLEOL=False )
    cmd  = commandize( cmd, cmdindent=0, ALLEOL=False )
    #imtxt = echoize(imtxt, efile=otxt, padpost=2)
    jsontxt = commandize( jsontxt, cmdindent=0, ALLEOL=False )
    jsontxt_cmd  = commandize( jsontxt_cmd, padpost=2 )

    lout = [comm, pre, cmd, jsontxt, jsontxt_cmd]
    return '\n\n'.join(lout)


# ========================== term echo ==============================

# @ss_review_basic dumped to text file
# 1
def apqc_term_ss_review_basic( opref ):

    comm  = '''basic information from processing'''

    pre = '''
    echo ""
    echo ""
    echo "# +++++++++++ Check output of @ss_review_basic +++++++++++ #"
    echo ""
    '''

    cmd0 = '''
    cat out.ss_review.${subj}.txt
    '''

    comm  = commentize( comm )
    pre   = commandize( pre, cmdindent=0, 
                       ALIGNASSIGN=True, ALLEOL=False )
    cmd0  = commandize( cmd0 )

    lout = [comm, pre, cmd0]
    return '\n\n'.join(lout)


# ======================== html page title ============================

def apqc_dat_html_title( opref, task_name = '' ):

    comm  = '''subject ID for html page title'''

    pre = '''
    set opref = {}
    set tjson  = _tmp.txt
    set ojson  = ${{odir_dat}}/${{opref}}.json
    '''.format( opref )

    #otxt  = "${odir_dat}/${opref}" + ".txt"
    jsontxt = '''
cat << EOF >! ${tjson}
    title  ::  "afni_proc.py single subject report"
    subj   ::  "${subj}"
    linkid :: "${subj}"
    linkid_hov :: top of page
EOF
'''
## !! to be added at some point, from new uvar:
##    taskname  ::  task

    jsontxt_cmd = '''
abids_json_tool.py   
    -overwrite       
    -txt2json              
    -delimiter_major '::'    
    -delimiter_minor ',,'     
    -input  ${tjson}
    -prefix ${ojson}
'''


#    cmd = '''
#echo "afni_proc.py single subject report" > ${odir_dat}/${opref}.dat
#echo "${subj}" >> ${odir_dat}/${opref}.dat
#    '''

    comm  = commentize( comm )
    pre   = commandize( pre, cmdindent=0, 
                       ALIGNASSIGN=True, ALLEOL=False )
    #cmd  = commandize( cmd, ALLEOL=False )
    jsontxt = commandize( jsontxt, cmdindent=0, ALLEOL=False )
    jsontxt_cmd  = commandize( jsontxt_cmd, padpost=2 )

    lout = [comm, pre, jsontxt, jsontxt_cmd]
    return '\n\n'.join(lout)



if __name__ == "__main__":
    print('Done.')
