#!/usr/bin/env python

import sys, copy

# This is a small library of inelegant functions for writing out a
# command in nicely vertically-spaced fashion over multiple lines.
# The functions do some guesswork about how to push things to multiple
# lines.
#
# started by PA Taylor (SSCC, NIMH, NIH)
#
# -------------------------------------------------------------------------
# 2022-01-14, ver 1.0 :  creazione
# 2022-01-14, ver 1.1 :  have afni_niceify_cmd_str() pass along kwargs
# 2022-03-20, ver 1.2 :  allow user to input list of args for splitting
#                        (bc opt names can take sub-opts that we would not
#                        want to split at---e.g., within afni_proc.py calls)
# 2022-03-20, ver 1.3 :  introduce second-level splitting of arg lists, to
#                        account for a primary option taking several
#                        secondary options (space the latter out to sep
#                        rows now---will be useful for AP)
# 2022-03-21, ver 1.4 :  recognize (and ignore) escaped quotes, when 
#                        calculating where quoted blocks occur
# 2022-08-09, ver 1.5 :  tweak opt proc: recognize -1Dmatrix_apply as
#                        an opt (even though it starts with -1.
#                        Also, remove trailing whitespace in final line
#                        of cmd.
# 2022-08-19, ver 1.6 :  isnumeric() -> isdigit(), for Py2.7 compatability.
# 2022-10-07, ver 1.7 :  afni_niceify_cmd_str() gets new big_list kwarg
#                        to play nice with AP help examples
# -------------------------------------------------------------------------

# DEFAULTS for kwargs in these funcs

AD = { \
       'quote_pair_list'   : [ "'", '"' ],
       'list_cmd_args' : [],
       'maxcount'      : 10**6,
       'nindent'       : 4,
       'max_lw'        : 78,
       'max_harg1'     : 40,
       'comment_start' : None,
       'verb'          : 0,
}

# -------------------------------------------------------------------------

# Primarily a suppl function of split_str_outside_quotes()
def find_next_quote_in_str( sss, quote_pair_list=AD['quote_pair_list'] ):
    '''Find the first instance of ' or " appearing in the string, by
default.  The user can optionally specify a different list of strings
to search for (e.g., just one particular quote); the quote_pair_list must
always be a list.

    If one is found, return:
    + the index i where it is found; 
    + the left index L of any non-whitespace block it is part of; 
    + the right index plus one R of any non-whitespace block it is part
      of (so, that str[L:R] would select the intact island, in Pythonese);
    + which kind of quote it is.  
    Otherwise, return: -1, -1, -1, None.

    This function will ignore escaped quotes.

    '''

    if type(quote_pair_list) != list :
        print("** ERROR: need to input a *list* of strings for searching")
        return -1, None

    # initialize a few things
    N     = len(sss)
    qind  = 0
    qtype = None
    FOUND = 0

    # find the first index where one of the quote_pair_list items appears;
    # don't use str.find() bc we want to avoid escaped quotes
    # NB: at the moment, each quote pair list item is assumed to be a single 
    # char.  Could generalize, but don't see the need at commandline at 
    # present; maybe the for the '[[' in bash scripts someday?
    while qind < N :
        if sss[qind] in quote_pair_list:
            # ensure quote is not escaped (which cannot happen on [0]th char)
            if qind == 0:
                FOUND = 1
            elif sss[qind-1] != '\\' :
                FOUND = 1
                
            if FOUND :
                qtype = sss[qind]
                break
        qind+= 1

    # now, qind contains the leftmost quote found (if any), and qtype
    # records what type it is, from within quote_pair_list items

    if FOUND :
        # calc left (=closed) boundary of non-whitespace island
        lind = qind
        while lind >= 0 :
            if not(sss[lind].isspace()) :
                lind-= 1
            else:
                break
        lind+= 1

        # calc right (=open) boundary of non-whitespace island
        rind = qind
        while rind < N :
            if not(sss[rind].isspace()) :
                rind+= 1
            else:
                break

        return qind, lind, rind, qtype
    else:
        return -1, -1, -1, qtype

# -------------------------------------------------------------------------

# Primarily a supply func of listify_argv_str()
def split_str_outside_quotes( sss, quote_pair_list=AD['quote_pair_list'], 
                              maxcount=AD['maxcount'] ):
    '''For the input string sss, leave parts that are contained within
quotes (either ' or ") intact, and split the rest at whitespace.
Retain any non-whitespace chars that a quote is attached to as part of
the intact region.

If strings are nested, the outer pair takes precedence (so, nesting
doesn't really matter).

maxcount is a number to guard against infinite loops.  Should just be
"big".

Return a list of strings.

    '''

    if type(sss) != str :
        print("** ERROR: need to input a single string for searching")
        return []
    elif len(sss) == 0:
        return []

    olist = []
    count = 0
    newstart = 0      # track each new start of string search
    ttt   = sss[:]

    top, ltop, rtop, qtype = find_next_quote_in_str(ttt, 
                                 quote_pair_list=AD['quote_pair_list'])

    while top >= 0 :
        if top :
            list1 = ttt[:ltop].split()
            olist.extend(list1)

        newstart+= top+1
        # look for a partner/closing quote of the matching variety
        top2, ltop2, rtop2, qtype2 = find_next_quote_in_str(ttt[top+1:], 
                                                            [qtype])
        
        if top2 >= 0 :
            # The case of finding a partner quote. NB: because of the
            # way we check substrings, indices look funny here, but work
            list2 = [ttt[ltop:top+1+rtop2]]
            olist.extend(list2)
            ttt   = ttt[top+1+rtop2:]
            newstart+= rtop2
            top, ltop, rtop, qtype = find_next_quote_in_str(ttt,
                                         quote_pair_list=AD['quote_pair_list'])
        else:
            # The case of not finding a partner quote.  At present, we
            # then ignore any other kinds of quotes.  Have to ponder
            # if that is reasonable...
            print("+* WARN: char [{}] is unmatched quote {}, ignore it"
                  "".format(newstart+top2, qtype))
            list2 = ttt[ltop:].split()
            olist.extend(list2)
            ttt = ''
            top = -1

        # purely to guard against infinite looping; shouldn't happen
        count+=1
        if count > maxcount:
            print("** ERROR: infinite loop (?) encountered. Truncating.")
            return []

    # split any remainder
    if len(ttt):
        olist.extend(ttt.split())

    return olist

# -------------------------------------------------------------------------

# A primary-ish function to organize a list-of-sublists form for the cmd.
def listify_argv_str( sss, quote_pair_list=AD['quote_pair_list'], 
                      list_cmd_args=AD['list_cmd_args'],
                      maxcount=AD['maxcount'] ):
    '''Take a command line string sss, and turn it into a list of lists,
where each 'inner'/sublist would be one line of items when printed
later.

For example, an option flag (no args) would be a single-item sublist;
an option with one or more args would comprise a single sublist; and a
'position' argument would be a single-item sublist.  If no
list_cmd_args is provided, then there is some fanciness about
recognizing that '-5' is likely an argument for an option, rather than
an option itself...

Return the list of sublists (= the big_list). Each sublist will
(potentially) be one row in the output (though line spacing is handled
elsewhere).  Each element in each sublist is stripped of whitespace
here.

    '''

    if type(sss) != str :
        print("** ERROR: need to input a single string")
        return []

    # do whitespace-based splitting while also retaining quoted regions
    arg_list = split_str_outside_quotes(sss, 
                                        quote_pair_list=AD['quote_pair_list'], 
                                        maxcount=AD['maxcount'])
    N        = len(arg_list)

    if not(N) :
        print("+* WARN: nothing in command line string?")
        return []

    if list_cmd_args :
        return make_big_list_from_args(arg_list,
                                       list_cmd_args)
    else:
        return make_big_list_auto(arg_list)

def make_big_list_auto(arg_list):
    '''Take the arge list already passed through whitespace splitting and
    quote-pairing and make a 'big_list' of the opts.  Namely, return a
    list of sub-lists, where the [0]th list is the program name and
    each subsequent list will contain an option and any args for it.

    In this function, the determination of a new sub-list is made
    automatically with some logic.

    See make_big_list_from_args(...) if you know the opt names for the
    program and want to split arg_list into sublists using that.

    '''

    # initialize list: [0]th item should be program name
    big_list  = [[arg_list[0]]]
    mini_list = []
    i = 1
    N = len(arg_list)

    while i < N :
        iarg = arg_list[i]
        narg = len(iarg)
        if iarg[0] == '-' and narg == 1 : 
            # looks like new opt: store any existing (non-empty)
            # mini_list, and start new one with this str
            if mini_list : 
                big_list.append(mini_list)
            mini_list = [iarg]
        elif iarg[0:2] == '--' or \
             ( iarg[0] == '-' and not(iarg[1:].isdigit()) ) :
            # looks like new opt: store any existing (non-empty)
            # mini_list, and start new one with this str
            if mini_list : 
                big_list.append(mini_list)
            mini_list = [iarg]
        elif mini_list :
            # otherwise, if a mini_list exists, keep adding to it
            mini_list.append(iarg)
        else:
            # otherwise, there is no mini_list and this looks like an
            # arg-by-position: is its own mini_list
            big_list.append([iarg])
        i+= 1
    if mini_list :
        # add any remaining mini_list
        big_list.append(mini_list)

    return big_list

def make_big_list_from_args(arg_list,
                            list_cmd_args=AD['list_cmd_args']):
    '''Take the arge list already passed through whitespace splitting and
    quote-pairing and make a 'big_list' of the opts.  Namely, return a
    list of sub-lists, where the [0]th list is the program name and
    each subsequent list will contain an option and any args for it.

    In this function, the determination of a new sub-list is made
    using a provided list of args.

    See make_big_list_auto(...) if you don't know the opt names for
    the program and want to split arg_list into sublists using some
    reasonable/automated logic.
    '''

    # initialize list: [0]th item should be program name
    big_list  = [[arg_list[0]]]
    mini_list = []
    i = 1
    N = len(arg_list)

    while i < N :
        iarg = arg_list[i]
        narg = len(iarg)
        if iarg in list_cmd_args :
            # looks like new opt: store any existing (non-empty)
            # mini_list, and start new one with this str
            if mini_list : 
                big_list.append(mini_list)
            mini_list = [iarg]
        elif mini_list :
            # otherwise, if a mini_list exists, keep adding to it
            mini_list.append(iarg)
        else:
            # otherwise, there is no mini_list and this looks like an
            # arg-by-position: is its own mini_list
            big_list.append([iarg])
        i+= 1
    if mini_list :
        # add any remaining mini_list
        big_list.append(mini_list)

    return big_list

# -------------------------------------------------------------------------

# A primary function for converting a big_list (= list-of-sublist)
# form of cmd to a multiline-string
def pad_argv_list_elements( big_list, 
                            nindent=AD['nindent'], max_lw=AD['max_lw'],
                            max_harg1=AD['max_harg1'],
                            comment_start=AD['comment_start'] ):

    '''The list big_list is a list of lists, each of the 'inner' lists
containing string elements that should be pieced together into a
single row.  This function makes a first iteration through at padding
pieces, and then a second at building the full command string.

Formatting is as follows:
+ Each line should be at most max_lw chars, except where unavoidable:
  - if it is one giant piece causing it, leave it where it is, unless
    the piece comes after a couple other items, in which case kick it to
    the next line
  - if it is a small piece sticking over, kick the piece to the next line
+ Each line after the initial one will be indented by nindent spaces.
+ The [1]th element in each line should be vertically aligned at horizontal
  spacing value harg1
  - unless the [0]th arg is too long, in which case it will sit further right
  - harg1 is set by being the min of the max_harg1 parameter and the 
    max char length of the end of the [0]th elements.
+ The comment_start can be applied to the start of each line: for example,
  for Python one might put comment_start='# ', then each line would be 
  commented (and space-padded by 1).

Returns the full command string, with continuation-of-line (COL)
characters, uniform vertical spacing, etc.

    '''

    if type(big_list) != list :
        print("** ERROR: need to input a list of lists")
        return ''

    if comment_start :
        if type(comment_start) != str :
            print("** ERROR: comment_start parameter needs to be a str")
            return ''
        
        # include this str in line width spacing considerations.
        ncomm = len(comment_start)
        max_lw-= ncomm
    
    nbig   = len(big_list)
    indent = nindent * ' '
    max_lw-= 1                         # bc of COL char

    # ------------ pad each row/sublist item appropriately ------------
    # + indent [0] items
    # + figure out the appropriate vertical spacing for any [1]st args
    # + apply padding to [0]th items for [1]st arg vert spacing
    # + we pad here, so we know how much horizontal spacing each element 
    #   takes

    max_harg0 = -1
    for i in range(1, nbig):
        # [0]th element gets indented and double padded (easier to read)
        big_list[i][0] = indent + big_list[i][0] + '  '

        # work to calc further reach of [0]th elements
        if len(big_list[i][0]) > max_harg0 :
            max_harg0 = len(big_list[i][0])

        # add a space to each remaining arg
        ilen = len(big_list[i])
        for j in range(1, ilen):
            big_list[i][j] = big_list[i][j] + ' '

    # the vertical spacing for all possible [1]st elements
    harg1     = min(max_harg1, max_harg0)
    # when kicking an element to the next line, how to pad it
    harg1_ind = max(nindent, harg1) * ' '

    # pad each row's [0]th arg for vert spacing of any [1]st args
    for j in range(1, nbig):
        big_list[j][0] = '''{text:<{harg1}s}'''.format(text=big_list[j][0],
                                                       harg1=harg1)

    # ----------------------- make output str --------------------------

    # Start with the [0][0]th element, which should be the prog name.
    # The cheese stands alone in this row
    ostr = '''{text:<{lw}s}'''.format(text=big_list[0][0], lw=max_lw)

    # loop over each sub_list
    for j in range(1, nbig):
        jlist = big_list[j]
        ostr += '''\\\n'''

        # start this row's/line's string, and keep running length tally
        row  = '''{text}'''.format(text=jlist[0])
        nrow = len(row)

        # loop over the items in the sub_list (skipping the [0th])
        for k in range(1, len(jlist)):
            if nrow + len(jlist[k]) > max_lw and \
               (len(harg1_ind + jlist[k]) < max_lw or k>1) :
                # Looks like we should put this item into a new
                # row. So, finish the current row...
                ostr += '''{text:<{lw}s}'''.format( text=row, lw=max_lw )
                ostr += '''\\\n'''
                # ... and then start building the next one
                row   = '''{pad}{text:<s}'''.format( pad=harg1_ind,
                                                     text=jlist[k] )
            elif k > 2 and len(jlist[k]) > 1 and \
                 jlist[k][0] == '-' and \
                 not(jlist[k][1].isdigit() or jlist[k][1] == '.') :
                # looks like the sub_list itself contains a list of
                # opts, so finish the current row..., just like above
                ostr += '''{text:<{lw}s}'''.format( text=row, lw=max_lw )
                ostr += '''\\\n'''
                # ... and then start building the next one
                row   = '''{pad}{text:<s}'''.format( pad=harg1_ind,
                                                     text=jlist[k] )
            else:
                # Simply continue with current row
                row+= jlist[k]

            # length of current row
            nrow = len(row)

        # done with [j]th sublist
        ostr+= '''{text:<{lw}s}'''.format(text=row, lw=max_lw)

    # Have gone through all sublists; put in any initial comment the
    # user asked for
    if comment_start :
        ostr = comment_start + ostr.replace('\n', '\n' + comment_start)

    # remove any trailing whitespace
    ostr = ostr.rstrip()

    return ostr

# -------------------------------------------------------------------------

# A supplementary function
def quick_check_argnum( sss1, sss2, verb=AD['verb'] ):
    '''A quick check for the number of pieces in two commands, ignoring
whitespace and COL chars.  Checks for the respective number of
non-whitespace 'islands', l1 and l2, respectively

    Parameters
    ----------
    sss1          :(str) command 1
    sss2          :(str) command 2

    Return
    ------
    n_diff       :(int) difference in length, l1-l2. So, returning 0 means
                  no difference, and anything else means 'is different' 
                  somehow

    '''

    if type(sss1) != str and type(sss2) != str :
        print("** ERROR: need ss1 and ss2 to be strings")
        sys.exit(3)

    l1    = len((' '.join(sss1.split('\\'))).split())
    l2    = len((' '.join(sss2.split('\\'))).split())
    ndiff = l1 - l2

    if verb :
        print("++ num args in sss1 : {}".format(l1))
        print("++ num args in sss2 : {}".format(l2))
        print("++ num different    : {}".format(ndiff))

    return ndiff

# -------------------------------------------------------------------------

# The main primary command to take a not-fancily-formatted str command
# and go through the couple steps to turn it into one
def afni_niceify_cmd_str( sss, big_list=None,
                          nindent=AD['nindent'], max_lw=AD['max_lw'],
                          list_cmd_args=AD['list_cmd_args'],
                          max_harg1=AD['max_harg1'],
                          comment_start=AD['comment_start'],
                          quote_pair_list=AD['quote_pair_list'], 
                          maxcount=AD['maxcount'],
                          verb=AD['verb'] ):
    '''Take a str sss that represents some command, and try to 'niceify'
    it for reading: one line per opt+arg(s) set; split long opt+arg
    lines appropriately; don't split text parts between pairs of
    quotes; indent the inputs after the program name; use horizontally
    aligned continuation-of-line chars whenever possible; use
    horizontally alignment for the [1]st items in each line, too (for
    ease of reading arguments after options).

    ***Note to self: add something about long COL characters at end of line?

    Parameters 
    ----------
    sss              : (str) a string, namely a command that would likely
                       be easier to read in multi-lined format
    big_list         : (list of lists) a pre-build list of lists, so that 
                       sss does not need to be parsed; this mainly exists
                       if another function has done the hard work of 
                       separating a command call into options and args
                       (e.g., in afni_proc.py). *At present*, sss will be 
                       ignored, and no checking will occur.
    nindent          : (int) number of spaces to indent each row (after the
                       [0]th
    max_lw           : (int) max line width to aim for (some text chunks may
                       stick over, like long file path names)
    list_cmd_args    : (list of str) explicit list of args to use for 
                       splitting the argv up line by line; if none is given,
                       which is the default scenario, then the program
                       aims to guess on its own
    max_harg1        : (int) max number of spaces for starting arg [1] in any
                       output command (unless arg0 sticks out further), for 
                       visual alignment.  If the longest [0]th arg in a line
                       is less than this, then use that horizontal spacing.
    comment_start    : (str) can prepent a comment char to the start of each
                       line if desired; e.g., for Python would probably use
                       '# ' here 
    quote_pair_list  : (list of str/char) list of characters to search for
                       in pairs, within which we don't split text, like 
                       quotes. 
                       NB: at present, each 'quote' is assumed to be a single
                       char
    maxcount         : (int) guard against infinite loops;
                       probably don't need to change
    verb             : (int) verbosity level

    Return 
    ------
    is_diff          : (int) a quick check value for whether some piece of 
                       the input sss might have gotten lost (or multiplied)
                       in translation; is_diff=0 means no difference (so 
                       the new str might be safe!), while any nonzero value
                       means something bad might have happened.  The value
                       is the number of pieces of str_nice minus that of 
                       sss.
    str_nice         : (str) the hopefully-nicely-formatted output version
                       of sss

    '''

    do_check = False

    if big_list == None :
        if type(sss) != str:
            print("** ERROR: need sss to be a string")
            return 1, ''

        big_list = listify_argv_str( sss, 
                                     list_cmd_args=list_cmd_args,
                                     quote_pair_list=quote_pair_list,
                                     maxcount=maxcount )
        do_check = True
        print('-'*80)
        print(big_list)
        print('-'*80)
    else:
        if verb :
            print("++ A parsed list of options has already been created.")

    str_nice = pad_argv_list_elements( big_list,
                                       nindent=nindent, 
                                       max_lw=max_lw,
                                       max_harg1=max_harg1,
                                       comment_start=comment_start )

    if do_check :
        # a quick check, as advertised
        is_diff = quick_check_argnum(str_nice, sss, verb=verb)
    else:
        is_diff = 0

    return is_diff, str_nice
    

if __name__ == "__main__" :
    band80 = 80 * '-'

    test_01 = '''abc "def" ' geh" ijk lmn opq "rst "uvw xyz \'''' 

    test_02 = '''afni_proc.py                                                 \
                 -subj_id subj123                                          \
                 -dsets epi_run1+orig.HEAD   epi_run2+orig.HEAD   epi_run3+orig.HEAD   epi_run4+orig.HEAD   epi_run5+orig.HEAD                                 \
                 -copy_anat anat+orig                                      \
                 -blocks despike tshift align tlrc volreg blur mask scale  regress                                               \
                 -tcat_remove_first_trs 3                                  \
                 -tlrc_base /some/very/loooooooooooooooooooooong_path_name/to/the/TEMPLATE.nii.gz \
                 -tlrc_NL_warp                                             \
                 -volreg_align_to MIN_OUTLIER                              \
                 -volreg_align_e2a                                         \
                 -volreg_tlrc_warp                                         \
                 -mask_epi_anat yes                                        \
                 -blur_size 4                                              \
                 -regress_censor_motion 0.2                                \
                 -regress_censor_outliers 0.05                             \
                 -regress_bandpass 0.01 0.1                                \
                 -regress_apply_mot_types demean deriv                     \
                 -regress_est_blur_epits                                   \
                 -regress_est_blur_errts 
'''

    test_03 = '''
              afni_proc.py                                                \
                 -subj_id sb23.e7.esoteric                                \
                 -dsets sb23/epi_r??+orig.HEAD                            \
                 -blocks tshift align tlrc volreg blur mask scale regress \
                 -copy_anat sb23/sb23_mpra+orig                           \
                 -tcat_remove_first_trs 3                                 \
                 -align_opts_aea -cost lpc+ZZ                             \
                 -tlrc_base MNI152_T1_2009c+tlrc                          \
                 -tlrc_NL_warp                                            \
                 -volreg_align_to MIN_OUTLIER                             \
                 -volreg_align_e2a                                        \
                 -volreg_tlrc_warp                                        \
                 -mask_epi_anat yes                                       \
                 -blur_size 4                                             \
                 -blur_in_automask                                        \
                 -regress_stim_times sb23/stim_files/blk_times.*.1D       \
                 -regress_stim_types times times times AM2 AM2 AM2 times  \
                     times times                                          \
                 -regress_stim_labels tneg tpos tneu eneg epos eneu fneg  \
                     fpos fneu                                            \
                 -regress_basis_multi 'BLOCK(30,1)' 'TENT(0,45,16)'       \
                     'BLOCK(30,1)' 'BLOCK(30,1)' 'TENT(0,45,16)'          \
                     'BLOCK(30,1)' 'BLOCK(30,1)' 'TENT(0,45,16)'          \
                     'BLOCK(30,1)'                                        \
                 -regress_apply_mot_types demean deriv                    \
                 -regress_motion_per_run                                  \
                 -regress_censor_motion 0.3                               \
                 -regress_censor_outliers 0.1                             \
                 -regress_compute_fitts                                   \
                 -regress_opts_3dD -bout -gltsym 'SYM: +eneg -fneg'       \
                     -glt_label 1 eneg_vs_fneg -jobs 4                    \
                 -regress_run_clustsim no                                 \
                 -regress_est_blur_epits                                  \
                 -regress_est_blur_errts 


'''




    all_test = [ test_01,
                 test_02,
                 test_03,
    ]

    for i in range(len(all_test)):
        test = all_test[i]

        # run the test
        check, nice_test = afni_niceify_cmd_str(test)
    
        # display
        print("\n\n")
        print("TEST {}... BEFORE:".format(i))
        print(band80)
        print(test)
        print(band80)
        print("... and AFTER:")
        print(band80)
        print(nice_test)
        print(band80)


