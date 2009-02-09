#!/usr/bin/env python

import sys, os, string, glob, math
import afni_base as BASE

# this file contains various afni utilities   17 Nov 2006 [rickr]

# given a path (leading directory or not) swap the trailing
# filename with the passed prefix and suffix
def change_path_basename(orig, prefix, suffix):
    if not orig or not prefix: return
    (head, tail) = os.path.split(orig)
    if head == '': return "%s%s" % (prefix, suffix)
    return "%s/%s%s" % (head, prefix, suffix)

# write text to a file
def write_text_to_file(fname, text, mode='w', wrap=0, wrapstr='\n'):
    """write the given text to the given file
          fname   : file name to write (or append) to
          text    : text to write
          mode    : optional write mode 'w' or 'a' [default='w']
          wrap    : optional wrap flag [default=0]
          wrapstr : optional wrap string: if wrap, apply this string
    """

    if not text or not fname:
        print "** WTTF: missing text or filename"
        return

    if wrap: text = add_line_wrappers(text, warpstr)
    
    try:
        fp = open(fname, mode)
    except:
        print "** failed to open text file '%s' for writing" % fname
        return

    fp.write(text)
    fp.close()

# given a list of text elements, return a new list where any
# existing quotes are escaped, and then if there are special
# characters, put the whole string in single quotes
#
# if the first character is '-', opt_prefix will be applied
#
# if skip_first, do not add initial prefix
def quotize_list(list, opt_prefix, skip_first=0):
    if not list or len(list) < 1: return list

    # okay, we haven't yet escaped any existing quotes...

    # qlist = "[({* "
    newlist = []
    first = 1   # ugly, but easier for option processing
    for string in list:
        prefix = ''
        if skip_first and first:
            first = 0       # use current (empty) prefix
        else:
            if string[0] == '-': prefix = opt_prefix
        if '[' in string or '(' in string or '{' in string or ' ' in string:
            newlist.append("'%s%s'" % (prefix,string))
        else:
            newlist.append("%s%s" % (prefix,string))

    return newlist

# given an argument list (such as argv), create a command string,
# including any prefix and/or suffix
def args_as_command(args, prefix='', suffix=''):
    if len(args) < 1: return

    cstr = "%s %s" % (os.path.basename(args[0]),
                            ' '.join(quotize_list(args[1:],'')))
    fstr = add_line_wrappers('%s%s%s' % (prefix,cstr,suffix))

    return fstr

# print the given argument list as a command
# (this allows users to see wildcard expansions, for example)
def show_args_as_command(args, note='command:'):
  print args_as_command(args,
     "----------------------------------------------------------------------\n"
     "%s\n\n    " % note,
     "\n----------------------------------------------------------------------"
  )

# given a list of text elements, create a list of afni_name elements,
# and check for unique prefixes
def uniq_list_as_dsets(dsets, showerr=0):
    if not dsets or len(dsets) < 2: return 1

    # iterate over dsets, searching for matches
    uniq = 1
    for i1 in range(len(dsets)):
        for i2 in range(i1+1, len(dsets)):
            if dsets[i1].prefix == dsets[i2].prefix:
                uniq = 0
                break
        if not uniq: break

    if not uniq and showerr:
        print                                                               \
          "-----------------------------------------------------------\n"   \
          "** error: dataset names are not unique\n\n"                      \
          "   (#%d == #%d, '%s' == '%s')\n\n"                               \
          "   note: if using a wildcard, please specify a suffix,\n"        \
          "         otherwise datasets may be listed twice\n"               \
          "            e.g.  bad use:    ED_r*+orig*\n"                     \
          "            e.g.  good use:   ED_r*+orig.HEAD\n"                 \
          "-----------------------------------------------------------\n"   \
          % (i1+1, i2+1, dsets[i1].pve(), dsets[i2].pve())

    return uniq


# given a list, return the list of afni_name elements
# - the list can include wildcarding
# - they must be valid names of existing datasets
# - return None on error
def list_to_datasets(words):
    if not words or len(words) < 1: return []
    dsets = []
    wlist = []
    errs  = 0
    for word in words:
        glist = glob.glob(word)  # first, check for globbing
        if glist:
            glist.sort()
            wlist += glist
        else: wlist.append(word)
    # now process all words
    for word in wlist:
        dset = BASE.afni_name(word)
        if dset.exist():
            dsets.append(dset)
        else:
            print "** no dataset match for '%s'" % word
            errs = 1

    if errs:
        print # for separation
        return None
    return dsets


# given a string, if the prefix is either GAM or BLOCK, then the basis
# function has a known response curve
def basis_has_known_response(basis):
    if not basis: return 0
    if basis[0:3] == 'GAM' or basis[0:5] == 'BLOCK': return 1
    else:                                            return 0

# compute a default polort, as done in 3dDeconvolve
def get_default_polort(tr, reps):
    if tr <= 0 or reps <= 0:
        print "** cannot guess polort from tr = %f, reps = %d" % (tr,reps)
        return 2        # return some default
    run_time = tr * reps
    return 1+math.floor(run_time/150.0)

def get_dset_reps_tr(dset, verb=1):
    """given an AFNI dataset, return err, reps, tr

       err  = error code (0 = success, else failure)
       reps = number of TRs in the dataset
       tr   = length of TR, in seconds"""

    # store timing info in a list (to get reps and timing units)
    tinfo = BASE.read_attribute(dset, 'TAXIS_NUMS')
    if tinfo == None:
        print "** failed to find the number of TRs from dset '%s'" % dset
        return 1, None, None

    # look for the number of repetitions
    try: reps = int(tinfo[0])
    except:
        print "** reps '%s' is not an int in dset %s?" % (tinfo[0], dset)
        return 1, None, None
    if reps < 1:
        print "** invalid nreps (%d) for dset %s" % (reps, dset)
        return 1, None, None

    # note the units (either sec (77002) or ms (77001))
    try: units = int(tinfo[2])
    except: units = 77002
    if units != 77001 and units != 77002: units = 77002

    # now read the TR (and apply previous units)
    tinfo = BASE.read_attribute(dset, 'TAXIS_FLOATS')
    if tinfo == None:
        print "** failed to find the TR length from dset '%s'" % dset
        return 1, None, None
    try: tr = float(tinfo[1])
    except:
        print "** TR '%s' is not a float?" % tinfo[0]
        return 1, None, None

    if verb > 1:
        if units == 77001: unit_str = 'ms'
        else             : unit_str = 's'
        print '-- dset %s : reps = %d, tr = %s%s' % (dset,reps,str(tr),unit_str)

    # and adjust TR
    if units == 77001: tr /= 1000.0

    return 0, reps, tr

# ----------------------------------------------------------------------
# begin matrix functions

# read a simple 1D file into a float matrix, and return the matrix
def read_1D_file(filename, nlines = -1, verb = 1):
    """skip leading '#', return a 2D array of floats"""
    try:
        fp = open(filename, 'r')
    except:
        if verb >= 0: print "failed to open 1D file %s" % filename
        return None

    if verb > 1: print "+d opened file %s" % filename

    retmat = []
    lnum   = 0
    data = fp.read()
    fp.close()
    for line in data.splitlines():
        if 0 <= nlines <= lnum: break   # then stop early
        if not line:
            if verb > 0: print "skipping empty line:"
            continue
        if line[0] == '#' or line[0] == '\0':
            if verb > 0: print "skipping comment line: %s" % line
            continue
        retmat.append([])
        tokens = line.split()
        for tok in tokens:
            try: fval = float(tok)
            except:
                if verb >= 0:
                    print "found bad float on line %d: '%s'" % (lnum+1,tok)
                return None
            retmat[lnum].append(float(tok))

        if verb > 2: print "+d line %d, length %d" % (lnum, len(retmat[lnum]))

        lnum += 1

    return retmat

# return the number of columns in a 1D file
def num_cols_1D(filename):
    mat = read_1D_file(filename)
    if not mat or len(mat) == 0: return 0
    return len(mat[0])

# return the number of columns in a 1D file
def num_rows_1D(filename):
    mat = read_1D_file(filename)
    if not mat: return 0
    return len(mat)

# return the larger of the number of rows or columns
def max_dim_1D(filename):
    mat = read_1D_file(filename)
    if not mat: return 0
    rows = len(mat)
    cols = len(mat[0])
    if rows >= cols: return rows
    else:            return cols

# transpose a 2D matrix, returning the new one
def transpose(matrix):
    rows = len(matrix)
    cols = len(matrix[0])
    newmat = []
    for c in range(cols):
        newrow = []
        for r in range(rows):
            newrow.append(matrix[r][c])
        newmat.append(newrow)
    return newmat

# end matrix functions
# ----------------------------------------------------------------------
# index encode/decode functions

def encode_1D_ints(cols):
   """convert a list of columns to a ',' and '..' separated string"""
   if not cols: return ''
   if len(cols) < 1: return ''

   text = '%d' % cols[0]
   prev = cols[0]
   ind  = 1
   while ind < len(cols):
      ncontinue = consec_len(cols, ind-1) - 1
      if ncontinue <= 1:     # then no '..' continuation, use ','
         text = text + ', %d' % cols[ind]
         ind += 1
      else:
         text = text + '..%d' % cols[ind+ncontinue-1]
         ind += ncontinue

   return text

def consec_len(cols, start):
   """return the length of consecutive numbers - always at least 1"""
   prev = cols[start]
   length = len(cols)
   ind  = start
   for ind in range(start+1, length+1):
      if ind == length: break
      if cols[ind] != prev + 1:
         break
      prev = cols[ind]
   if ind == start:  length = 1
   else:             length = ind-start

   return length

def decode_1D_ints(str, verb=1, max=-1):
    """Decode a comma-delimited string of ints, ranges and A@B syntax,
       and AFNI-style sub-brick selectors (including A..B(C)).
       If the A..B format is used, and B=='$', then B gets 'max'.
       - return a list of ints"""
    slist = str.split(',')
    if len(slist) == 0:
        if verb > 1: print "-- empty 1D_ints from string '%s'" % str
        return None
    ilist = []                  # init return list
    for s in slist:
        try:
            if s.find('@') >= 0:        # then expect "A@B"
                [N, val] = [int(n) for n in s.split('@')]
                ilist.extend([val for i in range(N)])
            elif s.find('..') >= 0:     # then expect "A..B"
                pos = s.find('..')
                if s.find('(', pos) > 0:    # look for "A..B(C)"
                   [v1, v2] = [n for n in s.split('..')]
                   v1 = int(v1)
                   [v2, step] = v2.split('(')
                   if v2 == '$': v2 = max
                   else:         v2 = int(v2)
                   # have start and end values, get step
                   step, junk = step.split(')')
                   step = int(step)
                   ilist.extend([i for i in range(v1, v2+1, step)])
                else:
                   [v1, v2] = [n for n in s.split('..')]
                   v1 = int(v1)
                   if v2 == '$': v2 = max
                   else:         v2 = int(v2)
                   ilist.extend([i for i in range(v1, v2+1)])
            else:
                ilist.extend([int(s)])
        except:
            print "** cannot decode_1D '%s' in '%s'" % (s, str)
            return None
    return ilist

# ----------------------------------------------------------------------
# line wrapper functions

# add line wrappers ('\'), and align them all
def add_line_wrappers(commands, wrapstr='\\\n'):
    """wrap long lines with 'wrapstr' (probably '\\\n' or just '\n')
       if '\\\n', align all wrapstr strings"""
    new_cmd = ''
    posn = 0
    while needs_wrapper(commands, 79, posn):
            
        end = find_command_end(commands, posn)

        if not needs_wrapper(commands, 79, posn, end): # command is okay
            if end < 0: new_cmd = new_cmd + commands[posn:]
            else      : new_cmd = new_cmd + commands[posn:end+1]
            posn = end+1
            continue

        # command needs wrapping
        new_cmd += insert_wrappers(commands, posn, end, wstring=wrapstr)

        posn = end + 1     # else, update posn and continue

    result = new_cmd + commands[posn:]

    # wrappers are in, now align them
    if wrapstr == '\\\n': return align_wrappers(result)
    else:                 return result

# align all '\\\n' string to be the largest offset from the previous '\n'
def align_wrappers(command):

    # first, find the maximum offset
    posn = 0
    max  = -1
    while 1:
        next = command.find('\n',posn)
        if next < 0: break
        if next > posn and command[next-1] == '\\':  # check against max
            width = next - 1 - posn
            if width > max: max = width
        posn = next + 1 # look past it

    if max < 0: return command  # none found

    # repeat the previous loop, but adding appropriate spaces
    new_cmd = ''
    posn = 0
    while 1:
        next = command.find('\n',posn)
        if next < 0: break
        if next > posn and command[next-1] == '\\':  # check against max
            width = next - 1 - posn
            if width < max:     # then insert appropriate spaces
                new_cmd += command[posn:next-1] + ' '*(max-width) + '\\\n'
                posn = next + 1
                continue

        # just duplicate from the previous posn
        new_cmd += command[posn:next+1]
        posn = next + 1 # look past it

    if posn < len(command): new_cmd += command[posn:]

    return new_cmd

# insert any '\\' chars for the given command
#
# return a new string, in any case
def insert_wrappers(command, start=0, end=-1, wstring='\\\n'):
    if end < 0: end = len(command) - start - 1

    nfirst = num_leading_line_spaces(command,start,1) # note initial indent
    prefix = get_next_indentation(command,start,end)
    plen   = len(prefix)
    maxlen = 78
    newcmd = ''
    cur    = start
    while needs_wrapper(command,maxlen,cur,end):
        endposn = command.find('\n',cur)
        if needs_wrapper(command,maxlen,cur,endposn):  # no change on this line

            lposn = find_last_space(command, cur, endposn, maxlen)
            maxlen = 78 - plen

            if nfirst+cur < lposn:   # woohoo, wrap away (at lposn)
                newcmd = newcmd + command[cur:lposn+1] + wstring + prefix
                cur = lposn+1
                continue

        # no change:
        # either the line does not need wrapping, or there is no space to do it
        if endposn < 0: endposn = end     # there may not be a '\n'
        newcmd += command[cur:endposn+1]
        cur = endposn + 1
        maxlen = 78 - plen

    if cur <= end: newcmd += command[cur:end+1]   # add remaining string

    return newcmd

# get any '#', plus leading spaces, from beginning or after first '\\\n'
def get_next_indentation(command,start=0,end=-1):
    if end < 0: end = len(command) - start - 1

    spaces = num_leading_line_spaces(command,start,1)
    prefix = command[start:start+spaces]+'    ' # grab those spaces, plus 4
    # now check for an indention prefix
    posn = command.find('\\\n', start)
    if posn >= 0:
        spaces = num_leading_line_spaces(command,posn+2,1)
        if posn > start and spaces >= 2:
            prefix = command[posn+2:posn+2+spaces] # grab those spaces

    return prefix

# does the current string need line wrappers
# 
# a string needs wrapping if there are more than 79 characters between
# any previous newline, and the next newline, wrap, or end
def needs_wrapper(command, max=79, start=0, end=-1):
    if end < 0: end_posn = len(command) - 1
    else:       end_posn = end

    cur_posn = start
    remain = end_posn - cur_posn
    while remain > max:
        
        # find next '\\\n'
        posn = command.find('\\\n', cur_posn)
        if 0 <= posn-cur_posn < max: # adjust and continue
            cur_posn = posn + 2
            remain = end_posn - cur_posn
            continue

        # find next '\n'
        posn = command.find('\n', cur_posn)
        if 0 <= posn-cur_posn < max: # adjust and continue
            cur_posn = posn + 1
            remain = end_posn - cur_posn
            continue

        return 1

    return 0        # if we get here, line wrapping is not needed

# find the next '\n' that is not preceeded by '\\', or return the
# last valid position (length-1)
def find_command_end(command, start=0):
    length = len(command)
    end = start-1
    while 1:
        start = end + 1
        end = command.find('\n',start)

        if end < 0: return length-1   # not in command
        elif end > start and command[end-1] == '\\':
            if length > end+1 and command[start] == '#'   \
                              and command[end+1] != '#':
                return end      # since comments cannot wrap
            else: continue 
        return end              # found

# count the number of leading non-whitespace chars
# (newline chars are not be counted, as they end a line)
# if pound, skip any leading '#'
def num_leading_line_spaces(str,start,pound=0):
    length = len(str)
    if start < 0: start = 0
    if length < 1 or length <= start: return 0
    posn = start
    if pound and str[posn] == '#': posn += 1

    while posn < length and str[posn].isspace() and str[posn] != '\n':
        posn += 1

    if posn == length: return 0   # none found
    return posn-start             # index equals num spaces from start

# find (index of) first space after start that isn't a newline
# (skip any leading indendation if skip_prefix is set)
# return -1 if none are found
def find_next_space(str,start,skip_prefix=0):
    length = len(str)
    index  = start
    if skip_prefix: index += num_leading_line_spaces(str,start,1)
    
    while 1:
        if index >= length: break
        if str[index] != '\n' and str[index].isspace(): break
        index += 1

    if index >= length : return -1
    return index

# find (index of) last space in current line range that isn't a newline
# if stretch and not found, search towards end
# return start-1 if none are found
def find_last_space(str,start,end,max_len=-1,stretch=1):
    if end < 0: end = len(str) - 1
    if max_len >= 0 and end-start >= max_len: index = start+max_len-1
    else:                                     index = end

    posn = index        # store current position in case of stretch
    
    while posn >= start and (str[posn] == '\n' or not str[posn].isspace()):
        posn -= 1

    if posn < start and stretch:       # then search towards end
        posn = index
        while posn < end and (str[posn] == '\n' or not str[posn].isspace()):
            posn += 1
        if posn > end: posn = start-1 # still failed

    return posn   # for either success or failure

# end line_wrapper functions
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# other functions

# 17 May, 2008 [rickr]
def vals_are_multiples(num, vals, digits=4):
    """decide whether every value in 'vals' is a multiple of 'num'
       (vals can be a single float or a list of them)

       Note, 'digits' can be used to specify the number of digits of accuracy
       in the test to see if a ratio is integral.  For example:
           vals_are_multiples(1.1, 3.3001, 3) == 1
           vals_are_multiples(1.1, 3.3001, 4) == 0

       return 1 if true, 0 otherwise (including error)"""

    if num == 0.0: return 0

    try:
        l = len(vals)
        vlist = vals
    except:
        vlist = [vals]

    for val in vlist:
        rat = val/num
        rem = rat - int(rat)

        if round(rem,digits) != 0.0: return 0

    return 1

def lists_are_same(list1, list2):
   """return 1 if the lists have identical values, else 0"""
   if not list1 and not list2: return 1
   if not list1: return 0
   if not list2: return 0
   if len(list1) != len(list2): return 0

   for ind in range(len(list1)):
      if list1[ind] != list2[ind]: return 0

   return 1

def float_list_string(vals, nchar=7, ndec=3, nspaces=2):
   """return a string to display the floats:
        vals    : the list of float values
        nchar   : [7] number of characters to display per float
        ndec    : [3] number of decimal places to print to
        nspaces : [2] number of spaces between each float
   """

   str = ''
   for val in vals: str += '%*.*f%*s' % (nchar, ndec, val, nspaces, '')

   return str

# ----------------------------------------------------------------------
# matematical functions

# in case 'sum' does not exist, such as on old machines
def loc_sum(vals):
   try: tot = sum(vals)
   except:
      tot = 0
      for val in vals: tot += val
   return tot

def min_mean_max_stdev(data):
    """return 4 values for data: min, max, mean, stdev (unbiased)"""
    if not data: return 0,0,0,0
    length = len(data)
    if length <  1: return 0,0,0,0
    if length == 1: return data[0], data[0], data[0], 0.0

    minval  = min(data)
    maxval  = max(data)
    meanval = loc_sum(data)/float(length)

    return minval, meanval, maxval, stdev_ub(data)

def stdev_ub(data):
    """unbiased standard deviation"""
    length = len(data)
    if length <  2: return 0.0

    meanval = loc_sum(data)/float(length)
    # compute standard deviation
    ssq = 0.0
    for val in data: ssq += val*val
    return math.sqrt((ssq - length*meanval*meanval)/(length-1.0))

def stdev(data):
    """(biased) standard deviation"""
    length = len(data)
    if length <  2: return 0.0

    meanval = loc_sum(data)/float(length)
    # compute standard deviation
    ssq = 0.0
    for val in data: ssq += val*val
    return math.sqrt((ssq - length*meanval*meanval)/length)


