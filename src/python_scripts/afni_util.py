#!/usr/bin/env python

import sys, os, math
import afni_base as BASE

# this file contains various afni utilities   17 Nov 2006 [rickr]

def change_path_basename(orig, prefix, suffix):
    """given a path (leading directory or not) swap the trailing
       filename with the passed prefix and suffix

          e.g. C_P_B('my/dir/pickles.yummy','toast','.1D') --> 'my/dir/toast.1D' 
    """
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

def quotize_list(list, opt_prefix, skip_first=0):
    """given a list of text elements, return a new list where any existing
       quotes are escaped, and then if there are special characters, put the
       whole string in single quotes

       if the first character is '-', opt_prefix will be applied

       if skip_first, do not add initial prefix
    """
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

def args_as_command(args, prefix='', suffix=''):
    """given an argument list (such as argv), create a command string,
       including any prefix and/or suffix strings"""

    if len(args) < 1: return

    cstr = "%s %s" % (os.path.basename(args[0]),
                            ' '.join(quotize_list(args[1:],'')))
    fstr = add_line_wrappers('%s%s%s' % (prefix,cstr,suffix))

    return fstr

def show_args_as_command(args, note='command:'):
     """print the given argument list as a command
        (this allows users to see wildcard expansions, for example)"""

     print args_as_command(args,
     "----------------------------------------------------------------------\n"
     "%s\n\n  " % note,
     "\n----------------------------------------------------------------------"
     )

def uniq_list_as_dsets(dsets, showerr=0):
    """given a list of text elements, create a list of afni_name elements,
       and check for unique prefixes"""

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


def list_to_datasets(words):
    """given a list, return the list of afni_name elements
         - the list can include wildcarding
         - they must be valid names of existing datasets
         - return None on error"""

    import glob # local, unless used elsewhere
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


def basis_has_known_response(basis, warn=0):
    """given a string, if the prefix is either GAM or BLOCK, then the basis
       function has a known response curve

       if warn, warn users about any basis function peculiarities"""
    if not basis: return 0

    if warn and basis == 'dmBLOCK':
        print '** basis function is dmBLOCK  ==>  script must be edited'
        print '   --> please change -stim_times to either'
        print '        -stim_times_AM1 or -stim_times_AM2'
        print '   (please mention this on the AFNI message board)'

    if basis[0:3] == 'GAM' or basis[0:5] == 'BLOCK': return 1
    else:                                            return 0

def get_default_polort(tr, reps):
    """compute a default polort, as done in 3dDeconvolve
       1+floor(time/150), time in seconds"""

    if tr <= 0 or reps <= 0:
        print "** cannot guess polort from tr = %f, reps = %d" % (tr,reps)
        return 2        # return some default
    run_time = tr * reps
    return 1+math.floor(run_time/150.0)

def get_typed_dset_attr_list(dset, attr, atype, verb=1):
    """given an AFNI dataset, return err (0=success), [typed attr list]

       dset  : (string) name of afni dataset
       attr  : (string) attribute name
       atype : (string) return type of attribute list
       verb  : verbose level"""

    alist = BASE.read_attribute(dset, attr)
    if alist == None:
        print "** GTDAL: failed to read dset attr %s, dset = %s" % (attr,dset)
        return 1, []

    err = 0
    try: result = [atype(val) for val in alist]
    except:
        print "** GTDAL: failed to convert attr %s to type %s for dset %s" % \
              (attr, atype, dset)
        err = 1
        result = []

    return err, result

def get_truncated_grid_dim(dset, verb=1):
    """return a new (isotropic) grid dimension based on the current grid
       - given md = min(DELTAS)
             if md >= 2.0: return md: truncated to int: floor(md)
             else:         return md: truncated to 2 significant bits
       - return <= 0 on failure
    """
    err, dims = get_typed_dset_attr_list(dset, 'DELTA', float)
    if err: return -1
    if len(dims) < 1: return -1
    for ind in range(len(dims)):
        dims[ind] = abs(dims[ind])
    md = min(dims)
    if md >= 2.0: return math.floor(md)
    if md <= 0:
        print '** failed to get truncated grid dim from %s' % dims
        return 0

    return truncate_to_2_bits(md, verb)

def truncate_to_2_bits(val, verb):
    """truncate the real value to most significant 2 bits

       val is required to be positive but less than 2.0"""

    if val <= 0 or val > 2.0:
        print '** T22B: illegal val to truncate: %g' % val
        return 0.0

    # find mult s.t.  2 <= mult*val < 4
    log2 = math.log(2.0)
    l2 = math.ceil(1 - math.log(val)/log2)
    mult = round(math.exp(l2 * log2))
    val2 = val * mult
    if verb > 2: print '-- GTGD: mult*val = %g*%g = %g' % (mult,val,val2)

    # truncate and divide by mult
    return math.floor(val2)/mult
    
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
        print '-- dset %s : reps = %d, tr = %s%s' %(dset,reps,str(tr),unit_str)

    # and adjust TR
    if units == 77001: tr /= 1000.0

    return 0, reps, tr

# ----------------------------------------------------------------------
# begin matrix functions

def read_1D_file(filename, nlines = -1, verb = 1):
    """read a simple 1D file into a float matrix, and return the matrix
       - skip leading '#', return a 2D array of floats"""
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

def num_cols_1D(filename):
    """return the number of columns in a 1D file"""
    mat = read_1D_file(filename)
    if not mat or len(mat) == 0: return 0
    return len(mat[0])

def num_rows_1D(filename):
    """return the number of columns in a 1D file"""
    mat = read_1D_file(filename)
    if not mat: return 0
    return len(mat)

def max_dim_1D(filename):
    """return the larger of the number of rows or columns"""
    mat = read_1D_file(filename)
    if not mat: return 0
    rows = len(mat)
    cols = len(mat[0])
    if rows >= cols: return rows
    else:            return cols

def transpose(matrix):
    """transpose a 2D matrix, returning the new one"""
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

def decode_1D_ints(istr, verb=1, max=-1):
    """Decode a comma-delimited string of ints, ranges and A@B syntax,
       and AFNI-style sub-brick selectors (including A..B(C)).
       If the A..B format is used, and B=='$', then B gets 'max'.
       If the list is enclosed in [], <> or ##, strip those characters.
       - return a list of ints"""

    newstr = strip_list_brackets(istr, verb)
    slist = newstr.split(',')
    if len(slist) == 0:
        if verb > 1: print "-- empty 1D_ints from string '%s'" % istr
        return None
    elif verb > 3: print "-- decoding stripped list '%s'" % newstr
    ilist = []                  # init return list
    for s in slist:
        try:
            if s.find('@') >= 0:        # then expect "A@B"
                [N, val] = [n for n in s.split('@')]
                N = int(N)
                val = to_int_special(val, '$', max)
                ilist.extend([val for i in range(N)])
            elif s.find('..') >= 0:     # then expect "A..B"
                pos = s.find('..')
                if s.find('(', pos) > 0:    # look for "A..B(C)"
                   [v1, v2] = [n for n in s.split('..')]
                   v1 = to_int_special(v1, '$', max)
                   [v2, step] = v2.split('(')
                   v2 = to_int_special(v2, '$', max)
                   # have start and end values, get step
                   step, junk = step.split(')')
                   step = int(step)
                   if   step > 0: inc = 1
                   elif step < 0: inc = -1
                   else:
                        print "** decode: illegal step of 0 in '%s'" % istr
                        return None
                   ilist.extend([i for i in range(v1, v2+inc, step)])
                else:
                   [v1, v2] = [n for n in s.split('..')]
                   v1 = to_int_special(v1, '$', max)
                   v2 = to_int_special(v2, '$', max)
                   if v1 < v2 : step = 1
                   else:        step = -1
                   ilist.extend([i for i in range(v1, v2+step, step)])
            else:
                ilist.extend([int(s)])
        except:
            print "** cannot decode_1D '%s' in '%s'" % (s, istr)
            return None
    if verb > 3: print '++ ilist: %s' % ilist
    del(newstr)
    return ilist

def to_int_special(cval, spec, sint):
   """basicall return int(cval), but if cval==spec, return sint

        cval: int as character string
        spec: special value as string
        sint: special value as int"""
   if cval == spec: return sint
   else:            return int(cval)

def strip_list_brackets(istr, verb=1):
   """strip of any [], {}, <> or ## surrounding this string
        - assume only one pair
        - allow the trailing character to be missing
      return the remaining string"""

   # strip any of these pairs
   for pairs in [ ['[',']'],  ['{','}'],  ['<','>'],  ['#','#'] ]:

      ind0 = istr.find(pairs[0])
      if ind0 >= 0:
         ind1 = istr.find(pairs[1], ind0+1)
         if verb > 1: print '-- stripping %s%s at %d,%d in %s' % \
                            (pairs[0],pairs[1],ind0,ind1,istr)
         if ind1 > ind0: return istr[ind0+1:ind1]
         else:           return istr[ind0+1:]

   if verb > 2: print "-- nothing to strip from '%s'" % istr

   return istr

# ----------------------------------------------------------------------
# line wrapper functions

# add line wrappers ('\'), and align them all
def add_line_wrappers(commands, wrapstr='\\\n'):
    """wrap long lines with 'wrapstr' (probably '\\\n' or just '\n')
       if '\\\n', align all wrapstr strings"""
    new_cmd = ''
    posn = 0

    while needs_wrapper(commands, 78, posn):
            
        end = find_command_end(commands, posn)

        if not needs_wrapper(commands, 78, posn, end): # command is okay
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

def align_wrappers(command):
    """align all '\\\n' strings to be the largest offset
       from the previous '\n'"""

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

def insert_wrappers(command, start=0, end=-1, wstring='\\\n'):
    """insert any '\\' chars for the given command
         - insert between start and end positions
         - apply specified wrap string wstring
       return a new string, in any case"""

    if end < 0: end = len(command) - start - 1

    nfirst = num_leading_line_spaces(command,start,1) # note initial indent
    prefix = get_next_indentation(command,start,end)
    plen   = len(prefix)
    maxlen = 78
    newcmd = ''
    cur    = start
    # rewrite: create new command strings after each wrap     29 May 2009
    while needs_wrapper(command,maxlen,cur,end):
        endposn = command.find('\n',cur)
        if needs_wrapper(command,maxlen,cur,endposn):  # no change on this line

            lposn = find_last_space(command, cur, endposn, maxlen)

            # if the last space is farther in than next indent, wrap
            if nfirst+plen+cur < lposn:   # woohoo, wrap away (at lposn)
                newcmd = newcmd + command[cur:lposn+1] + wstring
                # modify command to add prefix, reset end and cur
                command = prefix + command[lposn+1:]
                end = end + plen - (lposn+1)
                cur = 0
                continue

        # no change:
        # either the line does not need wrapping, or there is no space to do it
        if endposn < 0: endposn = end     # there may not be a '\n'
        newcmd += command[cur:endposn+1]
        cur = endposn + 1

    if cur <= end: newcmd += command[cur:end+1]   # add remaining string

    return newcmd

def get_next_indentation(command,start=0,end=-1):
    """get any '#' plus leading spaces, from beginning or after first '\\\n'"""
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

def needs_wrapper(command, maxlen=78, start=0, end=-1):
    """does the current string need line wrappers

       a string needs wrapping if there are more than 78 characters between
       any previous newline, and the next newline, wrap, or end"""

    if end < 0: end_posn = len(command) - 1
    else:       end_posn = end

    cur_posn = start
    remain = end_posn - cur_posn
    while remain > maxlen:
        
        # find next '\\\n'
        posn = command.find('\\\n', cur_posn)
        if 0 <= posn-cur_posn <= maxlen: # adjust and continue
            cur_posn = posn + 2
            remain = end_posn - cur_posn
            continue

        # find next '\n'
        posn = command.find('\n', cur_posn)
        if 0 <= posn-cur_posn <= maxlen: # adjust and continue
            cur_posn = posn + 1
            remain = end_posn - cur_posn
            continue

        return 1

    return 0        # if we get here, line wrapping is not needed

def find_command_end(command, start=0):
    """find the next '\n' that is not preceeded by '\\', or return the
       last valid position (length-1)"""

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

def num_leading_line_spaces(istr,start,pound=0):
    """count the number of leading non-whitespace chars
       (newline chars are not be counted, as they end a line)
       if pound, skip any leading '#'"""

    length = len(istr)
    if start < 0: start = 0
    if length < 1 or length <= start: return 0
    posn = start
    if pound and istr[posn] == '#': posn += 1

    while posn < length and istr[posn].isspace() and istr[posn] != '\n':
        posn += 1

    if posn == length: return 0   # none found
    return posn-start             # index equals num spaces from start

def find_next_space(istr,start,skip_prefix=0):
    """find (index of) first space after start that isn't a newline
       (skip any leading indendation if skip_prefix is set)
       return -1 if none are found"""

    length = len(istr)
    index  = start
    if skip_prefix: index += num_leading_line_spaces(istr,start,1)
    
    while 1:
        if index >= length: break
        if istr[index] != '\n' and istr[index].isspace(): break
        index += 1

    if index >= length : return -1
    return index

def find_last_space(istr,start,end,max_len=-1,stretch=1):
    """find (index of) last space in current line range that isn't a newline
       if stretch and not found, search towards end
       return start-1 if none are found"""

    if end < 0: end = len(istr) - 1
    if max_len >= 0 and end-start >= max_len: index = start+max_len-1
    else:                                     index = end

    posn = index        # store current position in case of stretch
    
    while posn >= start and (istr[posn] == '\n' or not istr[posn].isspace()):
        posn -= 1

    if posn < start and stretch:       # then search towards end
        posn = index
        while posn <= end and (istr[posn] == '\n' or not istr[posn].isspace()):
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

def vals_are_constant(vlist, cval=0):
   """determine whether every value in vlist is equal to cval
      (if cval == None, use vlist[0])"""

   if cval == None: cval = vlist[0]

   for val in vlist:
      if val != cval: return 0
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

def float_list_string(vals, nchar=7, ndec=3, nspaces=2, mesg=''):
   """return a string to display the floats:
        vals    : the list of float values
        nchar   : [7] number of characters to display per float
        ndec    : [3] number of decimal places to print to
        nspaces : [2] number of spaces between each float
   """

   istr = mesg
   for val in vals: istr += '%*.*f%*s' % (nchar, ndec, val, nspaces, '')

   return istr

def gen_float_list_string(vals, mesg='', nchar=0):
   """mesg is printed first, if nchar>0, it is min char width"""

   istr = mesg

   if nchar > 0:
      for val in vals: istr += '%*g ' % (nchar, val)
   else:
      for val in vals:
         istr += '%g ' % val

   return istr

def int_list_string(ilist, mesg='', nchar=0):
   """like float list string, but use general printing
      (mesg is printed first, if nchar>0, it is min char width)"""

   istr = mesg

   if nchar > 0:
      for val in ilist: istr += '%*d ' % (nchar, val)
   else:
      for val in ilist:
         istr += '%d ' % val

   return istr

def is_valid_int_list(ldata, imin=0, imax=-1, whine=0):
   """check whether:
        o  ldata is a of type []
        o  values are of type int
        o  values are in within imin..imax (only if imin <= imax)
        o  if whine: complain on error
      return 1 on true, 0 on false"""

   if not ldata or type(ldata) != type([]):
      if whine: print "** not valid as a list: '%s'" % ldata

   for ind in range(len(ldata)):
      val = ldata[ind]
      if type(val) != type(0):
         if whine: print "** non-int value %d in int list (@ %d)" % (val,ind)
         return 0
      if imin <= imax: # then also test bounds
         if val < imin:
            if whine: print "** list value %d not in [%d,%d]" %(val,imin,imax)
            return 0
         elif val > imax:
            if whine: print "** list value %d not in [%d,%d]" %(val,imin,imax)
            return 0
   return 1

# ----------------------------------------------------------------------
# matematical functions

def loc_sum(vals):
   """in case 'sum' does not exist, such as on old machines"""

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
    """unbiased standard deviation (divide by len-1, not just len)"""

    length = len(data)
    if length <  2: return 0.0

    meanval = loc_sum(data)/float(length)
    # compute standard deviation
    ssq = 0.0
    for val in data: ssq += val*val
    val = (ssq - length*meanval*meanval)/(length-1.0)

    # watch for truncation artifact
    if val < 0.0 : return 0.0
    return math.sqrt(val)

def stdev(data):
    """(biased) standard deviation (divide by len, not len-1)"""

    length = len(data)
    if length <  2: return 0.0

    meanval = loc_sum(data)/float(length)
    # compute standard deviation
    ssq = 0.0
    for val in data: ssq += val*val
    val = (ssq - length*meanval*meanval)/length

    # watch for truncation artifact
    if val < 0.0 : return 0.0
    return math.sqrt(val)

def shuffle(vlist):
    """randomize the order of list elements, where each perumuation is
       equally likely

       - akin to RSFgen, but do it with equal probabilities
         (search for swap in [index,N-1], not in [0,N-1])
       - random.shuffle() cannot produce all possibilities, don't use it"""

    # if we need random elsewhere, maybe do it globally
    import random

    size = len(vlist)

    for index in range(size):
        # find random index in [index,n] = index+rand[0,n-index]
        # note: random() is in [0,1)
        i2 = index + int((size-index)*random.random())

        if i2 != index:         # if we want a new location, swap
            val = vlist[i2]
            vlist[i2] = vlist[index]
            vlist[index] = val

    return

