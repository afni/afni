#!/usr/bin/env python

import sys, os, string, glob
import afni_base

# this file contains various afni utilities   17 Nov 2006 [rickr]


# given a path (leading directory or not) swap the trailing
# filename with the passed prefix and suffix
def change_path_basename(orig, prefix, suffix):
    if not orig or not prefix: return
    (head, tail) = os.path.split(orig)
    if head == '': return "%s%s" % (prefix, suffix)
    return "%s/%s%s" % (head, prefix, suffix)

# given a list of text elements, return a new list where any
# existing quotes are escaped, and then if there are special
# characters, put the whole string in single quotes
#
# if the first character is '-', opt_prefix will be applied
#
# if skip_first, do not add initial prefix
def quotize_list(list, opt_prefix, skip_first=False):
    if not list or len(list) < 1: return list

    # okay, we haven't yet escaped any existing quotes...

    # qlist = "[({* "
    newlist = []
    first = True   # ugly, but easier for option processing
    for string in list:
        prefix = ''
        if skip_first and first:
            first = False       # use current (empty) prefix
        else:
            if string[0] == '-': prefix = opt_prefix
        if '[' in string or '(' in string or '{' in string or ' ' in string:
            newlist.append("'%s%s'" % (prefix,string))
        else:
            newlist.append("%s%s" % (prefix,string))

    return newlist

# given a list of text elements, create a list of afni_name elements,
# and check for unique prefixes
def uniq_list_as_dsets(dsets, showerr=False):
    if not dsets or len(dsets) < 2: return True

    # iterate over dsets, searching for matches
    uniq = True
    for i1 in range(len(dsets)):
        for i2 in range(i1+1, len(dsets)):
            if dsets[i1].prefix == dsets[i2].prefix:
                uniq = False
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
    errs  = False
    for word in words:
        glist = glob.glob(word)  # first, check for globbing
        if glist:
            glist.sort()
            wlist += glist
        else: wlist.append(word)
    # now process all words
    for word in wlist:
        dset = afni_base.afni_name(word)
        if dset.exist():
            dsets.append(dset)
        else:
            print "** no dataset match for '%s'" % word
            errs = True

    if errs:
        print # for separation
        return None
    return dsets


# given a string, if the prefix is either GAM or BLOCK, then the basis
# function has a known response curve
def basis_has_known_response(basis):
    if not basis: return False
    if basis[0:3] == 'GAM' or basis[0:5] == 'BLOCK': return True
    else:                                            return False

# ----------------------------------------------------------------------
# begin matrix functions

# read a simple 1D file into a float matrix, and return the matrix
def read_1D_file(filename, nlines = -1, verb = 0):
    """skip leading '#', return a 2D array of floats"""
    try:
        fp = open(filename, 'r')
    except:
        if verb >= 0: print "failed to open 1D file %s" % filename
        return None

    if verb > 0: print "+d opened file %s" % filename

    retmat = []
    lnum   = 0
    data = fp.read()
    fp.close()
    for line in data.splitlines():
        if 0 <= nlines <= lnum: break   # then stop early
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

        if verb > 1: print "+d line %d, length %d" % (lnum, len(retmat[lnum]))

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
# line wrapper functions

# add line wrappers ('\'), and align them all
def add_line_wrappers(commands):
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
        new_cmd += insert_wrappers(commands, posn, end)
        posn = end + 1

    # wrappers are in, now align them
    return align_wrappers(new_cmd + commands[posn:])

# align all '\\\n' string to be the largest offset from the previous '\n'
def align_wrappers(command):

    # first, find the maximum offset
    posn = 0
    max  = -1
    while True:
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
    while True:
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
def insert_wrappers(command, start=0, end=-1):
    if end < 0: end = len(command) - start - 1

    nfirst = num_leading_line_spaces(command,start,True) # note initial indent
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

            if nfirst < lposn:   # woohoo, wrap away (at lposn)
                newcmd = newcmd + command[cur:lposn+1] + '\\\n' + prefix
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

    spaces = num_leading_line_spaces(command,start,True)
    prefix = command[start:start+spaces]+'    ' # grab those spaces, plus 4
    # now check for an indention prefix
    posn = command.find('\\\n', start)
    if posn >= 0:
        spaces = num_leading_line_spaces(command,posn+2,True)
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

        return True

    return False        # if we get here, line wrapping is not needed

# find the next '\n' that is not preceeded by '\\', or return -1
def find_command_end(command, start=0):
    length = len(command) - start
    end = start-1
    while True:
        start = end + 1
        end = command.find('\n',start)

        if end < 0: return -1   # not in command
        elif end > start and command[end-1] == '\\':
            if length > end+1 and command[start] == '#'   \
                              and command[end+1] != '#':
                return end      # since comments cannot wrap
            else: continue 
        return end              # found

# count the number of leading non-whitespace chars
# (newline chars are not be counted, as they end a line)
# if pound, skip any leading '#'
def num_leading_line_spaces(str,start,pound=False):
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
def find_next_space(str,start,skip_prefix=False):
    length = len(str)
    index  = start
    if skip_prefix: index += num_leading_line_spaces(str,start,True)
    
    while True:
        if index >= length: break
        if str[index] != '\n' and str[index].isspace(): break
        index += 1

    if index >= length : return -1
    return index

# find (index of) last space after before end that isn't a newline
# return -1 if none are found
def find_last_space(str,start,end,max_len=-1):
    if end < 0: end = len(str) - 1
    if max_len >= 0 and end-start >= max_len: index = start+max_len-1
    else:                                     index = end
    
    while index >= start and (str[index] == '\n' or not str[index].isspace()):
        index -= 1

    return index   # for either success or failure



# end line_wrapper functions
# ----------------------------------------------------------------------
