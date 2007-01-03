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

