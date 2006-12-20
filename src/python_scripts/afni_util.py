#!/usr/bin/env python

import sys, os, string
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
def quotize_list(list):
    if not list or len(list) < 1: return list

    # okay, we haven't yet escaped any existing quotes...

    # qlist = "[({* "
    newlist = []
    for string in list:
        if '[' in string or '(' in string or '{' in string or ' ' in string:
            newlist.append("'%s'" % string)
        else:
            newlist.append(string)

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
        print "failed to open 1D file %s" % filename
        return None

    if verb: print "+d opened file %s" % filename

    retmat = []
    lnum   = 0
    data = fp.read()
    fp.close()
    for line in data.splitlines():
        if 0 <= nlines <= lnum: break   # then stop early
        if line[0] == '#' or line[0] == '\0':
            if verb: print "skipping comment line: %s" % line
            continue
        retmat.append([])
        tokens = line.split()
        for tok in tokens:
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

