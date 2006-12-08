#!/usr/bin/env python

import sys, os, string
from option_list import *

# this file contains various afni utilities   17 Nov 2006 [rickr]


# given a path (leading directory or not) swap the trailing
# filename with the passed prefix and suffix
def change_path_basename(orig, prefix, suffix):
    if not orig or not prefix: return
    (head, tail) = os.path.split(orig)
    if head == '': return prefix
    return "%s/%s%s" % (head, prefix, suffix)


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


