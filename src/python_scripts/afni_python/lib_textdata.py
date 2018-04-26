#!/usr/bin/env python

# python3 status: started

# lib_textdata.py : I/O utilities for 1D/timing/married data files

# created 16 Aug, 2010 [rickr]

import sys, os, math

def write_1D_file(data, filename, as_rows=0, space=' '):
    """data can be 1D or 2D array of floats, write one index per line
       If 2D data, each element is treated as a column (unless as_rows).

       For tabbed output, consider space = '\t'.

       return 0 on success"""

    if type(data) != type([]):
        print("** write_1D_file, invalid type: %s" % type(data))
        return 1

    try: fp = open(filename, 'w')
    except:
        print("** failed to open '%s' for writing 1D" % filename)
        return 1

    if len(data) == 0:  # an empty file then
        fp.close()
        return 0

    if type(data[0]) == type([]):       # multi-column
        if as_rows:
            for row in data:
                fp.write("%s\n" % space.join(["%g" % val for val in row]))
        else:
            nt = len(data[0])
            for col in data:
                if len(col) != nt:
                    print('** write_1D_file: columns not of equal lengths')
                    return 1

            for ind in range(nt):
                fp.write("%s\n" % space.join(["%g"%col[ind] for col in data]))

    else:                               # single column
        for val in data:
            fp.write('%g\n' % val)

    fp.close()

    return 0

def read_1D_file(filename, nlines=-1, marry_ok=0, verb=1):
    """read a simple 1D file into a float matrix, and return the matrix
       - skip leading '#', return a 2D array of floats"""


    data, clines, alist = read_married_file(filename, nlines=nlines, verb=verb)
    if data == None:
       print('** failed to read 1D file %s' % filename)
       return None

    if married_type(data) and not marry_ok:
       print("** file %s is in married format" % filename)
       return None

    if not data_is_rect(data):
       print("** file %s is not rectangular" % filename)
       return None

    # return just the initial entries
    retmat = [[val[0] for val in row] for row in data]

    del(data)
    del(clines)
    del(alist)

    return retmat

def read_data_file(filename, nlines=-1, marry_ok=0, verb=1):
    """read a numerical text file (1D or timing, say) into a float matrix,
       and return the matrix and comment lines
       - skip leading '#', return a 2D array of floats and 1D array of text"""

    data, clines, alist = read_married_file(filename, nlines=nlines, verb=verb)
    if data == None:
       if verb > 0: print('** failed to read text data file %s' % filename)
       return None, None

    if married_type(data) and not marry_ok:
       if verb > 0: print("** file %s is in married format" % filename)
       return None, None

    # return just the initial entries
    retmat = [[val[0] for val in row] for row in data]

    del(data)
    del(alist)

    return retmat, clines

def read_married_file(filename, nlines = -1, verb = 1):
    """akin to read_data_file, but instead of returning a 2D array of floats,
       return a 2D array of married elements, where each element is of the
       form: [time, [modulators], duration]
          - all numbers are returned as floats
          - time will always be set
          - modulators may be empty (see common stim_times format)
          - duration may be None (see common stim_times format)
       The expected format of file element is:
          time*mod1*mod2*...*modN:duration
       Should we allow different separators?

       return
          - married data matrix
          - comment list
          - astcounts list (count of '*' characters per data line)
    """

    if filename == '-' or filename == 'stdin':
        fp = sys.stdin
    else:
       try: fp = open(filename, 'r')
       except:
           if verb > 0: print("** failed to open 1D file '%s'" % filename)
           return None, None, None

    if verb > 1: print("+d reading file %s" % filename)

    retmat = []
    clines = []
    astcounts = []
    lnum   = 0
    data = fp.read()
    fp.close()
    all_lines = data.splitlines()
    for lind in range(len(all_lines)):
        full_line = all_lines[lind]
        if 0 <= nlines <= lnum: break   # then stop early

        # nuke anything after first comment, proceed with line
        line = full_line
        cind = full_line.find('#')
        if cind >= 0:
            line = full_line[0:cind]
            cline = full_line[cind:]
            clines.append(cline)
            if verb > 2: print("-- have comment line: %s" % cline)

        # check for lines to skip
        if not line:
            if verb > 3: print("... skipping empty line:")
            continue
        if line.isspace():
            if verb > 3: print("... skipping blank line:")
            continue
        if line[0] == '\0':
            if verb > 2: print("have comment line: %s" % line)
            clines.append(line)
            continue

        # process the line
        if verb > 3: print('-- processing line #%d: %s' % (lind, line))
        rv, mlist, acount = process_one_data_line(line, verb)
        if rv: return None, None, None
        if verb > 4: print('++ mlist = %s' % mlist)

        retmat.append(mlist)    # even if empty
        astcounts.append(acount)

        if verb > 3: print("+d line %d, length %d" % (lnum, len(retmat[lnum])))
        lnum += 1

    # now just check for consistency
    if not married_mat_is_consistent(retmat, filename, verb=verb):
       return None, None, None

    return retmat, clines, astcounts

def married_mat_is_consistent(mmat, fname, verb=1):
    """just check for consistency: same number of modulators and
                                   if one duration, all have one
    """
    ind = 0
    ttok = None
    while ind < len(mmat):
      if len(mmat[ind]) > 0:
         ttok = mmat[ind][0]
         break
      ind += 1
    if ttok: # have something to test, else empty mmat
      modlen = len(ttok[1])
      moddur = ttok[2] > 0      # have duration
      for lind in range(len(mmat)):
         line = mmat[lind]
         for entry in line:
            if len(entry[1]) != modlen:
               if verb:
                  print("** married file %s, line %d: inconsistent num" \
                        " modulators: %d vs %d"                         \
                        % (fname, lind, modlen, len(entry[1])))
               return 0
            if moddur and entry[2]<=0:
               if verb:
                  print("** married file %s, line %d:"                  \
                        " inconsistent use of duration: dur (%g) <= 0"  \
                        % (fname, lind, entry[2]))
            if not moddur and entry[2]>0:
               if verb:
                  print("** married file %s, line %d:"                   \
                        " inconsistent use of duration: should be zero"  \
                        % (fname, lind))
               return 0

    return 1 # yay

def process_one_data_line(line, verb=1):
   """return an array of [time, [modulators], duration] elements
        - lines should not be empty (or comments)
        - skip '*' tokens (should be only way to get empty line)
        - look for married elements

      The returned acount is the number of '*'/',' found on this line.

      If any tokens are married, they should all have the same format.

      return result, [time tokens] [acounts] (result = 0 if OK, 1 on error)
   """

   if not line:
      print('** PODL: should not have empty line')
      return 1, [], 0
   if line.isspace():
      print('** PODL: should not have blank line')
      return 1, [], 0
   if line[0] == '\0' or line[0] == '#':
      print('** PODL: should not have comment line')
      return 1, [], 0

   tokens   = line.split()
   inc_warn = 1         # do not warn of inconsistency more than once
   res_sep  = None      # separator list result
   res_list = []        # result list
   acount = 0
   for tok in tokens:
      # for '*', just check first char (in case it is married)    5 Dec 2016
      if tok[0] == '*':
         if verb > 2: print("-- data file: skipping '*'")
         acount += 1
         continue
      vals, seps = split_token(tok)
      if verb > 3:
         print("found token = '%s'\n  vals = %s\n  seps = %s" \
               % (tok, vals, seps))

      # check for valid floats
      try: fvals = [float(val) for val in vals]
      except:
         if verb > 0: print("** unusable token, bad floats : %s" % tok)
         if verb > 0: print("   line = %s" % line)
         return 1, [], acount

      # first time, copy the sep list
      if res_sep == None: res_sep = seps[:]

      # just compare for separator consistency (inconsistent if not)
      if res_sep != seps and inc_warn:
         print('** warning: inconsistent separators on line: %s' % line)
         inc_warn = 0

      # see what is here: just time, with duration, or only with modulators
      # duration is 0 unless otherwise specified
      if len(seps) == 0:    res_list.append([fvals[0], [], 0])
      elif seps[-1] == ':': res_list.append([fvals[0], fvals[1:-1], fvals[-1]])
      else:                 res_list.append([fvals[0], fvals[1:], 0])

   return 0, res_list, acount

def split_token(tdata, seplist=[':','*',',']):
   """
      like str.split(), but allow a list of separators, and return 
      the corresponding list of separators (length should be one less
      than the length of the value list) along with the list of elements,
      i.e. return [split values] [separators]
   """

   # deal with empty token or no separators first
   if len(tdata) == 0: return [''], []

   toks = []
   seps = []
   start = 0    # start position for next token
   for posn in range(len(tdata)):
      if tdata[posn] in seplist:
         toks.append(tdata[start:posn])
         seps.append(tdata[posn])
         start = posn + 1

   # and append the final token
   toks.append(tdata[start:])

   return toks, seps

def married_type(mdata):
    """return whether there are modulators or durations
       (need only check first non-empty element, as consistency is required)

       return a bit mask:

          return 0 : simple times
          return 1 : has amplitude modulators
          return 2 : has duration modulators
          return 3 : has both
    """

    rv = 0
    for lind in range(len(mdata)):
       line = mdata[lind]
       if len(line) > 0:
         ttok = line[0]
         if len(ttok[1]) > 0: rv |= 1   # has amp mods
         if ttok[2] > 0:      rv |= 2   # has dur mods
         break
    return rv

def data_is_rect(mdata):
    """return whether the number of columns is consistent across rows"""
    if mdata == None: return 1
    if len(mdata) == 0: return 1
    rlen = len(mdata[0])
    for row in mdata:
        if len(row) != rlen: return 0
    return 1

def main():
   if len(sys.argv) > 2:
      if sys.argv[1] == '-eval':
         print(eval(' '.join(sys.argv[2:])))
         return 0

   print('lib_textdata.py: not intended as a main program')
   return 1

if __name__ == '__main__':
   sys.exit(main())
