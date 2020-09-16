#!/usr/bin/env python

# python3 status: compatible

# Take in a GE cdiflist and write out a gradient file+ set of bvals

# --------------------------------------------------------------------------
auth = 'PA Taylor'
#
#ver = '0.0' ; date = 'June 10, 2020'
# [PT] inputs
#
#ver = '0.1' ; date = 'June 10, 2020'
# [PT] debug/test
#
#ver = '0.2' ; date = 'June 10, 2020'
# [PT] update help-- note this should only be used at present on axial
#      slice acq;  thanks, Joelle!
#
ver = '0.3' ; date = 'June 22, 2020'
# [PT] the column vectors should be scaled by bval, as help says; that
# happens now
#
# --------------------------------------------------------------------------

import sys, os, copy, glob

from   afnipy import afni_base      as ab
from   afnipy import afni_util      as UTIL

# ---------------------------------------------------------------------------

# defaults
ddefs = {
    'DEF_ver'               : ver,
    'DEF_date'              : date,
    'DEF_auth'              : auth,
}

# ----------------------------------------------------------------------------

help_string = '''
  PURPOSE ~1~

  This program reads in a GE cdiflist and outputs gradient file + file
  of bvalues, which can be used in subsequent processing.

  Ver  : {DEF_ver}
  Date : {DEF_date}
  Auth : {DEF_auth}

------------------------------------------------------------------------------

INPUTS ~1~

  + cdiflist (from GE DWI scanning)
  + max bvalue used (in using s/mm^2), e.g., 1000 or 1100. 

------------------------------------------------------------------------------

OUTPUTS ~1~

  + row gradient file (unscaled, unit-magnitude)
  + col gradient file (scaled by b-magn)
  + row bval file     (bvalues)

------------------------------------------------------------------------------

RUNNING ~1~

 -cdiflist CDIFLIST
                :(req) name(s) of cdiflist text file, which can be
                 output by GE scanners when acquiring DWIs and has the
                 following format:
                 + first line is 1 number, which is the number of grads N
                   used in the aquisition
                 + N rows of 3 values each, which relate to the gradient
                   direction+strength (but they are *not* directly the 
                   grads themselves)

 -bval_max BBB  :(req) max bvalue used, which provides a reference value
                 for scaling everything else

 -prefix PP     :(req) output basename for the subsequent grad and bvalue
                 files.
                 Note that this can include path information, but both 
                 a suffix and a file extensions will be added for the
                 main outputs: 
                    _rvec.dat  (row-format of gradients, unit magn)
                    _bval.dat  (row-format of bvals)
                    _cvec.dat  (col-format of grads, scaled by b-values)

 -ver           :display current version
                 ({DEF_ver})

 -date          :display release/editing date of current version
                 ({DEF_date})

 -help          :display help (in terminal)
 -h             :display help (in terminal)

 -hview         :display help (in separate text editor)

------------------------------------------------------------------------------

NOTES ~1~

At this point in time, this program should only be used if the DWI
acquisition used *axial slices*.  This tends to be (by far) the most
common way to acquire the data, so this probably isn't a very
prohibitive restriction.  However, more option(s) would need to be
added for dealing with other slice acquisitions (based on how GE
stores the data).

Also, if you have any questions/uncertainty about the gradient info,
just ask.  And if you *really* want a correct answer, of course you
should ask Joelle, the real expert!

------------------------------------------------------------------------------

EXAMPLES ~1~

convert_cdiflist_to_grads.py             \\
    -cdiflist  cdiflist45                \\
    -bval_max  1100                      \\
    -prefix    grads_ge_45


'''.format(**ddefs)


# make list and dictionary of all opts, used below in parsing user inputs
opts_list = ab.parse_help_text_for_opts( help_string )
opts_dict  = {} 
for x in opts_list:
    opts_dict[x[1:]] = x

# ---------------------------------------------------------------------------

def read_in_cdiflist(fname, lines=1, strip=1, noblank=1, verb=1):
    """Read in a "cdiflist", which should have 1 number N at the top, 
    and then N rows of 3 values each.

    Return two items:
    + the integer N
    + a list of length N, where each element is a list of 3 floats

    """
    
    L  = UTIL.read_text_file( fname, 
                              lines=lines, strip=strip,
                              noblank=noblank, verb=verb )
    L2 = [x.split() for x in L]

    # ----------------- get top row value --------------
    if len(L2[0]) != 1 :
        ab.EP("Top row is not a single value, though that is expected\n"
              "for a cdiflist. Instead, it is:\n"
              "{}".format(' '.join(L2[0])))
    try:
        ntop = int(L2[0][0])
    except:
        ab.EP("Can't read top of cdiflist as an int????")

    # ------------ get properties of remaining rows -----------
    nrow, ncolmin, ncolmax, is_rag, is_sq \
        = UTIL.mat_row_mincol_maxcol_ragged_square(L2[1:])

    # more checks on size/shape
    if nrow != ntop :
        ab.EP("Top number in file is {}, so file should have {} rows total,\n"
              "but I detect {} rows total".format(ntop, ntop+1, nrow+1))
    if is_rag :
        ab.EP("Cannot have a ragged cdiflist")
    if ncolmax != 3 :
        ab.EP("Need 3 cols throughout most of cdiflist, {}".format(ncolmax))

    out = []
    for row in L2[1:]:
        out.append([float(x) for x in row])

    return ntop, out

def cdiflist_vals_to_grad_bval(L, bval=1):
    """Input is a list of cdiflist values (L = list of lists of 3 floats;
    each 3-list was a row in the original cdiflist).  A bval can
    (should!) also be provided, for scaling each value.

    Return: two lists of len(L):
    + bvalues for each 3-list
    + unit magnitude values of each 3-list
    """

    N = len(L)

    out_bval = []
    out_bvec = []

    for row in L:
        magn  = row[0]**2 + row[1]**2 + row[2]**2
        out_bval.append(bval*magn)
        if magn :
            out_bvec.append([(x/magn**0.5) for x in row])
        else: # must be a 0 row
            out_bvec.append([(x) for x in row])

    return out_bval, out_bvec

def write_files_from_bval_bvec(B, VEC, prefix):
    """Input
    B      :list of N bvals
    VEC    :list of N grads (unscaled)
    prefix :basename of output

    Write out: 
    + prefix_rvec.dat :row-format of N unscaled grads
    + prefix_bval.dat :row-format of N bvalues
    + prefix_cvec.dat :row-format of N *scaled* grads
    """

    N = len(B)

    # row vec of grads
    fname = prefix + '_rvec.dat'
    fff = open(fname,'w')
    for jj in range(3):
        for ii in range(N):
            fff.write(" {:>10.5f} ".format(VEC[ii][jj]))
        if jj != 2:
            fff.write("\n")
    fff.close()
    
    ab.IP("Wrote unscaled row vecs : {}".format(fname))

    # row bvals
    fname = prefix + '_bval.dat'
    fff = open(fname,'w')
    for ii in range(N):
        fff.write(" {:>10.3f} ".format(B[ii]))
    fff.close()

    ab.IP("Wrote b-value row file  : {}".format(fname))

    # col vec of grads
    fname = prefix + '_cvec.dat'
    fff = open(fname,'w')
    for ii in range(N):
        for jj in range(3):
            fff.write(" {:>10.3f} ".format(VEC[ii][jj]*B[ii]))
        if jj != N-1:
            fff.write("\n")
    fff.close()

    ab.IP("Wrote b-scaled col vecs : {}".format(fname))

# ---------------------------------------------------------------------------

class iopts_obj:
    """
    store all the argv-entered things opts to plot
    """

    def __init__( self ) :

        self.cdiflist         = None
        self.bval_max         = None
        self.prefix           = None

    # ---------- check ----------------

    def check_req(self):
        ''' Check for and point out any missing inputs.'''
        MISS = 0

        if not(self.prefix) :
            ab.EP("missing '{prefix}' value"
                  "".format(**opts_dict), end_exit=False)
            MISS+=1

        if not(self.cdiflist) :
            ab.EP("missing '{cdiflist}' value"
                  "".format(**opts_dict), end_exit=False)
            MISS+=1

        if not(self.bval_max) :
            ab.EP("missing '{bval_max}' value"
                  "".format(**opts_dict), end_exit=False)
            MISS+=1

        return MISS

    def finish_defs(self):
        """Check a couple things with input. 

        All input files/globs get parsed here.

        """

        if self.cdiflist :
            # replace with actual list of files, because it might just
            # have been the glob list that was input
            all_file = UTIL.list_files_by_glob( self.cdiflist,
                                                sort=True,
                                                exit_on_miss=True )
            if len(all_file) > 1 :
                ab.EP("Too many files found when globbing with:\n"
                      "\t {}".format(self.cdiflist))
            else:
                self.cdiflist = all_file[0] 

# ------------------------------------------------------------------

def parse_args(full_argv):
    """Go through user-entered options and fill an object with the values.
    These will be used to setup plotting.

    """

    argv = full_argv[1:]
    Narg = len(argv)

    if not(Narg):
        print(help_string)
        sys.exit(0)

    # initialize objs
    iopts          = iopts_obj()
    iopts.full_cmd = full_argv

    i = 0
    while i < Narg:
        if argv[i] == "{ver}".format(**opts_dict) :
            print(ver)
            sys.exit(0)

        elif argv[i] == "{date}".format(**opts_dict) :
            print(date)
            sys.exit(0)

        elif argv[i] == "{help}".format(**opts_dict) or \
             argv[i] == "{h}".format(**opts_dict) :
            print(help_string)
            sys.exit(0)

        elif argv[i] == "{hview}".format(**opts_dict) :
            prog = os.path.basename(full_argv[0])
            cmd = 'apsearch -view_prog_help {}'.format( prog )
            ab.simple_shell_exec(cmd)
            sys.exit(0)

        # ---------- req ---------------


        elif argv[i] == "{cdiflist}".format(**opts_dict) :
            if i >= Narg:
                ab.ARG_missing_arg(argv[i])
            i+= 1
            iopts.cdiflist = argv[i]

        elif argv[i] == "{prefix}".format(**opts_dict) :
            if i >= Narg:
                ab.ARG_missing_arg(argv[i])
            i+= 1
            iopts.prefix = argv[i]

        elif argv[i] == "{bval_max}".format(**opts_dict) :
            if i >= Narg:
                ab.ARG_missing_arg(argv[i])
            i+= 1
            iopts.bval_max = float(argv[i])

        # ---------- opt ---------------

        # --------- finish -------------

        else:
            print("** ERROR: unknown opt: '{}'".format(argv[i]))
            sys.exit(2)
        i+=1


    if iopts.check_req():
        print("   -------------------------------")
        ab.EP("Problem with input arguments. See detailed whining above.")

    iopts.finish_defs()

    return iopts

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
