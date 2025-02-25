#!/usr/bin/env python

# Combine header info across multiple files to help users Get To Know
# Their Data (GTKYD)

ver='2.0' ; date='Sep 10, 2024'
# + [PT] migrated over original tcsh script to this
#
###############################################################################

import sys, os, copy
import json
from afnipy import afni_base as BASE
from afnipy import afni_util as UTIL
from afnipy import lib_format_cmd_str as lfcs

# =============================================================================

hline_mini = '-'*27

# ---------------------------------------------------------------------------

list_3dinfo_ignore = [ '-hview', '-h_view',
                       '-hweb',  '-h_web', 
                       '-help',  '-HELP',
                       '-h_aspx', '-h_spx', 
                       '-h_find', '-h_raw', '-h',
                       '-all_opts',
                       '-echo_edu',         # shouldn't happen-- just in case
                       '-atr_delim', '-sb_delim',
                       '-NA_flag', 
                       '-sval_diff', '-val_diff', # require 2 dsets
                       '-same_all_grid',    # these '-same*' require 2 dsets
                       '-same_center',
                       '-same_delta',
                       '-same_dim',
                       '-same_grid',
                       '-same_obl',
                       '-same_orient',
                       '-n[i|j|k',          # woe; luckily, covered under '-n4'
]

# ---------------------------------------------------------------------------

allowed_header_items_progs = ['3dinfo', 'nifti_tool']

# dictionary of items for 3dinfo when running gtkyd_check, where value
# is num outputs (in future, could put list of types?)
gtkyd_3dinfo_all = {
    "prefix_noext" : 1,
    "n3" : 3,
    "nv" : 1,
    "orient" : 1,
    "ad3" : 3,
    "tr" : 1,
    "is_slice_timing_nz" : 1,
    "space" : 1,
    "av_space" : 1,
    "is_oblique" : 1,
    "obliquity" : 1,
    "o3" : 3,
    "datum" : 'PERBRICK',   # note the keyword; gets special proc
    "is_nifti" : 1,
}

# some things we want from the header; an additional function gets
# other stuff
gtkyd_nifti_tool_all = {
    'datatype' : 1,
    'sform_code' : 1,
    'qform_code' : 1,
}

gtkyd_nifti_tool_exts = {
    'has_exts' : 1,
    'has_afni_exts' : 1,
}

# right now, this one is just for nifti case
gtkyd_nifti_sidecar = {
    'has_sidecar' : 1,
}

gtkyd_brickstat_minmax = {
    'min' : 1,
    'max' : 1,
}

# =============================================================================

class GtkydInfo:

    def __init__(self, infiles, outdir='GTKYD', do_minmax=False, 
                 id_keeps_dirs=0, do_ow=0, verb=1):
        """Create object holding GTKYD info data, namely dictionaries of
        header info.

        """

        self.verb       = verb                # verbosity level
        self.do_ow      = do_ow               # bool, overwrite or not
        self.infiles    = infiles             # list of all input file fnames
        self.mixed_vols = False               # bool, have B/H *and* NIFTI?

        self.do_minmax  = do_minmax           # bool, run 3dBrickStat (slow)
        self.id_keeps_dirs = id_keeps_dirs    # int, add N dirs to prefix_noext
        self.outdir     = outdir              # str, output dir name
        self.outxls     = ''                  # str, XLS file (prepped below)

        self.all_fname  = []                  # list, all filenames (no path)
        self.all_absdir = []                  # list, all abs path dirs of infiles
        self.all_hdrs   = []                  # list, hdr dict (1 per infile)
        self.all_otxt   = []                  # list, all written txt per dset

        # ----- do work

        # prep/check
        if self.check_infiles() : 
            sys.exit(1)
        if self.check_and_prep_outnames() : 
            sys.exit(2)

        # main work, gathering info
        if self.read_all_hdrs() :
            sys.exit(3)

        # outputting
        if self.make_outdir() :
            sys.exit(4)
        if self.write_txt_per_dset() :
            sys.exit(5)
        if self.write_txt_per_item() :
            sys.exit(6)
        if self.write_gen_ss_review_table() :
            sys.exit(7)

        if self.closing_text() :
            sys.exit(8)

    # ---------- methods, etc.

    def closing_text(self):
        """some fond farewells"""

        BASE.IP(hline_mini)
        BASE.IP("DONE. See the outputs:")
        ttt = self.outxls
        print("   {:25s} : {}".format("group summary table", ttt))
        ttt = self.outdir + '/rep_gtkyd_detail_*.dat'
        print("   {:25s} : {}".format("group detailed values", ttt))
        ttt = self.outdir + '/rep_gtkyd_unique_*.dat'
        print("   {:25s} : {}".format("group unique values", ttt))
        ttt = self.outdir + '/dset_*.txt'
        print("   {:25s} : {}".format("individual value lists", ttt))

        return 0

    def write_gen_ss_review_table(self):
        """run GSSRT on the individual subject text files"""

        if self.do_ow :  ow_str = '-overwrite'
        else:            ow_str = ''

        # to avoid GSSRT guessing subject IDs oddly, jump into dir
        # with files (so need to jump back)
        here = os.getcwd()
        os.chdir(self.outdir)  # jump to outdir to do work

        cmd = "gen_ss_review_table.py {} ".format(ow_str)
        cmd+= "-tablefile ../{} ".format(self.outxls_name)
        cmd+= "-infiles {} ".format(' '.join(self.all_otxt))

        if self.verb > 1 :
            _c, cmd_frmt = lfcs.afni_niceify_cmd_str(cmd)
            BASE.IP("Running command to create table:\n{}"
                    "".format(cmd_frmt))

        com = BASE.shell_com(cmd, capture=1)
        com.run()
        os.chdir(here)  # jump back
        if com.se :
            return -1

        return 0

    def write_txt_per_item(self):
        """write out reports for each item: one of values across all subj, and
        one for unique values across all subj"""

        if self.verb > 1 :  BASE.IP(hline_mini)

        all_item = self.all_item
        nitem = len(all_item)
        for jj in range(nitem):
            key = all_item[jj]
            prog, opts, abbr = get_prog_of_key(key)
            top_str = "# {} {}{}\n".format(prog, opts, key)

            if self.verb > 1 :
                BASE.IP("{} ...".format(top_str[2:-1]))

            # list of all values, in order
            all_val = []
            maxlen  = -1
            for ii in range(self.ninfiles):
                val   = ' '.join(self.all_hdrs[ii][key])
                if len(val) > maxlen :
                    maxlen = len(val)
                all_val.append( val )

            # the detailed report
            ofile1 = self.outdir + '/' 
            ofile1+= 'rep_gtkyd_detail_' + abbr + '_' + key + '.dat'
            fff = open(ofile1, 'w')
            fff.write(top_str)
            for ii in range(self.ninfiles):
                val   = all_val[ii]
                fname = self.all_fname[ii]
                fff.write("{:<{}s}  {:<s}\n".format(val, maxlen, fname))
            fff.close()

            # list of unique values
            uniq_val = list(set(all_val))

            # the unique values report
            ofile2 = self.outdir + '/' 
            ofile2+= 'rep_gtkyd_unique_' + abbr + '_' + key + '.dat'
            fff = open(ofile2, 'w')
            fff.write(top_str)
            for val in uniq_val:
                fff.write("{:s}\n".format(val))
            fff.close()

        if self.verb > 1 :  BASE.IP(hline_mini)

        return 0

    def write_txt_per_dset(self):
        """write out full dictionary per subject, from self.all_hdrs"""

        for ii in range(self.ninfiles):

            if self.id_keeps_dirs :
                # calc extra parts to attach to subj ID and prefix_noext val
                lpath = self.all_absdir[ii].split('/')
                N = min(len(lpath), self.id_keeps_dirs)
                extra_subj = '_'.join(lpath[-N:]) + '_'
                extra_pref = '/'.join(lpath[-N:]) + '/'

            # dictionary and subj
            D = self.all_hdrs[ii]
            subj = D['prefix_noext'][0]
            if self.id_keeps_dirs :
                subj = extra_subj + subj
            ofile = 'dset_gtkyd_' + subj + '.txt'  # local filenames, no path

            fff = open(self.outdir + '/' + ofile, 'w')
            fff.write("\n")
            for key in D.keys():
                val = ' '.join(D[key])
                # special case here because 'subject ID' is special
                # col header in GSSRT; also now might include some
                # path info in subject ID
                if key == 'prefix_noext' : 
                    lll = 'subject ID'
                    if self.id_keeps_dirs :
                        val = extra_pref + val
                else:
                    lll = key
                fff.write("{:<20s} : {:<s}\n".format(lll, val))
            fff.close()
            self.all_otxt.append(ofile)

        return 0

    def make_outdir(self):
        """make output directory, if needed"""

        if not os.path.isdir(self.outdir) :
            os.mkdir(self.outdir)
            return 0
        return 0

    def read_all_hdrs(self):
        """go through all infiles and get header info. This populates a list
        of headers, one-for-one with the infiles"""
        
        if self.verb :
            BASE.IP("Now starting to Get To Know Your Data...")

        for fname in self.infiles :
            if self.verb > 1 :
                BASE.IP("Checking: {}".format(fname))

            _c, D = get_header_items_gtkyd(fname, 
                                           do_brickstat = self.do_minmax,
                                           do_nifti_if_brik = self.mixed_vols,
                                           verb = self.verb)
            if _c :    
                BASE.EP("Bad header extraction for {}"
                        "".format(fname), end_exit=0)
                return -1
            else:
                self.all_hdrs.append(D)

        return 0

    def check_infiles(self):
        """verify existence of all files, and check if mixed NIFTI and B/H"""
        
        if not self.ninfiles :
            BASE.EP("No infiles provided", end_exit=0)
            return -1

        bad_list = [x for x in self.infiles if not os.path.isfile(x)]
        
        if len(bad_list) :
            BASE.EP("At least one input file is not findable:\n{}"
                    "".format(', '.join(bad_list)), end_exit=0)
            return -1

        # are infiles all NIFTI or BRIK/HEAD, or mixed?
        all_nii = [x for x in self.infiles if is_nifti(x)]
        nnii = len(all_nii)

        if 0 < nnii and nnii < self.ninfiles :
            self.mixed_vols = True
            if self.verb :
                BASE.IP("Mixed set of input volumes: both NIFTI and BRIK/HEAD")

        # get list of fnames from infiles (no paths)
        self.all_fname = [x.split('/')[-1] for x in self.infiles]
        # ... and list of all abs path dirs from infiles
        self.all_absdir = [os.path.abspath(os.path.dirname(x))  \
                           for x in self.infiles]

        if self.verb :
            BASE.IP("Have {} dsets to check".format(self.ninfiles))

        return 0

    def check_and_prep_outnames(self):
        """check output path is all clear (or using overwrite); also remove
        any hanging '/', and prepare XLS output filename (also
        checking that for overwrite-ability)

        """

        if not self.outdir :
            BASE.EP("No outdir provided", end_exit=0)
            return -1

        # strip trailing '/' and create XLS filename
        self.outdir = self.outdir.rstrip('/')
        self.outxls = self.outdir + '.xls'

        if self.verb :
            BASE.IP("Making new output directory: {}".format(self.outdir))

        # check output dir existence
        if os.path.exists(self.outdir) :
            if self.do_ow :
                BASE.WP("removing preexisting output dir: {}"
                        "".format(self.outdir))
                cmd = """\\rm -rf {}""".format(self.outdir)
                com = BASE.shell_com(cmd, capture=1)
                com.run()
            else:
                BASE.EP("cannot overwrite existing dir:\n{}\n"
                        "Remove it, change '-outdir ..' or use '-overwrite'"
                        "".format(self.outdir), end_exit=0)
                return -1

        # check output xls existence
        if os.path.isfile(self.outxls) :
            if self.do_ow :
                BASE.WP("removing preexisting XLS file  : {}"
                        "".format(self.outxls))
                cmd = """\\rm -f {}""".format(self.outxls)
                com = BASE.shell_com(cmd, capture=1)
                com.run()
            else:
                BASE.EP("cannot overwrite existing file:\n{}\n"
                        "Remove it, change '-outdir ..' or use '-overwrite'"
                        "".format(self.outxls), end_exit=0)
                return -1

        return 0

    @property
    def outxls_name(self):
        """Just the filename of outxls, without no path"""
        return self.outxls.split('/')[-1]
    @property
    def ninfiles(self):
        """The total number of infiles."""
        return len(self.infiles)

    @property
    def all_item(self):
        """The set of item names (keys per subj in all_hdrs; should be
        constant per subj)."""

        if len(self.all_hdrs) :
            return list(self.all_hdrs[0].keys())
        else:
            return []

    @property
    def nitem(self):
        """The number of item names (keys per subj in all_hdrs; should be
        constant per subj). """
        return len(self.all_item)


# =============================================================================

def get_header_items_prog(fname, all_opt={}, prog='3dinfo', verb=0):
    '''For a given dictionary of program opt strings (keys = opt name
without '-' for 3dinfo; values = number of expected outputs), make a
dictionary whose keys are the same opt names and whose values are
those from the dset fname. The opts can be for one of the allowed
progs: 3dinfo, nifti_tool.

Parameters
----------
fname : str
    input volume name 'pon which to run 3dinfo.
all_opt : dict
    dictionary of opt strings (keys = opt name without '-'; values =
    number of expected outputs), which could be for any of the allowed
    progs
prog : str
    one of the allowed progs for parsing here
verb : int
    verbosity level for terminal output whilst running

Returns
-------
check : int
    0 for OK, nonzero for all else
dict_info : dict
    dictionary whose keys are 3dinfo opt names relevant for a single
    volume (not helpy ones or comparison ones like -same*); the '-'
    part is part of the key here, at the moment.

'''
    BAD_RETURN = -1, {}

    if prog not in allowed_header_items_progs :  return BAD_RETURN
    if not os.path.isfile(fname) :               return BAD_RETURN

    nopt = len(all_opt)
    if not(nopt) :
        return 0, {}

    # prep the command and counting
    opt_str   = ""
    opt_list  = []   # dict should be ordered, but just keep track here
    for opt in all_opt.keys():
        if prog == '3dinfo' :
            opt_str+= "-{} " .format(opt)
        elif prog == 'nifti_tool' :
            opt_str+= "-field {} " .format(opt)
        opt_list.append(opt)

    # run command
    if prog == '3dinfo' :
        cmd = '''3dinfo {} {}'''.format(opt_str, fname)
    elif prog == 'nifti_tool' :
        cmd = '''nifti_tool -quiet -disp_hdr {} -infiles {}'''.format(opt_str, 
                                                                      fname)
    if verb > 1 :
        BASE.IP("Header command:\n{}".format(cmd))
    com = BASE.shell_com(cmd, capture=1)
    com.run()
    header_str  = '\n'.join(com.so)
    header_list = header_str.split()

    # parse header list
    dict_info = {}
    count = 0
    for ii in range(nopt):
        opt   = opt_list[ii]
        nval  = all_opt[opt]
        if nval == 'PERBRICK' :
            nval = 1
            # split; get unique set; listify; sort
            aaa = list(set(header_list[count].split('|')))
            aaa.sort()
            dict_info[opt] = copy.deepcopy(aaa)
        else:
            dict_info[opt] = header_list[count:count+nval]
        count+= nval

    # for this prog, add some additional calcs
    if prog == 'nifti_tool' :
        tmp, dd_ext = get_nifti_tool_exts(fname, verb=verb)
        for key in dd_ext:
            dict_info[key] = dd_ext[key]

        tmp, dd_car = get_nifti_sidecar(fname, verb=verb)
        for key in dd_car:
            dict_info[key] = dd_car[key]

    return 0, dict_info

def get_nifti_sidecar(fname, verb=0):
    """Check if there is a JSON file accompanying fname.  For a given
nifti fname AAA.nii or AAA.nii.gz, just check if AAA.json exists

Parameters
----------
fname : str
    input volume name 'pon which to run check
verb : int
    verbosity level for terminal output whilst running

Returns
-------
check : int
    0 for OK, nonzero for all else
dict_info : dict
    dictionary of one response: 1 for yes (has sidecar), 0 for no.

    """
    BAD_RETURN = -1, {}

    if not os.path.isfile(fname) :               return BAD_RETURN

    dict_info = {}

    if fname.endswith('.nii.gz') :
        fname_base = fname[:-7]
    elif fname.endswith('.nii') :
        fname_base = fname[:-4]
    else:
        BASE.EP("In sidecar check, filename doesn't end .nii.gz or .nii : {}"
                "".format(fname), end_exit=1)
   
    # OK, simply do the check now.
    dict_info['has_sidecar'] = [str(int(os.path.isfile(fname_base + '.json')))]
    return 0, dict_info


def get_nifti_tool_exts(fname, verb=0):
    """Get specific header information from nifti_tool about
extensions. This effectively uses a pre-built list of labels to
construct a dictionary to output.

Parameters
----------
fname : str
    input volume name 'pon which to run nifti_tool
verb : int
    verbosity level for terminal output whilst running

Returns
-------
check : int
    0 for OK, nonzero for all else
dict_info : dict
    dictionary whose keys are made up but kind of refer to program
    options, re. extensions.

    """
    BAD_RETURN = -1, {}

    if not os.path.isfile(fname) :               return BAD_RETURN

    dict_info = {}

    cmd = '''nifti_tool -quiet -disp_exts -infiles {}'''.format(fname)
    if verb > 1 :
        BASE.IP("Running extension-check command:\n{}".format(cmd))
    com = BASE.shell_com(cmd, capture=1)
    com.run()
    header_str  = '\n'.join(com.so)
    
    # 1 or 0 for these fields, but match formatting of other dict
    dict_info['has_exts']      = [str(int(bool(len(header_str))))]
    dict_info['has_afni_exts'] = [str(int('AFNI' in header_str))]

    # quick check that these entries match the reference list for
    # them, from above. This is for programmers to keep these special
    # keys as part of a reference list
    for key in dict_info.keys():
        BAD = 0
        if key not in gtkyd_nifti_tool_exts :
            BASE.EP("Likely programmer error, not keeping dict entries "
                    "in sync; mismatched key: {}"
                    "".format(key), end_exit=1)
            BAD+= 1
        if BAD :
            return BAD_RETURN

    return 0, dict_info


def get_brickstat_minmax(fname, verb=0):
    """Get specific header information from 3dBrickStat. This effectively
uses a pre-built list of labels to construct a dictionary to
output. We don't use a dictionary of opts here, bc some of the
ordering might be pre-chosen for some opts.

Parameters
----------
fname : str
    input volume name 'pon which to run 3dinfo.
verb : int
    verbosity level for terminal output whilst running

Returns
-------
check : int
    0 for OK, nonzero for all else
dict_info : dict
    dictionary whose keys are 3dinfo opt names relevant for a single
    volume (not helpy ones or comparison ones like -same*); the '-'
    part is part of the key here, at the moment.

    """
    BAD_RETURN = -1, {}

    if not os.path.isfile(fname) :               return BAD_RETURN

    dict_info = {}

    cmd = '''3dBrickStat -slow -min -max {}'''.format(fname)
    if verb > 1 :
        BASE.IP("Running 3dBrickStat command:\n{}".format(cmd))
    com = BASE.shell_com(cmd, capture=1)
    com.run()
    header_str  = '\n'.join(com.so)
    header_list = header_str.split()

    if len(header_list) != 2 :    return BAD_RETURN

    dict_info['min'] = [header_list[0]]
    dict_info['max'] = [header_list[1]]

    # quick check that these entries match the reference list for
    # them, from above. This is for programmers to keep these special
    # keys as part of a reference list
    for key in dict_info.keys():
        BAD = 0
        if key not in gtkyd_brickstat_minmax :
            BASE.EP("Likely programmer error (2), not keeping dict entries "
                    "in sync; mismatched key: {}"
                    "".format(key), end_exit=1)
            BAD+= 1
        if BAD :
            return BAD_RETURN

    return 0, dict_info


def get_header_items_gtkyd(fname, 
                           do_brickstat=False, do_nifti_if_brik=False,
                           verb=0):
    """Get header info across 3dinfo, nifti_tool and (optionally)
3dBrickStat.  The latter is not on by default bc it is the slowest;
the do_brickstat flag can be set to True to get that info.

The do_nifti_if_brik opt exists for when the set of infiles includes
both NIFTI and BRIK/HEAD dsets. If True, nifti header fields are
reported even for BRIK/HEAD files (values = 'NA').

This is a major/primary function.

Parameters
----------
fname : str
    input volume name 'pon which to run 3dinfo.
do_brickstat : bool
    flag about whether to do 3dBrickStat calcs, which are the slowest
do_nifti_if_brik : bool
    flag about whether to output nifti header info even if input is
    BRIK/HEAD format
verb : int
    verbosity level for terminal output whilst running

Returns
-------
check : int
    0 for OK, nonzero for all else
dict_info : dict
    dictionary whose keys are basically opt names or special keys for
    header items across 3dinfo, nifti_tool and 3dBrickStat; the '-'
    part is part of the key here, at the moment.

    """

    BAD_RETURN = -1, {}

    if not os.path.isfile(fname) :               return BAD_RETURN

    # 3dinfo part
    _c, dict_info = get_header_items_prog(fname, all_opt=gtkyd_3dinfo_all, 
                                          prog='3dinfo', verb=verb)
    if _c :    return BAD_RETURN
    
    # nifti_tool part---note sometimes we don't have a NIFTI file here
    # but still want the relevant keys to have placeholder values
    # (like when mixing infiles of BRIK/HEAD and NIFTI dsets)
    if int(dict_info['is_nifti'][0]) :
        _c, D = get_header_items_prog(fname, all_opt=gtkyd_nifti_tool_all,
                                      prog='nifti_tool', verb=verb)
        if _c :    return BAD_RETURN

        for key in D.keys():
            dict_info[key] = D[key]

    elif do_nifti_if_brik :
        for key in gtkyd_nifti_tool_all.keys():
            dict_info[key] = [gtkyd_nifti_tool_all[x] * 'NA']
        for key in gtkyd_nifti_tool_exts.keys():
            dict_info[key] = [gtkyd_nifti_tool_exts[x] * 'NA']
        for key in gtkyd_nifti_sidecar.keys():
            dict_info[key] = [gtkyd_nifti_sidecar[x] * 'NA']

    # 3dBrickStat part
    if do_brickstat :
        _c, D = get_brickstat_minmax(fname, verb=verb)
        if _c :    return BAD_RETURN
    
        for key in D.keys():
            dict_info[key] = D[key]

    return 0, dict_info


def is_nifti(fname):
    """Quickest of quick checks, purely based on fname extension"""

    if fname.endswith('.nii') or fname.endswith('.nii.gz') :
        return True
    else:
        return False


def get_prog_of_key(key):
    """Based on key label, get what program created it

Parameters
----------
key: str
    potential option or key value

Returns
-------
prog : str
    full program name
opts : str
    opts given to prog to get info (spaces included/matter)
abbr : str
    abbreviation related to program name

"""

    if key in gtkyd_3dinfo_all :
        return '3dinfo', '-', 'info'
    elif key in gtkyd_nifti_tool_all :
        return 'nifti_tool', '-disp_hdr field ', 'nifti'
    elif key in gtkyd_nifti_tool_exts :
        return 'nifti_tool', '-disp_exts (with parsing) ', 'nifti'
    elif key in gtkyd_nifti_sidecar :
        return 'os.path.isfile(PREFIX.json)', ': ', 'nifti'
    elif key in gtkyd_brickstat_minmax :
        return '3dBrickStat', '-', 'brickstat'
    else: 
        BASE.EP('Unknown key parsed: {}'.format(key))


    return '', ''


# ===========================================================================

if __name__ == "__main__" :
    
    print('Done.')
