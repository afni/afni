#!/usr/bin/env python

# A library of functions for dealing with obliquity in datasets.
#
# auth : PA Taylor (SSCC, NIMH, NIH, USA)
# ----------------------------------------------------------------------------
# ver 1.0 : wrap around basic 3drefit and 3dWarp functionality
# ============================================================================

import sys, os, copy, glob

from   afnipy import afni_base          as ab
from   afnipy import afni_util          as au
from   afnipy import lib_format_cmd_str as lfcs
from   afnipy import lib_obl_child      as LOC
from   afnipy import lib_obl_name       as LON

# ============================================================================

# default values for the main obj
DOPTS = {
    'user_opts'       : [],                    # copy of cmd the user ran
    'inset'           : '',
    'prefix'          : '',
    'child_dsets'      : '',
    'child_prefixes'   : None,
    'child_outdir'     : None,
    'child_suffix'     : None,
    'workdir'         : '',
    'purge_obl_meth'  : 'keep_origin_raw',     # just name of a def val
    'overwrite'       : '',
    'do_qc'           : 'Yes',                 # below, Yes|No -> True|False
    'do_clean'        : 'Yes',
    'do_log'          : False,
    'verb'            : 1,
}

# ----------------------------------------------------------------------------

# NB: at the moment, users canNOT change the style of purging
# obliquity, since there doesn't seem to be a great need for them to
# do so, and this keeps the program usage simpler.  However, we
# preserve this code here in case we decide to use it later

# mapping between 'friend' keywords used here and 3drefit terms
DICT_purge_obl_header = {
    'keep_origin_round'   : '-oblique_recenter',      # good to use
    'keep_origin_raw'     : '-oblique_recenter_raw',  # fine to use
    'move_origin_matrix'  : '-oblique_origin',        # probably avoid
}
LIST_purge_obl_header_keys = list(DICT_purge_obl_header.keys())
STR_purge_obl_header_keys  = ', '.join(LIST_purge_obl_header_keys)

# ============================================================================

class MainObj:
    """Object for obliquity tool functionality.

Parameters
----------
inobj : InOpts object 
    object constructed from running obliquity_tool.py on the command
    line. At present, the only way to really provide inputs here.

    """

    def __init__(self, user_inobj=None):

        # ----- set up attributes

        # main input variables
        self.status          = 0                       # not used
        self.user_opts       = DOPTS['user_opts']      # command the user ran
        self.user_inobj      = user_inobj
        self.argv            = None

        # general variables
        self.do_clean        = DOPTS['do_clean']
        self.verb            = DOPTS['verb']
        self.overwrite       = DOPTS['overwrite']
        self.do_log          = DOPTS['do_log']

        # main data variables
        self.inset           = DOPTS['inset']
        self.inset_avsp      = ''                      # 3dinfo -av_space INSET
        self.prefix          = DOPTS['prefix']
        self.prefix_nobj     = None                    # NameObj for prefix

        self.child_dsets      = DOPTS['child_dsets']     # None or list
        self.child_prefixes   = DOPTS['child_prefixes']  # None or list
        self.child_outdir     = DOPTS['child_outdir']    # None or str
        self.child_suffix     = DOPTS['child_suffix']    # None or list

        # control variables
        self.workdir         = DOPTS['workdir']
        self.do_purge_obl    = DOPTS['purge_obliquity']
        self.do_qc           = DOPTS['do_qc']

        # ----- take action(s)

        # prelim stuff
        if user_inobj :
            tmp1 = self.load_from_inopts()
            tmp2 = self.basic_setup()

        if self.do_purge_obl :
            # edit header of single dset (for now, we don't allow
            # users to change the method of how this is done, but that
            # could be enabled by adding the kwarg setting here; also, 
            # write_aff_mats is always on now)
            tmp3 = purge_obliquity_from_header( self.inset, self.prefix, 
                              do_clean        = self.do_clean,
                              overwrite       = self.overwrite,
                              verb            = self.verb )

            # add a note to the main output dset header about this cmd
            tmp4 = self.attach_history_to_prefix()

            # ... and let any childs inherit the purged obliquity
            if self.nchild :
                tmp4 = self.construct_child_prefixes()
                tmp5 = self.distribute_obliquity_to_all_childs()


    # ----- methods

    def attach_history_to_prefix(self):
        """Use 3dNote to attach the history of this obliquity_tool.py cmd in
        the prefix history"""

        cmd = '''3dNotes -h "{}" '''.format(self.argv_str)
        cmd+= '''"{}" '''.format(self.prefix_nobj.name_fullest)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()

        return stat
        
    def construct_child_prefixes(self):
        """Go through a triage of generating a list of child_prefixes to match
        the list of child_dsets that has been input; can be either
        direct, or by construction of pieces.
        """

        if not(self.nchild) :
            txt = "No child dsets, "
            txt+= "so it is erroneous to try to construct child prefixes"
            ab.EP(txt)

        # ----- check about prefixes already existing; if so, just use them

        if not(self.child_prefixes is None) :
            if self.verb > 1 :
                ab.IP("Generate child_prefixes using: child_prefixes")
            nchild_prefs = len(self.child_prefixes)
            if nchild_prefs == self.nchild :
                # good to go
                return 0
            else:
                txt = "Mismatch: we have {} child dsets ".format(self.nchild)
                txt+= "but {} child prefixes".format(nchild_prefs)
                ab.EP(txt)

        # ----- if we get here, we need to construct prefixes

        # verify at least one of these methods was run
        NMETH = 0

        # first, do any child_suffix
        if not(self.child_suffix is None) :
            if self.verb > 1 :
                ab.IP("Generate child_prefixes using: child_suffix")
            NMETH+= 1
            L = self.apply_child_suffix()
        else:
            L = copy.deepcopy(self.child_dsets)

        # then apply any child_outdir 
        if not(self.child_outdir is None) :
            if self.verb > 1 :
                ab.IP("Generate child_prefixes using: child_outdir")
            NMETH+= 1
            L = self.apply_child_outdir(L)

        if not(NMETH) :
            txt = "We have {} child_dsets, ".format(self.nchild)
            txt+= "but used none of child_prefixes, child_suffix or child_outdir"
            ab.EP(txt)

        self.child_prefixes = copy.deepcopy(L)

        return 0


    def apply_child_outdir(self, L):
        """L is a list of file prefixes, which may/may not have path info,
        which will be made to have outdir as their attached directory. The 
        returned list M has the same length as L.
        Because other steps might happen, this intermediate function
        does _not_ update the child_prefixes attribute directly.
        """

        if not(self.child_outdir) :
            ab.EP("Cannot apply child_outdir if none was provided!")
        if not(self.child_dsets) :
            ab.EP("Cannot apply child_suffix; no child_dsets were provided!")

        M = []
        for dset in L:
            # build output name from input dirname, base and possibly ext
            nobj = LON.NameObj(dset)
            name = (self.child_outdir).rstrip('/') + '/' 
            name+= nobj.bname
            M.append(name)

        return M


    def apply_child_suffix(self):
        """Insert the specified suffix into the name of child_dsets, generating
        a new list of names that has the same length.  
        Because other steps might happen, this intermediate function
        does _not_ update the child_prefixes attribute directly.
        """
        
        if not(self.child_suffix) :
            ab.EP("Cannot apply child_suffix if none was provided!")
        if not(self.child_dsets) :
            ab.EP("Cannot apply child_suffix if no child_dsets were provided!")

        L = []
        for dset in self.child_dsets:
            # build output name from input dirname, base and possibly ext
            nobj = LON.NameObj(dset)
            name = nobj.dname + '/' 
            name+= nobj.bname_noext + self.child_suffix 
            name+= nobj.add_ext_for_nifti              # adds '' for AFNI type
            L.append(name)

        return L
                

    def distribute_obliquity_to_all_childs(self):
        """Loop over all child dsets, and call the function that applies the
        obliquity matrix to each."""

        for nn in range(self.nchilds):
            dset  = self.child_dsets[nn]
            nobj  = LON.NameObj(dset)
            dset_full = nobj.name_full
            opref = self.child_prefixes[nn]

            tmp1  = self.distribute_obliquity_to_child(dset_full, opref)

        return 0


    def distribute_obliquity_to_child(self, dset, opref):
        """Run the processing to pass along obliquity info to each child dset.
        """
    
        # shorter name of obj used to get path/names to obl matr
        nobj = self.prefix_nobj
        
        # predict names of primary outputs from main inset obliquity proc
        mat_obl    = nobj.dname + '/' + nobj.bname_noext + "_mat.aff12.1D"
        if not(os.path.isfile(mat_obl)) :
            txt = "Could not find est. obliquity matrix to apply: "+mat_obl
            ab.EP(txt)

        # the '-prefix ..' dset (for QC image); should be predictable like this
        inset_proc = nobj.name_fullest
        if not(au.info_dset_exists(inset_proc)) :
            txt = "Could not load est. prefix derived from inset: "+inset_proc
            ab.EP(txt)

        # set up the obliquity info and dset names
        child_obj = LOC.ChildObj( child_dset=dset, child_prefix=opref, 
                                mat_obl=mat_obl, 
                                do_qc=self.do_qc, inset_raw=self.inset, 
                                inset_proc=inset_proc,
                                overwrite=self.overwrite,
                                do_clean=self.do_clean, verb=self.verb )

        # ... and actually do the proc (make wdir, generate outputs, etc.)
        child_obj.proc_child()

        return 0


    def load_from_inopts(self):
        """Populate the input values using the command line interface
        input. The user information is provided as the self.user_inobj
        object, which gets parsed and redistributed here.
        """
        
        if not(self.user_inobj) :
            ab.WP("No user_inobj? Nothing to do.")
            return 0

        # shorter name to use, and less confusing with 'self' usage
        io = self.user_inobj

        if io.user_opts is not None :
            self.user_opts = io.user_opts
        if io.argv is not None :
            self.argv = io.argv

        # general variables
        if io.verb is not None :
            self.verb = io.verb
        if io.overwrite is not None :
            self.overwrite = io.overwrite
        if io.do_clean is not None :
            self.do_clean = io.do_clean
        if io.do_log is not None :
            self.do_log = io.do_log

        # main data variables
        if io.inset is not None :
            self.inset = io.inset
        if io.prefix is not None :
            self.prefix = io.prefix
        if io.child_dsets is not None :
            self.child_dsets = copy.deepcopy(io.child_dsets)
        if io.child_prefixes is not None :
            self.child_prefixes = io.child_prefixes
        if io.child_outdir is not None :
            self.child_outdir = io.child_outdir
        if io.child_suffix is not None :
            self.child_suffix = io.child_suffix

        # control variables
        if io.do_purge_obl is not None :
            self.do_purge_obl = io.do_purge_obl
        if io.workdir is not None :
            self.workdir = io.workdir
        if io.do_qc is not None :
            self.do_qc = io.do_qc

        return 0


    def basic_setup(self):
        """Run through basic checks of what has been input, and fill in any
        further information that is needed (like wdir, etc.)"""

        # check basic requirements

        if not(self.inset) :
            ab.EP("Need to provide an inset")
        else:
            nfail = au.check_all_dsets_exist([self.inset], label='inset', 
                                             verb=self.verb)
            if nfail :
                ab.EP("Failed to load inset")

            # get AFNI view of inset
            cmd  = '3dinfo -av_space ' + self.inset
            com  = ab.shell_com(cmd, capture=1)
            stat = com.run()
            avsp = com.so[0].strip()
            self.inset_avsp = avsp

        if not(self.prefix) : 
            ab.EP("Need to provide a prefix")

        # check various requirements/restrictions on -child_* opts
        if self.nchild :
            tmp = check_child_opt_usage(self.nchilds, self.child_prefixes,
                                       self.child_outdir, self.child_suffix)
            if tmp : sys.exit(-1)

            # check existence of child_dsets
            nfail = au.check_all_dsets_exist(self.child_dsets, label='child',
                                             verb=self.verb)
            if nfail :
                ab.EP("Failed to load at least one child_dset")
      
        # generate basic items

        # generate wdir with random component, if none provided
        if not(self.workdir) :
            cmd  = '3dnewid -fun11'
            com  = ab.shell_com(cmd, capture=1)
            stat = com.run()
            rstr = com.so[0].strip()
            self.workdir = '__wdir_obliquity_' + rstr

        # hold pieces of output file and dir names (outdir, etc.)
        self.prefix_nobj = LON.NameObj(name=self.prefix, 
                                       avsp=self.inset_avsp)

        # convert bool-ish opts to bools
        self.do_clean       = au.convert_to_bool_yn10(self.do_clean)
        self.do_qc          = au.convert_to_bool_yn10(self.do_qc)

        return 0


    # ----- decorators

    @property
    def nchild(self):
        """number of child_dsets"""
        return len(self.child_dsets)

    @property
    def argv_str(self):
        """string form of argv, like for history"""
        if not(self.argv is None) :
            return ' '.join(self.argv)
        else:
            return ''

# -----------------------------------------------------------------------

def purge_obliquity_from_header( inset, prefix, 
                                 method='keep_origin_raw', 
                                 write_aff_mats = True,
                                 do_clean = False,
                                 overwrite = '',
                                 verb = 1 ):
    """Purge (= remove) obliquity from the header in one of the following
allowed ways.  The method kwarg string maps to the actual 3drefit
option via this dictionary in the module:   DICT_purge_obl_header.

None of these will lead to regridding, but the choice affects what is
done to preserve (or not) the coordinate origin

Using write_aff_mats will lead to also outputting the extracted affine
matrix that would send output dset here to same place that
'3dWarp -deoblique DSET_IN' would.

Parameters
----------
inset : str
    name of input dset
prefix : str
    name out output dset
method : str
    name of obliquity_tool.py's header-deobliquing method to use;
    must be in known list, matching program options
write_aff_mats : bool
    do we write the extracted affine matrix out?
do_clean : bool
    remove working dir when done (or not)
overwrite : str
    either '' or '-overwrite', for whether we can overwrite the output 
    dset or not
verb : int
    if verb level > 2, then add '-echo' to output each line of tcsh
    script before executing

Returns
-------
stat : int
    status of running the adjunct_deob* cmd
so : str
    captured standard output
se : str
    captured standard err

    """

    if verb :
        ab.IP("Removing obliquity from header via mode: " + method)

    # verify selected method
    if method not in LIST_purge_obl_header_keys :
        txt = "The method '{}' is not in the known list:\n ".format(method)
        txt+= STR_purge_obl_header_keys
        ab.EP(txt)

    # build cmd
    cmd = 'adjunct_deob_around_origin '
    cmd+= ' {overwrite} '.format(overwrite=overwrite)
    cmd+= ' -input "{inset}" '.format(inset=inset)
    cmd+= ' -prefix {prefix} '.format(prefix=prefix)
    cmd+= DICT_purge_obl_header[method]
    if write_aff_mats :
        cmd+= ' -write_aff_mats '
    if not(do_clean) :
        cmd+= ' -no_clean '
    if verb > 2 :
        cmd+= ' -echo '

    if verb > 1 :
        tmp, cmd_frmt = lfcs.afni_niceify_cmd_str(cmd)
        ab.IP("... via this cmd:\n" + cmd_frmt)

    # execute cmd
    com  = ab.shell_com(cmd, capture=1)
    stat = com.run()
    so   = '\n'.join(com.so)
    se   = '\n'.join(com.se)

    # check for runtime errors
    if stat :
        txt = wrap_error_exit_text(cmd.split()[0], se)
        ab.EP1(txt)

    return stat

def wrap_error_exit_text(prog, se):
    """Format some of the output text messaging for failure exits.

Parameters
----------
prog : str
    name of program
se : str
    stderr text to output

Returns
-------
txt : str
    string of formatted text
"""

    txt = "early exit when running program: {}\n".format(prog)
    txt+= "stderr is:\n"
    txt+= "-"*40 + "\n"
    txt+= se + "\n"
    txt+= "-"*40 + "\n"

    return txt

# ----------------------------------------------------------------------------

def check_child_opt_usage(nchild, child_prefixes, child_outdir, child_suffix):
   """Check that the input and output specification for child dsets is
valid.

This function exists so we can run the same ~nontrivial checks in a
couple places without repeating functionality.  (It started as a
method in one object, but the variables were duplicated in a second one.)

Parameters
----------
nchild : int
    number of child_dsets input
child_prefixes : list (or None)
    list of output dset names
child_outdir : str (or None)
    name of output dir
child_suffix : str (or None)
    name of suffix to include to output files

Returns
-------
ok : int
    ok is 0 for valid, and nonzero for invalid usage
   """

   if nchild :
      if child_prefixes is None and \
         child_outdir is None and \
         child_suffix is None :
         BASE.EP1("must use an -child_* output opt when using -child_dsets")
         return -1
      elif child_prefixes is not None and \
         not(child_outdir is None and child_suffix is None) :
         BASE.EP1("cannot use -child_prefixes with other -child_* output opts")
         return -1
      elif child_prefixes is not None and len(child_prefixes) != nchilds :
         BASE.EP1("if using -child_prefixes, must match number of child dsets")
         return -1
   else:
       if not(child_prefixes is None and child_outdir is None and \
              child_suffix is None) :
         BASE.EP1("cannot use any -child_* output opt without -child_dsets")
         return -1

   return 0

# ============================================================================

if __name__ == "__main__" :

    # an example use case
    print("++ No example")

