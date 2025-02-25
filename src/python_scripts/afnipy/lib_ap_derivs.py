#!/usr/bin/env python

import sys
import os
import json
import copy

from   afnipy import afni_base     as ab
from   afnipy import afni_util     as au
from   afnipy import lib_ss_review as lssr

# ==========================================================================

DEF_deriv_dir = 'bids_deriv'  # def name of dir mapped from AP results

# Dictionary from AFNI's '3dinfo -space ..' to BIDS-deriv "standard
# template identifiers (AKA coordinate systems)listed template space
# names, as defined here:
# https://bids-specification.readthedocs.io/en/stable/appendices/coordinate-systems.html#standard-template-identifiers
aspace_to_bcoors = {
    # 'ICBM452Warp5Space'
    # 'IXI549Space'
    # 'fsaverage'
    # 'fsaverageSym'
    # 'fsLR'
    # 'MNIColin27'
    # 'MNI152Lin'
    # MNI152NLin2009[a-c][Sym|Asym]
    'MNI_2009c_asym' : 'MNI152NLin2009cAsym',
    # 'MNI152NLin6Sym'
    # 'MNI152NLin6ASym'
    # 'MNI305'
    # 'NIHPD'
    # 'OASIS30AntsOASISAnts'
    # 'OASIS30Atropos'
    'TT_N27' : 'Talairach', # 'Talairach'
    # 'UNCInfant'
} 
# ... and add in the unofficial ones, which BIDS deriv doesn't include
# at present
aspace_to_bcoors.update({
    'MNI' : 'MNI',
    'MNI_27' : 'MNI_27',
    'IBT_C1' : 'IBT_C1',             # Indian Brain Template
    'IBT_C2' : 'IBT_C2',             # Indian Brain Template
    'IBT_C3' : 'IBT_C3',             # Indian Brain Template
    'IBT_C4' : 'IBT_C4',             # Indian Brain Template
    'IBT_C5' : 'IBT_C5',             # Indian Brain Template
    'HaskinsPeds' : 'HaskinsPeds',   # Haskins Pediatric Template
    'NMT2' : 'NMT2',                 # NIH Macaque Template
    'MBMv3' : 'MBMv3',               # Marmoset Brain Map
})

# ==========================================================================

# uvars for the ap_deriv_obj.map_simple_3dcopy() method to process
list_uvars_simple_3dcopy = [
    'align_anat',
    'copy_anat',
    'errts_dset',
    'final_anat',
    'final_epi_dset',
    'mask_dset',
    'surf_vol',
    'tcat_dset',
    'template',
    'tsnr_dset',
    'vr_base_dset',
]

# uvars for the ap_deriv_obj.map_3dTsplit4D() method to process
list_uvars_3dTsplit4D = [
    'stats_dset',
]

# ==========================================================================

class ap_deriv_obj:
    """An object for holding information for mapping AP results directory
    information to a BIDS derivative-ish one.

    """

    def __init__( self, ap_res_dir, deriv_dir = '',
                  verb = 0, ow_mode_top = 'simple_ok',
                  ow_mode_subj = 'backup' ):
        """Create object holding mapping information from AP output to 
        derivatives. 

        Use the map_all() method after instantiation to make the
        mapping of data happen.
        """

        self.verb          = verb             # int, verbosity level
        self.ow_mode_top   = ow_mode_top      # str, manage overwriting topdir
        self.ow_mode_subj  = ow_mode_subj     # str, manage overwriting subjdir

        # AP results info
        self.ap_res_dir    = ''               # str, path of AP results dir
        self.ap_uvars_json = ''               # str, uvars JSON in AP res dir
        self.ap_ssdict     = {}               # dict, uvars (single subj dict)

        # deriv info
        self.deriv_dir     = ''               # str, path of deriv dir
        self.deriv_ssdict  = {}               # dict, mapping of uvars in deriv

        # info (to come via: uvars_json->ap_ssdict)
        self.subj          = ''               # str, subj ID (from ap_ssdict)
        self.map_dict      = {}               # dict, record map of AP->deriv

        # ----- set and check vars from inputs
        au.is_valid_ow_mode(ow_mode_top)
        au.is_valid_ow_mode(ow_mode_subj)

        self.set_ap_res_dir(ap_res_dir)
        self.set_ap_uvars_json()
        self.set_ap_ssdict()

        self.set_deriv_dir(deriv_dir)
        self.set_deriv_ssdict()

        # ----- do main work (now just called as separate methods
        #self.map_all()

    # ----------------------- functions

    def set_ap_res_dir(self, x):
        """Set AP results dir; rstrip any '/'."""
        y = x.rstrip('/')
        if not os.path.exists(y) :
            ab.EP("AP results dir '{}' does not exist".format(y))
        self.ap_res_dir = y

    def set_ap_uvars_json(self):
        """Set ap uvars fname; make sure it exists."""

        fname  = 'out.ss_review_uvars.json'
        ffname = self.ap_res_dir + '/' + fname
        if not os.path.isfile(ffname) :
            ab.EP("AP uvars json '{}' does not exist".format(ffname))
        self.ap_uvars_json = ffname

    def set_ap_ssdict(self):
        """Read uvar json to dict; set subj ID"""
        with open(self.ap_uvars_json, 'r') as fff:
            self.ap_ssdict = json.load(fff)    
        if 'subj' in self.ap_ssdict.keys() :
            self.subj = self.ap_ssdict['subj']
        else:
            ab.EP("No 'subj' entry in uvars dict?")

    def set_deriv_ssdict(self):
        """Map dict of uvars to derivs names (as many as used)"""
        self.deriv_ssdict = map_uvars_to_deriv_names(self.ap_ssdict,
                                                     self.ap_res_dir,
                                                     verb=self.verb)

    def set_deriv_dir(self, x=''):
        """Set deriv dir; rstrip any '/'."""
        y = x.rstrip('/')
        if y == '' :
            self.deriv_dir = self.ap_res_dir + '/' + DEF_deriv_dir
        else:
            self.deriv_dir = y

    def make_deriv_subdir(self, sss):
        """Make a subdirectory sss, if it doesn't exist already.  The way this
        function is called, sss already contains the name of the
        deriv_dir.  It is possible for sss to have an arbitrary depth
        of directory names.  We don't check for overwriting at this
        level."""

        subdir   = sss.rstrip('/')

        if not os.path.exists(subdir):
            cmd    = '''\\mkdir -p {}'''.format(subdir)
            com    = ab.shell_com(cmd, capture=1)
            stat   = com.run()

        return 0

    def map_all(self):
        """The main control function for mapping AP results to derivs
        renaming, basically going through all possible uvar keys;
        start by making output dir. Over time, this function will grow
        in number of uvar keys mapped."""

        # make top-level directory
        _tmp2 = au.make_new_odir(self.deriv_dir, ow_mode=self.ow_mode_top)
        
        # make subj-level directory
        sdir  = self.deriv_dir + '/' + self.subj
        _tmp2 = au.make_new_odir(sdir, ow_mode=self.ow_mode_subj)

        # process all the uvars that are simply dsets to rename+copy
        for uvar in list_uvars_simple_3dcopy:
            _tmp3 = self.map_simple_3dcopy(uvar)

        # process all the uvars that are split/burst in time
        for uvar in list_uvars_3dTsplit4D:
            _tmp4 = self.map_3dTsplit4D(uvar)

        # try to copy over the typical log file, output.proc.${subj}
        _tmp5 = self.map_log_output()
        
        return 0

    def map_log_output(self, log_ap=None):
        """Check if the standard output.proc.${subj} log file exists; it would
        be created using the AP opt '-execute', and be stored parallel
        to the AP results dir (by default). User could also enter
        their own log_ap name to check for (including path)."""

        if not(log_ap) :
            # look for what AP's '-execute' would produce
            fname  = 'output.proc.' + self.ap_ssdict['subj']
            aaa    = os.path.expanduser(self.ap_res_dir)
            bbb    = os.path.abspath(aaa)
            ccc    = os.path.dirname(bbb)
            log_ap = ccc + '/' + fname

        if not os.path.exists(log_ap) :
            ab.WP("Cannot find log file to copy: {}".format(log_ap))
            stat = -2
        else:
            # make derivative subdir for logs
            sss    = self.ap_ssdict['subj'] + '/' + 'logs'
            subdir = self.deriv_dir + '/' + sss
            self.make_deriv_subdir(subdir)

            # make bids deriv filename (with path)
            fname   = log_ap.split('/')[-1]
            log_drv = subdir + '/' + fname

            if self.verb :  
                ab.IP("map log: {:>21s} -> {}".format(fname, 
                                                      sss + '/' + fname))

            # copy text file
            cmd  = '''\cp {} {}'''.format(log_ap, log_drv)
            com  = ab.shell_com(cmd, capture=1)
            stat = com.run()

        return stat

    def map_simple_3dcopy(self, uvar):
        """Check if uvar (that can be 3dcopy'ed) is present; if so, map it
        over."""

        V, dset_ap, dset_drv = self.prep_map_simple(uvar)
        if V : return -1

        # list of uvars whose dsets are full names, with ext
        list_fullname = ['template']
        if not(uvar in list_fullname) :
            # the extension depends on that of the uvar's value
            ext = make_dset_ext(self.ap_ssdict[uvar])
            dset_drv+= ext

        # check about needing to first make a subdir
        if '/' in dset_drv :
            subdir = os.path.dirname(dset_drv)
            self.make_deriv_subdir(subdir)

        # are we overwriting?
        ow   = '-overwrite' #* int(self.overwrite)
        cmd  = '''3dcopy {} {} {}'''.format(ow, dset_ap, dset_drv)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()

        return stat

    def prep_map_simple(self, uvar):
        """Supplementary function, to check if a uvar is used, and then to
        prepare+return filenames if it is. This is the simple (but
        common) case of 1-to-1 mapping."""

        # default to return, if uvar is _not_ used
        RETURN_NULL = (-1, '', '')

        # check dependencies to proceed
        ldep = [uvar]
        if not check_dep(self.ap_ssdict, ldep) :  
            if self.verb > 1 :  
                ab.IP("no map uvar: {:>17}".format(uvar))
            return RETURN_NULL

        if self.verb :  
            ab.IP("map uvar: {:>20s} -> {}".format(uvar, 
                                                   self.deriv_ssdict[uvar]))

        # names to/from
        dset_ap  = self.ap_res_dir + '/' + self.ap_ssdict[uvar]
        dset_drv = self.deriv_dir  + '/' + self.deriv_ssdict[uvar]

        self.map_dict[self.ap_ssdict[uvar]] = self.deriv_ssdict[uvar]

        return 0, dset_ap, dset_drv

    def map_3dTsplit4D(self, uvar):
        """Check if uvar (that can be 3dTsplit4D'ed) is present; if so, map it
        over."""

        V, dset_ap, dset_drv = self.prep_map_simple(uvar)
        if V : return -1

        # the extension depends on that of the uvar's value
        # **** check about surface ones *****
        ext = make_dset_ext(self.ap_ssdict[uvar])
        dset_drv+= ext

        # check about needing to first make a subdir
        if '/' in dset_drv :
            subdir = os.path.dirname(dset_drv)
            self.make_deriv_subdir(subdir)

        # are we overwriting?
        ow   = '-overwrite' #* int(self.overwrite)
        cmd  = '''3dTsplit4D -keep_datum -label_prefix '''
        cmd += ''' {} -prefix {} {}'''.format(ow, dset_drv, dset_ap)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()

        return stat


# --------------------------------------------------------------------------

def map_uvars_to_deriv_names(U, ap_res_dir, verb=0):
    """A function that defines a primary reference object. For every
relevant uvar in the dictionary U (also called the ap_ssdict), write
what the name would be in the derivatives directory. Store the output
in a new dictionary D, to return.

Parameters
----------
U : dict
    input dictionary of uvars
ap_res_dir : str
    path to AP results directory
verb : int
    verbosity level

Returns
-------
D : dict
    output dictionary, whose keys are a subset of those in U; values
    are the names (or, for datasets that will be 3dcopy'ed, only the
    prefixes) of the files in the new derivs directory.

    """

    # init dict of uvars plus other info for parts of filename
    UC = {}

    # set up UC dict with null values from list of all possible uvars
    all_possible_uvar = [x[0] for x in lssr.g_ss_uvar_fields]
    for uvar in all_possible_uvar:
        UC[uvar] = 'EMPTY'

    # ... and then overwrite with any values we actually have
    for uvar in U:
        UC[uvar] = U[uvar]

    # this is how we deal with possibility that a ${ses} variable may
    # or may not be present (sigh)
    UC['ss_name'] = UC['subj']         # to be used in file names
    UC['ss_path'] = UC['subj']         # to be used in path struct
    if 'ses' in U.keys() :
        UC['ss_name']+= '_' + UC['ses']
        UC['ss_path']+= '/' + UC['ses']

    # ... then add any possible special ones, if needed
    if not('taskname' in UC.keys()) :
        UC['taskname'] = 'TASKNAME'
    if not('type_anat' in UC.keys()) :
        UC['type_anat'] = 'T1w'
    if not('spacename_anat' in UC.keys()) :
        # spacename for original/copied anat
        UC['spacename_anat'] = 'anat'  
    if not('spacename_final_epi' in UC.keys()) :
        # spacename of final EPI dset
        dset  = ap_res_dir + '/' + U['final_epi_dset']
        space = get_drv_spacename(dset, orig_map='boldref')
        UC['spacename_final_epi'] = space
    if not('spacename_final_anat' in UC.keys()) :
        # spacename of final anat dset
        dset  = ap_res_dir + '/' + U['final_anat']
        space = get_drv_spacename(dset, orig_map='anat')
        UC['spacename_final_anat'] = space
    if not('preproc_yn_final_anat' in UC.keys()) :
        # for final_anat, is preproc'ed? Not sure this matters much
        UC['preproc_yn_final_anat'] = 'preproc'

    # Make dict. NB: many file exts get added later, see make_dset_ext()
    # !!! add possibility of session level in here, too
    # !!! is echo needed in here?
    D = {}
    
    #anat aligned with orig EPI: "${subj}_task-${taskname}_[run-${runnum}_][echo-${echonum}_]space-boldref_${type_anat}.nii.gz
    D['align_anat']  = "{ss_path}/func/{ss_name}_task-{taskname}_space-boldref_{type_anat}".format(**UC)
    
    #${subj}_[${ses}_]space-orig_${type_anat}.nii.gz
    D['copy_anat']  = "{ss_path}/anat/{ss_name}_space-{spacename_anat}_{type_anat}".format(**UC)

    # ${subj}_[${ses}_]task-${taskname}_space-${spacename}_desc-resid_bold.nii.gz
    D['errts_dset']  = "{ss_path}/func/{ss_name}_task-{taskname}_space-{spacename_final_epi}_desc-resid_bold".format(**UC)

    #${subj}_[${ses}_]space-${spacename}_desc-${preprocessedornot}_${type_anat}.nii.gz
    D['final_anat']  = "{ss_path}/anat/{ss_name}-{spacename_final_anat}_desc-{preproc_yn_final_anat}_{type_anat}".format(**UC)

    #${subj}_[${ses}_]task-${taskname}_[run-${runnum}_][echo-${echonum}_]space-${spacename}_boldref.nii.gz
    D['final_epi_dset'] = "{ss_path}/func/{ss_name}_task-{taskname}_space-{spacename_final_epi}_desc-resid_bold".format(**UC)

    # ${subj}_[${ses}_]task-${taskname}_[run-${runnum}_][echo-${echonum}_]space-${spacename}_desc-brain_mask.nii.gz
    D['mask_dset']   = "{ss_path}/func/{ss_name}_task-{taskname}_space-{spacename_final_epi}_desc-brain_mask".format(**UC)

    # ${subj}_[${ses}_]task-${taskname}_space-${spacename}_contrast-${GLT label part}_stat-${single letter stat}_statmap.nii.gz
    D['stats_dset']  = "{ss_path}/func_stats/{ss_name}_task-{taskname}_space-{spacename_final_epi}_contrast-".format(**UC)

    # ${subj}_[${ses}_]space-${spacename}_desc-surfvol_${type_anat}.nii.gz
    D['surf_vol']    = "{ss_path}/anat/{ss_name}_space-{spacename_anat}_{type_anat}_desc-surfvol_{type_anat}".format(**UC)

    # ${subj}_[${ses}_]task-${taskname}_[run-${runnum}_][echo-${echonum}_]space-orig_desc-tcat_bold.nii.gz
    D['tcat_dset']   = "{ss_path}/func/{ss_name}_task-{taskname}_space-orig_desc-tcat_bold".format(**UC)

    # <just direct copy of filename>
    D['template']    = "{ss_path}/anat/{template}".format(**UC)

    # ${subj}_[${ses}_]task-${taskname}_space-${spacename}_stat-residtsnr_statmap.nii.gz
    D['tsnr_dset']   = "{ss_path}/func/{ss_name}_task-{taskname}_space-{spacename_final_epi}_stat-residtsnr_statmap".format(**UC)

    # ${subj}_[${ses}_]task-${taskname}_[run-${runnum}_][echo-${echonum}_]space-orig_boldref.nii.gz
    D['vr_base_dset'] = "{ss_path}/func/{ss_name}_task-{taskname}_space-orig_boldref".format(**UC)


    return D

def make_dset_ext(dset, do_zip_nii=True):
    """For a given dset, use the rules defined here to determine what the
file extension for the mapped version of the dset to be. User can
choose whether to zip output NIFTI files."""

    # List of dsets to get a particular output ext (all_vol get
    # NIFTI). Order of checks matters in some cases, like for
    # .aff.1D/.1D
    all_vol   = ['.nii', '.nii.gz', '.BRIK', '.BRIK.gz', '.HEAD', '.HEAD.gz']
    all_spec  = ['.spec']
    all_niml  = ['.lh.niml.dset', '.rh.niml.dset', '.both.niml.dset']
    all_gii   = ['.gii']
    all_aff1D = ['.aff.1D']
    all_1D    = ['.1D']
    all_txt   = ['.txt']
    all_dat   = ['.dat']

    for vol in (all_vol):
        if dset.endswith(vol) :
            return '.nii' + ('.gz' * int(do_zip_nii))

    # each niml hemi keeps its hemi name
    for niml in (all_niml):
        if dset.endswith(niml) :
            return niml

    for spec in (all_spec):
        if dset.endswith(spec) :
            return '.spec'

    for gii in (all_gii):
        if dset.endswith(gii) :
            return '.gii'

    for aff1D in (all_aff1D):
        if dset.endswith(aff1D) :
            return '.aff.1D'
    for _1D in (all_1D):
        if dset.endswith(_1D) :
            return '.1D'

    for txt in (all_txt):
        if dset.endswith(txt) :
            return '.txt'

    for dat in (all_dat):
        if dset.endswith(dat) :
            return '.dat'

    # fail on non-recognition
    ab.EP("I don't recognize the ext on this file to map: {}".format(dset))

# --------------------------------------------------------------------------

def check_dep(D, L):
    '''Does dictionary D contain *each* of the elements of the list L?'''

    ttt = type(L)
    if ttt != list :
        ab.EP("Input L to check_dep() must be a list, not: {}".format(ttt))

    for x in L:
        if not(x in D.keys()) :  return 0

    return 1

def quick_3dinfo(opt_list, dset_list):
    """
Parameters
----------
opt_list : list
    list of strings of options for 3dinfo
dset_list : list
    list of strings of dset names

Returns
-------
lll : list
    list of strings (one string per line) output 
"""

    opt_str  = ' '.join(opt_list)
    dset_str = ' '.join(dset_list)

    cmd    = '''3dinfo {} {}'''.format(opt_str, dset_str)
    com    = ab.shell_com(cmd, capture=1)
    stat   = com.run()
    lll    = com.so

    return lll

def get_drv_spacename(dset, orig_map='ORIG'):
    """For a given dset, return an appropriate(ish) BIDS derivative name,
if the AFNI space is recognized.

Parameters
----------
dset : str
    name of dataset
orig_map : str
    more specific name that original space should be called (or just
    'ORIG' by default)

Returns
-------
space : str
    name of space
"""

    lll    = quick_3dinfo(['-space'], [dset])
    space  = lll[0].split()[0]

    if space == 'ORIG' :
        # original space (likely needs to be mapped to something else,
        # which can be done here or elsewhere)
        return orig_map
    elif space in aspace_to_bcoors.keys():
        # some space known in our lookup dictionary
        return aspace_to_bcoors[space]
    else:
        return 'UNKNOWN'

# ===========================================================================
# ===========================================================================

if __name__ == "__main__" :

    pass
