#!/usr/bin/env python

# general functions for use by uber*.py

import sys, os
from time import asctime
import glob

import afni_base as BASE
import afni_util as UTIL
import lib_subjects as SUBJ

DEF_UBER_DIR = 'uber_results'        # top directory for output
DEF_TOP_SDIR = 'subject_results'     # top subject dir under uber_results
DEF_SUBJ_ID  = 'SUBJ'                # default subject, if none is known
DEF_GROUP_ID = 'G1'                  # default group, if none is known

g_history = """
  uber_subject.py history

    0.0  Feb 14, 2011: initial revision
         - functional GUI for anat/epi/stim, generates simple AP command
    0.1  Feb 15, 2011:
         - added CLI (command line interface)
         - additional help
    0.2  Feb 16, 2011: reorg (move files and functions around)
    0.3  Feb 16, 2011: epi or stim list from command line can init order/labels
    0.4  Feb 20, 2011:
         - added interfaces for 'expected' options
           (tcat_nfirst, volreg_base, motion_limit)
         - added corresponding volreg_warp abilities
         - small changes to top_dir use
    0.5  Mar  2, 2011
         - added actual write and execution of proc script
           (the exec method will probably change to be asynchronous)
         - added cvars instance to GUI
         - specify script name and overwrite in AP command
         - added control vars file_ap, file_proc and verb
         - write ap_command through AP_Subject class
         - allow numeric subject vars as text
         - store AP_Subject instance in GUI instance
         - added View menu to GUI
         - added menu item for browsing AFNI Message Board
    0.6  Mar  3, 2011
         - applied subj_dir control var, for location of output files
           (actions will be: cd sdir ; actions... ; cd -)
         - added -exec_ap_command/-exec_proc_script for command line actions
         - added -qt_gui for PyQt4 options
         - added -cvar to set control vars
         - added copy_script control var
         - added view 'proc output' and 'rvars' actions to View menu
         - created rvars, for access of subject results variables
         - wrote a little more command line help
         - moved exec functions to subject object
    0.7  Mar  7, 2011
         - added menu items to show: py command and command windows
         - exec proc script via new TcshCommandWindow class
         - added ProcessWindow class (exec system commands (not shell))
         - added PyCommandWindow class (exec internal python commands)
         - init rvars file names early, rather than at exec use time
         - improved file application
         - do not copy orig files, do that only via uber_proc.py
    0.8  Mar  8, 2011
         - moved menu items around
         - added 'view: uber_subject.py command' menu item
         - replaced warningMessage/errorMessage with guiWarning/guiError
         - small changes to lib_subjects.py
    0.9  Mar  9, 2011
         - minor command window changes
         - some prep for uber_proc.py control
         - subject and group ID are now required
    0.10 Mar 14, 2011
         - add gltsym help web link
         - added PyQt4 install notes and changed about format
         - other random updates
    0.11 Mar 15, 2011 :
         - added -regress_make_ideal_sum
         - added more subject variables
         - minor GUI text changes
    0.12 Mar 20, 2011 :
         - added hidden menu item to view GUI vars
         - process new subject variables gltsym, gltsym_label
         - added group box for symbolic GLTs (init to hidden)
         - added help tips to gbox buttons
         - if subj_dir, save uber_subject.py command
         - process toggle boxes as yes/no, rather than 1/0
    0.13 Mar 21, 2011 :
         - GUI/command vars are all strings, convert when creating AP_Subject
         - added group box for extra regress options
         - new subject variables outlier_limit, regress_jobs, regress_GOFORIT,
                                 reml_exec, run_clustsim, compute_fitts
         - slight change to format of gltsym labels
         - toggle buttons are yes/no strings
         - added -save_ap_command option
         - can apply types when merging VarsObject instances 
         - moved set_var_str_from_def to lib_uber_subject.py
    0.14 Mar 22, 2011 :
         - show processing status in 'exec proc script' window
         - added clear all options/fields File menu items
         - added -todo option
    0.15 Mar 23, 2011 :
         - moved gltsym group box to below stim
         - save output from afni_proc.py command
         - small mac install update
    0.16 Mar 24, 2011 :
         - added align option box: cost, giant_move
         - added tlrc option box: base, skull strip, OK_maxite
         - adjusted table resizing
         - added -help_install
    0.17 Mar 29, 2011 : changed subject directory to group.GROUP/subj.SUBJ
    0.18 Apr 06, 2011 : make table size depend on font
    0.19 Apr 07, 2011 : backport for Ubuntu 9 (thanks to J Bodurka)
         - call 'connect' from top-level, since old instances did not have
           each signal as an attribute (use QtCore.SIGNAL(sig_name))
         - no SaveAs QKeySequence in older version?
    0.20 Apr 11, 2011 : fixed lost warnings for no sid/gid
    0.21 Apr 19, 2011 : moved set_var_str_from_def to lib_subjects for now
    0.22 Apr 28, 2011 : mostly prep of library for uber_align.py
         - moved many functions to lib_subjects or afni_util.py
         - added option -show_default_vars
    0.23 May 11, 2011 : small help/todo update
    0.24 May 19, 2011 : revert to /usr/bin/env python
         - fink use may be ready on the macs (tried: fink install pyqt4-py27)
    0.25 Sep 22, 2011 : altered spacing and made other minor changes
    0.26 Oct  5, 2011 : do not re-create proc script on proc execution
         - was losing any user changes between creation and execution
    0.27 Oct 11, 2011 : small -help_install update
    0.28 Oct 18, 2011 :
         - added blur size control
         - removed requirement of stim timing files
    0.29 Nov 22, 2011 : allow for passing variables directly, not via -svar
         - added accompanying -show_svar_dict option
"""

g_version = '0.29 (November 22, 2011)'

# ----------------------------------------------------------------------
# global definition of default processing blocks
g_def_blocks      = ['tshift', 'volreg', 'blur', 'mask', 'scale', 'regress']
g_def_blocks_anat = ['tshift', 'align', 'tlrc', 'volreg', 'blur', 'mask',
                     'scale', 'regress']
g_vreg_base_list  = ['first', 'third', 'last']
g_def_vreg_base   = 'third'
g_align_cost_list = ['lpc', 'lpc+ZZ', 'lpc+', 'lpa', 'nmi', 'ls']
g_def_align_cost  = 'lpc'
g_tlrc_base_list  = ['TT_N27+tlrc', 'TT_avg152T1+tlrc', 'TT_icbm452+tlrc',
                     'MNI_avg152T1+tlrc']
g_def_tlrc_base   = 'TT_N27+tlrc'


# ----------------------------------------------------------------------
# global definitions of subject defaults for single subject analysis

# ---- control values passed in for class actions ----
g_ctrl_defs = SUBJ.VarsObject("uber_subject control defaults")
g_ctrl_defs.subj_dir      = '.'         # destination for scripts and results
g_ctrl_defs.copy_scripts  = 'yes'       # do we make .orig copies of scripts?
g_ctrl_defs.verb          = 1           # verbose level

# ---- resulting values returned after class actions ----
g_res_defs = SUBJ.VarsObject("uber_subject result variables")
g_res_defs.file_ap       = ''           # file name for afni_proc.py command
g_res_defs.file_proc     = ''           # file name for proc script
g_res_defs.results_dir   = ''           # results directory from proc script
g_res_defs.output_ap     = ''           # output from running AP command
g_res_defs.output_proc   = ''           # output from running proc script

# ----------------------------------------------------------------------
# a global definition of subject defaults for single subject analysis
g_subj_defs = SUBJ.VarsObject("Single Subject Dialog defaults")
g_subj_defs.blocks        = []
g_subj_defs.sid           = ''          # subject ID    (no spaces - required)
g_subj_defs.gid           = ''          # group ID      (no spaces)
g_subj_defs.anat          = ''          # anat dset name (probably .HEAD)
g_subj_defs.get_tlrc      = 'no'        # yes/no: include anat+tlrc
g_subj_defs.epi           = []          # EPI dset name list
g_subj_defs.epi_wildcard  = 'no'        # use wildcard form for EPIs
g_subj_defs.stim          = []          # EPI dset name list
g_subj_defs.stim_wildcard = 'no'        # use wildcard form for EPIs
g_subj_defs.stim_label    = []          # label for each stim file
g_subj_defs.stim_basis    = []          # basis functions: empty=GAM,
                                        #   valid lengths: 0, 1, len(stim)
# expected
g_subj_defs.tcat_nfirst   = 0           # first TRs to remove from each run
g_subj_defs.volreg_base   = g_def_vreg_base  # in g_vreg_base_list, or ''
g_subj_defs.motion_limit  = 0.3         # in mm
g_subj_defs.blur_size     = 4.0         # in mm

# symbolic GLTs
g_subj_defs.gltsym           = []       # list of -gltsym options (sans SYM:)
g_subj_defs.gltsym_label     = []       # list of -gltsym options (sans SYM:)

# extra regress opts
g_subj_defs.outlier_limit    = 0.0
g_subj_defs.regress_jobs     = 1
g_subj_defs.regress_GOFORIT  = 0
g_subj_defs.reml_exec        = 'no'     # only 'yes' or 'no'
g_subj_defs.run_clustsim     = 'yes'    # only 'yes' or 'no'
g_subj_defs.compute_fitts    = 'no'     # only 'yes' or 'no'

g_subj_defs.align_cost       = g_def_align_cost # -cost in align_opts_aea
g_subj_defs.tlrc_base        = g_def_tlrc_base  # template base for -tlrc_base
g_subj_defs.align_giant_move = 'no'     # y/n: -giant_move in align_opts_aea
g_subj_defs.tlrc_ss          = 'yes'    # y/n : 'no' implies -tlrc_no_ss
g_subj_defs.tlrc_ok_maxite   = 'no'     # pass -OK_maxite to @auto_tlrc

# ...
g_subj_defs.regress_opts_3dD = ''       # extra options for 3dDeconvolve
g_subj_defs.align_opts_aea   = ''       # extra aea opts, e.g. -AddEdge
g_subj_defs.tlrc_opts_at     = ''       # extra at opts

g_svar_dict = {
   'blocks'             : 'set list of processing blocks to apply',
   'sid'                : 'set subject ID',
   'gid'                : 'set group ID',
   'anat'               : 'set anatomical dataset name',
   'get_tlrc'           : 'yes/no: get any +tlrc anat dset',
   'epi'                : 'set list of EPI datasets',
   'epi_wildcard'       : 'yes/no: use wildcard for EPI dsets',
   'stim'               : 'set list of stim timing files',
   'stim_wildcard'      : 'yes/no: use wildcard for stim files',
   'stim_label'         : 'set stim file labels',
   'stim_basis'         : 'set basis functions for stim classes',

   'tcat_nfirst'        : 'set number of TRs to remove, per run',
   'volreg_base'        : 'set volreg base string (first/third/last)',
   'motion_limit'       : 'set per-TR motion limit, in mm',
   'blur_size'          : 'set blur size, in mm',

   'gltsym'             : 'specify list of symbolic GLTs',
   'gltsym_label'       : 'set corresponding GLT labels',

   'outlier_limit'      : 'specify outlier limit for censoring',
   'regress_jobs'       : 'number of jobs to use in 3dDeconvolve',
   'regress_GOFORIT'    : 'set GOFORIT level in 3dDeconvolve',
   'reml_exec'          : 'yes/no: whether to run 3dREMLfit',
   'run_clustsim'       : 'yes/no: whether to run 3dClustSim',
   'compute_fitts'      : 'yes/no: whether to just compute the fitts',

   'align_cost'         : 'specify cost function for anat/EPI alignment',
   'tlrc_base'          : 'specify anat for standard space alignment',
   'align_giant_move'   : 'yes/no: use -giant_move in AEA.py',
   'tlrc_ss'            : 'yes/no: whether anat has skull',
   'tlrc_ok_maxite'     : 'yes/no: pass -OK_maxite to @auto_tlrc',

   'regress_opts_3dD'   : 'specify extra options for 3dDeconvolve',
   'align_opts_aea'     : 'specify extra options for align_epi_anat.py',
   'tlrc_opts_at'       : 'specify extra options for @auto_tlrc',
}

# string versions of subject variables, to be used by GUI
g_cdef_strs = g_ctrl_defs.copy(as_strings=1)
g_sdef_strs = g_subj_defs.copy(as_strings=1)

# note: short vars (e.g. with epi)
#   use_dirs      - should we set any directory at all
#   use_tdir      - if it will be used as a parent to short_epi_dir
#   var_edir      - if EPI dir variable should be used ($epi or $top or '')
#   epi_dir       - common full dir prefix to all epi
#   short_epi_dir - either epi_dir or that under top_dir
#   epi           - list of full epi dset names
#   short_epi     - epi names under epi_dir

class AP_Subject(object):
   """subject for single-subject analysis scripting by afni_proc.py
        - svars : single subject variables
        - cvars : control variables
        - rvars : return variables

        ** input vars might be string types, convert on merge

        variables:
           LV            - local variables
           cvars         - control variables
           svars         - subject variables
           ap_command    - generated afni_proc.py command
           errors            --> array of resulting error messages
           warnings          --> array of resulting warning messages
   """
   def __init__(self, svars=None, cvars=None):


      # LV: variables local to this interface, not passed
      self.LV = SUBJ.VarsObject("local AP_Subject vars")
      self.LV.indent = 8                # default indent for AP options
      self.LV.istr   = ' '*self.LV.indent
      self.LV.warp   = ''               # '', 'adwarp', 'warp'
                                        # (how to get to tlrc space)
      self.LV.retdir = ''               # return directory (for jumping around)

      self.cvars = g_ctrl_defs.copy()   # start with default control vars
      self.cvars.merge(cvars, typedef=g_ctrl_defs) # include those passed

      self.svars = g_subj_defs.copy()   # start with default subject vars
      self.svars.merge(svars, typedef=g_subj_defs) # include those passed

      self.rvars = g_res_defs.copy()    # init result vars

      self.set_blocks()                 # choose processing blocks

      self.set_directories()            # data dirs: anat, epi, stim
                                        # (also sets use_dirs, use_adir, etc.)

      self.set_short_names()            # short data dirs: e.g. short_anat

      if self.cvars.verb > 3: self.LV.show('ready to start script:')

      self.set_ap_command()             # fill ap_command, warnings, errors

   def set_ap_command(self):
      """attempt to generate an afni_proc.py command in ap_command
            - keep a list of warnings and errors
            - if there are errors, ap_command might not be filled
      """

      self.errors = []                  # list of error strings
      self.warnings = []                # list of warning strings

      # first assign directories
      self.ap_command  = self.script_init()
      self.ap_command += self.script_set_dirs()
      self.ap_command += self.script_set_vars()

      self.ap_command += self.script_ap_init()
      self.ap_command += self.script_ap_blocks()
      self.ap_command += self.script_ap_anat()
      self.ap_command += self.script_ap_tcat()
      self.ap_command += self.script_ap_epi()
      self.ap_command += self.script_ap_align()
      self.ap_command += self.script_ap_tlrc()
      self.ap_command += self.script_ap_volreg()
      self.ap_command += self.script_ap_blur()
      self.ap_command += self.script_ap_regress()

      # alter ap_command, removing last '\'
      self.ap_command = UTIL.nuke_final_whitespace(self.ap_command)

      if len(self.errors) > 0: return   # if any errors so far, give up

      self.ap_command = UTIL.add_line_wrappers(self.ap_command)

      return

   def write_ap_command(self, fname=''):
      """if fname is set, use it, else generate"""

      if not self.ap_command:
         print '** no afni_proc.py command to write out'
         return 1
      if fname: name = fname
      else:
         if self.svars.sid: name = 'cmd.ap.%s' % self.svars.sid
         else:              name = 'cmd.ap'

      self.rvars.file_ap   = name # store which file we have written to
      self.rvars.output_ap = 'output.%s' % name # file for command output

      if self.cvars.verb>0: print '++ writing afni_proc.py command to %s'%name

      # if requested, make an original copy
      self.LV.retdir = SUBJ.goto_proc_dir(self.cvars.subj_dir)
      if self.cvars.copy_scripts == 'yes': # make an orig copy
         UTIL.write_text_to_file('.orig.%s'%name, self.ap_command, exe=1)
      rv = UTIL.write_text_to_file(name, self.ap_command, exe=1)
      self.LV.retdir = SUBJ.ret_from_proc_dir(self.LV.retdir)

      return rv

   def exec_ap_command(self):
      """execute the script rvars.file_ap, if set
         - return status and command output
      """

      pfile = self.rvars.val('file_ap')
      ofile = self.rvars.val('output_ap')

      # check if the ap script file is set and exists
      if not pfile:
         return 1, '** no file set as afni_proc.py command'
      elif not SUBJ.proc_dir_file_exists(self.cvars.subj_dir, pfile):
         return 1, '** afni_proc.py script not found: %s' % pfile

      # make the command
      if ofile: cstr = 'tcsh %s |& tee %s' % (pfile, ofile)
      else:     cstr = 'tcsh %s' % pfile

      # ---------- do the work ----------
      self.LV.retdir = SUBJ.goto_proc_dir(self.cvars.subj_dir)

      if self.cvars.verb > 0: print "++ executing: %s" % cstr

      cmd = BASE.shell_com('tcsh -c "%s"' % cstr, capture=1)
      cmd.run()
      self.LV.retdir = SUBJ.ret_from_proc_dir(self.LV.retdir)
      # ---------- done ----------

      # possibly make a backup file
      if self.cvars.copy_scripts == 'yes': self.copy_orig_proc()

      # set results_dir, since we might not be the ones to exec proc script
      if not self.rvars.results_dir:
         self.rvars.results_dir = '%s.results' % self.svars.sid

      return cmd.status, '\n'.join(cmd.so)

   def exec_proc_script(self, xterm=0):
      """execute the script rvars.file_proc, if set
         - if the results dir exists, nuke it
         - execute script (if xterm: run in a new xterm)
         - store results directory

         return status and command output
      """

      # check if the proc script is set and exists
      pfile = self.rvars.file_proc
      ofile = self.rvars.output_proc
      if not pfile: 
         return 1, '** proc script file not set'
      elif not SUBJ.proc_dir_file_exists(self.cvars.subj_dir, pfile):
         return 1, '** proc script not found: %s' % pfile

      # make the command
      if ofile: cstr = 'tcsh -xef %s |& tee %s' % (pfile, ofile)
      else:     cstr = 'tcsh -xef %s' % (pfile)

      self.nuke_old_results()   # nuke any old results

      # ------------------------- do the work -------------------------
      self.LV.retdir = SUBJ.goto_proc_dir(self.cvars.subj_dir)

      # execute script
      print '++ executing: %s' % cstr

      capture = 0       # init as output to terminal
      cstr = 'tcsh -c "%s"' % cstr
      if xterm:
         capture = 1
         cstr = 'xterm -e %s' % cstr

      cmd = BASE.shell_com(cstr, capture=capture)
      cmd.run()

      self.LV.retdir = SUBJ.ret_from_proc_dir(self.LV.retdir)
      # ------------------------- done -------------------------

      return cmd.status, '\n'.join(cmd.so)

   def nuke_old_results(self):
      """if the results directory exists, remove it"""

      if self.rvars.results_dir == '': return

      # ------------------------- do the work -------------------------
      self.LV.retdir = SUBJ.goto_proc_dir(self.cvars.subj_dir)
      
      if os.path.isdir(self.rvars.results_dir):
         print '-- nuking old results: %s' % self.rvars.results_dir
         os.system('rm -fr %s' % self.rvars.results_dir)

      self.LV.retdir = SUBJ.ret_from_proc_dir(self.LV.retdir)
      # ------------------------- done -------------------------


   def copy_orig_proc(self):
      """if the proc script exists, copy to .orig.SCRIPTNAME"""
      if self.rvars.file_proc == '': return
      pfile = self.rvars.file_proc

      # ------------------------- do the work -------------------------
      self.LV.retdir = SUBJ.goto_proc_dir(self.cvars.subj_dir)
      if os.path.isfile(pfile):
         cmd = 'cp -f %s .orig.%s' % (pfile, pfile)
         if self.cvars.verb > 1: print '++ exec: %s' % cmd
         os.system(cmd)
      elif self.cvars.verb > 1: print "** no proc '%s' to copy" % pfile
      self.LV.retdir = SUBJ.ret_from_proc_dir(self.LV.retdir)
      # ------------------------- done -------------------------

   def script_ap_regress(self):
      """add any -regress_* options
         - start with stim files, labels and basis function(s)
      """

      if 'regress' not in self.svars.blocks: return ''

      # stim files, labels, basis functions
      cmd  = self.script_ap_stim()
      cmd += self.script_ap_stim_labels()
      cmd += self.script_ap_stim_basis()
      cmd += self.script_ap_regress_other()
      cmd += self.script_ap_regress_opts_3dD()

      # ------------------------------------------------------------
      # at end, add post 3dD options
      if self.svars.stim:
         cmd += '%s-regress_make_ideal_sum sum_ideal.1D \\\n' % self.LV.istr
      cmd += '%s-regress_est_blur_epits \\\n' \
             '%s-regress_est_blur_errts \\\n' % (self.LV.istr, self.LV.istr)

      return cmd

   def script_ap_regress_other(self):
      """apply items with their own -regress_* options:

           motion_limit, outlier_limit, compute_fitts, reml_exec, run_clustsim
      """

      rstr = ''
      rstr += self.script_ap_apply_svar_1('motion_limit', vtype=float, 
                        defval=0.0, oname='-regress_censor_motion')
      rstr += self.script_ap_apply_svar_1('outlier_limit', vtype=float, 
                        defval=0.0, oname='-regress_censor_outliers')
      if self.svars.val('reml_exec') == 'yes':          # default is 'no'
         rstr += '%s-regress_reml_exec \\\n' % self.LV.istr
      if self.svars.val('run_clustsim') == 'no':        # default is 'yes'
         rstr += '%s-regress_run_clustsim no \\\n' % self.LV.istr
      if self.svars.val('compute_fitts') == 'yes':      # default is 'no'
         rstr += '%s-regress_compute_fitts \\\n' % self.LV.istr
      return rstr

   def script_ap_regress_opts_3dD(self):
      """apply -regress_opts_3dD, including explicit variables:

            gltsym, gltsym_label, regress_jobs, regress_GOFORIT
         
         Anything extra will be appended from regress_opts_3dD.
      """

      rstr = ''
      istr = '    ' + self.LV.istr      # 4 extra indentation spaces

      val = self.svars.val('regress_jobs')
      if val > 1: rstr += '%s-jobs %d \\\n' % (istr, val)

      val = self.svars.val('regress_GOFORIT')
      if val > 0: rstr += '%s-GOFORIT %d \\\n' % (istr, val)

      # apply any GLTs
      rstr += self.script_ap_regress_opts_gltsym()

      # if we have anything, create a formal string
      if rstr == '': return ''

      ostr = '%s-regress_opts_3dD \\\n' % self.LV.istr
      return ostr + rstr

   def script_ap_apply_svar_1(self, vname, vtype=None, defval=None, oname=None):
      """make one AP line based on varable name and type
           vtype:  if given, check type
           defval: if given, only apply if differs
           oname:  if given, actual afni_proc.py option name

         note: this function applies to simple types only, not lists

         return an additional option line
      """
      
      val = self.svars.val(vname)

      # first check for valid type
      if vtype != None:
         if type(val) != vtype:
            self.errors.append("** %s is not %s, have %s"%(vname, vtype, val))
            return ''

      # if default, skip
      if defval != None and val == defval: return ''

      # use oname (possibly vname) for AP option
      if oname == None: oname = '-' + vname

      # return option line
      return "%s%s %s \\\n" % (self.LV.istr, oname, val)

   def script_ap_regress_opts_gltsym(self):
      """apply any -gltsym and -glt_label options"""

      rstr = ''                         # init return string
      istr = '    ' + self.LV.istr      # 4 extra indentation spaces
      nglt = len(self.svars.gltsym)
      if nglt != len(self.svars.gltsym_label):
         self.errors.append("** have %d GLTs but %d GLT labels" \
                            % (nglt, len(self.svars.gltsym_label)))
      elif nglt > 0:
         gltsym = self.svars.gltsym
         if nglt > 10: rstr += '%s-num_glt %d \\\n' % (istr, nglt)
         for ind, label in enumerate(self.svars.gltsym_label):
            rv, estr = self.check_valid_gltsym(gltsym[ind])
            if rv:
               self.errors.append(estr)
               continue
            rv, estr = self.check_valid_label(label)
            if rv:
               self.errors.append(estr)
               continue

            rstr += "%s-gltsym 'SYM: %s' -glt_label %d %s \\\n" \
                    % (istr, gltsym[ind], ind+1, label)

      return rstr

   def check_valid_label(self, label):
      """check for bad characters in label:
           - whitespace for now
         return status (0=success) and error string
      """
      for c in label:
         if ' ' in label or '\t' in label or '\n' in label: 
            return 1, '** GLT labels cannot contain whitespace'
      return 0, ''

   def check_valid_gltsym(self, gltsym):
      """text should be list of entries of form:
                [+-][float*]LABEL[[index list]]  or  '\'

         don't work too hard here

         return status (0=success) and error string
      """
      # rcr - todo??
      return 0, ''

   def script_ap_stim_basis(self):
      slen = len(self.svars.stim_basis)
      if slen == 0: return ''

      if UTIL.vals_are_constant(self.svars.stim_basis):
         return "%s-regress_basis '%s' \\\n"  \
                   % (self.LV.istr, self.svars.stim_basis[0])

      if slen != len(self.svars.stim):
         self.errors.append('** error: num stim files != num stim basis\n')
         return ''
         
      return "%s-regress_basis_multi \\\n%*s%s \\\n" %         \
                (self.LV.istr, self.LV.indent+4, '',
                ' '.join(["'%s'"%b for b in self.svars.stim_basis]))

   def script_ap_stim_labels(self):
      slen = len(self.svars.stim_label)
      if slen == 0: return ''

      if slen != len(self.svars.stim):
         self.errors.append('** error: num stim files != num stim labels\n')
         return ''
         
      return "%s-regress_stim_labels \\\n%*s%s \\\n" %          \
                (self.LV.istr, self.LV.indent+4, '',
                 ' '.join(["%s"%b for b in self.svars.stim_label]))

   def script_ap_stim(self):
      """- check for existence of stimulus timing files
         - if wildcard is requested, check that the actual wildcard glob
              matches the list of stim names (else warning)
      """
      if not self.svars.stim:
         self.warnings.append('** warnings: no stim timing files given\n')
         return ''
      if len(self.svars.stim) == 0:
         self.errors.append('** error: no stim timing files given\n')
         return ''

      # if wildcard, input files must exist, and expansion must match list
      if self.svars.stim_wildcard == 'yes':
         self.LV.stim_wildform = UTIL.glob_form_from_list(self.LV.short_stim)
         if self.check_wildcard_errors('stim', self.svars.stim): return ''
         if self.LV.var_sdir:
            cstr = '%s/%s' % (self.LV.var_sdir, self.LV.stim_wildform)
         else: cstr = self.LV.stim_wildform

         return '%s-regress_stim_times %s \\\n' % (self.LV.istr, cstr)

      # no wildcarding, so check for just one stim
      if len(self.svars.stim) == 1:
         if self.LV.var_sdir:
            cstr = '%s/%s' % (self.LV.var_sdir, self.LV.short_stim[0])
         else: cstr = self.LV.short_stim[0]
         return '%s-regress_stim_times %s \\\n' % (self.LV.istr, cstr)

      # so we have multiple stim file, use just one per line
      cmd = '%s-regress_stim_times \\\n' % (self.LV.istr)
      istr = self.LV.istr + (' '*4)
      for name in self.LV.short_stim:
         if self.LV.var_sdir:
            cmd += ('%s%s/%s \\\n' % (istr, self.LV.var_sdir, name))
         else: cmd += ('%s%s \\\n' % (istr, name))

      return cmd

   def script_ap_align(self):
      """process align options

         possibly set -align_opts_aea:
             -cost COST, -giant_move
      """

      if 'align' not in self.svars.blocks: return ''

      rstr = '' # for now, will be part of '-align_opts_aea'

      # add each option after a space

      cost = self.svars.val('align_cost')
      if not self.svars.is_empty('align_cost') and cost != g_def_align_cost:
         rstr += ' -cost %s' % cost

      if self.svars.val('align_giant_move') == 'yes':
         rstr += ' -giant_move'

      if rstr != '': return '%s-align_opts_aea%s \\\n' % (self.LV.istr, rstr)
      else:          return ''

   def script_ap_tlrc(self):
      """process tlrc options

         possibly set:  -tlrc_base, -tlrc_no_ss,
                        -tlrc_opts_at: -OK_maxite
      """

      if 'tlrc' not in self.svars.blocks: return ''

      rstr = ''

      base = self.svars.val('tlrc_base')
      if not self.svars.is_empty('tlrc_base') and base != g_subj_defs.tlrc_base:
         rstr += self.script_ap_apply_svar_1('tlrc_base', vtype=str,
                        defval=g_subj_defs.tlrc_base)

      if self.svars.val('tlrc_ss') == 'no':
         rstr += '%s-tlrc_no_ss \\\n' % self.LV.istr
                 
      # now fill any -tlrc_opts_at options (put a space before each)
      topts = ''
      if self.svars.val('tlrc_ok_maxite') == 'yes':
         topts += ' -OK_maxite'

      if topts != '':
         rstr += '%s-tlrc_opts_at%s \\\n' % (self.LV.istr, topts)
                 

      return rstr

   def script_ap_volreg(self):
      """- possibly set the following options:
           -volreg_align_to, -volreg_align_e2a, -volreg_tlrc_(ad)warp
      """

      if 'volreg' not in self.svars.blocks: return ''

      # volreg base, default is third
      if self.svars.volreg_base == '': vrbase = 'third'
      elif self.svars.volreg_base in g_vreg_base_list:
         vrbase = self.svars.volreg_base
      else:
         err = '** error: unknown volreg base: %s\n' % self.svars.volreg_base
         self.errors.append(err)
         return ''

      cmd = '%s-volreg_align_to %s \\\n' % (self.LV.istr, vrbase)

      # if align block, align epi and anat
      if 'align' in self.svars.blocks:
         cmd += '%s-volreg_align_e2a \\\n' % self.LV.istr

      if self.LV.warp == 'warp':
         cmd += '%s-volreg_tlrc_warp \\\n' % self.LV.istr
      elif self.LV.warp == 'adwarp':
         cmd += '%s-volreg_tlrc_adwarp \\\n' % self.LV.istr

      return cmd

   def script_ap_blur(self):
      """- possibly set the following options: -blur_size
      """

      if 'blur' not in self.svars.blocks: return ''

      cmd = ''

      # add the -blur_size in any case, just to be explicit
      if self.svars.blur_size > 0:
         cmd += '%s-blur_size %s \\\n' % (self.LV.istr, self.svars.blur_size)

      return cmd

   def script_ap_epi(self):
      """- check for existence of EPI datasets
         - if wildcard is requested, check that the actual wildcard glob
              matches the list of EPI names (else warning)
      """
      if not self.svars.epi:
         self.errors.append('** error: no EPI datasets given\n')
         return ''

      # if wildcard, input files must exist, and expansion must match list
      if self.svars.epi_wildcard == 'yes':
         self.LV.epi_wildform=UTIL.glob_form_from_list(self.LV.short_epi)
         if self.check_wildcard_errors('EPI', self.svars.epi): return ''
         if self.LV.var_edir:
            cstr = '%s/%s' % (self.LV.var_edir, self.LV.epi_wildform)
         else: cstr = self.LV.epi_wildform

         return '%s-dsets %s \\\n' % (self.LV.istr, cstr)

      # no wildcarding, so check for just one EPI
      if len(self.svars.epi) == 1:
         if self.LV.var_edir:
            cstr = '%s/%s' % (self.LV.var_edir, self.LV.short_epi[0])
         else: cstr = self.LV.short_epi[0]
         return '%s-dsets %s \\\n' % (self.LV.istr, cstr)

      # so we have multiple EPI datasets, use just one per line
      cmd = '%s-dsets \\\n' % (self.LV.istr)
      istr = self.LV.istr + (' '*4)
      for name in self.LV.short_epi:
         if self.LV.var_edir:
            cmd += ('%s%s/%s \\\n' % (istr, self.LV.var_edir, name))
         else: cmd += ('%s%s \\\n' % (istr, name))

      return cmd

   def script_ap_tcat(self):
      try: nfirst = int(self.svars.tcat_nfirst)
      except:
         self.errors.append("** tcat_nfirst is not int, have %s" \
                            % self.svars.tcat_nfirst)
         return ''
        
      return '%s-tcat_remove_first_trs %d \\\n' % (self.LV.istr, nfirst)

   def script_ap_blocks(self):
      if not self.svars.blocks: return ''

      return '%s-blocks %s \\\n' \
             % (self.LV.istr, ' '.join(self.svars.blocks))

   def script_ap_anat(self):
      """set LV.warp and add -copy_anat command"""

      if not self.svars.anat:
         self.LV.warp = ''      # not going to tlrc space
         return ''

      self.LV.warp = 'warp'     # unless 'get' and manual
      aset = BASE.afni_name(self.svars.anat)
      if self.svars.get_tlrc == 'yes': # require existence and +orig extension
         if not aset.exist():
            self.errors.append('** get_tlrc: orig version not found\n')
            return ''
         if not aset.view == '+orig':
            self.errors.append('** get_tlrc: requires orig version dset\n')
            return ''
         # now check that tlrc view exists
         tset = aset.new(new_view='+tlrc')
         if not aset.exist():
            self.errors.append('** get_tlrc: tlrc version not found\n')
            return ''
         # check WARP_DATA attribute
         # len 30 -> @auto_tlrc, 360 -> manual
         wd = BASE.read_attribute(tset.ppv(), 'WARP_DATA')
         if not wd:
            err  = '** failed to read WARP_DATA attr from %s\n' % tset.ppv()
            err += '   (failing to get_tlrc...)\n'
            self.errors.append(err)
            return ''

         # finally, some happy cases, first note file name, then set LV.warp
         
         if self.LV.var_adir: file = '%s/%s' % (self.LV.var_adir, aset.prefix)
         else:                file = aset.prefix

         if len(wd) == 30:      self.LV.warp = 'warp'   # @auto_tlrc
         elif len(wd) == 360:   self.LV.warp = 'adwarp' # manual
         else:                  # unknown
            err  = '** bad WARP_DATA: %s\n' % wd
            err += '   (failing to get_tlrc...)\n'
            self.errors.append(err)
            return ''
      else:
         if self.LV.var_adir: file = '%s/%s' % (self.LV.var_adir, aset.pv())
         else:                file = aset.pv()

      if self.cvars.verb > 2: print '-- anat dset = %s' % file

      return '%s-copy_anat %s \\\n' % (self.LV.istr, file)

   def check_wildcard_errors(self, name, flist):
      """if any error, report the error and return
         - files must exist
         - files must be unique
         - files must be sorted
         - wildcard form must match files

         return 0 on success, 1 on errors
      """

      # existence
      missing = missing_files(flist)
      if len(missing) > 0:
         err = '** cannot use %s wildcard form with missing files:\n\n' % name
         for file in missing: err += ('      %s\n' % file)
         self.errors.append(err)
         return 1

      # unique (no accidental duplicates)
      if not UTIL.vals_are_unique(flist):
         self.errors.append('** %s filenames are not unique\n' % name)
         return 1

      # sorted
      if not UTIL.vals_are_sorted(flist):
         self.errors.append(                                    \
                '** cannot use %s wildcard form\n\n'            \
                '   filenames are not in alphabetical order,\n' \
                '   so wildcard order would differ\n' % name)
         return 1

      # now check that expansion matches files
      # (make wildcard form, then expand it and compare with flist)
      if not UTIL.glob_form_matches_list(flist):
         self.errors.append(                                            \
                '** cannot use %s wildcard form\n\n'                    \
                '   file list from shell does not match list in GUI\n'  \
                '   (e.g. maybe 3 files in GUI, but 5 match wildcard)\n' % name)
         return 1

      return 0

   def script_ap_init(self):
      # init all proc variables right away
      self.rvars.file_proc = 'proc.%s' % self.svars.sid
      self.rvars.output_proc = 'output.proc.%s' % self.svars.sid
      self.rvars.results_dir = '%s.results' % self.svars.sid

      self.rvars.file_proc = 'proc.%s' % self.svars.sid
      cmd  = '# run afni_proc.py to create a single subject processing script\n'
      cmd += 'afni_proc.py -subj_id $subj \\\n'         \
             '%s-script proc.$subj -scr_overwrite \\\n' % self.LV.istr

      return cmd

   def script_set_vars(self):
      if not self.svars.sid:
         # use SUBJ, but warn user
         self.svars.sid = 'SUBJ'
         warn = "** missing subject ID, using default %s\n" % self.svars.sid
         self.warnings.append(warn)

      cmd  = '# set subject and group identifiers\n'
      if self.svars.sid: cmd += 'set subj      = %s\n' % self.svars.sid
      if self.svars.gid: cmd += 'set group_id  = %s\n' % self.svars.gid
      if cmd != '': return cmd + '\n'
      else:         return cmd

   def script_set_dirs(self):
      if not self.LV.use_dirs: return ''

      cmd = '# set data directories\n'

      if self.LV.use_tdir:
         # then set var and apply to children
         cmd += 'set top_dir   = %s\n' % self.LV.top_dir
         tstr = '$top_dir/'
      else: tstr = ''

      if self.LV.var_adir and self.LV.var_adir != '$top_dir':
         cmd += 'set anat_dir  = %s%s\n' % (tstr, self.LV.short_anat_dir)
      if self.LV.var_edir and self.LV.var_edir != '$top_dir':
         cmd += 'set epi_dir   = %s%s\n' % (tstr, self.LV.short_epi_dir)
      if self.LV.var_sdir and self.LV.var_sdir != '$top_dir':
         cmd += 'set stim_dir  = %s%s\n' % (tstr, self.LV.short_stim_dir)

      return cmd + '\n'

   def script_init(self):
      return '#!/usr/bin/env tcsh\n\n'                          \
             '# created by uber_subject.py: version %s\n'       \
             '# creation date: %s\n\n' % (g_version, asctime())

   def use_dir(self, dir):
      if dir and dir != '.': return 1
      return 0

   def set_blocks(self):
      if len(self.svars.blocks) > 0: return     # use what is given

      if self.svars.anat:
         blocks = default_block_order(anat=1)
         if self.svars.get_tlrc == 'yes': blocks.remove('tlrc')
      else:
         blocks = default_block_order(anat=0)

      self.svars.blocks = blocks

   def set_directories(self):
      """set top_dir and anat_dir, epi_dir and stim_dir as sub-directories
         also set var_adir, edir and sdir, as the directory variables
      """

      all_files = []
      if self.svars.anat:          all_files.append(self.svars.anat)
      if len(self.svars.epi)  > 0: all_files.extend(self.svars.epi)
      if len(self.svars.stim) > 0: all_files.extend(self.svars.stim)

      self.LV.top_dir  = UTIL.common_dir(all_files)
      self.LV.anat_dir = UTIL.common_dir([self.svars.anat])
      self.LV.epi_dir  = UTIL.common_dir(self.svars.epi)
      self.LV.stim_dir = UTIL.common_dir(self.svars.stim)

      # init vars: if not '', then apply them as directories
      self.LV.var_adir = self.LV.var_edir = self.LV.var_sdir = ''
      if self.use_dir(self.LV.anat_dir): self.LV.var_adir = '$anat_dir'
      if self.use_dir(self.LV.epi_dir):  self.LV.var_edir = '$epi_dir'
      if self.use_dir(self.LV.stim_dir): self.LV.var_sdir = '$stim_dir'

      # decide whether to use top_dir
      # (it exists and is long enough or replaces all child dirs)
      self.LV.use_tdir = 0
      if self.LV.top_dir.count('/') > 1: self.LV.use_tdir = 1
      elif self.use_dir(self.LV.top_dir):
         if UTIL.vals_are_constant( [self.LV.anat_dir, self.LV.epi_dir,
                                     self.LV.stim_dir] ) :
            self.LV.use_tdir = 1

      # make short versions, but preserve long ones
      # maybe $epi_dir should be replaced with $top_dir
      if self.LV.use_tdir:
         self.LV.short_anat_dir = self.child_dir_name(self.LV.anat_dir)
         if self.LV.short_anat_dir == '.': self.LV.var_adir = '$top_dir'

         self.LV.short_epi_dir  = self.child_dir_name(self.LV.epi_dir)
         if self.LV.short_epi_dir == '.': self.LV.var_edir = '$top_dir'

         self.LV.short_stim_dir = self.child_dir_name(self.LV.stim_dir)
         if self.LV.short_stim_dir == '.': self.LV.var_sdir = '$top_dir'
      else:
         self.LV.short_anat_dir = self.LV.anat_dir
         self.LV.short_epi_dir  = self.LV.epi_dir
         self.LV.short_stim_dir = self.LV.stim_dir

      # note whether we are using any directory at all
      if self.use_dir(self.LV.top_dir) or self.LV.var_adir or \
         self.LV.var_edir or self.LV.var_sdir: self.LV.use_dirs = 1
      else: self.LV.use_dirs = 0

      if self.cvars.verb > 3:
         print ("++ APS.top_dir = %s\n   adir = %s\n   edir = %s\n   sdir = %s"\
                % (self.LV.top_dir, self.LV.anat_dir,
                   self.LV.epi_dir, self.LV.stim_dir))

   def set_short_names(self):
      """set short_anat, _epi, _stim from respective directories
         --> these are the names used in the script
             e.g. '$anat_dir/%s' % short_anat
      """
      # anat (one name)
      if self.LV.var_adir:
         dlen = len(self.LV.anat_dir)
         self.LV.short_anat = self.svars.anat[dlen+1:]
      else: # no directory
         self.LV.short_anat = self.svars.anat

      # EPI (list)
      if self.LV.var_edir:
         dlen = len(self.LV.epi_dir)
         self.LV.short_epi = [epi[dlen+1:] for epi in self.svars.epi]
      else: # no directory
         self.LV.short_epi = [epi[:] for epi in self.svars.epi]

      # stim (list)
      if self.LV.var_sdir:
         dlen = len(self.LV.stim_dir)
         self.LV.short_stim = [stim[dlen+1:] for stim in self.svars.stim]
      else: # no directory
         self.LV.short_stim = [stim[:] for stim in self.svars.stim]

   def child_dir_name(self, child, parent=''):
      if parent == '': parent = self.LV.top_dir
      if parent == '' or child == '': return child
      plen = len(parent)
      clen = len(child)
      if child[0:plen] != parent: return child

      # return everything after the separator
      if clen < plen + 2: return '.'
      else:               return child[plen+1:]

   def get_ap_command(self):
      """return status, message
             status = number of error message

             if 0: message = command
             else: message = error string

         requests for warnings must be made separately, since they would
         not be fatal
      """

      # if we have error messages, return them
      if len(self.errors) > 0:
         return 1, SUBJ.make_message_list_string(self.errors, "errors")

      return 0, self.ap_command

   def get_ap_warnings(self):
      """return the number of warnings and a warnings string"""
      return len(self.warnings), \
             SUBJ.make_message_list_string(self.warnings, "warnings")

   def subj_dir_filename(self, vname):
      """file is either fname or subj_dir/fname (if results is set)
         vname : results file variable (must convert to fname)
      """
      fname = self.rvars.val(vname)
      return self.cvars.file_under_dir('subj_dir', fname)

def get_dir_and_glob_form(flist):
   """return the common directory and glob form string of a list"""
   fdir = UTIL.common_dir(flist)
   dlen = len(fdir)
   if dlen > 0: short_list = [name[dlist+1:] for name in flist]
   else:        short_list = flist

   return fdir, UTIL.glob_form_from_list(short_list)

def get_uber_results_dir(topdir=None):
   """if we are in a DEF_UBER_DIR, return it
      else return cwd/DEF_UBER_DIR

      if topdir is passed, use that in place of the current directory
   """
   if topdir: cwd = topdir[:]
   else:      cwd = os.getcwd()
   base = os.path.basename(cwd)

   if base == DEF_UBER_DIR: return cwd
   else:                    return '%s/%s' % (cwd, DEF_UBER_DIR)

def get_def_subj_path(topdir=None, subj=None, gid=None, sid=None):
   """return something of the form subject_results/group.g1/subj.SUBJ"""

   if subj: # init from subject instance
      sid = subj.svars.val('sid')
      gid = subj.svars.val('gid')

   # if not set from any parameter, use defaults
   if sid == None: sid = DEF_SUBJ_ID
   if gid == None: gid = DEF_GROUP_ID

   sdir = '%s/group.%s/subj.%s' % (DEF_TOP_SDIR, gid, sid)

   if topdir: return '%s/%s' % (topdir, sdir)
   else:      return sdir

def make_gltsym_examples(labels):
   """given a list of labels, make a few gltsym examples

      exactly 2 labels: return a diff and a mean
      3 or more labels: return 3 pairwise diffs and a mean

      return a list of gltsym entries (sans SYM:) and labels
   """

   llen = len(labels)
   if llen < 2: return [], []

   if llen == 2:
      glist = []
      glist.append('%s -%s' % (labels[0], labels[1]))
      glist.append('0.5*%s +0.5*%s' % (labels[0], labels[1]))
      llist = []
      a = labels[0][0].upper()
      b = labels[1][0].upper()
      llist.append('%s-%s' % (a, b))
      llist.append('mean.%s%s' % (a, b))

      return glist, llist

   # so len > 2
   glist = []
   glist.append('%s -%s' % (labels[0], labels[1]))
   glist.append('%s -%s' % (labels[0], labels[2]))
   glist.append('%s -%s' % (labels[1], labels[2]))
   glist.append('0.333*%s +0.333*%s +0.333*%s'%(labels[0],labels[1],labels[2]))
   glist.append('%s -0.5*%s -0.5*%s'%(labels[0],labels[1],labels[2]))
   llist = []
   a = labels[0][0].upper()
   b = labels[1][0].upper()
   c = labels[2][0].upper()
   llist.append('%s-%s' % (labels[0], labels[1]))
   llist.append('%s-%s' % (labels[0], labels[2]))
   llist.append('%s-%s' % (labels[1], labels[2]))
   llist.append('mean.%s%s%s' % (a,b,c))
   llist.append('%s-%s%s' % (a,b,c))

   return glist, llist


# ===========================================================================
# end class AP_Subject
# ===========================================================================

def objs_are_equal(obj1, obj2):
   """- check for None
      - check for simple types
         - if so, check equality
         - else if lists
            - compare lengths and recur
         - else, just check equality

      return 1 if equal (completely), else 0
   """
   # check None
   if obj1 == None and obj2 == None: return 1
   if obj1 == None  or obj2 == None: return 0   # only 1

   # check simple types
   st1 = is_simple_type(obj1)
   st2 = is_simple_type(obj2)
   if st1 and st2:      # simple equality check
      if obj1 == obj2: return 1
      else:            return 0
   if st1 or st2:      return 0  # only 1 after previous test

   # check for type difference
   if type(obj1) != type(obj2): return 0

   # check lists
   if type(obj1) == type([]):
      len1 = len(obj1)
      len2 = len(obj2)
      if len1 != len2: return 0
      if len1 == 0:    return 1  # both empty

      # so non-empty lists of the same lengths: recur
      for ind in range(len1):
         if not objs_are_equal(obj1[ind], obj2[ind]): return 0
      return 1

   # some other type
   if obj1 == obj2: return 1
   else:            return 0

def default_block_order(anat=1):
   """rcr - reconcile with afni_proc.py, maybe make single interface"""

   if anat: return g_def_blocks_anat[:]
   else:    return g_def_blocks[:]

def is_simple_type(val):
   if type(val) in [int, float, str]: return 1
   else:                              return 0

def missing_files(flist):
   """return a sub-list of the files that are missing"""
   missing = []
   for file in flist:
      if not os.path.isfile(file): missing.append(file)
   return missing

def ap_command_from_svars(svars, cvars):
   """create an afni_proc.py command

      This might be run from the GUI or command line.

      return status, warnings and error string
        status =  0 on success
               =  1 if warnings
               = -1 if errors
   """

   # create command, save it (init directory tree?), show it
   apsubj = AP_Subject(svars)
   nwarn, wstr = apsubj.get_ap_warnings()
   status, mesg = apsubj.get_ap_command()

   if status == 0:
      if nwarn > 0: status = 1  # have warnings but no errors
   else:
      status = -1               # have errors

   return status, wstr, mesg

def update_cvars_from_special(name, cvars, check_sort=0):
   """nothing special to do here yet"""
   return 0

def set_vstr_from_def(obj, name, vlist, vars, verb=1, spec=0, csort=1):
   """try to set name = value based on vlist
        (just set as string)
      if name is not known by the defaults, return failure

      if spec: update_vars_from_special(csort)

      return 1 on change, 0 on unchanged, -1 on error
   """

   if obj == 'svars':
      defs = g_subj_defs
      sfunc = update_svars_from_special
   elif obj == 'cvars':
      defs = g_ctrl_defs
      sfunc = update_cvars_from_special
   else:
      print '** set_vstr_from_def: invalid obj name: %s' % obj
      return -1

   return SUBJ.set_var_str_from_def(obj, name, vlist, vars, defs=defs,
                                    verb=verb, csort=csort, spec=sfunc)

def update_svars_from_special(name, svars, check_sort=0):
   """in special cases, a special svar might need updates, and might suggest
      making other updates

      if check_sort, attempt to sort known file name lists by their
      implied indices

        epi:    - sort by index list (if check_sort)
        stim:   - sort by index list (if check_sort)
                - if labels are not yet set, try to init from file names

      return the number of applied changes
   """

   # quick check for field to work with
   if not name in ['epi', 'stim'] : return 0

   changes = 0

   if name == 'epi':
      fnames = svars.epi
      nf = len(fnames)
      if nf < 2: return 0       # nothing to do

      if check_sort: # try to sort by implied index list
         dir, snames, gstr = UTIL.flist_to_table_pieces(fnames)
         indlist = UTIL.list_minus_glob_form(snames)
         apply = 0
         try:
            indlist = [int(val) for val in indlist]
            apply = 1
         except: pass
         # might as well check if already sorted
         if apply and UTIL.vals_are_increasing(indlist): apply = 0
         if apply and UTIL.vals_are_unique(indlist):
            # attach index and name in 2-D array, sort, extract names
            vlist = [[indlist[ind], fnames[ind]] for ind in range(nf)]
            vlist.sort()
            svars.set_var(name, [val[1] for val in vlist])
            changes += 1

   elif name == 'stim':
      fnames = svars.stim
      nf = len(fnames)
      if nf < 2: return 0               # nothing to do

      # stim file names are more complex...
      dir, snames, gstr = UTIL.flist_to_table_pieces(fnames)
      stable = UTIL.parse_as_stim_list(snames)

      if len(stable) != nf: return 0    # nothing to do

      # if sorting, extract and process indlist
      if check_sort: # try to sort by implied index list
         indlist = [entry[0] for entry in stable]
         apply = 0
         try:
            indlist = [int(val) for val in indlist]
            apply = 1
         except: pass
         # might as well check if already sorted
         if apply and UTIL.vals_are_increasing(indlist): apply = 0
         if apply and UTIL.vals_are_unique(indlist):
            # attach index and name in 2-D array, sort, extract names
            vlist = [[indlist[ind], fnames[ind]] for ind in range(nf)]
            vlist.sort()
            svars.set_var(name, [val[1] for val in vlist])
            changes += 1

      # apply labels unless some already exist
      apply = 1
      if svars.valid('stim_label'):
         if len(svars.stim_label) > 0: apply = 0

      if apply:
         labs = [entry[1] for entry in stable]
         if not '' in labs:
            svars.set_var('stim_label', labs)
            changes += 1

   return changes


# ===========================================================================
# help strings accessed both from command-line and GUI
# ===========================================================================

helpstr_todo = """
---------------------------------------------------------------------------
                        todo list:  

- change print statements to LOG statements
   - optionally store to pass up to GUI (maybe start applying APSubj.status)
   - GUI could process (display as html?) and clear log
   - maybe register callback function to let GUI know?
- reconcile 'anat has skull' between align and tlrc options
- make 3dAutomask default method for EPI in aea?  NO
- help buttons for expected and extra regress options
- does tcsh exist?
- make UberInterface class in uber_subject.py?
- more verb output
- group box : choose blocks
   - display 2 methods, method 1 is grayed when 2 is not empty
   1. list of toggle boxes showing all standard blocks (what about copy_anat?)
   2. text edit field to simply type list of blocks

- allow stim_file regressors and labels
- add range error checking for many variables?
  (types are done when converting from str)
- be able to change font (individual windows?  whole GUI?)
   - no, that should be controllable through desktop environment
- think about how to pass subject info back up to uber_proc.py
   - maybe keep list of AP_Subject instances in UProc
      - maintain state: INIT_AP, INIT_PROC, TERMINATED, COMPLETE, INSANITY
- uber_proc.py:
   - be able to generate looping script for AP commands
     (process an entire list of INIT_PROC subjects?)
      - generate foreach subj loop, and apply $subj wherever appropriate
        (directories and input file names)

tools (maybe put in uber_proc.py, instead):
   - compute average blur
   - plot regressors of interest (run xmat_tool.py or ExamineXmat?)
   - test alignment (uber_align_test.py - give 'afni' command to cut-n-paste?)
   - help to create and play with stimulus timing design (sug. by A Barbey)
---------------------------------------------------------------------------
"""

helpstr_usubj_gui = """
===========================================================================
==  Until uber_subject.py is fully integrated within the parent program
==  uber_proc.py, it is recommended to use uber_subject.py mostly to help
==  write afni_proc.py commands, and to learn about processing and shell
==  scripting.
==
==  From the GUI, there is no control over importing options from other
==  subjects or verifying that options are consistent across subjects
==  (though importing options can be done from the command line by running
==  a previous .orig.cmd.usubj.SUBJECT script, which should get created for
==  each SUBJECT).
==
==  Integration under uber_proc.py will include those abilities, and will
==  provide control for analysis of multiple subjects and groups at once.
==
==  From uber_proc.py, one should eventually be able to:
==
==     - analyze many subjects/groups using similar processing options
==     - process all of the data at once (not having to watch one subject
==       get processed at a time)
==     - generate and run group analysis scripts for commands like:
==          3dttest++, 3dMEMA, 3dANOVA2/3, others as interest dictates
==     - incorporate tools like uber_align_test.py, reporting average blur,
==         censor counts and fractions, max F-stats, plotting regressors of
==         interest
==
==  Currently, uber_subject.py provides (what we think are) good defaults
==  for afni_proc.py (and therefore a good sample of how to process data).
===========================================================================

uber_subject.py GUI             - a graphical interface to afni_proc.py

   purposes:

      o  to run a single subject analysis or generate processing scripts
      o  to help teach users:
            - how to process data, including use of new methods or tools
            - scripting techniques
            - where to get more help

   required inputs:

      o  subject ID and group ID (codes for file naming)
      o  EPI datasets            (in AFNI or NIfTI format)
      o  stimulus timing files   (time=0.0 refers to start of steady state)

   optional inputs:

      o  anatomical dataset
      o  stim file labels and basis functions
      o  whether to use wildcards
      o  many processing options

   typical outputs:

      o  output from single subject processing (e.g. SUBJ.results)
      o  scripts to get there:
            - afni_proc.py command script (e.g. cmd.ap.SUBJ)
            - single subject processing script (e.g. proc.SUBJ)
            - text output from processing script (e.g. output.proc.SUBJ)

---------------------------------------------------------------------------
Overview:

   One generally goes through the following steps:

      0. specify overview variables (subject and group ID codes)
      1. specify input files (anat, EPI, stim timing)
      2. specify some additional options
      3. view the resulting afni_proc.py command
      4. process the subject

   The graphical interface is set up for users to specify the most pertinent
   inputs first.  In the future, one should be able to initialize the interface
   based on a previous subject.

   Step 0. subject and group ID codes are specified at the very top, in the
           'general subject info' section.

              inputs: subject ID, group ID

   Step 1. Specify input files.  The anatomy, EPI and stimulus timing files can
           be specified via file browsers.  File names can be altered after
           being set.

           Be careful of the EPI and stimulus timing file orders.  The ordering
           can come from any column the user clicks the heading for.  If the
           indices are found from the file names, that order is likely to be
           appropriate.

              inputs: anat, EPI, stimulus timing files

   Step 2. Specify additional options as desired.  

   Step 3. click to generate the afni_proc.py command

   Step 4. click to process the subject (run AP command and execute proc script)

- R Reynolds  Feb, 2011
===========================================================================
"""

