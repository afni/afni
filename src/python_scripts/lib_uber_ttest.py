#!/usr/bin/env python

# general functions for use by uber_tool*.py

import sys, os
from time import asctime
import glob

import afni_base as BASE
import afni_util as UTIL
import lib_subjects as SUBJ

DEF_UBER_DIR = 'uber_results'        # top directory for output
DEF_TOP_DIR  = 'group_results'       # top output dir under uber_results

g_history = """
  uber_ttest.py history

    0.0  22 Sep, 2011: initial revision
         - has basic 3dttest++ capabilities
"""

g_version = '0.0 (September 22, 2011)'

# ----------------------------------------------------------------------
# global definitions

g_prog_list = ['3dttest++', '3dMEMA']


# ----------------------------------------------------------------------
# global definitions of result, control and user defaults
# (as well as string versions of control and user defaults)

# ---- resulting values returned after class actions ----
g_res_defs = SUBJ.VarsObject("uber_ttest result variables")
g_res_defs.file_proc     = ''   # file name for process script
g_res_defs.output_proc   = ''   # output from running proc script

# ---- control variables: process control, not set by user in GUI

g_ctrl_defs = SUBJ.VarsObject("uber_ttest control defaults")
g_ctrl_defs.proc_dir     = '.'    # process dir: holds scripts and result dir
g_ctrl_defs.verb         = 1      # verbose level
g_ctrl_defs.copy_scripts = 'yes'  # make .orig copies of scripts?
g_ctrl_defs.results_dir  = 'ttest.results'  # output directory for results


# ---- user variables ----

g_user_defs = SUBJ.VarsObject("uber_ttest user defaults")

# required inputs
g_user_defs.program        = g_prog_list[0]
g_user_defs.script         = ''         # output script name
g_user_defs.prefix         = ''         # output dataset prefix

g_user_defs.dsets_A        = []
g_user_defs.sids_A         = []
g_user_defs.set_name_A     = ''
g_user_defs.beta_A         = ''
g_user_defs.tstat_A        = ''

g_user_defs.dsets_B        = []
g_user_defs.sids_B         = []
g_user_defs.set_name_B     = ''
g_user_defs.beta_B         = ''
g_user_defs.tstat_B        = ''

g_user_defs.mask           = ''

# 3dttest++ specific
g_user_defs.tt_options     = []
g_user_defs.MM_options     = []

g_user_defs.tt_cov_file    = ''
g_user_defs.tt_center_meth = 'DIFF'     # NONE, DIFF, SAME
g_user_defs.tt_paired      = 'no'
g_user_defs.tt_pooled      = 'no'
g_user_defs.tt_toz         = 'no'
g_user_defs.tt_zskip       = 'no'

# 3dMEMA specific

# string versions of variables - used by GUI and main
# (when creating TTest object, string versions of vars are passed)
g_cdef_strs = g_ctrl_defs.copy(as_strings=1)
g_udef_strs = g_user_defs.copy(as_strings=1)


# main class definition
class TTest(object):
   """class for running a group t-test

        - cvars : control variables
        - uvars : user variables
        - rvars : return variables

        ** input vars might be string types, convert on merge

        variables:
           LV            - local variables
           cvars         - control variables
           uvars         - user variables
           cmd_text      - generated alignment script
           errors            --> array of resulting error messages
           warnings          --> array of resulting warning messages
   """
   def __init__(self, cvars=None, uvars=None):

      # ------------------------------------------------------------
      # variables

      # LV: variables local to this interface, not passed
      self.LV = SUBJ.VarsObject("local AP_Subject vars")
      self.LV.indent = 8                # default indent for main options
      self.LV.istr   = ' '*self.LV.indent
      self.LV.retdir = ''               # return directory (for jumping around)

      # merge passed user variables with defaults
      self.cvars = g_ctrl_defs.copy()
      self.uvars = g_user_defs.copy()
      self.cvars.merge(cvars, typedef=g_ctrl_defs)
      self.uvars.merge(uvars, typedef=g_user_defs)

      # output variables
      self.rvars  = g_res_defs.copy()   # init result vars
      self.script = ''                  # resulting script
      self.errors = []                  # list of error strings
      self.warnings = []                # list of warning strings
      # ------------------------------------------------------------

      # ------------------------------------------------------------
      # preperatory settings

      if self.check_inputs(): return    # require at least anat and epi

      self.set_directories()            # data dirs: anat, epi

      if self.cvars.verb > 3: self.LV.show('ready to start script')

      # do the work
      self.create_script()

   def check_inputs(self):
      """check for required inputs: anat, epi (check existence?)"""
      if self.uvars.is_empty('program'):
         self.errors.append('** unspecified program name')

      if self.uvars.is_empty('dsets_A'):
         self.errors.append('** unspecified setA datasets')

      return len(self.errors)

   def set_directories(self):
      """decide on use of top_dir (use it or nothing - no anat_dir)

         ==> if top_dir is set, use $top_dir/short_names
             else just use anat and epi directly
      """
      all_files = [self.uvars.val('dsets_A')]
      more_files = self.uvars.val('dsets_B')
      if more_files: all_files.append(more_files)
      top_dir, parent_dirs, short_dirs, short_names =    \
         UTIL.common_parent_dirs(all_files)

      if self.cvars.verb > 2:
         print '== top_dir = %s\n   parent_dirs = %s\n   short_dirs = %s\n' \
               '   short_names = %s' \
               %(top_dir, parent_dirs, short_dirs, short_names)

      # and store it
      self.LV.top_dir     = top_dir
      self.LV.parent_dirs = parent_dirs
      self.LV.short_dirs  = short_dirs
      self.LV.short_names = short_names

      # if top_dir isn't long enough, do not bother with it
      if self.LV.top_dir.count('/') < 2:
         self.LV.top_dir = ''
         if self.cvars.verb > 2: print '   (top_dir not worth using...)'

   def create_script(self):
      """attempt to generate an alignment script
            - write align_script
            - keep a list of any warnings or errors
            - 
            - if there are errors, align_script might not be filled
      """

      # script prep, headers and variable assignments
      self.script  = self.script_init()
      self.script += self.script_set_vars()

      # do some actual work
      self.script += self.script_results_dir()
      # self.script += self.script_copy_data()
      self.script += self.script_main()

      self.script = UTIL.nuke_final_whitespace(self.script)

      return

   def script_main(self):
      """write command with prefix, datasets and extra options
      """
      
      cmd = SUBJ.comment_section_string('process the data') + '\n'

      if self.uvars.program == '3dttest++':
         cmd += self.script_ttest()
      elif self.uvars.program == '3dMEMA':
         cmd += self.script_MEMA()
      else:
         self.errors.append('** bad program name: %s' % self.uvars.program)
         return ''

      return UTIL.add_line_wrappers(cmd)

   def script_MEMA(self):
      """write command for 3dMEMA program
      """
      self.errors.append('** not ready to create 3dMEMA script')
      return ''

   def script_ttest(self):
      """write command for 3dttest++ program
      """
      if not self.uvars.prefix:
         self.errors.append('** unspecified output prefix')
         return ''

      if self.cvars.results_dir: rdir = '$results_dir'
      else:                      rdir = '.'
      prefix = '%s/%s' % (rdir, self.uvars.prefix)

      if self.uvars.dsets_B: diff = ' -AminusB'
      else:                  diff = ''
      
      cmd  = '%s -prefix %s%s \\\n' % (self.uvars.program, prefix, diff)

      if len(self.uvars.tt_options) > 0:
         cmd += ' '*10 + ' '.join(self.uvars.tt_options) + '\\\n'
      cmd += self.make_tt_setlist('A')
      cmd += self.make_tt_setlist('B')

      return cmd

   def make_tt_setlist(self, choice='A', indent=9):
      if choice == 'A':   short_name_ind = 0
      elif choice == 'B': short_name_ind = 1
      else:
         print '** MTS: bad choice %s' % choice
         return ''

      # init dsets based on the choice, and to see if there is anything to do
      dsets = self.uvars.val('dsets_%s'%choice)
      if not dsets: return ''

      sname = self.uvars.val('set_name_%s'%choice)
      if not sname: sname = 'setlist%s' % choice

      istr = ' ' * indent
      cmd = '%s -set%s %s \\\n' % (istr, choice, sname)

      beta = self.uvars.val('beta_%s'%choice)
      if not beta: beta = '0'

      dir = self.LV.val('dir%s'%choice)

      if dir: dsets = self.LV.short_names[short_name_ind]
      istr += '    '

      sids = self.uvars.val('sids_%s'%choice)

      cmd += self.make_dset_list(dsets, sids=sids, dirstr=dir, selector=beta,
                                 indent=istr)

      return cmd

   def make_dset_list(self, dsets, sids=[], dirstr='', selector='', indent=''):
      """make a list of lines of the form:
            INDENT.SID."DIRSTR/DSET[SELECTO]" \\
      """

      ndsets = len(dsets)
      nsids  = len(sids)
      if ndsets < 1: return ''

      if type(indent) == str:
         istr = indent
      elif type(indent) == int:
         istr = ' '*indent
      else:
         print '** MDL: unexpected indent %s' % type(indent)
         return ''

      dstr = dirstr
      if dstr:
         if dstr[-1] != '/': dstr = dstr + '/'
      if nsids > 0:
         if nsids != ndsets:
            print '** MDL, have %d dsets but %d sids' % (ndsets, nsids)
            return ''

      cstr = ''
      sid = ''
      for ind, dset in enumerate(dsets):
         dname = self.strip_suffix(dset, '.HEAD')

         if sids: sid = '%s ' % sids[ind]
         if selector:
            cstr += '%s%s"%s%s[%s]" \\\n' % (istr, sid, dstr, dname, selector)
         else:
            cstr += '%s%s%s%s \\\n' % (istr, sid, dstr, dname)

      return cstr

   def strip_suffix(self, fname, suffix):
      """if fname ends with suffix, return a new string without it"""
      hloc = fname.rfind(suffix)
      if hloc > 0: return fname[0:hloc]
      return fname

   def script_align_datasets(self):
      """actually run align_epi_anat.py

         only current option is -mult_cost, everything else is via variables
      """
      return '== rcr =='

      if len(self.uvars.cost_list) > 1: mstr = ' -multi_cost $cost_list'
      else:                             mstr = ''

      cmd = SUBJ.comment_section_string('align data') + '\n'

      cmd += \
       '# test alignment, using variables set above\n'          \
       'align_epi_anat.py -anat anat+orig -epi epi+orig '       \
                         '-epi_base $in_ebase \\\n'             \
       '                  -cost $cost_main%s $align_opts\n'     \
       '\n' % mstr

      return cmd

   def script_align_centers(self):
      """if align_centers should be run, deoblique both and align with
         center_base (probably TT_N27+tlrc)

         note: these commands should be fixed, since the dataset names are
      """
      return '== rcr =='

      if self.uvars.align_centers != 'yes': return ''

      cmd = SUBJ.comment_section_string('align centers') + '\n'

      cmd += '# since altering grid, remove any oblique transformation\n'    \
             '3drefit -deoblique anat+orig epi+orig\n'                       \
             '\n'                                                            \
             '# align volume centers (we do not trust spatial locations)\n'  \
             '@Align_Centers -no_cp -base TT_N27+tlrc -dset anat+orig\n'     \
             '@Align_Centers -no_cp -base TT_N27+tlrc -dset epi+orig\n'      \
             '\n'

      return cmd

   def script_copy_data(self):
      """these commands only vary based on results_dir"""

      # nothing to do here
      return ''

   def script_init(self):
      cmd = '#!/bin/tcsh -xef\n\n'                              \
            '# created by uber_ttest.py: version %s\n'     \
            '# creation date: %s\n\n' % (g_version, asctime())

      return cmd

   def script_results_dir(self):

      # if no results dir, just put everything here
      if self.cvars.is_trivial_dir('results_dir'): return ''

      cmd  = '# specify and possibly create results directory\n'\
             'set results_dir = %s\n'                           \
             'if ( ! -d $results_dir ) mkdir $results_dir\n\n'  \
             % self.cvars.results_dir

      return cmd

   def script_set_vars(self):
      """use variables for dataset directories, only
      """

      # init with a section comment
      hdr = SUBJ.comment_section_string('set process variables') + '\n'
      cmd = ''

      # init to nothing
      self.LV.dirA = ''
      self.LV.dirB = ''

      if self.uvars.mask:
         cmd += 'set mask_dset = %s\n\n' \
                % self.strip_suffix(self.uvars.mask, '.HEAD')

      # set dirA and possibly dirB
      dirs_set = 0
      if not UTIL.is_trivial_dir(self.LV.parent_dirs[0]):
         cmd += 'set dirA = %s\n' % self.LV.parent_dirs[0]
         self.LV.dirA = '$dirA'
         dirs_set = 1

      if len(self.uvars.dsets_B) > 0:
         if not UTIL.is_trivial_dir(self.LV.parent_dirs[1]):
            if self.LV.parent_dirs[0] != self.LV.parent_dirs[1]:
               cmd += 'set dirB = %s\n' % self.LV.parent_dirs[1]
               self.LV.dirB = '$dirB'
               dirs_set = 1
            else: self.LV.dirB = '$dirA'
      if dirs_set: cmd += '\n'

      if cmd: return hdr + cmd
      else:   return ''

   def make_align_opts_str(self):
      """any align options, one per line"""

      # keep comment separate to get indent length
      cmnt = '# all other align_epi_anat.py options\n' \

      cstr = 'set align_opts = ( '
      clen = len(cstr)
      istr = ' '*clen

      # put one option on first line
      cstr += '%s \\\n' % '-tshift off'

      # ---------- here is the main option application ---------

      # then add each option offset by initial indentation
      cstr += '%s%s \\\n' % (istr, '-volreg off')

      if self.uvars.giant_move == 'yes':
         cstr += '%s%s \\\n' % (istr, '-giant_move')

      if self.uvars.add_edge == 'yes':
         if len(self.uvars.cost_list) > 1:
            self.errors.append(
               '** -AddEdge does not currently work with -multi_cost')
         else:
            cstr += '%s%s \\\n' % (istr, '-AddEdge')

      if len(self.uvars.aea_opts) > 0:
            cstr += '%s%s \\\n' % (istr, ' '.join(self.uvars.aea_opts))

      # does the anatomy have a skull?
      if self.uvars.anat_has_skull != 'yes':
         cstr += '%s%s \\\n' % (istr, '-anat_has_skull no')

      # -epi_strip method
      if self.uvars.epi_strip_meth != '3dSkullStrip':
         cstr += '%s%s \\\n' % (istr,'-epi_strip %s'%self.uvars.epi_strip_meth)

      # want -save_all, -prep_off?

      # last indent is left by 2 to align ()
      cstr += '%*s)\n\n' % (clen-2, '')

      # finally, align the line wrappers
      cstr = UTIL.add_line_wrappers(cstr)

      return cmnt + cstr

   def get_script(self):
      """return status, message

                status = number of error messages
                if 0: message = command
                else: message = error string

         Requests for warnings must be made separately, since they
         are not fatal.
      """

      if len(self.errors) > 0:
         return 1, SUBJ.make_message_list_string(self.errors, "errors")

      return 0, self.script

   def get_warnings(self):
      """return the number of warnings and a warnings string"""

      return len(self.warnings), \
             SUBJ.make_message_list_string(self.warnings, "warnings")

   def proc_dir_filename(self, vname):
      """file is either fname or proc_dir/fname (if results is set)
         vname : results file variable (must convert to fname)
      """
      fname = self.rvars.val(vname)
      return self.cvars.file_under_dir('proc_dir', fname)

   def nuke_old_results(self):
      """if the results directory exists, remove it"""

      if self.cvars.results_dir == '': return

      # ------------------------- do the work -------------------------
      self.LV.retdir = SUBJ.goto_proc_dir(self.cvars.proc_dir)

      if os.path.isdir(self.cvars.results_dir):
         print '-- nuking old results: %s' % self.cvars.results_dir
         os.system('rm -fr %s' % self.cvars.results_dir)

      self.LV.retdir = SUBJ.ret_from_proc_dir(self.LV.retdir)
      # ------------------------- done -------------------------


   def copy_orig_proc(self):
      """if the proc script exists, copy to .orig.SCRIPTNAME"""
      if self.rvars.file_proc == '': return
      pfile = self.rvars.file_proc

      # ------------------------- do the work -------------------------
      self.LV.retdir = SUBJ.goto_proc_dir(self.cvars.proc_dir)
      if os.path.isfile(pfile):
         cmd = 'cp -f %s .orig.%s' % (pfile, pfile)
         if self.cvars.verb > 1: print '++ exec: %s' % cmd
         os.system(cmd)
      elif self.cvars.verb > 1: print "** no proc '%s' to copy" % pfile
      self.LV.retdir = SUBJ.ret_from_proc_dir(self.LV.retdir)
      # ------------------------- done -------------------------

   def write_script(self, fname=''):
      """write processing script to a file (in the proc_dir)
         - if fname is set, use it, else generate
         - set rvars.file_proc and output_proc
      """

      if not self.script:
         print '** no alignment script to write out'
         return 1
      if fname: name = fname
      else:
         # if self.svars.sid: name = 'script.align.%s' % self.svars.sid
         name = 'script.ttest'

      # store (intended) names for calling tool to execute with
      self.rvars.file_proc = name # store which file we have written to
      self.rvars.output_proc = 'output.%s' % name # file for command output

      if self.cvars.verb > 0: print '++ writing script to %s' % name

      # if requested, make an original copy
      self.LV.retdir = SUBJ.goto_proc_dir(self.cvars.proc_dir)

      if self.cvars.copy_scripts == 'yes': # make an orig copy
         UTIL.write_text_to_file('.orig.%s'%name, self.script, exe=1)
      rv = UTIL.write_text_to_file(name, self.script, exe=1)

      self.LV.retdir = SUBJ.ret_from_proc_dir(self.LV.retdir)
         
      return rv


# ===========================================================================
# help strings accessed both from command-line and GUI
# ===========================================================================

helpstr_todo = """
---------------------------------------------------------------------------
                        todo list:  

- test center distance in GUI to suggest align centers
---------------------------------------------------------------------------
"""

helpstr_gui = """
===========================================================================
uber_ttest.py (GUI)      - a graphical interface for group t-tests

   Generate and run 3dttest++/3dMEMA tcsh scripts for group ttests.

   purposes:
   required inputs:
   optional inputs:
   typical outputs:

---------------------------------------------------------------------------
Overview:

- R Reynolds  September, 2011
===========================================================================
"""
