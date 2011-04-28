#!/usr/bin/env python

# general functions for use by uber_tool*.py

import sys, os
from time import asctime
import glob

import afni_base as BASE
import afni_util as UTIL
import lib_subjects as SUBJ

DEF_UBER_DIR = 'uber_results'        # top directory for output
DEF_TOP_DIR  = 'tool_results'     # top subject dir under uber_results

g_history = """
  uber_align_test.py history

    0.0  April 05, 2011: initial revision
"""

g_version = '0.0 (April 5, 2011)'

# ----------------------------------------------------------------------
# global definition of default processing blocks
g_def_multi_list  = ['lpc', 'lpc+ZZ', 'lpc+', 'lpa', 'nmi', 'ls']
g_tlrc_base_list  = ['TT_N27+tlrc', 'TT_avg152T1+tlrc', 'TT_icbm452+tlrc',
                     'MNI_avg152T1+tlrc']
g_def_tlrc_base   = 'TT_N27+tlrc'


# ----------------------------------------------------------------------
# global definitions of control and result defaults

# ---- resulting values returned after class actions ----
g_res_defs = SUBJ.VarsObject("uber_align result variables")
g_res_defs.file_proc     = ''           # file name for process script
g_res_defs.output_proc   = ''           # output from running proc script

# ---- control values passed in for class actions ----
g_ctrl_defs = SUBJ.VarsObject("uber_align control defaults")
g_ctrl_defs.verb           = 1          # verbose level
g_ctrl_defs.proc_dir       = '.'        # destination for scripts and results
g_ctrl_defs.copy_scripts   = 'yes'      # do we make .orig copies of scripts?

# ---- processing vars ------------------------------

# required inputs
g_ctrl_defs.anat           = ''         # anatomical volume to align
g_ctrl_defs.epi            = ''         # EPI dataset
g_ctrl_defs.epi_base       = 0          # EPI alignment base index

# options
g_ctrl_defs.results_dir    = 'align.results' # where processing is done
g_ctrl_defs.cost           = 'lpc'
g_ctrl_defs.use_multi      = 'yes'
g_ctrl_defs.multi_list     = ['lpc+ZZ', 'lpc+', 'lpa', 'nmi', 'ls']
g_ctrl_defs.giant_move     = 'no'
g_ctrl_defs.align_centers  = 'no'
g_ctrl_defs.center_base    = 'TT_N27+tlrc'  # must find it
g_ctrl_defs.aea_opts       = []         # other align_epi_anat.py options

# later...
g_ctrl_defs.add_edge       = 'no'
g_ctrl_defs.anat_has_skull = 'yes'
g_ctrl_defs.epi_strip_meth = '3dSkullStrip'


# string versions of subject variables, to be used by GUI
g_cdef_strs = g_ctrl_defs.copy(as_strings=1)


class AlignTest(object):
   """class for testing anat to EPI alignment

        - cvars : control variables
        - rvars : return variables

        ** input vars might be string types, convert on merge

        variables:
           LV            - local variables
           cvars         - control variables
           cmd_text      - generated alignment script
           errors            --> array of resulting error messages
           warnings          --> array of resulting warning messages
   """
   def __init__(self, cvars=None):

      # ------------------------------------------------------------
      # variables

      # LV: variables local to this interface, not passed
      self.LV = SUBJ.VarsObject("local AP_Subject vars")
      self.LV.indent = 8                # default indent for main options
      self.LV.istr   = ' '*self.LV.indent
      self.LV.retdir = ''               # return directory (for jumping around)

      # merge passed control variables with defaults
      self.cvars = g_ctrl_defs.copy()
      self.cvars.merge(cvars, typedef=g_ctrl_defs)

      # output variables
      self.rvars = g_res_defs.copy()    # init result vars
      self.align_script = ''            # resulting script
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
      if self.cvars.is_empty('anat'):
         self.errors.append('** unspecified anatomical dataset')

      if self.cvars.is_empty('epi'):
         self.errors.append('** unspecified EPI dataset')

      if len(self.cvars.multi_list) < 1 and self.cvars.use_multi == 'yes':
         self.errors.append('** want -multi_cost, but have no cost list')

      if len(self.cvars.multi_list) > 1 and \
            self.cvars.cost in self.cvars.multi_list:
         self.errors.append("** cost '%s' cannot also be in -multi_cost list" \
                            % self.cvars.cost)

      return len(self.errors)

   def set_directories(self):
      """decide on use of top_dir (use it or nothing - no anat_dir)

         ==> if top_dir is set, use $top_dir/short_names
             else just use anat and epi directly
      """
      top_dir, parent_dirs, short_dirs, short_names =    \
                UTIL.common_parent_dirs([[self.cvars.anat, self.cvars.epi]])

      self.LV.top_dir     = parent_dirs[0]  # common parent dir
      self.LV.short_names = short_names # if top_dir is used, they are under it

      if self.cvars.verb > 2:
         print '-- set_dirs: top_dir    = %s\n' \
               '             short_anat = %s\n' \
               '             short_epi  = %s\n' \
               % (parent_dir, short_names[0][0], short_names[0][1])

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
      self.align_script  = self.script_init()
      self.align_script += self.script_set_vars()

      # do some actual work
      self.align_script += self.script_results_dir()
      self.align_script += self.script_copy_data()
      if self.cvars.align_centers == 'yes':
         self.align_script += self.script_align_centers()
      self.align_script += self.script_align_datasets()

      # add commands ...

      if len(self.errors) > 0: return   # if any errors so far, give up

      return

   def script_align_datasets(self):
      """actually run align_epi_anat.py

         only current option is use_multi, everything else is via variables
      """

      if self.cvars.use_multi: mstr = ' -multi_cost $cost_list'
      else:                    mstr = ''

      cmd = SUBJ.comment_section_string('align data') + '\n'

  # rcr - here
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

      if self.cvars.align_centers != 'yes': return ''

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

      cmd = SUBJ.comment_section_string('copy data') + '\n'

      if self.cvars.is_trivial_dir('results_dir'): rdir = '.'
      else:                                        rdir = '$results_dir'

      cmd += '# copy dataset to processing directory\n'         \
             '3dbucket -prefix %s/anat $in_anat\n'              \
             '3dbucket -prefix %s/epi $in_epi"[$in_ebase]"\n'   \
             '\n' % (rdir, rdir)

      if rdir != '.':
         cmd += '# enter the processing directory\n'    \
                'cd %s\n\n' % rdir

      return cmd

   def script_init(self):
      cmd = '#!/bin/tcsh -xef\n\n'                              \
            '# created by uber_align_test.py: version %s\n'     \
            '# creation date: %s\n\n' % (g_version, asctime())

      return cmd

   def script_results_dir(self):

      # if no results dir, just put everything here
      if self.cvars.is_trivial_dir('results_dir'): return ''

      cmd  = SUBJ.comment_section_string('test and create results dir') + '\n'

      cmd += '# note directory for results\n'           \
             'set results_dir = %s\n\n' % self.cvars.results_dir

      cmd += '# make sure it does not yet exist\n'      \
             'if ( -e $results_dir ) then\n'            \
             '    echo "** results dir \'$results_dir\' already exists"\n'  \
             '    exit\n'                               \
             'endif\n\n'

      cmd += '# create results directory, where the work will be done\n' \
             'mkdir $results_dir\n\n'

      return cmd

   def script_set_vars(self):
      """use variables for inputs (anat, epi, epi_base) and for
         options (cost_main, cost_list, align_opts)
      """

      # init with a section comment
      cmd = SUBJ.comment_section_string('set processing variables') + '\n'

      # maybe init with top_dir
      if not self.LV.is_trivial_dir('top_dir'):
         cmd += '# top data directory\n' \
                'set top_dir = %s\n\n' % self.LV.top_dir

      # anat and epi might use top_dir
      if self.LV.is_trivial_dir('top_dir'):
         astr = self.cvars.anat
         estr = self.cvars.epi
      else:
         astr = '$top_dir/%s' % self.LV.short_names[0][0]
         estr = '$top_dir/%s' % self.LV.short_names[0][1]

      cmd += '# input dataset options (ebase is EPI index)\n'           \
             'set in_anat  = %s\n'                                      \
             'set in_epi   = %s\n'                                      \
             'set in_ebase = %d\n\n'                                    \
             % (astr, estr, self.cvars.epi_base)

      # note whether to use multi_cost
      cmd += '# main options\n' \
             'set cost_main = %s\n' % self.cvars.cost
      if self.cvars.use_multi == 'yes':
         cmd += 'set cost_list = ( %s )\n' % ' '.join(self.cvars.multi_list)
      cmd += '\n'

      # possibly add align_opts list variable
      cmd += self.make_align_opts_str()

      return cmd

   def make_align_opts_str(self):
      """if 2 or fewer align options, put on one line, else use one per line"""
      if self.cvars.use_multi != 'yes':  return ''

      # keep comment separate to get indent length
      cmnt = '# all other align_epi_anat.py options\n' \

      cstr = 'set align_opts = ( '
      clen = len(cstr)
      istr = ' '*clen

      # put one option on first line
      cstr += '%s \\\n' % '-tshift off'

      # ---------- here it the main option application ---------

      # then add each option offset by initial indentation
      cstr += '%s%s \\\n' % (istr, '-volreg off')

      if self.cvars.giant_move == 'yes':
         cstr += '%s%s \\\n' % (istr, '-giant_move')

      if self.cvars.add_edge == 'yes':
         if self.cvars.use_multi == 'yes':
            self.errors.append(
               '** -AddEdge does not currently work with -multi_cost')
         else:
            cstr += '%s%s \\\n' % (istr, '-AddEdge')

      if len(self.cvars.aea_opts) > 0:
            cstr += '%s%s \\\n' % (istr, ' '.join(self.cvars.aea_opts))

      # does the anatomy have a skull?
      if self.cvars.anat_has_skull != 'yes':
         cstr += '%s%s \\\n' % (istr, '-anat_has_skull no')

      # -epi_strip method
      if self.cvars.epi_strip_meth != '3dSkullStrip':
         cstr += '%s%s \\\n' % (istr,'-epi_strip %s'%self.cvars.epi_strip_meth)

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

      return 0, self.align_script

   def get_warnings(self):
      """return the number of warnings and a warnings string"""

      return len(self.warnings), \
             SUBJ.make_message_list_string(self.warnings, "warnings")

   def write_script(self, fname=''):
      """write processing script to a file (in the proc_dir)
         - if fname is set, use it, else generate
         - set rvars.file_proc and output_proc
      """

      if not self.align_script:
         print '** no alignment script to write out'
         return 1
      if fname: name = fname
      else:
         # if self.svars.sid: name = 'script.align.%s' % self.svars.sid
         name = 'script.align'

      # store (intended) names for calling tool to execute with
      self.rvars.file_proc = name # store which file we have written to
      self.rvars.output_proc = 'output.%s' % name # file for command output

      if self.cvars.verb > 0: print '++ writing script to %s' % name

      # if requested, make an original copy
      self.LV.retdir = SUBJ.goto_proc_dir(self.cvars.proc_dir)

      if self.cvars.copy_scripts == 'yes': # make an orig copy
         UTIL.write_text_to_file('.orig.%s'%name, self.align_script, exe=1)
      rv = UTIL.write_text_to_file(name, self.align_script, exe=1)

      self.LV.retdir = SUBJ.ret_from_proc_dir(self.LV.retdir)
         
      return rv


# ===========================================================================
# help strings accessed both from command-line and GUI
# ===========================================================================

helpstr_todo = """
---------------------------------------------------------------------------
                        todo list:  

- create GUI
- test center distance in GUI to suggest align centers
- show corresponding afni_proc.py options
- show corresponding uber_subjec.py options?
---------------------------------------------------------------------------
"""

helpstr_gui = """
===========================================================================
uber_subject.py GUI             - a graphical interface for testing alignment

   Find good alignment options, possibly to add to afni_proc.py command.

   purposes:
   required inputs:
   optional inputs:
   typical outputs:

---------------------------------------------------------------------------
Overview:

- R Reynolds  Feb, 2011
===========================================================================
"""

