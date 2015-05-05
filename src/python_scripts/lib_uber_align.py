#!/usr/bin/env python

# general functions for use by uber_tool*.py

import sys, os
from time import asctime
import glob

import afni_base as BASE
import afni_util as UTIL
import lib_subjects as SUBJ
import lib_vars_object as VO

DEF_UBER_DIR = 'uber_results'        # top directory for output
DEF_TOP_DIR  = 'tool_results'     # top subject dir under uber_results

g_history = """
  uber_align_test.py history

    0.0  05 Apr, 2011: initial revision
    0.1  11 May, 2011:
         - replaced cost/multi_cost/multi_list with just cost_list
         - added -help_howto_program (maybe move to shell program)
         - added basic GUI, only anat field so far...
    0.2  12 May, 2011:
         - added control vars object
         - and renamed old cvars (control vars) to uvars (user vars)
           (proc_dir is currently only cvar)
         - added EPI line to GUI (so can actually generate basic script)
         ==> this version will be copied off as uber_skel.py,
             lib_uber_skel.py and gui_uber_skel.py
    0.3  13 May, 2011: wrote functioning GUI (help still needs to be written)
    0.4  16 May, 2011:
         - added 'check center dist' button, to display the current distance
         - added menu item to show afni command for viewing results
         - added menu items to show python and shell command windows
         - added much more help, including main and section buttons
         - added browsing of align_epi_anat.py help
    0.5  19 May, 2011: revert to /usr/bin/env python
    0.6  06 Jul, 2011: apply set_var_with_defs (mostly to test it out)
    0.7  22 Sep, 2011: moved get_def_tool_path to library
"""

g_version = '0.7 (September 22, 2011)'

# ----------------------------------------------------------------------
# global definition of default processing blocks
g_tlrc_base_list  = ['TT_N27+tlrc', 'TT_avg152T1+tlrc', 'TT_icbm452+tlrc',
                     'MNI_avg152T1+tlrc']
g_center_base_list= ['TT_N27+tlrc', 'MNI_avg152T1+tlrc']
g_def_tlrc_base   = 'TT_N27+tlrc'
g_def_main_costs  = ['lpc', 'lpc+ZZ', 'lpc+', 'lpa', 'nmi', 'ls']
g_epi_strip_list  = ['3dSkullStrip', '3dAutomask', 'None']


# ----------------------------------------------------------------------
# global definitions of result, control and user defaults
# (as well as string versions of control and user defaults)

# ---- resulting values returned after class actions ----
g_res_defs = VO.VarsObject("uber_align result variables")
g_res_defs.file_proc     = ''   # file name for process script
g_res_defs.output_proc   = ''   # output from running proc script

# ---- control variables: process control, not set by user in GUI

g_ctrl_defs = VO.VarsObject("uber_align control defaults")
g_ctrl_defs.proc_dir     = '.'  # process dir: holds scripts and result dir


# ---- user variables: process control, alignment inputs and options ----

g_user_defs = VO.VarsObject("uber_align user defaults")
g_user_defs.verb           = 1          # verbose level
g_user_defs.copy_scripts   = 'yes'      # do we make .orig copies of scripts?

# required inputs
g_user_defs.anat           = ''         # anatomical volume to align
g_user_defs.epi            = ''         # EPI dataset
g_user_defs.epi_base       = 0          # EPI alignment base index

# options
g_user_defs.results_dir    = 'align.results' # where script puts results
g_user_defs.cost_list      = ['lpc', 'lpc+ZZ', 'lpa', 'nmi']
g_user_defs.giant_move     = 'no'
g_user_defs.align_centers  = 'no'
g_user_defs.center_base    = 'TT_N27+tlrc'
g_user_defs.aea_opts       = []         # other align_epi_anat.py options

g_user_defs.add_edge       = 'no'
g_user_defs.anat_has_skull = 'yes'
g_user_defs.epi_strip_meth = '3dSkullStrip'


# string versions of variables - used by GUI and main
# (when creating AlignTest object, string versions of vars are passed)
g_cdef_strs = g_ctrl_defs.copy(as_strings=1)
g_udef_strs = g_user_defs.copy(as_strings=1)


# main class definition
class AlignTest(object):
   """class for testing anat to EPI alignment

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
      self.LV = VO.VarsObject("local AP_Subject vars")
      self.LV.indent = 8                # default indent for main options
      self.LV.istr   = ' '*self.LV.indent
      self.LV.retdir = ''               # return directory (for jumping around)

      # merge passed user variables with defaults
      self.cvars = g_ctrl_defs.copy()
      self.uvars = g_user_defs.copy()
      self.cvars.merge(cvars, typedef=g_ctrl_defs)
      self.uvars.merge(uvars, typedef=g_user_defs)

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

      if self.uvars.verb > 3: self.LV.show('ready to start script')

      # do the work
      self.create_script()

   def check_inputs(self):
      """check for required inputs: anat, epi (check existence?)"""
      if self.uvars.is_empty('anat'):
         self.errors.append('** unspecified anatomical dataset')

      if self.uvars.is_empty('epi'):
         self.errors.append('** unspecified EPI dataset')

      if len(self.uvars.cost_list) < 1:
         self.errors.append('** unspecified cost function(s)')

      if not UTIL.vals_are_unique(self.uvars.cost_list):
         self.errors.append('** cost functions are not unique')

      if self.uvars.align_centers == 'yes' and self.uvars.giant_move == 'no':
         self.warnings.append(                                          \
              "** 'align centers' without 'giant move' is dangerous,\n" \
              "   consider adding 'giant move'")

      return len(self.errors)

   def set_directories(self):
      """decide on use of top_dir (use it or nothing - no anat_dir)

         ==> if top_dir is set, use $top_dir/short_names
             else just use anat and epi directly
      """
      top_dir, parent_dirs, short_dirs, short_names =    \
                UTIL.common_parent_dirs([[self.uvars.anat, self.uvars.epi]])

      self.LV.top_dir     = parent_dirs[0]  # common parent dir
      self.LV.short_names = short_names # if top_dir is used, they are under it

      if self.uvars.verb > 2:
         print '-- set_dirs: top_dir    = %s\n' \
               '             short_anat = %s\n' \
               '             short_epi  = %s\n' \
               % (self.LV.top_dir, short_names[0][0], short_names[0][1])

      # if top_dir isn't long enough, do not bother with it
      if self.LV.top_dir.count('/') < 2:
         self.LV.top_dir = ''
         if self.uvars.verb > 2: print '   (top_dir not worth using...)'

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
      if self.uvars.align_centers == 'yes':
         self.align_script += self.script_align_centers()
      self.align_script += self.script_align_datasets()

      # add commands ...

      if len(self.errors) > 0: return   # if any errors so far, give up

      return

   def script_align_datasets(self):
      """actually run align_epi_anat.py

         only current option is -mult_cost, everything else is via variables
      """

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

      if self.uvars.align_centers != 'yes': return ''

      cmd = SUBJ.comment_section_string('align centers') + '\n'

      cmd += '# since altering grid, remove any oblique transformation\n'    \
             '3drefit -deoblique anat+orig epi+orig\n'                       \
             '\n'                                                            \
             '# align volume centers (we do not trust spatial locations)\n'  \
             '@Align_Centers -no_cp -base $center_base -dset anat+orig\n'    \
             '@Align_Centers -no_cp -base $center_base -dset epi+orig\n\n'   \

      return cmd

   def script_copy_data(self):
      """these commands only vary based on results_dir"""

      cmd = SUBJ.comment_section_string('copy data') + '\n'

      if self.uvars.is_trivial_dir('results_dir'): rdir = '.'
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
      if self.uvars.is_trivial_dir('results_dir'): return ''

      cmd  = SUBJ.comment_section_string('test and create results dir') + '\n'

      cmd += '# note directory for results\n'           \
             'set results_dir = %s\n\n' % self.uvars.results_dir

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
         astr = self.uvars.anat
         estr = self.uvars.epi
      else:
         astr = '$top_dir/%s' % self.LV.short_names[0][0]
         estr = '$top_dir/%s' % self.LV.short_names[0][1]

      # if we are aligning centers, set center_base variable (maybe w/top_dir)
      if self.uvars.align_centers == 'yes':
         if self.LV.is_trivial_dir('top_dir'): cstr = self.uvars.center_base
         else:
            cdir = UTIL.child_dir_name(self.LV.top_dir, self.uvars.center_base)
            if cdir == self.uvars.center_base: cstr = cdir
            else: cstr = '$top_dir/%s' % cdir
         ccmd = 'set center_base = %s\n' % cstr
      else: ccmd = ''

      # now set variables for inputs, possibly including center_base
      cmd += '# input dataset options (ebase is EPI index)\n'           \
             'set in_anat     = %s\n'                                   \
             'set in_epi      = %s\n'                                   \
             'set in_ebase    = %d\n'                                   \
             '%s'                                                       \
             '\n'                                                       \
             % (astr, estr, self.uvars.epi_base, ccmd)

      # note whether to use multi_cost
      cmd += '# main options\n' \
             'set cost_main = %s\n' % self.uvars.cost_list[0]
      if len(self.uvars.cost_list) > 1:
         cmd += 'set cost_list = ( %s )\n' % ' '.join(self.uvars.cost_list[1:])
      cmd += '\n'

      # possibly add align_opts list variable
      cmd += self.make_align_opts_str()

      return cmd

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

      return 0, self.align_script

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

      if self.uvars.results_dir == '': return

      # ------------------------- do the work -------------------------
      self.LV.retdir = SUBJ.goto_proc_dir(self.cvars.proc_dir)

      if os.path.isdir(self.uvars.results_dir):
         print '-- nuking old results: %s' % self.uvars.results_dir
         os.system('rm -fr %s' % self.uvars.results_dir)

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
         if self.uvars.verb > 1: print '++ exec: %s' % cmd
         os.system(cmd)
      elif self.uvars.verb > 1: print "** no proc '%s' to copy" % pfile
      self.LV.retdir = SUBJ.ret_from_proc_dir(self.LV.retdir)
      # ------------------------- done -------------------------

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

      if self.uvars.verb > 0: print '++ writing script to %s' % name

      # if requested, make an original copy
      self.LV.retdir = SUBJ.goto_proc_dir(self.cvars.proc_dir)

      if self.uvars.copy_scripts == 'yes': # make an orig copy
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

- show corresponding afni_proc.py options
- show corresponding uber_subjec.py options?
- warn on any unknown cost functions
- partial coverage?
---------------------------------------------------------------------------
"""

helpstr_gui = """
===========================================================================
uber_align_test.py (GUI)      - for testing anat/EPI alignment costs/options

   purpose:

      o  to easily test various cost functions and options for anat/EPI
         alignment via align_epi_anat.py

      --> resulting options would presumably be then given to afni_proc.py
          (or its GUI uber_subject.py) for use in single subject analysis

         This program creates a script which eventually calls align_epi_anat.py
         to do the alignment test.  The user should then view the results with
         afni to determine which options are most appropriate.

   required inputs:

      o  anatomical dataset
      o  EPI dataset (possibly contains pre-steady state TRs)

   optional inputs:

      o  EPI sub-brick index (default = 0)
      o  list of cost functions
      o  flag: whether to first align dataset centers
      o  if aligning centers, a 'center base' must be chosen
      o  flag: whether to include the -giant_move option
      o  flag: whether to include the -AddEdge option to align_epi_anat.py
         (if so, only one cost function may be chosen)
      o  flag: whether the anat data still has the subject skull
      o  EPI skull-stripping method (3dAutomask, 3dSkullStrip or None)
      o  any other options to provide to align_epi_anat.py

   typical outputs:

      o  anat_al+orig      : anat alignment from first cost function
      o  anat_al_COST+orig : anat alignment from each other COST function

      Hopefully at least one of these anatomical datasets aligns with the EPI.

   sample path to a main result (aligned anatomy with first cost function):

      tool_results/tool.001.align_test/align.results/anat_al+orig

   output directory structure and files (example):

      tool_results/tool.001.align_test/    - contains all output
         script.align                      - alignment script
         output.script.align               - output from script
         align.results/                    - script processing directory
            anat_al+orig                   - aligned anat dataset
            epi+orig                       - EPI volume used for alignment

---------------------------------------------------------------------------
Overview:

   The goal of using this program is to find options to align_epi_anat.py
   that help to succeed in actually aligning the anat and EPI data for one
   subject.  The use of those options is secondary, but might be one of:

        o  alignment is the end result
        o  want to pass options on to afni_proc.py, to use in single subject
           analysis stream
        o  or to uber_subject.py, the GUI for afni_proc.py
        o  want to find options for using align_epi_anat.py in the future

   When doing single subject analysis (e.g. with afni_proc.py), it is suggested
   to align the anatomical and EPI datasets.  But finding a good alignment is
   actually difficult, and might require the user to provide extra options to
   the alignment program, align_epi_anat.py.  This program is meant to test
   such options.

   Once alignment succeeds, it is suggested to put those options into the
   otherwise standard single subject analysis stream.

   ** This interface produces an alignment script.  Reviewing (or even copying,
      modifying and applying) the resulting script might be very useful.

   Typical steps when using the GUI:

   Step 1. Specify anatomical and EPI datasets for alignment.

           This is enough input to run a test with.  Everything else is an
           option.

   Step 2. Choose which EPI sub-brick (volume index) to use for alignment.

           Note that volume #0 might be a pre-steady state index.  Likely
           candidates for the EPI base index are sub-bricks:

            0 : the first TR, whether pre-steady state or not
            4 : (for example) the first steady-state TR
           99 : (for example) the last TR

           Keep in mind that when the alignment base is passed to afni_proc.py,
           that base index will not include any pre-SS TRs that are removed
           via -tcat_remove_first_trs.

           See "afni_proc.py" -help for more details.

   Step 3. Choose the cost functions to try out.

           Choose as many from the check list as desired, or add additional
           costs that are not listed.

           See "3dAllineate -HELP" for all available cost functions.  They
           are not all presented using just -help.

   Step 3. Choose whether to align dataset centers.

           Some datasets do not have a properly defined location in space,
           and so they are very far apart when overlayed on each other (in
           afni).  By aligning centers, the middle of each dataset will be
           set to that of the 'center base' dataset, so they at least start
           off in somewhat the same location.

        ** When aligning centers, 'giant_move' is highly recommended.

           With this option, @Align_Centers will be run.

           See "@Align_Centers -help" for details.

   Step 4. add any other options

        o  giant_move

           The default search space is up to 6 degrees and 10 mm (in each
           angular or distance parameter).  Using the -giant move option in
           align_epi_anat.py increases the search space to 45 degrees and
           45 mm, and also adds a center of mass adjustment.

           This is to add the -giant_move option to align_epi_anat.py.

           See "align_epi_anat.py -help" for more details.

        o  add edge

           This option can only be used with a single cost function at a time.

           If this option is set, -AddEdge will be added to align_epi_anat.py,
           which creates addition output to evaluate the end alignment.  An
           'AddEdge' sub-directory will be created, with edge-enhanced copies
           of the aligned anat and EPI.

        o  anat has skull

           If the anatomy has already been skull-stripped, this option should
           be cleared.  Otherwise, align_epi_anat.py will run 3dSkullStrip to
           remove the non-brain matter in the anatomical dataset.

        o  EPI strip method

           By default, the EPI dataset is also skull-stripped using
           3dSkullStrip.  This can be changed to either 3dAutomask (which
           should be a little faster, but is not the default) or None (if
           the EPI has already been stripped).

        o  other AEA opts

           Use this box to add any other options to pass to align_epi_anat.py.

           See "align_epi_anat.py -help" for details.


- R Reynolds  May, 2011
===========================================================================
"""

helpstr_create_program = """
===========================================================================
This is a brief overview about creating a new program/GUI for uber_proc.py.

There are (currently) 3 basic files used, the main program, the library and
the GUI.  The intention is that one can run the main program to generate or
execute processing scripts (goal of the GUI) without actually using the GUI.
Some common GUI routines/classes are in lib_qt_gui.py.

So the purpose of the GUI is to set user variables to pass to the library.

   1. uber_align_test.py: main program
      - handles just a few options
         - options for help, version, etc.
         - options to set user or other vars (for gui or library)
         - options to execute main processing functions, akin to GUI
      - should be able to create and execute scripts
        (to be able to do the main operations of the GUI)
      - by default, start GUI (unless -no_gui)
      - give command help (this can be very simple, learning is via GUI)

   2. lib_uber_align.py: main library for program
      - defines processing class that accepts user vars and generates
        processing scripts
      - main inputs:
         - user variables *as strings*
           These are converted to local user vars with types, e.g.
           uvars.merge(new_uvars, typedef=user_vars_w_types).  This
           allows higher-level interfaces to not worry about the types.
      - return (internal) data:
         - processing script
         - error list (script is garbage if list is not empty)
         - warning list (to be shown to user, but script may still be good)
         - return vars struct
            - suggested script file name and output file name
      - script is currently created upon init
      - library should be able to do the main work, so command line and GUI
        programs do not repeat functionality
      - examples of additional functions
         - get_script() - return either error string or script text
         - get_warnings() - return warnings string
         - write_script: - go to proc dir, write script (and orig?), return

   3. gui_uber_align_test.py: graphical user interface
      - defines main GUI class that accepts user vars
      - init user vars from library defaults, then merge with any passed
      - purpose is interface to display (string) options to user with useful
        defaults, allowing them to create and execute processing scripts
      - hopefully this can be integrated with uber_proc.py
      - under the main 'uber_results' directory, each tool should write new
        results under 'tool_results/tool.001.align_test', for example
        (making 'tool' output, indexed, and with tool name)


Writing the main program

   This can generally be short.  Start with a few terminal options (e.g. help,
   help_gui, hist, ver), add one for setting user or other options (e.g.
   -uvar), and finally add ability to invoke GUI or create library script.


Writing the library

   Define a class that takes some user variables (VarsObject) as input
   and attempts to create a processing script.  At first, a trivial "script"
   could simply be returned.

   Then have it merge passed vars with defaults and create the script (doing
   error checking as it goes).  Any error messages should be added to the error
   list, and any warnings to to the warnings list.  Errors can be terminal, so
   returning early should be okay.

   The script should be a simple string.  The error and warnings lists should
   be lists of strings.  And the return vars struct is another VarsObject.


Writing the GUI

   Start by tracing the basic GUI to get a feel for what it is doing.  The
   main work is just providing interfaces to control the main variables
   passed to the library.  Smaller things like the menu bar, tool bar, status
   bar, and menu functionality like creating and processing the script can be
   traced separately.

   One could start by having the library genrate a very simple script, and
   then setting up the GUI to deal with it.  Then just add the interfaces for
   all of the variables.  That will allow starting with a testable platform.

   The current 'todo' list when adding a new variable interface to the GUI:

        - init g_subj_defs in library (with default value or None)
        - add GUI for it, initialized by uvar
          (if table, consider 3 functions starting with group_box_gltsym)
        - call-back update (check and set uvar)
           - LineVAR in CB_line_text->update_textLine_check: set uvar
           - if separate button list to update textLine, have callback to
             both update the textLine and to set uvar
           - if table, add to update_svars_from_tables()
              - also, deal with table udpates (e.g. browse, clear, add, help)
              - processed in CB_gbox_PushB?
        - add var to apply_uvar_in_gui (for updating GUI from vars)
        - add var to update_uvars_from_gui
        - add to restoration of defaults, if necessary (cb_clear_options)
        - add GUI help
        - non-GUI: process in creation of script
        - add regression testing case

        * adding a table
           - create table (gvars.Table_gltsym), e.g. write make_gltsym_table
           - create group box for table (e.g. write group_box_gltsym)
           - populate table (vars->table, e.g. write self.gltsym_list_to_table)
           - table->vars noted above (updates_svars_from_tables())
           - resize with resize_table_cols(table)
           * when clearing or initializing the table (maybe with buttons),
             update the vars and call the vars->table function
           * when processing table edits (maybe only when writing script),
             call table->vars function
           * if adding/deleting rows, maybe resize_table_cols()
===========================================================================
"""
