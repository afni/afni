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
DEF_GROUP_ID = 'group_A'             # default group, if none is known

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
"""

g_version = '0.10 (March 14, 2011)'

# ----------------------------------------------------------------------
# global definition of default processing blocks
g_def_blocks      = ['tshift', 'volreg', 'blur', 'mask', 'scale', 'regress']
g_def_blocks_anat = ['tshift', 'align', 'tlrc', 'volreg', 'blur', 'mask',
                     'scale', 'regress']
g_vreg_base_list  = ['first', 'third', 'last']
g_def_vreg_base   = 'third'


# ----------------------------------------------------------------------
# global definitions of subject defaults for single subject analysis

# ---- control values passed in for class actions ----
g_ctrl_defs = SUBJ.VarsObject("uber_subject control defaults")
g_ctrl_defs.subj_dir      = '.'         # destination for scripts and results
g_ctrl_defs.copy_scripts  = 0           # do we make .orig copies of scripts?
g_ctrl_defs.verb          = 1           # verbose level

# ---- resulting values returned after class actions ----
g_res_defs = SUBJ.VarsObject("uber_subject result variables")
g_res_defs.file_ap       = ''           # file name for afni_proc.py command
g_res_defs.file_proc     = ''           # file name for proc script
g_res_defs.results_dir   = ''           # results directory from proc script
g_res_defs.output_proc   = ''           # output from running proc script

# ----------------------------------------------------------------------
# a global definition of subject defaults for single subject analysis
g_subj_defs = SUBJ.VarsObject("Single Subject Dialog defaults")
g_subj_defs.blocks        = []
g_subj_defs.sid           = ''          # subject ID    (no spaces - required)
g_subj_defs.gid           = ''          # group ID      (no spaces)
g_subj_defs.anat          = ''          # anat dset name (probably .HEAD)
g_subj_defs.get_tlrc      = 0           # toggle, include anat+tlrc
g_subj_defs.epi           = []          # EPI dset name list
g_subj_defs.epi_wildcard  = 0           # use wildcard form for EPIs
g_subj_defs.stim          = []          # EPI dset name list
g_subj_defs.stim_wildcard = 0           # use wildcard form for EPIs
g_subj_defs.stim_label    = []          # label for each stim file
g_subj_defs.stim_basis    = []          # basis functions: empty=GAM,
                                        #   valid lengths: 0, 1, len(stim)
g_subj_defs.tcat_nfirst   = 0           # first TRs to remove from each run
g_subj_defs.volreg_base   = g_def_vreg_base  # in g_vreg_base_list, or ''
g_subj_defs.motion_limit  = 0.3         # in mm


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
      self.cvars.merge(cvars)           # expand to include those passed

      self.svars = g_subj_defs.copy()   # start with default subject vars
      self.svars.merge(svars)           # expand to include those passed

      self.rvars = g_res_defs.copy()    # init result vars

      self.set_blocks()                 # choose processing blocks

      self.set_directories()            # data dirs: anat, epi, stim
                                        # (also sets use_dirs, use_adir, etc.)

      self.set_short_names()            # short data dirs: e.g. short_anat

      if self.cvars.verb > 3: self.LV.show('ready to start script')

      self.set_ap_command()             # fill ap_command, warnings, errors

   def set_ap_command(self):
      """attempt to generate an afni_proc.py command in ap_command
            - keep a list of warnings and errors
            - if there are errors, ap_command might not be filled
      """

      self.errors = []                  # list of error strings
      self.warnings = []                # list of warning strings

      # first assign directories
      self.ap_command = self.script_init()
      self.ap_command += self.script_set_dirs()
      self.ap_command += self.script_set_vars()

      self.ap_command += self.script_ap_init()
      self.ap_command += self.script_ap_blocks()
      self.ap_command += self.script_ap_anat()
      self.ap_command += self.script_ap_tcat()
      self.ap_command += self.script_ap_epi()
      self.ap_command += self.script_ap_volreg()
      self.ap_command += self.script_ap_regress()

      # alter ap_command, removing last '\'
      self.ap_command = self.script_ap_nuke_last_LC(self.ap_command)

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

      self.rvars.file_ap = name # store which file we have written to

      if self.cvars.verb>0: print '++ writing afni_proc.py command to %s'%name

      # if requiested, make an original copy
      self.goto_subj_dir()              # if set
      if self.cvars.copy_scripts:       # make an orig copy
         UTIL.write_text_to_file('.orig.%s'%name, self.ap_command)
      rv = UTIL.write_text_to_file(name, self.ap_command)
      self.ret_from_subj_dir()  # if set
         
      return rv

   def goto_subj_dir(self):
      """if cvars.subj_dir is set
            - if subj_dir does not exist, create it
            - set LV.retdir and cd
         (should be called 'atomically' with ret_from_subj_dir)"""
      self.LV.retdir = ''  # init to no return
      sdir = self.cvars.subj_dir
      if sdir == '' or sdir == '.': return

      retdir = os.getcwd()

      # if the directory does not yet exist, create it
      if not os.path.isdir(sdir):
         try: os.makedirs(sdir)
         except:
            print '** failed makedirs(%s)' % sdir
            return

      # now try to go there
      try: os.chdir(sdir)
      except:
         self.LV.retdir = ''
         print '** failed to go to subject dir, %s' % sdir
         return

      self.LV.retdir = retdir   # only set on success

   def ret_from_subj_dir(self):
      """if cvars.subj_dir and LV.retdir are set, cd to LV.retdir
         (should be called 'atomically' with goto_subj_dir)"""

      if self.cvars.subj_dir == '' or self.cvars.subj_dir == '.': return
      if self.LV.retdir      == '' or self.LV.retdir      == '.': return

      try: os.chdir(self.LV.retdir)
      except:
         print '** failed to return to %s from subject dir' % self.LV.retdir

      self.LV.retdir = ''       # either way, nuke old var

   def exec_ap_command(self):
      """execute the script rvars.file_ap, if set
         - return status and command output
      """

      pfile = self.subj_dir_filename('file_ap')
      if not pfile:
         return 1, '** no file set as afni_proc.py command'
      elif not os.path.isfile(pfile):
         return 1, '** afni_proc.py script not found: %s' % pfile

      self.goto_subj_dir()
      cstr = 'tcsh %s' % self.rvars.file_ap
      if self.cvars.verb > 0:
         print "++ executing: %s" % cstr
      cmd = BASE.shell_com('tcsh -c "%s"' % cstr, capture=1)
      cmd.run()
      self.ret_from_subj_dir()

      # possibly make a backup file
      if self.cvars.copy_scripts: self.copy_orig_proc()

      # set results_dir, since we might not be the ones to exec proc script
      if not self.rvars.results_dir:
         self.rvars.results_dir = '%s.results' % self.svars.sid

      return cmd.status, '\n'.join(cmd.so)

   def exec_proc_script(self, xterm=0):
      """execute the script rvars.file_proc, if set
         - if the results dir exists, nuke it
         - execute script (if xterm: run in a new xterm)
         - store results directory
         - return status and command output
      """

      pfile = self.subj_dir_filename('file_proc')
      if not pfile: 
         return 1, '** proc script file not set'
      elif not os.path.isfile(pfile):
         return 1, '** proc script not found: %s' % pfile

      self.nuke_old_results()   # nuke any old results

      self.goto_subj_dir() # ---------- do the work ----------

      # execute script
      pfile = self.rvars.file_proc
      ofile = self.rvars.output_proc
      if ofile: cstr = 'tcsh -xef %s |& tee %s' % (pfile, ofile)
      else:     cstr = 'tcsh -xef %s' % (pfile)
      print '++ executing: %s' % cstr

      capture = 0       # init as output to terminal
      cstr = 'tcsh -c "%s"' % cstr
      if xterm:
         capture = 1
         cstr = 'xterm -e %s' % cstr

      cmd = BASE.shell_com(cstr, capture=capture)
      cmd.run()

      self.ret_from_subj_dir() # ---------- done ----------

      # rcr - return anything?  want to show output.proc.SUBJ?
      return cmd.status, '\n'.join(cmd.so)

   def nuke_old_results(self):
      """if the results directory exists, remove it"""

      if self.rvars.results_dir == '': return

      self.goto_subj_dir()      # ---------- do the work ----------
      
      if os.path.isdir(self.rvars.results_dir):
         print '-- nuking old results: %s' % self.rvars.results_dir
         os.system('rm -fr %s' % self.rvars.results_dir)

      self.ret_from_subj_dir()  # ---------- done ----------


   def copy_orig_proc(self):
      """if the proc script exists, copy to .orig.SCRIPTNAME"""
      if self.rvars.file_proc == '': return
      pfile = self.rvars.file_proc

      self.goto_subj_dir()      # if set
      if os.path.isfile(pfile):
         cmd = 'cp -f %s .orig.%s' % (pfile, pfile)
         if self.cvars.verb > 1: print '++ exec: %s' % cmd
         os.system(cmd)
      elif self.cvars.verb > 1: print "** no proc '%s' to copy" % pfile
      self.ret_from_subj_dir()  # if set

   def script_ap_nuke_last_LC(self, cmd):
      """Find last useful character (not in {space, newline, '\\'}).
         That should end the command (insert newline).
      """

      clen = len(cmd)
      ind = clen-1
      skipchars = [' ', '\t', '\n', '\\']
      while ind > 0 and cmd[ind] in skipchars: ind -= 1

      return cmd[0:ind+1]+'\n\n'

   def script_ap_regress(self):
      """add any -regress_* options
         - start with stim files, labels and basis function(s)
      """
      # stim files, labels, basis functions
      cmd  = self.script_ap_stim()
      cmd += self.script_ap_stim_labels()
      cmd += self.script_ap_stim_basis()

      # motion
      try: mlimit = float(self.svars.motion_limit)
      except:
         self.errors.append("** motion_limit is not float, have %s" \
                            % self.svars.motion_limit)
         return ''
      if mlimit > 0.0:
         cmd += '%s-regress_censor_motion %g \\\n' % (self.LV.istr, mlimit)

      # ------------------------------------------------------------
      # at end, add post 3dD options
         cmd += '%s-regress_est_blur_epits \\\n' \
                '%s-regress_est_blur_errts \\\n' % (self.LV.istr, self.LV.istr)

      return cmd

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
         self.errors.append('** error: no stim timing files given\n')
         return ''
      if len(self.svars.stim) == 0:
         self.errors.append('** error: no stim timing files given\n')
         return ''

      # if wildcard, input files must exist, and expansion must match list
      if self.svars.stim_wildcard:
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

   def script_ap_volreg(self):
      """- possibly set the following options:
           -volreg_align_to, -volreg_align_e2a, -volreg_tlrc_(ad)warp
      """

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

   def script_ap_epi(self):
      """- check for existence of EPI datasets
         - if wildcard is requested, check that the actual wildcard glob
              matches the list of EPI names (else warning)
      """
      if not self.svars.epi:
         self.errors.append('** error: no EPI datasets given\n')
         return ''

      # if wildcard, input files must exist, and expansion must match list
      if self.svars.epi_wildcard:
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
      if self.svars.get_tlrc:   # require existence and +orig extension
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

      if self.cvars.verb > 2: print '-- tlrc file = %s' % file

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
         err = '** cannot use %s wildcard form with missing files: \n' % name
         for file in missing: err += ('      %s\n' % file)
         self.errors.append(err)
         return 1

      # unique (no accidental duplicates)
      if not UTIL.vals_are_unique(flist):
         self.errors.append('** %s filenames are not unique\n' % name)
         return 1

      # sorted
      if not UTIL.vals_are_sorted(flist):
         self.errors.append('** %s filenames are not in alphabetical order\n'\
                            '   (so wildcard order would differ)\n' % name)
         return 1

      # now check that expansion matches files
      # (make wildcard form, then expand it and compare with flist)
      if not UTIL.glob_form_matches_list(flist):
         self.errors.append(                                              \
                '** %s wildcard form does not exactly match file list\n'  \
                '   (so use of wildcard form is not appropriate)\n' % name)
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
      if self.svars.gid: cmd += 'set group     = %s\n' % self.svars.gid
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
         if self.svars.get_tlrc: blocks.remove('tlrc')
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
         return 1, self.make_message_list_string(self.errors, "errors")

      return 0, self.ap_command

   def get_ap_warnings(self):
      """return the number of warnings and a warnings string"""
      return len(self.warnings), \
             self.make_message_list_string(self.warnings, "warnings")

   def make_message_list_string(self, mlist, title):
      if len(mlist) == 0: return ''
      mesg = ''
      for mm in mlist: mesg += (mm + '\n')
      return mesg

   def subj_dir_filename(self, fname):
      """file is either fname or subj_dir/fname (if results is set)
         fname : results file variable
      """
      dir = self.cvars.val('subj_dir')
      fname = self.rvars.val(fname)
      if fname == None or fname == '': return None
      # if no directory, just use fname, else append it to directory
      if dir == None or dir == '' or dir == '.': return fname
      else:                                      return '%s/%s' % (dir, fname)

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
   """return something of the form subject_results/group_A/SUBJ"""

   if subj: # init from subject instance
      sid = subj.svars.val('sid')
      gid = subj.svars.val('gid')

   # if not set from any parameter, use defaults
   if sid == None: sid = DEF_SUBJ_ID
   if gid == None: gid = DEF_GROUP_ID

   sdir = '%s/%s/%s' % (DEF_TOP_SDIR, gid, sid)

   if topdir: return '%s/%s' % (topdir, sdir)
   else:      return sdir

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

def update_vars_from_special(obj, name, vars, check_sort=0):
   """call one of the updates based on obj"""
   if   obj == 'svars': update_svars_from_special(name, vars, check_sort)
   elif obj == 'cvars': update_cvars_from_special(name, vars, check_sort)
   else: print '** ULIB,UVFS: unknown obj type %s' % obj

def update_cvars_from_special(name, cvars, check_sort=0):
   """nothing special to do here yet"""
   return 0

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
         dir, snames, gstr = flist_to_table_pieces(fnames)
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
      dir, snames, gstr = flist_to_table_pieces(fnames)
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

def flist_to_table_pieces(flist):
      """return:
           - common directory name
           - short dlist names (after removing directory name)
           - glob string of short names
         note: short names will be new data (not pointers to flist)
      """
      if len(flist) == 0: return '', [], ''

      ddir = UTIL.common_dir(flist)
      dirlen = len(ddir)
      if dirlen > 0: snames = [dset[dirlen+1:] for dset in flist]
      else:          snames = [dset[:]         for dset in flist]

      globstr = UTIL.glob_form_from_list(snames)

      return ddir, snames, globstr


# ===========================================================================
# help strings accessed both from command-line and GUI
# ===========================================================================

helpstr_usubj_gui = """
===========================================================================
uber_subject.py GUI             - a graphical interface to afni_proc.py

   purposes:

      o  to run a single subject analysis or generate processing scripts
      o  to help teach users:
            - how to process data, including new methods or tools
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

