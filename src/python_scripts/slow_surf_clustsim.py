#!/usr/bin/env python

# system libraries
import sys, os

try:    sys.path.extend(['.', '%s/abin' % os.getenv('HOME')])
except: pass

# AFNI libraries
import option_list as OPT
import afni_util as UTIL
import lib_subjects as SUBJ
import lib_surf_clustsim as CLUST

# ----------------------------------------------------------------------
# globals

g_command_help = """
=============================================================================
slow_surf_clustsim.py    - generate a tcsh script to run clustsim on surface

------------------------------------------

   examples:

   1. basic: give 3 required inputs, all else is default

        slow_surf_clustsim.py -save_script surf.clustsim \\
            -uvar spec_file sb23_lh_141_std.spec         \\
            -uvar surf_vol sb23_SurfVol_aligned+orig     \\
            -uvar vol_mask mask_3mm+orig

   2. basic, but on the surface (so no vol_mask is provided)

        slow_surf_clustsim.py -save_script surf.sim.2    \\
            -on_surface                                  \\
            -uvar spec_file sb23_lh_141_std.spec         \\
            -uvar surf_vol sb23_SurfVol_aligned+orig

------------------------------------------

   script outline:

        set control variables
        create and enter results directory
        convert p-value list (pthr_list) to z-scores (zthr_list)
        create dummy time series of length itersize
        for each iter ( iteration list )
            3dcalc: generate noise volume
            3dVol2Surf: map noise to surface
            SurfSmooth: blur to FWHM
            for each index ( itersize list )
                for each zthr ( zthr_list )
                    SurfClust: make clust file clust.out.$iter.$index.$zthr
        extract lists of maximum areas

------------------------------------------

   terminal options:

      -help                     : show this help
      -hist                     : show module history
      -show_default_vars        : list user variable defaults
      -show_valid_opts          : list valid options
      -ver                      : show current version

   other options
      -on_surface               : start from noise on the surface
                                  (so no volume data is involved)
      -print_script             : print script to terminal
      -save_script FILE         : save script to given file
      -uvar value ...           : set the user variable
                                  (use -show_default_vars to see user vars)
      -verb LEVEL               : set the verbosity level

-----------------------------------------------------------------------------
R Reynolds    June 2011
=============================================================================
"""


class MyInterface:
   """interface class for MyLibrary (whatever that is)
     
      This uses lib_1D.py as an example."""
   def __init__(self, verb=1):
      # main variables
      self.status          = 0                       # exit value
      self.valid_opts      = None
      self.user_opts       = None

      # general variables
      self.verb            = verb

      # initialize valid_opts
      self.valid_opts = self.get_valid_opts()

   def get_valid_opts(self):
      vopts = OPT.OptionList('valid opts')

      # short, terminal arguments
      vopts.add_opt('-help', 0, [], helpstr='display program help')
      vopts.add_opt('-hist', 0, [], helpstr='display the modification history')
      vopts.add_opt('-show_default_vars',0,[],helpstr='show variable defaults')
      vopts.add_opt('-show_valid_opts',0,[],helpstr='display all valid options')
      vopts.add_opt('-ver', 0, [],helpstr='display the current version number')

      vopts.add_opt('-cvar', -2, [], helpstr='set control variable')
      vopts.add_opt('-uvar', -2, [], helpstr='set user variable to value')

      # general options
      vopts.add_opt('-on_surface', 1, [],
                    helpstr='work directly on the surface (yes/no)')
      vopts.add_opt('-print_script', 0, [],
                    helpstr='print script to terminal window')
      vopts.add_opt('-save_script', 1, [],
                    helpstr='write script to given file')
      vopts.add_opt('-verb', 1, [],helpstr='set the verbose level (default=1)')

      return vopts

   def process_options(self):

      argv = sys.argv

      if len(argv) == 0:        # non-gui out
         print g_command_help
         return 1

      # process any optlist_ options
      self.valid_opts.check_special_opts(argv)

      # process terminal options without the option_list interface
      # (so that errors are not reported)

      # if no arguments are given, apply -help
      if len(argv) <= 1 or '-help' in argv:
         print g_command_help
         return 1

      if '-hist' in argv:
         print CLUST.g_history
         return 1

      if '-show_default_vars' in argv:
         CLUST.g_user_defs.show('')
         return 1

      if '-show_valid_opts' in argv:
         self.valid_opts.show('', 1)
         return 1

      if '-ver' in argv:
         print CLUST.g_version
         return 1

      # ============================================================
      # read options specified by the user
      self.user_opts = OPT.read_options(argv, self.valid_opts)
      uopts = self.user_opts            # convenience variable
      if not uopts: return -1           # error condition

      # ------------------------------------------------------------
      # init subject options struct

      self.cvars = SUBJ.VarsObject('control vars from command line')
      self.uvars = SUBJ.VarsObject('user vars from command line')

      val, err = uopts.get_type_opt(int, '-verb')
      if val != None and not err: self.verb = val

      SUBJ.set_var_str_from_def('uvars', 'verb', ['%d'%self.verb], self.uvars,
                                 defs=CLUST.g_user_defs)

      # first process all setup options
      errs = 0
      for opt in uopts.olist:
         # skip -verb (any terminal option should block getting here)
         if opt.name == '-verb':                continue

         # and skip and post-setup options (print command, save, etc.)
         elif opt.name == '-print_script':      continue
         elif opt.name == '-save_script':       continue

         # now go after "normal" options

         elif opt.name == '-on_surface':
            val, err = uopts.get_string_list('', opt=opt)
            if val != None and err: return -1
            if self.cvars.set_var_with_defs(opt.name[1:],val,CLUST.g_ctrl_defs,
                        as_type=1, oname='cvars', verb=self.verb) < 0:
               errs += 1
               continue

         # cvar requires at least 2 parameters, name and value
         elif opt.name == '-cvar':
            val, err = uopts.get_string_list('', opt=opt)
            if val != None and err: return -1
            # and set it from the form name = [value_list]
            if SUBJ.set_var_str_from_def('cvars', val[0], val[1:], self.cvars,
                        CLUST.g_ctrl_defs, verb=self.verb) < 0:
               errs += 1
               continue

         # uvar requires at least 2 parameters, name and value
         elif opt.name == '-uvar':
            val, err = uopts.get_string_list('', opt=opt)
            if val != None and err: return -1
            # and set it from the form name = [value_list]
            if SUBJ.set_var_str_from_def('uvars', val[0], val[1:], self.uvars,
                        CLUST.g_user_defs, verb=self.verb) < 0:
               errs += 1
               continue

         else:
            print '** unknown option %s' % opt.name
            errs += 1

      if self.verb > 2:
         print '-' * 75
         self.uvars.show('post-init uvars', name=0)
         self.cvars.show('post-init cvars', name=0)
         print '-' * 75

      if errs:    return -1
      else:       return  0     # no error, and continue on return

   def execute(self):

      if not self.ready_for_action(): return 1

      if self.verb > 1: print '-- processing...'

      uopts = self.user_opts

      if uopts.find_opt('-print_script'): self.print_script()

      opt = uopts.find_opt('-save_script')
      if opt != None:
         val, err = uopts.get_string_opt('', opt=opt)
         if val != None and not err: self.save_script(val)

   def ready_for_action(self):
      """perform any final tests before execution"""

      ready = 1

      return ready

   def print_script(self):
      """create script and print to terminal"""

      ctest, cmd = self.get_script()
      print cmd

   def save_script(self, fname):

      ctest, cmd = self.get_script()
      if cmd == '': return

      if ctest.write_script(fname):
         print '** failed to write slow_surf_clustsim script to disk'

   def get_script(self):
      """return the SurfClust object and script
         (print warnings and errors to screen)"""

      ctest = CLUST.SurfClust(self.cvars, self.uvars)

      nwarn, wstr = ctest.get_warnings()
      status, mesg = ctest.get_script()

      if status:        # only show errors
         print '%s\nERRORS:\n\n%s\n' % (75*'*', mesg)
         cmd = ''
      else:
         if wstr: print '%s\n**** Warnings:\n\n%s\n%s\n' % (75*'-',wstr,75*'-')
         cmd = '### surf clust script:\n\n%s\n' % mesg

      return ctest, cmd

   def test(self, verb=3):
      print '------------------------ initial tests -----------------------'
      self.verb = verb

      print '------------------------ reset files -----------------------'

      print '------------------------ should fail -----------------------'

      print '------------------------ more tests ------------------------'

      return None

def main():
   me = MyInterface()
   if not me: return 1
   if me.status: return me.status

   rv = me.process_options()
   if rv > 0:  return 0 # terminal success
   if rv < 0:  return 1 # terminal failure
   # else rv == 0, so continue

   rv = me.execute()
   if rv > 0: return 1

   return me.status

if __name__ == '__main__':
   sys.exit(main())


