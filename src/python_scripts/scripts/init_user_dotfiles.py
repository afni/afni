#!/usr/bin/env python

# python3 status: compatible

# system libraries
import sys, os
import platform

# AFNI libraries
from afnipy import option_list as OL
from afnipy import afni_base as BASE
from afnipy import afni_util as UTIL

# ----------------------------------------------------------------------
# globals

g_help_string = """
=============================================================================
init_dotfiles.py - innitialize user dotfiles (.cshrc, .tcshrc, .bashrc ...)


   babble...

------------------------------------------

   terminal options:

      -help                     : show this help
      -hist                     : show module history
      -show_valid_opts          : list valid options
      -ver                      : show current version

   other options
      -verb LEVEL               : set the verbosity level

-----------------------------------------------------------------------------
R Reynolds    December 2022
=============================================================================
"""

g_history = """
   init_dotfiles.py history:

   0.0  Dec  8, 2012    - ripped from the heart of @update.afni.binaries...
"""

g_version = "init_dotfiles.py version 0.0, December 8, 2022"

# ---------------------------------------------------------------------------
# general functions

# message functions leaving room for control
def MESG(mstr):
  print(mstr)

def MESGg(mstr, pre=''):
  """general message func"""
  if pre: pre += ' '
  print("%s%s", (pre, mstr))

def MESGe(mstr):
  print("** error: %s" % mstr)

def MESGm(mstr):
  print("-- %s" % mstr)

def MESGp(mstr):
  print("++ %s" % mstr)

# ---------------------------------------------------------------------------


class MyInterface:
   """interface class for MyLibrary (whatever that is)
     
      This uses lib_1D.py as an example."""
   def __init__(self, verb=1):
      # main control variables
      self.valid_opts      = None
      self.user_opts       = None

      # command-line controlled variables
      self.dir_bin         = ''     # dir to add to PATH
      self.dir_work        = ''     # HOME or specified
      self.do_apsearch     = 0      # do we run apserach?
      self.do_dotfiles     = 0      # do we actually modify dotfiles?
      self.do_flatdir      = 0      # do we update for flat_namespace?
      self.make_backup     = 1      # do we run apserach?

      self.verb            = verb

      # uncontrollable variables
      self.bak_suffix      = '.iud.bak' # suffix for backup files
      self.cmd_file        = ''         # write any run shell commands
      self.dir_abin        = ''         # any found abin in PATH
      self.dir_orig        = ''         # starting dir, if we care
      self.tmpfile         = '.tmp.iu.dotfile'  # file for temp writing

      # possible mac stuff
      self.sysname         = platform.system()
      self.is_mac          = self.sysname == 'Darwin'
      self.flatdir         = '/opt/X11/lib/flat_namespace'

      # initialize valid_opts
      self.init_options()

   def show_vars(self, mesg=''):
      """display the class variables
         (all attributes of atomic type)
      """
      # maybe the calling function wants to add a message
      if mesg != '':
         mstr = "(%s) " % mesg
      else:
         mstr = ''

      goodtypes = [str, int, float, bool]

      MESG("== main interface vars %s:" % mstr)
      keys = list(self.__dict__.keys())
      keys.sort()
      for attr in keys:
         val = getattr(self,attr)
         tval = type(val)
         # skip no-atomic types
         if tval not in goodtypes:
            continue
         MESG("   %-15s : %-15s : %s" % (attr, tval, val))
      MESG("")

   def init_options(self):
      self.valid_opts = OL.OptionList('valid opts')

      # short, terminal options
      self.valid_opts.add_opt('-help', 0, [],
                      helpstr='display program help')
      self.valid_opts.add_opt('-hist', 0, [],
                      helpstr='display the modification history')
      self.valid_opts.add_opt('-show_valid_opts', 0, [],
                      helpstr='display all valid options')
      self.valid_opts.add_opt('-ver', 0, [],
                      helpstr='display the current version number')

      # main options
      self.valid_opts.add_opt('-dir_bin', 1, [], 
                      helpstr='directory to add to PATH')
      self.valid_opts.add_opt('-dir_work', 1, [], 
                      helpstr='directory to mod files under (def=$HOME)')
      self.valid_opts.add_opt('-do_apsearch', 1, [], 
                      acplist=['yes','no'],
                      helpstr='make updates regarding apsearch (yes/no)')
      self.valid_opts.add_opt('-do_dotfiles', 1, [], 
                      acplist=['yes','no'],
                      helpstr='actually update the dotfiles (yes/no)')
      self.valid_opts.add_opt('-make_backup', 1, [], 
                      acplist=['yes','no'],
                      helpstr='back up each edited file (yes/no)')

      # general options
      self.valid_opts.add_opt('-verb', 1, [], 
                      helpstr='set the verbose level (default is 1)')

      return 0

   def process_options(self):
      """return  1 on valid and exit        (e.g. -help)
         return  0 on valid and continue    (e.g. do main processing)
         return -1 on invalid               (bad things, panic, abort)
      """

      # process any optlist_ options
      self.valid_opts.check_special_opts(sys.argv)

      # process terminal options without the option_list interface
      # (so that errors are not reported)
      # return 1 (valid, but terminal)

      # if no arguments are given, apply -help
      if len(sys.argv) <= 1 or '-help' in sys.argv:
         MESG(g_help_string)
         return 1

      if '-hist' in sys.argv:
         MESG(g_history)
         return 1

      if '-show_valid_opts' in sys.argv:
         self.valid_opts.show('', 1)
         return 1

      if '-ver' in sys.argv:
         MESG(g_version)
         return 1

      # ============================================================
      # read options specified by the user
      self.user_opts = OL.read_options(sys.argv, self.valid_opts)
      uopts = self.user_opts            # convenience variable
      if not uopts: return -1           # error condition

      # ------------------------------------------------------------
      # process options sequentially, to make them like a script

      for opt in uopts.olist:

         # main options
         if opt.name == '-dir_bin':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return -1
            self.dir_bin = val

         elif opt.name == '-dir_work':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return -1
            self.dir_work = val

         elif opt.name == '-do_apsearch':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return -1
            if val.lower() == 'yes':
               self.do_apserach = 1

         elif opt.name == '-do_dotfiles':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return -1
            if val.lower() == 'yes':
               self.do_dotfiles = 1

         elif opt.name == '-make_backup':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return -1
            if val.lower() == 'yes':
               self.make_backup = 1

         # general options

         elif opt.name == '-verb':
            val, err = uopts.get_type_opt(int, '', opt=opt)
            if val != None and err: return -1
            else: self.verb = val
            continue

      return 0

   def execute(self):
      """main processing
           - dir_abin = `which afni_proc.py`
           - update dir_bin
           - cd to dir_work (usually $HOME)
           ...

         return  0 on success
                 1 on non-fatal termination error
                -1 on fatal error
      """

      if self.verb > 1:
         MESGm('processing...')

      if self.set_dir_vars():
         return -1

      if self.verb > 2:
         self.show_vars("have abin")

      return 0

   def set_dir_vars(self):
      """set dirs: abin, orig, bin, work (where dotfiles are expected to be)
         return 0 on success, -1 on fatal error
      """
      # ------------------------------
      # automatic dirs (orig, abin)
      self.dir_orig = os.getcwd()        # start dir

      if self.set_dir_abin():
         return -1

      # ------------------------------
      # user-controllable dir: bin
      if self.dir_bin == '':
         if self.dir_abin == '':
            MESGe("have no found abin, so please use -dir_bin")
            return -1
         self.dir_bin = self.dir_abin

      # verify dir_bin now
      if not os.path.isdir(self.dir_bin):
         MESGe("-dir_bin is not an existing directory")
         return -1

      # ------------------------------
      # user-controllable dir: work
      if self.dir_work == '':
         # if not set, use HOME directory
         self.dir_work = os.getenv("HOME")

      # verify dir_work now
      if not os.path.isdir(self.dir_work):
         MESGe("-dir_work is not an existing directory")
         return -1

      return 0

   def set_dir_abin(self):
      """try to set an abin directory from the shell PATH
         return 0 on success, -1 on fatal error
      """
      prog = 'afni_proc.py' # since 'afni' might not be in text distribution

      pdir = self.program_dir(prog)
      if pdir != '':
         self.dir_abin = pdir
         if self.verb > 1:
            MESGp("have original abin %s" % self.dir_abin)
         return 0

      # rcr - do anything else here?
      if self.verb > 1:
         MESGm("no %s in original PATH" % prog)

      return 0

   def program_dir(self, prog):
      """return directory that 'prog' resides in (according to 'which')
          - if none found, return ''
      """

      pdir = ''
      cmd = 'which %s' % prog
      st, so, se = BASE.simple_shell_exec(cmd, capture=1)
      if st:
         if self.verb > 2:
            MESGe("no prog %s in PATH" % prog)
         return pdir

      so = so.strip()
      trail = '/%s' % prog
      tlen = len(trail)
      if so[-tlen:] == trail:
         pdir = so[0:-tlen]

      if self.verb > 2:
         MESGp("PATH dir to %s shows '%s'" % (prog, pdir))

      return pdir

   def ready_for_action(self):
      """perform any final tests before execution"""

      ready = 1

      return ready

def main():
   me = MyInterface()
   if not me: return 1

   rv = me.process_options()
   if rv > 0: return 0  # exit with success (e.g. -help)
   if rv < 0:           # exit with error status
      MESGe('failed to process options...')
      return 1

   # else: rv==0, continue with main processing ...

   rv = me.execute()
   if rv > 0: return 0  # non-fatal early termination

   return rv

if __name__ == '__main__':
   sys.exit(main())


