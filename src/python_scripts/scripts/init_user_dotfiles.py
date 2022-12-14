#!/usr/bin/env python

# python3 status: compatible

# system libraries
import sys, os
import platform

# AFNI libraries
from afnipy import option_list as OL
from afnipy import afni_base as BASE
from afnipy import afni_util as UTIL
from afnipy import lib_vars_object as VO

# ----------------------------------------------------------------------
# globals

g_help_string = """
=============================================================================
init_dotfiles.py - initialize user dotfiles (.cshrc, .tcshrc, .bashrc ...)

   For some background, please see:

      afni_system_check.py -help_dot_files

   1. Add ABIN to the PATH in all evaluated dot/RC files.
      ABIN can be set by -dir_bin, else it will be come from:

         which afni_proc.py

   2. If requested and on a mac, set DYLD_LIBRARY_PATH.

   3. If requested, run apsearch?  Maybe?  We shall see...


   rcr - more babble...

------------------------------------------
examples:

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

   0.0  Dec  8, 2012 - ripped from the heart of @update.afni.binaries...
                       encased in a block of ice... zillagod
   0.1  Dec 14, 2012 - file objects, tokens, -force 
"""

g_version = "init_dotfiles.py version 0.1, December 14, 2022"

g_rc_all = [ '.bash_dyld_vars', '.bash_login', '.bash_profile', '.bashrc',
             '.cshrc', '.login', '.tcshrc',
             '.profile',
             '.zlogin', '.zprofile', '.zshenv', '.zshrc']

g_rc_mod = [ '.profile',
             '.bash_profile', '.bashrc',
             '.zshrc',
             '.cshrc', '.tcshrc',
           ]


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

def MESGw(mstr):
  print("** warning: %s" % mstr)

def MESGm(mstr):
  print("-- %s" % mstr)

def MESGp(mstr):
  print("++ %s" % mstr)

# creation of file object
def file_object(fname, verb=1):
   """return a vars object for the given file"""

   vo = VO.VarsObject(fname)
   vo.name   = fname    # already implied, buy hey
   vo.isfile = os.path.isfile(fname)
   vo.isread = 0
   vo.tlines = []
   # try to read, but no whining
   if vo.isfile:
      try:
         tlines = UTIL.read_text_file(fname, lines=1, strip=0, verb=0)
         vo.isread = 1
         vo.tlines = tlines
      except:
         if verb: MESGw("failed to read existing dot file %s" % fname)
   vo.nlines = len(vo.tlines)
   vo.follow = 0    # for .[t]cshrc, does it source the other

   # note whether to modify, and then what mods are needed
   vo.nmods  = 0    # main question, number of desired mods to file
   vo.m_path = 0    # needs mod: PATH
   vo.m_flat = 0    # needs mod: flat_namespace
   vo.m_aps  = 0    # needs mod: apsearch

   return vo

# akin to grep
def fo_has_token(fo, t0, sub0=0):
   """does some file_object line contain token t0?
      (like grep -ql, but try to handle comments)

      - ignore anything after a '#'
      - if sub0, that token can be a substring
      - do not try to be TOO intelligent
         - no line continuation searches
   """
   for tline in fo.tlines:
      cposn = tline.find('#')
      # skip any pure comment line
      if cposn == 0:
         continue
      if cposn > 0:
         tt = tline[0:cposn]
      else:
         tt = tline
      toks = tt.split()
      if len(toks) < 1:
         continue

      # do we have our token?
      found = False
      for tind in range(len(toks)):
         tok = toks[tind]
         if sub0: found = tok.find(t0) >= 0
         else:    found = t0 == tok

         # MAIN CHECK: is it there?
         if found:
            return 1

   return 0

# akin to grep, but want sequential tokens not after comment
def fo_has_token_pair(fo, t0, t1, sub0=0, sub1=0):
   """does some file_object line contain sequential tokens t0 and t1?

      - ignore anything after a '#'
      - if sub0,sub1, that token can be a substring
      - do not try to be TOO intelligent
         - no line continuation searches
   """
   for tline in fo.tlines:
      cposn = tline.find('#')
      # skip any pure comment line
      if cposn == 0:
         continue
      if cposn > 0:
         tt = tline[0:cposn]
      else:
         tt = tline
      toks = tt.split()
      if len(toks) < 2:
         continue

      # we have at least 2 tokens, try find t0
      found = False
      for tind in range(len(toks)-1):
         tok = toks[tind]
         if sub0: found = t0 in tok
         else:    found = t0 == tok
         if found:
            if sub1: found = t1 in toks[tind+1]
            else:    found = t1 == toks[tind+1]

         # MAIN CHECK: do we have a pair?
         if found:
            return 1

   return 0

# ---------------------------------------------------------------------------


class MyInterface:
   """interface class for MyLibrary (whatever that is)
     
      This uses lib_1D.py as an example."""
   def __init__(self, verb=1):
      # main control variables
      self.valid_opts      = None
      self.user_opts       = None

      # command-line controlled variables
      self.dflist          = None   # user-specified dotfile list
      self.dir_bin         = ''     # dir to add to PATH
      self.dir_dot         = ''     # HOME or specified location of DF
      self.do_apsearch     = 0      # do we run apserach?
      self.do_dotfiles     = 0      # do we actually modify dotfiles?
      self.do_flatdir      = 0      # do we update for flat_namespace?
      self.force           = 0      # force updates, even for grep failures
      self.make_backup     = 1      # do we run apserach?
      self.verb            = verb

      # rcr - info that means not making any edits
      self.list_dotfiles   = 0
      self.dry_run         = 0
     
      # uncontrollable variables
      self.bak_suffix      = '.iud.bak' # suffix for backup files
      self.cmd_file        = ''         # write any run shell commands
      self.dir_abin        = ''         # any found abin in PATH
      self.dir_orig        = ''         # starting dir, if we care
      self.dfobjs          = {}         # dict of VO for each file
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

      goodtypes = [str, int, float, bool, list]

      MESG("== main interface vars %s:" % mstr)
      keys = list(self.__dict__.keys())
      keys.sort()
      for attr in keys:
         val = getattr(self,attr)
         tval = type(val)
         # skip invalid types
         if val is None or tval in goodtypes:
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
      self.valid_opts.add_opt('-dflist', -1, [], 
                      helpstr='specify list of dotfiles to possibly modify')
      self.valid_opts.add_opt('-dir_bin', 1, [], 
                      helpstr='directory to add to PATH')
      self.valid_opts.add_opt('-dir_dot', 1, [], 
                      helpstr='directory to mod files under (def=$HOME)')
      self.valid_opts.add_opt('-do_apsearch', 1, [], 
                      acplist=['yes','no'],
                      helpstr='make updates regarding apsearch (yes/no)')
      self.valid_opts.add_opt('-do_dotfiles', 1, [], 
                      acplist=['yes','no'],
                      helpstr='actually update the dotfiles (yes/no)')
      self.valid_opts.add_opt('-force', 1, [], 
                      acplist=['yes','no'],
                      helpstr='force all updates (yes/no)')
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
         if opt.name == '-dflist':
            val, err = uopts.get_string_list('', opt=opt)
            if val == None or err: return -1
            self.dflist = val

         elif opt.name == '-dir_bin':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return -1
            self.dir_bin = val

         elif opt.name == '-dir_dot':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return -1
            self.dir_dot = val

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

         elif opt.name == '-force':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return -1
            if val.lower() == 'yes':
               self.force = 1

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
           - cd to dir_dot (usually $HOME)
           ...

         return  0 on success
                 1 on non-fatal termination error
                -1 on fatal error
      """

      if self.verb > 1:
         MESGm('processing...')

      # set up and verify directory variables
      if self.set_dir_vars():
         return -1

      # 'cd' to dir_dot
      try:
         os.chdir(self.dir_dot)
      except:
         MESGe("failed to 'cd' to dir_dot, '%s'" % self.dir_dot)
         return -1

      # and attack dot/rc files
      if self.modify_dotfiles():
         return -1

      return 0

   def modify_dotfiles(self):
      """first detect what updates might be needed, then make them
         - use self.dflist if not None, else the default g_rc_mod
      """

      # init dflist if not set by user
      if self.dflist is None:
         if self.verb > 1:
            MESGm("using default dotfile list")
         self.dflist = g_rc_mod

      rv = self.set_file_mod_flags()

      return rv

   def set_file_mod_flags(self):
      """within the dfobjs list, set all mod flags
         - this is informative, do not actually make changes here
         - return status (0 on success) and the new list

         side effect: populate dfobjs: nmods and m_*

         0. dfile list entries should be known and not conntain paths
         1. populate dfobjs (read in all found files)
         2. if both .cshrc and .tcshrc, determine whether one soures the other
         3. main point: for each file obj, set 'nmods'
            - 

         possible list: '.bash_profile', '.bashrc', '.cshrc', '.tcshrc',
                        '.profile', '.zshrc'

         return 0 on success
      """

      # ------------------------------------------------------------
      # check that entires are known (and do not contain '/')
      errs = 0
      for fname in self.dflist:
         if fname not in g_rc_mod:
            if fname.find('/') >= 0 :
               MESGe("dotfile %s contain a path ('/' char)\n"%fname +
                     "   (use -dir_dot to specify location)")
            else:
               MESGe("not sure how to modify file %s" % fname)
            errs += 1

      # fail on any bad names
      if errs:
         return errs

      # ------------------------------------------------------------
      # try to read in whatever files exist into file objects
      for dfname in self.dflist:
         vo = file_object(dfname, verb=self.verb)
         self.dfobjs[dfname] = vo
         if self.verb > 1:
            if vo.isfile: lstr = "%3d lines" % vo.nlines
            else:         lstr = "not found"
            MESGp("%-20s : %s" % (dfname, lstr))

      # ------------------------------------------------------------
      # check on having both .cshrc and .tcshrc
      errs = self.check_for_cshrc_tcshrc()

      # ------------------------------------------------------------
      # check on generally needed mods: 
      errs += self.note_files_to_modify()

      return errs

   def note_files_to_modify(self):
      """depending on options, check (and set corresponding m_ for):
            - m_path: adding to PATH
            - m_flat: applying flat_namespace
            - m_aps:  applying apsearch
      """
      # check for needed path update
      for name, fo in self.dfobjs.items():
         self.check_to_set_path(fo)

      # check for needed flat_namespace update
      if self.do_flatdir:
         for name, fo in self.dfobjs.items():
            self.check_to_add_flatdir(fo)

      # check for needed apsearch update
      if self.do_apsearch:
         for name, fo in self.dfobjs.items():
            self.check_to_add_apsearch(fo)

      # let user know of plans
      if self.verb > 0:
         ndfo = len(self.dfobjs)
         ntot = sum([fo.nmods for n,fo in self.dfobjs.items()])
         if ntot == 0:
            MESG("no modifcations needed across %d files" % ndfo)
         else:
            MESG("want %d modifications across %d files:" % (ntot, ndfo))
            MESG("   file             path  flatdir  apsearch\n" \
                 "   ---------------  ----  -------  --------")
            for name, fo in self.dfobjs.items():
               ml = []
               MESG("   %-15s  %-4d  %-7d  %-8d" % \
                    (name, fo.m_path, fo.m_flat, fo.m_aps))

      return 0

   def check_to_add_flatdir(self, fo):
      pass
   def check_to_add_apsearch(self, fo):
      pass
   def check_to_set_path(self, fo):
      """try to determine whether file is setting dir_bin

         How to check PATH at this point?
           - for now, mimic @uab and do a simple grep for the abin tail
      """
      doit = 0
      if self.verb > 2:
         MESG("== check_to_set_path: %s" % fo.name)

      # if follower, no worries
      if fo.follow:
         if self.verb > 2:
            MESGm("file %s is a follower" % fo.name)
      # if forcing updates, no worries
      elif self.force:
         if self.verb > 2:
            MESGm("file %s has forced updates" % fo.name)
         doit = 1

      # actual "thinking": for now, just search for PATH tail
      else:
         tail = self.path_tail(self.dir_bin)
         if fo_has_token(fo, tail, sub0=1):
            if self.verb > 2:
               MESGm("found PATH token %s in file %s" % (tail, fo.name))
         else:
            if self.verb > 2:
               MESGm("no PATH token %s in file %s" % (tail, fo.name))
            doit = 1

      if doit:
         if self.verb > 2:
            MESGp("will add %s to PATH in file %s" % (self.dir_bin, fo.name))
         fo.nmods += 1
         fo.m_path = 1

   def path_tail(self, somepath):
      """like $val:t in tcsh"""

      posn = somepath.rfind('/')

      if posn < 0:
         return somepath
      else:
         return somepath[posn+1:]

   def check_for_cshrc_tcshrc(self):
      """if both .cshrc and .tcshrc exist, we would like to see that 
         .cshrc is sourced by .tcshrc

         side effect: set follow if sourcing the other dot file
         return 1 on some fatal error
      """
      fc = '.cshrc'
      ft = '.tcshrc'
      in_c = fc in self.dflist
      in_t = ft in self.dflist

      # if neither file is of interest, just run away
      if not in_c and not in_t:
         return 0

      # we are checking at least one file, do they both exist?
      if in_c: ec = self.dfobjs[fc].isfile
      else:    ec = os.path.isfile(fc)
      if in_t: et = self.dfobjs[ft].isfile
      else:    et = os.path.isfile(ft)

      # if not, just run away
      if not ec or not et:
         return 0

      # both files exist, and at least one is in dflist, so we want .cshrc
      # to be sourced by .tcshrc (or the reverse), and therefore we might
      # need file objects for each

      # create an object if not in dfobjs
      if in_c: fco = self.dfobjs[fc]
      else:    fco = file_object(fc, verb=self.verb)
      if in_t: fto = self.dfobjs[ft]
      else:    fto = file_object(ft, verb=self.verb)

      # ------------------------------------------------------------
      # main work: does .tcshrc source .cshrc?
      sd = fo_has_token_pair(fto, '.', fc, sub0=0, sub1=1)
      ss = fo_has_token_pair(fto, 'source', fc, sub0=0, sub1=1)
      follow_t = sd or ss

      sd = fo_has_token_pair(fco, '.', ft, sub0=0, sub1=1)
      ss = fo_has_token_pair(fco, 'source', ft, sub0=0, sub1=1)
      follow_c = sd or ss

      # we want exactly one to follow the other
      retval = 0

      if follow_t:
         fto.follow = 1
         if self.verb > 0:
            MESGm("good: %s seems to contain 'source %s'" % (ft, fc))
         # if they source each other, this should be fatal
         if follow_c:
            MESGe("both %s and %s seem to source each other" % (ft, fc))
            retval = 1
      elif follow_c:
         fco.follow = 1
         if self.verb > 0:
            MESGm("good: %s seems to contain 'source %s'" % (fc, ft))
      else:
         if self.verb > 0:
            MESGw("%s does NOT seem to contain 'source %s'" % (ft, fc))
            MESG("   (csh and tcsh will use different files)")

      # free any temporary file object
      if not in_c: del(fco)
      if not in_t: del(fto)

      return retval

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
         # then use dir_abin, if populated
         if self.dir_abin != '':
            MESGe("have no found abin, so please use -dir_bin")
            return -1
         self.dir_bin = self.dir_abin
         if self.verb > 1:
            MESGm("setting dir_bin to ABIN %s" % self.dir_abin)

      # verify dir_bin now
      if not os.path.isdir(self.dir_bin):
         MESGe("-dir_bin is not an existing directory, too afraid to proceed")
         return -1

      # and require an absolute path (could be applied from anywhere)
      if self.dir_bin[0] != '/':
         MESGe("-dir_bin must be an absolute path (start with '/'),\n" +
               "   since dot files can be sourced from any directory\n" +
               "   dir_bin = %s" % self.dir_bin)
         return -1

      # ------------------------------
      # user-controllable dir: work
      if self.dir_dot == '':
         # if not set, use HOME directory
         self.dir_dot = os.getenv("HOME")
         if self.verb > 1:
            MESGm("setting dir_dot to $HOME")

      # verify dir_dot now
      if not os.path.isdir(self.dir_dot):
         MESGe("-dir_dot is not an existing directory: '%s'" % self.dir_dot)
         return -1

      if self.verb > 2:
         self.show_vars("have abin")

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
            MESGw("no prog %s in PATH" % prog)
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
   if rv < 0: return 1  # fatal

   return 0

if __name__ == '__main__':
   sys.exit(main())


