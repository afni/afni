#!/usr/bin/env python

# python3 status: compatible

# system libraries
import sys, os, platform
import shutil

# AFNI libraries
from afnipy import option_list as OL
from afnipy import afni_util as UTIL
from afnipy import lib_vars_object as VO

# ----------------------------------------------------------------------
# globals

g_help_string = """
=============================================================================
build_afni.py - more plans for world dominance

------------------------------------------
todo:
  - opts for git branch and label
  - if no -package, try to guess
  - allow choice of make target
  - allow source rebuild?

later:
  - worry about sync to abin
  - help

------------------------------------------
examples: ~1~

    0.

------------------------------------------
terminal options: ~1~

      -help                     : show this help
      -hist                     : show module history
      -show_valid_opts          : list valid options
      -ver                      : show current version

other options:

      -build_dir                : root for AFNI build

          This is the root directory that building takes place under.  It
          will contain (if it exists):

                git             - contains 'afni' git repo

          and optionally any of:

                atlases         - atlases to go in abin
                cmake_build     - location of any cmake build
                src_build       - location of any src build
            
      -verb LEVEL               : set the verbosity level (default 1)

          e.g., -verb 2

          Specify how verbose the program should be, from 0=quiet to 4=max.
          As is typical, the default level is 1.

-----------------------------------------------------------------------------
R Reynolds    sometime 2023
=============================================================================
"""

g_history = """
   build_afni.py history:

   0.0  Dec  8, 2022 -
"""

g_prog = "build_afni.py"
g_version = "%s, version 0.0, February 8, 2023" % g_prog

g_git_html = "https://github.com/afni/afni.git"
g_afni_site = "https://afni.nimh.nih.gov"
g_atlas_pack = "afni_atlases_dist"      # package name
g_atlas_html = "%s/pub/dist/atlases/%s.tgz" % (g_afni_site, g_atlas_pack)


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

def MESGi(mstr):
  print("   %s" % mstr)

# basic path functions
def path_head(somepath):
  """like $val:h in tcsh"""

  posn = somepath.rfind('/')

  if posn < 0:
     return somepath
  else:
     return somepath[0:posn]

def path_tail(somepath):
  """like $val:t in tcsh"""

  posn = somepath.rfind('/')

  if posn < 0:
     return somepath
  else:
     return somepath[posn+1:]

# ---------------------------------------------------------------------------
# dir object?  vname (var), dname (on disk), exists, prev,
def dirobj(vname, dname, verb=1):

   vo         = VO.VarsObject(vname)
   vo.vname   = vname                   # variable name (e.g. build_src)
   vo.dname   = dname                   # actual name on disk (prob same)
   vo.exists  = os.path.exists(dname)   # does it exist?

   # use these for any actual work
   vo.abspath = os.path.abspath(dname)  # full path to dname

   vo.head    = path_head(vo.abspath)   # directory containing dname
   vo.tail    = path_tail(vo.abspath)   # trailing dname

   return vo

# ---------------------------------------------------------------------------

class MyInterface:
   """interface class for MyLibrary (whatever that is)
     
      This uses lib_1D.py as an example."""
   def __init__(self, verb=1):
      # main control variables
      self.valid_opts      = None
      self.user_opts       = None

      # command-line controlled variables
      self.build_label     = ''     # most recent label, given one or commit
                                    # (e.g. LAST_LAB, LAST_COMMIT, AFNI_23...)
      self.package         = ''     # to imply Makefile and build dir

      self.clean_root      = 1      # clean old root dirs?
      self.run_cmake       = 0      # actually run camke?
      self.run_make        = 1      # actually run make?
      self.update_git      = 1      # do git clone or pull

      self.verb            = verb   # verbosity level

      # user controlled, but not directly with options

      # uncontrollable variables
      # for dirs git, build_src, build_cmake, atlases:
      #   name, exists, backup
      self.do_orig_abin    = None   # will be set automatically
      self.do_orig_dir     = None   # will be set automatically

      self.do_abin         = None   # abin dirobj
      self.do_root         = None   # build root dirobj

      self.final_mesg      = []     # final messages to show to user
      self.history         = []     # shell/system command history
      self.hist_file       = 'cmd_history.txt' # final history file

      self.pold            = 'prev.'        # prefix for old version
      self.dcbuild         = 'build_cmake'
      self.dsbuild         = 'build_src'

      # system and possible mac stuff
      self.sysname         = platform.system()
      self.is_mac          = self.sysname == 'Darwin'

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

      keys = list(self.__dict__.keys())
      keys.sort()
      nvo = 0
      for attr in keys:
         val = getattr(self,attr)
         tval = type(val)
         # skip invalid types
         if val is None or tval in VO.g_valid_atomic_types:
             MESGi("%-15s : %-15s : %s" % (attr, tval, val))
         elif isinstance(val, VO.VarsObject):
             nvo += 1
             MESGi("%-15s : %-15s : %s" % (attr, tval, val.name))
      if nvo and self.verb > 2:
         MESG("")
         MESG("=== have %d VarsObjects:" % nvo)
         for attr in keys:
            val = getattr(self,attr)
            if isinstance(val, VO.VarsObject):
                val.show()
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
      self.valid_opts.add_opt('-abin', 1, [], 
                      helpstr='dir to put compiled AFNI binaries in')
      self.valid_opts.add_opt('-root_dir', 1, [], 
                      helpstr='the root of the building tree')

      self.valid_opts.add_opt('-package', 1, [], 
                      helpstr='the binary package to build')

      # general options
      self.valid_opts.add_opt('-build_label', 1, [], 
                      helpstr='the git label to build')
      self.valid_opts.add_opt('-clean_root', 1, [], 
                      helpstr='clean up from old work? (def=y)')
      self.valid_opts.add_opt('-run_cmake', 1, [], 
                      acplist=['yes','no'],
                      helpstr="should we run a 'cmake' build?")
      self.valid_opts.add_opt('-run_make', 1, [], 
                      acplist=['yes','no'],
                      helpstr="should we run a 'make' build? (def=y)")
      self.valid_opts.add_opt('-update_git', 1, [], 
                      acplist=['yes','no'],
                      helpstr='should we update the local git repo')
      self.valid_opts.add_opt('-verb', 1, [], 
                      helpstr='set the verbose level (default is 1)')

      return 0

   def process_options(self, argv):
      """return  1 on valid and exit        (e.g. -help)
         return  0 on valid and continue    (e.g. do main processing)
         return -1 on invalid               (bad things, panic, abort)
      """

      # process any optlist_ options
      self.valid_opts.check_special_opts(argv)

      # process terminal options without the option_list interface
      # (so that errors are not reported)
      # return 1 (valid, but terminal)

      # if no arguments are given, apply -help
      if len(argv) <= 1 or '-help' in argv:
         MESG(g_help_string)
         return 1

      # ** -help_{dot,shells}* are below, to handle -verb

      if '-hist' in argv:
         MESG(g_history)
         return 1

      if '-show_valid_opts' in argv:
         self.valid_opts.show('', 1)
         return 1

      if '-ver' in argv:
         MESG(g_version)
         return 1

      # ============================================================
      # read options specified by the user
      self.user_opts = OL.read_options(argv, self.valid_opts)
      uopts = self.user_opts            # convenience variable
      if not uopts: return -1           # error condition

      # ------------------------------------------------------------
      # process options sequentially, to make them like a script

      for opt in uopts.olist:

         # TERMINAL help options that might need -verb

         # main options

         if opt.name == '-abin':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return -1
            self.do_abin = dirobj('abin_dir', val)

         elif opt.name == '-root_dir':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return -1
            self.do_root = dirobj('root_dir', val)

         # general options

         elif opt.name == '-build_label':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return -1
            self.build_label = val

         elif opt.name == '-clean_root':
            if OL.opt_is_yes(opt):
               self.clean_root = 1
            else:
               # do not clean, do not update git
               self.clean_root = 0
               self.update_git = 0

         elif opt.name == '-package':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return -1
            self.package = val

         elif opt.name == '-run_make':
            if OL.opt_is_yes(opt):
               self.run_make = 1
            else:
               self.run_make = 0

         elif opt.name == '-run_cmake':
            if OL.opt_is_yes(opt):
               self.run_cmake = 1
            else:
               self.run_cmake = 0

         elif opt.name == '-update_git':
            if OL.opt_is_yes(opt):
               self.update_git = 1
            else:
               self.update_git = 0

         elif opt.name == '-verb':
            val, err = uopts.get_type_opt(int, '', opt=opt)
            if val == None or err: return -1
            else: self.verb = val

         else:
            MESGe("unhandled option '%s'" % opt.name)
            return -1

      # for now, require self.package
      if self.package == '':
         MESGe("option -package is currently required")

      return 0

   def execute(self):
      """evaluate what needs to be done, report on it, any apply

         - evaluate directories
             - abin_path
             - root_path

         return  0 on success
                 1 on non-fatal termination error
                -1 on fatal error
      """
      # note where we are starting from, and any current abin
      rv = self.set_orig_dirs()
      if rv: return rv

      rv = self.check_progs()
      if rv: return rv

      if self.verb > 2:
          self.show_vars("ready to process...")

      if self.do_root is not None:
         rv = self.run_main_build()

      self.show_history(disp=self.verb>2, save=1, sdir=self.do_root.abspath)

      if self.verb:
         self.show_final_messages()

      return rv

   def show_final_messages(self):
      """display contents of self.final_mesg"""
      if len(self.final_mesg) == 0:
         return 0

      ind = '    '
      MESG("")
      for mesg in self.final_mesg:
         MESGi("%s%s" % (ind, mesg))
      MESG("")

      return 0

   def check_progs(self):
      errs = 0
      plist = ['git', 'make', 'curl']
      if self.run_cmake: plist.append('cmake')

      for prog in ['git', 'make']:
         wp = UTIL.which(prog)
         if wp == '':
            MESGe("missing system program: %s" % prog)
            errs += 1

      if errs:
         return 1
      return 0

   def show_history(self, disp=1, save=0, sdir=''):
      """display or save the history, possibly in a given directory
         (if sdir, cd;write;cd-)  -- does this go in the history??  no???
      """
      if len(self.history) == 0:
         return

      # generate a single message string
      mesg = "shell/system command history:"
      hnew = [mesg]
      for ind, cmd in enumerate(self.history):
         hnew.append('cmd %2d :  %s' % (ind, cmd))
      hstr = '\n   '.join(hnew) + '\n\n'
      del(hnew)

      if disp:
         UTIL.write_text_to_file('-', hstr)

      if save:
         self.write_history_file(hstr, sdir)

   def write_history_file(self, hstr, sdir):
      """cd sdir ; write ; cd -
         return 0 on success, 1 on error
      """
      # convenience
      hfile = self.hist_file

      # if no save dir, just write to current one
      if not sdir:
        return UTIL.write_text_to_file(hfile, hstr)

      if os.path.isdir(sdir):
          cwd = os.path.abspath(os.path.curdir)
          rv, ot = self.run_cmd('cd', sdir, pc=1)
          if rv: return rv

          # possibly make a backue
          if os.path.exists(hfile):
             newf = '%s%s' % (self.pold, hfile)
             rv, ot = self.run_cmd('mv', [hfile, newf], pc=1)
          UTIL.write_text_to_file(hfile, hstr)

          self.final_mesg.append("------------------------------")
          self.final_mesg.append("shell/sytem command history is in:")
          self.final_mesg.append("   %s/%s" % (sdir, hfile))

          rv, ot = self.run_cmd('cd', cwd, pc=1)
          if rv: return rv
      else:
          MESGw("no root dir to write history to: %s" % sdir)

   def run_main_build(self):
      """do the main building and such under do_root

         return 0 on success, else error
      """
      if self.do_root is None:
         return 0

      if self.do_root.exists and self.clean_root:
         if self.clean_old_root():
            return 1

      rv = self.prepare_root()

      # build source - the main purpose of this program
      if self.run_make:
         if self.f_build_via_make():
            return 1

      return 0

   def prepare_root(self):
      """under root_dir, make sure we have current:
            git
            atlases

         return 0 on success, else error
      """

      if self.verb:
         MESGm("preparing root dir, %s" % self.do_root.dname)

      # if there is no root dir yet, make it
      if not os.path.isdir(self.do_root.abspath):
         if self.verb:
            MESGm("creating root dir, %s" % self.do_root.dname)
         st, ot = self.run_cmd('mkdir', self.do_root.abspath, pc=1)
         if st: return st

      # if git exists, do a git pull, else do a clone
      if self.f_update_git():
         return 1

      # get atlases
      if self.f_get_atlases():
         return 1

      return 0

   def f_build_via_make(self):
      """run a make build
            - have or try to choose a suitable package
            - copy git/afni/src tree
            - find corresponding Makefile
            - run build
      """

      MESGm("will run 'make' build of package %s" % self.package)

      st, ot = self.run_cmd('cd', self.do_root.abspath, pc=1)
      if st: return st

      # if we already have a build dir, the user requested not to clean
      if os.path.isdir(self.dsbuild):
         MESGm("will reuse existing src director, %s" % self.dsbuild)
      else:
         st, ot = self.run_cmd('cp -rp', ['git/afni/src', self.dsbuild])
         if st: return st

      # for convenience:
      buildpath = '%s/%s' % (self.do_root.dname, self.dsbuild)

      st, ot = self.run_cmd('cd', self.dsbuild, pc=1)
      if st: return st

      # copy package Makefile
      # if we already have one, the user requested not to clean
      if not os.path.isfile('Makefile'):
         mfile = 'Makefile.%s' % self.package
         mtmp = mfile
         if not os.path.isfile(mfile):
            mtmp = 'other_builds/%s' % mfile
            if not os.path.isfile(mtmp):
               MESGe("failed to find suitable Makefile for package %s" \
                     % self.package)
               return 1

         MESGm("copying %s to %s" % (mtmp, "Makefile"))
         st, ot = self.run_cmd('cp', [mtmp, 'Makefile'], pc=1)
         if st: return st

      # run the build (this is why we are here!)
      self.final_mesg.append("------------------------------")
      self.final_mesg.append("to rerun make build:")
      self.final_mesg.append("   cd %s" % buildpath)
      self.final_mesg.append("   make itall")

      logfile = 'log.make.txt'
      target = 'itall'
      MESGm("building ...")
      MESGi("consider monitoring the build in a separate window with:")
      MESGi("    cd %s" % self.do_orig_dir.abspath)
      MESGi("    tail -f %s/%s" % (buildpath, logfile))
      MESGi("    (use ctrl-c to terminate 'tail' command (not the build))")
      st, ot = self.run_cmd('make %s >& %s' % (target, logfile))

      if st: tmesg = 'FAILED'
      else:  tmesg = 'SUCCEEDED'
      MESGp("building %s" % tmesg)
      MESGi("see log file %s/%s" % (buildpath, logfile))
      if st: return st

      # test the build
      logfile = 'log.test.txt'
      binopt = '-bin_dir %s' % self.package
      MESGm("testing the build results ...")

      cmd = "tcsh scripts_src/test.afni.prog.help %s" % binopt
      self.final_mesg.append("------------------------------")
      self.final_mesg.append("to rerun test of make build:")
      self.final_mesg.append("   cd %s" % buildpath)
      self.final_mesg.append("   %s" % cmd)

      # append redirect to cmd after saving sample command for user
      cmd += " >& %s" % logfile
      st, ot = self.run_cmd(cmd)

      if st: tmesg = 'FAILED'
      else:  tmesg = 'SUCCEEDED'
      MESGp("testing %s" % tmesg)
      MESGi("see log file %s/%s" % (buildpath, logfile))
      if st: return st

      return 0

   def f_get_atlases(self):
      """if no afni_atlases_dist dir, download

         - download g_atlas_pack (package) from g_atlas_html

         return 0 on success
      """
      # be sure to start from the root dir
      st, ot = self.run_cmd('cd', self.do_root.abspath, pc=1)
      if st: return st

      if os.path.exists(g_atlas_pack):
         if not os.path.isdir(g_atlas_pack):
            MESGe("** have root_dir/%s, but it is not a directory??" \
                  % g_atlas_pack)
            return 1

         # consider updating g_atlas_pack dir
         # - or let user delete since we currently have no versioning

         MESGm("will reuse existing atlas directory, %s" % g_atlas_pack)
         return 0

      # make sure there is no previous download
      tgzfile = '%s.tgz' % g_atlas_pack
      if os.path.exists(tgzfile):
         st, ot = self.run_cmd('rm', tgzfile)
         if st: return st

      # download and unpack atlas package
      MESGm("downloading AFNI atlas package, %s" % tgzfile)
      st, ot = self.run_cmd('curl -O', g_atlas_html)
      if st: return st

      MESGm("unpacking atlas package, %s" % g_atlas_pack)
      st, ot = self.run_cmd('tar xfz %s' % tgzfile)
      if st: return st
      st, ot = self.run_cmd('rm', tgzfile)
      if st: return st

      return 0

   def f_update_git(self, gitd='git/afni'):
      """if git exists, do a git pull (if desired), else do a clone
         return 0 on success
      """

      # be sure to start from the root dir
      st, ot = self.run_cmd('cd', self.do_root.abspath, pc=1)
      if st: return st

      if os.path.exists(gitd):
         if self.update_git:
            st, ot = self.run_cmd('cd', gitd, pc=1)
            if st: return st
            st, ot = self.run_cmd('git checkout master')
            if st: return st
            MESGm("running 'git pull' in afni repo...")
            st, ot = self.run_cmd('git pull')
            if st: return st
         else:
            MESGm("skipping 'git pull', using current repo")
      else:
         if not os.path.exists('git'):
            st, ot = self.run_cmd('mkdir', 'git', pc=1)
            if st: return st
         st, ot = self.run_cmd('cd', 'git', pc=1)
         if st: return st

         MESGm("running 'git clone' on afni repo ...")
         MESGi("(please be patient)")
         st, ot = self.run_cmd('git', 'clone %s' % g_git_html)

      return 0

   def clean_old_root(self):
      """an old root_dir exists, clean up or back up

            atlases     : okay, ignore (add opt to update)
            build_cmake : for now, move to prev.build_cmake
            build_src   : for now, move to prev.build_src
            git         : okay, ignore (will pull)

         After this:
            prev.* dirs might exist
            atlases, git might exist
            build_* will not exist

         return 0 on success, else error
      """

      if self.verb:
         MESGm("cleaning old root dir, %s" % self.do_root.dname)

      st, ot = self.run_cmd('cd', self.do_root.abspath, pc=1)
      if st: return st

      # if build dirs exist, rename to prev.*
      for sdir in [self.dsbuild, self.dcbuild]:
         prev = '%s%s' % (self.pold, sdir)
         if os.path.exists(sdir):
             if self.verb:
                MESGm("backing up dir %s" % sdir)

             # remove old prev
             if os.path.exists(prev):
                st, ot = self.run_cmd('rmtree', prev, pc=1)
                if st: return st
             # mv current to prev
             st, ot = self.run_cmd('mv', [sdir, prev], pc=1)
             if st: return st

      return 0

   def run_cmd(self, cmd, params='', pc=0):
      """main purpose is to store commands in history
            cmd     : a simple command or a full one
            params  : can be a simple string or a list of them
            pc      : denotes python command
                        - cmd is a single word ('cd', 'mkdir' ...)
                        - params are separate (we will see how it goes)
         return status, text output
      """
      rv = 0
      if isinstance(params, str):
         pstr = params
      elif isinstance(params, list):
         # if a list, we probably need to proess the list anyway
         pstr = ' '.join(params)
      else:
         MESGe("run_cmd: invalid param type for %s" % params)
         return 1, ''

      if self.verb > 2:
         MESGm("attempting cmd: %s %s" % (cmd, pstr))

      # handle system commands first (non-python commands, pc=0)
      if not pc:
         if pstr != '': cmd += " %s" % pstr
         self.history.append(cmd)
         st, otext = UTIL.exec_tcsh_command(cmd)
         return st, otext

      # now python commands
      if cmd == 'cd':
         cstr = "os.chdir('%s')" % pstr
         self.history.append(cstr)
      elif cmd == 'mkdir':
         cstr = "os.makedirs('%s')" % pstr
         self.history.append(cstr)
      elif cmd == 'cp':
         cstr = "shutil.copy('%s', '%s')" % (params[0], params[1])
         self.history.append(cstr)
      elif cmd == 'mv':
         # fail here for a list of params
         try:
            cstr = "os.rename('%s', '%s')" % (params[0], params[1])
         except:
            MESGe("os.rename(%s) - missing params" % pstr)
            return 1, ''
         self.history.append(cstr)
      elif cmd == 'rmtree':
         # allow this to be a file, to simply and work like rm -fr
         if os.path.isfile(pstr):
            cstr = "os.remove('%s')" % pstr
         else:
            cstr = "shutil.rmtree('%s')" % pstr
         self.history.append(cstr)
      else:
         MESGe("unknown run_cmd: %s %s" % (cmd, pstr))
         return 1, ''

      # ready to run it
      try:
         eval(cstr)
      except:
         MESGe("failed run_cmd: %s %s" % (cmd, pstr))
         rv = 1

      return rv, ''

   def set_orig_dirs(self):
      """note starting dir, and any abin in PATH

         try to set an abin directory from the shell PATH

         return 0 on success, -1 on fatal error
      """
      # note where we are starting from
      self.do_orig_dir = dirobj('orig_dir', '.')

      prog = 'afni_proc.py' # since 'afni' might not be in text distribution
      wp = UTIL.which(prog)
      if wp != '':
         self.do_orig_abin = dirobj('orig_abin_dir', wp)
         if self.verb > 1:
            MESGp("have original abin %s" % self.do_orig_abin.abspath)
      else:
         if self.verb > 1:
            MESGm("no %s in original PATH to set orig_abin from" % prog)

      return 0

def main(argv):
   me = MyInterface()
   if not me: return 1

   rv = me.process_options(argv)
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
   sys.exit(main(sys.argv))


