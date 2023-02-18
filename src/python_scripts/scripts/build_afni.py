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
build_afni.py - compile an AFNI package ~1~

   This program is meant to compile AFNI from the git repository.
   It is intended as a quick and convenient way to compile.

 * This is NOT intended as a platform for developers.
   This is meant only for compiling, not for making updates to the code.

   The main process (for a new directory) might be something like:

      - create working tree from the specified -root_dir
      - prepare git
         - clone AFNI's git repository
         - possibly checkout a branch (master)
         - possibly checkout the most recent tag (AFNI_XX.X.XX)
      - prepare atlases
         - download and extract afni_atlases_dist.tgz package
      - prepare src build
         - copy git/afni/src to build_src
         - copy specified Makefile
         - run build
      - prepare cmake build (optional)
         - run build

   Final comments will be shown about:

      - how to rerun the make build
      - how to rerun the make build test
      - where a command (shell/system) history file is stored
        (cmd_history.txt is generally stored in the -root_dir)

------------------------------------------
examples: ~1~

   0. basic, start fresh or with updates ~2~

      Either start from nothing from a clean and updated version.

        build_afni.py -root_dir my/build/dir

      notes:
        - if there is an existing git tree, pull any updates
        - if there is an existing build_src tree, rename it and start clean

   1. simple, but continue where we left off ~2~

      Use this method to :
        - continue a previously terminated build
        - to rebuild after making updates to the build_src tree
        - to rebuild after installing additional system libraries

        build_afni.py -root_dir my/build/dir -clean_root no

      notes:
        - if there is an existing git tree, use it (with no updates)
        - if there is an existing build_src directory, keep and use it

   2. basic, but specify an existing build package ~2~

      This implies a Makefile to use for the build.

        build_afni.py -root_dir my/build/dir -package linux_centos_7_64

   3. use an alternate Makefile, but do not update git repo ~2~

        build_afni.py -root_dir my/build/dir -git_update no \\
                      -makefile my_better_makefile

   4. test the setup, but do not run any make (using -prep_only) ~2~

        build_afni.py -root_dir my/build/dir -prep_only \\
                      -git_update no -makefile my_better_makefile 


------------------------------------------
todo:
  - opts to pass to cmake
  - given a Makefile, will want to detect output package name
  - pick a method for guessing an appropriate Makefile
    Ubuntu vs Fedora vs RedHat vs other vs macos (12+?)

later:
  - worry about sync to abin

------------------------------------------
terminal options: ~1~

      -help                     : show this help
      -hist                     : show module history
      -show_valid_opts          : list valid options
      -ver                      : show current version

required:

      -root_dir                 : root directory to use for git and building

other options:

      -clean_root yes/no        : specify whether to clean up the root_dir

          default -clean_root yes
          e.g.    -clean_root no

          If 'no' is specified, the git directory will not be updated and the
          build_src directory will not be remade.

      -git_branch BRANCH        : specify a branch to checkout in git

          default -git_branch master
          e.g.    -git_branch some_stupid_branch

          This will just lead to 'git checkout BRANCH'.

      -git_tag TAG              : specify a tag to checkout in git

          default -git_tag LAST_TAG
          e.g.    -git_tag NONE

          This will lead to 'git checkout TAG', of some sort, depending:

             LAST_TAG   : checkout most recent (annotated) AFNI_XX.X.XX tag.
                          (annotated tags come from official AFNI builds)
             NONE       : do not checkout any specific tag

      -git_update yes/no        : specify whether to update git repo

          default -git_update yes
          e.g.    -git_update no

          If 'no' is specified, the git/afni/src tree must already exist, and
          nothing will be done to it.  This option cannot be used with
          -git_branch or -git_tag.

      -make_target TARGET       : specify target for make command

          default -make_target itall
          e.g.    -make_target totality
          e.g.    -make_target afni

          When the 'make' command is run under build_src, use the given target.
          Since an individual program make would probably be done directly on
          the command line (rather than using this program), the most typical
          reason to do this might be to save disk space.  Using totality
          (instead of itall) would clean up after the make.

      -makefile MAKEFILE        : specify an alternate Makefile to build from

          default -makefile Makefile.PACKAGE (for the given PACKAGE)
          e.g.    -makefile my.better.makefile

          This option is a mechanism for specifying a Makefile that is not
          (currently) part of the AFNI distribution.

      -package PACKAGE          : specify the desired package to build

          e.g. -package linux_centos_7_64

          The package will imply a Makefile to use, Makefile.PACKAGE.
          It will also be the name of the output binary directory.

      -prep_only                : prepare to but do not run (c)make

          e.g. -prep_only

          This is for testing or for practice.

          Do everything leading up to running cmake or make commands,
          but do not actually run them (make/cmake).  This still requires a
          git tree, but using "-git_update no" is okay.

      -run_cmake yes/no         : choose whether to run a cmake build

          default: -run_cmake no
          e.g.   : -run_cmake yes

          If requested, run a cmake build under the build_cmake directory.

      -run_make yes/no          : choose whether to run a make build

          default: -run_cmake yes
          e.g.   : -run_cmake no

          By default, a make build will be run.  Use this option to specify
          not to.

      -verb LEVEL               : set the verbosity level (default 1)

          e.g. -verb 2

          Specify how verbose the program should be, from 0=quiet to 4=max.
          As is typical, the default level is 1.

-----------------------------------------------------------------------------
R Reynolds    sometime 2023
=============================================================================
"""

g_history = """
   build_afni.py history:

   0.0  Feb  8, 2022 - getting started
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
      self.clean_root      = 1      # clean old root dirs?
      self.git_branch      = ''     # branch to check out (def master)
      self.git_tag         = ''     # tag to check out (def LAST_TAG)
      self.git_update      = 1      # do git clone or pull
      self.package         = ''     # to imply Makefile and build dir

      self.prep_only       = 0      # prepare (c)make, but do not run
      self.run_cmake       = 0      # actually run camke?
      self.run_make        = 1      # actually run make?
      self.make_target     = 'itall' # target in "make" command
      self.makefile        = ''     # an alternate Makefile to build from

      self.verb            = verb   # verbosity level

      # user controlled, but not directly with options

      # uncontrollable variables
      # for dirs git, build_src, build_cmake, atlases:
      #   name, exists, backup
      self.do_orig_abin    = None   # will be set automatically
      self.do_orig_aver    = ''     # try to set a current AFNI version
      self.do_orig_dir     = None   # will be set automatically

      self.do_abin         = None   # abin dirobj
      self.do_root         = None   # build root dirobj

      self.final_mesg      = []     # final messages to show to user
      self.history         = []     # shell/system command history
      self.hist_file       = 'cmd_history.txt' # final history file
      self.makefile_path   = ''     # abspath to -makefile

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
      self.valid_opts.add_opt('-clean_root', 1, [],
                      helpstr='clean up from old work? (def=y)')
      self.valid_opts.add_opt('-git_branch', 1, [],
                      helpstr='the git branch to checkout (def=%s)' \
                              % self.git_branch)
      self.valid_opts.add_opt('-git_tag', 1, [],
                      helpstr='the git tag to use (def=%s)'%self.git_tag)
      self.valid_opts.add_opt('-git_update', 1, [],
                      acplist=['yes','no'],
                      helpstr='should we update the local git repo')

      self.valid_opts.add_opt('-make_target', 1, [],
                      helpstr="specify target for make (def=itall)")
      self.valid_opts.add_opt('-makefile', 1, [],
                      helpstr="specify an alternate Makefile to build from")
      self.valid_opts.add_opt('-prep_only', 0, [],
                      helpstr="only prepare for (c)make, do not run it")
      self.valid_opts.add_opt('-run_cmake', 1, [],
                      acplist=['yes','no'],
                      helpstr="should we run a 'cmake' build?")
      self.valid_opts.add_opt('-run_make', 1, [],
                      acplist=['yes','no'],
                      helpstr="should we run a 'make' build? (def=y)")
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
            MESGe("-abin not yet implemented")

         elif opt.name == '-root_dir':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return -1
            self.do_root = dirobj('root_dir', val)

         # general options

         elif opt.name == '-clean_root':
            if OL.opt_is_yes(opt):
               self.clean_root = 1
            else:
               # do not clean, do not update git
               self.clean_root = 0
               self.git_update = 0

         elif opt.name == '-git_branch':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return -1
            self.git_branch = val

         elif opt.name == '-git_tag':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return -1
            self.git_tag = val

         elif opt.name == '-git_update':
            if OL.opt_is_yes(opt):
               self.git_update = 1
            else:
               self.git_update = 0

         elif opt.name == '-make_target':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return -1
            self.make_target = val

         elif opt.name == '-makefile':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return -1
            self.makefile = val

         elif opt.name == '-package':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return -1
            self.package = val

         elif opt.name == '-prep_only':
            self.prep_only = 1

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

         elif opt.name == '-verb':
            val, err = uopts.get_type_opt(int, '', opt=opt)
            if val == None or err: return -1
            else: self.verb = val

         else:
            MESGe("unhandled option '%s'" % opt.name)
            return -1

      # misc checks
      if self.git_update == 0 and (self.git_branch or self.git_tag):
         MESGe("cannot use -git_branch or -git_tag without -git_update yes")
         return -1

      # if we have a makefile, make sure it exists
      if self.makefile:
         if not os.path.isfile(self.makefile):
            MESGe("cannot find given -makefile '%s'" % self.makefile)
            return -1
         # note the full path before any 'cd'
         self.makefile_path = os.path.abspath(self.makefile)

      # assign any needed defaults - usually corresponding to 'misc checks'
      if self.git_branch == '':
         self.git_branch = 'master'

      if self.git_tag == '':
         self.git_tag = 'LAST_TAG'

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

      # save history, either way
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
      if self.run_cmake:
         plist.extend(['cmake', 'gcc', 'g++'])

      for prog in plist:
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

      if self.package == '' and self.makefile == '':
         MESGe("-package unspecified and not implied by current version")
         MESGi("(and no -makefile specified)")
         return 1

      if self.do_root.exists and self.clean_root:
         if self.clean_old_root():
            return 1

      if self.prepare_root():
         return 1

      # build source - the main purpose of this program
      if self.run_make:
         if self.f_build_via_make():
            return 1

      # run the build via cmake
      if self.run_cmake:
         if self.f_build_via_cmake():
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

   def f_build_via_cmake(self):
      """run a cmake build

         run cmake from shell script (to not affect current environment)
            - set CC and CCX to which gcc, g++
            - cmake ../git/afni |& tee log_1_cmake.txt
            - make VERBOSE=1 |& tee log_2_make.txt
      """

      MESGm("will run 'cmake' build")

      st, ot = self.run_cmd('cd', self.do_root.abspath, pc=1)
      if st: return st

      # if we already have a build dir, the user requested not to clean
      if os.path.isdir(self.dcbuild):
         MESGm("will reuse existing src director, %s" % self.dcbuild)
      else:
         st, ot = self.run_cmd('mkdir', self.dcbuild, pc=1)
         if st: return st

      # for convenience:
      buildpath = '%s/%s' % (self.do_root.dname, self.dcbuild)

      st, ot = self.run_cmd('cd', self.dcbuild, pc=1)
      if st: return st

      # create cmake script
      sfile = 'run_cmake.txt'
      logc = 'log_1_cmake.txt'
      logm = 'log_2_make.txt'
      stext = '# cmake script\n'                                        \
              '# (to set env vars without affecting current shell)\n'   \
              '\n'                                                      \
              'setenv CC `which gcc`\n'                                 \
              'setenv CCX `which g++`\n'                                \
              'cmake ../git/afni |& tee %s\n'                           \
              '\n'                                                      \
              'make VERBOSE=1 |& tee %s\n'                              \
              '\n'                                                      \
              % (logc, logm)

      if UTIL.write_text_to_file(sfile, stext):
         return 1

      # run the build (this is why we are here!)
      self.final_mesg.append("------------------------------")
      self.final_mesg.append("to rerun cake build:")
      self.final_mesg.append("   cd %s" % buildpath)
      self.final_mesg.append("   make VERBOSE=1")
      self.final_mesg.append("   (or run %s script)" % sfile)

      # if -prep_only, we are done
      if self.prep_only:
         MESGp("have -prep_only, skipping actual cmake and make")
         return 0

      MESGm("building (cmake) ...")
      MESGi("consider monitoring the build in a separate window with:")
      MESGi("    cd %s" % self.do_orig_dir.abspath)
      MESGi("    tail -f %s/%s" % (buildpath, logm))
      MESGi("    # use ctrl-c to terminate 'tail' command (not the build)")

      st, ot = self.run_cmd('tcsh', '-x %s' % sfile)
      if st: return st

      return 0

   def f_build_via_make(self):
      """run a make build
            - have or try to choose a suitable package
            - copy git/afni/src tree
            - find corresponding Makefile
              (or try to copy given one)
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
      if os.path.isfile('Makefile'):
         MESGm("have Makefile, ignoring -package %s" % self.package)
         if self.makefile:
            MESGi("also ignoring -makefile %s" % self.makefile)
      # else if one was specified, copy it here
      elif self.makefile:
         MESGm("copying %s to %s" % (self.makefile, "Makefile"))
         st, ot = self.run_cmd('cp', [self.makefile_path, 'Makefile'], pc=1)
         if st: return st
      # else use what is implied from package
      else:
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
      target = self.make_target

      # final messages
      self.final_mesg.append("------------------------------")
      self.final_mesg.append("to rerun make build:")
      self.final_mesg.append("   cd %s" % buildpath)
      self.final_mesg.append("   make %s" % target)
      # if there is some known abin, mention possibly rsync
      do = None
      if self.do_abin is not None:
         do = self.do_abin
      elif self.do_orig_abin is not None:
         do = self.do_orig_abin
      if do is not None:
         full_bpath = '%s/%s' % (self.do_root.abspath, self.dsbuild)
         self.final_mesg.append("------------------------------")
         self.final_mesg.append("to possibly rsync make output:")
         self.final_mesg.append("   rsync -av %s/%s/ %s/" \
             % (full_bpath, self.package, do.abspath))

      MESGm("building make target '%s'" % target)

      # if -prep_only, we are done
      if self.prep_only:
         MESGp("have -prep_only, skipping make and test")
         return 0

      logfile = 'log_make.txt'
      MESGp("building ...")
      MESGi("consider monitoring the build in a separate window with:")
      MESGi("    cd %s" % self.do_orig_dir.abspath)
      MESGi("    tail -f %s/%s" % (buildpath, logfile))
      MESGi("    # use ctrl-c to terminate 'tail' command (not the build)")
      st, ot = self.run_cmd('make %s >& %s' % (target, logfile))

      if st: tmesg = 'FAILED'
      else:  tmesg = 'SUCCEEDED'
      MESGm("building %s" % tmesg)
      MESGi("see log file %s/%s" % (buildpath, logfile))
      if st: return st

      # test the build
      logfile = 'log_test.txt'
      binopt = '-bin_dir %s' % self.package
      MESGp("testing the build results ...")

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
      MESGm("testing %s" % tmesg)
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

      # if the user does not want any udpates, we are done
      # (but note what is being used)
      if not self.git_update:
         MESGm("skipping any git updates")
         return self.report_branch_tag(gitd)

      # if git/afni exists, cd there and update
      if os.path.exists(gitd):
         st, ot = self.run_cmd('cd', gitd, pc=1)
         if st: return st
         st, ot = self.run_cmd('git fetch --all')
         if st: return st
         st, ot = self.run_cmd('git checkout %s' % self.git_branch)
         if st: return st
         st, ot = self.run_cmd('git pull origin %s' % self.git_branch)
         if st: return st
      # otherwise, initialize the git tree
      else:
         if not os.path.exists('git'):
            st, ot = self.run_cmd('mkdir', 'git', pc=1)
            if st: return st
         st, ot = self.run_cmd('cd', 'git', pc=1)
         if st: return st

         MESGm("running 'git clone' on afni repo ...")
         MESGi("(please be patient)")
         st, ot = self.run_cmd('git', 'clone %s' % g_git_html)
         if st: return st
         st, ot = self.run_cmd('git checkout %s' % self.git_branch)
         if st: return st

      # now possibly checkout a tag ('' means unset)
      if self.git_tag == 'LAST_TAG':
         tag = self.most_recent_tag()
      else:
         tag = self.git_tag

      if tag != '' and tag != 'NONE':
         MESGm("checking out git tag %s" % tag)
         st, ot = self.run_cmd('git checkout %s' % tag)
         if st: return st
      else:
         MESGm("not checking out any git tag")

      # report the branch and tag that we are working with
      self.report_branch_tag()

      return 0

   def report_branch_tag(self, cdpath=''):
      """report the current branch and tag
          if cdpath is set, change directory first
          return 0 on success
      """
      if cdpath != '':
         st, ot = self.run_cmd('cd', cdpath, pc=1)
         if st: return st

      st, obr = self.run_cmd('git branch --show-current')
      st, otag = self.run_cmd('git describe')
      if obr == '':
         obr = '(detached)'
      MESGm("using repo branch %s, tag %s" % (obr, otag))

      return 0

   def most_recent_tag(self):
      """just return the most recent AFNI_* tag (a clean one of length 12)
         return an empty string on failure
      """
      st, tag = self.run_cmd("git describe --tags --abbrev=0")
      if not tag.startswith('AFNI_') or len(tag) != 12:
         MESGe("failed to find recent AFNI build tag, have %s" % tag)
         return ''
      MESGm("have default git tag LAST_TAG, will checkout %s" % tag)

      return tag

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

   def run_cmd(self, cmd, params='', pc=0, strip=1):
      """main purpose is to store commands in history
            cmd     : a simple command or a full one
            params  : can be a simple string or a list of them
            pc      : denotes python command
                        - cmd is a single word ('cd', 'mkdir' ...)
                        - params are separate (we will see how it goes)
            strip   : strip any non-pc text (in rare case that we want text)
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
         if st:
            MESGe("failed run_cmd: %s" % cmd)
            MESGe(otext)
         if strip:
            otext = otext.strip()
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
         MESGe("failed run_cmd(p): %s %s" % (cmd, pstr))
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
         self.do_orig_abin = dirobj('orig_abin_dir', path_head(wp))
         self.get_orig_abin_info(self.do_orig_abin)
      else:
         if self.verb > 1:
            MESGm("no %s in original PATH to set orig_abin from" % prog)

      return 0

   def get_orig_abin_info(self, do_abin):
      """if possible, add to do_orig_abin: version, package, date
         read from AFNI_version.txt
      """
      # init to unknown
      do_abin.version = ''
      do_abin.package = ''
      do_abin.date = ''

      if self.verb > 1:
         MESGp("have original abin %s" % do_abin.abspath)

      vfile = '%s/AFNI_version.txt' % do_abin.abspath

      tdata = UTIL.read_text_file(vfile, lines=1, strip=1, verb=0)
      if len(tdata) < 3:
         if self.verb > 1:
            MESGw("failed to read version info from %s" % vfile)
         return 1

      do_abin.version = tdata[0]
      do_abin.package = tdata[1]
      do_abin.date    = tdata[2]
      if self.verb > 0:
         MESGm("current AFNI: %s, %s, %s" % (tdata[0], tdata[1], tdata[2]))

      # if package is empty, set from current version
      if self.package == '':
         MESGm("will init unset -package with current '%s'" % do_abin.package)
         self.package = do_abin.package

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

