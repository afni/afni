#!/usr/bin/env python

# python3 status: compatible

# system libraries
import sys, os, platform
import shutil
import glob

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

      - create main working tree from the specified -build_root
         - and 'cd' to it
      - prepare git directory tree
         - clone AFNI's git repository under new 'git' directory
         - possibly checkout a branch (master)
         - possibly checkout the most recent tag (AFNI_XX.X.XX)
      - prepare atlases
         - download and extract afni_atlases_dist.tgz package
         - if afni_atlases_dist exists, new atlases will not be pulled
           unless -update_atlases is given
      - prepare src build
         - copy git/afni/src to build_src
         - copy git/afni/doc/README/README.* to build_src
         - copy specified Makefile
         - run build
      - prepare cmake build (optional)
         - run build

   Final comments will be shown about:

      - how to rerun the make build
      - how to rerun the make build test
      - where a command (shell/system) history file is stored
        (hist_commands.txt is generally stored in the -build_root)
      - where the screen output history is stored

------------------------------------------
examples: ~1~

   0. basic, start fresh or with updates ~2~

      Either start from nothing from a clean and updated version.

        build_afni.py -build_root my/build/dir

      notes:
        - if there is an existing git tree, pull any updates
        - if there is an existing build_src tree, rename it and start clean

   1. simple, but continue where we left off ~2~

      Use this method to :
        - continue a previously terminated build
        - to rebuild after making updates to the build_src tree
        - to rebuild after installing additional system libraries

        build_afni.py -build_root my/build/dir -clean_root no

      notes:
        - if there is an existing git tree, use it (with no updates)
        - if there is an existing build_src directory, keep and use it

   2. basic, but specify an existing build package ~2~

      This implies a Makefile to use for the build.

        build_afni.py -build_root my/build/dir -package linux_centos_7_64

   3. use an alternate Makefile, but do not update git repo ~2~

        build_afni.py -build_root my/build/dir -git_update no \\
                      -makefile preferred_makefile

   4. do not check out any tag ~2~

      Check out and update to the most recent state of the 'current' branch,
      but do not check out any tag.  Also, specify a build package.

        build_afni.py -build_root my/build/dir          \\
                      -git_branch master -git_tag NONE  \\
                      -package linux_centos_7_64

   5. test the setup, but do not run any make (using -prep_only) ~2~

        build_afni.py -build_root my/build/dir -prep_only \\
                      -git_update no -makefile preferred_makefile 


------------------------------------------
todo:
  - opts to pass to cmake
  - given a Makefile, will want to detect output package name
  - pick a method for guessing an appropriate Makefile
    Ubuntu vs Fedora vs RedHat vs other vs macos (12+?)

later:
  - sync atlases and build
  - worry about sync to abin

------------------------------------------
terminal options: ~1~

      -help                     : show this help
      -hist                     : show module history
      -show_valid_opts          : list valid options
      -ver                      : show current version

required:

      -build_root BUILD_ROOT    : root directory to use for git and building

other options:

      -abin ABIN                : specify AFNI binary install directory

          default -abin <directory containing afni_proc.py>
          e.g.    -abin $HOME/my_new_abin

          When this option is given, any installation of the compiled binaries
          will be placed into this ABIN directory.  If this option is not
          given, it will be determined by `which afni_proc.py`.

          If this directory does not exist, it will be created upon install.

      -backup_method BACK_METH  : specify how to perform the backup

          default -backup_method rsync
          e.g.    -backup_method mv

          This option is used to specify how a backup of ABIN is made.  It
          should be one of:

                mv      : apply the Unix 'mv' command

                        + Benefit: ABIN is really cleaned, and will not contain
                          any removed files.  This method should be faster.

                rsync   : apply the Unix 'rsync' command
                          (this is the default method)

                        + Benefit: ABIN is preserved during the backup process.
                          Even if the program is terminated while making the
                          backup, ABIN will be maintained.

                        + Benefit: old ABIN files are removed.
                          So old files do not accumulate.
                          If some file or program is no longer built and
                          distributed, it will not linger in the ABIN.
                
                          After the backup stage, ABIN is emptied before
                          repopulating it with a new install.

                rsync_preserve : use 'rsync', but do not remove old files

                        + Benefit: ABIN is preserved.

                        + Benefit: old ABIN files are never removed.
                          So old files accumulate over time.
                          If some file or program is no longer built and
                          distributed, it will linger in the ABIN.

          See also -do_backup.

      -clean_root yes/no        : specify whether to clean up the build_root

          default -clean_root yes
          e.g.    -clean_root no

          If 'no' is specified, the git directory will not be updated and the
          build_src directory will not be remade.

      -do_backup yes/no         : specify whether to back up abin before install

          default -do_backup yes
          e.g.    -do_backup no

          By default backup will be made whenever a full installation is done
          (of both AFNI binaries and atlases).  The backup (of ABIN, specified
          by -abin) will be placed under the BUILD_ROOT directory (specified
          by -build_root).

          The backup is made by moving the full contents of the abin, so that
          AFNI updates that remove files or programs will indeed remove them.

          If a full install will not be done, a backup will not be made.

          One may use -backup_method to control the command used to make the
          backup.

          See also -backup_method.

      -do_install yes/no       : specify whether to install compiled binaries

          default -do_install yes
          e.g.    -do_install no

          By default, compiled AFNI binaries and atlases will be installed
          into the ABIN directory given by -abin (or else from the $PATH).
          
          If 'no' is specified, no installation will take place (and no backup
          will be made).

          See also -abin, -do_backup.

      -git_branch BRANCH        : specify a branch to checkout in git

          default -git_branch master
          e.g.    -git_branch some_stupid_branch

          This will checkout and pull the branch.  To build of the most recent
          version of a branch (and not the most recent tag), include:

            -git_tag NONE

          Unless using '-git_update no', the current branch will be updated
          (default master), to make sure any relevant tag will exist.

          Note that precise tags generally refer to a specific branch.  So it
          is easy to specify a branch and a tag that is not actually associated
          with that branch.

          See also -git_tag.

      -git_tag TAG              : specify a tag to checkout in git

          default -git_tag LAST_TAG
          e.g.    -git_tag NONE

          This will lead to 'git checkout TAG', of some sort, depending:

             LAST_TAG   : checkout most recent (annotated) AFNI_XX.X.XX tag.
                          (annotated tags come from official AFNI builds)
             NONE       : do not checkout any specific tag
                          (use this to build from the current branch state)

          By default, the most recent tag is checked out (for the purpose of
          aligning the build with AFNI releases).  To build off of the most
          recent state of a branch, use "-git_tag NONE".

          The LAST_TAG option will generally imply the most recent "official"
          AFNI tag based on the master branch.

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

      -update_atlases yes/no    : update atlases, even if the package exists

          default: -update_atlases yes
          e.g.   : -update_atlases no

          By default, even if the atlases directory exists (afni_atlases_dist),
          it will be updated.  Use this option to avoid a new download.

          If -clean_root is 'no', atlases will not be updated.

      -update_niivue yes/no    : update NiiVue, even if the package exists

          default: -update_niivue yes
          e.g.   : -update_niivue no

          By default, even if NiiVue exists, it will be updated.  Use this
          option to avoid a new download.

          If -clean_root is 'no', NiiVue will not be updated.

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

   0.0  Feb  8, 2023 - getting started
   0.1  Feb 18, 2023 - initial release, but have features to add
   0.2  Mar  2, 2023 - rename -root_dir to -build_root (as ordered by PT)
   0.3  Jun 22, 2023 - include AFNI_WHOMADEIT in make
   0.4  Aug 28, 2023 - test -help using renamed test_afni_prog_help.tcsh
   0.5  Sep  8, 2023 - back up and install the build results
   0.6  Sep 26, 2023 - install NiiVue, add option -update_atlases
   0.7  Oct 10, 2023
        - require -build_root
        - show 'tail log_make.txt' on build failure
        - default to updating atlases
   0.8  Oct 12, 2023 - advise patience
   0.9  Nov 20, 2023
        - add 'rsync_preserve' backup_method
        - 'rsync' method now cleans up the old abin contents
   0.10 Dec  8, 2023
        - copy git/afni/doc/README/README.* into build_src
        - make prev a directory, not a name prefix
   0.11 Jun 11, 2024
        - remove extra backup dirs (save 1, and must contain afni)
        - add -update_niivue option
   0.12 Jun 24, 2024 - for make, warn if CC is set
"""

g_prog = "build_afni.py"
g_version = "%s, version 0.12, June 24, 2024" % g_prog

g_git_html    = "https://github.com/afni/afni.git"
g_afni_site   = "https://afni.nimh.nih.gov"
g_atlas_pack  = "afni_atlases_dist"      # package name
g_atlas_html  = "%s/pub/dist/atlases/%s.tgz" % (g_afni_site, g_atlas_pack)
g_niivue_file = "niivue_afni.umd.js"    # file name
g_niivue_html = "%s/pub/dist/bin/misc/%s" % (g_afni_site, g_niivue_file)

g_mesg_log   = []   # message history/log (if None, do not log)


# ---------------------------------------------------------------------------
# general functions

# message functions leaving room for control
def MESG(mstr, disp=1):
  """(possibly) display and log the message
  """
  if disp: print(mstr)
  if g_mesg_log is not None:
     g_mesg_log.append(mstr)

def MESGe(mstr, disp=1):
  """(possibly) display and log the message
     - display as an error
  """
  MESG("** error: %s" % mstr)

def MESGw(mstr, disp=1):
  """(possibly) display and log the message
     - display as a warning
  """
  MESG("** warning: %s" % mstr)

def MESGm(mstr, disp=1):
  """(possibly) display and log the message
     - display with a 'minus' prefix (--)
  """
  MESG("-- %s" % mstr)

def MESGp(mstr, disp=1):
  """(possibly) display and log the message
     - display with a 'plus' prefix (++)
  """
  MESG("++ %s" % mstr)

def MESGi(mstr, disp=1):
  """(possibly) display and log the message
     - display with a 'space' indentation prefix (  )
  """
  MESG("   %s" % mstr)

def MESG_write_log(fname):
   """write the stored message log to the given text file
   """
   if g_mesg_log is not None:
      return UTIL.write_text_to_file(fname, '\n'.join(g_mesg_log) + '\n')

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
      self.make_target     = 'itall' # target in "make" command
      self.makefile        = ''     # an alternate Makefile to build from
      self.package         = ''     # to imply Makefile and build dir

      self.prep_only       = 0      # prepare (c)make, but do not run
      self.run_cmake       = 0      # actually run camke?
      self.run_make        = 1      # actually run make?
      self.run_backup      = 1      # install build results and atlases
      self.run_install     = 1      # install build results and atlases
      self.update_atlases  = 1      # do we force an atlas update
      self.update_niivue   = 1      # do we force a NiiVue update

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
      self.do_mb_abin      = None   # make build install dir object

      self.sync_src_atlas  = ''     # directory to sync atlases from
      self.sync_src_make   = ''     # directory to sync make build from
      self.sync_src_niivue = ''     # file to sync niivue from
      self.backup_abin     = ''     # directory of any abin backup
      self.backup_prefix   = 'backup.abin.' # prefix for any abin backup

      self.backup_method   = 'rsync' # method for backup: mv or rsync
      self.cmd_history     = []     # shell/system command history
      self.final_mesg      = []     # final messages to show to user
      self.hist_file       = 'hist_commands.txt' # final history file
      self.mesg_file       = 'hist_messages.txt' # message history file
      self.rsync_file      = 'hist_rsync.txt' # rsync history file
      self.makefile_path   = ''     # abspath to -makefile

      self.poldp           = 'prev'         # prefix for old version
      self.pold            = self.poldp+'/' # include dot or directory slash
      self.dcbuild         = 'build_cmake'
      self.dsbuild         = 'build_src'
      self.dsdoc           = 'doc'

      # system and possible mac stuff
      self.ekeys           = os.environ.keys()  # store env keys
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
      self.valid_opts.add_opt('-build_root', 1, [],
                      helpstr='(required) the root of the building tree')

      self.valid_opts.add_opt('-package', 1, [],
                      helpstr='the binary package to build')

      # general options
      self.valid_opts.add_opt('-backup_method', 1, [],
                      acplist=['mv','rsync','rsync_preseve'],
                      helpstr='specify method of backup (def=rsync)')
      self.valid_opts.add_opt('-clean_root', 1, [],
                      helpstr='clean up from old work? (def=y)')
      self.valid_opts.add_opt('-do_backup', 1, [],
                      acplist=['yes','no'],
                      helpstr='back up abin before installing into it (def=y)')
      self.valid_opts.add_opt('-do_install', 1, [],
                      acplist=['yes','no'],
                      helpstr='install build results into abin (def=y)')
      self.valid_opts.add_opt('-git_branch', 1, [],
                      helpstr='the git branch to checkout (def=master)')
      self.valid_opts.add_opt('-git_tag', 1, [],
                      helpstr='the git tag to use (def=LAST_TAG)')
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
      self.valid_opts.add_opt('-update_atlases', 1, [],
                      acplist=['yes','no'],
                      helpstr="should we re-download atlases? (def=y)")
      self.valid_opts.add_opt('-update_niivue', 1, [],
                      acplist=['yes','no'],
                      helpstr="should we re-download NiiVue? (def=y)")
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

         elif opt.name == '-build_root':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return -1
            self.do_root = dirobj('build_root', val)

         # general options

         elif opt.name == '-backup_method':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return -1
            self.backup_method = val

         elif opt.name == '-clean_root':
            if OL.opt_is_yes(opt):
               self.clean_root = 1
            else:
               # do not clean, do not update git
               self.clean_root = 0
               self.git_update = 0
               self.update_atlases = 0
               self.update_niivue = 0

         elif opt.name == '-do_backup':
            if OL.opt_is_no(opt):
               self.run_backup = 0
            else:
               self.run_backup = 1

         elif opt.name == '-do_install':
            if OL.opt_is_no(opt):
               self.run_install = 0
            else:
               self.run_install = 1

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

         elif opt.name == '-update_atlases':
            if OL.opt_is_yes(opt):
               self.update_atlases = 1
            else:
               self.update_atlases = 0

         elif opt.name == '-update_niivue':
            if OL.opt_is_yes(opt):
               self.update_niivue = 1
            else:
               self.update_niivue = 0

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

      # require -build_root if there are no terminal options
      if self.do_root is None:
         MESGe("refusing to proceed without -build_root")
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
      # -----------------------------------------------------------------
      # note where we are starting from, and any current abin
      rv = self.set_orig_dirs()
      if rv: return rv

      rv = self.check_progs()
      if rv: return rv

      if self.verb > 2:
          self.show_vars("ready to process...")

      # -----------------------------------------------------------------
      # actually do the build work
      if self.do_root is not None:
         rv = self.run_main_build()

         # if we succeeded, rsync the results
         if self.run_install and not self.prep_only and not rv:
            rv = self.f_run_install()

      # -----------------------------------------------------------------
      # histories and logs
      # (first history, then final messages, then MESG_write_log)

      # save command history, either way
      self.show_cmd_history(disp=self.verb>2, save=1, sdir=self.do_root.abspath)

      # if logging, state where
      # (to store in log and to have on screen before final messages)
      mesg_hist_file = '%s/%s' % (self.do_root.abspath, self.mesg_file)
      if self.verb and g_mesg_log is not None:
         MESG("")
         MESGp("screen text history is in:")
         MESGi("   %s" % mesg_hist_file)
         MESG("")

      # show final messages (history and logging should come after)
      if self.verb:
         self.show_final_messages()

      # also save the message log text (screen text)
      self.make_backup_file(mesg_hist_file)
      MESG_write_log(mesg_hist_file)

      return rv

   def add_final_mesg(self, mesg, ind=''):
      """append the (indented) message to self.final_mesg
         (for later display and possible logging)
      """
      self.final_mesg.append(ind+mesg)

   def show_final_messages(self):
      """display contents of self.final_mesg"""
      if len(self.final_mesg) == 0:
         return 0

      MESG("")
      MESG('='*30 + " overview " + '='*30)
      MESG("")
      for mesg in self.final_mesg:
         MESG(mesg)
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

   def show_cmd_history(self, disp=1, save=0, sdir=''):
      """display or save the cmd_history, possibly in a given directory
         (if sdir, cd;write;cd-)  -- does this go in the history??  no???
      """
      if len(self.cmd_history) == 0:
         return

      # generate a single message string
      mesg = "shell/system command history:"
      hnew = [mesg]
      for ind, cmd in enumerate(self.cmd_history):
         hnew.append('cmd %2d :  %s' % (ind, cmd))
      hstr = '\n   '.join(hnew) + '\n\n'
      del(hnew)

      if disp:
         UTIL.write_text_to_file('-', hstr)

      if save:
         self.write_history_file(hstr, sdir)

   def make_backup_file(self, fname, verb=0):
      """back up a text file, using self.backup_prefix
         return 0 on success, 1 on error
      """

      if not os.path.exists(fname):
         return 0

      parts = os.path.split(fname)
      if len(parts) != 2:
         MESGe("cannot split %s to make backup for" % fname)
         return 1

      # new file has the prefix inserted
      newf = os.path.join(parts[0], self.pold + parts[1])
      rv, ot = self.run_cmd('mv', [fname, newf], pc=1)

      if verb:
         MESGm("moving old %s to %s" % (fname, newf))

      return 0

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

          # possibly make a backup
          self.make_backup_file(hfile)

          # and write the actual text
          UTIL.write_text_to_file(hfile, hstr)

          MESG("")
          MESGm("shell/system command history is in:")
          MESGi("   %s/%s" % (sdir, hfile))

          rv, ot = self.run_cmd('cd', cwd, pc=1)
          if rv: return rv
      else:
          MESGw("no build_root dir to write history to: %s" % sdir)

   def f_run_install(self):
      """install the make results and atlases
         - if no install, might still suggest rsync
         - mv current abin contents to backup

         return 0 on success, else error
      """

      # tell user what we are plotting
      if self.verb:
         MESG("")
         if self.run_install:
            MESGm("will install 'make' build results and atlases")
         else:
            MESGm("skipping install")

      # go to build root (to possibly create backup directory)
      st, ot = self.run_cmd('cd', self.do_root.abspath, pc=1)
      if st: return st

      # note where we would back up from and to
      abin = ''
      do = self.f_get_rsync_abin_do()
      if do is not None:
         abin = do.abspath
      self.backup_abin = self.f_make_backup_abin_name()
      if os.path.exists(self.backup_abin):
         MESGe("failed to make backup.abin name, please delete some")
         self.backup_abin = ''

      # optimally, we now know: abin, backup_abin, atlas_src, make_src
      # chat
      if self.verb > 2:
         MESGi("atlas dir   : %s" % self.sync_src_atlas)
         MESGi("build dir   : %s" % self.sync_src_make)
         MESGi("niivue file : %s" % self.sync_src_niivue)
         MESGi("install dir : %s" % self.backup_abin)
         MESGi("run install : %s" % self.run_install)
         MESG("")

      # if no main abin, no directory to install to
      if abin == '':
         MESGm("no known abin, so no install to ponder")
         return 0

      # ------------------------------------------------------------
      # if no backup directory or no install, just recommend rsync
      if self.backup_abin == '' or not self.run_install:
         # if there is no abin dest, we are done
         if abin == '':
            MESGm("no rsync abin destination to ponder")
            return 0

         have_sync = 0

         # possibly suggest make sync
         if self.sync_src_make:
            have_sync = 1
            self.add_final_mesg("------------------------------")
            self.add_final_mesg("to possibly rsync make output:")
            self.add_final_mesg("   rsync -av %s/ %s/" \
               % (self.sync_src_make, abin))

         # possibly suggest atlas sync
         if self.sync_src_atlas:
            have_sync = 1
            self.add_final_mesg("------------------------------")
            self.add_final_mesg("to possibly rsync atlases:")
            self.add_final_mesg("   rsync -av %s/ %s/" \
               % (self.sync_src_atlas, abin))

         # possibly suggest atlas sync
         if self.sync_src_niivue:
            have_sync = 1
            self.add_final_mesg("------------------------------")
            self.add_final_mesg("to possibly rsync NiiVue:")
            self.add_final_mesg("   rsync -av %s %s/" \
               % (self.sync_src_niivue, abin))

         if not have_sync:
            MESGm("no make build, atlases or NiiVue to sync")

         return 0

      # ------------------------------------------------------------
      # actually do the install
      # ------------------------------------------------------------

      MESG("")
      MESGi("    PLEASE WAIT FOR THE INSTALL TO COMPLETE...")
      MESG("")

      # if abin does not exist, create it (and cancel any backup)
      if not os.path.exists(abin):
         MESGp("creating install abin: %s" % abin)
         st, ot = self.run_cmd('mkdir', abin, pc=1)
         if st: return st
         self.run_backup = 0

      # ------------------------------------------------------------
      # possibly run backup

      st = self.run_backup_abin(abin)
      if st: return st

      # ------------------------------------------------------------
      # now actually sync to the destination

      if self.sync_src_atlas or self.sync_src_make:
         # possibly make a backup
         self.make_backup_file(self.rsync_file, verb=self.verb)
         st, ot = self.run_cmd('echo "" > %s' % self.rsync_file)
         if st: return st
         
      if self.sync_src_atlas:
         MESGp("installing atlases under %s" % abin)
         self.add_final_mesg("------------------------------")
         self.add_final_mesg("atlases installed to   %s" % abin)
         self.add_final_mesg("        installed from %s" % self.sync_src_atlas)
         st, ot = self.run_cmd('rsync -av %s/ %s/ >> %s' \
                      % (self.sync_src_atlas, abin, self.rsync_file)) 
         if st: return st

      if self.sync_src_make:
         MESGp("installing build results under %s" % abin)
         self.add_final_mesg("------------------------------")
         self.add_final_mesg("binaries installed to   %s" % abin)
         self.add_final_mesg("         installed from %s" % self.sync_src_make)
         st, ot = self.run_cmd('rsync -av %s/ %s/ >> %s' \
                      % (self.sync_src_make, abin, self.rsync_file)) 
         if st: return st

      if self.sync_src_niivue:
         MESGp("installing NiiVue under %s" % abin)
         self.add_final_mesg("------------------------------")
         self.add_final_mesg("NiiVue installed to %s" % abin)
         st, ot = self.run_cmd('rsync -av %s %s/ >> %s' \
                      % (self.sync_src_niivue, abin, self.rsync_file)) 
         if st: return st

      # inform user how many backup directories exist, and possibly clean up
      st = self.run_clean_backup()
      if st: return st

      return 0

   def run_clean_backup(self):
      """remove some of the backup.abin directories:

         save 1 or 2 (if different):
           - save most recent that contains afni
           - save most recent
      """

      glist = glob.glob('%s*' % self.backup_prefix)
      glist.sort()
      nback = len(glist)
      self.add_final_mesg("------------------------------")
      self.add_final_mesg("have %d backup abin directories, %s*" \
            % (nback, self.backup_prefix))

      # maybe there is nothing to do
      if nback <= 1:
         if self.verb > 1: MESGm("no extra backups to remove")
         self.add_final_mesg("have %d backup abin directories, %s*" \
               % (nback, self.backup_prefix))
         return 0

      # definitely keep the current backup
      keepers = []
      if self.backup_abin in glist: # should not be necessary, but...
         keep = self.backup_abin
      else:
         keep = glist[-1]
      glist.remove(keep)
      keepers.append(keep)
      nback = len(glist)

      # if this does not contain 'afni', try to find the most recent that does
      if not os.path.isfile('%s/afni' % keep):
         for bind in range(nback-1, -1, -1):
            if os.path.isfile('%s/afni' % glist[bind]):
               # append to keepers, the remove from current list
               keepers.append(glist[bind])
               glist.remove(glist[bind])
               nback -= 1
               break

      # report result
      if self.verb:
         MESGm("backup directories: keeping %d, removing %d" \
               % (len(keepers), nback))
         if self.verb > 1:
            print("keep   : %s" % '\n       : '.join(keepers))
            print("remove : %s" % '\n       : '.join(glist))

      # do the damage
      if nback > 0:
         MESGm("removing %d backup dirs..." % len(glist))
         for rm in glist:
            st, ot = self.run_cmd('rmtree', rm, pc=1)
            if st: return st

      self.add_final_mesg("have %d final backup abin directory(ies), %s*" \
                          % (len(keepers), self.backup_prefix))

      del(glist)

   def run_backup_abin(self, abin):
      """decide whether to backup the install abin directory, and do it
         return 0 on success, else error

         other options to consider for backups:
            goal: make sure the abin contents are never lost
            goal: be fast
            goal: save disk space: do not have too many copies of results
         
            If abin is not a link, we could 'mv' the actual abin directory to
            the backup and recreate it with the new contents.
            That might be the fastest way to go, plus it should preserve abin
            without concerns about the file system handling rsync before rm.
            Perhaps the only question would be whether the user owns the
            *parent* directory.  Also, if 'mv' goes across file systems,
            maybe it becomes the same sync question again.
         
            If we finally went ahead and populated a local install directory,
            then we could use rsync --delete for cleaning, or even use
            --delete-after.
      """

      # if not syncing, we should not backup
      if not self.sync_src_atlas or not self.sync_src_make:
         if self.run_backup:
            MESGp("skipping backup since no full sync to abin")
            self.run_backup = 0

      if not self.run_backup:
         MESGm("skipping abin backup")
         return 0

      if not os.path.isdir(abin):
         MESGe("** run_backup_abin: no '%s' directory???" % abin)
         return 1

      # ------------------------------------------------------------
      # ready to make a backup

      MESGp("backing up %s to %s" % (abin, self.backup_abin))
      MESGi("(backup via %s)" % self.backup_method)

      # create backup directory
      st, ot = self.run_cmd('mkdir', self.backup_abin, pc=1)
      if st: return st

      # and actually do the backup
      # (use backup method: mv or rsync)
      if self.backup_method == 'mv':
         cmd = 'mv %s/* %s/' % (abin, self.backup_abin)
         st, ot = self.run_cmd(cmd)
         if st: return st
      else:
         # use one of the rsync methods

         # first make the actual backup
         cmd = 'rsync -av %s/ %s/' % (abin, self.backup_abin)
         st, ot = self.run_cmd(cmd)
         if st: return st

         # if we are not preserving the old files, clean out the abin
         # ** on a large file system, are we sure the rsync is finished?
         if self.backup_method != 'rsync_preserve':
            MESGm("cleaning old abin")
            # after sync, delete abin before repopulating
            cmd = 'if ( `ls -a %s/ | wc -l` > 2 ) rm -fr %s/*' % (abin,abin)
            st, ot = self.run_cmd(cmd)
            if st: return st

      self.add_final_mesg("------------------------------")
      self.add_final_mesg("to revert from backup, run:")
      self.add_final_mesg("   rsync -av %s/ %s/" % (self.backup_abin, abin))


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
      """under build_root, make sure we have current:
            git
            atlases

         return 0 on success, else error
      """

      if self.verb:
         MESGm("preparing build_root dir, %s" % self.do_root.dname)

      # if there is no root dir yet, make it
      if not os.path.isdir(self.do_root.abspath):
         if self.verb:
            MESGm("creating build_root dir, %s" % self.do_root.dname)
         st, ot = self.run_cmd('mkdir', self.do_root.abspath, pc=1)
         if st: return st

      # if git exists, do a git pull, else do a clone
      if self.f_update_git():
         return 1

      # get atlases
      if self.f_get_extras():
         return 1

      return 0

   def f_build_via_cmake(self):
      """run a cmake build

         run cmake from shell script (to not affect current environment)
            - set CC and CCX to which gcc, g++
            - cmake ../git/afni |& tee log_1_cmake.txt
            - make VERBOSE=1 |& tee log_2_make.txt
      """

      if self.verb: MESG("")
      MESGm("preparing to run 'cmake' build")

      st, ot = self.run_cmd('cd', self.do_root.abspath, pc=1)
      if st: return st

      # if we already have a build dir, the user requested not to clean
      if os.path.isdir(self.dcbuild):
         MESGm("will reuse existing src directory, %s" % self.dcbuild)
      else:
         st, ot = self.run_cmd('mkdir', self.dcbuild, pc=1)
         if st: return st

      st, ot = self.run_cmd('cd', self.dcbuild, pc=1)
      if st: return st

      # -----------------------------------------------------------------
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

      # -----------------------------------------------------------------
      # some instructive messages

      # for convenience
      buildpath = '%s/%s' % (self.do_root.dname, self.dcbuild)

      self.add_final_mesg("------------------------------")
      self.add_final_mesg("to rerun cake build:")
      self.add_final_mesg("   cd %s" % buildpath)
      self.add_final_mesg("   make VERBOSE=1")
      self.add_final_mesg("   (or run %s script)" % sfile)

      MESGm("building via script %s" % sfile)

      # if -prep_only, we are done
      if self.prep_only:
         MESG("")
         MESGp("have -prep_only, skipping actual cmake and make")
         return 0

      MESG("")
      MESGm("consider monitoring the build in a separate window with:")
      MESGi("    cd %s" % self.do_orig_dir.abspath)
      MESGi("    tail -f %s/%s" % (buildpath, logm))
      MESGi("    # use ctrl-c to terminate 'tail' command (not the build)")
      MESGm("building (cmake) (please be patient)...")

      # -----------------------------------------------------------------
      # actually run the build (this is why we are here!)
      st, ot = self.run_cmd('tcsh', '-xe %s' % sfile)
      if st: tmesg = 'FAILURE'
      else:  tmesg = 'SUCCESS'
      MESGm("status: cmake build %s" % tmesg)
      if st: return st

      return 0

   def f_build_via_make(self):
      """run a make build
            - fail or warn if CC is already set
            - have or try to choose a suitable package
            - copy git/afni/src tree
            - find corresponding Makefile
              (or try to copy given one)
            - run build
      """

      if self.verb: MESG("")
      MESGm("preparing to run 'make' build of package %s" % self.package)

      st, ot = self.run_cmd('cd', self.do_root.abspath, pc=1)
      if st: return st

      # if we already have a doc dir, the user requested not to clean
      #
      # This directory is not currently used, so let's not create it.
      # Uncomment this section to retrieve.
      #
      # if os.path.isdir(self.dsdoc):
      #    MESGm("will reuse existing doc directory, %s" % self.dsdoc)
      # else:
      #    st, ot = self.run_cmd('cp -rp', ['git/afni/doc', self.dsdoc])
      #    if st: return st

      # if we already have a build dir, the user requested not to clean
      if os.path.isdir(self.dsbuild):
         MESGm("will reuse existing src directory, %s" % self.dsbuild)
      else:
         st, ot = self.run_cmd('cp -rp', ['git/afni/src', self.dsbuild])
         if st: return st
         # and get the README files from the same git tree
         readpre = 'git/afni/doc/README/README'
         if os.path.isfile(readpre+'.environment'):
            MESGm("copying README files")
            st, ot = self.run_cmd('cp -p', [readpre+'.*', self.dsbuild])
            if st: return st

      st, ot = self.run_cmd('cd', self.dsbuild, pc=1)
      if st: return st

      # -----------------------------------------------------------------
      # check the environment

      # we do not want CC set, warn for now
      if 'CC' in self.ekeys:
         MESGw("running make, but CC == %s" % os.environ['CC'])
         MESGi("(CC should likely not be set)")

      # -----------------------------------------------------------------
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

      # -----------------------------------------------------------------
      # lots of messages before running the build

      # for convenience:
      buildpath = '%s/%s' % (self.do_root.dname, self.dsbuild)

      MESGm("building make target '%s'" % self.make_target)

      # if -prep_only, we are done
      if self.prep_only:
         MESG("")
         MESGp("have -prep_only, skipping make and test")
         return 0

      logfile = 'log_make.txt'
      MESG("")
      MESGm("consider monitoring the build in a separate window with:")
      MESGi("    cd %s" % self.do_orig_dir.abspath)
      MESGi("    tail -f %s/%s" % (buildpath, logfile))
      MESGi("    # use ctrl-c to terminate 'tail' command (not the build)")
      MESGp("building (please be patient)...")

      # -----------------------------------------------------------------
      # actually run the main build
      # expected AFNI_WHOMADEIT values:
      #    local    : default
      #    build    : from this (build_afni.py)
      #    official : official distributed binaires
      who = 'AFNI_WHOMADEIT=build'
      st, ot = self.run_cmd('make %s %s >& %s' % (who,self.make_target,logfile))

      if st: tmesg = 'FAILURE'
      else:  tmesg = 'SUCCESS'
      MESGm("status: building %s" % tmesg)
      MESGi("see 'make' log file %s/%s" % (buildpath, logfile))
      # return on failure
      if st:
         # on failure, show tail of make log
         if os.path.isfile(logfile):
            makelines = UTIL.read_text_file(logfile, lines=1)
            ss = '   --------------------------------------------------\n'
            MESGe("tail from %s:\n%s   %s\n%s" \
                  % (logfile, ss, "\n   ".join(makelines[-10:]), ss))

            # note whether we are in a conda environment
            self.check_conda_evars()

         return st

      # -----------------------------------------------------------------
      # finished with build, try to capture version info
      # (with local version file)
      do = dirobj('mb_abin', '.')

      if self.set_afni_version_info(do):
         MESGe("** failed to set ANFI version info")
         return 1

      if do.package and os.path.isdir(do.package):
         do = dirobj('mb_abin', do.package)
         self.set_afni_version_info(do)
         self.do_mb_abin = do

      if self.verb > 1:
         MESGp("have make build abin %s" % do.abspath)
      MESGm("make build AFNI: %s, %s, %s" % (do.version, do.package, do.date))

      # if possible rsync, note source and dest dirs
      do = self.f_get_rsync_abin_do()
      if do is not None and self.do_mb_abin is not None:
         self.sync_src_make = self.do_mb_abin.abspath

      # -----------------------------------------------------------------
      # final messages: how to rerun make
      self.add_final_mesg("------------------------------")
      self.add_final_mesg("to rerun make build:")
      self.add_final_mesg("   cd %s" % buildpath)
      self.add_final_mesg("   make %s" % self.make_target)

      # prepare to test the build, and final messages on testing
      logfile = 'log_test.txt'
      binopt = '-bin_dir %s' % self.package
      MESGp("testing the build results ...")

      # test -help using test_afni from src dir (or from PATH)
      test_prog = "test_afni_prog_help.tcsh"
      if os.path.isfile("scripts_install/%s" % test_prog):
         test_cmd = "tcsh scripts_install/%s" % test_prog
      else: 
         test_cmd = test_prog
         
      cmd = "%s %s" % (test_cmd, binopt)
      self.add_final_mesg("------------------------------")
      self.add_final_mesg("to rerun test of make build:")
      self.add_final_mesg("   cd %s" % buildpath)
      self.add_final_mesg("   %s" % cmd)

      # -----------------------------------------------------------------
      # append redirect to cmd after saving sample command for user
      cmd += " >& %s" % logfile
      st, ot = self.run_cmd(cmd)

      if st: tmesg = 'FAILURE'
      else:  tmesg = 'SUCCESS'
      MESGm("status: build testing %s" % tmesg)
      MESGi("see 'testing' log file %s/%s" % (buildpath, logfile))
      if st: return st

      return 0

   def check_conda_evars(self):
      """note whether we are in a conda environment
         print: CONDA_SHLVL and CONDA_DEFAULT_ENV, if set

         return whether we are in conda (SHLVL set)
      """
      elvl = 'CONDA_SHLVL'
      eenv = 'CONDA_DEFAULT_ENV'
      # if no shell level, we are done
      if elvl not in self.ekeys:
         return 0

      # init main vars and check DEF_ENV
      vlvl = os.environ[elvl]
      venv = ''

      # make a string for DEFAULT_ENV
      if eenv in self.ekeys:
         venv = ', %s = %s' % (eenv, os.environ[eenv])

      MESGw('in conda environment')
      MESGi('%s = %s%s' % (elvl, vlvl, venv))

      return 1

   def f_get_rsync_abin_do(self):
      """return the directory object for install abin
      """
      do = None
      if self.do_abin is not None:
         do = self.do_abin
      elif self.do_orig_abin is not None:
         do = self.do_orig_abin
      return do

   def f_fill_datestring(self, form='%Y_%m_%d_%H_%M_%S'):
      """return date string for the given form, else NODATE"""

      dstr = ''
      # first using datetime
      try:
         from datetime import datetime
         dstr = datetime.now().strftime(form)
         if self.verb > 2:
            MESGm("using datetime date string %s" % dstr)
      except:
         pass

      # if failure, try same from shell date
      if dstr == '':
         st, dt = UTIL.exec_tcsh_command('date +%s' % form)
         # if success, store
         if not st:
            dstr = dt
            if self.verb > 2:
               MESGm("using shell date string %s" % dstr)

      # if failure, use NODATE
      if dstr == '':
         dstr = 'NODATE'

      return dstr

   def f_make_backup_abin_name(self):
      """make up a name for backing up abin

         backup.abin.YYYY_MM_DD_hh_mm_ss
         (self.backup.prefix.)YYYY_MM_DD_hh_mm_ss
      """

      # try to make a date signature
      dstr = self.f_fill_datestring()

      # now set prefix, and if needed, find an incremenal suffix
      bname = '%s%s' % (self.backup_prefix, dstr)

      # see if bname is sufficient (should usually be)
      # if it exists, try adding a suffix for a while before failure
      if os.path.exists(bname):
         for ind in range(1,10):
            btmp = '%s.%02d' % (bname, ind)
            # if not here, we have a good candidate
            if not os.path.exists(btmp):
               bname = btmp
               break
            # else it exists, keep searching

      return bname

   def f_get_extras(self):
      """get atlases and niivue

         return 0 on success
      """
      # be sure to start from the root dir
      st, ot = self.run_cmd('cd', self.do_root.abspath, pc=1)
      if st: return st

      st = self.f_get_atlases()
      if st: return st

      st = self.f_get_niivue()
      if st: return st

   def f_get_atlases(self):
      """if no afni_atlases_dist dir, download

         - we start in the correct directory
         - download g_atlas_pack (package) from g_atlas_html

         return 0 on success
      """

      # note the atlas package
      # (use a local variable in case it later comes from elsewhere)
      atlas_pack = g_atlas_pack
      renamed = '%s%s' % (self.pold, atlas_pack)

      # and note atlas path for possible install or rsync suggestion
      self.sync_src_atlas = '%s/%s' % (self.do_root.abspath, atlas_pack)

      # if atlases already exist, use them (or re-download)
      if os.path.exists(atlas_pack):
         # if NOT updating atlases, just check and return
         if not self.update_atlases:
            if not os.path.isdir(atlas_pack):
               MESGe("** have build_root/%s, but it is not a directory??" \
                     % atlas_pack)
               return 1

            # we are done here
            MESGm("will reuse existing atlas directory, %s" % atlas_pack)
            return 0

         # update atlases: just move the old stuff out of the way

         if os.path.exists(renamed):
            MESGm("removing old atlas dir, %s" % renamed)
            st, ot = self.run_cmd('rmtree', renamed, pc=1)
            if st: return st

         # now rename the atlas dir to the backup name
         MESGm("moving old atlas dir %s to %s" % (atlas_pack, renamed))
         st, ot = self.run_cmd('mv', [atlas_pack, renamed], pc=1)
         if st: return st

         # and proceed with the install, below...


      # ------------- download and unpack

      # make sure there is no previous download
      tgzfile = '%s.tgz' % atlas_pack
      if os.path.exists(tgzfile):
         st, ot = self.run_cmd('rm', tgzfile)
         if st: return st

      # download and unpack atlas package
      MESGm("downloading AFNI atlas package, %s" % tgzfile)
      st, ot = self.run_cmd('curl -f -O', g_atlas_html)
      if st:
         # if we have a backup, restore it
         MESGe("failed to download AFNI atlas package")
         if os.path.exists(renamed):
            MESGp("restoring old atlas dir %s to %s" % (renamed, atlas_pack))
            st, ot = self.run_cmd('mv', [renamed, atlas_pack], pc=1)
            if st: return st
         else:
            MESGm("no atlas backup to restore from")
            MESGi("continuing with build, but will not install")
            self.run_install = 0

         # and continue without any unpacking
         return 0

      MESGm("unpacking atlas package, %s" % atlas_pack)
      st, ot = self.run_cmd('tar xfz %s' % tgzfile)
      if st: return st
      st, ot = self.run_cmd('rm', tgzfile)
      if st: return st

      return 0

   def f_get_niivue(self):
      """if no niivue file, download

         - we start in the correct directory
         - always attempt to download
            - if failure, whine but proceed (try to use backup)

         return 0 on success
      """

      # note original and potential backup names
      niivue = g_niivue_file
      backup = self.pold + g_niivue_file

      # and note atlas path for possible install or rsync suggestion
      self.sync_src_niivue = '%s/%s' % (self.do_root.abspath, niivue)

      # if it already exists, move to backup
      if os.path.exists(niivue):

         # if NOT updating niivue, just check and return
         if not self.update_niivue:
            if not os.path.isfile(niivue):
               MESGe("** have build_root/%s, but it is not a file??" \
                     % niivue)
               return 1

            # we are done here
            MESGm("will reuse existing NiiVue file, %s" % niivue)
            return 0

         # remove old backup
         if os.path.exists(backup):
            MESGm("removing old NiiVue backup, %s" % backup)
            st, ot = self.run_cmd('rm', backup)
            if st: return st

         # now rename as backup
         MESGm("moving old niivue %s to %s" % (niivue, backup))
         st, ot = self.run_cmd('mv', [niivue, backup], pc=1)
         if st: return st

         # and proceed with the install, below...

      # ------------- download

      MESGm("downloading NiiVue, %s" % niivue)
      st, ot = self.run_cmd('curl -f -O', g_niivue_html)

      # on failure, whine but proceed
      if st:
         MESGw("failed to download NiiVue, proceeding anyway...")

         if os.path.exists(backup):
            MESGm("restoring NiiVue from backup")
            # remove any partial download
            if os.path.exists(niivue):
               st, ot = self.run_cmd('rm', niivue)
            # and restore from backup
            st, ot = self.run_cmd('mv', [backup, niivue], pc=1)
            if st: return st
         else:
            MESGi("(no NiiVue backup to restore from)")

      # call anything else success

      return 0

   def f_update_git(self, gitd='git/afni'):
      """if git exists, do a git pull (if desired), else do a clone
         return 0 on success
      """

      # be sure to start from the root dir
      st, ot = self.run_cmd('cd', self.do_root.abspath, pc=1)
      if st: return st

      # if the user does not want any updates, we are done
      # (but note what is being used)
      if not self.git_update:
         MESGm("skipping any git updates")
         return self.report_branch_tag(gitd)

      # if git/afni exists, cd there and update
      # (if no git_branch opt, update master to be current)
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
         st, ot = self.run_cmd('cd', 'afni', pc=1)
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

      st, obr = self.run_cmd('git branch --show-current', quiet=1)
      if st: 
         st, obr = self.run_cmd('git rev-parse --abbrev-ref HEAD', quiet=1)
      if st: 
         obr = 'UNKNOWN_BRANCH'
      if obr == '':
         obr = '(detached)'
      st, otag = self.run_cmd('git describe', quiet=1)
      if st:
         otag = 'UNKNOWN_TAG'
      MESGm("using repo branch %s, tag %s" % (obr, otag))

      return 0

   def most_recent_tag(self):
      """just return the most recent AFNI_* tag (a clean one of length 12)
         return an empty string on failure
      """
      st, tag = self.run_cmd("git describe --tags --abbrev=0", quiet=1)
      if not tag.startswith('AFNI_') or len(tag) != 12:
         MESGe("failed to find recent AFNI build tag, have %s" % tag)
         return ''
      MESGm("have default git tag LAST_TAG, will checkout %s" % tag)

      return tag

   def clean_old_root(self):
      """an old build_root exists, clean up or back up

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
         MESGm("cleaning old build root dir, %s" % self.do_root.dname)

      st, ot = self.run_cmd('cd', self.do_root.abspath, pc=1)
      if st: return st

      # if prev stuff is a directory, create it
      if self.pold.endswith('/') and not os.path.exists(self.poldp):
         st, ot = self.run_cmd('mkdir', self.poldp, pc=1)
         if st: return st

      # if build dirs exist, rename to prev.*
      for sdir in [self.dsbuild, self.dcbuild, self.dsdoc]:
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

   def run_cmd(self, cmd, params='', pc=0, strip=1, quiet=0):
      """main purpose is to store commands in history
            cmd     : a simple command or a full one
            params  : can be a simple string or a list of them
            pc      : denotes python command
                        - cmd is a single word ('cd', 'mkdir' ...)
                        - params are separate (we will see how it goes)
            strip   : strip any non-pc text (in rare case that we want text)
            quiet   : if set, be quiet on errors
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
         self.cmd_history.append(cmd)
         st, otext = UTIL.exec_tcsh_command(cmd)
         if st:
            if not quiet:
               MESGe("failed run_cmd: %s" % cmd)
               if len(otext) > 0:
                  MESGe(otext)
         if strip:
            otext = otext.strip()
         return st, otext

      # now python commands
      if cmd == 'cd':
         cstr = "os.chdir('%s')" % pstr
         self.cmd_history.append(cstr)
      elif cmd == 'mkdir':
         cstr = "os.makedirs('%s')" % pstr
         self.cmd_history.append(cstr)
      elif cmd == 'cp':
         cstr = "shutil.copy('%s', '%s')" % (params[0], params[1])
         self.cmd_history.append(cstr)
      elif cmd == 'mv':
         # fail here for a list of params
         try:
            cstr = "os.rename('%s', '%s')" % (params[0], params[1])
         except:
            if not quiet:
               MESGe("os.rename(%s) - missing params" % pstr)
            return 1, ''
         self.cmd_history.append(cstr)
      elif cmd == 'rmtree':
         # allow this to be a file, to simplify and work like rm -fr
         if os.path.isfile(pstr):
            cstr = "os.remove('%s')" % pstr
         else:
            cstr = "shutil.rmtree('%s')" % pstr
         self.cmd_history.append(cstr)
      else:
         MESGe("unknown run_cmd: %s %s" % (cmd, pstr))
         return 1, ''

      # ready to run it
      try:
         eval(cstr)
      except:
         if not quiet:
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

      MESG("")
      prog = 'afni_proc.py' # since 'afni' might not be in text distribution
      wp = UTIL.which(prog)
      if wp != '':
         self.do_orig_abin = dirobj('orig_abin_dir', path_head(wp))
         do = self.do_orig_abin # convenience
         # try to init orig version info from AFNI_version.txt
         self.set_afni_version_info(do)
         if self.verb > 1:
            MESGp("have original abin %s" % do.abspath)
         MESGm("current AFNI: %s, %s, %s" % (do.version, do.package, do.date))
         # if package is empty, set from current version
         if self.package == '':
            MESGm("will init unset -package with current '%s'" % do.package)
            self.package = do.package
      else:
         if self.verb > 1:
            MESGm("no %s in original PATH to set orig_abin from" % prog)

      return 0

   def set_afni_version_info(self, do_abin):
      """if possible, add to do_orig_abin: version, package, date
         read from AFNI_version.txt into do_abin
      """
      # init to unknown
      do_abin.version = ''
      do_abin.package = ''
      do_abin.date = ''

      vfile = '%s/AFNI_version.txt' % do_abin.abspath

      tdata = UTIL.read_text_file(vfile, lines=1, strip=1, verb=0)
      if len(tdata) < 3:
         if self.verb > 1:
            MESGw("failed to read version info from %s" % vfile)
         return 1

      do_abin.version = tdata[0]
      do_abin.package = tdata[1]
      do_abin.date    = tdata[2]

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

