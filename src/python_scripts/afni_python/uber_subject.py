#!/usr/bin/env python

# python3 status: compatible

# basically, a GUI to write an afni_proc.py command

import sys, os, copy, math

# system libraries : test, then import as local symbols
import module_test_lib
testlibs = ['copy', 'signal']
if module_test_lib.num_import_failures(testlibs): sys.exit(1)
import copy

import afni_util as UTIL
import lib_vars_object as VO
import lib_uber_subject as USUBJ
import option_list as OPT

g_command_help = """
===========================================================================
uber_subject.py               - graphical interface to afni_proc.py

The expected use of this program is to simply run it without any options.
That will start the graphical user interface (GUI), which has its own set
of help and help tools.

        usage:  uber_subject.py

---

This help describes only the command line options to this program, which
enables one to:

        - run without the GUI
        - initialize subject variables in the GUI
        - initialize control variables for control of execution
        - pass PyQt4 options directly to the GUI

----------------------------------------------------------------------
Examples:

   GUI examples:

      uber_subject.py
      uber_subject.py -qt_opts -style=motif
      uber_subject.py -svar sid FT -svar gid idiots  \\
                      -svar anat FT_anat+orig.HEAD   \\
                      -svar epi FT_epi_r*.HEAD       \\
                      -svar stim AV*.txt             \\
                      -svar stim_basis 'BLOCK(15,1)'
      uber_subject.py -cvar subj_dir my/subject/dir

   Informational examples:

      uber_subject.py -help
      uber_subject.py -help_gui
      uber_subject.py -help_install
      uber_subject.py -hist
      uber_subject.py -show_valid_opts
      uber_subject.py -show_default_vars
      uber_subject.py -todo

   Non-GUI examples (all have -no_gui):

      1. Akin to the GUI example, but use subject variables directly, not
         via -svar.

         uber_subject.py -no_gui -save_ap_command cmd.AP.1 \\
             -sid FT -gid horses                           \\
             -anat FT_anat+orig.HEAD -epi FT_epi_r*.HEAD   \\
             -stim AV*.txt -stim_basis 'BLOCK(15,1)'

      2. Process the EPI data as resting state analysis.

         Pass a subject ID, anat and EPI datasets, and # TRs to remove.
         Also, bandpass via 3dDeconvolve (while censoring), and regress
         motion derivatives (in addition to motion).

         uber_subject.py -no_gui -save_ap_command cmd.rest_state  \\
             -sid FT.rest -tcat_nfirst 2                          \\
             -anat FT/FT_anat+orig -epi FT/FT_epi_r*.HEAD         \\
             -regress_bandpass 0.01 0.1 -regress_mot_deriv yes

----------------------------------------------------------------------
Note, for passing subject variables, use of -svar is safer then using
variable names directly (e.g. "-svar stim AV*.txt" vs. "-stim AV*.txt"),
because if there is a mistake in the variable name, it would be grouped
with the previous variable.

For example, compare these 2 versions of the same mistake:

        -svar stim stim_*.txt -svar eppppi EPI_r*.HEAD
   vs.
        -stim stim_*.txt      -eppppi EPI_r*.HEAD

In the former case, there would be an error about epppi not being a
valid variable.  But in the latter case, the program would not know
that you mean -eppppi as a new variable, so -eppppi and the EPI*.HEAD
files would be taken as more -stim inputs.

In any case, passing variables this way is mostly available for my own
evil purposes.  This is supposed to be a GUI after all...

----------------------------------------------------------------------
"""

g_help_trailer = """

- R Reynolds  Feb, 2011
===========================================================================
"""

def get_valid_opts():
   """return an OptionsList of valid program options"""

   # terminal, informative options
   vopts = OPT.OptionList('uber_subject.py options')
   vopts.add_opt('-help', 0, [], helpstr='show this help')
   vopts.add_opt('-help_gui', 0, [], helpstr='show help for GUI')
   vopts.add_opt('-help_howto_program', 0, [], helpstr='help for programming')
   vopts.add_opt('-help_install', 0, [], helpstr='show install notes')
   vopts.add_opt('-help_install_nokia', 0, [], helpstr='Nokia install help')
   vopts.add_opt('-hist', 0, [], helpstr='show revision history')
   vopts.add_opt('-show_default_vars',0,[],helpstr='show variable defaults')
   vopts.add_opt('-show_valid_opts',0,[],helpstr='show all valid options')
   vopts.add_opt('-show_svar_dict',0,[],helpstr='show subject var dictionary')
   vopts.add_opt('-ver', 0, [], helpstr='show module version')

   vopts.add_opt('-verb', 1, [], helpstr='set verbose level')

   vopts.add_opt('-no_gui', 0, [], helpstr='do not open graphical interface')
   vopts.add_opt('-qt_opts', -1, [], helpstr='pass the given options to PyQt')
   vopts.add_opt('-print_ap_command',0,[],helpstr='show afni_proc.py script')
   vopts.add_opt('-save_ap_command',1,[],helpstr='save afni_proc.py script')
   vopts.add_opt('-exec_ap_command',0,[],helpstr='run afni_proc.py command')
   vopts.add_opt('-exec_proc_script',0,[],helpstr='run proc script')
   vopts.add_opt('-cvar', -2, [], helpstr='set control variable to value')
   vopts.add_opt('-svar', -2, [], helpstr='set subject variable to value')

   # and add all subject vars directly
   keys = list(USUBJ.g_svar_dict.keys())
   keys.sort()
   for name in keys:
      vopts.add_opt('-'+name, -1, [], helpstr=USUBJ.g_svar_dict[name])

   vopts.trailers = 0   # do not allow unknown options

   return vopts

def process_options(valid_opts, argv):
   """return status and VarsObject structs of subject and control variables

        - given list of valid options, read and process the user options
        - if terminal option or -no_gui, return 0 (succesful quit)

      return  1 : on success and terminate
              0 : on success and continue with GUI
             -1 : on error condition
   """

   # a quick out
   if len(argv) == 0: return 0, None, None, None

   # process any optlist_ options
   valid_opts.check_special_opts(argv)

   # ------------------------------------------------------------
   # check for terminal options before processing the rest
   if '-help' in sys.argv:
      print(g_command_help)
      valid_opts.show('', 1, show_count=0)
      print(g_help_trailer)
      return 1, None, None, None

   if '-help_gui' in sys.argv:
      print(USUBJ.helpstr_usubj_gui)
      return 1, None, None, None

   if '-help_howto_program' in sys.argv:
      print(USUBJ.helpstr_howto_program)
      return 1, None, None, None

   if '-help_install' in sys.argv:
      print(g_install_str)
      return 1, None, None, None

   if '-help_install_nokia' in sys.argv:
      print(g_install_nokia)
      return 1, None, None, None

   if '-hist' in sys.argv:
      print(USUBJ.g_history)
      return 1, None, None, None

   if '-show_default_vars' in sys.argv:
      USUBJ.g_ctrl_defs.show('default cvars :')
      USUBJ.g_subj_defs.show('default svars :')
      return 1, None, None, None

   if '-show_valid_opts' in sys.argv:
      valid_opts.show('', 1)
      return 1, None, None, None

   if '-show_svar_dict' in sys.argv:
      dict = USUBJ.g_svar_dict
      keys = list(dict.keys())
      keys.sort()
      for key in keys:
         print('   %-20s : %s' % (key, dict[key]))
      return 1, None, None, None

   if '-todo' in sys.argv:
      print(USUBJ.helpstr_todo)
      return 1, None, None, None

   if '-ver' in sys.argv:
      print('uber_subject.py: version %s' % USUBJ.g_version)
      return 1, None, None, None

   # ------------------------------------------------------------
   # read and process user options (no check for terminal opts)
   uopts = OPT.read_options(argv, valid_opts)
   if not uopts: return -1, None, None, None

   # init subject options struct
   svars = VO.VarsObject('subject vars from command line')
   cvars = VO.VarsObject('control vars from command line')
   guiopts = ['uber_subject.py'] 

   # first set verbose level
   val, err = uopts.get_type_opt(int, '-verb')
   if val != None and not err: verb = val
   else: verb = 1

   USUBJ.set_vstr_from_def('cvars', 'verb', ['%d'%verb], cvars)

   use_gui = 1 # assume GUI unless we hear otherwise
   svar_keys = list(USUBJ.g_svar_dict.keys())

   errs = 0

   # ------------------------------------------------------------
   # first process all setup options (e.g. -anal_type/domain)
   # - since they might go via -svar, we must search
   for opt in uopts.olist:
      # just check for 'rest' here
      if opt.name == '-anal_type':
         val, err = uopts.get_string_opt('', opt=opt)
         if val == None or err: return -1, None, None, None
         if val == 'rest':
            if verb > 1: print('-- init from rest defaults')
            svars.merge(USUBJ.g_rdef_strs)
      elif opt.name == '-anal_domain':
         val, err = uopts.get_string_opt('', opt=opt)
         if val == None or err: return -1, None, None, None
         if val == 'surface':
            print('** uber_subject.py: not ready for surface analysis')
            return -1, None, None, None

      elif opt.name == '-svar':
         val, err = uopts.get_string_list('', opt=opt)
         if val == None or err: return -1, None, None, None
         if val[0] == '-anal_type':
            if val[1] == 'rest':
               if verb > 1: print('-- init from rest defaults')
               svars.merge(USUBJ.g_rdef_strs)
         elif val[0] == '-anal_domain':
            if val[1] == 'surface':
               print('** uber_subject.py: not ready for surface analysis')
               return -1, None, None, None

   # done with analysis init options
   # ------------------------------------------------------------

   for opt in uopts.olist:
      # skip -verb and any terminal option (though they should not be here)
      if   opt.name == '-help':            continue
      elif opt.name == '-help_gui':        continue
      elif opt.name == '-hist':            continue
      elif opt.name == '-show_valid_opts': continue
      elif opt.name == '-todo':            continue
      elif opt.name == '-ver':             continue

      elif opt.name == '-verb':            continue

      # and skip any pre-setup options ...
      elif opt.name == '-anal_type':       continue
      elif opt.name == '-anal_domain':     continue

      # and skip any post-setup options ...
      elif opt.name == '-print_ap_command':continue
      elif opt.name == '-save_ap_command': continue
      elif opt.name == '-exec_ap_command': continue
      elif opt.name == '-exec_proc_script':continue

      vname = opt.name[1:]

      # now go after "normal" options

      if opt.name == '-no_gui':
         use_gui = 0
         continue

      # get any PyQt4 options
      elif opt.name == '-qt_opts':
         val, err = uopts.get_string_list('', opt=opt)
         if val != None and err: return -1, None, None, None
         guiopts.extend(val)

      # cvar requires at least 2 parameters, name and value
      elif opt.name == '-cvar':
         val, err = uopts.get_string_list('', opt=opt)
         if val != None and err: return -1, None, None, None
         # and set it from the form name = [value_list]
         if USUBJ.set_vstr_from_def('cvars', val[0], val[1:],
                                   cvars, verb=verb, spec=1) < 0:
            errs += 1
            continue

      # svar requires at least 2 parameters, name and value
      elif opt.name == '-svar':
         val, err = uopts.get_string_list('', opt=opt)
         if val == None or err: return -1, None, None, None
         # and set it from the form name = [value_list]
         if USUBJ.set_vstr_from_def('svars', val[0], val[1:],
                                   svars, verb=verb, spec=1) < 0:
            errs += 1
            continue

      # go after a direct svar key
      elif vname in svar_keys:
         val, err = uopts.get_string_list('', opt=opt)
         if val == None or err: return -1, None, None, None
         if USUBJ.set_vstr_from_def('svars', vname, val,
                                   svars, verb=verb, spec=1) < 0:
            errs += 1
            continue

      else:
         print('** invalid option: %s' % opt.name)
         errs += 1
         continue

   if not errs:         # then we can handle any processing options
      if uopts.find_opt('-print_ap_command'):
         print_ap_command(svars, cvars)

   if not errs:         # then we can handle any processing options
      opt = uopts.find_opt('-save_ap_command')
      if opt != None:
         val, err = uopts.get_string_opt('', opt=opt)
         if val == None or err: return -1, None, None, None
         save_ap_command(svars, cvars, val)

   if not errs:         # then we can handle any processing options
      if uopts.find_opt('-exec_ap_command'):
         subj = run_ap_command(svars, cvars)
         if subj != None and uopts.find_opt('-exec_proc_script'):
            subj.exec_proc_script()

   if verb > 2: # show applied subject variables
      changestr = cvars.changed_attrs_str(USUBJ.g_cdef_strs, skiplist='name',
                                         showskip=0, showdel=0)
      print('++ applied control variables: %s\n' % changestr)
      changestr = svars.changed_attrs_str(USUBJ.g_sdef_strs, skiplist='name',
                                         showskip=0, showdel=0)
      print('++ applied subject variables: %s\n' % changestr)

   if errs:    return -1, None, None, None
   if use_gui: return  0, svars, cvars, guiopts
   else:       return  1, svars, cvars, guiopts

def run_ap_command(svars, cvars):
   """create and run the afni_proc.py command
      return the subject object, or None on failure"""

   # get command (writes warnings and errors to terminal)
   subj, cmd = get_ap_command(svars, cvars)

   # write command to file
   if subj.write_ap_command():
      print('** failed to write afni_proc.py command to disk')
      return None

   # in any case, execute the command, showing the output
   status, mesg = subj.exec_ap_command()

   if status:
      print('%s\nAP Errors:\n\n%s\n' % (75*'*', mesg))
      return None
   else:
      print('%s\noutput from afni_proc.py:\n\n%s\n' % (75*'-', mesg))
      return subj

   return subj

def print_ap_command(svars, cvars):
   """run and optionally display the afni_proc.py command"""

   subj, cmd = get_ap_command(svars, cvars)
   print(cmd)

def save_ap_command(svars, cvars, fname):
   """run and optionally display the afni_proc.py command"""

   subj, cmd = get_ap_command(svars, cvars)
   if subj.write_ap_command(fname=fname):
      print('** failed to write afni_proc.py command to disk')

def get_ap_command(svars, cvars):
   """return the subject and command string
      (print warnings and errors to screen)"""

   # create the subject, check warnings and either a command or errors
   apsubj = USUBJ.AP_Subject(svars=svars, cvars=cvars)

   nwarn, wstr = apsubj.get_ap_warnings()
   status, mesg = apsubj.get_ap_command()

   if status:  # then only mention errors
      print('%s\nERRORS:\n\n%s\n' % (75*'*', mesg))
      cmd = ''
   else:
      if wstr: print('%s\n**** Warnings:\n\n%s\n%s\n' % (75*'-',wstr,75*'-'))
      cmd = '### afni_proc.py script:\n\n%s\n' % mesg

   return apsubj, cmd

g_install_str = """
   ------------------------------------------------------------------
   A. Linux

      A1. Fedora

         yum install PyQt4

   ----------

      A2. Debian/Ubuntu

         apt-get install python-qt4

   ----------

   B. OS X

      B1. via homebrew

         0. homebrew should be installed

               https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/background_install/install_instructs/steps_mac.html

         1. install pyqt

               brew install pyqt

            Note: for this to apply, /sw/bin needs to be before /usr/bin in
                  the PATH.

         2. possibly update PYTHONPATH to point to new site-packages directory
            (put this in .cshrc)

               setenv PYTHONPATH /usr/local/lib/python2.7/site-packages

      B2. do we still need fink instructions?


   For extra details, see the output from:

        afni_system_check.py -check_all
"""

g_install_nokia = """
   ------------------------------------------------------------------

      B2. directly from nokia and riverbank computing (NO LONGER RECOMMENDED):

      ** this has not been working well lately, so now fink is recommended

         { NO LONGER RECOMMENDED, but MIGHT work on: OS X 10.5, 10.6 }

         0. XCode and python (2.6) should already be installed

         1. Qt SDK for mac (large: 1.1 GB download):
            - http://qt.nokia.com/downloads
            - download LGP version of Qt SDK

         2. SIP (interface between C++ and python - small)
            - http://www.riverbankcomputing.co.uk/software/sip/download
            - tar xf sip-4.12.1.tar        ('tar xfz' if .tar.gz)
            - cd sip-4.12.1                (for example)
            - python configure.py -d /Library/Python/2.6/site-packages
            - make
            - sudo make install

         3. PyQt (small, but may take 15-20 minutes to compile):
            - http://www.riverbankcomputing.co.uk/software/pyqt/download
            - download OS/X source package (e.g. PyQt-mac-gpl-4.8.3.tar.gz)
            - cd PyQt-mac-gpl-4.8.3        (for example)
            - python configure.py -d /Library/Python/2.6/site-packages
            - make
            - sudo make install


         ** OS X 10.5 systems:

            For python version 2.5, replace /Library/Python/2.6/site-packages
            in the 2 'configure' commands below with directory Python/2.5,
            instead (i.e. use /Library/Python/2.5/site-packages).
   ------------------------------------------------------------------
"""

def run_gui(svars=None, cvars=None, guiopts=[]):
   try: from PyQt4 import QtGui
   except:
      print('\n**** failed to import PyQt4.QtGui ****\n\n'                \
            '   PyQt4 must be installed to run the uber_subject.py GUI\n' \
            '   --> see the output of: uber_subject.py -help_install\n')
      return 1

   # if the above worked, let any GUI import errors show normally
   import gui_uber_subj as GUS

   app = QtGui.QApplication(guiopts)
   dialog = GUS.SingleSubjectWindow(subj_vars=svars, ctrl_vars=cvars,
                                    set_sdir=1)
   QtGui.QApplication.setStyle(QtGui.QStyleFactory.create(dialog.gvars.style))
   dialog.show()
   app.exec_()

   return 0

def main():

   # - set any subject variables or options
   # - run the gui unless -no_gui is given

   valid_opts = get_valid_opts()
   rv, svars, cvars, guiopts  = process_options(valid_opts, sys.argv)

   if   rv > 0: return 0        # terminal success
   elif rv < 0: return 1        # terminal failure
   # else rv == 0, so continue with GUI

   return run_gui(svars, cvars, guiopts)

if __name__ == '__main__':
   sys.exit(main())
