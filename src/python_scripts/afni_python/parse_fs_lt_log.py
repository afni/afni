#!/usr/bin/env python

# system libraries
import sys, os

# AFNI libraries
import afni_util as UTIL
import option_list as OL

# ----------------------------------------------------------------------
# globals

g_program = 'parse_fs_lt_log.py'

g_help_01 = """
=============================================================================
parse_fs_lt_log.py      - parse FreeSurfer labeltable log file

   Get labeltable indices from a rank log file, such as:

        aparc+aseg_rank.niml.lt.log

   usage: parse_fs_lt_log.py -logfile aparc+aseg_rank.niml.lt.log  \\
                             -labels CC_Posterior CC_Mid_Posterior

"""
g_help_examples = """
------------------------------------------
examples:

   Example 0: common usage - simply get original indices for aparc+aseg.nii

      parse_fs_lt_log.py -logfile aparc+aseg_rank.niml.lt.log \\
                         -labels FS_white_matter -verb 0 -show_orig

   Example 1: get known FreeSurfer labels

      parse_fs_lt_log.py -logfile aparc+aseg_rank.niml.lt.log  \\
                         -labels FS_white_matter

      parse_fs_lt_log.py -logfile aparc+aseg_rank.niml.lt.log  \\
                         -labels FS_ventricles

   Example 2: get a specific list of list labels

      parse_fs_lt_log.py -logfile aparc+aseg_rank.niml.lt.log  \\
                         -labels CC_Posterior CC_Mid_Posterior

   Example 3: get known plus extra labels

      parse_fs_lt_log.py -logfile aparc+aseg_rank.niml.lt.log             \\
                         -labels FS_white_matter Left-Cerebellum-Exterior \\
                         -show_all_orig

"""
g_help_02 = """
------------------------------------------
terminal options:

   -help                : show this help
   -hist                : show the revision history
   -ver                 : show the version number

------------------------------------------
process options:

   -labels              : specify a list of labels to search for

     e.g. -labels Left-Cerebral-White-Matter  Left-Cerebellum-White-Matter  \\
                  Right-Cerebral-White-Matter Right-Cerebellum-White-Matter \\
                  CC_Posterior CC_Mid_Posterior CC_Central CC_Mid_Anterior  \\
                  CC_Anterior Brain-Stem

     e.g. -labels FS_white_matter

     For convenience, there are 2 label groups:

        FS_white_matter (as in the example):

             Left-Cerebral-White-Matter  Left-Cerebellum-White-Matter
             Right-Cerebral-White-Matter Right-Cerebellum-White-Matter
             CC_Posterior CC_Mid_Posterior CC_Central CC_Mid_Anterior
             CC_Anterior Brain-Stem

        FS_ventricles

             Left-Lateral-Ventricle Left-Inf-Lat-Vent
             3rd-Ventricle 4th-Ventricle CSF
             Right-Lateral-Ventricle Right-Inf-Lat-Vent 5th-Ventricle

   -logfile             : specify rank log file

      e.g. -logfile aparc+aseg_rank.niml.lt.log

------------------------------------------
R Reynolds    May, 2016
=============================================================================
"""

def show_help():
   print g_help_01 + g_help_examples + g_help_02

g_todo = """
   todo list:

        - eat more cheese
"""

g_history = """
   parse_fs_lt_log.py history:

   0.0  May 23, 2016 - initial version
"""

g_version = "parse_fs_lt_log.py version 0.0, May 23, 2016"

# default label lists
g_fs_wm_labels = [ \
    'Left-Cerebral-White-Matter', 'Left-Cerebellum-White-Matter',
    'Right-Cerebral-White-Matter', 'Right-Cerebellum-White-Matter',
    'CC_Posterior', 'CC_Mid_Posterior', 'CC_Central', 'CC_Mid_Anterior',
    'CC_Anterior', 'Brain-Stem' ]

g_fs_vent_labels = [ \
    'Left-Lateral-Ventricle', 'Left-Inf-Lat-Vent',
    '3rd-Ventricle', '4th-Ventricle', 'CSF',
    'Right-Lateral-Ventricle', 'Right-Inf-Lat-Vent', '5th-Ventricle' ]

# what to show
SHOW_ORIG = 1
SHOW_RANK = 2
SHOW_ALL_ORIG  = 4

class MyInterface:
   """interface class for MyLibrary (whatever that is)
   """
   def __init__(self, verb=1):
      # main variables
      self.valid_opts      = None
      self.user_opts       = None

      # control
      self.verb            = 1

      # logfile name parsing
      self.logfile         = ''
      self.labels          = []
      self.show_vals       = SHOW_ORIG | SHOW_RANK

      # initialize valid_opts
      self.valid_opts = self.get_valid_opts()

   def get_valid_opts(self):
      vopts = OL.OptionList('valid opts')

      # short, terminal arguments
      vopts.add_opt('-help', 0, [], helpstr='display program help')
      vopts.add_opt('-help_examples', 0, [], helpstr='display program examples')
      vopts.add_opt('-hist', 0, [], helpstr='display the modification history')
      vopts.add_opt('-ver', 0, [], helpstr='display the current version number')

      # general options
      vopts.add_opt('-labels', -1, [],
                    helpstr='specify labels to search for')
      vopts.add_opt('-logfile', 1, [],
                    helpstr='specify input label table log file')
      vopts.add_opt('-show_all_orig', 0, [],
                    helpstr='show all unranked indices, even if not found')
      vopts.add_opt('-show_orig', 0, [],
                    helpstr='only show unranked indices')
      vopts.add_opt('-show_rank', 0, [],
                    helpstr='only show ranked indices')
      vopts.add_opt('-verb', 1, [], helpstr='set the verbose level (def=1)')

      vopts.sort()

      return vopts

   def process_options(self):
      """return  1 on valid and exit
         return  0 on valid and continue
         return -1 on invalid
      """

      argv = sys.argv

      # process any optlist_ options
      self.valid_opts.check_special_opts(argv)

      # process terminal options without the option_list interface
      # (so that errors are not reported)

      # if no arguments are given, do default processing
      if '-help' in argv or len(argv) < 2:
         show_help()
         return 1

      if '-help_examples' in argv:
         print g_help_examples
         return 1

      if '-hist' in argv:
         print g_history
         return 1

      if '-show_valid_opts' in argv:
         self.valid_opts.show('', 1)
         return 1

      if '-ver' in argv:
         print g_version
         return 1

      # ============================================================
      # read options specified by the user
      self.user_opts = OL.read_options(argv, self.valid_opts)
      uopts = self.user_opts            # convenience variable
      if not uopts: return -1           # error condition

      # ------------------------------------------------------------
      # process verb first

      val, err = uopts.get_type_opt(int, '-verb')
      if val != None and not err: self.verb = val

      # ------------------------------------------------------------
      # process options sequentially, to make them like a script
      errs = 0
      for opt in self.user_opts.olist:
         # check for anything to skip
         if opt.name == '-verb': pass

         elif opt.name == '-logfile':
            self.logfile, err = uopts.get_string_opt('', opt=opt)
            if self.logfile == None or err:
               print '** failed to read -logfile name'
               errs +=1

         elif opt.name == '-labels':
            self.labels, err = uopts.get_string_list('', opt=opt)
            if self.labels == None or err:
               print '** option -labels: failed to process option'
               errs +=1

         elif opt.name == '-show_all_orig':
            pass        # process later

         elif opt.name == '-show_orig':
            self.show_vals = SHOW_ORIG

         elif opt.name == '-show_rank':
            self.show_vals = SHOW_RANK

      if self.user_opts.find_opt('-show_all_orig'):
         if not (self.show_vals & SHOW_ORIG):
            print "** -show_all_orig requires showing orig"
            errs += 1
         self.show_vals |= SHOW_ALL_ORIG

      # allow early and late error returns
      if errs: return -1

      # ------------------------------------------------------------
      ## apply any trailing logic

      if self.logfile == '':
         print '** missing -logfile option'
         errs += 1

      if len(self.labels) < 1:
         print '** missing -labels to search for'
         errs += 1

      if errs: return -1

      return 0

   def expand_labels(self):
      """replace known label groups with actual labels
            FS_white_matter      - white matter labels
            FS_ventricles        - fentricle labels
      """
      newlabs = []
      for label in self.labels:
         if label == 'FS_white_matter': newlabs.extend(g_fs_wm_labels)
         elif label == 'FS_ventricles': newlabs.extend(g_fs_vent_labels)
         else:                          newlabs.append(label)

      self.labels = newlabs

   def make_label_list(self, lines):
      """create list of [label, orig, rank] values

         only allow lines that look like:   XXX  INTEGER   LABEL
      """

      if self.verb: print

      llist = []
      for line in lines:
         lvals = line.split()
         if len(lvals) < 3: continue

         rankstr = lvals[0]
         origstr = lvals[1]
         label = lvals[2]

         # view in reverse order, to not waste time
         if label not in self.labels: continue

         try: vv = int(origstr)
         except: continue

         # rankstring might be an integer, else not_found and finally None
         try: vv = int(rankstr)
         except: rankstr = 'not_found'

         if self.verb: print '%-35s  %-5s -> %s' % (label, origstr, rankstr)

         if rankstr == 'not_found': rankstr = None
         llist.append([label, origstr, rankstr])

      if self.verb: print

      return llist


   def process_logfile(self):
      """main function to process logfile
      """

      # replace any known label groups
      self.expand_labels()

      # read logfile
      inlines = UTIL.read_text_file(self.logfile, lines=1, verb=self.verb)

      llist = self.make_label_list(inlines)

      if self.show_vals & SHOW_RANK:
         if self.verb: pref = 'rank : '
         else:         pref = ''
         lshow = [ll[2] for ll in llist if ll[2] != None]
         print '%s%s' % (pref, ','.join(lshow))
      if self.show_vals & SHOW_ORIG:
         if self.verb: pref = 'orig : '
         else:         pref = ''
         if self.show_vals & SHOW_ALL_ORIG:
            lshow = [ll[1] for ll in llist]
         else:
            lshow = [ll[1] for ll in llist if ll[2] != None]
         print '%s%s' % (pref, ','.join(lshow))

      return 0
         
def main():
   me = MyInterface()
   if not me: return 1

   rv = me.process_options()
   if rv > 0: return 0  # exit with success
   if rv < 0:           # exit with error status
      print '** failed to process options...'
      return 1

   if me.process_logfile(): return 1

   return 0

if __name__ == '__main__':
   sys.exit(main())


