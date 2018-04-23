#!/usr/bin/env python

# python3 status: compatible

# system libraries
import sys, os, glob

if 1 :  # for testing, might add the current dir and ~/abin to the PATH
   try:    sys.path.extend(['.', '%s/abin' % os.getenv('HOME')])
   except: pass

# AFNI libraries
import afni_util as UTIL
import option_list as OL

# ----------------------------------------------------------------------
# globals

g_help_string = """
=============================================================================
gen_ss_review_table.py - generate a table from ss_review_basic output files

   Given many output text files (e.g. of the form out.ss_review.SUBJECT.txt),
   make a tab-delimited table of output fields, one infile/subject per line.

   The program is based on processing lines of the form:

        description label : value1 value2 ...

   A resulting table will have one row per input, and one column per value,
   with columns separated by a tab character, for input into a spreadsheet.

   The top row of the output will have labels.
   The second row will have value_N entries, corresponding to the labels.
   The first column will be either detected group names from the inputs,
      or will simply be the input file names.

 * See "gen_ss_review_scripts.py -help_fields" for short descriptions of
   the fields.

------------------------------------------
examples:

   1. typical usage: input all out.ss_review files across groups and subjects

      gen_ss_review_table.py -tablefile review_table.xls        \\
                -infiles group.*/subj.*/*.results/out.ss_review.*

   2. just show label table

      gen_ss_review_table.py -showlabs -infiles gr*/sub*/*.res*/out.ss_rev*

------------------------------------------
terminal options:

   -help                : show this help
   -hist                : show the revision history
   -ver                 : show the version number

------------------------------------------
process options:

   -infiles FILE1 ...   : specify @ss_review_basic output text files to process

         e.g. -infiles out.ss_review.subj12345.txt
         e.g. -infiles group.*/subj.*/*.results/out.ss_review.*

      The resulting table will be based on all of the fields in these files.

      This program can be used as a pipe for input and output, using '-'
      or file stream names.

   -overwrite           : overwrite the output -tablefile, if it exists

      Without this option, an existing -tablefile will not be overwritten.

   -separator SEP       : use SEP for the label/vals separator (default = ':')

         e.g. -separator :
         e.g. -separator tab
         e.g. -separator whitespace

      Use this option to specify the separation character or string between
      the labels and values.

   -showlabs            : display counts of all labels found, with parents

      This is mainly to help create a list of labels and parent labels.

   -show_missing        : display all missing keys

      Show all missing keys from all infiles.

   -tablefile OUT_NAME  : write final table to the given file

      If the specified file already exists, it will not be overwritten
      unless the -overwrite option is specified.

   -verb LEVEL          : be verbose (default LEVEL = 1)

------------------------------------------
Thanks to J Jarcho for encouragement and suggestions.

R Reynolds    April 2014
=============================================================================
"""

g_todo = """
   todo list:

      - when an unknown label is found, have user inform rick?
      - execute @ss_review_basic scripts for text output?
"""

g_history = """
   gen_ss_review_table.py history:

   0.0  Apr 07, 2014   - initial version: does the basic job
   0.1  Apr 08, 2014   - try to parse SID/GID from input file names
   0.2  Apr 09, 2014   - help update, separators, parents
   0.3  Jun 26, 2014   - label typos: track 'degress of freedom' as 'degrees'
   0.4  Aug 25, 2014   - defined oind
   0.5  May 19, 2014   - mention gen_ss_review_scripts.py -help_fields
   0.6  Aug 19, 2015   - added -show_missing, to display missing keys
   0.7  Oct 28, 2015   - make 'a/E mask Dice coef' parent of 'mask correlation'
   0.8  Aug 17, 2016   - 'blur estimates (FWHM)' is parent of 'blur estimates'
   1.0  Dec 28, 2017   - python3 compatible
"""

g_version = "gen_ss_review_table.py version 1.0, December 28, 2017"


class MyInterface:
   """main interface class
     
      This uses lib_1D.py as an example."""
   def __init__(self, verb=1):
      # main variables
      self.valid_opts      = None
      self.user_opts       = None
      self.showlabs        = 0          # flag - print labels at end
      self.show_missing    = 0          # flag - print missing keys

      # control
      self.separator       = ':'        # field separator (only first applies)
      self.seplen          = 1          # length, to avoid recomputing
      self.overwrite       = 0
      self.verb            = 1

      # infile name parsing
      self.infiles         = []
      self.snames          = []
      self.gnames          = []
      self.tablefile       = ''

      # result variables
      self.labels          = [] # list of input labels
      self.parents         = [] # list of input label parents
      self.ldict           = [] # corresponding list of infile dictionaries
      self.maxcounts       = {} # max count of elements per dict entry
      self.subjcounts      = {} # number of infiles having each label

      # initialize valid_opts
      self.valid_opts = self.get_valid_opts()

   def get_valid_opts(self):
      vopts = OL.OptionList('valid opts')

      # short, terminal arguments
      vopts.add_opt('-help', 0, [], helpstr='display program help')
      vopts.add_opt('-hist', 0, [], helpstr='display the modification history')
      vopts.add_opt('-ver', 0, [], helpstr='display the current version number')

      # general options
      vopts.add_opt('-infiles', -1, [],
                    helpstr='input text files (from @ss_review_basic)')
      vopts.add_opt('-overwrite', 0, [],
                    helpstr='allow overwrite for output table file')
      vopts.add_opt('-separator', 1, [],
                    helpstr="specify field separator (default=':')")
      vopts.add_opt('-showlabs', 0, [],
                    helpstr='show list of labels found')
      vopts.add_opt('-show_missing', 0, [],
                    helpstr='show all missing keys')
      vopts.add_opt('-tablefile', 1, [],
                    helpstr='file name for output table')
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
         print(g_help_string)
         return 1

      if '-hist' in argv:
         print(g_history)
         return 1

      if '-show_valid_opts' in argv:
         self.valid_opts.show('', 1)
         return 1

      if '-ver' in argv:
         print(g_version)
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

         elif opt.name == '-infiles':
            self.infiles, err = uopts.get_string_list('', opt=opt)
            if self.infiles == None or err:
               print('** failed to read -infiles list')
               errs +=1

            self.parse_infile_names()

         elif opt.name == '-overwrite':
            self.overwrite = 1

         elif opt.name == '-separator':
            self.separator, err = uopts.get_string_opt('', opt=opt)
            if self.separator == None or err:
               print("** bad -tablefile option")
               errs += 1
            if   self.separator == 'tab': self.separator = '\t'
            elif self.separator == 'whitespace': self.separator = 'ws'
            self.seplen = len(self.separator)

         elif opt.name == '-showlabs':
            self.showlabs = 1

         elif opt.name == '-show_missing':
            self.show_missing = 1

         elif opt.name == '-tablefile':
            self.tablefile, err = uopts.get_string_opt('', opt=opt)
            if self.tablefile == None or err:
               print("** bad -tablefile option")
               errs +=1

         else:
            oind = self.user_opts.olist.index(opt)
            print('** unknown option # %d: %s' % (oind+1, opt.name))
            errs += 1
            break

      # allow early and late error returns
      if errs: return -1

      # ------------------------------------------------------------
      # apply any trailing logic

      if len(self.infiles) < 1:
         print('** missing -infiles option')
         errs += 1

      if errs: return -1

      return 0

   def parse_infiles(self):
      """make a list of field names and dictionaries
         (fill self.labels, self.ldict)

         for field name list, sorting is hard if fields are not consistent
            - for a new field (after file 0), try inserting after or before
              surrounding fields

         make list of 
         for each infile: make a dictionary of name:valuelist
      """

      errs = 0
      # check file existence first
      for ifile in self.infiles:
         if ifile in ['-', 'stdin']: pass
         elif not os.path.isfile(ifile):
            print('** input file not found: %s' % ifile)
            errs += 1
      if errs: return 1
         
      # check for existence separately
      for ifile in self.infiles:
         if self.verb > 2: print('++ processing %s ...' % ifile)

         # open, read, close
         if ifile in ['-', 'stdin']: fp = sys.stdin
         else:
            try: fp = open(ifile)
            except:
               print("** failed to open input file %s" % ifile)
               return 1
         ilines = fp.readlines()
         if ifile != sys.stdin: fp.close()

         # empty should be a terminal failure
         if len(ilines) < 1:
            print('** empty input for file %s' % ifile)
            return 1

         if len(self.labels) == 0:
            rv, self.labels = self.make_labels(ilines)
            self.parents = [self.find_parent_label(lab) for lab in self.labels]
            if rv: return 1

         rv, ldict = self.make_dict(ilines)
         if rv: return 1

         self.ldict.append(ldict)

      return 0

   def make_labels(self, ilines):
      """parse a list of the form
                LABEL   : VALUES ...
         and return a LABEL list (with no trailing separator (':'))
         initialize maxcounts, subjcounts here
      """

      llist = []
      for lind, lstr in enumerate(ilines):
         # get label and value list
         rv, label, vals = self.get_label_vals(lstr)
         if rv < 1: continue

         nvals = len(vals)

         # label = self.find_parent_label(label)

         if self.verb > 2: print('++ label: %s, %d val(s)' % (label, nvals))

         llist.append(label)
         self.maxcounts[label] = nvals
         self.subjcounts[label] = 0

      if not UTIL.vals_are_unique(llist):
         print('** warning: labels are not unique, will use only last values')
         llist = UTIL.get_unique_sublist(llist)

      return 0, llist

   def find_parent_label(self, label):
      # try to replace any old fields with new ones
      if label == 'maximum F-stat':
         return 'maximum F-stat (masked)'
      if label == 'num censored TRs per run':
         return 'num TRs per run (censored)'
      if label == 'num TRs per stim':
         return 'num TRs per stim (orig)'
      if label == 'degress of freedom left':    # typo (fixed 6/25/14)
         return 'degrees of freedom left'
      if label == 'degress of freedom used':    # typo (fixed 6/25/14)
         return 'degrees of freedom used'
      if label == 'anat/EPI mask correlation':
         return 'anat/EPI mask Dice coef'
      if label == 'blur estimates':
         return 'blur estimates (FWHM)'

      return label

   def make_dict(self, ilines):
      """parse a list of the form
                LABEL   : VALUES ...
         and return a dictionary of dd[LABEL] = [values]

         monitor maxcounts
         accumulate subjcounts
      """

      ldict = {}
      for lind, lstr in enumerate(ilines):
         # get label and value list
         rv, label, vals = self.get_label_vals(lstr)
         if rv < 1: continue

         nvals = len(vals)

         # label = self.find_parent_label(label)

         if self.verb > 3: print('++ dict[%s] = %s' % (label, vals))

         # if new label, try parent, else add
         if label not in self.labels:
            parent = self.find_parent_label(label)
            if parent in self.parents:
               ll = self.labels[self.parents.index(parent)]
               if self.verb > 3:
                  print('-- converting label %s to %s' % (label, ll))
               label = ll
            else: self.insert_new_label(label, lind, nvals)

         ldict[label] = vals
         self.update_max_counts(label, nvals)

      return 0, ldict

   def get_label_vals(self, line, getvals=1):
      """parse a line into label and values

         special case: if separator == ws (whitespace) split the whole line

         return status, label, vals (unless getvals==0)

         status: -1 : error
                  0 : no separator found
                  1 : success
      """
      # either split whole line (for whitespace) or go after specific separator
      if self.separator == 'ws':
         fields = line.split()
         label = fields.pop(0)
         vals = fields
         if label == '': return 0, '', []
         
      else:
         cind = line.find(self.separator)
         if cind < 0: return 0, '', []

         label = line[0:cind].strip()
         vals = line[cind+self.seplen:].split()

      if self.verb > 4:
         print('-- GLV: label %s, vals %s' % (label, vals))

      return 1, label, vals

   def update_max_counts(self, label, nvals):
      """update maxcounts and subjcounts"""
      if label not in self.maxcounts:
         if self.verb > 1:
            print('** found new label key: %s' % label)
         self.maxcounts[label] = nvals

      else: # rcr - safe as one line?  will it be parsed?
         if nvals > self.maxcounts[label]: self.maxcounts[label] = nvals

      self.subjcounts[label] += 1

   def insert_new_label(self, label, index, nvals):
      """insert the new label into the labels list and init maxcounts"""
      if label in self.labels: return
      self.labels.append(label)
      self.parents.append(self.find_parent_label(label))
      self.maxcounts[label] = nvals
      self.subjcounts[label] = 0

   def parse_infile_names(self):
      """try to get subject and possibly group names from infiles

         fill self.snames and self.gnames, if possible

         1. get SID
            - if files look like out.ss_review.SID.txt, that is a good start
            - else, look for varying part of filename
         2. get GID
            - replace SID in infile names and for varying group name
      """

      rv, slist = UTIL.list_minus_pref_suf(self.infiles,'out.ss_review.','.txt')
      if rv < 0: return
      if rv > 0:
         if self.verb > 1: print('++ trying to get SID from glob form')
         slist = UTIL.list_minus_glob_form(self.infiles, strip='dir')
      else:
         if self.verb > 1: print("++ have SIDs from 'out.ss_reiview' form")

      if len(slist) == 0:
         if self.verb > 1: print("-- empty SID list")
         return

      # make sure names are unique and not empty
      if not UTIL.vals_are_unique(slist):
         if self.verb > 1: print('-- SIDs not detected: not unique')
         return
      minlen = min([len(ss) for ss in slist])
      if minlen < 1:
         if self.verb > 1: print('-- SIDs not detected: some would be empty')
         return

      # we have a subject list
      self.snames = slist

      # now go for GID, start by replacing SIDs in infiles
      newfiles = [fname.replace(slist[ind], 'SUBJ') for ind, fname in
                        enumerate(self.infiles)]

      if UTIL.vals_are_constant(newfiles):
         print('-- no groups detected from filenames')
         return

      # okay, try to make a group list
      glist = UTIL.list_minus_glob_form(newfiles)

      # cannot have dirs in result
      for gid in glist:
         if gid.find('/') >= 0:
            if self.verb>1: print('-- no GIDs, dirs vary in multiple places')
            return

      minlen = min([len(ss) for ss in glist])
      if minlen < 1:
         if self.verb > 1: print('-- GIDs not detected: some would be empty')
         return

      if self.verb > 1: print("++ have GIDs from infiles")
      self.gnames = glist

   def display_labels(self):
      """display the final labels list"""

      nsubj = len(self.infiles)

      print('-- final label table (length %d):' % len(self.labels))
      for label in self.labels:
         nv = self.maxcounts[label]
         if nv == 1: cstr = '%3d val' % nv
         else:       cstr = '%3d vals' % nv
         nv = self.subjcounts[label]
         if nv == 1: sstr = '%3d file' % nv
         else:       sstr = '%3d files' % nv

         if nv < nsubj: short = '  (short)'
         else:          short = ''
         print('%-30s : %-10s : %-10s%s' % (label, cstr, sstr, short))

   def write_table(self):

      if not self.tablefile:
         if self.verb: print('-- no tablefile to write')
         return 0

      if len(self.labels) < 1:
         print('** no labels for output table')
         return 1

      if len(self.ldict) < 1:
         print('** no label dictionaries')
         return 1

      # open output file
      if self.tablefile in ['-', 'stdout']:
         fp = sys.stdout
      elif os.path.exists(self.tablefile) and not self.overwrite:
         print('** output table file %s exists, and no overwrite given' \
               % self.tablefile)
         return 1
      else:
         try: fp = open(self.tablefile, 'w')
         except:
            print("** failed to open table '%s' for writing" % self.tablefile)
            return 1

      if self.write_header_lines(fp): return 1
      if self.write_value_lines(fp): return 1

      if fp != sys.stdout: fp.close()

      return 0

   def write_header_lines(self, fp):
      """write 2 header lines:
            - the field labels
            - the list of corresponding values

         start with either group name (if they exist) or infile name
         next is subject name, if they exist

         Each field label should take as many columns as its values.
      """
      if len(self.labels) < 1: return 1

      
      # labels, starting with input files

      # start with group or infile string, along with subject, if possible
      if len(self.gnames) == len(self.infiles): fp.write('group')
      else:                                     fp.write('infile')

      # if len(self.snames) == len(self.infiles): fp.write('\tsubject')

      for label in self.labels:
         nf = self.maxcounts[label]-1
         fp.write('\t%s'%label)
         fp.write('\t'*nf)
      fp.write('\n')

      # next line: group and subject, if possible
      fp.write('value') # this is for group/infile
      # if len(self.snames) == len(self.infiles): fp.write('\tvalue')

      for label in self.labels:
         nf = self.maxcounts[label]
         for ind in range(nf): fp.write('\tvalue_%d' % (ind+1))
      fp.write('\n')

   def write_value_lines(self, fp):
      """write value lines, "left justified" to maxcount fields

         for each infile
            for each label
               if dict[label]: print values
               print any needed tabs
      """
      if len(self.labels) < 1: return 1

      nfiles = len(self.infiles)

      # labels, starting with input files

      # start with subject, if possible
      dosubj = len(self.snames) == len(self.infiles)
      dogrp  = len(self.gnames) == len(self.infiles)

      for ind, infile in enumerate(self.infiles):
         # first is group or infile
         if dogrp:  fp.write('%s' % self.gnames[ind])
         else: # infile instead of group
            if infile == '-': fp.write('stdin')
            else:             fp.write('%s' % infile)

         # subject, if possible (repeat?)
         # if dosubj: fp.write('\t%s' % self.snames[ind])

         for label in self.labels:
            nf = self.maxcounts[label]
            try: vals = self.ldict[ind][label]
            except:
               if self.verb>2:print('** infile %s missing key %s'%(infile,label))
               vals = []
            nv = len(vals)
            if nv > 0: fp.write('\t'+'\t'.join(vals))
            if nf > nv: fp.write('\t'*(nf-nv))
         fp.write('\n')

   def display_missing(self):
      """show files where keys are missing
      """
      if len(self.labels) < 1: return 1

      nfiles = len(self.infiles)

      # first generate list of missing labels per input file
      # (plus allmissing: a list of all missing labels)
      allmissing = []
      mlist = [] # list of file, lablist of missing labels
      for ind, infile in enumerate(self.infiles):
         missing = []
         for label in self.labels:
            if label not in self.ldict[ind]:
               missing.append(label)
            if not label in allmissing: allmissing.append(label)
         if len(missing) > 0:
            mlist.append([infile, missing])

      if len(mlist) == 0: return

      # note longest infile name length
      lens = [len(mm[0]) for mm in mlist]
      maxflen = max(lens)

      # --- set oneline, based on max missing labels and max label length ---

      # note maximum number of missing labels (over files)
      lens = [len(mm[1]) for mm in mlist]
      maxmissing = max(lens)

      # note longest (missing) label
      lens = [len(lab) for lab in allmissing]
      maxllen = max(lens)

      oneline = maxmissing < 2 or maxllen <= 10


      # show results, on one or multiple lines, each
      for mm in mlist:
         infile = mm[0]
         missing = mm[1]
         if oneline:
            print('missing keys in %-*s : %s' \
                  % (maxflen, infile, ', '.join(missing)))
         else:
            for lab in missing:
               print('missing key in %-*s : %s' % (maxflen, infile, lab))

def main():
   me = MyInterface()
   if not me: return 1

   rv = me.process_options()
   if rv > 0: return 0  # valid and exit
   if rv < 0: # error and exit
      print('** failed to process options...')
      return 1

   if me.parse_infiles():
      return 1

   if me.write_table():
      return 1

   if me.showlabs: me.display_labels()

   if me.show_missing: me.display_missing()

   return 0

if __name__ == '__main__':
   sys.exit(main())


