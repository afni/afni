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

      gen_ss_review_table.py -write_table review_table.xls        \\
                -infiles group.*/subj.*/*.results/out.ss_review.*

   2. just show label table

      gen_ss_review_table.py -showlabs -infiles gr*/sub*/*.res*/out.ss_rev*

   3. report outliers: subjects with "outlier" table values
      (include all 'degrees of freedom left' values in the table)

      gen_ss_review_table.py                                          \\
              -outlier_sep space                                      \\
              -report_outliers 'censor fraction' GE 0.1               \\
              -report_outliers 'average censored motion' GE 0.1       \\
              -report_outliers 'max censored displacement' GE 8       \\
              -report_outliers 'TSNR average' LT 300                  \\
              -report_outliers 'degrees of freedom left' SHOW         \\
              -infiles sub*/s*.results/out.ss*.txt                    \\
              -write_outliers outliers.values.txt

   4. report outliers: subjects with varying columns, where they should not

      gen_ss_review_table.py                                          \\
              -outlier_sep space                                      \\
              -report_outliers 'AFNI version' VARY                    \\
              -report_outliers 'num regs of interest' VARY            \\
              -report_outliers 'final voxel resolution' VARY          \\
              -report_outliers 'num TRs per run' VARY                 \\
              -infiles sub*/s*.results/out.ss*.txt                    \\
              -write_outliers outliers.vary.txt

      * Note that examples 3 and 4 could be put together, but it might make
        processing easier to keep them separate.

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

   -overwrite           : overwrite the output -write_table, if it exists

      Without this option, an existing -write_table will not be overwritten.

   -outlier_sep SEP     : use SEP for the outlier table separator

         e.g.     -outlier_sep tab
         default. -outlier_sep space

      Use this option to specify how the fields in the outlier table are
      separated.  SEP can be basically anything, with some special cases:

         space  : (default) make the columns spatially aligned
         comma  : use commas ',' for field separators
         tab    : use tabs '\\t' for field separators
         STRING : otherwise, use the given STRING as it is provided

   -separator SEP       : use SEP for the label/vals separator (default = ':')

         e.g. -separator :
         e.g. -separator tab
         e.g. -separator whitespace

      Use this option to specify the separation character or string between
      the labels and values.

   -showlabs            : display counts of all labels found, with parents

      This is mainly to help create a list of labels and parent labels.

   -show_infiles        : include input files in reviewtable result

      Force the first output column to be the input files.

   -report_outliers LABEL COMP [VAL] : report outliers, where comparison holds

        e.g. -report_outliers 'censor fraction' GE 0.1
        e.g. -report_outliers 'average censored motion' GE 0.1
        e.g. -report_outliers 'TSNR average' LT 100
        e.g. -report_outliers 'AFNI version' VARY
        e.g. -report_outliers 'global correlation (GCOR)' SHOW

      This option is used to make a table of outlier subjects.  If any
      comparison function is true for a subject (other than SHOW), that subject
      will be included in the output table.  By default, only the values seen
      as outliers will be shown (see -report_outliers_fill_style).

      The outlier table will be spatially aligned by default, though the
      option -outlier_sep can be used to control the field separator.

      In general, the comparison will be an outlier if it is true, meaning
      "LABEL COMP VAL" defines what is an outlier (as opposed to defining what
      is okay).  The parameters include:

        LABEL   : the (probably quoted) label from the input out.ss files
                  (it should be quoted to be applied as a single parameter,
                  including spaces, parentheses or other special characters)

        COMP    : a comparison operator, one of:
                  SHOW  : (no VAL) show the value, for any output subject
                  VARY  : (no VAL) show any value that varies from first subj
                  EQ    : equals (outlier if subject value equals VAL)
                  LT    : less than
                  LE    : less than or equal to
                  GT    : greater than
                  GE    : greater than or equal to
                  
        VAL     : a comparison value (if needed, based on COMP)

      RO example 1.

            -report_outliers 'censor fraction' GE 0.1

         Any subject with a 'censor fraction' that is greater than or equal to
         0.1 will be considered an outlier, with that subject line shown, and
         with that field value shown.

      RO example 2.

            -report_outliers 'AFNI version' VARY

         In determining whether 'AFNI version' varies across subjects, each
         subject is simply compared with the first.  If they differ, that
         subject is considered an outlier, with the version shown.

      RO example 3.

            -report_outliers 'global correlation (GCOR)' SHOW

         SHOW is not actually an outlier comparison, it simply means to show
         the given field value in any output.  This will not affect which
         subject lines are displayed.  But for those that are, the GCOR column
         (in this example) and values will be included.

        See also -report_outliers_fill_style and -outlier_sep.

   -report_outliers_fill_style STYLE : how to fill non-outliers in table

        e.g. -report_outliers_fill_style na
        default: -report_outliers_fill_style blank

      Aside from the comparison operator of 'SHOW', by default, the outlier
      table will be sparse, with empty positions where values are not
      outliers.  This option specifies how to fill non-outlier positions.

            blank   : (default) leave position blank
            na      : show the text, 'na'
            value   : show the original data value

   -show_missing        : display all missing keys

      Show all missing keys from all infiles.

   -write_outliers FNAME : write outlier table to given file, FNAME

      If FNAME is '-' 'stdout', write to stdout.

   -write_table FNAME    : write final table to the given file
   -tablefile   FNAME    : (same)

      Write the full spreadsheet to the given file.

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
      - add help for -report_outliers
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
   1.1  Feb 27, 2019
        - added -report_outliers, to flag concerning subjects and columns
        - make internal review table, -
        - added -outlier_sep, and include 'space' to make aligned table
        - added -show_infiles, to explicitly include input files
        - synchronize group/subj/infile in table
   1.2  Mar  7, 2019:
        - added -report_outliers_fill_style (for Paul)
        - added -write_outliers
        - added -write_table to replace -tablefile (though it still works)
"""

g_version = "gen_ss_review_table.py version 1.2, March 7, 2019"


class MyInterface:
   """main interface class
     
      This uses lib_1D.py as an example."""
   def __init__(self, verb=1):
      # main variables
      self.valid_opts      = None
      self.user_opts       = None
      self.showlabs        = 0  # flag - print labels at end
      self.show_infiles    = 0  # flag - include input file in table
      self.show_missing    = 0  # flag - print missing keys

      # control
      self.valid_out_seps  = ['space', 'comma', 'tab']
      self.separator       = ':'# input field separator (only first applies)
      self.seplen          = 1  # length, to avoid recomputing
      self.out_sep         = '\t'# output field separator
      self.overwrite       = 0
      self.verb            = 1

      # outlier table
      self.report_outliers = 0
      self.ro_tablefile    = '-'
      self.ro_list         = [] # list of [LABEL, COMPARE, VAL,...]
      self.ro_valid_comps  = ['SHOW', 'VARY', 'EQ', 'NE',
                              'LT', 'LE', 'GT', 'GE']
      self.ro_valid_fills  = ['blank', 'na', 'value']
      self.ro_valid_heads  = ['label', 'acronym', 'blank']
      self.ro_fill_type    = 'blank'    # blank, na, value
      self.ro_head_type    = 'acronym'  # label, index, acronym
      self.ro_sep_type     = 'space'    # space, comma, tab

      # infile name parsing
      self.infiles         = []
      self.snames          = []
      self.gnames          = []
      self.tablefile       = ''

      # result variables
      self.review_table    = [] # full subject review table
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
      vopts.add_opt('-report_outliers', -2, [],
                    helpstr='report outlier subjects for test')
      vopts.add_opt('-report_outliers_fill_style', 1, [],
                    acplist=self.ro_valid_fills,
                    helpstr='how to fill empty (non-outlier) entries')
      vopts.add_opt('-report_outliers_header_style', 1, [],
                    acplist=self.ro_valid_heads,
                    helpstr='how to format column headers')
      vopts.add_opt('-separator', 1, [],
                    helpstr="specify field separator (default=':')")
      vopts.add_opt('-outlier_sep', 1, [],
                    helpstr="output field separator (default=tab)")
      vopts.add_opt('-showlabs', 0, [],
                    helpstr='show list of labels found')
      vopts.add_opt('-show_infiles', 0, [],
                    helpstr='make tablefile output start with input files')
      vopts.add_opt('-show_missing', 0, [],
                    helpstr='show all missing keys')
      vopts.add_opt('-write_outliers', 1, [],
                    helpstr='file name for outlier table (default=stdout)')
      vopts.add_opt('-write_table', 1, [],
                    helpstr='file name for output table (dupe for -tablefile)')
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
               print("** bad -separator option")
               errs += 1
            if   self.separator == 'tab': self.separator = '\t'
            elif self.separator == 'whitespace': self.separator = 'ws'
            self.seplen = len(self.separator)

         elif opt.name == '-outlier_sep':
            self.out_sep, err = uopts.get_string_opt('', opt=opt)
            if self.out_sep == None or err:
               print("** bad -outlier_sep option")
               errs += 1
            if   self.out_sep == 'tab': self.out_sep = '\t'
            elif self.out_sep == 'comma': self.out_sep = ','
            # 'space' is handled as a special case

         # ------------------------------
         # report outliers
         elif opt.name == '-report_outliers':
            params, err = uopts.get_string_list('', opt=opt)
            if params == None or err:
               print('** failed to parse -report_outliers %s' % params)
               errs +=1
               continue

            if self.add_test_to_outlier_report(params):
               errs +=1
               continue

            self.report_outliers = 1

         elif opt.name == '-report_outliers_fill_style':
            self.ro_fill_type, err = uopts.get_string_opt('', opt=opt)
            if self.ro_fill_type is None or err:
               print("** bad opt: -report_outliers_fill_style %s" \
                     % self.ro_fill_type)
               errs += 1

         elif opt.name == '-report_outliers_header_style':
            self.ro_head_type, err = uopts.get_string_opt('', opt=opt)
            if self.ro_head_type is None or err:
               print("** bad opt: -report_outliers_header_style %s" \
                     % self.ro_head_type)
               errs += 1

         # ------------------------------ outliers end

         elif opt.name == '-showlabs':
            self.showlabs = 1

         elif opt.name == '-show_infiles':
            self.show_infiles = 1

         elif opt.name == '-show_missing':
            self.show_missing = 1

         # try to replace -tablefile with -write_table
         elif opt.name == '-write_table' or opt.name == '-tablefile':
            self.tablefile, err = uopts.get_string_opt('', opt=opt)
            if self.tablefile == None or err:
               print("** bad %s option" % opt.name)
               errs +=1

         elif opt.name == '-write_outliers':
            self.ro_tablefile, err = uopts.get_string_opt('', opt=opt)
            if self.ro_tablefile == None or err:
               print("** bad -write_outliers option")
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
         if self.verb > 1: print("++ have SIDs from 'out.ss_review' form")

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

      if UTIL.vals_are_constant(newfiles) and self.verb > 1:
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

   def add_test_to_outlier_report(self, test_params):
      """add a comparison test to the outlier report
         --> this adds a column (or set of them) to the report
      """
      # first be sure there are enough entries to test
      nvals = len(test_params) - 2
      if nvals < 0:
         print("** incomplete outlier test: %s" % ' '.join(test_params))
         return 1
      # this check might go away
      if nvals > 1:
         print("** too many vals in outlier test: %s" % ' '.join(test_params))
         return 1

      self.ro_list.append(test_params)

      return 0

   def make_outlier_report(self):
      """make a table of subject outliers

         subject labelV1 labelV2 ...
         limit   LIMIT   LIMIT
         subj    val     val
         subj            val

         return 0 on success
      """

      # verify labels, operators and nvals
      if not self.outlier_tests_are_valid():
         return 1

      # make a complete table for the tested labels
      rev_labels = [otest[0] for otest in self.ro_list]
      rv, test_report = self.create_review_table(labels=rev_labels)
      if rv: return 1

      if self.ro_insert_test_labels(test_report, self.ro_list):
         return 1

      # clear any elements that are not considered failures
      rv, test_report = self.ro_apply_tests(test_report, self.ro_list)
      if rv: return 1

      have_outliers = (len(test_report) > 2)

      if have_outliers or (self.verb > 1):
         self.write_table(otable=test_report, ofile=self.ro_tablefile)
      if not have_outliers and self.verb > 0:
         print("-- no outlier subjects to list")

      return 0

   def ro_apply_tests(self, table, test_list):
      """for any non-SHOW columns, apply tests
         (run per subject so we can note which to keep)
      """

      nrows = len(table)
      rev_labels = [otest[0] for otest in test_list]
      firstind = table[0].index(rev_labels[0])
      ntestcols = sum([self.maxcounts[label] for label in rev_labels])
      # copy original row 2 for VARY comparison
      varyrow = table[2][:]

      # list of "failure" rows to keep in table (start with 2 header rows)
      faillist = [0, 1]
      for rind, row in enumerate(table):
         # skip header rows
         if rind < 2: continue

         # for each subject, do they  pass all tests
         all_passed = 1 

         # remaining columns as needed, offset by firstind
         posn = firstind
         for otest in test_list:
            label = otest[0]
            check = otest[1]
            nchecks = self.maxcounts[label]
            # might differ from given one
            test_check = check

            # if SHOW, just move along, and keep table entries
            if check == 'SHOW':
               posn += nchecks
               continue

            # avoid 'SHOW' and 'VARY', to be sure [2] exists
            if check != 'VARY':
               baseval = otest[2]

            # the main purpose: look for errors
            for repind in range(nchecks):
               if check == 'VARY':
                  # convert 'VARY' to 'NE' initial_value
                  usecheck = 'NE'
                  baseval = varyrow[posn]
               else:
                  usecheck = check

               testval = table[rind][posn]
               outlier = self.ro_val_is_outlier(testval, usecheck, baseval)
               # failure to run test
               if outlier < 0: return 1, []

               # outliers are kept but tracked, else values are cleared
               if outlier:
                  all_passed = 0
               else:
                  # fill the position based on type
                  if self.ro_fill_type == 'blank':
                     table[rind][posn] = ''
                  elif self.ro_fill_type == 'na':
                     table[rind][posn] = 'na'
                  # else leave as value
               posn += 1

         if not all_passed:
            faillist.append(rind)

      # if faillist is empty (len==2), what to do?
      failtable = [table[failind] for failind in faillist]

      return 0, failtable

   def ro_val_is_outlier(self, tval, comp, bval):
      """return whether "tval comp bval" seems true, e.g.
                         0.82 GE   1.0
         comp_list: ['SHOW', 'VARY', 'EQ', 'NE', 'LT', 'LE', 'GT', 'GE']

         all numerical tests are as floats

         return 1 if true
                0 if false
               -1 on error
      """
      # handle non-numeric first
      if comp == 'SHOW': return 0

      # handle non-numeric first
      if comp == 'VARY': return 0

      # get float directional comparison, if possible
      # (-1, 0, 1, or -2 on error)
      fcomp = self.ro_float_compare(tval, bval)

      # perform string comparison
      scomp = (tval == bval)

      # equality tests can be initially tested as strings
      #   - equality test would imply for floats, but is more general
      if comp == 'EQ':
         if scomp:       return 1 # equal strings
         if fcomp == -2: return 0 # cannot tell as floats, call different
         # float result
         if fcomp == 0:  return 1
         else:           return 0
         
      if comp == 'NE':
         if scomp:       return 0 # equal strings, this is an outlier
         if fcomp == -2: return 1 # cannot tell as floats, call different
         # float result
         if fcomp == 0:  return 0
         else:           return 1
         
      # continue with pure numerical tests (forget scomp)
      if comp == 'LT':
         if fcomp == -2: return 1 # cannot tell, call outlier
         return (fcomp < 0)
         
      if comp == 'LE':
         if fcomp == -2: return 1 # cannot tell, call outlier
         return (fcomp <= 0)
         
      if comp == 'GT':
         if fcomp == -2: return 1 # cannot tell, call outlier
         return (fcomp > 0)
         
      if comp == 'GE':
         if fcomp == -2: return 1 # cannot tell, call outlier
         return (fcomp >= 0)
         
      print("** ro_val_is_outlier: unknown comp: %s" % comp)
      return 0

   def ro_float_compare(self, v1, v2):
      """try to compare as floats -1, 0, 1
         return -2 on "failure to convert"
      """
      try:
         f1 = float(v1)
         f2 = float(v2)
         if f1 < f2: return -1
         if f1 > f2: return  1
         return 0
      except:
         return -2

   def ro_insert_test_labels(self, table, test_list):
      """replace table[1] value entries with test info
      """
      rev_labels = [otest[0] for otest in test_list]
      firstind = table[0].index(rev_labels[0])
      # note how many test columns there are
      ntestcols = sum([self.maxcounts[label] for label in rev_labels])

      # just verify what we have
      if len(table[0]) != firstind + ntestcols:
         print("** insert_test_labels: inconsistent lengths")
         return 1

      # first columns all get show
      for ind in range(firstind):
         table[1][ind] = 'SHOW'

      # remaining columns as needed, offset by firstind
      posn = firstind
      for otest in test_list:
         label = otest[0]
         check = otest[1]
         if check == 'SHOW' or check == 'VARY':
            newlab = check
         else:
            newlab = '%s:%s' % (check, otest[2])

         for repind in range(self.maxcounts[label]):
            table[1][posn] = newlab
            posn += 1

      return 0

   def outlier_tests_are_valid(self):
      """validate outlier tests in ro_list
         - test all before returning

         - labels must be known
         - comparison must be valid
         - just require 1 operand for now?
            
            - what if they do not completely match?
            - some subject does not have enough; ALL subjects?
      """

      # accumulate errors and messages
      emessages = []
      for otest in self.ro_list:
         label = otest[0]
         check = otest[1]
         nop   = len(otest) - 2

         estr = ''

         if label not in self.labels:
            estr = "** bad label, '%s'" % label
         if check not in self.ro_valid_comps:
            estr  = "** invalid comparison, '%s'" % check
            estr += "   should be in: %s" % ', '.join(self.ro_valid_comps)
         if check != 'SHOW' and check != 'VARY' and nop < 1:
            estr = ("** outlier test: missing parameter")

         if estr:
            emessages.append('== outlier test: %s' % ' '.join(otest))
            emessages.append(estr)

      # if there are errors, report and fail
      if len(emessages) > 0:
         print("%s\n\n" % '\n'.join(emessages))
         return 0

      return 1

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

   def fill_table(self):
      """create an internal copy of the full table to be printed
      """
      if len(self.labels) < 1:
         print('** fill_table: no labels')
         return 1

      if len(self.ldict) < 1:
         print('** fill_table: no label dictionaries')
         return 1

      rv, self.review_table = self.create_review_table()
      if rv: return 1

      if not self.table_is_rectangular(self.review_table):
         return 1

      return 0

   def table_is_rectangular(self, table):
      """check that review_table is rectangular
      """
      if len(table) == 0: return 1

      ncols = len(table[0])
      for row in table:
         if len(row) != ncols:
            print("** review table is not rectangular starting on row:\n" \
                  "   %s" % ' '.join(row))
            return 0

      return 1

   def create_review_table(self, labels=[]):
      """fill value lines

         for each infile
            for each label
               if dict[label]: append values
               append any needed empty spaces
      """

      passed_labels = 0
      if labels == []:
         labels = self.labels
      else:
         passed_labels = 1
         # be sure each label is in proper list
         for label in labels:
            if not label in self.labels:
               print("** review_table: given label '%s' not in list" % label)
               return 1, []
        
      if len(labels) < 1: return 1, []

      # labels, starting with input files
      RT = []
      tline = []

      nfiles = len(self.infiles)

      # --------------------
      # labels, starting with input files

      # start with subject, if possible
      dosubj = len(self.snames) == len(self.infiles)
      dogrp  = len(self.gnames) == len(self.infiles)
      doinfiles = (not dogrp and not dosubj)

      # if "subject ID" is label[0], and they are unique,
      # then clear dosubj and doinfiles
      if labels[0] == 'subject ID':
         # if subject IDs are uniq and fir
         label = labels[0]
         sid_list = [self.ldict[ind][label] for ind in range(nfiles)]
         if UTIL.vals_are_unique(sid_list):
            dosubj = 0
            doinfiles = 0

      # allow user to force inclusion
      if self.show_infiles:
         doinfiles = 1

      # ------------------------------------------------------------
      # first 2 lines, fill header lines

      # --------------------
      # main header of labels
      tline = []
      if doinfiles: tline.append('infile')
      if dogrp:     tline.append('group')
      if dosubj:    tline.append('subject')

      # add value positions
      for label in labels:
         nf = self.maxcounts[label]-1
         tline.append('%s'%label)
         tline.extend(['']*nf)

      RT.append(tline)
      tline = []

      # --------------------
      # and value labels
      if doinfiles: tline.append('value')
      if dogrp:     tline.append('value')
      if dosubj:    tline.append('value')

      for label in labels:
         nf = self.maxcounts[label]
         for ind in range(nf): tline.append('value_%d' % (ind+1))
      RT.append(tline)

      # ------------------------------------------------------------
      # add value lines, one per input file
      for ind, infile in enumerate(self.infiles):
         tline = []  # current line to add to table

         # first is infile, if requested or nothing else to show
         if doinfiles:
            if infile == '-': tline.append('stdin')
            else:             tline.append('%s' % infile)

         # then possibly group
         if dogrp: tline.append('%s' % self.gnames[ind])

         # then possibly subject
         if dosubj: tline.append('%s' % self.snames[ind])

         for label in labels:
            nf = self.maxcounts[label]
            try: vals = self.ldict[ind][label]
            except:
               if self.verb > 2:
                  print('** infile %s missing key %s'%(infile,label))
               vals = []
            nv = len(vals)
            if nv > 0: tline.extend(vals)
            if nf > nv: tline.extend(['']*(nf-nv))
         RT.append(tline)

      return 0, RT

   def write_table(self, otable=None, ofile=''):

      # do we use inputs or defaults?
      if ofile: outfile = ofile
      else:     outfile = self.tablefile

      if otable: outtable = otable
      else:      outtable = self.review_table

      if not outfile:
         if self.verb: print('-- no tablefile to write')
         return 0

      if len(outtable) < 1:
         print('** no review_table for output')
         return 1

      # open output file
      if outfile in ['-', 'stdout']:
         fp = sys.stdout
      elif os.path.exists(outfile) and not self.overwrite:
         print('** output table file %s exists, and no overwrite given' \
               % outfile)
         return 1
      else:
         try: fp = open(outfile, 'w')
         except:
            print("** failed to open table '%s' for writing" % outfile)
            return 1

      # handle space as a special case
      if self.out_sep == 'space':
         return self.write_spaced_table(outtable, fp)

      for tline in outtable:
         fp.write(self.out_sep.join(tline))
         fp.write('\n')

      if fp != sys.stdout: fp.close()

      return 0

   def write_spaced_table(self, table, fp, pad=3):
      """write space separated table to file pointer

         compute the maximum column widths, then use for printing
      """
      if not self.table_is_rectangular(table):
         return 1

      ncols = len(table[0])

      # make a list of max column widths
      max_lens = [0] * ncols
      for row in table:
         rlens = [len(v) for v in row]
         for cind in range(ncols):
            if rlens[cind] > max_lens[cind]:
               max_lens[cind] = rlens[cind]

      # and write
      joinstr = ' '*pad
      for row in table:
         svals = ["%-*s" % (max_lens[vind], row[vind]) for vind in range(ncols)]
         fp.write(joinstr.join(svals))
         fp.write('\n')

      return 0

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

   # make subject dictionaries
   if me.parse_infiles():
      return 1

   # make internal copy of table
   if me.fill_table():
      return 1

   # possibly make external copy of table
   if me.tablefile:
      if me.write_table():
         return 1

   # possibly make a table of concerning subjects and values
   if me.report_outliers:
      if me.make_outlier_report():
         return 1

   # possibly show all found labels
   if me.showlabs: me.display_labels()

   if me.show_missing: me.display_missing()

   return 0

if __name__ == '__main__':
   sys.exit(main())


