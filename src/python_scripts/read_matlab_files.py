#!/usr/bin/env python

# system libraries
import sys, os, glob

# AFNI libraries
import option_list as OL
import afni_util as UTIL
import lib_afni1D as LD

# ----------------------------------------------------------------------
# globals

g_help_string = """
=============================================================================
read_matlab_files.py    - describe or convert MATLAB files (to 1D)

   Describe the contents of matlab files, and possibly convert them to 1D.

   Using only -infiles, all file objects (names not starting with '__') will
   be reported.  With the addition of -prefix, all numpy matrices will be
   converted to 1D format.

------------------------------------------
examples:

   1. Describe the contents of all matlab files.

      read_matlab_files.py -infiles *.mat

   1. Convert all matlab files in the current directory to test.*.1D

      read_matlab_files.py -infiles *.mat -prefix test

------------------------------------------
terminal options:

   -help                : show this help
   -hist                : show the revision history
   -ver                 : show the version number

------------------------------------------
process options:

   -infiles             : specify input files
   -overwrite           : overwrite any output file
   -prefix PREFIX       : prefix for output file names

        Using -prefix, output files will have the naming format:

           PREFIX.INDEX.KEY.1D

              PREFIX    : as specified with -prefix
              INDEX     : 1-based index of objects found in file
              KEY       : key (label) corresponding to the given object

------------------------------------------
R Reynolds    January 2015
=============================================================================
"""

g_todo = """
   todo list:
"""

g_history = """
   read_matlab_files.py history:

   0.0  Jan 14, 2015 - initial version
"""

g_version = "read_matlab_files.py version 0.0, Jan 14, 2015"


class MyInterface:
   """interface class for MyLibrary (whatever that is)
   """
   def __init__(self, verb=1):
      # main variables
      self.valid_opts      = None
      self.user_opts       = None

      # control
      self.verb            = 1

      # process vars
      self.infiles         = []
      self.prefix          = ''
      self.overwrite       = 0

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
                    helpstr='specify input files')
      vopts.add_opt('-overwrite', 0, [],
                    helpstr='flag to overwrite any existing output files')
      vopts.add_opt('-prefix', 1, [],
                    helpstr='specify prefix for output files')
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
         print g_help_string
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

         elif opt.name == '-infiles':
            self.infiles, err = uopts.get_string_list('', opt=opt)
            if self.infiles == None or err:
               print '** failed to read -infiles list'
               errs +=1

         elif opt.name == '-overwrite':
            self.overwrite = 1

         elif opt.name == '-prefix':
            val, err = uopts.get_string_opt(opt=opt)
            if val != None and not err: self.prefix = val

      # allow early and late error returns
      if errs: return -1

      # ------------------------------------------------------------
      # apply any trailing logic

      # if here, require input files
      if len(self.infiles) < 1:
         print '** missing -infiles option'
         errs += 1

      # if no -prefix and no -verb, default verb to 2
      if not self.user_opts.find_opt('-verb') and self.prefix == '':
         self.verb = 2

      if errs: return -1

      return 0

   def process_matlab_file(self, fname, index=0):
      """process matlab files
      """

      try:
         import scipy.io
         import numpy
      except:
         print '** missing library: scipy.io'
         print '   (please install scipy)'
         return 1

      if not os.path.isfile(fname):
         print "** missing file '%s'" % fname
         return 1

      mfile = scipy.io.loadmat(fname)
      if mfile == None: return 1

      # prepare output prefix
      prefix = self.prefix
      if prefix != '' and len(self.infiles) > 1:
         prefix = '%s.%02d' % (self.prefix, index+1)

      klist = [key for key in mfile.keys() if key[0:2] != '__']
      maxlen = max([len(key) for key in klist])

      if self.verb:
         if self.verb > 1: print
         print '-- file %s has %d key(s)' % (fname, len(klist))

      for key in klist:
         obj = mfile[key]
         if self.verb > 1:
            print ('   %-*s %s' % (maxlen, key, type(obj))),
            if type(obj) is numpy.ndarray: print ' shape %s' % str(obj.shape)
            else:                          print

         # maybe write any numpy data
         if prefix != '' and type(obj) is numpy.ndarray:
            # convert object to Afni1D and transpose
            olist = obj.tolist()
            adata = LD.Afni1D(from_mat=1, matrix=olist, verb=self.verb)
            adata.transpose()

            # if model, break apart, else just write
            if key == 'model' and adata.nvec >= 2:
               self.write_model_files(adata, prefix)
            else:
               ofile = '%s.%s.1D'%(prefix,key)
               print '++ writing ndarry to %s' % ofile
               adata.write(ofile, overwrite=self.overwrite)

      return 0

   def write_model_files(self, adata, prefix):
      """break model matrix into many 1D files
      """

      if adata.nvec < 1: return 0

      for ind in range(adata.nvec):
         avec = LD.Afni1D(from_mat=1, matrix=[adata.mat[ind]], verb=self.verb)
         ofile = '%s.model.%02d.1D' % (prefix, ind+1)
         print '++ writing model file %s' % ofile
         avec.write(ofile, overwrite=self.overwrite)

      return 0

   def process_files(self):
      """process matlab files
      """

      print '-- have %d files to process' % len(self.infiles)

      # check file existence first
      for ind, ifile in enumerate(self.infiles):
         if self.process_matlab_file(ifile, ind): return 1

      return 0
         
def main():
   me = MyInterface()
   if not me: return 1

   rv = me.process_options()
   if rv > 0: return 0  # exit with success
   if rv < 0:           # exit with error status
      print '** failed to process options...'
      return 1

   if me.process_files(): return 1

   return 0

if __name__ == '__main__':
   sys.exit(main())


