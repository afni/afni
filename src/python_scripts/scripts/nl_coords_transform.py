#!/usr/bin/env python

# python3 status: compatible

# system libraries
import sys, os

if 1 :  # for testing, might add the current dir and ~/abin to the PATH
   try:    sys.path.extend(['.', '%s/abin' % os.getenv('HOME')])
   except: pass

# AFNI libraries
from afnipy import option_list as OL
from afnipy import afni_util as UTIL        # not actually used, but probably will be

# ----------------------------------------------------------------------
# globals

g_help_string = """
=============================================================================
nl_coords_transform.py - transform coordinates using supplied nonlinear warps 
and affine transformation. Supply input coordinates in a text file (AFNI 1D, 
csv,fcsv). 

For fcsv, these files contain both header lines and text strings
on every line that contains a set of x,y,z coordinates in each line of input. 
The output is decorated similarly to the input for the fcsv input.
------------------------------------------


   terminal options:
      Required 
      -infile mydata.csv        : the input file of coordinates. Data should be a
                                  text file that may be 
                                    1D (columns of numbers separated by spaces)
                                    csv (columns of numbers separated by commas)
                                    fcsv (columns of numbers separated by commas
                                          with additional columns and header lines)
                                  fcsv input files are mirrored in the output, 
                                  i.e. all input header lines and non-xyz columns
                                  are duplicated in the output
  
      -inwarp 'WARPSTRING'      : warp string. This will often consist of 
                                  an affine transform and a nonlinear warp dataset.
                                  For example (using @animal_warper output type data), 
                         'composite_linear_to_template.1D template_shft_WARP.nii.gz'

      -xyzcol_start nn          : column in the input file for the xyz columns
                                  Assumed that x column, y column and z column are
                                  consecutive
      -prefix ppppp             : name of output coordinate file. If prefix ends
                                  with 1D, csv or fcsv, then that name will be used.
                                  If one of these extensions is missing, then the
                                  extension is assumed based on the input coordinate
                                  file
      -delim 'c'                : delimiter of input and output coordinate files
                                  For csv and fcsv files, this is assumed to be a
                                  comma character ','. For all other files, a space
                                  is assumed
      -orient 'LPI'/'RAI'       : orientation of input coordinates. Only RAI/LPI are
                                  accepted. Here RAI means coordinates are defined
                                  as 
                                   Right-to-left,
                                   Anterior-to-posterior,
                                   Inferior-to-superior.
                                  LPI is defined as 
                                   Left-to-right,
                                   Posterior-to-anterior,
                                   Inferior-to-superior
                                 The LPI and RAI coordinates simply negate the first
                                 two rows to convert between them

      -help                     : show this help
      -hist                     : show module history
      -show_valid_opts          : list valid options
      -ver                      : show current version

   other options
      -verb LEVEL               : set the verbosity level

-----------------------------------------------------------------------------
Daniel Glen    April 2022
=============================================================================
"""

g_history = """
   nl_coords_transform.py history:

   0.0  Apr, 2022    - initial version (using eg_main_chrono.py and AFIDS coords.csh script)
"""

g_version = "nl_coords_transform.py version 0.0, April 22, 2022"


class coords_transform:
   """interface class for MyLibrary (whatever that is)
     
      This uses lib_1D.py as an example."""
   def __init__(self, verb=1):
      # main variables
      self.status          = 0                       # exit value
      self.valid_opts      = None
      self.user_opts       = None

      # main data variables, based on -infile, prefix and warp
      self.infile          = None
      self.outfile         = None
      self.warpstring      = None

      # general variables
      self.verb            = verb
      self.delim           = ''    # delimiter - leave empty for now, 
                                   # but usually comma or space

      self.headerlines     = []    # list of string from header lines
      self.nonheadlist     = []    # list of lists a b c x y z d e f 
      self.inorient        = "LPI"
      self.fxyzname        = "tempxyzin.1D"
      self.xfxyzname       = "xformxyzout.1D"

      self.oexec           = ""

      # initialize valid_opts
      self.init_options()

   def init_options(self):
      self.valid_opts = OL.OptionList('valid opts')

      # short, terminal arguments
      self.valid_opts.add_opt('-help', 0, [],           \
                      helpstr='display program help')
      self.valid_opts.add_opt('-hist', 0, [],           \
                      helpstr='display the modification history')
      self.valid_opts.add_opt('-show_valid_opts', 0, [],\
                      helpstr='display all valid options')
      self.valid_opts.add_opt('-ver', 0, [],            \
                      helpstr='display the current version number')

      # required parameters
      self.valid_opts.add_opt('-infile', 1, [], 
                      helpstr='read the given 1D file with xyz coordinates')
      self.valid_opts.add_opt('-inwarp', 1, [],
                      helpstr='warp string with affine and nonlinear warp')

      # general options
      self.valid_opts.add_opt('-prefix', 1, [],
                      helpstr='output name')

      self.valid_opts.add_opt('-delim', 1, [],
                      helpstr='delimiter character - \",\" (comma) or \" \" (space)')

      self.valid_opts.add_opt('-xyzcol_start', 1, [],
                      helpstr='starting column of xyz input')


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
         print(g_help_string)
         return 1

      if '-hist' in sys.argv:
         print(g_history)
         return 1

      if '-show_valid_opts' in sys.argv:
         self.valid_opts.show('', 1)
         return 1

      if '-ver' in sys.argv:
         print(g_version)
         return 1

      # ============================================================
      # read options specified by the user
      self.user_opts = OL.read_options(sys.argv, self.valid_opts)
      uopts = self.user_opts            # convenience variable
      if not uopts: return -1           # error condition

      # ------------------------------------------------------------
      # process non-chronological options, verb comes first

      val, err = uopts.get_type_opt(int, '-verb')
      if val != None and not err: self.verb = val

      # ------------------------------------------------------------
      # process options sequentially, to make them like a script

      for opt in uopts.olist:

         # main options
         if opt.name == '-infile':
            if self.main_data != None:
               print('** only 1 -infile option allowed')
               return -1
            val, err = uopts.get_string_opt('', opt=opt)
            if val != None and err: return -1
            self.infile = val

         elif opt.name == '-inwarp':
            val, err = uopts.get_string_opt('', opt=opt)
            if val != None and err: return -1
            inwarp = val


         elif opt.name == '-prefix':
            val, err = uopts.get_string_opt('', opt=opt)
            if val != None and err: return -1
            prefix = val

         elif opt.name == '-delim':
            val, err = uopts.get_string_opt('', opt=opt)
            if val != None and err: return -1
            self.delim = val


         elif opt.name == '-xyzcol_start':
            val, err = uopts.get_string_opt('', opt=opt)
            if val != None and err: return -1
            self.xcol = int(xyzcol_start)
            self.ycol = self.xcol+1
            self.zcol = self.ycol+1

         elif opt.name == '-orient':
            val, err = uopts.get_string_opt('', opt=opt)
            if val != None and err: return -1
            if val in ("LPI", "RAI"):
               self.inorient = val
            else :
               print("This program only accepts LPI or RAI for now")
               return 1

         # general options

         elif opt.name == '-verb':
            val, err = uopts.get_type_opt(int, '', opt=opt)
            if val != None and err: return -1
            else: self.verb = val
            continue

      if (self.infile == ""):
         print("No input xyz file defined")
         return 1
      else:
         if(prefix.endswith(('.csv','.fcsv','.txt','.1D'))):
            self.outxyz = prefix
         else:
            ff, ext = os.path.splitext(self.infile)
            self.outxyz = prefix+ext

         # ll = len(self.infile)
         pp = self.infile.endswith(('.csv','.fcsv'))
         if (pp and (self.delim == '')):
            self.delim = ','
         else:
            if (self.delim != ''):
               self.delim = ' '  # default to space for non-csv files

      if (self.inwarp == ""):
         print("No input warp string provided")
         return 1

      self.status = 0 # update to success

      return 0

   def execute(self):

      if not self.ready_for_action(): return 1

      if self.verb > 1:
         print('-- processing...')

      # main things the program does go here
      # mostly just going to do system calls for AFNI C programs
      # fcsv files are formatted comma separated values
      # these contain information in a header and other fields in the same line
      # with the xyz information as in this excerpt from the AFIDS_macaca project:
      #
      # # Markups fiducial file version = 4.6
      # # CoordinateSystem = 0
      # # columns = id,x,y,z,ow,ox,oy,oz,vis,sel,lock,label,desc,associatedNodeID
      # vtkMRMLMarkupsFiducialNode_1,-0.115733333333333,0.128533333333333,-0.1804,0,0,0,1,1,1,0,1,AC,vtkMRMLScalarVolumeNode1
      # vtkMRMLMarkupsFiducialNode_2,-0.0898666666666667,-13.5647333333333,1.014,0,0,0,1,1,1,0,2,PC,vtkMRMLScalarVolumeNode1
      # vtkMRMLMarkupsFiducialNode_3,-0.154949266666667,-20.8540666666667,-1.18409,0,0,0,1,1,1,0,3,infracollicular sulcus,vtkMRMLScalarVolumeNode1
      try:
         infileptr = open(infile,'r')
         # templines = infileptr.readlines()
         fxyzptr = open(fxyzname,"w")
         for templine in infileptr :
            templine = infileptr.readline()
            # check if line starts with '#' and put that into header line
            ind = templine.find('#')
            if (ind == 0 ) :
               self.headerlines.append[templine]
            else : # split on spaces, commas or some specified delimiter
               tls = templine.split(self.delim)
               self.nonheadlist.append(tls)
               tlsx = tls[self.xcol]
               tlsy = tls[self.ycol]
               tlsz = tls[self.zcol]
               if (self.inorient == "LPI"):
                  tlsx = -tlsx
                  tlsy = -tlsy
               fxyzptr.writeline("%s %s %s\n" % (tlsx, tlsy, tlsz))

         fxyzptr.close()

         infileptr.close()
         print("Finished reading input coordinates, writing temp xyz")
      except:
         print("ERROR: could not read input coordinate file")
         return 1

      try:
         com = shell_com(  \
            # apply warp to xyz (inverted warp per definition in 3dNwarpXYZ help)
            # this will be slightly different than inverting warp volume and applying
            # this warp is the combined warp in the intermediate directory from both
            # affine transformation and nonlinear warp
            "3dNwarpXYZ -nwarp %s -iwarp %s > %s" % (inwarp, fxyzname, xfxyzname), \
               self.oexec)
         # if this fails, notify the user
         if com.run():
            print("ERROR: 3dNwarpXYZ failed")
            return 1

      except:
         print("ERROR: could not transform xyz with supplied warp")
         return 1


      try:
         # read in just converted RAI xyz coordinates
         infileptr = open(xfxyzname,'r')
         # templines = infileptr.readlines()
         outxyzptr = open(outxyz,"w")

         # write the same header as the input
         outxyzptr.writeline(self.headerlines)

         # replace the xyz from original list and keep the rest of the line
         for coordline in self.nonheadlist:
            templine = infileptr.readline()
            outxyz_coords = templine.split()
            coordline[self.xcol] = outxyz_coords[0]
            coordline[self.ycol] = outxyz_coords[1]
            coordline[self.zcol] = outxyz_coords[2]
            if (self.inorient == "LPI"): # RAI -> original LPI, negate x,y
               coordline[self.xcol] = -coord[self.xcol]
               coordline[self.ycol] = -coord[self.ycol]
            outxyzptr.writeline(coordline)

      except:
         print("ERROR: could not write final transformed coordinates")
         return 1

   def ready_for_action(self):
      """perform any final tests before execution"""
      if self.iniwarp == "":
          print("ERROR: Must provide warp string.")
          ready = 0
      else :
          ready = 1

      return ready

   def init_from_file(self, fname):
      """load a 1D file, and init the main class elements"""

      self.status = 1 # init to failure

      print("--  process input coordinates from '%s'" % fname)
      self.infile = fname  # set main data object from '-infile fname'

      self.status = 0 # update to success

      return 0

   def test(self, verb=3):
      """one might want to be able to run internal tests,
         alternatively, test from the shell
      """
      print('------------------------ initial tests -----------------------')
      self.verb = verb

      print('------------------------ reset files -----------------------')

      print('------------------------ should fail -----------------------')

      print('------------------------ more tests ------------------------')

      return None

def main():
   ct_me = coords_transform() # initialize class
   if not ct_me: return 1

   rv = ct_me.process_options() # process user input
   if rv > 0: return 0  # exit with success (e.g. -help)
   if rv < 0:           # exit with error status
      print('** failed to process options...')
      return 1

   # else: rv==0, continue with main processing ...

   rv = ct_me.execute() # main processing
   if rv > 0: return 1

   return ct_me.status

if __name__ == '__main__':
   sys.exit(main())


