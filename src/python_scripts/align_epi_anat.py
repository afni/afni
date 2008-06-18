#!/usr/bin/env python
# way to run script example
# python ~/afni/src/python_scripts/align_epi_anat.py \
#   -anat anat+orig -epi epi_r1+orig -base_epi median -ex_mode dry_run
# more examples

# align_epi_anat.py -anat anat+orig -epi epi+orig -epi_base 5

# align_epi_anat.py -anat sb23_mpra+orig -epi epi_r03+orig \
#   -epi2anat -ex_mode dry_run -epi_base 6 -child_epi epi_r??+orig.HEAD \
#   -anat2epi -epi2anat -tlrc_apar sb23_mpra_at+tlrc -suffix _alx2

import sys
import copy
from time import asctime

# AFNI modules
from afni_base import *
from afni_util import *
from option_list import *
from db_mod import *
import ask_me

g_help_string = """
    ===========================================================================
    align_epi_anat.py     - align EPI to anatomical datasets or vice versa
    
    This python script computes the alignment between an EPI and anatomical
    structural dataset and applies the resulting transformation to one or the
    other to bring them into alignment.
    This python script computes the transforms needed to align EPI and  
    anatomical datasets using a cost function tailored for this purpose. The  
    script combines multiple transformations, thereby minimizing the amount of 
    interpolation to the data.
    
    Basic Usage:
      align_epi_anat.py -anat anat+orig -epi epi+orig -epi_base 5
    
    The user must provide EPI and anatomical datasets and specify the EPI
    sub-brick to use as a base in the alignment.  

    Internally, the script always aligns the anatomical to the EPI dataset,
    and the resulting transformation is saved to a 1D file. 
    As a user option, The inverse of this transformation may be applied to the 
    EPI dataset in order to align it to the anatomical data instead.

    This program generates several kinds of output in the form of datasets
    and transformation matrices which can be applied to other datasets if
    needed. Time-series volume registration, oblique data transformations and
    talairach transformations will be combined as needed.
    
    Depending upon selected options, the script's output contains the following:
        Datasets:
          ANAT_al+orig: A version of the anatomy that is aligned to the EPI
          EPI_al+orig: A version of the EPI dataset aligned to the anatomy
          EPI_al+tlrc: A version of the EPI dataset aligned to a standard
                       template
        These transformations include slice timing correction and
          time-series registation by default.

        Transformation matrices:
          ANAT_al_mat.aff12.1D: matrix to align anatomy to the EPI
          EPI_al_mat.aff12.1D:  matrix to align EPI to anatomy 
                                   (inverse of above)
          EPI_vr_al_mat.aff12.1D: matrix to volume register EPI
          EPI_reg_al_mat.aff12.1D: matrix to volume register and align epi
                                      to anatomy (combination of the two
                                      previous matrices)

        Motion parameters from optional volume registration:
          EPI_reg_al_motion.1D: motion parameters from EPI time-series 
                                registration
          
    where the uppercase "ANAT" and "EPI" are replaced by the names of the
    input datasets, and the suffix can be changed from "_al" as a user
    option.
          
        You can use these transformation matrices to align other datasets:
         3dAllineate -cubic -1Dmatrix_apply epi_r1_al_mat.aff12.1D  \\
                     -prefix epi_alman epi_r2+orig

        Also, because input volumes are preprocessed before using 3dAllineate,
        the script outputs copies of the preprocessed volumes as they were used
        in 3dAllineate.

         EPI_epi_in_3dAl_al+orig : EPI volume for 3dAllineate's -base
         ANAT_epi_in_3dAl_al+orig: ANAT volume for 3dAllineate's -input
         EPI_wt_3dAl_al+orig     : weight volume for 3dAllineate's -weight
               
    The goodness of the alignment should always be assessed. At the face of it,
    most of 3dAllineate's cost functions, and those of registration programs
    from other packages, will produce a plausible alignment but it may not be
    the best. You need to examine the results carefully if alignment quality is
    crucial for your analysis.

    In the absence of a gold standard, and given the low contrast of EPI data,
    it is difficult to judge alignment quality by just looking at the two
    volumes. This is the case, even when you toggle quickly between one volume
    and the next; turning overlay off and using 'u' key in the slice window.
    To aid with the assessment of alignment, you can use the script
    @AddEdge. See the help for @AddEdge for more information on that script.
        
    ---------------------------------------------
    REQUIRED OPTIONS:
    
    -epi dset   : name of EPI dataset
    -anat dset  : name of structural dataset
    -epi_base   : the epi base used in alignment 
                     (0/mean/median/max/subbrick#)

    MAJOR OPTIONS:
    -help       : this help message

    -anat2epi   : align anatomical to EPI dataset (default)
    -epi2anat   : align EPI to anatomical dataset
                  

    -suffix ssss: append the suffix to the original anat/epi dataset to use
                     in the resulting dataset names (default is "_al")
     
    -child_epi dset1 dset2 ... : specify other EPI datasets to align.
        Time series volume registration will be done to the same
        base as the main parent EPI dataset. 
        
    -big_move   : indicates that large displacement is needed to align the
                  two volumes. This option is off by default.
    -partial_coverage: indicates that the EPI dataset covers only a part of 
                  the brain.    

    -keep_rm_files : keep all temporary files (default is to remove them)
    -prep_only  : do preprocessing steps only
    -verb nn    : provide verbose messages during processing (default is 0)
    -anat_has_skull yes/no: Anat is assumed to have skull ([yes]/no)
    -epi_strip  :  method to mask brain in EPI data 
                   ([3dSkullStrip]/3dAutomask/None)
    -volreg_method : method to do time series volume registration of EPI data 
                   ([3dvolreg],3dWarpDrive). 3dvolreg is for 6 parameter 
                   (rigid-body) and 3dWarpDrive is for 12 parameter.

    A template registered anatomical dataset such as a talairach-transformed
       dataset may be additionally specified so that output data are
       in template space. The advantage of specifying this transform here is
       that all transformations are applied simultaneously, thereby minimizing 
       data interpolation.
       
    -tlrc_apar ANAT+tlrc : structural dataset that has been aligned to
                  a master template such as a tlrc dataset. If this option
                  is supplied, then an epi+tlrc dataset will be created.


    Other options:
    -ex_mode       : execute mode (echo/dry_run/quiet/[script]). "dry_run" can
                     be used to show the commands that would be executed without
                     actually running them. "echo" shows the commands as they 
                     are executed."quiet" doesn't display commands at all.
                     "script" is like echo but doesn't show stdout, stderr 
                     header lines and "cd" lines. The "dry_run" option can be
                     used to generate scripts which can be further customized
                     beyond what may be available through the options of this
                     program.
    -Allineate_opts '-ssss  -sss' : options to use with 3dAllineate. Default
                     options are 
                     "-weight_frac 1.0 -maxrot 6 -maxshf 10 -VERB -warp aff "
    -volreg        : do volume registration on EPI dataset before alignment
                     ([on]/off)
    -volreg_opts   : options to use with 3dvolreg
    -volreg_base   : the epi base used in time series volume registration.
                     The default is to use the same base as the epi_base.
                     If another subbrick or base type is used, an additional
                     transformation will be computed between volume registration
                     and the epi_base
                     (0/mean/median/max/subbrick#)

    -tshift        : do time shifting of EPI dataset before alignment ([on]/off)
    -tshift_opts   : options to use with 3dTshift
                     The script will determine if slice timing correction is
                     necessary unless tshift is set to off.

    -deoblique     : deoblique datasets before alignment ([on]/off)
    -deoblique_opts: options to use with 3dWarp deobliquing
                     The script will try to determine if either EPI or anat data
                     is oblique and do the initial transformation to align anat
                     to epi data using the oblique transformation matrices
                     in the dataset headers.
    
    -master_epi    : master grid resolution for aligned epi output
    -master_tlrc   : master grid resolution for epi+tlrc output
    -master_anat   : master grid resolution for aligned anatomical data output
                     (SOURCE/BASE/MIN_DXYZ/dsetname/n.nn)
                     Each of the 'master' options can be set to SOURCE,BASE,
                     a specific master dataset, MIN_DXYZ or a specified cubic 
                     voxel size in mm. 
                     
                     MIN_DXYZ uses the smallest voxel dimension as the basis
                     for cubic output voxel resolution within the bounding box
                     of the BASE dataset.
                     
                     SOURCE and BASE are used as in 3dAllineate help.
                     
                     The default value for master_epi and master_anat is SOURCE,
                     that is the output resolution and coordinates should be
                     the same as the input. This is appropriate for small
                     movements.
                   
                     For cases where either dataset is oblique (and larger
                     rotations can occur), the default becomes MIN_DXYZ.
                     
                     The default value for master_tlrc is MIN_DXYZ.

    Examples:
      # align anat to sub-brick 5 of epi+orig. In addition, do slice timing
      # correction on epi+orig and register all sub-bricks to sub-brick 5
      # (Sample data files are in AFNI_data4/sb23 in sample class data)

      align_epi_anat.py -anat sb23_mpra+orig -epi epi_r03+orig     \\
                        -epi_base 5
      
      # same as example above, but also process other epi runs
      # in the same way as epi_r03+orig

      align_epi_anat.py -anat sb23_mpra+orig -epi epi_r03+orig     \\
                        -suffix _al2epi -epi_base 5                \\
                        -child_epi epi_r??+orig.HEAD
                        
      # Instead of aligning the anatomy to an epi, transform the epi
      # to match the anatomy. Children get the same treatment. Note that
      # epi sub-bricks are transformed once in the process.

      align_epi_anat.py -anat sb23_mpra+orig -epi epi_r03+orig      \\
                        -epi_base 5 -child_epi epi_r??+orig.HEAD    \\
                        -epi2anat -suffix al2anat
      
      # Bells and whistles:
      # - create talairach transformed epi datasets (still one transform)
      # - do not execute, just show the commands that would be executed.
      #   These commands can be saved in a script or modified.
      # + a bunch of other options to tickle your mind
      # The talairach transformation requires auto-talairaching 
      # the anatomical dataset first
      @auto_tlrc -base ~/abin/TT_N27+tlrc -input sb23_mpra+orig
      align_epi_anat.py -anat sb23_mpra+orig -epi epi_r03+orig      \\
                        -epi_base 6 -child_epi epi_r??+orig.HEAD    \\
                        -ex_mode dry_run -epi2anat -suffix _altest  \\
                        -tlrc_apar sb23_mpra_at+tlrc


    Our HBM 2008 abstract describing the alignment tools is available here:
      http://afni.nimh.nih.gov/sscc/rwcox/abstracts

"""   
#   -cost          : cost function used by 3dAllineate. Default is lpc, anything
#                    else is inferior!
#    -cmass cmass+ss: center of mass option for 3dAllineate 
#                     ('cmass+a','cmass+xy','nocmass',...) Default is cmass+xy.
#    -child_anat dset1 dset2 ... : specify other anatomical datasets to align.
#        The anatomical data will be aligned first to the parent
#                 structural dataset. If aligning to EPI data, then the
#                 transformation of the parent anatomical to the EPI data will
#                 be combined with the inter-structural transformation.
#
#     -fresh      : remove any temporary files at start from a previous run
#    Weighting mask options:
#    A weighting mask is used in the alignment. The default weighting mask
#    is the stripped epi dataset normalized from 0 to 1.
#    -pow_mask n.n  : raise the epi masked dataset to a power before normalizing
#                     (default is 1.0)

#    -tlrc_epar epi_template_dset : EPI  dataset that has been aligned to
#                  a master template such as a tlrc dataset If this option
#                  is supplied, then an anat+tlrc dataset will be created.



## BEGIN common functions across scripts (loosely of course)
class RegWrap:
   def __init__(self, label):
      self.align_version = 1.06 # software version (update for changes)
      self.label = label
      self.valid_opts = None
      self.user_opts = None
      self.verb = 1    # a little talkative by default
      self.rewrite = 0 #Do not recreate existing volumes
      self.oexec = "" #dry_run is an option
      self.epi2anat = 0 # align epi to anat optionally
      self.anat2epi = 1 # align anat to epi by default
      self.rmrm = 1   # remove temporary files
      self.prep_only = 0  # do preprocessing only
      self.odir = os.getcwd()    
      self.tshift_flag = 0  # do time shifting on EPI
      self.volreg_flag = 0  # do volume registration on EPI
      self.deoblique_flag = 1  # deoblique datasets first
      self.deoblique_opt = "" # deobliquing/obliquing options
      self.cmass = "" # no center of mass option for 3dAllineate
      self.epi_base = None  # don't assume representative epi
      self.reg_mat = "" # volume registration matrix 1D file
      self.obl_a2e_mat = ""  # oblique anat to epi matrix

      self.save_Al_in = 0  # don't save 3dAllineate input files
      self.master_epi_option = '-master SOURCE'
      self.master_tlrc_option = '-master SOURCE'
      self.master_anat_option = ''
      return

# box, bin and fat mask are not used for now

   def init_opts(self):
      self.valid_opts = OptionList('init_opts')
       
      self.valid_opts.add_opt('-epi',  1, [], \
               helpstr="EPI dataset to align or to which to align")
      self.valid_opts.add_opt('-anat', 1, [], \
               helpstr="Anatomical dataset to align or to which to align")
      self.valid_opts.add_opt('-keep_rm_files', 0, [])
      self.valid_opts.add_opt('-prep_only', 0, [])
      self.valid_opts.add_opt('-help', 0, [], \
               helpstr="The main help describing this program")
      self.valid_opts.add_opt('-full_help', 0, [], \
               helpstr="The main help and all available options")
      self.valid_opts.add_opt('-option_help', 0, [], \
               helpstr="Help for all available options")
      self.valid_opts.add_opt('-verb', 1, [])
      self.valid_opts.add_opt('-align_centers', 1, ['no'], ['yes', 'no'])
      self.valid_opts.add_opt('-anat_has_skull', 1, ['yes'], ['yes', 'no'])
      self.valid_opts.add_opt('-epi_strip', 1, ['3dSkullStrip'],           \
                              ['3dSkullStrip', '3dAutomask', 'None'])
      self.valid_opts.add_opt('-volreg_method', 1, ['3dvolreg'], \
                              ['3dvolreg', '3dWarpDrive', '3dAllineate'],\
                      helpstr="Time series volume registration method\n" \
                              "3dvolreg: rigid body least squares\n"     \
                              "3dWarpDrive: 12 parameter least squares\n"\
                              "3dAllineate: 12 parameter mutual info\n")
      self.valid_opts.add_opt('-ex_mode', 1, ['script'],                   \
                              ['quiet', 'echo', 'dry_run', 'script'],      \
                              helpstr="Command execution mode.\n"          \
                                       "quiet: execute commands quietly\n" \
                                       "echo: echo commands executed\n"    \
                                       "dry_run: only echo commands\n" )

      self.valid_opts.add_opt('-overwrite', 0, [],\
                               helpstr="Overwrite existing files")
      self.valid_opts.add_opt('-big_move', 0, [])
      self.valid_opts.add_opt('-partial_coverage', 0, [])

      self.valid_opts.add_opt('-Allineate_opts', -1,                       \
                             ["-weight_frac 1.0 -maxrot 6 -maxshf 10 -VERB"\
                              " -warp aff"],\
                               helpstr="Options passed to 3dAllineate.")
      self.valid_opts.add_opt('-perc', 1, ['90'])
#      self.valid_opts.add_opt('-fresh', 0, [])
      self.valid_opts.add_opt('-suffix', 1,['_al'])
      self.valid_opts.add_opt('-cost', 1,['lpc'])
#      self.valid_opts.add_opt('-fat', 1, ['1'])

      # transform anat to epi by default, but allow the other way
      # the resulting transformation will be done at the end to include
      #  any volreg and oblique transformations
      self.valid_opts.add_opt('-epi2anat', 0, [])
      self.valid_opts.add_opt('-anat2epi', 0, [])
      
      # select base EPI dataset type
      self.valid_opts.add_opt('-epi_base', 1, [], [],                  \
              helpstr = "Base to use for volume registration\n"        \
                        "Choose sub-brick number or statistic type\n"  \
                        "Valid choices can be, for example, 0,5,mean")
#                   ['0', 'sub-brick-n', 'mean', 'median', 'max'])

      # select base EPI type for volume registration
      self.valid_opts.add_opt('-volreg_base', 1, [], [],               \
              helpstr = "Base to use for volume registration\n"        \
                        "Choose sub-brick number or statistic type\n"  \
                        "Valid choices can be, for example, 0,5,median")

      # do volume registration of EPI as part of this whole mess
      self.valid_opts.add_opt('-volreg', 1, ['on'], ['on','off'])
      self.valid_opts.add_opt('-volreg_opts', -1, ["-cubic"])
 
      # do time shifting
      self.valid_opts.add_opt('-tshift', 1, ['on'], ['on','off'])
      self.valid_opts.add_opt('-tshift_opts', -1, [])

      # obliquity options
      self.valid_opts.add_opt('-deoblique', 1, ['on'], ['on','off'])
      self.valid_opts.add_opt('-deoblique_opts', -1, [])

      # 3dAllineate cmass options
      self.valid_opts.add_opt('-cmass', 1, ['cmass+xy'] )
      
      # talairach transformed anatomical parent dataset
      self.valid_opts.add_opt('-tlrc_apar', 1, [], \
         helpstr="If this is set, the results will include +tlrc\n"
                 "template transformed datasets for the epi aligned\n"
                 "to the anatomical combined with this additional\n"
                 "transformation to template of this parent dataset\n"
                 "The result will be EPI_al+tlrc.HEAD\n")

      # talairach transformed EPI parent dataset
      self.valid_opts.add_opt('-tlrc_epar', 1, [], \
         helpstr="Not available yet.\n"
                 "If this is set, the results will include +tlrc\n"
                 "template transformed datasets for the anatomical\n"
                 "aligned to the epi combined with this additional\n"
                 "transformation to template of this parent dataset\n"
                 "The result will be ANAT_al+tlrc.HEAD\n")

      # auto_talairach results
      self.valid_opts.add_opt('-auto_tlrc', 0, [], \
         helpstr="Not available yet.\n"
                 "If this is set, the results will also be aligned\n"
                 "to a template using the @auto_tlrc script.\n"
                 "Transformations computed from that will be combined\n"
                 "with the anat to epi transformations and epi to anat\n"
                 "(and volreg) transformations\n"
                 "0nly one of the -tlrc_apar, -tlrc_epar or the -auto_tlrc\n"
                 "options may be used\n")
      # child epi datasets
      self.valid_opts.add_opt('-child_epi', -1,[],\
                               helpstr="Names of child EPI datasets")

      # child anat datasets
      self.valid_opts.add_opt('-child_anat', -1,[],\
                               helpstr="Not available yet.\n"
                               "Names of child anatomical datasets")

      # master resampling options for alignment
      self.valid_opts.add_opt('-master_epi', 1,['SOURCE'],\
             helpstr="-master grid resolution for epi to anat alignment\n" \
                    "MIN_DXYZ uses the smallest dimension\n"
                    "Other options are SOURCE and BASE as in 3dAllineate help\n"
                    "For cases where either dataset is oblique, the default\n"
                    "becomes MIN_DXYZ")
      self.valid_opts.add_opt('-master_tlrc', 1,['MIN_DXYZ'],\
             helpstr="-master grid resolution for epi to tlrc anat alignment\n"\
                    "MIN_DXYZ uses the smallest dimension\n"
                    "Other options are SOURCE and BASE as in 3dAllineate help")
      self.valid_opts.add_opt('-master_anat', 1,['MIN_DXYZ'],\
             helpstr="-master grid resolution for anat to epi obliquing\n"\
                       "MIN_DXYZ uses the smallest dimension\n"
                    "Other options are ddd mm or gridset")

      # create edge images
      # do edge-based alignment

      self.valid_opts.trailers = 0   # do not allow unknown options
      # save datasets used as input to 3dAllineate
      self.valid_opts.add_opt('-save_Al_in', 0, [],    \
               helpstr = "Save datasets used as input to 3dAllineate")
      
      # weighting mask options
      self.valid_opts.add_opt('-pow_mask', 1, ['1.0'], \
               helpstr = "power for weighting 1 or 2")
      self.valid_opts.add_opt('-bin_mask', 1, ['no'], ['yes', 'no'], \
               helpstr = "convert weighting mask to 0 or 1 - Unused")
      self.valid_opts.add_opt('-box_mask', 1, ['no'], ['yes', 'no'], \
               helpstr = "Unused")

      self.valid_opts.add_opt('-mask', -1, ['vent'], \
               helpstr="Not available yet.\n"
                       "Mask to apply to data.")
  
   def dry_run(self):
      if self.oexec != "dry_run":
         return 0
      else:
         return 1
        
   def apply_initial_opts(self, opt_list):
      opt = opt_list.find_opt('-verb')    # set and use verb
      if opt != None: self.verb = int(opt.parlist[0])
      
      opt = opt_list.find_opt('-ex_mode')    # set execute mode
      if opt != None: self.oexec = opt.parlist[0]

      opt = opt_list.find_opt('-keep_rm_files')    # keep temp files
      if opt != None: self.rmrm = 0

      opt = opt_list.find_opt('-prep_only')    # preprocessing only
      if opt != None: self.prep_only = 1
            
      opt = opt_list.find_opt('-help')    # does the user want help?
      if opt != None:
         ps.self_help()
         ps.ciao(0)  # terminate

      opt = opt_list.find_opt('-full_help')  # more help?
      if opt != None:
         ps.self_help(2)
         ps.ciao(0)  # terminate

      opt = opt_list.find_opt('-option_help')  # help for options only
      if opt != None:
         ps.self_help(1)
         ps.ciao(0)  # terminate
         
      opt = opt_list.find_opt('-perc')    # set and use percentile for weight
      if opt != None: self.perc = float(opt.parlist[0])
      
      opt = opt_list.find_opt('-suffix')    
      if opt != None: 
          self.suffix = opt.parlist[0]
          if((opt=="") or (opt==" ")) :
            self.error_msg("Cannot have blank suffix")
            ps.ciao(1);

      opt = opt_list.find_opt('-cost')    
      if opt != None: self.cost = opt.parlist[0]
      
      opt = opt_list.find_opt('-pow_mask')    
      if opt != None: self.sqmask = opt.parlist[0]
      
      opt = opt_list.find_opt('-box_mask')    
      if opt != None: self.boxmask = opt.parlist[0]
      
      opt = opt_list.find_opt('-bin_mask')    
      if opt != None: self.binmask = opt.parlist[0]

      opt = opt_list.find_opt('-epi2anat')    # align epi to anat
      if opt != None: 
         self.epi2anat = 1
         self.anat2epi = 0     # turn off anat to epi unless requested
         opt = opt_list.find_opt('-anat2epi')    # align anat to epi
         if opt != None: self.anat2epi = 1

      opt = opt_list.find_opt('-deoblique')    # deoblique data
      if opt != None: 
          if(opt.parlist[0]=='on'):
              self.deoblique_flag = 1
          elif(opt.parlist[0]=='off'):
              self.deoblique_flag = 0
          else:
              self.error_msg("deoblique option not on/off")
              self.ciao(1)
      else:
          self.deoblique_flag = 1              

      opt = opt_list.find_opt('-tshift')    # do time shifting
      if opt != None: 
          if(opt.parlist[0]=='on'):
              self.tshift_flag = 1
          elif(opt.parlist[0]=='off'):
              self.tshift_flag = 0
          else:
              self.error_msg("tshift option not on/off")
              self.ciao(1)
      else:
          self.tshift_flag = 1              


      opt = opt_list.find_opt('-volreg')    # do volume registration
      if opt != None: 
          if(opt.parlist[0]=='on'):
              self.volreg_flag = 1
          elif(opt.parlist[0]=='off'):
              self.volreg_flag = 0
              self.info_msg("turning off volume registration")
          else:
              self.error_msg("volreg option not on/off");
              self.ciao(1)
      else:
          self.volreg_flag = 1              

      opt = opt_list.find_opt('-save_Al_in')  # save 3dAllineate input datasets
      if opt != None: self.save_Al_in = 1

     
   def get_user_opts(self):
      self.user_opts = read_options(sys.argv, self.valid_opts)
      if self.user_opts == None: return 1 #bad
      # no options: apply -help
      if ( len(self.user_opts.olist) == 0 or \
           len(sys.argv) <= 1 ) :
         ps.self_help()
         ps.ciao(0)  # terminate
      if self.user_opts.trailers:
         opt = self.user_opts.find_opt('trailers')
         if not opt: 
             print "** ERROR: seem to have trailers, but cannot find them!"
         else:
             print "** ERROR: have invalid trailing args: %s", opt.show()
         return 1  # failure

      # apply the user options
      if self.apply_initial_opts(self.user_opts): return 1

      if self.verb > 3: 
         self.show('------ found options ------ ')    

      return
    
   def show(self, mesg=""):
      print '%s: %s' % (mesg, self.label)
      if self.verb > 2: self.valid_opts.show('valid_opts: ')
      self.user_opts.show('user_opts: ')
   
   def info_msg(self, mesg=""):
       if(self.verb >= 1) :
          print "#++ %s" % mesg

   def error_msg(self, mesg=""):
       print "#**ERROR %s" % mesg

   def exists_msg(self, dsetname=""):
       print "** Dataset: %s already exists" % dset_name
       print "** Not overwriting."
       if(not ps.dry_run()):
           self.ciao(1)
       
   def ciao(self, i):
      if i > 0:
         print "** ERROR - script failed"
      elif i==0:
         print ""

      os.chdir(self.odir)
      sys.exit()   

   # show help
   # if help_level is 1, then show options help only
   # if help_level is 2, then show main help and options help
   def self_help(self, help_level=0):
      if(help_level!=1) :
         print g_help_string
      if(help_level):  
         print "A full list of options for %s:\n" % ps.label
         for opt in self.valid_opts.olist:
            print "   %-20s" % (opt.name )
            if (opt.helpstr != ''):
               print "   %-20s   %s" % \
                  ("   use:", opt.helpstr.replace("\n","\n   %-20s   "%' '))
            if (opt.acceptlist):
               print "   %-20s   %s" % \
                  ("   allowed:" , string.join(opt.acceptlist,', '))
            if (opt.deflist):
               print "   %-20s   %s" % \
                  ("   default:",string.join(opt.deflist,' '))
      return 1
   
   # remove all the temporary files for epi and anat base names
   def cleanup(self, rmold=0):
      opt = self.user_opts.find_opt('-epi')
      e = afni_name(opt.parlist[0]) 
      
      opt = self.user_opts.find_opt('-anat')
      a = afni_name(opt.parlist[0])
      self.fresh_start(e.prefix, a.prefix, rmold = rmold)
      return 1

   def version(self):
      self.info_msg("align_epi_anat version: %s" % self.align_version)

   # copy dataset 1 to dataset 2
   # show message and check if dset1 is the same as dset2
   # return non-zero error if can not copy
   def copy_dset(self, dset1, dset2, message, exec_mode):
      self.info_msg(message)

      if(dset1.input()==dset2.input()):
         print "# copy is not necessary"
         return 0
         
      dset2.delete(exec_mode)
      com = shell_com(  \
            "3dcopy %s %s" % (dset1.input(), dset2.out_prefix()), exec_mode)
      com.run()
      if ((not dset2.exist())and (exec_mode!='dry_run')):
         print "** ERROR: Could not rename %s\n" % dset1.input()
         return 1
      return 0
   
   

## BEGIN script specific functions   
   def process_input(self):
      #Do the default test on all options entered. 
      #NOTE that default options that take no parameters will not go 
      #through test, but that is no big deal
      for opt in self.user_opts.olist:
         if (opt.test() == None): ps.ciao(1)

      # skull stripping is on by default
      opt = self.user_opts.find_opt('-anat_has_skull')
      if opt != None and opt.parlist[0] == 'no':
          ps.skullstrip = 0
      else:
          ps.skullstrip = 1

      #Allineate extras
      opt = self.user_opts.find_opt('-Allineate_opts')
      if opt != None: 
         ps.AlOpt = string.join(opt.parlist, ' ')
      else:
         ps.AlOpt = ''

      #big_move?
      opt = self.user_opts.find_opt('-big_move')
      if opt == None:
         ps.AlOpt.join(' -onepass')
      else:
         ps.AlOpt.join(' -twopass')
      
      #get anat and epi
      opt = self.user_opts.find_opt('-epi')
      if opt != None: 
         e = afni_name(opt.parlist[0]) 
      else:
         print "** ERROR: Must use -epi option\n"
         return 0
      ps.epi = e

      opt = self.user_opts.find_opt('-anat')
      if opt != None: 
         a = ps.anat0 = afni_name(opt.parlist[0])
      else:
         print "** ERROR: Must use -anat option\n"
         ps.ciao(1)
     
#      if ps.user_opts.find_opt('-fresh'):
#         ps.fresh_start(e.prefix, a.prefix)
         
      #epi input
      if not e.exist():
         print "** ERROR: Could not find epi dataset\n   %s " % e.input()
         ps.ciao(1)
         
      #anat input
      if not a.exist():
         print "** ERROR: Could not find anat dataset\n   %s " % a.input()
         ps.ciao(1)

      #get 3dTshift options
      opt = self.user_opts.find_opt('-tshift_opts')
      if opt != None: 
         ps.tshift_opt = string.join(opt.parlist, ' ')
      else:
         ps.tshift_opt = '-cubic'

      #get 3dvolreg options
      opt = self.user_opts.find_opt('-volreg_opts')
      if opt != None: 
         ps.reg_opt = string.join(opt.parlist, ' ')
      else:
         ps.reg_opt = ''

      #get epi base type for alignment (specific sub-brick/median/mean)
      opt = self.user_opts.find_opt('-epi_base')
      if opt != None: 
         ps.epi_base = opt.parlist[0]
      else:
         ps.error_msg("Must use -epi_base option")
         ps.ciao(1)
   
      #get volreg_base (matches epi_base by default)
      opt = self.user_opts.find_opt('-volreg_base')
      if opt != None: 
         ps.volreg_base = opt.parlist[0]
      else:
         ps.volreg_base = ps.epi_base

# may not need this and only the epi_base parameter instead
      #get 3dTstat options
      opt = self.user_opts.find_opt('-tstat_opts')
      if opt != None: 
         ps.tstat_opt = string.join(opt.parlist, ' ')
      else:
         ps.tstat_opt = ''

      #check for various center of mass options
      optc = self.user_opts.find_opt('-cmass')
      if optc == None :
         ps.cmass = 'cmass+xy'
         #if no  cmass option entered, partial coverage?
         opt = self.user_opts.find_opt('-partial_coverage')
         if opt != None:
            ps.cmass = 'nocmass'
      else:
         ps.cmass = optc.parlist[0]

      #get talairached anatomical dataset
      opt = self.user_opts.find_opt('-tlrc_apar')
      if opt != None: 
         anat_tlrc = afni_name(opt.parlist[0]) 
         ps.tlrc_apar = anat_tlrc
         if not anat_tlrc.exist():
            self.error_msg("Could not find anat talairach template dataset\n" \
                  "  %s " %  anat_tlrc.prefix)
         else:
            self.info_msg("Talairach transformed anatomical: %s" % \
                         (anat_tlrc.input()))
      else :
         ps.tlrc_apar = ""
      opt = self.user_opts.find_opt('-tlrc_epar')
      if opt != None: 
         at = afni_name(opt.parlist[0]) 
         ps.tlrc_epar = at
         if not at.exist():
            self.error_msg("Could not find epi talairach template dataset\n %s "
                  % at.input())
      else :
         ps.tlrc_epar = ""

      # check on the children
      ps.child_epis = self.user_opts.find_opt('-child_epi')
      if ps.child_epis != None: 
         self.info_msg("-child_epi option given")
         for child_epi_name in ps.child_epis.parlist:
            child_epi = afni_name(child_epi_name) 
            # it's 11:00, do you know where your children are?
            if not child_epi.exist():
               self.error_msg("Could not find child epi\n %s "
                     % child_epi.input())
            else:
               self.info_msg("Found child epi %s" % child_epi.input())
       
      opt = self.user_opts.find_opt('-master_epi')  # epi to anat resolution
      if opt != None: 
          self.master_epi_option = "-master %s" % opt.parlist[0]
      
          if(opt.parlist[0]=='MIN_DXYZ'):
              min_d =  self.min_dim_dset(ps.epi)
              self.master_epi_option = "-mast_dxyz %f" % min_d
              self.info_msg("Spacing for EPI to anat alignment is %f" % min_d)


      opt = self.user_opts.find_opt('-master_tlrc')  # epi to tlrc resolution
      if opt != None: 
          self.master_tlrc_option = "-master %s" % opt.parlist[0]
          if(opt.parlist[0]=='MIN_DXYZ'):
              min_d =  self.min_dim_dset(ps.epi)
              self.master_tlrc_option = "-mast_dxyz %f" % min_d
              self.info_msg("Spacing for EPI to tlrc alignment is %f" % min_d)
      else :   # default is to use smallest dimension
          min_d =  min_dim_dset(ps.epi)
          self.master_tlrc_option = "-mast_dxyz %f" % min_d
          self.info_msg("Spacing for EPI to tlrc alignment is %f mm" % min_d)

      # anat oblique resolution
      min_d =  self.min_dim_dset(ps.anat0)
      self.master_anat_option = "-newgrid %f" % min_d
      self.master_anat_3dAl_option = "-master SOURCE"  # if not oblique, just use original
      self.info_msg("Spacing for anat to EPI deobliquing is %f mm" % min_d)

      opt = self.user_opts.find_opt('-master_anat') 
      if opt != None: 
          if(opt.parlist[0]!='MIN_DXYZ'):
              if(isFloat(opt.parlist[0])):
                 min_d = float(opt.parlist[0])
                 self.master_anat_option = "-newgrid %f" % min_d
                 self.master_anat_3dAl_option = "-mast_dxyz %f" % min_d
                 self.info_msg("Spacing for anat to EPI obliquing is %f mm" % min_d)
              else:
                 self.master_anat_option = "-gridset %s" % opt.parlist[0]
                 self.master_anat_3dAl_option = "-master %s" % opt.parlist[0]
                 
      #get deobliquing options
      opt = self.user_opts.find_opt('-deoblique_opts')
      if opt != None: 
         ps.deoblique_opt = string.join(opt.parlist, ' ')
      else:
         ps.deoblique_opt = ''

      # user says it's okay to overwrite existing files 
      opt = self.user_opts.find_opt('-overwrite')
      if opt != None:
         ps.rewrite = 1


      # all inputs look okay  - this goes after all inputs. ##########
      return 1
  

   # find smallest dimension of dataset in x,y,z
   def min_dim_dset(self, dset=None) :
       com = shell_com(  \
                "3dAttribute DELTA %s" % dset.input(), ps.oexec,capture=1)
       com.run()
       if  ps.dry_run():
          return (1.234567)

       # new purty python way (donated by rick)
       min_dx = min([abs(float(com.val(0,i))) for i in range(3)])
       
       # old non-pythonesque way
       if 0:
          min_dx = 1000.0       # some high number to start
          i = 0
          while i < 3 :    # find the smallest of the 3 dimensions
             dx = abs(float(com.val(0,i)))
             if (dx<min_dx):
                 min_dx = dx
             i += 1    


       if(min_dx==0.0):
           min_dx = 1.0
       return (min_dx)
   
   # determine if dataset has time shifts in slices
   def tshiftable_dset( self, dset=None) :
       com = shell_com(  \
                "3dAttribute TAXIS_OFFSETS %s" % dset.input(), ps.oexec,capture=1)
       com.run()
       if  ps.dry_run():
           return (1)
       if(len(com.so)): status = 1
       else: status = 0
       return (status)
     
   # determine if dataset is oblique
   def oblique_dset( self, dset=None) :
      com = shell_com(  \
        "3dinfo %s | grep 'Data Axes Tilt:'|grep 'Oblique'" % dset.input(),\
          ps.oexec,capture=1)
      com.run()
      if  ps.dry_run():
          return (1)   # default for dry run is to assume oblique
      if(len(com.so)):
#        print("length of so is %d" % len(com.so))
#        oblstr = com.val(len(com.so),4)
#        if(oblstr=="Oblique"):
           self.info_msg( "Dataset %s is ***oblique****" % dset.input())
           return (1)
      self.info_msg( "Dataset %s is not oblique" % dset.input())
      return (0)  # if here, then not oblique

# align the anatomical data to the epi data using 3dAllineate
# this is the real meat of the program
# note for some of the reasoning:
# the output dataset is the result of the alignment of the anatomical to the EPI
# This is the default and the preferred output for several reasons despite 
# its not being standard to the usual processing in the past
# First, this does not require the EPI data to be resampled other than volume
# registration
# For medium to large differences of alignment, the EPI data resampled to its
# original grid may lose effective resolution. This is caused by the typical
# large difference in slice thickness relative to the EPI x,y voxel size within
# slice resolution.
# Secondly, the anatomical data is usually higher resolution and relatively
# isotropic in voxel dimensions
# Thirdly, the anatomical dataset is typically used for structural reference
# while the EPI voxel values matter for the analysis. Slight blurring of the
# anatomical data is relatively unimportant
# Forthly, the anatomical dataset's higher resolution allows for finer
# structural alignment (versus downsampling the anatomical to match the EPI)
# One could get around these various issues by using the inverse transform as
# done in the epi2anat method and then resampling the output grid to a finer
# resolution, but this will not usually be necessary.   
  
   def align_anat2epi(  self, e=None, a=None, m=None, \
                        alopt=" -onepass -weight_frac 1.0 -maxrot 6 " \
                               "-maxshf 10 -VERB -warp aff ",\
                        suf = "_alnd_epi", costfunction = "lpc"):
                        #m is the weight brick
      # for oblique data, use temporary data for alignment
      if(ps.obl_a2e_mat!="") :
         o = a.new("%s_temp%s" % (a.prefix, suf))
         self.anat_mat = "%s%s_a2e_only_mat.aff12.1D" %  (ps.anat0.out_prefix(),suf)
      else:
         o = a.new("%s%s" % (a.prefix, suf))
         self.anat_mat = "%s%s_mat.aff12.1D" %  (ps.anat0.out_prefix(),suf)
         
      ow = a.new("%s%s_wtal" % (a.prefix, suf))
      if (not o.exist() or ps.rewrite or ps.dry_run()):
         o.delete(ps.oexec)
         ow.delete(ps.oexec)
         if m:
            wtopt = "-wtprefix %s -weight %s" % (ow.prefix, m.input())
         else:
            wtopt = "-wtprefix %s " % (ow.prefix)
         if(ps.cmass==""):
            cmass = ""
         else:
            cmass = "-%s" % ps.cmass
            
         self.info_msg( "Aligning anatomical data to epi data")
         com = shell_com(  \
                 "3dAllineate -%s "  # costfunction       \
                  "%s "              # weighting          \
                  "-source %s -source_automask+4 "        \
                  "-prefix %s -base %s "                  \
                  "%s "  # center of mass options (cmass) \
                  "-1Dmatrix_save %s "                    \
                  "%s %s "  # master grid, other 3dAllineate options (may be user specified) \
                  % (costfunction, wtopt, a.input(), o.out_prefix(), e.input(),\
                    cmass, self.anat_mat, self.master_anat_3dAl_option, alopt), ps.oexec)
         com.run()

         if(ps.obl_a2e_mat!="") :
            o = a.new("%s%s" % (ps.anat0.prefix, suf)) # save the permanent data
            if (not o.exist() or ps.rewrite or ps.dry_run()):
               o.delete(ps.oexec)
            else:
               self.exists_msg(o.input)

            a2e_mat = "%s%s_mat.aff12.1D" %  (ps.anat0.out_prefix(),suf)
            # combine transformations
            com = shell_com(  \
                  "cat_matvec -ONELINE %s %s > %s" % \
                  (self.anat_mat, ps.obl_a2e_mat, a2e_mat), ps.oexec)
            com.run();
            self.info_msg( "Combining anat to epi and oblique transformations")
            com = shell_com(  \
                  "3dAllineate -base %s -1Dmatrix_apply %s " \
                  "-prefix %s -input %s  %s "   %  \
                  ( e.input(), a2e_mat, o.out_prefix(), ps.anat0.input(),\
                    self.master_anat_3dAl_option ), ps.oexec)

            com.run()
      else:
         self.exists_msg(o.input)

      if (not o.exist() and not ps.dry_run()):
         self.error_msg( "Could not square a circle " \
                         "(3dAllineate could not align anat to epi)")
         return None
      return o, ow
      
   # align the epi to the anatomical but do it using the inverse 
   # transformation of the alignment of anat to epi
   def align_epi2anat(  self, e=None, a=None, \
        alopt="",\
        suf = "_alnd_anat"):

      self.info_msg(" Aligning %s to anat" % e.input())

      o = afni_name("%s%s" % (self.epi.prefix, suf)) # was e.prefix here
      o.view = '+orig'

      if (not o.exist() or ps.rewrite or ps.dry_run()):
         o.delete(ps.oexec)
         # anat_mat = "%s%s_mat.aff12.1D" %  (ps.anat0.prefix,suf)
         epi_mat = "%s%s_mat.aff12.1D" %  (self.epi.prefix,suf)
         self.info_msg("Inverting anat to epi matrix")
         if(ps.obl_a2e_mat!="") :
            oblique_mat = "%s -I" % ps.obl_a2e_mat
         else :
            oblique_mat = ""
         
         com = shell_com(  \
                  "cat_matvec -ONELINE %s -I %s > %s" % \
                  (ps.anat_mat, oblique_mat, epi_mat), ps.oexec)
         com.run();
         
         # concatenate volume registration from epi data
         if(ps.volreg_flag):
            self.info_msg("Concatenating volreg and epi " \
                          "to anat transformations")
            epi_mat = "%s%s_reg_mat.aff12.1D" % (self.epi.prefix, suf)
            com = shell_com(  \
                     "cat_matvec -ONELINE %s -I %s %s> %s" % \
                     (ps.anat_mat, self.reg_mat, oblique_mat, epi_mat), ps.oexec)
            com.run();

# deobliquing epi can also be combined here
# apply with 3dAllineate, 3dWarp, 3dvolreg?
 
         self.info_msg( "Applying transformation of epi to anat")
         com = shell_com(  \
               "3dAllineate -base %s -1Dmatrix_apply %s " \
               "-prefix %s -input %s  %s "   %  \
               ( a.input(), epi_mat, o.out_prefix(), e.input(),\
                 self.master_epi_option), ps.oexec)
         
         com.run()
         
         # mark as not oblique if deobliqued
         if(oblique_mat!="") :
            com = shell_com ("3drefit -deoblique %s" % o.input(), ps.oexec)
            com.run()

#         if (not o.exist() and not ps.dry_run()):
#            self.error_msg("Could not apply transformation to epi data")
#            return None

         # concatenate talairach transformation
         if(self.tlrc_apar != ""):
            self.info_msg( "Concatenating talairach, volume registration," \
                           " epi to anat transformations")
            
            com = shell_com(  \
                     "3dAttribute WARP_TYPE %s" % \
                      self.tlrc_apar.input(), ps.oexec, capture=1)
            com.run();
            if(com.status != 0) :
               self.error_msg("Warp type not defined for this dataset: %s" % \
                         anat_tlrc.input())
               return o

            tlrc_type = int(com.val(0,0))
            if(tlrc_type != 0) :
               self.error_msg("Can not compute transformations for manually"
                         " talairached data")
               return o
               
            anat_tlrc_mat = "%s::WARP_DATA" % (self.tlrc_apar.input())

            epi_mat = "%s%s_tlrc_mat.aff12.1D" % (self.epi.prefix, suf)

            # note registration matrix, reg_mat, can be blank and ignored
            com = shell_com(  \
                   "cat_matvec -ONELINE %s %s -I %s %s > %s" % \
                   (anat_tlrc_mat, ps.anat_mat, self.reg_mat,oblique_mat,\
                     epi_mat), ps.oexec)
            com.run();

            tlrc_dset = afni_name("%s_tlrc%s" % (self.epi.prefix, suf))
            tlrc_dset.view = '+tlrc'
            # silliness because 3dAllineate makes +orig when applying matrix
            tlrc_orig_dset = afni_name("%s_tlrc%s" % (self.epi.prefix, suf))
            tlrc_orig_dset.view = '+orig'

            if tlrc_dset.exist():
               tlrc_dset.delete(ps.oexec)
            if tlrc_orig_dset.exist():
               tlrc_orig_dset.delete(ps.oexec)

            self.info_msg( "Applying transformation of epi to anat+tlrc")
            com = shell_com(  \
                  "3dAllineate -base %s -1Dmatrix_apply %s " \
                  "-prefix %s -input %s  -verb  %s"   %  \
                  ( a.input(), epi_mat, tlrc_dset.input(), e.input(), \
                    self.master_tlrc_option), ps.oexec)

            com.run()
            # deoblique dataset header info if already applied deobliquing
            if(oblique_mat!="") :
               deob_str = "-deoblique %s+orig" % tlrc_dset.input()
            else:
               deob_str = ""
            # 3dAllineate doesn't write out the correct view
            # so rename the files for AFNI BRIK, HEAD pairs
            if (tlrc_dset.type == 'BRIK'):
               com = shell_com ("3drefit %s -view tlrc %s+orig" %     \
                                deob_str, tlrc_dset.input(), ps.oexec)
               com.run()
      else:
         self.exists_msg(o.input)
            
#          if (not o.exist() and not ps.dry_run()):
#             self.error_msg("Could not apply tlrc transformation to epi data")
#             return None


      return o

   # reduce EPI dataset to a single representative sub-brick
   def tstat_epi(self, e=None, tstat_opt="", prefix = "temp_ts"  ):
      o = e.new(prefix)
      if (not o.exist() or ps.rewrite or ps.dry_run()):
         o.delete(ps.oexec)
         # if more than 1 sub-brick
         if (ps.dry_run() or \
            ((not ps.dry_run() and dset_dims(e.input())[3] > 1))):
         # could be: if number choose bucket else use that as stat
         # if((ps.epi_base=='median') or (ps.epi_base=='max') or \
         # (ps.epi_base=='mean')):   
         # choose a statistic as representative
            self.info_msg("Creating representative epi sub-brick")
            # if an integer, choose a single sub-brick
            if(ps.epi_base.isdigit()): 
            # if an integer, choose a single sub-brick
               com = shell_com(  \
               "3dbucket -prefix %s %s'[%s]'" % \
               (o.out_prefix(), e.input(), ps.epi_base) , ps.oexec)
            else:          
               com = shell_com(  \
               "3dTstat -%s -prefix %s %s" % \
               (ps.epi_base, o.out_prefix(), e.input()), ps.oexec)
         else:   # choose a single sub-brick (sub-brick 0)
            self.info_msg("using 0th sub-brick because only one found")
            com = shell_com(  \
            "3dbucket -prefix %s %s'[0]'" % (o.out_prefix(), e.input()), ps.oexec)
         com.run();
         if (not o.exist() and not ps.dry_run()):
            print "** ERROR: Could not 3dTstat epi"
            return None
      else:
         self.exists_msg(o.input)

            
      return o

   # deoblique epi dataset
   def deoblique_epi(self, e=None, deoblique_opt="", prefix="temp_deob"):
      o = e.new(prefix)  
      if (not o.exist() or ps.rewrite or ps.dry_run()):
         o.delete(ps.oexec)
         self.info_msg( "Deobliquing")
         com = shell_com(  \
               "3dWarp -deoblique -prefix %s %s %s "   %  \
               ( o.out_prefix(), deoblique_opt, e.input()), ps.oexec)
         com.run();
         if (not o.exist() and not ps.dry_run()):
            print "** ERROR: Could not deoblique epi data\n"
            return None
      else:
         self.exists_msg(o.input)

      return o

   # oblique anat to epi dataset
   # even if neither is really oblique, this shouldn't hurt
   def oblique_anat2epi(self,a=None,e=None,oblique_opt="",suffix="_ob"):
      o = a.new("%s%s" % (a.out_prefix(), suffix))
      self.obl_a2e_mat = "%s_obla2e_mat.1D" % a.out_prefix()
      if (not o.exist() or ps.rewrite or ps.dry_run()):
         o.delete(ps.oexec)
         self.info_msg( "Matching obliquity of anat to epi")
         com = shell_com(  \
               "3dWarp -verb -card2oblique %s -prefix %s %s %s %s " \
               "  | grep  -A 4 '# mat44 Obliquity Transformation ::'" \
               "  > %s " \
               % (e.input(), o.out_prefix(),        \
               self.master_anat_option, oblique_opt,\
               a.input(), self.obl_a2e_mat), ps.oexec)
         com.run();
         if (not o.exist() and not ps.dry_run()):
            print "** ERROR: Could not oblique anat to epi data\n"
            return None
      else:
         self.exists_msg(o.input)

      return o


   # do time shifting of EPI dataset
   def tshift_epi(  self, e=None, tshift_opt="-cubic", prefix="temp_tsh"):
      o = e.new(prefix)  
# why doesn't afni_name work here instead?

      if (not o.exist() or ps.rewrite or ps.dry_run()):
         o.delete(ps.oexec)
         self.info_msg( "Correcting for slice timing")
         com = shell_com(  \
               "3dTshift -prefix %s %s %s "   %  \
               ( o.out_prefix(), tshift_opt, e.input()), ps.oexec)
         com.run();
         if (not o.exist() and not ps.dry_run()):
            print "** ERROR: Could not do time shifting of epi data\n"
            return None
      else:
         self.exists_msg(o.input)

      return o

 
   # do volume registration of EPI dataset
   def register_epi(self, e=None, reg_opt="-quintic", prefix="temp_vr", \
                      childflag=0):
      o = e.new(prefix)

      if (not o.exist() or ps.rewrite or ps.dry_run()):
         o.delete(ps.oexec)
         # save the volreg output to file names based on original epi name
         #  (not temporary __tt_ names)
         self.mot_1D = "%s_motion.1D" % o.out_prefix()      # prefix
         self.reg_mat = "%s_mat.aff12.1D" % o.out_prefix()  # prefix
         self.info_msg( "Volume registration for epi data")
         # user option for which registration program (3dvolreg,3dWarpDrive,...)
         opt = self.user_opts.find_opt('-volreg_method')
         if opt != None: 
            vrcom = opt.parlist[0]
         else:
            vrcom = '3dvolreg'

         # find base for registration
         # could be: if number just use that as base
         # if((ps.volreg_base=='median') or (ps.volreg_base=='max') 
         #   or (ps.volreg_base=='mean')):   
         # choose a statistic as representative
         # if an integer, choose a single sub-brick
         if(childflag):
            base = "%s.'[%s]'"  %  (ps.epi.input(), ps.volreg_base)
         elif(ps.volreg_base.isdigit()): 
            base = "%s" % ps.volreg_base

         # otherwise median, mean or max
         else:          
           # if more than 1 sub-brick, compute stat, otherwise use 0th
           if 0:  # (dset_dims(e.ppve())[3] < 2 ):
              self.info_msg("Not enough sub-bricks to compute %s" % \
                             ps.volreg_base)
              base = "0"
           else:
              ots = e.new("%s_ts_tempalpha" % prefix)
              base = "%s.'[0]'" % ots.input()
              if (not ots.exist() or ps.rewrite):
                 ots.delete(ps.oexec)

              # compute stats, volreg, then recompute stats
              com = shell_com(  \
                "3dTstat -%s -prefix %s %s" % \
                (ps.volreg_base, ots.out_prefix(), e.input()), ps.oexec)
              com.run()

              if(not ots.exist() and not ps.dry_run()):
                 self.error_msg("Could not create intermediate data" \
                                "for time series registration")
                 ps.ciao(1)

              ovr_alpha = e.new("%s_vr_tempalpha" % prefix)

              com = shell_com(                                       \
                    "%s -prefix %s -base %s %s %s "  %               \
                ( vrcom, ovr_alpha.out_prefix(), base,               \
                  reg_opt, e.input()), ps.oexec)
              com.run()

              ots = e.new("%s_vrt" % prefix)
              base = "%s.'[0]'" % ots.input()
              if (not ots.exist() or ps.rewrite):
                 ots.delete(ps.oexec)

                 com = shell_com(  \
                   "3dTstat -%s -prefix %s %s" % \
                   (ps.volreg_base, ots.out_prefix(), ovr_alpha.input()), \
                   ps.oexec)
                 com.run()

              if(not ots.exist() and not ps.dry_run()):
                 self.error_msg("Could not create intermediate data" \
                                "for time series registration")
                 ps.ciao(1)

         com = shell_com(                                      \
               "%s -1Dfile %s -1Dmatrix_save %s "              \
               "-prefix %s -base %s %s %s "  %                 \
           ( vrcom, self.mot_1D, self.reg_mat, o.out_prefix(), base, \
             reg_opt, e.input()), ps.oexec)
         com.run()

         if (not o.exist() and not ps.dry_run()):
            self.error_msg( "Could not do volume registration")
            return None
      else:
         self.exists_msg(o.input)

      return o

   # resample EPI data to match higher resolution anatomical data
   def resample_epi(  self, e=None, resample_opt="", prefix="temp_rs"):
      o = self.epi.new(prefix)
      if (not o.exist() or ps.rewrite or ps.dry_run()):
         o.delete(ps.oexec)
         self.info_msg( "resampling epi to match anatomical data")
         com = shell_com(  \
               "3dresample -master %s -prefix %s -inset %s -rmode Cu" \
                  % (ps.anat_ns.input(), o.out_prefix(), e.input()), ps.oexec)
         com.run()
         if (not o.exist() and not ps.dry_run()):
            print "** ERROR: Could not resample\n"
            return None          
      else:
         self.exists_msg(o.input)

      return o
      
   # remove skull or outside brain area
   def skullstrip_data(self, e=None, use_ss='3dSkullStrip', \
       skullstrip_opt="", prefix = "temp_ns"):
      self.info_msg( "removing skull or area outside brain")
      if (use_ss == '3dSkullStrip'):     #skullstrip epi
         n = e.new(prefix)
         if (not n.exist() or ps.rewrite or ps.dry_run()):
            n.delete(ps.oexec)
            com = shell_com(  \
                  "3dSkullStrip -input %s -prefix %s" \
                  % (e.input(), n.out_prefix()) , ps.oexec)
            com.run()
            if (not n.exist() and not ps.dry_run()):
               print "** ERROR: Could not strip skull\n"
               return None
         else:
            self.exists_msg(n.input)
      elif use_ss == '3dAutomask': #Automask epi
         n = e.new(prefix)
         j = e.new("junk")
         if (not n.exist() or ps.rewrite or ps.dry_run()):
            n.delete(ps.oexec)
            com = shell_com(  \
                  "3dAutomask -prefix %s %s && 3dcalc -a %s "\
                  "-b %s -prefix %s -expr 'step(b)*a'" \
                  % (   j.out_prefix(), e.input(), e.input(), 
                        j.input(), n.out_prefix()), ps.oexec)
            com.run()
            if (not n.exist() and not ps.dry_run()):
               print "** ERROR: Could not strip skull with automask\n"
               return None
            j.delete(ps.oexec)
         else:
            self.exists_msg(n.input)
      else:
         n = e;
      return n

   # create weighting volume for matching (upper 10% by default)
   def create_weight(self, e, sq=1.0, box='no', \
                     binit = 'no', perci = -1.0, \
                     fati = -1, suf="_wt"):
   #e is a preprocessed epi, no skull
   # box, bin and fat mask are not used
      a = ps.anat_ns
      
      o = e.new("%s%s" % (e.prefix, suf))            
      if perci < 0:
         perci = 90.0;
      self.info_msg( "Computing weight mask")
      com_str = "3dBrickStat -automask -percentile %f 1 %f %s" \
                        % (perci, perci, e.input())
      if(not ps.dry_run()):
         com = shell_com( com_str, ps.oexec, capture=1)
         com.run()
         th = float(com.val(0,1))
         self.info_msg( "Applying threshold of %f on %s" % (th, e.input()))
      else:
         com = shell_com( com_str, "dry_run")
         com.run()
         th = -999
         self.info_msg( "Would be applying threshold for real run here")
            
      if sq == 1.0:
         com = shell_com( 
               "3dcalc -datum float -prefix %s "\
               "-a %s -expr 'min(1,(a/%f))'" \
               % (o.out_prefix(), e.input(), th), ps.oexec)
      else:
         com = shell_com( 
               "3dcalc -datum float -prefix %s -a "\
               "%s -expr 'min(1,(a/%f))**%f'" \
               % (o.out_prefix(), e.input(), th, sq), ps.oexec)

      if (not o.exist() or ps.rewrite or ps.dry_run()):
         o.delete(ps.oexec)
         com.run()
      else:
         self.exists_msg(o.input)
         
      if (not o.exist() and not ps.dry_run()):
         print "** ERROR: Could not create weight dset\n"
         return None
      
      return o
      

   # do the preprocessing of the EPI data    
   def process_epi(self, use_ss='3dSkullStrip', childflag=0):
      basesuff = ""
      basename = self.epi.prefix
      o = self.epi;
      if childflag:    # no temporary files for children
         prepre = ""
      else:
         prepre = "__tt_"
      suff = ps.suffix
 
      # time shift epi data, prepend a prefix
      if(self.tshift_flag):
         if(self.tshiftable_dset(o)) :
            basesuff = "%s_tsh" % basesuff
            prefix = "%s%s%s%s" % (prepre,basename,basesuff,suff)
            o = self.tshift_epi( o, ps.tshift_opt, prefix=prefix)
         else:
            self.info_msg("Can not do time shifting of slices. "
                          "Data is already time shifted")
      # if timeshifting was done, this will be the final epi dataset
      # and then the concatenated volreg, 3dAllineate transformations will be
      # applied
      tshift_o = o         
      # do volume registration
      if(self.volreg_flag):
         if(ps.dry_run() or \
	   (not ps.dry_run() and (dset_dims(o.input())[3] > 1))) :
             basesuff = "%s_vr" % basesuff
             prefix = "%s%s%s" % (basename,basesuff,suff) # don't use prepre
             o = self.register_epi( o, ps.reg_opt, prefix, childflag=childflag)
         else:
            self.info_msg("Skipping time series volume registration. "
                          "Must have more than a single sub-brick.")
            self.reg_mat = "" # children can be skipped too

      volreg_o = o

      # if just processing child epi datasets, just go home
      #   and skip reduction, resampling, skullstripping
      if(childflag):
         return tshift_o, volreg_o, volreg_o
 
      # reduce epi to a single representative sub-brick
      basesuff = "%s_ts" % basesuff
      prefix = "%s%s%s%s" % (prepre,basename,basesuff,suff)
      o = self.tstat_epi(o, ps.tstat_opt, prefix)

      # resample epi to match anat
      basesuff = "%s_rs" % basesuff
      prefix = "%s%s%s%s" % (prepre,basename,basesuff,suff)
      e = self.resample_epi( o,"", prefix)

      # remove outside brain or skull
      basesuff = "%s_ns" % basesuff
      prefix = "%s%s%s%s" % (prepre,basename,basesuff,suff)
      skullstrip_o = self.skullstrip_data( e, use_ss, "", prefix)

      return  tshift_o, volreg_o, skullstrip_o

   # do the preprocessing of the anatomical data
   def process_anat(self):
      #copy original anat to a temporary file
      self.anat = afni_name("__tt_%s" % self.anat0.prefix)
      self.anat.view = '+orig'
      
      if (not self.anat.exist() or ps.rewrite or ps.dry_run()):
         com = shell_com( "3dcopy %s %s" % \
           (self.anat0.input(), self.anat.out_prefix()), ps.oexec)
         com.run();
         if (not self.anat.exist() and not ps.dry_run()):
            print "** ERROR: Could not copy anat (%d)" % self.anat.exist()
            ps.ciao(1)
      else:
         self.exists_msg(self.anat.input)

      a = self.anat;

      #do we need to strip ?
      if(ps.skullstrip):
         n = a.new("%s_ns" % a.prefix, "+orig")  #don't use same type of input.
         if (not n.exist() or ps.rewrite or ps.dry_run()):
            n.delete(ps.oexec)
            self.info_msg( "Removing skull from anatomical data")
            com = shell_com(  \
                  "3dSkullStrip -input %s -prefix %s" \
                  % (a.input(), n.out_prefix()), ps.oexec)
            com.run()
            if (not n.exist() and not ps.dry_run()):
               print "** ERROR: Could not strip skull\n"
               return None
         else:
            self.exists_msg(o.input)
      else:
         n = a
         
      # match obliquity of anat to epi data
      if(self.deoblique_flag):
         # if either anat or epi is oblique, move anat to match epi
         if((self.oblique_dset(n)) or (self.oblique_dset(ps.epi))) :
            # set default output spacing if not already set with user options
            opt = self.user_opts.find_opt('-master_anat') 
            if opt == None: 
                min_d =  self.min_dim_dset(ps.anat0)
                self.master_3dAl_option = "-mast_dxyz %f" % min_d
                self.info_msg("Spacing for anat to oblique EPI alignment is %f" % min_d)
               
            n = self.oblique_anat2epi(n, ps.epi, ps.deoblique_opt)
      
      #do we need to shift?
#      optc = self.user_opts.find_opt('-align_centers')
#      if optc != None and optc.parlist[0] == 'yes': 

#         com = shell_com(  \
#               "@Align_Centers -base %s -dset %s" \
#               % (ps.epi.ppve(), n.ppve() ) , ps.oexec)
#         com.run() 
#         a_shft = n.new("%s_shft" % n.prefix,"+orig")
#         if (not a_shft.exist() and not ps.dry_run()):
#            print "** ERROR: Could not shift anat (%d)" % a_shft.exist()
#            return None
#         ps.anat_ns = a_shft
#      else:
      ps.anat_ns = n; 
      return 1

   # create final output files (non-temporary names)
   def create_output(self, aae, w, eaa, suf, epi_in=None, anat_in=None):

      #Create a properly named version of anatomy aligned to  EPI
      opt = ps.user_opts.find_opt('-anat')
      ain = afni_name(opt.parlist[0])
      opt = ps.user_opts.find_opt('-epi')
      ein = afni_name(opt.parlist[0])

      # save skull stripped anat
      if(ps.skullstrip):
         ao_ns = ain.new("%s_ns%s" % (ain.prefix, suf))
         self.copy_dset( ps.anat_ns, ao_ns, "Creating final output: skullstripped anatomical data", ps.oexec)

      # save anatomy aligned to epi 
      if (aae):
         # save aligned anatomy
         o = ain.new("%s%s" % (ain.prefix, suf))
         self.copy_dset( aae, o, "Creating final output: anat data aligned to epi data", ps.oexec)

         # save the timeshifted EPI data
         if (self.epi_ts and self.tshift_flag) :
            eo = epi_in.new("%s_tshft%s" % (ein.prefix, suf))
            self.copy_dset( self.epi_ts, eo,"Creating final output: time-shifted epi", ps.oexec)

         # save the volume registered EPI data
         if (self.epi_vr and self.volreg_flag):
            eo = epi_in.new("%s_vr%s" % (ein.prefix, suf))
            self.copy_dset( self.epi_vr, eo, \
              "Creating final output: time series volume-registered epi", ps.oexec)

      # save Allineate input datasets
      if(ps.save_Al_in):
         # save weight used in 3dAllineate
         if w:
            ow = ain.new("%s_wt_in_3dAl%s" % (ein.prefix,suf))
            self.copy_dset( w, ow, "Creating final output: weighting data", ps.oexec)

         #save a version of the epi as it went into 3dAllineate         
         if epi_in:
            eo = epi_in.new("%s_epi_in_3dAl%s" % (ein.prefix, suf))
            self.copy_dset( epi_in, eo,     \
            "Creating final output: epi representative data as used by 3dAllineate", ps.oexec)

         #save a version of the anat as it went into 3dAllineate
         if anat_in:  
            ao = epi_in.new("%s_anat_in_3dAl%s" % (ain.prefix, suf))
            self.copy_dset( anat_in, ao, "Creating final output: anat data as used by 3dAllineate", ps.oexec)

      #Now create a version of the epi that is aligned to the anatomy
      if (eaa):
         #save the epi aligned to anat
         o = e.new("%s%s" % (ein.prefix, suf))
         self.copy_dset( eaa, o, "Creating final output: epi data aligned to anat", ps.oexec)
       
      return

   # remove all the temporary files from a previous run
   # remove old results too optionally
   def fresh_start(self, epref="", apref="", rmold = 0 ):
      self.info_msg("Removing all the temporary files")
      if epref == "" and apref == "":
         com = shell_com(  "rm -f __tt_*", ps.oexec)
         com.run()     
      else:
         if epref != "":
            com = shell_com(  "rm -f __tt_%s*" % (epref), ps.oexec)
            com.run() 
            if(rmold):
               com = shell_com(  "rm -f %s*%s*" % (epref, ps.suffix), ps.oexec)
               com.run() 

         if apref != "":
            com = shell_com(  "rm -f __tt_%s*" % (apref), ps.oexec)
            com.run()  
            if(rmold):
               com = shell_com(  "rm -f %s*%s*" % (apref, ps.suffix), ps.oexec)
               com.run() 

      return


   # process children EPI as for parent but 
   #   no alignment of anat to EPI and no representative EPI
   # Do time shifting, time series volume registration
   def process_child_epi(self, childepi) :
      # do preprocessing of epi
      # do time shifting, volume registration of child epi data
      #   if requested; otherwise, just keep the input epi
      if(childepi.input()==ps.epi.input()) :    # skip the parent if it's included
         return                             #   in child list
      child = copy.copy(ps)      
      
      child.epi = childepi
      self.info_msg("Parent %s:  Child: %s" % (ps.epi.input(), child.epi.input()))
            
      child.epi_ts, child.epi_vr, child.epi_ns = \
          child.process_epi(childflag=1)
      if (not child.epi_ns):
         child.ciao(1)

      e = child.epi_ns
      a = ps.anat0       # use parent anat

      if(ps.prep_only):  # if preprocessing only, exit now
         ps.ciao(0)

      if (ps.epi2anat) :   # does the user want the epi aligned to the anat
         # compute transformation just from applying inverse
         child.epi_alnd = \
            child.align_epi2anat(child.epi_ts, a, ps.AlOpt, suf=ps.suffix)
         if (not child.epi_alnd):
            ps.ciao(1)
      else:
         child.epi_alnd = ''

      
# Main:
if __name__ == '__main__':


   ps = RegWrap('align_epi_anat.py')
   ps.init_opts()
   ps.version()
   rv = ps.get_user_opts()
   if (rv != None): ps.ciao(1) 
   
   #process and check input params
   if(not (ps.process_input())):
      ps.ciao(1)

   # get rid of any previous temporary data
   ps.cleanup()

   
   #Now process anatomy and epi
   if (not ps.process_anat()):
      ps.ciao(1)
   # do preprocessing of epi
   # final epi2anat option may use timeshifted data as basis
   ps.epi_ts, ps.epi_vr, ps.epi_ns = \
      ps.process_epi(use_ss=(ps.user_opts.find_opt('-epi_strip').parlist[0]))
   if (not ps.epi_ns):
      ps.ciao(1)
   
   e = ps.epi_ns
   a = ps.anat_ns
      
   #Create a weight for final pass
   ps.epi_wt = \
      ps.create_weight( e, float(ps.sqmask), ps.boxmask, \
                        ps.binmask, ps.perc, -1, suf = "_wt")
   if(ps.prep_only):  # if preprocessing only, exit now
      ps.ciao(0)
      
   #Do alignment to that pesky little epi
   ps.anat_alnd, ps.anat_alndwt = \
      ps.align_anat2epi(e, a, ps.epi_wt, ps.AlOpt, ps.suffix, ps.cost)
   if (not ps.anat_alnd):
      ps.ciao(1)
   if (ps.epi2anat) :   # does the user want the epi aligned to the anat
      # compute transformation just from applying inverse
      a = ps.anat0      # especially important for oblique datasets
      ps.epi_alnd = \
         ps.align_epi2anat(ps.epi_ts, a, ps.AlOpt, suf=ps.suffix)
      if (not ps.epi_alnd):
         ps.ciao(1)
   else:
      ps.epi_alnd = ''
      
   if (not ps.anat2epi):
      ps.anat_alnd = ''
      
   #Create final results
   ps.create_output(ps.anat_alnd, ps.anat_alndwt, ps.epi_alnd, \
        "%s" % ps.suffix, e, a)
   
   # process the children
   if ps.child_epis != None: 
      for child_epi_name in ps.child_epis.parlist:
         child_epi = afni_name(child_epi_name) 
         ps.process_child_epi(child_epi)

   #cleanup?
   if (ps.rmrm):
      ps.cleanup()

   print "\n# Finished alignment successfully"
   ps.ciao(0)
