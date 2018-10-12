#!/usr/bin/env python

# python3 status: started

import sys, os
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
    auto_warp.py     - Nonlinear regisration 
    
    Basic Usage:
      auto_warp.py -base TT_N27+tlrc -input anat.nii  \\
                   -skull_strip_input yes

    ---------------------------------------------
    REQUIRED OPTIONS:
    
    -base   : name of reference or template volume
    -input  : name of dataset to be registered
    
    MAJOR OPTIONS:

    -help       : this help message

    OTHER OPTIONS:

    -qblur bB bS : specify 3dQwarp blurs for base and source volumes
    -qworkhard i0 i1: set the two values for 3dQwarp's -workhard option
    -qw_opts 'OPTS': Pass all of OPTS as extra options directly to 3dQwarp 
"""   

## BEGIN common functions across scripts (loosely of course)
class RegWrap:
   def __init__(self, label):
      self.align_version = "0.04" # software version (update for changes)
      self.label = label
      self.valid_opts = None
      self.user_opts = None
      self.verb = 1    # a little talkative by default
      self.save_script = '' # save completed script into given file
      self.rewrite = 0 #Do not recreate existing volumes
      self.oexec = "" #dry_run is an option
      self.anat2temp = 1 # align anat to template by default
      self.rmrm = 1   # remove temporary files
      self.prep_only = 0  # do preprocessing only
      self.odir = os.getcwd()    
      self.resample_flag = 1 # do resample
      self.deoblique_flag = 1  # deoblique datasets first
      self.deoblique_opt = "" # deobliquing/obliquing options
      self.at_opt = "" # @auto_tlrc options
      self.qw_opts = "" # 3dQwarp extra options
      self.qworkhard = [0, 1]
      self.qblur = [-3, -3]# blurs for 3dQwarp
      self.output_dir = '' # user assigned path for anat and EPI
      
      return


   #Define and initialize defaults of user options
   def init_opts(self):
      self.valid_opts = OptionList('init_opts')
       
      self.valid_opts.add_opt('-base',  1, [], \
               helpstr="Template volume.")
               
      self.valid_opts.add_opt('-input',  1, [], \
               helpstr="dataset to be aligned to the template")

      self.valid_opts.add_opt('-keep_rm_files', 0, [], \
               helpstr="Don't delete any of the temporary files created here")
               
      self.valid_opts.add_opt('-prep_only', 0, [], \
               helpstr="Do preprocessing steps only without alignment")
               
      self.valid_opts.add_opt('-help', 0, [], \
               helpstr="The main help describing this program with options")
               
      self.valid_opts.add_opt('-limited_help', 0, [], \
               helpstr="The main help without all available options")
               
      self.valid_opts.add_opt('-option_help', 0, [], \
               helpstr="Help for all available options")
               
      self.valid_opts.add_opt('-version', 0, [], \
               helpstr="Show version number and exit")
               
      self.valid_opts.add_opt('-ver', 0, [], \
               helpstr="Show version number and exit")
               
      self.valid_opts.add_opt('-verb', 1, [], \
               helpstr="Be verbose in messages and options" )
               
      # 26 Nov 2012 [rickr]
      self.valid_opts.add_opt('-save_script', 1, [], \
               helpstr="save executed script in given file" )

      self.valid_opts.add_opt('-skip_affine', 1, ['no'], ['yes', 'no'],  \
               helpstr="Skip the affine registration process\n" \
                 "Equivalent to -affine_input_xmat ID \n" \
                 "(apply identity transformation)\n")
               
      self.valid_opts.add_opt('-skull_strip_base', 1, ['no'], ['yes', 'no'],\
               helpstr="Do not skullstrip base/template dataset")
      self.valid_opts.add_opt('-skull_strip_input', 1, ['no'], ['yes', 'no'],\
               helpstr="Do not skullstrip input dataset")

      self.valid_opts.add_opt('-ex_mode', 1, ['script'],                   \
                              ['quiet', 'echo', 'dry_run', 'script'],      \
                              helpstr="Command execution mode.\n"          \
                                       "quiet: execute commands quietly\n" \
                                       "echo: echo commands executed\n"    \
                                       "dry_run: only echo commands\n" )

      self.valid_opts.add_opt('-overwrite', 0, [],\
                               helpstr="Overwrite existing files")

      self.valid_opts.add_opt('-suffix', 1,['_al'])

      # child anat datasets
      self.valid_opts.add_opt('-child_anat', -1,[],\
                               helpstr="Names of child anatomical datasets")


      #resolutions for computing transforms

      # 16 Dec 2013 [rickr]
      self.valid_opts.add_opt('-qblur', 2, [],
             helpstr="3dQwarp base and source blurs (FWHM)\n")

      # June 2nd 2014 [ZSS]
      self.valid_opts.add_opt('-qw_opts', -1, [],
             helpstr="3dQwarp miscellaneous options.\n"  \
                     "Parameters will get passed directly to 3dQwarp.\n")
      self.valid_opts.add_opt('-qworkhard', 2, [0, 1],
             helpstr="3dQwarp -workhard values\n")
             
      self.valid_opts.add_opt('-warp_dxyz', 1,[0.0],\
             helpstr="Resolution used for computing warp (cubic only)\n")
      self.valid_opts.add_opt('-affine_dxyz', 1,[0.0],\
             helpstr="Resolution used for computing initial transform (cubic only)\n")
      self.valid_opts.add_opt('-affine_input_xmat', 1,deflist=['AUTO'],\
             helpstr="Affine transform to put input in standard space.\n"\
                     "Special values are:\n"\
                     "    'AUTO' to use @auto_tlrc\n"\
                     "    'ID' to do nothing\n"\
                     "    'FILE.1D' for a pre-computed matrix FILE.1D will\n"\
                     "              get applied to the input before Qwarping\n")
      self.valid_opts.add_opt('-smooth_anat', -1,[],\
             helpstr="Smooth anatomy before registration\n")
      self.valid_opts.add_opt('-smooth_base', -1,[],\
             helpstr="Smooth template before registration\n")
      self.valid_opts.add_opt(name='-unifize_input', npar = 1, deflist='yes',acplist=['yes','no'],\
             helpstr="To unifize or not unifize the input\n")
      self.valid_opts.add_opt('-output_dir', 1,['awpy'],\
             helpstr="Set directory for output datasets\n")
      
      self.valid_opts.add_opt('-followers', npar=-1, deflist=[],\
             helpstr="Specify follower datasets\n")
      self.valid_opts.add_opt('-affine_followers_xmat', npar=-1, deflist=[],\
             helpstr="Specify follower datasets' affine transforms\n")    

      self.valid_opts.add_opt('-skullstrip_opts', -1, [],
             helpstr="3dSkullstrip miscellaneous options.\n"  \
                     "Parameters will get passed directly to 3dSkullstrip.\n")

      self.valid_opts.add_opt('-at_opts', -1, [],
             helpstr="@auto_tlrc miscellaneous options.\n"  \
                     "Parameters will get passed directly to @auto_tlrc.\n")

      self.valid_opts.trailers = 0   # do not allow unknown options
        
  
   def dry_run(self):
      if self.oexec != "dry_run":
         return 0
      else:
         return 1
        
   def apply_initial_opts(self, opt_list):
      opt1 = opt_list.find_opt('-version') # user only wants version
      opt2 = opt_list.find_opt('-ver') 
      if ((opt1 != None) or (opt2 != None)):
         # ps.version()
         ps.ciao(0)   # terminate 
      opt = opt_list.find_opt('-verb')    # set and use verb
      if opt != None: self.verb = int(opt.parlist[0])

      opt = opt_list.find_opt('-save_script') # save executed script
      if opt != None: self.save_script = opt.parlist[0]
            
      opt = opt_list.find_opt('-ex_mode')    # set execute mode
      if opt != None: self.oexec = opt.parlist[0]

      opt = opt_list.find_opt('-keep_rm_files')    # keep temp files
      if opt != None: self.rmrm = 0

      opt = opt_list.find_opt('-prep_only')    # preprocessing only
      if opt != None: self.prep_only = 1
            
      opt = opt_list.find_opt('-help')    # does the user want help?
      if opt != None:
         ps.self_help(2)   # always give full help now by default
         ps.ciao(0)  # terminate

      opt = opt_list.find_opt('-limited_help')  # less help?
      if opt != None:
         ps.self_help()
         ps.ciao(0)  # terminate

      opt = opt_list.find_opt('-option_help')  # help for options only
      if opt != None:
         ps.self_help(1)
         ps.ciao(0)  # terminate
         
      opt = opt_list.find_opt('-suffix')    
      if opt != None: 
          self.suffix = opt.parlist[0]
          if((opt=="") or (opt==" ")) :
            self.error_msg("Cannot have blank suffix")
            ps.ciao(1);

      # 13 Dec, 2013 [rickr] - for Peter Molfese
      vals, rv = opt_list.get_type_list(float, '-qblur', length=2)
      if rv: ps.ciao(1)
      if vals != None: self.qblur = vals
      
      
      vals, rv = opt_list.get_type_list(float, '-qworkhard', length=2)
      if rv: ps.ciao(1)
      if vals != None: 
         if len(vals) == 2:
            self.qworkhard = vals
         else:
            ps.ciao(1)
         
      opt = opt_list.find_opt('-qw_opts')
      istr = '  '
      if opt != None:
         self.qw_opts = '    %s' % \
          ' '.join(UTIL.quotize_list(opt.parlist, '\\\n%s    '%istr, 1))
      
      opt = opt_list.find_opt('-warp_dxyz')    
      if opt != None: 
         self.warp_dxyz = float(opt.parlist[0])
         if (self.warp_dxyz != 0.0 and (self.warp_dxyz < 0.01 or self.warp_dxyz > 10.0)):
            self.error_msg("Bad value for -dxyz of %s" % opt.parlist[0]) 
            ps.ciao(1);
      else:
         self.error_msg("This should not happen for pre-defined options");
         ps.ciao(1)

      opt = opt_list.find_opt('-affine_dxyz')    
      if opt != None: 
         self.affine_dxyz = float(opt.parlist[0])
         if (self.affine_dxyz != 0.0 and (self.affine_dxyz < 0.01 or self.affine_dxyz > 10.0)):
            self.error_msg("Bad value for -dxyz of %s" % opt.parlist[0]) 
            ps.ciao(1);
      else:
         self.error_msg("This should not happen for pre-defined options");
         ps.ciao(1)
   
      opt = opt_list.find_opt('-unifize_input')
      self.unifize_input = opt_is_yes(opt)
 
      # any affine transform supplied (defaults to auto_tlrc call)     
      opt = opt_list.find_opt('-affine_input_xmat')
      self.affine_input_xmat = opt.parlist[0]

      # skip_affine is same as input of IDentity matrix
      opt = opt_list.find_opt('-skip_affine')
      if opt_is_yes(opt):
         self.affine_input_xmat = "ID"
      
      opt = opt_list.find_opt('-followers')
      if (opt == None):
         self.followers=[]
      else:
         self.followers = []
         for fol in opt.parlist:
            self.followers.append(afni_name(fol)) 
      opt = opt_list.find_opt('-affine_followers_xmat')
      if (opt == None):
         self.affine_followers_xmat=[]
      else:
         self.affine_followers_xmat = opt.parlist
      
      if (len(self.followers)):
         if (len(self.affine_followers_xmat) and \
             len(self.affine_followers_xmat)!=len(self.followers)):
            error_ex("followers and their transforms don't jive")
         if (len(self.affine_followers_xmat)==0):
            for kk in self.followers:
               self.affine_followers_xmat.append("ID")
      
   #Parse user options            
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
             print("** ERROR: seem to have trailers, but cannot find them!")
         else:
             print("** ERROR: have invalid trailing args: %s" % opt.parlist)
         return 1  # failure

      # apply the user options
      if self.apply_initial_opts(self.user_opts): return 1

      if self.verb > 3: 
         self.show('------ found options ------ ')    

      return
    
   def show(self, mesg=""):
      print('%s: %s' % (mesg, self.label))
      if self.verb > 2: self.valid_opts.show('valid_opts: ')
      self.user_opts.show('user_opts: ')
   
   def info_msg(self, mesg=""):
       if(self.verb >= 1) :
          print("#++ %s" % mesg)

   def error_msg(self, mesg=""):
       print("#**ERROR %s" % mesg)

   def error_ex(self, mesg=""):
       print("#**ERROR %s" % mesg)
       self.ciao(1)

   def exists_msg(self, dsetname=""):
       self.info_msg(mesg="** Dataset: %s already exists: Reusing." % dsetname);
       
       
   def ciao(self, i):
      if i > 0:
         print("** ERROR - script failed")
      elif i==0:
         print("")

      os.chdir(self.odir)

      if self.save_script:
         write_afni_com_history(self.save_script)

      sys.exit()   
      
   # save the script command arguments to the dataset history
   def save_history(self, dset, exec_mode):
      self.info_msg("Saving history")  # sounds dramatic, doesn't it?
      cmdline = args_as_command(sys.argv, \
                 '3dNotes -h "', '" %s' % dset.input())
      com = shell_com(  "%s\n" % cmdline, exec_mode)
      com.run()

   # show help
   # if help_level is 1, then show options help only
   # if help_level is 2, then show main help and options help
   def self_help(self, help_level=0):
      if(help_level!=1) :
         print(g_help_string)
      if(help_level):  
         print("A full list of options for %s:\n" % ps.label)
         for opt in self.valid_opts.olist:
            print("   %-20s" % (opt.name ))
            if (opt.helpstr != ''):
               print("   %-20s   %s" % \
                  ("   use:", opt.helpstr.replace("\n","\n   %-20s   "%' ')))
            if (opt.acceptlist):
               print("   %-20s   %s" % \
                  ("   allowed:" , ', '.join(opt.acceptlist)))
            if (opt.deflist):
               if type(opt.deflist[0]) != str:  # 31 Mar 2014 [rickr]
                  print("   %-20s   %s" % ("   default:",opt.deflist))
               else:
                  print("   %-20s   %s" % \
                     ("   default:",' '.join(opt.deflist)))
      return 1
   
   # remove all the temporary files for epi and anat base names
   def cleanup(self, rmold=0):
      opt = self.user_opts.find_opt('-output_dir')
      return 0

   def version(self):
      self.info_msg("auto_warp.py version: %s" % self.align_version)

   # copy dataset 1 to dataset 2
   # show message and check if dset1 is the same as dset2
   # return non-zero error if cannot copy
   def copy_dset(self, a, prefix):
      n = afni_name(prefix)
      if (not n.exist() or ps.rewrite or ps.dry_run()):
         n.delete(ps.oexec)
         com = shell_com(  \
               "3dcopy %s %s" \
               % (a.input(), n.input()) , ps.oexec)
         com.run()
         if (not n.exist() and not ps.dry_run()):
            print("** ERROR: Could not copy dset\n")
            ps.ciao(1)
      else:
         self.exists_msg(n.input())
      
      return n


## BEGIN script specific functions   
   def process_input(self):
      for opt in self.user_opts.olist:
         if (opt.test() == None): ps.ciao(1)

      # do not allow AFNI_COMPRESSOR (or avoid NIFTI)  10 Jun 2015 [rickr] */
      ename = 'AFNI_COMPRESSOR'
      if ename in os.environ:
         print('-- clearing %s ...' % ename)
         del os.environ[ename]

      opt = self.user_opts.find_opt('-skull_strip_input')
      if opt == None:
         self.error_ex("should not be empty")
      
      if opt.parlist[0] == 'no':
          ps.skullstrip_input = 0
      else:
          ps.skullstrip_input = 1
      
      
      opt = self.user_opts.find_opt('-skull_strip_base')
      if opt == None:
         self.error_ex("should not be empty")
      
      if opt.parlist[0] == 'no':
          ps.skullstrip_base = 0
      else:
          ps.skullstrip_base = 1
      
      #get anat and epi

      opt = self.user_opts.find_opt('-input')
      if opt == None: 
         self.error_ex("No -input");

      ps.input = afni_name(opt.parlist[0]) 
      if (not ps.input.exist()):
         self.error_ex("Could not find input")
      
      opt = self.user_opts.find_opt('-base')
      if opt == None: 
         self.error_ex("No -template");
      
      ps.base = afni_name(opt.parlist[0]) 
      ps.base.locate()
      if (not ps.base.exist()):
         self.error_ex("Could not find base")

      #get 3dSkullstrip options
      opt = self.user_opts.find_opt('-skullstrip_opts')
      if opt != None: 
         ps.skullstrip_opt = ' '.join(opt.parlist)
      else:
         ps.skullstrip_opt = ''

      #get auto_tlrc options
      opt = self.user_opts.find_opt('-at_opts')
      if opt != None: 
         ps.at_opt = ' '.join(opt.parlist)
      else:
         ps.at_opt = ''


      # user says it's okay to overwrite existing files 
      opt = self.user_opts.find_opt('-overwrite')
      if opt != None:
         ps.rewrite = 1

      opt = self.user_opts.find_opt('-output_dir') # set alternative output directory
      if opt != None: 
         self.output_dir = opt.parlist[0]
         self.output_dir = "%s/" % os.path.realpath(self.output_dir)
         print("# Output directory %s" % self.output_dir)

      com = shell_com(("mkdir %s" % self.output_dir), self.oexec)
      com.run()
      print("cd %s" % self.output_dir)
      if(not self.dry_run()):
         os.chdir(self.output_dir)
         
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
       
       if(min_dx==0.0):
           min_dx = 1.0
       return (min_dx)

        
   # resample EPI data to match higher resolution anatomical data
   def resample_epi(  self, e=None, resample_opt="", prefix="temp_rs", \
        subbrick=""):
      o = afni_name(prefix)
      if (not o.exist() or ps.rewrite or ps.dry_run()):
         o.delete(ps.oexec)
         self.info_msg( "resampling %s to match %s data" % \
           (ps.dset2_generic_name, ps.dset1_generic_name ))

         if (subbrick == ""):
             sb = ""
         else:
             if(subbrick.isdigit()): 
                sb = "[%s]" % subbrick
             else:
                sb = "[0]"
                             
         com = shell_com(  \
               "3dresample -master %s -prefix %s -inset %s'%s' -rmode Cu" \
                % (ps.anat_ns.ppv(), o.pp(), e.input(),sb), ps.oexec)
         com.run()
         if (not o.exist() and not ps.dry_run()):
            print("** ERROR: Could not resample\n")
            ps.ciao(1)          
      else:
         self.exists_msg(o.pve())

      return o
      
   # remove skull or outside brain area
   def skullstrip_data(self, e=None, use_ss='3dSkullStrip', \
                       skullstrip_opt="", prefix = "temp_ns"):
      self.info_msg( "removing skull or area outside brain")
      if (use_ss == '3dSkullStrip'):     #skullstrip epi
         n = afni_name(prefix)
         if (not n.exist() or ps.rewrite or ps.dry_run()):
            n.delete(ps.oexec)
            com = shell_com(  \
                  "3dSkullStrip -orig_vol %s -input %s -prefix %s" \
                  % (skullstrip_opt, e.input(), n.input()) , ps.oexec)
            com.run()
            if (not n.exist() and not ps.dry_run()):
               print("** ERROR: Could not strip skull\n")
               ps.ciao(1)
         else:
            self.exists_msg(n.input())
      elif use_ss == '3dAutomask': #Automask epi
         n = afni_name(prefix)
         j = afni_name("%s__tt_am_%s" % (n.p(),n.pve()))
         if (not n.exist() or ps.rewrite or ps.dry_run()):
            n.delete(ps.oexec)
            com = shell_com(  \
                  "3dAutomask -prefix %s %s && 3dcalc -a %s "\
                  "-b %s -prefix %s -expr 'step(b)*a'" \
                  % (   j.pp(), e.input(), e.input(), 
                        j.input(), n.pp()), ps.oexec)
            com.run()
            if (not n.exist() and not ps.dry_run()):
               print("** ERROR: Could not strip skull with automask\n")
               ps.ciao(1)
            j.delete(ps.oexec)
         else:
            self.exists_msg(n.input())
      else:
         n = e;
      return n
   
   #unifize?
   def unifize_data(self, a, prefix = "temp_un"):
      n = afni_name(prefix)
      if (not n.exist() or ps.rewrite or ps.dry_run()):
         n.delete(ps.oexec)
         com = shell_com(  \
               "3dUnifize -GM -input %s -prefix %s" \
               % (a.input(), n.input()) , ps.oexec)
         com.run()
         if (not n.exist() and not ps.dry_run()):
            print("** ERROR: Could not strip skull\n")
            ps.ciao(1)
      else:
         self.exists_msg(n.input())  
      return n
   
   # prepare input    
   def prepare_input(self, use_ss='3dSkullStrip'):
      prefix = "%s/anat.ns.nii" % (ps.output_dir)
      if (ps.skullstrip_input): 
         skullstrip_o = self.skullstrip_data(self.input, use_ss, ps.skullstrip_opt, prefix)
      else:
         skullstrip_o = self.copy_dset (self.input, prefix="%s/anat.nii" % (ps.output_dir))
      if (ps.unifize_input):
         prefix = "%s/anat.un.nii" % (ps.output_dir)
         skullstrip_o = self.unifize_data(skullstrip_o, prefix)
         
      return skullstrip_o

   # do the preprocessing of the anatomical data
   def prepare_base(self, use_ss='3dSkullStrip'):
      prefix = "%s/base.ns.nii" % (ps.output_dir)
      if (ps.skullstrip_base): 
         skullstrip_o = self.skullstrip_data( self.base, use_ss, ps.skullstrip_opt, prefix)
      else:
         skullstrip_o = self.copy_dset (self.base, prefix="%s/base.nii" % (ps.output_dir))
         
      return skullstrip_o 

   def resample(self,a,prefix='resampled', dxyz=0.0, m=None):
      n = afni_name(prefix)
      if (m == None):
         m = a
      if (not n.exist() or ps.rewrite or ps.dry_run()):
         n.delete(ps.oexec)
         if (dxyz != 0.0):
            com = shell_com(  \
                  "3dresample -inset %s -prefix %s -dxyz %f %f %f "\
                  "           -rmode Li -master %s" \
                  % (   a.input(),  n.input(), dxyz, dxyz, dxyz,
                        m.input()), ps.oexec)
         else:
            com = shell_com(  \
                  "3dresample -inset %s -prefix %s                "\
                  "           -rmode Li -master %s" \
                  % (   a.input(),  n.input(), 
                        m.input()), ps.oexec)
         com.run()
         if (not n.exist() and not ps.dry_run()):
            print("** ERROR: Could not strip skull with automask\n")
            ps.ciao(1)
      else:
         self.exists_msg(n.input())    
      return(n)
   
      
   def match_resolutions(self, a, b, suf, dxyz=0.0, m=None):
      if (dxyz != 0.0):
         print("%f" % (dxyz))
         ar = self.resample(a,prefix="anat%s.nii" % suf, dxyz=dxyz, m=m)
         br = self.resample(b,prefix="base%s.nii" % suf, dxyz=dxyz, m=m)
      else:
         ar = a
         br = b
         dxa = self.min_dim_dset(a)
         dxb = self.min_dim_dset(b)
         print("%f %f" % (dxa, dxb))
         if (dxb < dxa):
            br = self.resample(b,prefix="base%s.nii" % suf, dxyz=dxa, m=m)    
         if (dxa > dxb):
            ar = self.resample(a,prefix="anat%s.nii" % suf, dxyz=dxb, m=m)   
      
      com = shell_com(\
               "3dinfo -same_grid %s %s" % (ar.input(), br.input()), ps.oexec, capture=1)
      com.run()
      if (len(com.so) and int(com.so[0]) == 0):
         ar = self.resample(ar,prefix="anat%sb.nii" % suf ,m=br)

      return ar,br

   
   def align_auto_tlrc(  self, a=None, b=None,  \
                        alopt=" ",\
                        suf = ".aff", at_opt = "",
                        xmat=None):
                        #m is the weight brick
      self.info_msg( "Aligning %s data to %s data" % \
           (b.input(), a.input() ))
      n = a.new(new_pref="%s%s" % (a.prefix, suf), parse_pref=1)
      if (not n.exist() or ps.rewrite or ps.dry_run()):
         n.delete(ps.oexec)
         if (xmat==None):
            com = shell_com(  \
                   "@auto_tlrc -base   %s "      \
                   "          -input  %s "      \
                   "          -suffix %s "      \
                   "          -no_ss -no_pre"   \
                   "          -init_xform CENTER %s" \
                   % ( b.input(), a.input(), suf, at_opt ), \
                     ps.oexec)
         else:
            com = shell_com(  \
                   "3dWarp -matvec_out2in %s -prefix %s %s "      \
                   % ( xmat, n.input(), a.input() ), \
                     ps.oexec)
         com.run()
         if (not n.exist() and not ps.dry_run()):
            self.error_msg("Failed in affine step");
            ps.ciao(1)
         xmat = "%s.Xat.1D" % n.prefix
         if (not os.path.isfile(xmat) and not ps.dry_run()):
            self.error_msg("Failed to find xmat %s" % xmat);
            ps.ciao(1)
      else:
         self.exists_msg(n.input())
         if (xmat==None):
            xmat = "%s.Xat.1D" % n.prefix
             
      return n,xmat
   
   def qwarping(self, a=None, b = None, prefix=None):
      self.info_msg( "Aligning %s data to %s data" % \
           (b.input(), a.input() ))
      if (prefix==None):
         prefix = "%s.qw" % a.prefix   
      n = a.new(new_pref=prefix, parse_pref=1)
      if (not n.exist() or ps.rewrite or ps.dry_run()):
         n.delete(ps.oexec)
         if self.qworkhard[0] < 0:
            whopt = '-workhard'
         else: whopt = "-workhard:%d:%d" % (self.qworkhard[0], self.qworkhard[1])
         com = shell_com(  \
                "3dQwarp                                       "\
                "         -prefix %s                           "\
                "         -blur %s %s %s                       "\
                "         %s -base %s -source %s               "\
                % ( n.input(), self.qblur[0], self.qblur[1],
                    whopt,
                    self.qw_opts, b.input(), a.input()), ps.oexec)
         com.run()
         if (not n.exist() and not ps.dry_run()):
            self.error_msg("Failed in warping step");
            ps.ciao(1)
      else:
         self.exists_msg(n.input())  
      
      w = n.new(new_pref="%s_WARP" % n.prefix)
      
      return (n,w)
      
   def qwarp_applying(self, a, aff, wrp, prefix=None, dxyz=0.0, master=None):
      self.info_msg( "Applying warps to %s" % \
           ( a.input() ))
      if (prefix==None):
         prefix = "./%s.aw.nii" % a.prefix   
      n = afni_name(prefix)
      n.new_path("./")
      if (not n.exist() or ps.rewrite or ps.dry_run()):
         n.delete(ps.oexec)
         if (aff != "ID"):
            # waff = "-affter %s" % aff
            nwarp = '"%s %s"' % (wrp.input(), aff)      # 7 Nov, 2014 [rickr]
         else:
            # waff = ""
            nwarp = '%s' % wrp.input()
         if (dxyz==0.0):
            dxopt = ""
         else:
            dxopt = "-dxyz %f" % dxyz

         # warp datasets may need to grow, so allow for a different master
         if master == None: mast_str = "NWARP"
         else:              mast_str = "%s" % master.input()

         com = shell_com(  \
                "3dNwarpApply          "\
                "-nwarp %s             "\
                "-master %s            "\
                "     %s               "\
                "-source %s            "\
                "-prefix %s            "\
                % ( nwarp, mast_str, dxopt, a.input(), n.input()),
                    ps.oexec)
         com.run()
         if (not n.exist() and not ps.dry_run()):
            n.show()
            self.error_msg("Failed in warping step");
            ps.ciao(1)
      else:
         self.exists_msg(n.input())
      return n
      
   def align_epi_anat(self, e, a, aff):
      prefix = "%s_al" % e.prefix
      n = e.new(new_pref=prefix)
      n.new_path("./")
      affout = "%s_al_post_mat.aff12.1D" % e.prefix
      if (not os.path.isfile(affout) or not n.exist() or ps.rewrite or ps.dry_run()):
         n.delete(ps.oexec)
         if (aff != "ID"):
            waff = "-post_matrix %s" % aff
         else:
            waff = ""
         com = shell_com(  \
                "align_epi_anat.py                              "\
                "   -epi  %s   -epi_base 3  -epi2anat           "\
                "   -anat %s  -big_move                         "\
                "   -anat_has_skull no  -overwrite              "\
                "   %s                                          "\
                % ( e.input(), a.input(), waff), \
                ps.oexec)
         com.run()
         if (not os.path.isfile(affout) and not ps.dry_run()):
            self.error_msg("Failed in warping step");
            ps.ciao(1)
      else:
         self.exists_msg(n.input())
      return(affout)
         
# Main:
if __name__ == '__main__':


   ps = RegWrap('auto_warp.py')
   ps.init_opts()
   ps.version()
   rv = ps.get_user_opts()
   if (rv != None): ps.ciao(1) 
   
   #process and check input params
   if(not (ps.process_input())):
      ps.ciao(1)

   # get rid of any previous temporary data
   ps.cleanup()
   
   ps.anat_no_skull = a=ps.prepare_input()
   b=ps.prepare_base()
   if (ps.affine_input_xmat == 'AUTO'):   #@auto_tlrc
      if (ps.affine_dxyz != 0.0):
         a,b = ps.match_resolutions(a,b, '.rs', ps.affine_dxyz)
      #now run auto_tlrc
      a, ps.affine_input_xmat = ps.align_auto_tlrc(a,b, at_opt=ps.at_opt)
   elif (ps.affine_input_xmat == 'ID'): #input already in std space
      if (0): print("Nothing to do")
   else: #User specified matrix to take input to std space
      a, ps.affine_input_xmat = ps.align_auto_tlrc(a,b, xmat=ps.affine_input_xmat)
      
   #set resolutions properly now with master always the base
   a,b = ps.match_resolutions(a,b, '.rw', ps.warp_dxyz, m=b)
   
   #compute the warp 
   a, ps.warp_input_xform = ps.qwarping(a=a, b=b)
   
   #apply warps
   # warp datasets may grow, so pass base as master     26 Mar 2014 [rcr/zss]
   aw = ps.qwarp_applying(a=ps.input, aff=ps.affine_input_xmat,
                          wrp=ps.warp_input_xform, master=b)
   
   ps.save_history(aw, ps.oexec)
   
   #apply warps for all followers
   if (len(ps.followers)):
      k = 0
      for fol in ps.followers:
         if (ps.affine_followers_xmat[k] == 'AUTO'):
            ps.affine_followers_xmat[k] = ps.align_epi_anat(e=fol, a = ps.anat_no_skull, aff=ps.affine_input_xmat)
         ps.qwarp_applying(a=fol, aff=ps.affine_followers_xmat[k], wrp=ps.warp_input_xform, dxyz=2.5)
         k = k + 1
   
   #cleanup after the parents too?
   if (ps.rmrm):
      ps.cleanup()
            
   ps.ciao(0)
