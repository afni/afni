#!/usr/bin/env python


# note: in the script, runs are 1-based (probably expected)

import sys
from time import asctime

# AFNI modules
from afni_base import *
from afni_util import *
from option_list import *
from db_mod import *
import ask_me

g_help_string = """
    ===========================================================================
    This script is superceded by align_epi_anat.py. 
    Please use that program instead.
    ===========================================================================      
"""

old_g_help_string = """    
    This script is used to align an anatomical T1 to an epi (T2/T2*) volume.
    Alignment parameters are set presuming the two volumes to be in close
    alignment. 
    The script resamples and removes the skull off of anat and epi volumes. A
    3dAllineate command is then executed.
    
    Basic Usage:
      lpc_align.py -anat ANAT   -epi EPI   
    
    Extra Options:
      -big_move : indicates that large displacement is needed to align the
                  two volumes. This option is off by default.
      -partial_coverage: indicates that the EPI dataset covers a part of the 
                         brain.    
    
    The script outputs the following:
        ANAT_alepi: A version of the anatomy that is aligned to the epi 
        anat2epi.aff12.1D: A transformation matrix to align anatomy to the epi 
         You can use this transform or its inverse with programs such as
         3dAllineate -cubic -1Dmatrix_apply anat2epi.aff12.1D \\
                     -prefix ANAT_alman ANAT
         To align the EPI to the anatomy, first get the inverse of 
         anat2epi.aff12.1D with:
            cat_matvec -ONELINE anat2epi.aff12.1D -I > epi2anat.aff12.1D
         then use 3dAllineate:
         3dAllineate -cubic -1Dmatrix_apply epi2anat.aff12.1D  \\
                     -prefix EPI_alman EPI
        Also, since the input volumes are preprocessed before using 3dAllineate,
        the script outputs copies of the preprocessed volumes as they were used
        in 3dAllineate.
         _lpc.EPI : EPI volume for 3dAllineate's -base
         _lpc.ANAT: ANAT volume for 3dAllineate's -input
         _lpc.wt.EPI: weight volume for 3dAllineate's -weight
               
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
    @AddEdge. For each pair of volumes (E, A), @AddEdge finds the edges eE in E, 
    and eA in A, and creates a new version of E with both sets of edges. The 
    edges eE are given a low value, edges eA a higher value and the highest 
    value at voxels where eE and eA overlap. Although not all edges are 
    relevant, one can from experience focus on edges that are relevant. 
    Here is a simple example, where one can judge the improvement of alignment.
    Say we have anat+orig, epi+orig and we ran: 
      lpc_align.py -anat anat+orig -epi epi+orig 
    The relevant output of lpc_align.py is 
      anat_alepi+orig, _lpc.anat+orig, _lpc.epi+orig :
    To judge the improvement of alignment, we run:
      @AddEdge _lpc.epi+orig _lpc.anat+orig anat_alepi+orig 
      where the first option is the epi as passed to 3dAllineate. I recommend
      you use _lpc.epi+orig and _lpc.anat+orig rather than epi+orig and
      anat+orig, because edge enhancement is much better without skulls. 
      Anatomical volumes of various alignments with the EPI can be listed next.
      Here we are only examing pre- and post-lpc_align.py alignment.
    @AddEdge will create new, edge enhanced volumes with names starting by _ae. A
    new pair of volumes is created for each pair at input. Once done, @AddEdge
    proposes you run the following commands:
      afni -niml -yesplugouts &
      @AddEdge 
    With no options, @AddEdge will now cycle trough the pairs of (E,A)
    displaying an edge enhanced A in the backgroung and E in the foregound
    (colored). Assuming your colorscale is 'Spectrum:red_to_blue', the edges
    from the EPI are blue, edges from anatomical are orange and overlapping
    edges red. The script will open two slice viewers, navigate around to see
    how the contours match up. Remember that edges will not correspond perfectly
    and everywhere. Edges sometimes model different structures in different
    volumes. After all, if edges matched that well, we'd use them in
    registration! Look at the volumes closely and in different modes to
    appreciate what is being displayed. Although AFNI is being driven by the
    script, it is still fully interactive.
    @AddEdge then awaits user input at the shell to show the next pair of
    volumes. All you need is to hit enter, or enter the number of the pair 
    you want to examine next. A .jpg of the images is saved as you switch from
    one pair to the next. Cycling between one pair and the next, helps you
    appreciate which alignment is best.
    
    This script is still in very beta mode so please don't disseminate it to
    younguns. DO send us all the feedback you have and of course, let us know if
    it fails. We'll probably ask that you send us some data to look into it
    ourselves.
    
    Our abstract describing the alignment tools is available here:
      https://afni.nimh.nih.gov/sscc/rwcox/abstracts/file.2008-02-21.4176173435   
    
    ===========================================================================      
"""

## BEGIN common functions accross scripts (loosely of course)
class RegWrap:
   def __init__(self, label):
      self.label = label
      self.valid_opts = None
      self.user_opts = None
      self.verb = 0
      self.rewrite = 0 #Do not recreate existing volumes
      self.oexec = "" #dry_run is an option
      self.rmrm = 1
      self.prep_only = 0
      self.odir = os.getcwd()    
      
      return
      
   def init_opts(self):
      self.valid_opts = OptionList('init_opts')
       
      self.valid_opts.add_opt('-epi',  1, [])
      self.valid_opts.add_opt('-anat', 1, [])
      self.valid_opts.add_opt('-mask', -1, ['vent']) 
      self.valid_opts.add_opt('-keep_rm_files', 0, [])
      self.valid_opts.add_opt('-prep_only', 0, [])
      self.valid_opts.add_opt('-help', 0, [])
      self.valid_opts.add_opt('-verb', 1, [])
      self.valid_opts.add_opt('-align_centers', 1, ['no'], ['yes', 'no'])
      self.valid_opts.add_opt('-strip_anat_skull', 1, ['yes'], ['yes', 'no'])
      self.valid_opts.add_opt('-epi_strip', 1, ['3dSkullStrip'], \
                              ['3dSkullStrip', '3dAutomask', 'None'])
      self.valid_opts.add_opt('-pow_mask', 1, ['1.0'])
      self.valid_opts.add_opt('-bin_mask', 1, ['no'], ['yes', 'no'])
      self.valid_opts.add_opt('-box_mask', 1, ['no'], ['yes', 'no'])
      self.valid_opts.add_opt('-ex_mode', 1, ['echo'],   \
                              ['quiet', 'echo', 'dry_run'],  \
                              helpstr="Command execution mode.\n"\
                                       "quiet: execute commands quietly\n"\
                                       "echo: echo commands executed\n"\
                                       "dry_run: only echo commands\n" )
      self.valid_opts.add_opt('-big_move', 0, [])
      self.valid_opts.add_opt('-partial_coverage', 0, [])
      self.valid_opts.add_opt('-Allineate_opts', -1, \
                              ["-lpc -weight_frac 1.0 "\
                               "-VERB -warp aff "\
                               "-maxrot 6 -maxshf 10 "\
                               "-source_automask+4 "\
                               ], \
                               helpstr="Options passed to 3dAllineate.")
      self.valid_opts.add_opt('-perc', 1, ['50'])
      self.valid_opts.add_opt('-fresh', 0, [])
      self.valid_opts.add_opt('-suffix', 1,['_alepi'])
      self.valid_opts.trailers = 0   # do not allow unknown options
   
   def dry_run(self):
      if self.oexec != "dry_run":
         return 0
      else:
         return 1
         
   def apply_initial_opts(self, opt_list):
      opt = opt_list.find_opt('-verb')    # set and use verb
      if opt != None: self.verb = int(opt.parlist[0])
      
      opt = opt_list.find_opt('-ex_mode')    # set and use verb
      if opt != None: self.oexec = opt.parlist[0]

      opt = opt_list.find_opt('-keep_rm_files')    # set and use verb
      if opt != None: self.rmrm = 0

      opt = opt_list.find_opt('-prep_only')    # set and use verb
      if opt != None: self.prep_only = 1
            
      opt = opt_list.find_opt('-help')    # does the user want help?
      if opt != None:
         ps.self_help()
         ps.ciao(0)  # terminate
         
      opt = opt_list.find_opt('-perc')    # set and use verb
      if opt != None: self.perc = float(opt.parlist[0])
      
      opt = opt_list.find_opt('-suffix')    
      if opt != None: self.suffix = opt.parlist[0]
      
      opt = opt_list.find_opt('-pow_mask')    
      if opt != None: self.sqmask = opt.parlist[0]
      
      opt = opt_list.find_opt('-box_mask')    
      if opt != None: self.boxmask = opt.parlist[0]
      
      opt = opt_list.find_opt('-bin_mask')    
      if opt != None: self.binmask = opt.parlist[0]
      
   def get_user_opts(self):
      if len(sys.argv) != 2 and len(sys.argv) < 5:      # not enough options: apply -help
         print "** Need at least -epi and -anat options.\n"\
               "   Use -help option for details.\n" 
         return 1
      self.user_opts = read_options(sys.argv, self.valid_opts)
      if self.user_opts == None: return 1 #bad
      if self.user_opts.trailers:
         opt = self.user_opts.find_opt('trailers')
         if not opt: print "** seem to have trailers, but cannot find them!"
         else: print "** have invalid trailing args: %s" % opt.parlist
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
   
   def ciao(self, i):
      if i > 0:
         print "** script failed"
      elif i==0:
         print "\n"
         print ""

      os.chdir(self.odir)
      sys.exit()   

   def self_help(self):
      print g_help_string
      if 1: #not ready for this yet
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
               print "   %-20s   %s" % ("   default:" , string.join(opt.deflist,' '))
         
      return 1
   
   def cleanup(self):
      opt = self.user_opts.find_opt('-epi')
      e = afni_name(opt.parlist[0]) 
      
      opt = self.user_opts.find_opt('-anat')
      a = afni_name(opt.parlist[0])
      
      com = "rm -f __tt_%s*" % a.prefix
      shell_exec(com,ps.oexec)
      com = "rm -f __tt_%s*" % e.prefix
      shell_exec(com,ps.oexec)
      return 1

## BEGIN script specific functions   
   def process_input(self):
      #Do the default test on all options entered. 
      #NOTE that default options that take no parameters will not go 
      #through test, but that is no big deal
      for opt in self.user_opts.olist:
         if (opt.test() == None): ps.ciao(1)
      
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
      
      #partial coverage?
      opt = self.user_opts.find_opt('-partial_coverage')
      if opt == None:
         ps.AlOpt.join(' -cmass+xy')
      else:
         ps.AlOpt.join(' -nocmass')
            
      #get anat and epi
      opt = self.user_opts.find_opt('-epi')
      if opt != None: 
         e = afni_name(opt.parlist[0]) 
      else:
         print "Error: Must use -epi option\n"
         return None
      opt = self.user_opts.find_opt('-anat')
      if opt != None: 
         a = afni_name(opt.parlist[0])
      else:
         print "Error: Must use -anat option\n"
         return None
     
      if ps.user_opts.find_opt('-fresh'):
         ps.fresh_start(e.prefix, a.prefix)
         
      #epi input
      if not e.exist():
         print "Error: Could not find epi dataset\n   %s " % e.ppv()
         return None
      else: #copy it 
         ps.epi = afni_name("__tt_%s" % e.prefix)
         ps.epi.view = '+orig'
         if (not ps.epi.exist() or ps.rewrite):
            ps.epi.delete(ps.oexec)
            e.dims()
            if e.dims()[3] > 1:
               com = shell_com("3dTstat -median -prefix %s %s" % (ps.epi.pv(), e.ppve()), ps.oexec)
            else:
               com = shell_com("3dbucket -prefix %s %s'[0]'" % (ps.epi.pv(), e.ppve()), ps.oexec)   
            com.run()
            if (not ps.epi.exist() and not ps.dry_run()):
               print "Error: Could not copy epi"
               return None
      
      #anat input
      if not a.exist():
         print "Error: Could not find anat dataset\n   %s " % a.ppv()
         return None
      else: #copy it
         ps.anat = afni_name("__tt_%s" % a.prefix)
         ps.anat.view = '+orig'
         if (not ps.anat.exist() or ps.rewrite):
            ps.anat.delete(ps.oexec)
            com = "3dcopy %s %s" % (a.ppve(), ps.anat.pv())
            shell_exec(com, ps.oexec)
            if (not ps.anat.exist() and not ps.dry_run()):
               print "Error: Could not copy anat (%d)" % ps.anat.exist()
               return None
      #OK
      return 1
      
    
   def process_epi(self, use_ss='3dSkullStrip'):
      o = self.epi;
      
      #resample epi 
      e = o.new("%s_rs" % o.prefix, "+orig")
      if (not e.exist() or ps.rewrite):
         e.delete(ps.oexec)
         com = shell_com(  \
               "3dresample -master %s -prefix %s -inset %s -rmode Cu" \
                  % (ps.anat_ns.ppv(), e.prefix, o.ppv()), ps.oexec)
         com.run()
         if (not e.exist() and not ps.dry_run()):
            print "Error: Could not strip skull\n"
            return None
       
                     
      
      if (use_ss == '3dSkullStrip'):     #skullstrip epi
         n = e.new("%s_ns" % e.prefix, "+orig")
         if (not n.exist() or ps.rewrite):
            n.delete(ps.oexec)
            com = shell_com(  \
                  "3dSkullStrip -input %s -prefix %s" \
                  % (e.ppve(), n.prefix) , ps.oexec)
            com.run()
            if (not n.exist() and not ps.dry_run()):
               print "Error: Could not strip skull\n"
               return None
      elif use_ss == '3dAutomask': #Automask epi
         n = e.new("%s_ns" % e.prefix)
         j = e.new("junk")
         if (not n.exist() or ps.rewrite):
            n.delete(ps.oexec)
            com = shell_com(  \
                  "3dAutomask -prefix %s %s && 3dcalc -a %s "\
                  "-b %s -prefix %s -expr 'step(b)*a'" \
                  % (   j.prefix, e.ppv(), e.ppv(), 
                        j.ppv(), n.prefix), ps.oexec)
            com.run()
            if (not n.exist() and not ps.dry_run()):
               print "Error: Could not strip skull with automask\n"
               return None
            j.delete(ps.oexec)
      else:
         n = e;
               
      return  e, n
   
   def process_anat(self):
      a = self.anat;
      #do we need to strip ?
      optc = self.user_opts.find_opt('-strip_anat_skull')
      if optc != None and optc.parlist[0] == 'yes': 
         n = a.new("%s_ns" % a.prefix, "+orig")  #don't use same type of input.
         if (not n.exist() or ps.rewrite):
            n.delete(ps.oexec)
            com = shell_com(  \
                  "3dSkullStrip -input %s -prefix %s" \
                  % (a.ppve(), n.prefix), ps.oexec)
            com.run()
            if (not n.exist() and not ps.dry_run()):
               print "Error: Could not strip skull\n"
               return None
      else:
         n = a
      
      #do we need to shift?
      optc = self.user_opts.find_opt('-align_centers')
      if optc != None and optc.parlist[0] == 'yes': 
         com = shell_com(  \
               "@Align_Centers -base %s -dset %s" \
               % (ps.epi.ppve(), n.ppve() ) , ps.oexec)
         com.run() 
         a_shft = n.new("%s_shft" % n.prefix,"+orig")
         if (not a_shft.exist() and not ps.dry_run()):
            print "Error: Could not shift anat (%d)" % a_shft.exist()
            return None
         ps.anat_ns = a_shft           
      else:
         ps.anat_ns = n; 
      return 1

   def align_anat2epi(  self, e=None, a=None, m=None, \
                        alopt="", suf = "_alnd_epi"):  #m is the weight brick
      o = a.new("%s%s" % (a.prefix, suf))
      ow = a.new("%s%s_wtal" % (a.prefix, suf))
      if (not o.exist() or ps.rewrite):
         o.delete(ps.oexec)
         ow.delete(ps.oexec)
         if m:
            wtopt = "-wtprefix %s -weight %s" % (ow.prefix, m.ppv())
         else:
            wtopt = "-wtprefix %s " % (ow.prefix)
         com = shell_com(   "3dAllineate " \
                           "%s " \
                           "-source %s " \
                           "-1Dmatrix_save anat2epi.aff12.1D " \
                           "-prefix %s -base %s " \
                           "%s " \
                  % (wtopt, a.ppve(), o.prefix, e.ppv(), alopt), ps.oexec)
         com.run();
      if (not o.exist() and not ps.dry_run()):
         print "Error: Could not square a circle\n"
         return None
      return o, ow
            
   def create_weight(self, e, sq=1.0, box='no', \
                     binit = 'no', perci = -1.0, \
                     fati = -1, suf="_wt"):  #e is a preprocessed epi, no skull
      a = ps.anat_ns
      
      o = e.new("%s%s" % (e.prefix, suf))            
      if perci < 0:
         perci = 90.0;
      com = shell_com( "3dBrickStat -automask -percentile %f 1 %f %s" \
                        % (perci, perci, e.ppve()), ps.oexec, capture=1)
      com.run()
      if ps.oexec == "dry_run":
         th = -999;  # a flag for the dry_run
      else:
         th = float(com.val(0,1))
      print "Applying threshold of %f on %s" % (th, e.ppve())
      
      if sq == 1.0:
         com = shell_com( 
               "3dcalc -datum float -prefix %s "\
               "-a %s -expr 'min(1,(a/%f))'" \
               % (o.prefix, e.ppve(), th), ps.oexec)
      else:
         com = shell_com( 
               "3dcalc -datum float -prefix %s -a "\
               "%s -expr 'min(1,(a/%f))**%f'" \
               % (o.prefix, e.ppve(), th, sq), ps.oexec)
      if (not o.exist() or ps.rewrite):
         o.delete(ps.oexec)
         com.run()
         
      if (not o.exist() and not ps.dry_run()):
         print "Error: Could not create weight dset\n"
         return None
      
      return o
      
   def create_output(self, aae, w, suf, epi_in=None, anat_in=None):
      #Create a properly named version of anatomy aligned to  EPI
      opt = ps.user_opts.find_opt('-anat')
      a = afni_name(opt.parlist[0])
      o = a.new("%s%s" % (a.prefix, suf))
      opt = ps.user_opts.find_opt('-epi')
      ein = afni_name(opt.parlist[0])

      if (aae):
         o.delete(ps.oexec)
         #Really should be a rename, but it does not work well 
         # with the keep_rm_files option
         #com = "3drename %s %s" % (aae.pv(), o.prefix)
         com = shell_com(  \
               "3dcopy %s %s" % (aae.ppv(), o.pve()), ps.oexec)
         com.run()      
         if (not o.exist() and not ps.dry_run()):
            print "Error: Could not rename %s\n" % aae.ppv()
            return None
      if w:
         ow = o.new("_lpc.wt.%s" % (ein.prefix))
         ow.delete(ps.oexec)
         com = shell_com(  \
                  "3dcopy %s %s" % (w.ppv(), ow.pve()), ps.oexec)
         com.run()
      if epi_in:  #save a version of the epi as it went into 3dAllineate
         eo = epi_in.new("_lpc.%s" % (ein.prefix))
         eo.delete(ps.oexec)
         com = shell_com(  \
                  "3dcopy %s %s" % (epi_in.ppv(), eo.pve()), ps.oexec)
         com.run()
      if anat_in:  #save a version of the anat as it went into 3dAllineate
         opt = ps.user_opts.find_opt('-anat')
         ain = afni_name(opt.parlist[0])
         ao = epi_in.new("_lpc.%s" % (ain.prefix))
         ao.delete(ps.oexec)
         com = shell_com(  \
                  "3dcopy %s %s" % (anat_in.ppv(), ao.pve()), ps.oexec)
         com.run()
            
      #Now create a version of the epi that is aligned to the anatomy
      
      
      return o, ow

   def fresh_start(self, epref="", apref=""):
      if epref == "" and apref == "":
         com = shell_com(  "rm -f __tt_*", ps.oexec)
         com.run()           
      else:
         if epref != "":
            com = shell_com(  "rm -f __tt_%s*" % (epref), ps.oexec)
            com.run() 
         if apref != "":
            com = shell_com(  "rm -f __tt_%s*" % (apref), ps.oexec)
            com.run()  
      return
      
# Main:
if __name__ == '__main__':


   ps = RegWrap('lpc_align.py')
   print g_help_string
   ps.ciao(0)  # terminate
   ps.init_opts()

   rv = ps.get_user_opts()
   if (rv != None): ps.ciao(1) 

   
   #show options   
   #ps.show('A show of options:\n')
   
   #process and check input params
   if (not ps.process_input()):
      ps.ciao(1)

   
   #Now process anatomy and epi
   if (not ps.process_anat()):
      ps.ciao(1)
   ps.epi_rs, ps.epi_ns = \
      ps.process_epi(ps.user_opts.find_opt('-epi_strip').parlist[0])
   if (not ps.epi_ns):
      ps.ciao(1)
   
   e = ps.epi_ns
   a = ps.anat_ns
      
   #Create a weight for final pass
   ps.epi_wt = \
      ps.create_weight( e, float(ps.sqmask), ps.boxmask, \
                        ps.binmask, -1, -1, suf = "_wt")

      
   #Do alignment to that pesky little epi
   ps.anat_alnd, ps.anat_alndwt = \
      ps.align_anat2epi(e, a, ps.epi_wt, ps.AlOpt, "_alepi")
   if (not ps.anat_alnd):
      ps.ciao(1)
   #ps.anat_alnd_free, ps.anat_alndwt_free = \
   #   ps.align_anat2epi(e, a, None, ps.AlOpt, "_alepi_free")
      
   
   #Create final results
   ps.anat_alnd, ps.anat_alndwt = \
      ps.create_output(ps.anat_alnd, ps.anat_alndwt, "%s" % ps.suffix, e, a)
   
   #ps.anat_alnd_free, ps.anat_alndwt_free = \
   #   ps.create_output( ps.anat_alnd_free, ps.anat_alndwt_free, \
   #                     "%s_free" % ps.suffix, e, a)
      
   
   if (not ps.anat_alnd):
      ps.ciao(1)
   
      
   #cleanup?
   if (ps.rmrm):
      ps.cleanup()
      
   ps.ciao(0)
