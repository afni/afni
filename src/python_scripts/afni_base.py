#!/usr/bin/python
import os, sys, glob, operator, string, afni_base

class afni_name:
   def __init__(self, name=""):
      self.initname = name
      res = parse_afni_name(name)
      self.path = res['path']
      self.prefix = res['prefix']
      self.view = res['view']
      self.extension = res['extension']
      self.type = res['type']
      self.colsel = res['col']
      self.nodesel = res['node']
      self.rowsel = res['row']
      self.rangesel = res['range']
      return
   def p(self):   #Full path 
      pp = "%s/" % os.path.abspath('./')  #full path at this location
      fn = string.find(pp,self.path)      #is path at end of abspath?
      if (fn > 0 and fn+len(self.path) == len(pp)): #path is at end of abs path
         return pp
      else:
         return "%s/" % os.path.abspath(self.path)
   def ppve(self):
      s = "%s%s%s%s" % (self.p(), self.prefix, \
                         self.view, self.extension)
      return s
   def ppves(self):
      s = "%s%s%s%s'%s%s%s%s'" % (self.p(), self.prefix, \
                         self.view, self.extension,\
                         self.colsel, self.rowsel,\
                         self.nodesel, self.rangesel)
      return s
      
   def input(self):
      if self.type == 'BRIK':
         return self.ppv()
      else:
         return self.ppve() 

   def shortinput(self):
      if self.type == 'BRIK':
         return self.pv()
      else:
         return self.pve() 

   def out_prefix(self):
      if self.type == 'BRIK':
         return self.prefix
      else:
         return self.pve() 
   def ppv(self):
      s = "%s%s%s" % (self.p(), self.prefix, self.view)
      return s
   def rpv(self): # relative path, prefix, view (no leading './')
      rp = string.replace(self.path, "%s/" % os.path.abspath(os.curdir), '')
      s = "%s%s%s" % (rp, self.prefix, self.view)
      return s
   def rpve(self): # relative path, prefix, view (no leading './'), extension
      rp = string.replace(self.path, "%s/" % os.path.abspath(os.curdir), '')
      s = "%s%s%s%s" % (rp, self.prefix, self.view, self.extension)
      return s
   def pp(self):
      return "%s%s" % (self.p(), self.prefix)
   def pv(self):
      return "%s%s" % (self.prefix, self.view)
   def pve(self):
      return "%s%s%s" % (self.prefix, self.view, self.extension)
   def dims(self):
      return dset_dims(self.ppves())
   def exist(self):
      if (self.type == 'NIFTI'):
         if (     os.path.isfile("%s.nii" % self.ppv()) or \
                  os.path.isfile("%s.nii.gz" % self.ppv()) \
            ):
            return 1
         else: return 0
      elif (self.type == 'BRIK'):
         if (     os.path.isfile("%s.HEAD" % self.ppv()) \
               and  \
               (  os.path.isfile("%s.BRIK" % self.ppv()) or \
                  os.path.isfile("%s.BRIK.gz" % self.ppv()) or \
                  os.path.isfile("%s.BRIK.bz2" % self.ppv()) or \
                  os.path.isfile("%s.BRIK.Z" % self.ppv()) )   \
            ):
            return 1
         else: return 0
      elif (self.type == 'NIML'):
         if (     os.path.isfile("%s.niml.dset" % self.ppv()) \
            ):
            return 1
         else: return 0
      else:
         if (     os.path.isfile(self.ppve()) ):
            return 1
         else: return 0
   def delete(self, oexec=""): #delete files on disk!
      if (self.type == 'BRIK'):
         if os.path.isfile("%s.HEAD" % self.ppv()):
            shell_com("rm %s.HEAD" % self.ppv(), oexec).run()
         if os.path.isfile("%s.BRIK" % self.ppv()):
            shell_com("rm %s.BRIK" % self.ppv(), oexec).run()
         if os.path.isfile("%s.BRIK.gz" % self.ppv()):
            shell_com("rm %s.BRIK.gz" % self.ppv(), oexec).run()
         if os.path.isfile("%s.BRIK.bz2" % self.ppv()):
            shell_com("rm %s.BRIK.bz2" % self.ppv(), oexec).run()
         if os.path.isfile("%s.BRIK.Z" % self.ppv()):
            shell_com("rm %s.BRIK.Z" % self.ppv(), oexec).run()
      else:
         if os.path.isfile(self.ppve()):
            shell_com("rm %s" % self.ppve(), oexec).run()
      return
   def move_to_dir(self, path="", oexec=""):
      #self.show()
      #print path
      found = 0
      if os.path.isdir(path):
         if (self.type == 'BRIK'):
            if os.path.isfile("%s.HEAD" % self.ppv()):
               sv = shell_com("mv %s %s/" % (self.head(), path), oexec).run()
               found = found + 1
            if os.path.isfile("%s.BRIK" % self.ppv()):           
               sv = shell_com("mv %s %s/" % (self.brick(), path), oexec).run()
               found = found + 1
            if os.path.isfile("%s.BRIK.gz" % self.ppv()):
               sv = shell_com("mv %s %s/" % (self.brickgz(), path), oexec).run()
               found = found + 1         
            if os.path.isfile("%s.BRIK.bz2" % self.ppv()):
               sv = shell_com("mv %s %s/" % (self.brickbz2(), path), oexec).run()
               found = found + 1 
            if os.path.isfile("%s.BRIK.Z" % self.ppv()):
               sv = shell_com("mv %s %s/" % (self.brickZ(), path), oexec).run()
               found = found + 1 
            if (found > 0):
               self.new_path(path)
               if ( not self.exist() and oexec != "dry_run"):
                  print "Error: Move to %s failed" % (self.ppv())
                  return 0
            else:
               print "Error: Found no .HEAD or .BRIK or .BRIK.gz (or .bz2 or .Z) of %s" % (self.ppv())
               return 0
         else:
            if os.path.isfile("%s" % self.ppve()):
               sv = shell_com("mv %s %s/" % (self.ppve(), path), oexec).run()
               found = found + 1
            if (found > 0):
               self.new_path(path)
               if ( not self.exist() and oexec != "dry_run"):
                  print "Error: Move to %s failed" % (self.ppv())
                  return 0
            else:
               print "Error: Found no file %s to move." % self.ppve()
               return 0
      else:
         print "Error: Path %s not found for moving %s." % (path, self.ppv())
         return 0
      return 1
      
   def head(self):
      return "%s.HEAD" % self.ppv()
   def brick(self):
      return "%s.BRIK" % self.ppv()      
   def brickgz(self):
      return "%s.BRIK.gz" % self.ppv() 
   def brickbz2(self):
      return "%s.BRIK.bz2" % self.ppv() 
   def brickZ(self):
      return "%s.BRIK.Z" % self.ppv() 
   def new_path(self,path=""):
      #give name a new path
      if len(path) == 0:
         self.path = "%s/" % os.path.abspath("./")
      else:
         self.path = "%s/" % os.path.abspath(path)
   def new_prefix(self, prefix=""):
      self.prefix = prefix
   def new_view(self,view=""):
      self.view = view
   def show(self):
      print "AFNI filename:"
      print "   curdir  : %s" % os.path.abspath(os.curdir)
      print "   initial : %s" % self.initname
      print "   name    : %s" % self.ppve()
      print "   path    : %s" % self.path
      print "   prefix  : %s" % self.prefix   
      print "   view    : %s" % self.view
      print "   exten.  : %s" % self.extension
      print "   type    : %s" % self.type
      print "   On Disk : %d" % self.exist()
      print "   Row Sel : %s" % self.rowsel
      print "   Col Sel : %s" % self.colsel
      print "   Node Sel: %s" % self.nodesel
      print "   RangeSel: %s" % self.rangesel
      
   def new(self,new_pref='', new_view=''):  
      #return a copy of class member with new_prefix and new_view if needed
      an = afni_name()
      an.path = self.path
      if len(new_pref):
         an.prefix = new_pref
      else:
         an.prefix = self.prefix
      if len(new_view):
         an.view = new_view
      else:
         an.view = self.view
      an.extension = self.extension
      an.type = self.type
      return an
               
class comopt:
   def __init__(self, name, npar, defpar, acplist=[], helpstr=""):
      self.name = name
      self.i_name = -1      # index of option name in argv
      self.n_exp = npar     # Number of expected params, 0 if no params, 
                            #   -1 if any number > 0 is OK.
                            #   N if exactly N numbers are expected 
      self.n_found = -1     # Number of parameters found after parsing
                            #   0 means was on command line but had no params
      self.parlist = None   # parameter strings list following option 
      self.deflist = defpar # default parameter list,if any
      self.acceptlist = acplist # acceptable values if any
      self.required = 0     # is the argument required?
      self.helpstr = helpstr  # The help string
      return 

   def show(self, mesg = '', short = 0):
      print "%sComopt: %s" % (mesg, self.name)
      if short: return      # 22 Jan 2008 [rickr]

      print "  (i_name, n_exp, n_found) = (%d, %d, %d)" % \
               (self.i_name, self.n_exp, self.n_found)
      print "  parlist = %s" % self.parlist
      print "  deflist = %s" % self.deflist
      print "  acceptlist = %s" % self.acceptlist

   def test(self):
      if (len(self.deflist) != 0 and self.parlist == None):
         # some checks possible, parlist not set yet
         if self.n_exp >= 0:
            if len(self.deflist) != self.n_exp:
               print "Error: Option %s needs %d parameters\n" \
                     "Default list has %d parameters." \
                        % (self.name, self.n_exp, len(self.deflist))
               return None
         else:
            if len(self.deflist) < -self.n_exp:
               print "Error: Option %s needs at least %d parameters\n"  \
                     "Default list has %d parameters."\
                        % (self.name, -self.n_exp, len(self.deflist))
               return None 
      else :
         if self.n_exp >= 0:
            #print "option %s n_exp = %d, len(parlist)=%d" % (self.name, self.n_exp, len(self.parlist))
            #self.show()
            if len(self.parlist) != self.n_exp:
               print "Error: Option %s needs %d parameters\n" \
                     "Parameter list has %d parameters." \
                        % (self.name, self.n_exp, len(self.parlist))
               return None
         else:
            if len(self.parlist) < -self.n_exp:
               print "Error: Option %s needs at least %d parameters\n"  \
                     "Parameter list has %d parameters."\
                        % (self.name, -self.n_exp, len(self.parlist))
               return None 
      return 1

class shell_com:
   def __init__(self, com, eo="", capture=0):
      self.com = com    # command string to be executed
      self.eo = eo      # echo mode (echo/dry_run/script/"")
      self.dir = os.getcwd()
      self.exc = 0      #command not executed yet
      self.so = ''
      self.se = ''
      if (self.eo == "quiet"):
         self.capture = 1
      else:
         self.capture = capture; #Want stdout and stderr captured?
      #If command line is long, trim it, if possible
      l1 = len(self.com)
      if (l1 > 80):
         self.trimcom = self.trim()
         #if (len(self.com) < l1):
         #print "Command trimmed to: %s" % (self.com)
      else:
         self.trimcom = self.com
   def trim(self):
      #try to remove absolute path
      if self.dir[-1] != '/':
         tcom = string.replace(self.com, "%s/" % (self.dir), '')
      else:
         tcom = string.replace(self.com, self.dir, '')
      return tcom
   def echo(self): 
      if (len(self.trimcom) < len(self.com)):
         ms = " (command trimmed)"
      else:
         ms = ""
      if self.eo == "echo":
         print "#Now running%s:\n   cd %s\n   %s" % (ms, self.dir, self.trimcom)
         sys.stdout.flush()
      elif self.eo == "dry_run":
         print "#Would be running%s:\n  %s" % (ms, self.trimcom)
         sys.stdout.flush()
      elif (self.eo == "script"):
         print "#Script is running%s:\n  %s" % (ms, self.trimcom)
         sys.stdout.flush()
      elif (self.eo == "quiet"):
         pass
      
      if self.exc==1:
         print "#    WARNING: that command has been executed already! "
         sys.stdout.flush()
      return

   def run(self):
      self.echo()
      if(self.exc==1):
         return 0
      if(self.eo=="dry_run"):
         self.status = 0
         self.exc = 1
         return 0
      self.status, self.so, self.se = shell_exec2(self.trimcom, self.capture) 
      self.exc = 1
      return self.status
      
   def run_echo(self,eo=""):
      self.eo = eo;
      self.run()

   def stdout(self):
      if (len(self.so)):
         print "++++++++++ stdout:" 
         sys.stdout.flush()
         for ln in self.so:
            print "   %s" % ln
            sys.stdout.flush()
   def stderr(self):   
      if (len(self.se)):
            print "---------- stderr:" 
            sys.stdout.flush()
            for ln in self.se:
               print "   %s" % ln
   def out(self):
      if self.exc:
         self.stdout()
         self.stderr()
      else:
         print "#............. not executed."
         sys.stdout.flush()
   
   def val(self, i, j=-1): #return the jth string from the ith line of output. if j=-1, return all ith line
      if not self.exc:
         print "Error: Command not executed"
         return None
      elif self.eo == "dry_run":
         return "0"  #Just something that won't cause trouble for places expecting numbers
      elif len(self.so) == 0:
         print "Error: Empty output."
         return None
      elif len(self.so) <= i:
         print "Error: First index i=%d >= to number of elements (%d) in output " %  \
               (i, len(self.so))
         return None
      
      if j>= 0:
         l = string.split(self.so[i])
         if len(l) <= j:
            print "Error: Second index j=%d is >= to number of elements (%d) in %dth line of output" % \
                     (j, len(l), i)
            return None
         else:
            return l[j]
      else:
         return self.so[i]


# return the attribute list for the given dataset and attribute
def read_attribute(dset, atr):
    [so, se] = shell_exec('3dAttribute %s %s' % (atr, dset))
    if len(so) == 0:
        print '** 3dAttribute exec failure for "%s %s"' % (atr, dset)
        if len(se) > 0: print se
        return None
    list = so[0].split()
    if len(list) > 0: return list
    else:
        print '** 3dAttribute failure for "%s %s":' % (atr, dset)
        return None

# return dimensions of dset, 4th dimension included
def dset_dims(dset):
   dl = [-1 -1 -1 -1]
   if 0: #This approach fails with selectors!
      ld = read_attribute(dset, 'DATASET_DIMENSIONS')
      lr = read_attribute(dset, 'DATASET_RANK')
      dl = []
      for dd in ld[0:3]:
         dl.append(int(dd))
      dl.append(int(lr[1]))
   else:
      com = shell_com('3dnvals -all %s' % dset, capture=1);
      if (com.run()):
         print '** failed to get dimensions.'
      dl = [int(com.val(0,0)), int(com.val(0,1)), int(com.val(0,2)), int(com.val(0,3))]   
   return dl
   

#transform a list of afni names to one string for shell script usage
def anlist(vlst, sb=''):
   namelst = []
   if len(sb):
      sbs = "'%s'" % sb
   else:
      sbs = ''
   for an in vlst:
      namelst.append("%s%s" % (an.ppv(), sbs))    
   return string.join(namelst,' ')


#parse options, put into dictionary
def getopts(argv):
   opts = {}
   while argv:
      if argv[0][0] == '-':
         opts[argv[0]] = argv[1]
         argv = argv[2:]
      else:
         argv = argv[1:]
   return opts

def show_opts2(opts):
   if opts == None:
      print "Option dictionary is None\n"
      return
   print opts
   for key in opts.keys():
      print "Option Name: %s" % key
      print "       Found: %d" % opts[key].n_found
      print "       User Parameter List: %s" % opts[key].parlist
      print "       Default Parameter List: %s\n" % opts[key].deflist
   return
   
def getopts2(argv,oplist):
   """ A function to parse command line arguments.
   to use it, you need to set up the options list.
   So, from a main you can do the following:

   oplist = []
   # an option that needs no params
   oplist.append(afni_base.comopt('-dicom', 0, []))  
   # an option that needs 2 params, with 2 options, defaulting to 2 and 10.0
   oplist.append(afni_base.comopt('-clust', 2, ['2', '10.0']))  
   # an option that needs an undetermined number of parameters
   # (-1 for 1 or more, -2 for 2 or more)
   oplist.append(afni_base.comopt('-dsets', -1, []))
   
   once the list is made, you call getopts2 with argv and oplist
   
   opts = afni_base.getopts2(sys.argv, oplist)
   
   opts is a dictionary with the name of oplist elements as keys
   
   to get a quick look at it use:
   afni_base.show_opts2(opts) """
   opts = {}
   if len(argv) == 0:
      return opts
   #Add the program name
   op = comopt('basename',0, [])
   opts['basename'] = op
   argv.remove( argv[0] )
   
   #form a list of the known options
   optnames = []
   for op in oplist:
      optnames.append(op.name)

   #find those options in oplist
   for op in oplist:
      if op.name in argv:
         op.n_found = 0          #found that argument
         op.iname = argv.index(op.name)   #copy index into list
         argv.remove(op.name)             #remove this option from list
         op.parlist = []
         if op.n_exp < 0 or op.n_exp > 0: #parameters expected, get them
            while ((op.n_exp < 0 and op.iname < len(argv)) or \
               (op.n_exp > 0 and len(op.parlist) < op.n_exp and len(argv) > 0))\
                 and argv[op.iname] not in optnames:
               if len(op.acceptlist):
                  if argv[op.iname] not in op.acceptlist:
                     print "Error: parameter value %s for %s is not "   \
                           "acceptable\nChoose from %s" %               \
                           (argv[op.iname], op.name,                    \
                           string.join(op.acceptlist, ' , '))
               op.parlist.append(argv[op.iname]) #string added
               argv.remove(argv[op.iname])       #remove this string from list          
            op.n_found = len(op.parlist)
               
      else : #No option in argv, just copy option
         op.parlist = op.deflist
      
      #Now copy results to dictionary
      opts[op.name] = op #a bit of redundancy, but I don't care
      
      if (op.test() == None):
         afni_base.show_opts2(opts)
         return None
         
         
   #Any remaining?
   for op in oplist:
      if op.name == 'loose':  #Expecting loose params
         if op.n_exp < 0 or op.n_exp > 0: #parameters expected, get them
            op.parlist.extend(argv)    #stick'em all in
            opts[op.name] = op
            if op.n_exp > 0 and len(op.parlist) != op.n_exp:
               print "Error: Expecting %d parameters\n" \
                     "Have %d on command line (%s).\n" % \
                     (op.n_exp, len(op.parlist), op.parlist)
               return None
      elif len(argv) > 0:
         print "Error: Expecting no loose parameters.\n"        \
               "Have %d loose parameters (or bad option) on "   \
               "command line (%s).\n" % (len(argv), argv)
         return None
   
   #go west young man
   return opts        
                     
#Strip the extensions from extlist out of name
#returns name without the extension and the extension
#found, if any.
#Example: 
# res = strip_extension('Hello.Jim', ['.paul', '.Jim'])
# --> res[0] = 'Hello'
# --> res[1] = '.Jim'
#
# To remove anything after the last 'dot'
# res = strip_extension('Hello.Jim', [])
# 
def strip_extension(name, extlist):
   res = {}
   nle = len(name)
   if len(extlist):
      while extlist:
         xle = len(extlist[0])
         if nle > xle:
            if name[-xle:] == extlist[0]:
               res[0] = name[:-xle]
               res[1] = extlist[0]
               return res
         #else:
            #nada
            #print name
         #Go to next element
         extlist = extlist[1:]
   else: #Nothing specified, work the dot
      spl = string.split(name,'.')
      if len(spl) > 1:
         res[0] = string.join(spl[0:-1],'.')
         res[1] = '.'+spl[-1]
         return res
         
   #defaults
   res[0] = name
   res[1] = ''
   return res


#parse an afni name
def parse_afni_name(name):
   res = {}
   #get the path  #Can also use os.path.split
   rp = os.path.dirname(name) #relative path
   ap = os.path.abspath(rp) #absolute path
   fn = os.path.basename(name)
   #Get selectors out of the way:
   res['col'], res['row'], res['node'], res['range'], fn = afni_selectors(fn)
   
   #is this a .nii volume?
   rni = strip_extension(fn,['.nii', '.nii.gz'])
   if (len(rni[1]) > 0):
      vi = ''  #No view
      ex = rni[1]
      pr = rni[0]
      tp = 'NIFTI'
   else: 
      rni = strip_extension(fn,['.HEAD','.BRIK','.BRIK.gz','.BRIK.bz2','.BRIK.Z','.1D', '.',  \
                                '.1D.dset', '.niml.dset'])
      ex = rni[1]
      if (ex == '.1D' or ex == '.1D.dset'):
         tp = "1D"
      elif (ex == '.niml.dset'):
         tp = "NIML"
      else:
         tp = 'BRIK'
      if (ex == '.'):
         ex = ''  #dump the dot
      rni = strip_extension(rni[0], ['+orig','+tlrc','+acpc'])
      vi = rni[1]
      pr = rni[0]
   #get selectors 
   
   #Build the dictionary result
   if len(rp) == 0:
      rp = '.'
   res['path'] = "%s/" % ap   #A world of trouble when relative path is used. So use ap instead April 08
   res['prefix'] = pr
   res['view'] = vi
   res['extension'] = ex
   res['type'] = tp
   return res

#utilitiarian laziness
def afni_prefix(names):
   pref = []
   for run in range(0, len(names)):
      res = parse_afni_name(names[run])
      pref.append(res['prefix'])
   return pref
      
def afni_view(names):
   pref = []
   for run in range(0, len(names)):
      res = parse_afni_name(names[run])
      pref.append(res['view'])
   return pref

def afni_selectors(names):
   sqsel = ""
   cusel = ""
   pnsel = ""
   ltsel = ""
   namestr = names
   
   nse = names.count('[')
   if (nse == 1 and nse == names.count(']')):
      sqsel = names[names.find('['):names.find(']')+1]
      namestr = namestr.replace(sqsel,'')
   
   nse = names.count('{')
   if (nse == 1 and nse == names.count('}')):
      cusel = names[names.find('{'):names.find('}')+1]
      namestr = namestr.replace(cusel,'')
   
   nse = names.count('<')
   if (nse == 1 and nse == names.count('>')):
      ltsel = names[names.find('<'):names.find('>')+1]
      namestr = namestr.replace(ltsel,'')
   
   nse = names.count('#')
   if (nse == 2):
      nf = names.find('#')
      pnsel = names[nf[0]:nf[1]+1]
      namestr = namestr.replace(pnsel,'')
   
   return sqsel, cusel, pnsel, ltsel, namestr
   
#execute a shell command and when capture is 1 returns results in:
# so (stdout) and se (stderr) and status
#status is only reliable with python versions 2.5 and above
def shell_exec(s,opt="",capture=1):
   #opt is left here for backwards compatibility.
   #no echoing should be done here. It is better
   #to use the shell_com objects
   if opt == "dry_run":
      print "#In %s, would execute:\n   %s" % (os.getcwd(), s)
      sys.stdout.flush()
   elif opt == "echo":
      print "#In %s, about to execute:\n   %s" % (os.getcwd(), s)   
      sys.stdout.flush()
      
   status, so, se = shell_exec2(s,capture)
   return so, se
   
def shell_exec2(s, capture=0):

   vs = sys.version.split()[0]
   v = vs.split('.')
   if len(v) > 1:
      vs = "%s.%s" % (v[0], v[1])
   else:
      vs = v[0]

   v = float(vs)

   if (v < 2.5): #Use old version and pray
      #if there is no capture in option: run os.system
      if(not capture):
         os.system("%s"%s)
         status = 0; #Don't got status here 
         so = ""
         se = ""
      else:
         i,o,e = os.popen3(s) #captures stdout in o,  stderr in e and stdin in i      
         #The readlines seems to hang below despite all the attempts at limiting the size
         #and flushing, etc. The hangup happens when a program spews out a lot to stdout
         #So when that is expected, redirect output to a file at the command.
         #Or use the "script" execution mode
         so = o.readlines(64)  #default is to read till EOF but that might make python hang 
         se = e.readlines(64)  # output to stdout and stderr is too large.
         o.close             #Have tried readlines(1024) and (256) to little effect 
         e.close             #
         status = 0; #Don't got status here 
   else:
      import subprocess as SP
      if(not capture):
         pipe = SP.Popen(s,shell=True, executable='/bin/tcsh', stdout=None, stderr=None, close_fds=True)
         status = pipe.wait() #Wait till it is over and store returncode
         so = ""
         se = ""
      else:
         pipe = SP.Popen(s,shell=True, stdout=SP.PIPE, stderr=SP.PIPE, close_fds=True)
         o,e = pipe.communicate()   #This won't return until command is over
         status = pipe.returncode   #NOw get returncode
         so = o.splitlines()
         se = e.splitlines()                           

   return status, so, se
   
#generic unique function, from:
#  http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/52560/index_txt
def unique(s):
    """Return a list of the elements in s, but without duplicates.

    For example, unique([1,2,3,1,2,3]) is some permutation of [1,2,3],
    unique("abcabc") some permutation of ["a", "b", "c"], and
    unique(([1, 2], [2, 3], [1, 2])) some permutation of
    [[2, 3], [1, 2]].

    For best speed, all sequence elements should be hashable.  Then
    unique() will usually work in linear time.

    If not possible, the sequence elements should enjoy a total
    ordering, and if list(s).sort() doesn't raise TypeError it's
    assumed that they do enjoy a total ordering.  Then unique() will
    usually work in O(N*log2(N)) time.

    If that's not possible either, the sequence elements must support
    equality-testing.  Then unique() will usually work in quadratic
    time.
    """

    n = len(s)
    if n == 0:
        return []

    # Try using a dict first, as that's the fastest and will usually
    # work.  If it doesn't work, it will usually fail quickly, so it
    # usually doesn't cost much to *try* it.  It requires that all the
    # sequence elements be hashable, and support equality comparison.
    u = {}
    try:
        for x in s:
            u[x] = 1
    except TypeError:
        del u  # move on to the next method
    else:
        return u.keys()

    # We can't hash all the elements.  Second fastest is to sort,
    # which brings the equal elements together; then duplicates are
    # easy to weed out in a single pass.
    # NOTE:  Python's list.sort() was designed to be efficient in the
    # presence of many duplicate elements.  This isn't true of all
    # sort functions in all languages or libraries, so this approach
    # is more effective in Python than it may be elsewhere.
    try:
        t = list(s)
        t.sort()
    except TypeError:
        del t  # move on to the next method
    else:
        assert n > 0
        last = t[0]
        lasti = i = 1
        while i < n:
            if t[i] != last:
                t[lasti] = last = t[i]
                lasti += 1
            i += 1
        return t[:lasti]

    # Brute force is all that's left.
    u = []
    for x in s:
        if x not in u:
            u.append(x)
    return u


#Get files from a wild card list
#e.g: GetFiles(["*.HEAD", "*.1D"])
def GetFiles(wild):
   #print "wild is: >>>%s<<<" % wild
   an = reduce(operator.add, map(glob.glob, wild))
   #print "Expanded is: %s" % an
   return an

def PrintIndexedList(l):
   cnt = 0
   for il in l:
      print "%d-  %s" % (cnt, il)
      cnt += 1
   print ""

def match(txt, l):
   lm = []
   for il in l:
      fnd = il.find(txt)
      if  fnd >= 0:
         lm.append(il)
   return lm

def unique_match(txt, l):
   lm = match(txt,l)
   if len(lm) == 1:
      return lm[0]
   else:
      return None

      
def GetSelectionFromList(l, prmpt = ""):
   PrintIndexedList(l)
   if len(prmpt)==0:
      prmpt = 'Enter Selection by number or name: '
   cnt = 0
   while cnt < 10:
      name = raw_input(prmpt)
      if not name:
         return None
      if name.isdigit():
         if int(name) < len(l) and int(name) >= 0:
            return l[int(name)]
         else:
            print "Input error: number must be between 0 and %d" % (len(l)-1)
      else:
         if name in l:
            return name
         nameg = unique_match(name, l)
         if nameg:
            return nameg
         else:
            print "Input error: selection %s has %d matches in list." %  \
                  ( name, len(match(name, l)))
      cnt += 1
   print "Vous ne comprenez pas l'anglais?"
   print "Ciao"
   
# determine if a string is a valid floating point number
# from http://mail.python.org/pipermail/python-list/2002-September/164892.html
# used like isnum() or isalpha() built-in string methods
def isFloat(s):
    try:
        float(s)
        return True
    except (ValueError, TypeError), e:
        return False
