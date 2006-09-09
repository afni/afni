#!/usr/bin/python
import os, sys, glob, operator, string, afni_base

class afni_name:
   def __init__(self, name=""):
      res = parse_afni_name(name)
      self.path = res['path']
      self.prefix = res['prefix']
      self.view = res['view']
      self.extension = res['extension']
      self.type = res['type']
      return
   def p(self):
      return os.path.abspath(self.path)
   def ppve(self):
      s = "%s/%s%s%s" % (os.path.abspath(self.path), self.prefix, self.view, self.extension)
      return s
   def ppv(self):
      s = "%s/%s%s" % (os.path.abspath(self.path), self.prefix, self.view)
      return s
   def pp(self):
      return "%s/%s" % (os.path.abspath(self.path), self.prefix)
   def pv(self):
      return "%s%s" % (self.prefix, self.view)
   def exist(self):
      if os.path.isfile("%s.HEAD" % self.ppv()) and \
         (os.path.isfile("%s.BRIK" % self.ppv()) or os.path.isfile("%s.BRIK.gz" % self.ppv())):
         return 1
      else:
         return 0
   def delete(self): #delete files on disk!
      if os.path.isfile("%s.HEAD" % self.ppv()):
         shell_exec("rm %s.HEAD" % self.ppv())
      if os.path.isfile("%s.BRIK" % self.ppv()):
         shell_exec("rm %s.BRIK" % self.ppv())
      if os.path.isfile("%s.BRIK.gz" % self.ppv()):
         shell_exec("rm %s.BRIK.gz" % self.ppv())
      return
   def move_to_dir(self, path=""):
      #self.show()
      #print path
      if os.path.isdir(path):
         sv = shell_com("mv %s %s %s %s/" % (self.head(), self.brick(), self.brickgz(), path))
         self.new_path(path)
         if ( not self.exist() ):
            print "Error: Move failed"
            return 0
      else:
         return 0
      return 1
   def head(self):
      return "%s.HEAD" % self.ppv()
   def brick(self):
      return "%s.BRIK" % self.ppv()      
   def brickgz(self):
      return "%s.BRIK.gz" % self.ppv() 
   def new_path(self,path=""):
      #give name a new path
      if len(path) == 0:
         self.path = "./"
      else:
         if path[-1] == '/':
            self.path = path
         else:
            self.path = "%s/" % path
   def new_prefix(self, prefix=""):
      self.prefix = prefix
   def new_view(self,view=""):
      self.view = view
   def show(self):
      print "AFNI filename:"
      print "   name    : %s" % self.ppve()
      print "   path    : %s" % self.path
      print "   prefix  : %s" % self.prefix   
      print "   view    : %s" % self.view
      print "   exten.  : %s" % self.extension
      print "   type    : %s" % self.type
      print "   On Disk : %d" % self.exist()
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
   def __init__(self, name, npar, defpar, acplist=[]):
      self.name = name
      self.i_name = -1      #index of option name in argv
      self.n_exp = npar     #Number of expected params, 0 if no params, 
                            #-1 if any number > 0 is OK.
                            #N if exactly N numbers are expected 
      self.n_found = -1     #Number of parameters found after parsing
                            # 0 means option was on command line but had no parameters
      self.parlist = None     #parameter strings list following option 
      self.deflist = defpar #default parameter list,if any
      self.acceptlist = acplist #acceptable values if any
      return 
   def test(self):
      if (len(self.deflist) != 0 and self.parlist == None):  #some checks possible, parlist not set yet
         if self.n_exp >= 0:
            if len(self.deflist) != self.n_exp:
               print "Error: Option %s needs %d parameters\nDefault list has %d parameters." \
                        % (self.name, self.n_exp, len(self.deflist))
               return None
         else:
            if len(self.deflist) < -self.n_exp:
               print "Error: Option %s needs at least %d parameters\nDefault list has %d parameters."\
                        % (self.name, -self.n_exp, len(self.deflist))
               return None 
      else :
         if self.n_exp >= 0:
            if len(self.parlist) != self.n_exp:
               print "Error: Option %s needs %d parameters\nParameter list has %d parameters." \
                        % (self.name, self.n_exp, len(self.parlist))
               return None
         else:
            if len(self.parlist) < -self.n_exp:
               print "Error: Option %s needs at least %d parameters\nParameter list has %d parameters."\
                        % (self.name, -self.n_exp, len(self.parlist))
               return None 
      return 1

class shell_com:
   def __init__(self, com, eo="echo"):
      self.com = com #command
      self.dir = os.getcwd()
      self.exc = 0      #command not executed yet
      self.so = ''
      self.se = ''
      if eo == "echo":
         print "#Now running:\n   cd %s\n   %s" % (self.dir, self.com)
         self.run()
         self.out()
      elif eo == "dry_run":
         print "#Would be running:\n   cd %s\n   %s" % (self.dir, self.com)
         self.out()
      return
   def run(self):
      so, se = shell_exec(self.com, "")
      self.so = so
      self.se = se
      self.exc = 1
   def stdout(self):
      if (len(self.so)):
         print "++++++++++ stdout:" 
         for ln in self.so:
            print "   %s" % ln
   def stderr(self):   
      if (len(self.se)):
            print "---------- stderr:" 
            for ln in self.se:
               print "   %s" % ln
   def out(self):
      if self.exc:
         self.stdout()
         self.stderr()
      else:
         print "#............. not executed."
   def val(self, i):
      if not self.exc:
         print "Command not executed"
         return None
      elif len(self.so) == 0:
         print "Empty output."
         return None
      elif len(self.so) <= i:
         print "Index %d larger than number of elements (%d) in output " % (i, len(self.so))
         return None
      else:
         return self.so[i]
              

#transform a list of afni names to one string for shell script usage
def anlist(vlst):
   namelst = []
   for an in vlst:
      namelst.append(an.ppv())    
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
   #an option that needs no params
   oplist.append(afni_base.comopt('-dicom', 0, []))  
   #an option that needs 2 params, with 2 options, defaulting to 2 and 10.0
   oplist.append(afni_base.comopt('-clust', 2, ['2', '10.0']))  
   #an option that needs an undetermined number of parameters ( 1 or more, -2 for 2 or more)
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
               (op.n_exp > 0 and len(op.parlist) < op.n_exp and len(argv) > 0)) and \
               argv[op.iname] not in optnames:
               if len(op.acceptlist):
                  if argv[op.iname] not in op.acceptlist:
                     print "Error: parameter value %s for %s is not acceptable\nChoose from %s" % \
                           (argv[op.iname], op.name, string.join(op.acceptlist, ' , '))
               op.parlist.append(argv[op.iname]) #string added
               argv.remove(argv[op.iname])             #remove this string from list          
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
               print "Error: Expecting %d parameters\nHave %d on command line (%s).\n" % \
                  (op.n_exp, len(op.parlist), op.parlist)
               return None
      elif len(argv) > 0:
         print "Error: Expecting no loose parameters.\nHave %d loose parameters (or bad option) on command line (%s).\n" % \
                  (len(argv), argv)
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
   #ap = os.path.abspath(name) #absolute path
   fn = os.path.basename(name)
   #is this a .nii volume?
   rni = strip_extension(fn,['.nii', '.nii.gz'])
   if (len(rni[1]) > 0):
      vi = ''  #No view
      ex = rni[1]
      pr = rni[0]
      tp = 'NIFTI'
   else: 
      rni = strip_extension(fn,['.HEAD','.BRIK','.BRIK.gz','.1D', '.','.1D.dset', '.niml.dset'])
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
   #Build the dictionary result
   if len(rp) == 0:
      rp = '.'
   res['path'] = "%s/" % rp
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

#exectute a shell command and return results in so (stdout) and se (stderr)
def shell_exec(s,opt=""):
   if opt == "dry_run":
      print "In %s, would execute:\n%s" % (os.getcwd(), s)
      return "", ""
   elif opt == "echo":
      print "In %s, about to execute:\n%s" % (os.getcwd(), s)
   
   i,o,e = os.popen3(s,'r') #captures stdout in o,  stderr in e and stdin in i      
   so = o.readlines()
   se = e.readlines()
   o.close
   e.close                     
   if (len(so) and opt == "echo"):
      print "++++++++++ stdout:" 
      for ln in so:
         print "   %s" % ln
   if (len(se) and opt == "echo"):
      print "---------- stderr:" 
      for ln in se:
         print "   %s" % ln
      
   return so, se

#generic unique function, from:  http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/52560/index_txt
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
