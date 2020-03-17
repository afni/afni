#!/usr/bin/env python

# python3 status: started

import os, sys, glob, operator, string, re

valid_afni_views = ['+orig', '+acpc', '+tlrc']
valid_new_views  = ['+orig', '+acpc', '+tlrc', '']

# limits for shell_com history
SAVE_SHELL_HISTORY = 400
MAX_SHELL_HISTORY  = 600

class afni_name(object):
   def __init__(self, name="", do_sel=1, view=None):
      """do_sel : apply selectors (col, row, range)"""
      self.initname = name
      self.do_sel = do_sel
      res = parse_afni_name(name, do_sel=self.do_sel)
      self.path = res['path']
      self.prefix = res['prefix']
      self.view = res['view']
      self.extension = res['extension']
      self.type = res['type']
      self.colsel = res['col']
      self.nodesel = res['node']
      self.rowsel = res['row']
      self.rangesel = res['range']
      self.selquote = '"'       # selector quote
      if view in valid_new_views: self.new_view(view)
      return

   def p(self):   #Full path 
      """show path only, no dataset name"""
      pp = "%s/" % os.path.abspath('./')  #full path at this location
      fn = pp.find(self.path)      #is path at end of abspath?
      if (fn > 0 and fn+len(self.path) == len(pp)): #path is at end of abs path
         return pp
      else:
         return "%s/" % os.path.abspath(self.path)

   def realp(self):   #Full path following symbolic links 
      """show path only, no dataset name"""
      pp = "%s/" % os.path.realpath('./')  #full path at this location
      fn = str.find(pp,self.path)      #is path at end of abspath?
      if (fn > 0 and fn+len(self.path) == len(pp)): #path is at end of abs path
         return pp
      else:
         return "%s/" % os.path.realpath(self.path)

   def ppve(self, sel=0):
      """show path, prefix, view and extension"""
      s = "%s%s" % (self.p(), self.pve(sel=sel))
      return s

   def rppve(self, sel=0):
      """show path, prefix, view and extension"""
      s = "%s%s" % (self.realp(), self.pve(sel=sel))
      return s

   # selectors, along with many sel=0 function parameters  7 Jun 2016 [rickr]
   def selectors(self):
      """return all selectors, usually in double quotes
         (colsel, rowsel, nodesel, rangesel)"""

      sstuff = '%s%s%s%s' % (self.colsel, self.rowsel, self.nodesel,
                             self.rangesel)

      if sstuff == '': return sstuff
    
      return "%s%s%s" % (self.selquote, sstuff, self.selquote)
      
   def ppves(self, quotes=1):
      """show path, prefix, view, extension and all selectors
         (colsel, rowsel, nodesel, rangesel)

         this is identically ppve(sel=1), but maybe without quotes
      """
      
      # if no selectors, do not incude quotes    7 Apr 2015 [rickr]

      # if no quotes, clear and reset internal selqute
      if not quotes:
         qstr = self.selquote
         self.selquote = ''

      pstuff = self.ppve(sel=1)

      if not quotes:
         self.selquote = qstr

      return pstuff

      # s = "%s%s%s%s'%s%s%s%s'" % (self.p(), self.prefix, \
      #                    self.view, self.extension,\
      #                    self.colsel, self.rowsel,\
      #                    self.nodesel, self.rangesel)
      #
      # return s
      
   def input(self):
      """full path to dataset in 'input' format
         e.g. +orig, but no .HEAD
         e.g. would include .nii"""
      if self.type == 'BRIK':
         return self.ppv()
      else:
         return self.ppve() 

   def real_input(self):
      """full path to dataset in 'input' format
         follow symbolic links!!!!
         e.g. +orig, but no .HEAD
         e.g. would include .nii"""
      if self.type == 'BRIK':
         return self.rppv()
      else:
         return self.rppve() 

   def rel_input(self, head=0, sel=0):
      """relative path to dataset in 'input' format
         e.g. +orig, but no .HEAD
         e.g. would include .nii"""
      if self.type == 'BRIK':
         # separate selectors for HEAD case
         if sel: sstr = self.selectors()
         else:   sstr = ''
         name = self.rpv()
         if head: return '%s%s%s' % (name, '.HEAD', sstr)
         else:    return '%s%s' % (name, sstr)
      else:
         return self.rpve(sel=sel) 

   def shortinput(self, head=0, sel=0):
      """dataset name in 'input' format
         - no directory prefix
         - include extension if non-BRIK format
         - if head: include .HEAD suffix
         - if sel: include selectors
      """
      if self.type == 'BRIK':
         # separate selectors for HEAD case
         if sel: sstr = self.selectors()
         else:   sstr = ''
         name = self.pv()
         if head: return '%s%s%s' % (name, '.HEAD', sstr)
         else:    return '%s%s' % (name, sstr)
      else:
         return self.pve(sel=sel) 

   def out_prefix(self):
      """dataset name in 'output' format
         - no directory
         - include extension if non-BRIK format"""
      if self.type == 'BRIK':
         return self.prefix
      else:
         return self.pve() 
   def ppv(self, sel=0):
      """return path, prefix, view formatted name"""
      s = "%s%s" % (self.p(), self.pv(sel=sel))
      return s
   def rppv(self, sel=0):
      """return path, prefix, view formatted name resolving symbolic links"""
      s = "%s%s" % (self.realp(), self.pv(sel=sel))
      return s
   def rel_dir(self, sel=0):
      """return relative directory of an object
         - this will be empty (instead of "./") if in current directory
      """
      cwd = os.path.abspath(os.curdir)
      # if not at root (not mentioning any names, Dylan), append '/'
      # to remove current directory from prefix   30 Nov 2017 [rickr]
      if cwd != '/': cwd += '/'
      # check if equal, where staring from len() would fail
      if self.path == cwd:
          pp = ''
      elif self.path.startswith(cwd):
          pp = self.path[len(cwd):]
      else:
          pp = self.path
      return pp
   def rpv(self, sel=0):
      """return relative path, prefix, view formatted name
         - do not include ./ as relative path"""
      # rp = str.replace(self.path, "%s/" % os.path.abspath(os.curdir), '')
      rp = self.rel_dir()
      s = "%s%s" % (rp, self.pv(sel=sel))
      return s
   def rpve(self, sel=0):
      """return relative path, prefix, view, extension formatted name
         - do not include ./ as relative path"""
      rp = self.rel_dir()
      s = "%s%s" % (rp, self.pve(sel=sel))
      return s
   def pp(self):
      """return path, prefix formatted name"""
      return "%s%s" % (self.p(), self.prefix)
   def pv(self, sel=0):
      """return prefix, view formatted name"""
      if sel: sstr = self.selectors()
      else:   sstr = ''
      if self.type == 'BRIK':
         return "%s%s%s" % (self.prefix, self.view, sstr)
      else:
         return self.pve(sel=sel)
   def pve(self, sel=0):
      """return prefix, view, extension formatted name"""
      if sel: sstr = self.selectors()
      else:   sstr = ''
      return "%s%s%s%s" % (self.prefix, self.view, self.extension, sstr)
   def dims(self, quotes=1):
      """return xyzt dimensions, as a list of ints"""
      return dset_dims(self.ppves(quotes=quotes))
   def exist(self):
      """return whether the dataset seems to exist on disk"""
      locppv = self.ppv()
      if (self.type == 'NIFTI'):
         if (     os.path.isfile("%s" % locppv) or \
                  os.path.isfile("%s.gz" % locppv) \
            ):
            return 1
         else: return 0
      elif (self.type == 'BRIK'):
         if (     os.path.isfile("%s.HEAD" % locppv)        \
               and                                          \
               (  os.path.isfile("%s.BRIK" % locppv) or     \
                  os.path.isfile("%s.BRIK.gz" % locppv) or  \
                  os.path.isfile("%s.BRIK.bz2" % locppv) or \
                  os.path.isfile("%s.BRIK.Z" % locppv) )    \
            ):
            return 1
         else: return 0
      elif (self.type == 'NIML'):
         # ppv leads down to pve for non-BRIK
         if (     os.path.isfile(locppv) or               \
                  os.path.isfile("%s.niml.dset" % locppv) \
            ):
            return 1
         else: return 0
      else:
         if (     os.path.isfile(locppv) ):
            return 1
         else: return 0
   
   def locate(self, oexec=""):
      """Attempt to locate the file and if found, update its info"""
      if (self.exist()):
         return 1
      else:
         #could it be in abin, etc.
         cmd = '@FindAfniDsetPath %s' % self.pv()
         com=shell_com(cmd,oexec, capture=1)
         com.run()

         if com.status or not com.so or len(com.so[0]) < 2:
           # call this a non-fatal error for now
           if 0:
              print('   status = %s' % com.status)
              print('   stdout = %s' % com.so)
              print('   stderr = %s' % com.se)
           return 0

         # self.path = com.so[0].decode()
         self.path = com.so[0]
         # nuke any newline character
         newline = self.path.find('\n')
         if newline > 1: self.path = self.path[0:newline]
      return 0
      
   def delete(self, oexec=""): #delete files on disk!
      """delete the files via a shell command"""
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
                  print("Error: Move to %s failed" % (self.ppv()))
                  return 0
            else:
               print("Error: Found no .HEAD or .BRIK or .BRIK.gz (or .bz2 or .Z) of %s" % (self.ppv()))
               return 0
         else:
            if os.path.isfile("%s" % self.ppve()):
               sv = shell_com("mv %s %s/" % (self.ppve(), path), oexec).run()
               found = found + 1
            if (found > 0):
               self.new_path(path)
               if ( not self.exist() and oexec != "dry_run"):
                  print("Error: Move to %s failed" % (self.ppv()))
                  return 0
            else:
               print("Error: Found no file %s to move." % self.ppve())
               return 0
      else:
         print("Error: Path %s not found for moving %s." % (path, self.ppv()))
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
      #give name a new path (check for root)
      if len(path) == 0: pp = "./"
      else:              pp = path
      ap = os.path.abspath(pp)
      # require this to end in a '/'
      if ap[-1] != '/': ap += '/'
      self.path = ap
   def new_prefix(self, prefix=""):
      self.prefix = prefix
   def new_view(self,view=""):
      self.view = view
   def show(self, mesg='', verb=1):
      """options to see the path require verb>1"""
      if mesg: mesg = ' (%s)' % mesg
      print("AFNI filename%s:" % mesg)
      if verb > 1: print("   curdir  : %s" % os.path.abspath(os.curdir))

      print("   initial : %s" % self.initname)
      if verb > 1: print("   name    : %s" % self.ppve())
      if verb > 1: print("   path    : %s" % self.path)

      print("   prefix  : %s" % self.prefix)   
      print("   view    : %s" % self.view)
      print("   exten.  : %s" % self.extension)
      print("   type    : %s" % self.type)
      print("   On Disk : %d" % self.exist())
      print("   Row Sel : %s" % self.rowsel)
      print("   Col Sel : %s" % self.colsel)
      print("   Node Sel: %s" % self.nodesel)
      print("   RangeSel: %s" % self.rangesel)
      
   def new(self, new_pref='', new_view='', parse_pref=0):  
      """return a copy with optional new_prefix and new_view
         if parse_pref, parse prefix as afni_name
      """
      an = afni_name()
      an.path = self.path
      if len(new_pref):
         # maybe parse prefix as afni_name
         if parse_pref:
            ant = parse_afni_name(new_pref, do_sel=self.do_sel)
            an.prefix = ant['prefix']
         else: an.prefix = new_pref
      else:
         an.prefix = self.prefix
      if len(new_view):
         an.view = new_view
      else:
         an.view = self.view
      an.extension = self.extension
      an.type = self.type
      return an

   def initial_view(self):
      """return any initial view (e.g. +tlrc) from self.initial"""
      pdict = parse_afni_name(self.initname, do_sel=self.do_sel)
      view = pdict['view']
      if view in ['+orig', '+acpc', '+tlrc']: return view
      return ''
               
   def to_afni(self, new_view=''):  
      """modify to be BRIK type, with possible new_view (default is +orig)"""

      # be sure there is some view
      if new_view in valid_afni_views:        self.view = new_view
      elif self.view not in valid_afni_views: self.view = '+orig'

      if self.type == 'BRIK': return

      self.type = 'BRIK'
      self.extension = ''  # clear 
      return
               
class comopt(object):
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
      print("%sComopt: %s" % (mesg, self.name))
      if short: return      # 22 Jan 2008 [rickr]

      print("  (i_name, n_exp, n_found) = (%d, %d, %d)" % \
               (self.i_name, self.n_exp, self.n_found))
      print("  parlist = %s" % self.parlist)
      print("  deflist = %s" % self.deflist)
      print("  acceptlist = %s" % self.acceptlist)

   def test(self):
      if (len(self.deflist) != 0 and self.parlist == None):
         # some checks possible, parlist not set yet
         if self.n_exp >= 0:
            if len(self.deflist) != self.n_exp:
               print("Error: Option %s needs %d parameters\n" \
                     "Default list has %d parameters." \
                        % (self.name, self.n_exp, len(self.deflist)))
               return None
         else:
            if len(self.deflist) < -self.n_exp:
               print("Error: Option %s needs at least %d parameters\n"  \
                     "Default list has %d parameters."\
                        % (self.name, -self.n_exp, len(self.deflist)))
               return None 
      else :
         if self.n_exp >= 0:
            #print "option %s n_exp = %d, len(parlist)=%d" % (self.name, self.n_exp, len(self.parlist))
            #self.show()
            if len(self.parlist) != self.n_exp:
               print("Error: Option %s needs %d parameters\n" \
                     "Parameter list has %d parameters." \
                        % (self.name, self.n_exp, len(self.parlist)))
               return None
         else:
            if len(self.parlist) < -self.n_exp:
               print("Error: Option %s needs at least %d parameters\n"  \
                     "Parameter list has %d parameters."\
                        % (self.name, -self.n_exp, len(self.parlist)))
               return None 
      return 1

class shell_com(object):
   history = []         # shell_com history
   save_hist = 1        # whether to record as we go

   def __init__(self, com, eo="", capture=0, save_hist=1):
      """create instance of shell command class

            com         command to execute (or echo, etc)
            eo          echo mode string: echo/dry_run/script/""
            capture     flag: store output from command?
            save_hist   flag: store history of commands across class instances?
      """

      self.com = com    # command string to be executed
      self.eo = eo      # echo mode (echo/dry_run/script/"")
                        # note: getcwdu() would be an option, but not needed
      self.dir = os.getcwd()
      self.exc = 0      #command not executed yet
      self.so = ''
      self.se = ''
      if (self.eo == "quiet"):
         self.capture = 1
      else:
         self.capture = capture; #Want stdout and stderr captured?
      self.save_hist = save_hist

      #If command line is long, trim it, if possible
      l1 = len(self.com)
      if (l1 > 80):
         self.trimcom = self.trim()
         #if (len(self.com) < l1):
         #print "Command trimmed to: %s" % (self.com)
      else:
         self.trimcom = re.sub(r"[ ]{2,}", ' ', self.com)
         #  string.join(string.split(self.com)) is bad for commands like 3dNotes
   def trim(self):
      #try to remove absolute path and numerous blanks
      if self.dir[-1] != '/':
         tcom = re.sub(r"[ ]{2,}", ' ', self.com).replace("%s/" % (self.dir), './')
      else:
         tcom = re.sub(r"[ ]{2,}", ' ', self.com).replace(self.dir, './')
      return tcom
   def echo(self): 
      if (len(self.trimcom) < len(self.com)):
         ms = " (command trimmed)"
      else:
         ms = ""
      if self.eo == "echo":
         print("#Now running%s:\n   cd %s\n   %s" % (ms, self.dir, self.trimcom))
         sys.stdout.flush()
      elif self.eo == "dry_run":
         print("#Would be running%s:\n  %s" % (ms, self.trimcom))
         sys.stdout.flush()
      elif (self.eo == "script"):
         print("#Script is running%s:\n  %s" % (ms, self.trimcom))
         sys.stdout.flush()
      elif (self.eo == "quiet"):
         pass
      
      if self.exc==1:
         print("#    WARNING: that command has been executed already! ")
         sys.stdout.flush()
      else: self.add_to_history()

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

   def add_to_history(self):
      """append the current command (trimcom) to the history, truncating
         if it is too long"""
      if not self.save_hist: return
      if len(self.history) >= MAX_SHELL_HISTORY:
         self.history = self.history[-SAVE_SHELL_HISTORY:]
      self.history.append(self.trimcom)

   def shell_history(self, nhist=0):
      if nhist == 0 or nhist > len(self.history): return self.history
      else:                                  return self.history[-nhist]

   def stdout(self):
      if (len(self.so)):
         print("++++++++++ stdout:") 
         sys.stdout.flush()
         for ln in self.so:
            print("   %s" % ln)
            sys.stdout.flush()
   def stderr(self):   
      if (len(self.se)):
            print("---------- stderr:") 
            sys.stdout.flush()
            for ln in self.se:
               print("   %s" % ln)
   def out(self):
      if self.exc:
         self.stdout()
         self.stderr()
      else:
         print("#............. not executed.")
         sys.stdout.flush()
   
   def val(self, i, j=-1): #return the jth string from the ith line of output. if j=-1, return all ith line
      if not self.exc:
         print("Error: Command not executed")
         return None
      elif self.eo == "dry_run":
         return "0"  #Just something that won't cause trouble for places expecting numbers
      elif len(self.so) == 0:
         print("Error: Empty output.")
         self.stderr()
         return None
      elif len(self.so) <= i:
         print("Error: First index i=%d >= to number of elements (%d) in output " %  \
               (i, len(self.so)))
         return None
      
      if j>= 0:
         l = self.so[i].split()
         if len(l) <= j:
            print("Error: Second index j=%d is >= to number of elements (%d) in %dth line of output" % \
                     (j, len(l), i))
            return None
         else:
            # return l[j].decode()
            return l[j]
      else:
         # return self.so[i].decode()
         return self.so[i]

# return the attribute list for the given dataset and attribute
def read_attribute(dset, atr, verb=1):
    [so, se] = shell_exec('3dAttribute %s %s' % (atr, dset))
    if len(so) == 0:
        if verb > 0:
           print('** 3dAttribute exec failure for "%s %s"' % (atr, dset))
           if len(se) > 0: print("shell error:\n   %s\n" % '\n   '.join(se))
        return None
    list = so[0].split()
    if len(list) > 0: return list
    else:
        if verb > 0: print('** 3dAttribute failure for "%s %s":' % (atr, dset))
        return None

# return dimensions of dset, 4th dimension included
def dset_dims(dset):
   # always return 4 values, trap some errors    7 Apr 2015 [rickr]
   dl = [-1, -1, -1, -1]
   if 0: #This approach fails with selectors!
      ld = read_attribute(dset, 'DATASET_DIMENSIONS')
      lr = read_attribute(dset, 'DATASET_RANK')
      dl = []
      for dd in ld[0:3]:
         dl.append(int(dd))
      dl.append(int(lr[1]))
   else:
      cstr = '3dnvals -all %s' % dset
      com = shell_com(cstr, capture=1);
      rv = com.run()
      if rv:
         print('** Failed "%s"' % cstr)
         return dl
      if len(com.so) < 1:
         print('** no stdout from "%s"' % cstr)
         return dl
      vlist = com.so[0].split()
      if len(vlist) != 4:
         print('** failed "%s"' % cstr)
         return dl
      try:
         vl = [int(val) for val in vlist]
         # so only if success
         dl = vl
      except:
         print('** could not convert output to int:\n'  \
               '   command: %s\n'                       \
               '   output: %s' % (cstr, com.so))
      # dl = [int(com.val(0,0)), int(com.val(0,1)),
      #       int(com.val(0,2)), int(com.val(0,3))]   
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
   return str.join(' ',namelst)


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
      print("Option dictionary is None\n")
      return
   print(opts)
   for key in list(opts.keys()):
      print("Option Name: %s" % key)
      print("       Found: %d" % opts[key].n_found)
      print("       User Parameter List: %s" % opts[key].parlist)
      print("       Default Parameter List: %s\n" % opts[key].deflist)
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
                     print("Error: parameter value %s for %s is not "   \
                           "acceptable\nChoose from %s" %               \
                           (argv[op.iname], op.name,                    \
                           str.join(' , ',op.acceptlist)))
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
               print("Error: Expecting %d parameters\n" \
                     "Have %d on command line (%s).\n" % \
                     (op.n_exp, len(op.parlist), op.parlist))
               return None
      elif len(argv) > 0:
         print("Error: Expecting no loose parameters.\n"        \
               "Have %d loose parameters (or bad option) on "   \
               "command line (%s).\n" % (len(argv), argv))
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
      spl = name.split('.')
      if len(spl) > 1:
         res[0] = str.join('.',spl[0:-1])
         res[1] = '.'+spl[-1]
         return res
         
   #defaults
   res[0] = name
   res[1] = ''
   return res


#parse an afni name
def parse_afni_name(name, aname=None, do_sel=1):
   """do_sel : apply afni_selectors"""
   res = {}
   #get the path  #Can also use os.path.split
   rp = os.path.dirname(name) #relative path
   ap = os.path.abspath(rp) #absolute path
   fn = os.path.basename(name)
   #Get selectors out of the way:
   if do_sel:
      res['col'], res['row'], res['node'], res['range'], fn = afni_selectors(fn)
   else:
      res['col'] = res['row'] = res['node'] = res['range'] = ''
   
   #is this a .nii volume?
   rni = strip_extension(fn,['.nii', '.nii.gz'])
   if (len(rni[1]) > 0):
      vi = ''  #No view
      ex = rni[1]
      pr = rni[0]
      tp = 'NIFTI'
   else: 
      rni = strip_extension(fn,['.HEAD','.BRIK','.BRIK.gz','.BRIK.bz2',
                            '.BRIK.Z','.1D', '.',  '.1D.dset', '.niml.dset'])
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
   # A world of trouble when relative path is used. So use ap instead April 08
   # (do not append '/' to root)
   if ap == '/': res['path'] = ap
   else:         res['path'] = "%s/" % ap
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

   # Use modified namestr for continued processing, as no nesting should
   # be considered here, and '#' can be in [] selectors.

   nse = namestr.count('[')
   if (nse == 1 and nse == namestr.count(']')):
      sqsel = namestr[namestr.find('['):names.find(']')+1]
      namestr = namestr.replace(sqsel,'')
   
   # handle enclosed shell variables, like "${subj}"  13 Jun 2019 [rickr]
   clist = find_all_non_var_curlys(namestr)
   if len(clist) == 1:  # maybe > 0 is okay, should we really distinguish?
      if namestr.count('}', clist[-1]) == 1:
         cend = namestr.find('}',clist[-1])  # for clarity
         cusel = namestr[clist[-1]:cend+1]
         namestr = namestr.replace(cusel,'')
   
   nse = namestr.count('<')
   if (nse == 1 and nse == namestr.count('>')):
      ltsel = namestr[namestr.find('<'):namestr.find('>')+1]
      namestr = namestr.replace(ltsel,'')
   
   nse = namestr.count('#')
   if (nse == 2):
      nf = namestr.find('#')
      pnsel = namestr[nf[0]:nf[1]+1]
      namestr = namestr.replace(pnsel,'')
   
   return sqsel, cusel, pnsel, ltsel, namestr

def find_all_non_var_curlys(stext):
   """return a 
   """
   clist = []
   start = 0
   cind = stext.find('{', start)
   while cind >= start:
      # if preceded by '$', just ignore (okay if cind-1 == -1)
      if cind == 0 or stext[cind-1] != '$':
         # have found '{', and not preceded by '$'
         clist.append(cind)
      # and look for next
      start = cind + 1
      cind = stext.find('{', start)
   return clist
   
#execute a shell command and when capture is 1 returns results in:
# so (stdout) and se (stderr) and status
#status is only reliable with python versions 2.5 and above
def shell_exec(s,opt="",capture=1):
   #opt is left here for backwards compatibility.
   #no echoing should be done here. It is better
   #to use the shell_com objects
   if opt == "dry_run":
      print("#In %s, would execute:\n   %s" % (os.getcwd(), s))
      sys.stdout.flush()
   elif opt == "echo":
      print("#In %s, about to execute:\n   %s" % (os.getcwd(), s))   
      sys.stdout.flush()
      
   status, so, se = shell_exec2(s,capture)
   
   return so, se
   
def shell_exec2(s, capture=0):

   # moved to python_ver_float()   16 May 2011 [rickr]
   if (python_ver_float() < 2.5): #Use old version and pray
      #if there is no capture in option: run os.system
      if(not capture):
         os.system("%s"%s)
         status = 0; #Don't got status here 
         so = []     # should return arrays    24 Apr, 2014 [rickr]
         se = []
      else:
         i,o,e = os.popen3(s) #captures stdout in o,  stderr in e and stdin in i      
         # The readlines seems to hang below despite all the attempts at
         # limiting the size and flushing, etc. The hangup happens when a
         # program spews out a lot to stdout.  So when that is expected,
         # redirect output to a file at the command.
         # Or use the "script" execution mode

         # Forget readlines() and just use readline().  From that,
         # construct the desired lists.        12 Mar 2015 [rickr]

         so = []
         ll = o.readline()
         while len(ll) > 0:
            so.append(ll)
            ll = o.readline()

         se = []
         ll = e.readline()
         while len(ll) > 0:
            se.append(ll)
            ll = e.readline()

         #so = o.readlines(64)  - read till EOF, but python might hang 
         #se = e.readlines(64)  - if output to stdout and stderr is too large.
         #Have tried readlines(1024) and (256) to little effect 

         o.close
         e.close             #
         status = 0; #Don't got status here 
   else:
      import subprocess as SP
      if(not capture):
         pipe = SP.Popen(s,shell=True, stdout=None, stderr=None, close_fds=True)
#         pipe = SP.Popen(s,shell=True, executable='/bin/tcsh', stdout=None, stderr=None, close_fds=True)
         status = pipe.wait() #Wait till it is over and store returncode
         so = []
         se = []
      else:
         pipe = SP.Popen(s,shell=True, stdout=SP.PIPE, stderr=SP.PIPE, close_fds=True)
         o,e = pipe.communicate()   #This won't return until command is over
         status = pipe.returncode   #NOw get returncode

         # for python3, convert bytes to unicode (note type is bytes, but
         # that matches str in p2), just use stupid python version
         if python_ver_float() >= 3.0:
            o = o.decode()
            e = e.decode()

         so = o.splitlines()
         se = e.splitlines()                           

         #if decode:
         #   so = [l.decode() for l in so]
         #   se = [l.decode() for l in se]

   return status, so, se
   
# basically shell_exec2, but no splitlines()            16 May 2011 [rickr]
def simple_shell_exec(command, capture=0):
   """return status, so, se  (without any splitlines)"""

   if (python_ver_float() < 2.5):
      # abuse old version, re-join split lines
      status, so, se = shell_exec2(command, capture=capture)
      return status, '\n'.join(so), '\n'.join(se)

   import subprocess as SP

   if capture:
      pipe = SP.Popen(command,shell=True, stdout=SP.PIPE, stderr=SP.PIPE,
                      close_fds=True)
      so, se = pipe.communicate() # returns after command is done
      status = pipe.returncode

      # for python3, convert bytes to unicode (cannot use type(so) == bytes)
      if python_ver_float() >= 3.0:
         so = so.decode()
         se = se.decode()

   else:
#      pipe = SP.Popen(command,shell=True, executable='/bin/tcsh',
      pipe = SP.Popen(command,shell=True,
                      stdout=None, stderr=None, close_fds=True)
      status = pipe.wait() #Wait till it is over and store returncode
      so, se = "", ""

   return status, so, se

# we may want this in more than one location            16 May 2011 [rickr]
def python_ver_float():
   """return the python version, as a float"""
   vs = sys.version.split()[0]
   vlist = vs.split('.')
   if len(vlist) > 1:
      vs = "%s.%s" % (vlist[0], vlist[1])
   else:
      vs = vlist[0]

   return float(vs)

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
        return list(u.keys())

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
   # was reduce(operator.add, list(map(glob.glob, wild))), but be simple
   rl = []
   for mstr in wild:
      rl.extend(glob.glob(mstr))
   return rl
     
def PrintIndexedList(l):
   cnt = 0
   for il in l:
      print("%d-  %s" % (cnt, il))
      cnt += 1
   print("")

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
      name = input(prmpt)
      if not name:
         return None
      if name.isdigit():
         if int(name) < len(l) and int(name) >= 0:
            return l[int(name)]
         else:
            print("Input error: number must be between 0 and %d" % (len(l)-1))
      else:
         if name in l:
            return name
         nameg = unique_match(name, l)
         if nameg:
            return nameg
         else:
            print("Input error: selection %s has %d matches in list." %  \
                  ( name, len(match(name, l))))
      cnt += 1
   print("Vous ne comprenez pas l'anglais?")
   print("Ciao")
   
# determine if a string is a valid floating point number
# from http://mail.python.org/pipermail/python-list/2002-September/164892.html
# used like isnum() or isalpha() built-in string methods
def isFloat(s):
    try:
        float(s)
        return True
    except:
        return False
