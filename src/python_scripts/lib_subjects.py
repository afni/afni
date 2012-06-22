#!/usr/bin/env python

import sys, os
import copy, glob
import afni_util as UTIL

g_mema_tests = [None, 'paired', 'unpaired']
g_ttpp_tests = ['-AminusB', '-BminusA']

# atomic (type within nested list) and simple types for VarsObject
g_valid_atomic_types = [int, float, str, list]
g_simple_types = [int, float, str]

class VarsObject(object):
   """a general class for holding variables, essentially treated as a
      struct to group variables"""

   def __init__(self, name='noname'):
      self.name = name

   def set_var(self, name, newval):
      """if the value has changed (or is not a simple type), update it
         - use deepcopy (nuke it from orbit, it's the only way to be sure)

         return 1 if an update is performed, else 0
      """

      # if the attribute exists and has a simple type, check for no change
      if self.valid(name) and type(newval) in g_simple_types:
         oldval = getattr(self, name)
         if oldval == newval: return 0

      setattr(self, name, copy.deepcopy(newval))

      return 1

   def get_type(self, atr):
      """return the type of an object, if atomic, else None
         return one of int, float, str, list or None
      """

      val = getattr(self, atr)
      if type(val) in g_valid_atomic_types: return type(val)

      return None       # not a simple atomic type

   def get_atomic_type(self, atr):
      """return the atomic type of an object
         return one of int, float, str, list

         - list is returned in the case of an empty list or None
         - lists are only tested down the [0][0]... path
      """

      val = getattr(self, atr)
      while type(val) == list:
         if val == []: return list
         try: val = val[0]
         except: return list

      if val == None: return list  # special: NoneType does not seem defined
      if type(val) in g_valid_atomic_types: return type(val)

      return None       # not a simple atomic type

   def has_simple_type(self, atr):
      """attribute must exist and have type int, float or str (no list)
         else return 0"""
      val = getattr(self, atr)
      if type(val) in g_simple_types: return 1
      else:                           return 0

   def count(self, getall=0):
      """return the number of attributes

         if getall, count all
         otherwise count those with basic atomic types
      """
      return len(self.attributes(getall=getall))

   def attributes(self, getall=0):
      """same as dir(), but return only those with:
            - a name not starting with '_'
            - a simple type
         if getall: will be more like dir()
      """
      dlist = dir(self)
      retlist = []
      for atr in dlist:
         if atr[0] == '_': continue
         if self.get_atomic_type(atr) == None and not getall: continue
         retlist.append(atr)
      retlist.sort()
      return retlist

   def copy(self, name=None, as_strings=0):
      """return a copy of this class item by creating a new instance
         and copying all simple attributes

         if as_strings: copy any simple type (int, float) as string
      """

      dupe = VarsObject()
      for atr in self.attributes():
         if as_strings:
            val = getattr(self, atr)
            if type(val) == int or type(val) == float:
               setattr(dupe, atr, '%s'%val)
               continue
         # if wasn't converted from int or float, just copy
         setattr(dupe, atr, self.valcopy(atr))
      if name: dupe.name = name

      return dupe

   def merge(self, v2, typedef=None, verb=1):
      """merge in attributes from v2
         if typedef is a VarsObject, convert any simple type atrs based on it
      """

      if v2 == None: return

      if type(v2) != VarsObject:
         print ("** trying to merge %s with VarsObject" % type(v2))
         return

      if typedef != None:
         if type(typedef) != VarsObject:
            print ("** invalid object used for typedef in merge")
            return

      newatrs = v2.attributes()
      for atr in newatrs:
         newval = v2.valcopy(atr)
         val = newval  # default is no conversion
         dtype = '<default type>'

         # if we have a type definition, try to convert
         if typedef != None:
            dtype = type(typedef.val(atr))
            if dtype == int:
               try: val = int(newval)
               except:
                  print "** failed to merge %s '%s' as int" % (atr, newval)
                  val = newval
            elif dtype == float:
               try: val = float(newval)
               except:
                  print "** failed to merge %s '%s' as float" % (atr, newval)
                  val = newval

         setattr(self, atr, val)

         if verb > 1:
            print "== merge: setting %s %s = %s" % (atr, dtype, val)

   def set_var_with_defs(self, vname, vlist, defs, as_type=0, oname='',
                        verb=1, spec=None, csort=1):
      """try to set vname = value based on vlist
           (if as_type, convert to type, else leave as string)
         if vname is not known by the defaults, return failure

         if spec: update_vars_from_special(csort)

         This function will generally be called via a user interface library
         or in the user interface itself.  Since we are setting variables as
         strings.

            requred params:

                vname           : name of variable
                vlist           : value list (set from vlist[0] or vlist)
                defs            : obj to get var type from
                
            optional params:

                as_type         : flag - convert to type in defs (else str)
                oname           : object name (for messages)
                verb            : verbose level
                spec            : function to handle some special cases
                csort           : flag - tell 'spec' to check sorting

         return 1 on change, 0 on unchanged, -1 on error

         *** this should replace SUBJ.set_var_str_from_def ***
      """

      # if vname is not passed set one (for verbose output)
      if oname == '':
         if self.valid(vname): oname = self.name
         else:                 oname = '<noname>'

      if not defs.valid(vname):
         print '** SVWD: invalid %s variable: %s' % (oname, vname)
         return -1

      dtype = type(defs.val(vname))
      if dtype not in g_valid_atomic_types:
         print '** SVWD: unknown %s variable type for %s' % (oname, vname)
         return -1

      # if simple type but have list, fail
      if dtype != list and len(vlist) > 1:
         print "** SVWD: simple variable '%s' %s\n" \
               "         but have list value: %s" % (vname, dtype, vlist)
         return -1

      # ----------------------------------------
      # try to apply the value (list)

      val = None

      # update val,
      # check that the it can be properly converted (if simple type)
      if defs.has_simple_type(vname):
         val = vlist[0]
         try: vv = dtype(val)
         except:
            print '** SVWD %s.%s, cannot convert value %s to %s' \
                  (oname, vname, val, dtype)
            return -1
         # possibly apply the defs type
         if as_type: val = vv
      elif dtype == list: val = vlist
      else: 
         print '** SVWD: invalid type %s for %s'%(dtype,vname)
         return -1

      # actually set the value
      rv = self.set_var(vname, val)
      if verb > 1:
         if rv: print '++ %s: updating %s to %s %s' \
                      % (oname, vname, val, type(val))
         else:  print '++ %s: no update for %s to %s' % (oname, vname, val)

      # if no update, we're outta here
      if rv == 0: return rv

      # ----------------------------------------------------------------------
      # handle some special cases, such as indices and labels, which might
      # come with file name lists

      # this function must be passed, since it will vary per library

      if spec != None: spec(vname, self, check_sort=csort)

      return rv

   def valcopy(self, atr):
      """use deepcopy to copy any value, since it may be a list"""
      if self.get_atomic_type(atr) == None:
         print ("** attribute '%s' is not simple, copy may be bad" % atr)

      return copy.deepcopy(self.val(atr))

   def val(self, atr):
      """convenience - return the attribute value (None if not found)"""
      if hasattr(self, atr): return getattr(self, atr)
      else:                  return None

   def valid(self, atr):
      """convenience - return whether the atr is in the class instance"""
      if hasattr(self, atr): return 1
      else:                  return 0

   def is_empty(self, atr):
      """true if not set or is '' or []"""
      val = self.val(atr)
      if val == None: return True
      if val == '' or val == []: return True
      return False

   def val_len(self, atr):
      """return 0 or len(atr)"""
      val = self.val(atr)
      if type(val) == list: return len(val)
      return 0

   def is_not_empty(self, atr):
      """true if set and neither '' nor []"""
      return not self.is_empty(atr)

   def is_non_trivial_dir(self, atr):
      return not self.is_trivial_dir(atr)

   def is_trivial_dir(self, atr):
      """return true of atr is empty or '.'"""
      if self.is_empty(atr): return True
      val = self.val(atr)
      if val == '.' or val == './': return True

      return False

   def file_under_dir(self, dname, fname):
      """return fname or self.dname/fname (if dname is set and non-trivial)"""

      if self.is_trivial_dir(dname): return fname
      else:                          return '%s/%s' % (self.val(dname), fname)

   def vals_are_equal(self, atr, vobj):
      """direct comparison is okay for any valid atomic type
      """
      if type(vobj) != VarsObject:
         print '** vals_are_equal: no VarsObject'
         return False

      if self.get_atomic_type(atr) not in g_valid_atomic_types: return False
      if vobj.get_atomic_type(atr) not in g_valid_atomic_types: return False

      return self.val(atr) == vobj.val(atr)

   def changed_attrs(self, vobj):
      """return a list of attributes that differ from those in passed vobj
      """

      retlist = []
      for attr in self.attributes():
         if not self.get_atomic_type(attr): continue
         if self.val(attr) != vobj.val(attr): retlist.append(attr)

      return retlist

   def deleted_attrs(self, vobj):
      """return a list of attributes that no longer exist
         (i.e. they are in the passed vobj, but not the local instance)
      """

      retlist = []
      for attr in vobj.attributes():
         if not hasattr(self, attr): retlist.append(attr)

      return retlist

   def valid_atr_type(self, atr='noname', atype=None, alevel=0, exists=0):
      """check for the existence and type of the given variable 'atr'

                atr     : attribute name
                atype   : expected (simple) type of variable (no arrays)
                          e.g. float, int, str
                          (list is not a valid atr type)
                alevel  : array level (N levels of array nesting before atype)

         
         if alevel > 0:
            - assume array is consistent in depth and type
              (so can focus on val[0][0][0]...)
            - being empty at some depth is valid

         return 1 if valid, else 0"""

      # make sure atr is a string
      if type(atr) != str:
         print "** valid_atr_type: 'atr' must be passed as a string"
         return 0

      # make sure atype is valid as well
      if not atype in [float, int, str]:
         print "** valid_atr_type: invalid atype %s" % atype
         return 0

      # first just check for existence
      if not hasattr(self, atr): return 0

      if exists: return 1       # since found, exists test is done

      tt = self.get_atomic_type(atr)
      depth = self.atr_depth(atr)

      if depth != alevel: return 0      # bad level is bad
      if tt == list: return 1           # else, empty list is valid

      # level is good, so just compare types
      if tt == atype: return 1
      else:           return 0

   def atr_depth(self, atr='noname'):
      """return the array 'depth' of the given atr

         The depth of an empty array is considered 1, though there is no atr
         type.

         return -1: if the atr does not exist
                 N: else, the number of array nestings"""

      # first just check for existence
      if not hasattr(self, atr): return -1

      # get the variable value
      val = getattr(self, atr)

      # compute its depth
      depth = 0
      while type(val) == list:
         depth += 1
         try: val = val[0]
         except: break

      return depth

   def show(self, mesg='', prefix=None, pattern=None, name=1, all=0):
      if all: name=1
      print self.make_show_str(mesg=mesg, prefix=prefix, pattern=pattern,
                               name=name, all=all)

   def make_show_str(self, mesg='', prefix=None, pattern=None, name=1, all=0):
      """if all, put atomic attributes first and instancemethods last"""

      if prefix: pstr = ", prefix = '%s'" % prefix
      else:      pstr = ''

      sstr = "-- %s values in var '%s'%s\n" % (mesg, self.name, pstr)

      # start with atomic only, though loop is almost identical
      for atr in self.attributes(getall=all):
         if not name and atr == 'name': continue
         match = (prefix == None and pattern == None)
         if prefix:
            if atr.startswith(prefix): match = 1
         if pattern:
            if atr.find(pattern) >= 0: match = 1
         if match: sstr += "      %-20s : %s\n" % (atr, self.val(atr))

      # follow with non-atomic only, if requested (instance methods last)
      if all:
         imtype = type(self.make_show_str)
         # non-instancemethods
         for atr in self.attributes(getall=all):
            match = (prefix == None and pattern == None)
            if prefix:
               if atr.startswith(prefix): match = 1
            if pattern:
               if atr.find(pattern) >= 0: match = 1
            if match:
               atype = type(self.val(atr))
               if self.get_atomic_type(atr) == None and atype != imtype:
                  sstr += "      %-20s : %s\n" % (atr, atype)
         # instancemethods
         for atr in self.attributes(getall=all):
            match = (prefix == None and pattern == None)
            if prefix:
               if atr.startswith(prefix): match = 1
            if pattern:
               if atr.find(pattern) >= 0: match = 1
            if match:
               atype = type(self.val(atr))
               if atype == imtype:
                  sstr += "      %-20s : %s\n" % (atr, atype)

      return sstr

def comment_section_string(comment, length=70, cchar='-'):
   """return a string of the form:
      # -------------- some comment --------------
      where the total length is given
   """
   clen = len(comment)
    
   ndash = (length - clen - 4) / 2      # one '#' and 3 spaces

   # if no space for multiple dashes, don't use any
   if ndash < 2: return '# %s' % comment

   dstr = cchar * ndash  # make one ------------- string

   return '# %s %s %s\n' % (dstr, comment, dstr)

def make_message_list_string(mlist, title):
   if len(mlist) == 0: return ''
   mesg = ''
   for ind, mm in enumerate(mlist):
      if ind == 0: mesg += mm
      else:        mesg += ('\n' + mm)
   return mesg


# ===========================================================================
# begin Subject stuff  (class should be rewritten to use VarsObject types)
# ===========================================================================

def subj_compare(subj0, subj1):
   """compare 2 Subject objects:        used for sorting a list of subjects
         if _sort_key is set, compare by key, then by sid
         else, compare by sid
   """

   cval = 0
   key = subj0._sort_key
   if key != None:
      if subj0.atrs.has_key(key): v0 = subj0.atrs[key]
      else: v0 = None
      if subj1.atrs.has_key(key): v1 = subj1.atrs[key]
      else: v1 = None
      cval = cmp(v0, v1)

   if cval != 0: return subj0._order*cval
   return subj0._order*cmp(subj0.sid, subj1.sid)

def set_var_str_from_def(obj, name, vlist, vars, defs,
                         verb=1, csort=1, spec=None):
   """try to set name = value based on vlist
        (just set as string)
      if name is not known by the defaults, return failure

      if spec: update_vars_from_special(csort)

      This function will generally be called via a user interface library
      or in the user interface itself.  Since we are setting variables as
      string

      return 1 on change, 0 on unchanged, -1 on error
   """

   if not defs.valid(name):
      print '** invalid %s variable: %s' % (obj, name)
      return -1

   dtype = type(defs.val(name))
   if dtype not in g_valid_atomic_types:
      print '** SVSFD: unknown %s variable type for %s' % (obj, name)
      return -1

   # if simple type but have list, fail
   if dtype != list and len(vlist) > 1:
      print "** SVSFD: simple variable '%s' %s\n" \
            "          but have list value: %s" % (name, dtype, vlist)
      return -1

   # ----------------------------------------
   # try to apply the value (list)

   val = None

   # update val,
   # check that the it can be properly converted (if simple type)
   if defs.has_simple_type(name):
      val = vlist[0]
      try: vv = dtype(val)
      except:
         print '** SVSFD %s.%s, cannot convert value %s to %s' \
               (obj, name, val, dtype)
         return -1
   elif dtype == list: val = vlist
   else: 
      print '** SVSFD: invalid type %s for %s'%(dtype,name)
      return -1

   # actually set the value
   rv = vars.set_var(name, val)
   if verb > 1:
      if rv: print '++ %s: updating %s to %s %s' % (obj, name, val, type(val))
      else:  print '++ %s: no update for %s to %s' % (obj, name, val)

   # if no update, we're outta here
   if rv == 0: return rv

   # ----------------------------------------------------------------------
   # handle some special cases, such as indices and labels, which might
   # come with file name lists

   # this function must be passed, since it will vary per library

   if spec != None: spec(name, vars, check_sort=csort)

   return rv

def goto_proc_dir(dname):
   """ go to processing directory, returning return_dir
        - if proc_dir does not exist, create it
        - cd
        - return ret_dir
   """
   if UTIL.is_trivial_dir(dname): return '' # nowhere to go

   retdir = os.getcwd()                     # so need return directory

   # if the directory does not yet exist, create it
   if not os.path.isdir(dname):
      try: os.makedirs(dname)
      except:
         print '** failed makedirs(%s)' % dname
         return ''

   # now try to go there
   try: os.chdir(dname)
   except:
      print '** failed to go to process dir, %s' % dname
      return ''

   return retdir   # only returned on success

def ret_from_proc_dir(rname):
   """if retdir is set, cd to retdir
      (should be called 'atomically' with goto_proc_dir)
      return '', to use to clear previous return dir"""

   # if retdir is useless, bail
   if rname == None or rname == '' or rname == '.': return ''

   try: os.chdir(rname)
   except:
      print '** failed to return to %s from process dir' % rname

   return ''

def proc_dir_file_exists(dname, fname):
   if UTIL.is_trivial_dir(dname): pathname = fname
   else:                          pathname = '%s/%s' % (dname, fname)

   return os.path.isfile(pathname)
   
def get_def_tool_path(prog_name, top_dir='tool_results', prefix='tool',
                                 keep_if_missing='', digits=3):
   """return something of the form top_dir/prefix.0001.prog_name

      if top_dir exists:
         - look for anything of the form prefix.*
         - if keep_is_missing and the last entry does not contain it,
              then return that last entry
              ** goal is to only create new directory when process has happened
         - find the lowest index that is not used
         - return top_dir/prefix.NEW_INDEX.tname

      else: return top_dir/prefix.001.tname

      e.g. tool_results/tool.004.align_test
      e.g. group_results/test.004.3dttest++
   """

   tname = prog_name            # yeah, shorter, but less descriptive ...
   tdir = top_dir
   index = 1                    # default index

   # generate form for name (e.g. tr/t.%03d.t); insert index later
   form = '%s/%s.%%0%dd.%s' % (tdir, prefix, digits, tname)
   sform = '%s/%s.%%0%dd' % (tdir, prefix, digits) # short form

   # if tdir does not yet exist, we can start with the default
   if not os.path.isdir(tdir):
      return form % index

   # see what is under tdir, and go with default if nothing is found
   glist = glob.glob('%s/%s.*.*' % (tdir, prefix))
   if len(glist) == 0: return form % index

   # get trailing directory name
   tlist = [name.split('/')[-1] for name in glist]

   # abuse '.': make a list of integers from field 1 when split over '.'
   try: ilist = [int(name.split('.')[1]) for name in tlist]
   except:
      print '** found non-int VAL in %s/%s.VAL.*' % (tdir, prefix)
      return form % 999

   ilist.sort()
   nvals = len(ilist)

   # if the keep_if_missing file does NOT exist underneath the last dir,
   # return the directory for that index
   if keep_if_missing:
      fdir = sform % ilist[-1]
      if not UTIL.glob_form_has_match('%s.*/%s' % (fdir, keep_if_missing)):
         return form % ilist[-1]        # longer form includes tool name

   # quick check, if ilist[n-1] <= n, just increment
   # (or should we forget this, since non-unique values break logic?)
   # (no, tool name may change without 'keep' file/dir, so values may repeat)
   if ilist[-1] <= nvals: return form % (ilist[-1]+1)

   # finally!  now find first value > index+1 (else, just use next)
 
   for ind, ival in enumerate(ilist):
      if ival > ind+1: break

   return form % (ind+1)

class Subject(object):
   """a simple subject object holding an ID, dataset name, and an
      attribute dictionary"""

   _sort_key = None             # attribute key used in compare()
   _order    = 1                # 1 for normal, -1 for reverse

   def __init__(self, sid='', dset='', atrs=None):

      self.sid   = sid          # subject ID (string)
      self.dset  = dset         # dset name (string: existing filename)
      self.atrs  = None         # attributes (VarsObject instance)

      self.ddir  = '.'          # split dset name into directory and file
      self.dfile = ''

      dir, file = os.path.split(dset)   # and update them
      if dir: self.ddir = dir
      self.dfile = file

      # init to empty, and merge if something is passed
      self.atrs = VarsObject('subject %s' % sid)
      if atrs != None: self.atrs.merge(atrs)

   def show(self):
      natr = self.atrs.count()-1  # do not include name
      print("Subject %s, dset %s, natr = %d" % (self.sid, self.dset, natr))
      print("   ddir = %s\n   dfile = %s\n" % (self.ddir, self.dfile))
      if natr > 0:
         self.atrs.show('  attributes: ')

class SubjectList(object):
   """list of Subject elements, with attributes and command writing functions

        - can pass list of SIDs, dset names, attributes (VarsObject instances)
        - if any lists are past, the lengths must be equal

        - given a list of datasets (and no SIDs), try to extract SIDs
          by parsing into a regular expression

        - sort comparison should be by _sort_key (else sid), then by sid
   """

   def __init__(self, name='subject list', sid_l=None, dset_l=None, atr_l=None,
                verb=1):

      self.name         = name     # in case there are multiple lists in use
      self.subjects     = []       # list of Subject instances
      self.atrl         = []       # list of subject attributes (names only)
      self.disp_atrs    = []       # attributes to display, in order
      self.common_dir   = ''       # common parent dir to subject dsets
      self.common_dname = ''       # variable to apply for common_dir ($ddir)
      self.verb         = verb     # verbose level 
      self.status       = 0        # non-zero is bad 

      if sid_l == None and dset_l == None and atr_l == None: return

      # set the length, and check for consistency
      llen = - 1
      errs = 0
      for ilist in [sid_l, dset_l, atr_l]:
         if ilist == None: continue
         if llen < 0: llen = len(ilist)         # set, or
         elif llen != len(ilist): errs += 1     # ... test

      if errs > 0:
         print '** SubjectList init requires equal lengths (or None)'
         print '   sid_l  = %s' % sid_l
         print '   dset_l = %s' % dset_l
         print '   atr_l  = %s' % atr_l
         self.status = 1
         return

      if llen == 0: return      # empty lists?

      # okay, fill the subjects and atrs fields
      sid = ''
      dname = ''
      atr = None
      for ind in range(llen):
         if sid_l  != None: sid  = sid_l[ind]
         if dset_l != None: dset = dset_l[ind]
         if atr_l  != None: atr = atr_l[ind]
         self.add(Subject(sid=sid, dset=dset, atrs=atr))

   def copy(self, sid_l=[], atr='', atrval=None):
      """make a full copy of the SubjectList element

         start with a complete copy
         if len(sid_l)>0, remove subjects not in list
         if atr is not '', remove subjects for which atrs.atr!=atrval
      """
      # start with everything and then delete
      olen = len(newSL.subjects)
      newSL = copy.deepcopy(self)
      if len(sid_l) > 0:
         skeep = []
         snuke = []
         for subj in self.subjects:
            if subj.sid in sid_l: skeep.append(subj)
            else                : snuke.append(subj)
         if self.verb > 2:
            print '++ SL copy: nuking %d subjs not in sid list' % len(snuke)
         for subj in snuke: del(subj)
         newSL.subjects = skeep
      if atr != '':
         skeep = []
         snuke = []
         for subj in self.subjects:
            if subj.atrs.val(atr) == atrval: skeep.append(subj)
            else:                            snuke.append(subj)
         if self.verb>2: print '++ SL copy: nuking %d subjs with atr[%s] != %s'\
                               % (atr, atrval)
         for subj in snuke: del(subj)
         newSL.subjects = skeep
      if self.verb > 1: print '++ SL copy: keeping %d of %d subjects' \
                              % (len(newSL.subjects), olen)
      return newSL

   def show(self, mesg=''):
      if mesg: mstr = " (%s)" % mesg
      else:    mstr = ''
      print("SubjectList: %s%s" % (self.name, mstr))
      print("  nsubj = %d, natrs = %d, ndisp_atrs = %d" % \
               (len(self.subjects), len(self.atrl), len(self.disp_atrs)))
      print("  atrl      = %s" % self.atrl)
      print("  disp_atrs = %s" % self.disp_atrs)

      if len(self.subjects) == 0: return

      print("  subject sort key: %s" % self.subjects[0]._sort_key)
      for subj in self.subjects:
         subj.show()

   def add(self, subj):
      """add the subject to the list and update the atrl list"""

      for atr in subj.atrs.attributes():
         if not atr in self.atrl: self.atrl.append(atr)

      self.atrl.sort()

      self.subjects.append(subj)

   def set_common_data_dir(self, cname='data_dir'):
      """return the directory common to all subject ddir names"""
      cdir = UTIL.common_dir([s.dset for s in self.subjects])
      if UTIL.is_trivial_dir(cdir):
         self.common_dir   = ''
         self.common_dname = ''
      else:
         self.common_dir   = cdir
         self.common_dname = cname
         if self.verb > 1:
            print '++ setting common dir, %s = %s' % (cname, cdir)

   def set_ids_from_dsets(self, prefix='', suffix='', hpad=0, tpad=0):
      """use the varying part of the dataset names for subject IDs

         If hpad > 0 or tpad > 0, expand into the head or tail of the dsets.
         If prefix or suffix is passed, apply them.

         return 0 on success, 1 on error
      """

      if hpad < 0 or tpad < 0:
         print '** set_ids_from_dsets: will not apply negative padding'
         return 1
      dlist = [s.dset.split('/')[-1] for s in self.subjects]
      if UTIL.vals_are_constant(dlist):
         print '** constant dataset names (%s)' % dlist[0]
         print '   trying directories...'
         dlist = [s.dset for s in self.subjects]

      slist = UTIL.list_minus_glob_form(dlist, hpad, tpad)

      # in the case of diretories, check for success
      for val in slist:
         if '/' in val:
            print '** failed to extract subject IDs from directory list'
            print '   (directories do not vary at single level)'
            return 1

      if len(slist) != len(self.subjects):
         print '** failed to set SIDs from dset names\n'        \
               '   dsets = %s\n'                                \
               '   slist = %s' % (dlist, slist)
         return 1

      if not UTIL.vals_are_unique(slist):
         print '** cannot set IDs from dsets, labels not unique: %s' % slist
         return 1

      for ind, subj in enumerate(self.subjects):
         subj.sid = '%s%s%s' % (prefix, slist[ind], suffix)

      return 0

   def sort(self, key=None, order=1):
      if len(self.subjects) == 0: return
      Subject._sort_key = key     # None or otherwise
      Subject._order = order      # 1 for small first, -1 for reverse
      self.subjects.sort(cmp=subj_compare)

   def make_anova2_command(self, bsubs=None, prefix=None, options=None, verb=1):
      """create a basic 3dANOVA2 -type 3 command

         ** bsubs should be lists of strings, even if integral sub-bricks
            (they are applied as sub-brick selectors)
         
         attach options after subject lists

            bsubs          - beta sub-bricks (1 list of sub-brick selectors)
            prefix         - prefix for command output
            options        - other options added to the command
            verb           - verbose level

         return None on failure, command on success
      """

      if prefix == '' or prefix == None: prefix = 'anova2_result'
      if verb > 1: print '-- make_anova2_command: have prefix %s' % prefix

      if bsubs == None:
         print '** missing sub-brick selection list'
         return None
      if len(bsubs) < 2:
         print '** anova2_command: need at least 2 sub-bricks (have %d)' \
               % len(bsubs)
         return None

      indent = 9  # minimum indent: spaces to following -set option

      cmd   = '#!/bin/tcsh\n\n'

      # maybe we will use directory variables
      self.set_common_data_dir()
      if not UTIL.is_trivial_dir(self.common_dir):
         self.common_dname = 'data'
         cmd += '# apply any data directories with variables\n' \
               'set %s = %s\n' % (self.common_dname, self.common_dir)
         cmd += '\n'

      cmd += '# note: factor A is condition, B is subject\n\n'

      # command and first set of subject files
      cmd += '3dANOVA2 -type 3 \\\n' \
             '%s' %  self.make_anova2_set_list(bsubs, indent)

      if len(options) > 0: cmd += '%*s%s \\\n' % (indent,'', ' '.join(options))
      else:     # add some basic option
         opt = '-amean 1 amean1'
         cmd += '%*s%s \\\n' % (indent, '', opt)
         print '++ no contrast options given, adding simple: %s' % opt

      if prefix.find('/') >= 0: pp = prefix
      else:                     pp = './%s' % prefix
      cmd += '%*s-bucket %s\n' % (indent, '', pp)

      cmd += '\n'

      return cmd

   def make_anova3_command(self, bsubs=None, prefix=None, subjlists=None,
                           options=None, factors=[], verb=1):
      """create a basic 3dANOVA3 -type 5 command

         ** other types may be added later...

         ** bsubs should be lists of strings, even if integral sub-bricks
            (they are applied as sub-brick selectors)
         
         attach options after subject lists

            bsubs          - beta sub-bricks (1 list of sub-brick selectors)
            prefix         - prefix for command output
            subjlists      - len > 1 for type 5
            options        - other options added to the command
            atype          - 3dANOVA3 -type (should be 4 or 5)
            factors        - if type 4, #factors of each type (f0*f1 = len(b))
            verb           - verbose level

         Note: for type 5: factor A is group, B is condition, C is subject

         return None on failure, command on success
      """

      if prefix == '' or prefix == None: prefix = 'anova3_result'
      if verb > 1: print '-- make_anova2_command: have prefix %s' % prefix

      if bsubs == None:
         print '** missing sub-brick selection list'
         return None
      if len(bsubs) < 2:
         print '** anova3_command: need at least 2 sub-bricks (have %d)' \
               % len(bsubs)
         return None

      ncond = len(factors)
      ngroups = len(subjlists)

      atype = 0
      if ngroups > 1: atype = 5
      elif ncond == 2: atype = 4

      if atype == 4:
         if ngroups != 1:
            print '** anova3_cmd: -type 4 requires only 1 dset group'
            return None
         if ncond != 2:
            print '** anova3_cmd: -type 4 requires 2 factor lengths'
            print '               (product should be length -subs_betas)'
            return None
         if factors[0]*factors[1] != len(bsubs):
            print '** anova3_cmd: -type 4 factor mismatch'
            print '               (%d x %d design requires %d betas, have %d' \
                  % (factors[0], factors[1], factors[0]*factors[1], len(bsubs))
            return None
      elif atype == 5:
         if ngroups < 2:
            print '** anova3_cmd: -type 5 requires >= 2 subject lists'
            return None
         if ncond > 1:
            print '** anova3_cmd: -type 5 should not have sets of factors'
            return None
      else:
         print '** anova3_cmd: cannot detect -type 4 or -type 5, seek -help!'
         return None

      indent = 4  # indent after main command

      # maybe we will use directory variables
      cmd   = '#!/bin/tcsh\n\n'
      found = 0
      slen0 = len(subjlists[0].subjects)
      subjlists[0].set_common_data_dir()
      cd0 = subjlists[0].common_dir

      for ilist, slist in enumerate(subjlists):
         slist.set_common_data_dir()
         if not UTIL.is_trivial_dir(slist.common_dir):
            if not found: # first time found
               cmd += '# apply any data directories with variables\n'
               found = 1
            if ilist > 0 and slist.common_dir == cd0:
               slist.common_dname = 'data1'
            else:
               slist.common_dname = 'data%d' % (ilist+1)
               cmd += 'set %s = %s\n' % (slist.common_dname, slist.common_dir)
      if found: cmd += '\n'

      if atype == 4:
         cmd += '# note: factor A is cond 1, B is cond 2, C is subject\n\n'
         sfunc = self.make_anova3_t4_set_list
      else:
         cmd += '# note: factor A is group, B is condition, C is subject\n\n'
         sfunc = self.make_anova3_t5_set_list

      # command and first set of subject files
      cmd += '3dANOVA3 -type %d \\\n' \
             '%s' % (atype, sfunc(bsubs, subjlists, factors, indent))

      if len(options) > 0: cmd += '%*s%s \\\n' % (indent,'', ' '.join(options))
      else:     # add some basic options
         opt = '-amean 1 amean1 -bmean 1 bmean1'
         cmd += '%*s-amean 1 amean1 \\\n' \
                '%*s-bmean 1 bmean1 \\\n' % (indent, '', indent, '')
         print '++ no contrast options given, adding simple: %s' % opt

      if prefix.find('/') >= 0: pp = prefix
      else:                     pp = './%s' % prefix
      cmd += '%*s-bucket %s\n' % (indent, '', pp)

      cmd += '\n'

      return cmd

   def make_ttestpp_command(self, set_labs=None, bsubs=None, subjlist2=None,
                             prefix=None, comp_dir=None, options=None, verb=1):
      """create a basic 3dttest++ command

         Note: this is almost identical to make_mema_command, except for use 
               of tsubs.

         ** labs, bsubs should be lists of strings, even if they are integral
            sub-bricks

         if set_labs=None, use defaults depending on # of subject lists
         else, set_labs must have 1 or 2 elements, for 1 or 2 sets of subjects
         bsubs can be of length 1 even with 2 set_labs, in that case:
            - length 1: must have subjlist2 set, and apply to it
            - length 2: no subjlist2, use with subjlist 1
         attach options after subject lists

            set_labs       - set labels (None, 1 or 2 labels)
            bsubs          - beta sub-bricks (length 1 or 2, matching labels)
            subjlist2      - second subject list for 2-sample test (want if the
                             datasets differ across sets)
            prefix         - prefix for 3dtest++ output
            comp_dir       - comparison direction, either -AminusB or -BminusA
                             (if 2 sets)
            options        - other options added to the 3dtest++ command
            verb           - verbose level

         return None on failure, command on success
      """

      if prefix == '' or prefix == None: prefix = 'ttest++_result'
      if verb > 1: print '-- make_ttest++_command: have prefix %s' % prefix
      s2 = subjlist2    # sooooo much typing...

      if set_labs == None:
         if s2 == None: set_labs = ['setA']
         else:                 set_labs = ['setA', 'setB']
         if verb > 2: print '-- tt++_cmd: adding default set labels'
      if bsubs == None: bsubs, tsubs = ['0'], ['1']

      indent = 10  # minimum indent: spaces to following -set option

      # want any '-AminusB' option at top of command
      if len(set_labs) > 1: copt = '%*s%s \\\n' % (indent, '', comp_dir)
      else:                 copt = ''

      # maybe we will use directory variables
      self.set_common_data_dir()
      if not UTIL.is_trivial_dir(self.common_dir):
         if s2 != None:
            s2.set_common_data_dir()
            if UTIL.is_trivial_dir(s2.common_dir):
               # then do not use either
               self.common_dir = ''
               self.common_dname = ''
            # else, use both (if same, default variable is okay)
            elif self.common_dir != s2.common_dir:
               # different, so update the names to be different
               self.common_dname = 'data1'
               s2.common_dname = 'data2'

      cmd = ''
      if not UTIL.is_trivial_dir(self.common_dir):
         cmd += '# apply any data directories with variables\n' \
               'set %s = %s\n' % (self.common_dname, self.common_dir)
         if s2 != None:
            if not UTIL.is_trivial_dir(s2.common_dir) \
               and s2.common_dir != self.common_dir:
               cmd += 'set %s = %s\n' % (s2.common_dname, s2.common_dir)
         cmd += '\n'

      # command and first set of subject files
      cmd += '3dttest++ -prefix %s \\\n'    \
             '%s'                           \
             '          -setA %s \\\n%s' %  (prefix, copt, set_labs[0],
                                self.make_ttpp_set_list(bsubs[0], indent+3))

      # maybe add second set of subject files
      if len(set_labs) > 1:
         if verb > 2: print '-- tt++_cmd: have labels for second subject set'

         # separate tests for bad test types
         if comp_dir == None:
            print '** make_tt++_cmd: missing test type, should be in %s' \
                  % g_ttpp_tests
            return      # failure
         if comp_dir not in g_ttpp_tests:
            print '** make_tt++_cmd: comp_dir (%s) must be in the list %s' \
                  % (comp_dir, g_ttpp_tests)
            return      # failure

         # note subject list and sub-brick labels
         if s2 != None:
            S = s2
            if verb > 2: print '-- second subject list was passed'
         else:
            S = self
            if verb > 2: print '-- no second subject list, using same list'
         if len(bsubs) > 1: b = bsubs[1]
         else:
            if S != s2:
               print '** make_tt++_cmd: same subject list in comparison'
            b = bsubs[0]
         cmd += '%*s-setB %s \\\n%s' % \
                (indent, ' ', set_labs[1], S.make_ttpp_set_list(b, indent+3))

      if len(options) > 0: cmd += '%*s%s' % (indent, '', ' '.join(options))
      cmd += '\n\n'

      return cmd

   def make_mema_command(self, set_labs=None, bsubs=None, tsubs=None,
                         subjlist2=None, prefix=None, ttype=None, options=None,
                         verb=1):
      """create a basic 3dMEMA command

         ** labs, bsubs, tsubs should be lists of strings, even if they are
            integral sub-bricks

         if set_labs=None, use defaults depending on # of subject lists
         else, set_labs must have 1 or 2 elements, for 1 or 2 sets of subjects
         bsubs and tsubs can be of length 1 even with 2 set_labs, in that case:
            - length 1: must have subjlist2 set, and apply to it
            - length 2: no subjlist2, use with subjlist 1
         attach options after subject lists

            set_labs       - set labels (None, 1 or 2 labels)
            bsubs          - beta sub-bricks (length 1 or 2, matching labels)
            tsubs          - t-stat sub-bricks (as with bsubs)
            subjlist2      - second subject list for 2-sample test (want if the
                             datasets differ across sets)
            prefix         - prefix for 3dMEMA output
            'ttype'        - used for a 2-sample test, to distinguish between
                             paired and unpaired  (results is using either
                             -conditions (paired) or -group (un-))
            options        - other options added to the 3dMEMA command
            verb           - verbose level

         return None on failure, command on success
      """

      if prefix == '' or prefix == None: prefix = 'mema_result'
      if verb > 1: print '++ make_mema_cmd: have prefix %s' % prefix
      s2 = subjlist2    # sooooo much typing...

      if set_labs == None:
         if s2 == None: set_labs = ['setA']
         else:                 set_labs = ['setA', 'setB']
         if verb > 2: print '++ mema_cmd: adding default set labels'
      if bsubs == None: bsubs, tsubs = ['0'], ['1']

      # maybe we will use directory variables
      self.set_common_data_dir()
      if not UTIL.is_trivial_dir(self.common_dir):
         if s2 != None:
            s2.set_common_data_dir()
            if UTIL.is_trivial_dir(s2.common_dir):
               # then do not use either
               self.common_dir = ''
               self.common_dname = ''
            # else, use both (if same, default variable is okay)
            elif self.common_dir != s2.common_dir:
               # different, so update the names to be different
               self.common_dname = 'data1'
               s2.common_dname = 'data2'

      cmd = ''
      if not UTIL.is_trivial_dir(self.common_dir):
         cmd += '# apply any data directories with variables\n' \
               'set %s = %s\n' % (self.common_dname, self.common_dir)
         if s2 != None:
            if not UTIL.is_trivial_dir(s2.common_dir) \
               and s2.common_dir != self.common_dir:
               cmd += 'set %s = %s\n' % (s2.common_dname, s2.common_dir)
         cmd += '\n'

      # command and first set of subject files
      cmd += '3dMEMA -prefix %s \\\n'    \
             '       -set %s \\\n%s' %   \
             (prefix,set_labs[0],self.make_mema_set_list(bsubs[0],tsubs[0],10))

      # maybe add second set of subject files
      if len(set_labs) > 1:
         if verb > 2: print '-- mema_cmd: have labels for second subject set'
         # note subject list and sub-brick labels
         if s2 != None:
            S = s2
            if verb > 2: print '-- second subject list was passed'
         else:
            S = self
            if verb > 2: print '-- no second subject list, using same list'
         if len(bsubs) > 1: b, t = bsubs[1], tsubs[1]
         else:
            if S != s2:
               print '** make_mema_cmd: same subject list in comparison'
            b, t = bsubs[0], tsubs[0]
         cmd += '%7s-set %s \\\n%s' % \
                (' ', set_labs[1], S.make_mema_set_list(b, t, 10))

         # either 2-sample or paired
         if ttype not in g_mema_tests:
            print "** invalid 3dMEMA test %s, not in %s" % (ttype,g_mema_tests)
            return None
         if ttype == 'paired': opt = '-conditions'
         else:                 opt = '-groups'
         cmd += '%7s%s %s %s \\\n' % ('', opt, set_labs[0], set_labs[1])

      if len(options) > 0: cmd += '%7s%s' % ('', ' '.join(options))
      cmd += '\n\n'

      return cmd

   def make_mema_set_list(self, bsub, tsub, indent=0):
      """return a multi-line string of the form:
                SID1 "dset1[bsub]"
                     "dset1[tsub]"
                SID2 "dset2[bsub]"
                     "dset2[tsub]"
                ...
         indent is the initial indentation
      """
      # note the max subject ID length
      ml = 0
      for subj in self.subjects:
         if len(subj.sid) > ml: ml = len(subj.sid)

      if not UTIL.is_trivial_dir(self.common_dir) and self.common_dname:
         sdir = self.common_dname
      else: sdir = ''

      sstr = ''
      for subj in self.subjects:
         if sdir:
            # see if the dataset is in a directory underneath
            cdir = UTIL.child_dir_name(self.common_dir, subj.ddir)
            if UTIL.is_trivial_dir(cdir): cstr = ''
            else: cstr = '%s/' % cdir
            dset = '$%s/%s%s' % (sdir, cstr, subj.dfile)
         else:    dset = subj.dset
         sstr += '%*s%s "%s[%s]" \\\n%*s "%s[%s]" \\\n' % \
                 (indent,    '', subj.sid, dset, bsub,
                  indent+ml, '',           dset, tsub)
      return sstr

   def make_ttpp_set_list(self, bsub, indent=0):
      """return a multi-line string of the form:
                SID1 "dset1[bsub]"
                SID2 "dset2[bsub]"
                ...
         indent is the initial indentation
      """
      # note the max subject ID length
      ml = 0
      for subj in self.subjects:
         if len(subj.sid) > ml: ml = len(subj.sid)

      if not UTIL.is_trivial_dir(self.common_dir) and self.common_dname:
         sdir = self.common_dname
      else: sdir = ''

      sstr = ''
      for subj in self.subjects:
         if sdir:
            # see if the dataset is in a directory underneath
            cdir = UTIL.child_dir_name(self.common_dir, subj.ddir)
            if UTIL.is_trivial_dir(cdir): cstr = ''
            else: cstr = '%s/' % cdir
            dset = '$%s/%s%s' % (sdir, cstr, subj.dfile)
         else:    dset = subj.dset
         sstr += '%*s%s "%s[%s]" \\\n' % (indent, '', subj.sid, dset, bsub)

      return sstr

   def make_anova2_set_list(self, bsub, indent=0):
      """return a multi-line string of the form:
                -alevels #bsub
                -blevels #subj
                -dset ALEVEL BLEVEL "dset#A[bsub#B]"
                ...
         indent is the initial indentation
      """
      sdir = self.common_dname
      sstr = '%*s-alevels %d \\\n' \
             '%*s-blevels %d \\\n' \
             % (indent,'', len(bsub), indent, '', len(self.subjects))

      for isubj, subj in enumerate(self.subjects):
         if sdir:
            # see if the dataset is in a directory underneath
            cdir = UTIL.child_dir_name(self.common_dir, subj.ddir)
            if UTIL.is_trivial_dir(cdir): cstr = ''
            else: cstr = '%s/' % cdir
            dset = '$%s/%s%s' % (sdir, cstr, subj.dfile)
         else: dset = subj.dset
         for ibeta, beta in enumerate(bsub):
            sstr += '%*s-dset %2d %2d "%s[%s]" \\\n' \
                    % (indent, '', ibeta+1, isubj+1, dset, beta)

      return sstr

   def make_anova3_t4_set_list(self, bsub, subjlists, factors, indent=0):
      """return a multi-line string of the form:
                -alevels #alevels
                -blevels #blevels
                -clevels #subj
                -dset ALEVEL BLEVEL SUBJ "dset#A[bsub#B]"
                ...
         - factors should be of length 2
         - indent is the initial indentation
         - as in type 5, have A change slower than B, but subj be slowest
           (so factor order per subject matches command line)
      """

      errs  = 0

      if len(subjlists) != 1:
         print '** MAt4SL: bad subject list count = %d' % len(subjlists)
         return None

      nA = factors[0]
      nB = factors[1]
      slist = subjlists[0]
      if nA*nB != len(bsub):
         print '** MAt4SL: bad factor count: %d, %d, %d' % (nA, nB, len(bsub))
         return None

      sstr = ''
      sstr += '%*s-alevels %d \\\n' % (indent, '', nA)
      sstr += '%*s-blevels %d \\\n' % (indent, '', nB)
      sstr += '%*s-clevels %d \\\n' % (indent, '', len(slist.subjects))

      sdir = slist.common_dname
      for isubj, subj in enumerate(slist.subjects):
         if sdir:
            # see if the dataset is in a directory underneath
            cdir = UTIL.child_dir_name(slist.common_dir, subj.ddir)
            if UTIL.is_trivial_dir(cdir): cstr = ''
            else: cstr = '%s/' % cdir
            dset = '$%s/%s%s' % (sdir, cstr, subj.dfile)
         else: dset = subj.dset

         for iA in range(nA):
            for iB in range(nB):
               sstr += '%*s-dset %2d %2d %2d "%s[%s]" \\\n' \
                       % (indent, '', iA+1, iB+1, isubj+1, dset,
                          bsub[iA*nB+iB])

      if errs: return None

      return sstr

   def make_anova3_t5_set_list(self, bsub, subjlists, factors=0, indent=0):
      """return a multi-line string of the form:
                -alevels #subjlists
                -blevels #bsub
                -clevels #subj
                -dset GROUP BLEVEL SUBJ "dset#A[bsub#B]"
                ...
         factors is ignored, and exists only to match type4 function
         indent is the initial indentation
      """
      sstr = ''
      sstr += '%*s-alevels %d \\\n' % (indent, '', len(subjlists))
      sstr += '%*s-blevels %d \\\n' % (indent, '', len(bsub))
      sstr += '%*s-clevels %d \\\n' % (indent, '', len(subjlists[0].subjects))

      slen0 = len(subjlists[0].subjects)
      errs  = 0
      for ilist, slist in enumerate(subjlists):
         if len(slist.subjects) != slen0:
            print '** subject list %d length differs from SL 1 (%d != %d)\n'\
                  (ilist+1, len(slist.subjects), slen0)
            errs += 1
         sdir = slist.common_dname

         for isubj, subj in enumerate(slist.subjects):
            if sdir:
               # see if the dataset is in a directory underneath
               cdir = UTIL.child_dir_name(slist.common_dir, subj.ddir)
               if UTIL.is_trivial_dir(cdir): cstr = ''
               else: cstr = '%s/' % cdir
               dset = '$%s/%s%s' % (sdir, cstr, subj.dfile)
            else: dset = subj.dset
            for ibeta, beta in enumerate(bsub):
               sstr += '%*s-dset %2d %2d %2d "%s[%s]" \\\n' \
                       % (indent, '', ilist+1, ibeta+1, isubj+1, dset, beta)

      if errs: return None

      return sstr

if __name__ == '__main__':
   print '** this is not a main program'

