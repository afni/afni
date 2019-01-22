#!/usr/bin/env python

# python3 status: started

import sys, os
import copy

# whine about execution as a main program
if __name__ == '__main__':
   print('** %s: not a main program' % sys.argv[0].split('/')[-1])
   sys.exit(1)


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

   def attributes(self, getall=0, name=1):
      """same as dir(), but return only those with:
            - a name not starting with '_'
            - a simple type
            - if not name, not 'name'
         if getall: will be more like dir()
      """
      dlist = dir(self)
      retlist = []
      for atr in dlist:
         if atr[0] == '_': continue
         if self.get_atomic_type(atr) == None and not getall: continue
         if atr == 'name' and not name: continue
         retlist.append(atr)
      retlist.sort()
      return retlist

   def get_attribute_dict(self, getall=0):
      dupe = {}
      for atr in self.attributes():
         if self.get_atomic_type(atr) == None and not getall: continue
         val = self.valcopy(atr)
         dupe[atr] = val
      return dupe

   def copy(self, name=None, as_strings=0):
      """return a copy of this class item by creating a new instance
         and copying all simple attributes

         if as_strings: copy any simple type (int, float) or depth 1 list
         as string
      """

      dupe = VarsObject()
      for atr in self.attributes():
         if as_strings: val = self.copy_2_str(getattr(self, atr))
         else:          val = self.valcopy(atr)
         setattr(dupe, atr, val)

      if name: dupe.name = name

      return dupe

   def copy_2_str(self, value):
      """copy value as a string, using recursion for lists    16 Oct 2012

         if int or float, return string version
         else if list, return string list from recursive calls
         else, return value
      """
      tv = type(value)
      if   tv == int:   return '%s' % value
      elif tv == float: return '%s' % value
      elif tv == list:  return [self.copy_2_str(lv) for lv in value]
      else:             return value

   def merge(self, v2, typedef=None, verb=1):
      """merge in attributes from v2
         if typedef is a VarsObject, convert any simple type atrs based on it
      """

      if v2 == None: return

      if type(v2) != VarsObject:
         print("** trying to merge %s with VarsObject" % type(v2))
         return

      if typedef != None:
         if type(typedef) != VarsObject:
            print("** invalid object used for typedef in merge")
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
                  print("** failed to merge %s '%s' as int" % (atr, newval))
                  val = newval
            elif dtype == float:
               try: val = float(newval)
               except:
                  print("** failed to merge %s '%s' as float" % (atr, newval))
                  val = newval

         setattr(self, atr, val)

         if verb > 1:
            print("== merge: setting %s %s = %s" % (atr, dtype, val))

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
         print('** SVWD: invalid %s variable: %s' % (oname, vname))
         return -1

      dtype = type(defs.val(vname))
      if dtype not in g_valid_atomic_types:
         print('** SVWD: unknown %s variable type for %s' % (oname, vname))
         return -1

      # if simple type but have list, fail
      if dtype != list and len(vlist) > 1:
         print("** SVWD: simple variable '%s' %s\n" \
               "         but have list value: %s" % (vname, dtype, vlist))
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
            print('** SVWD %s.%s, cannot convert value %s to %s' \
                  (oname, vname, val, dtype))
            return -1
         # possibly apply the defs type
         if as_type: val = vv
      elif dtype == list: val = vlist
      else: 
         print('** SVWD: invalid type %s for %s'%(dtype,vname))
         return -1

      # actually set the value
      rv = self.set_var(vname, val)
      if verb > 1:
         if rv: print('++ %s: updating %s to %s %s' \
                      % (oname, vname, val, type(val)))
         else:  print('++ %s: no update for %s to %s' % (oname, vname, val))

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
         print("** attribute '%s' is not simple, copy may be bad" % atr)

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
         print('** vals_are_equal: no VarsObject')
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

   def changed_attrs_str(self, checkobj, skiplist=[], showskip=1, showdel=1):
      """return a string that lists differences between self and vobj

         do not report those in skiplist

         for each changed_attrs()
            if in skiplist: continue
            else list
         for each changed in skiplist (and not 'name')
            list

         return a printable string
      """
      rlist = []

      # start with options (things not in skiplist)
      clist = self.changed_attrs(checkobj)
      acount = 0
      if len(clist) > 0:
         for attr in clist:
            if attr in skiplist: continue
            acount += 1
            if self.get_type(attr) == list:
               # show list if short enough
               lstr = ' '.join(self.val(attr))
               if len(lstr)>52: lstr='[list of %d elements]'%self.val_len(attr)
               rlist.append('  %-20s : %s' % (attr,lstr))
            else: rlist.append('  %-20s : %s' % (attr, self.val(attr)))
         if acount > 0:
            rlist.insert(0, 'options changed from defaults (%d):\n' % acount)
            rlist.append('')
         else:
            rlist.insert(0, 'options: using all defaults\n')

      # now go after ONLY skiplist attrs (these are not as options)
      acount = 0
      nlist = []
      if showskip and len(skiplist) > 0 and len(clist) > 0:
         for attr in clist:
            if attr not in skiplist: continue
            if attr == 'name': continue
            acount += 1
            if self.get_type(attr) == list:
               # show list if short enough
               lstr = ' '.join(self.val(attr))
               if len(lstr)>52: lstr='[list of %d elements]'%self.val_len(attr)
               nlist.append('  %-20s : %s' % (attr,lstr))
            else: nlist.append('  %-20s : %s' % (attr, self.val(attr)))
         if acount > 0:
            nlist.insert(0, 'applied subject variables (%d):\n' % acount)
         else:
            nlist.insert(0, '** no subject variables set?\n')
         nlist.append('')
         rlist.extend(nlist)

      if showdel:
         clist = self.deleted_attrs(checkobj)
         if len(clist) > 0:
            rlist.append('deleted vars (%d):\n' % len(clist))
            for attr in clist:
               rlist.append('  %s' % attr)
            rlist.append('')

      if len(rlist) == 0: return '** using all defaults'

      return '\n'.join(rlist)

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
         print("** valid_atr_type: 'atr' must be passed as a string")
         return 0

      # make sure atype is valid as well
      if not atype in [float, int, str]:
         print("** valid_atr_type: invalid atype %s" % atype)
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
      print(self.make_show_str(mesg=mesg, prefix=prefix, pattern=pattern,
                               name=name, all=all))

   def make_show_str(self, mesg='', prefix=None, pattern=None, name=1, all=0):
      """if all, put atomic attributes first and instancemethods last"""

      if prefix: pstr = ", prefix = '%s'" % prefix
      else:      pstr = ''

      sstr = "-- %s values in var '%s'%s\n" % (mesg, self.name, pstr)

      # start with atomic only, though loop is almost identical
      for atr in self.attributes(getall=all, name=name):
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
         for atr in self.attributes(getall=all, name=name):
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
         for atr in self.attributes(getall=all, name=name):
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

