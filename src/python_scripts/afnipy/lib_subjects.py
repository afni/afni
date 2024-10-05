#!/usr/bin/env python

# python3 status: compatible

import sys, os
import copy, glob
from afnipy import afni_util as UTIL
from afnipy import lib_vars_object as VO

g_mema_tests = [None, 'paired', 'unpaired']
g_ttpp_tests = ['-AminusB', '-BminusA']

# atomic (type within nested list) and simple types for VarsObject
g_valid_atomic_types = [int, float, str, list]
g_simple_types = [int, float, str]
g_subject_sort_key = None

def comment_section_string(comment, length=70, cchar='-'):
   """return a string of the form:
      # -------------- some comment --------------
      where the total length is given
   """
   clen = len(comment)
    
   ndash = (length - clen - 4) // 2     # one '#' and 3 spaces

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

def subj_compare_key(subj):
   """subj_compare() will no longer be valid in python3, as sort() will
      lose the cmp= attribute.  Generate a key that would be equivalent,
      and just use reverse() after the fact, if desired.

      The key is now stored in g_subject_sort_key.

      Return a [key, sid] pair (or sid, if key==None).
   """
   if g_subject_sort_key == None:
      return subj.sid

   if g_subject_sort_key in subj.atrs:
      val = subj.atrs[g_subject_sort_key]
   else:
      val = None

   return [val, subj.sid]
   
def subj_compare(subj0, subj1):
   """compare 2 Subject objects:        used for sorting a list of subjects
         if _sort_key is set, compare by key, then by sid
         else, compare by sid
   """

   cval = 0
   key = subj0._sort_key
   if key != None:
      if key in subj0.atrs: v0 = subj0.atrs[key]
      else: v0 = None
      if key in subj1.atrs: v1 = subj1.atrs[key]
      else: v1 = None
      cval = cmp(v0, v1)

   # remove subj0._order, as compare must be replaced by key
   if cval != 0: return cval
   return cmp(subj0.sid, subj1.sid)

def set_var_str_from_def(obj, name, vlist, vobj, defs,
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
      print('** invalid %s variable: %s' % (obj, name))
      return -1

   dtype = type(defs.val(name))
   if dtype not in g_valid_atomic_types:
      print('** SVSFD: unknown %s variable type for %s' % (obj, name))
      return -1

   # if simple type but have list, fail
   if dtype != list and len(vlist) > 1:
      print("** SVSFD: simple variable '%s' %s\n" \
            "          but have list value: %s" % (name, dtype, vlist))
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
         print('** SVSFD %s.%s, cannot convert value %s to %s' \
               % (obj, name, val, dtype))
         return -1
   elif dtype == list: val = vlist
   else: 
      print('** SVSFD: invalid type %s for %s'%(dtype,name))
      return -1

   # actually set the value
   rv = vobj.set_var(name, val)
   if verb > 1:
      if rv: print('++ %s: updating %s to %s %s' % (obj, name, val, type(val)))
      else:  print('++ %s: no update for %s to %s' % (obj, name, val))

   # if no update, we're outta here
   if rv == 0: return rv

   # ----------------------------------------------------------------------
   # handle some special cases, such as indices and labels, which might
   # come with file name lists

   # this function must be passed, since it will vary per library

   if spec != None: spec(name, vobj, check_sort=csort)

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
         print('** failed makedirs(%s)' % dname)
         return ''

   # now try to go there
   try: os.chdir(dname)
   except:
      print('** failed to go to process dir, %s' % dname)
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
      print('** failed to return to %s from process dir' % rname)

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
      print('** found non-int VAL in %s/%s.VAL.*' % (tdir, prefix))
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
      self.maxlinelen = 0       # if set, maximum line len for subj in command
                                # rcr - todo

      dir, file = os.path.split(dset)   # and update them
      if dir: self.ddir = dir
      self.dfile = file

      # init to empty, and merge if something is passed
      self.atrs = VO.VarsObject('subject %s' % sid)
      if atrs != None: self.atrs.merge(atrs)

   def show(self):
      natr = self.atrs.count()-1  # do not include name
      print("Subject %s, natr = %d" % (self.sid, natr))
      print("   dset = %s" % self.dset)
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
      llen = -1
      errs = 0
      for ilist in [sid_l, dset_l, atr_l]:
         if ilist == None: continue
         if llen < 0: llen = len(ilist)         # set, or
         elif llen != len(ilist): errs += 1     # ... test

      if errs > 0:
         print('** SubjectList init requires equal lengths (or None)')
         print('   sid_l  = %s' % sid_l)
         print('   dset_l = %s' % dset_l)
         print('   atr_l  = %s' % atr_l)
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
            print('++ SL copy: nuking %d subjs not in sid list' % len(snuke))
         for subj in snuke: del(subj)
         newSL.subjects = skeep
      if atr != '':
         skeep = []
         snuke = []
         for subj in self.subjects:
            if subj.atrs.val(atr) == atrval: skeep.append(subj)
            else:                            snuke.append(subj)
         if self.verb>2: print('++ SL copy: nuking %d subjs with atr[%s] != %s'\
                               % (atr, atrval))
         for subj in snuke: del(subj)
         newSL.subjects = skeep
      if self.verb > 1: print('++ SL copy: keeping %d of %d subjects' \
                              % (len(newSL.subjects), olen))
      return newSL

   def show(self, mesg='', verb=-1):
      if verb == -1: verb = self.verb

      if mesg: mstr = " (%s)" % mesg
      else:    mstr = ''
      print("SubjectList: %s%s" % (self.name, mstr))
      print("  nsubj = %d, natrs = %d, ndisp_atrs = %d" % \
               (len(self.subjects), len(self.atrl), len(self.disp_atrs)))
      print("  common_dir   = %s" % self.common_dir)
      print("  common_dname = %s" % self.common_dname)
      print("  atrl         = %s" % self.atrl)
      print("  disp_atrs    = %s" % self.disp_atrs)

      if len(self.subjects) == 0: return

      print("  subject sort key: %s" % self.subjects[0]._sort_key)
      if verb > 1:
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
      if UTIL.is_trivial_dir(cdir) or (len(cdir) < len(cname)):
         self.common_dir   = ''
         self.common_dname = ''
      else:
         self.common_dir   = cdir
         self.common_dname = cname
         if self.verb > 1:
            print('++ setting common dir, %s = %s' % (cname, cdir))

   def set_ids_from_dsets(self, prefix='', suffix='', hpad=0, tpad=0, dpre=2):
      """use the varying part of the dataset names for subject IDs

         If hpad > 0 or tpad > 0, expand into the head or tail of the dsets.
         If prefix or suffix is passed, apply them.

         return 0 on success, 1 on error
      """

      if hpad < 0 or tpad < 0:
         print('** set_ids_from_dsets: will not apply negative padding')
         return 1

      # try filenames without paths, first
      dlist = [s.dset.split('/')[-1] for s in self.subjects]
      if UTIL.vals_are_constant(dlist):
         print('** constant dataset names (%s)' % dlist[0])
         print('   trying directories...')
         dlist = [s.dset for s in self.subjects]

      slist = UTIL.list_minus_glob_form(dlist, hpad, tpad, keep_dent_pre=dpre)

      # in the case of directories, check for success
      # (maybe we can try to skip past them, that might be okay)
      for index in range(len(slist)):
         if '/' in slist[index]:
            posn = slist[index].rfind('/')
            slist[index] = slist[index][posn+1:]
            if len(slist[index]) < 1:
               print('** failed to extract subject IDs from directory list')
               print('   (directories do not vary at single level)')
               return 1

      if len(slist) != len(self.subjects):
         print('** failed to set SIDs from dset names\n'        \
               '   dsets = %s\n'                                \
               '   slist = %s' % (dlist, slist))
         return 1

      if not UTIL.vals_are_unique(slist):
         print('** cannot set IDs from dsets, labels not unique: %s' % slist)
         print('-- labels come from dsets: %s' % dlist)
         return 1

      for ind, subj in enumerate(self.subjects):
         subj.sid = '%s%s%s' % (prefix, slist[ind], suffix)

      return 0

   def restrict_ids_to_dsets(self, valid_ids=[], require=1):
      """restrict subject IDs to those in valid_ids list
         require all valid_ids to exist, or fail

         return 0 on success
      """
      # bail if either list is empty
      if len(self.subjects) == 0: return 0
      if len(valid_ids) == 0: return 0

      # check that valid_ids are unique
      if not UTIL.vals_are_unique(valid_ids):
         print('** restrict_ids: ids are not unique')
         return 1

      # check that all valid_ids exist, and generate new subject list
      all_ids = [subj.sid for subj in self.subjects]

      new_subjs = []
      missing = 0
      missed_id = ''    # example of missing ID
      for sid in valid_ids:
         if sid in all_ids:
            old_index = all_ids.index(sid)
            new_subjs.append(self.subjects[old_index])
         else:
            if self.verb > 1:
               print("** restrict_ids: cannot restrict to missing ID '%s'"%sid)
            missed_id = sid
            missing += 1
      if missing:
         print("** restrict_ids: missing %d of %d IDs" \
               % (missing,len(valid_ids)))
         print("   IDs look like: %s" % ' '.join(all_ids[:3]))
         print("   missing IDs look like: %s" % missed_id)
         if require:
            return 1
         else:
            print("-- restrict_ids: allowing %d missing IDs..." % missing)

      # apply restricted list
      self.subjects = new_subjs

      return 0

   def remove_ids_from_dsets(self, remove_ids=[], require=1):
      """restrict subject IDs to those not in remove_ids list
         if require: require all remove_ids to exist, or fail

         return 0 on success
      """
      # bail if either list is empty
      if len(self.subjects) == 0: return 0
      if len(remove_ids) == 0: return 0

      # check that remove_ids are unique
      if not UTIL.vals_are_unique(remove_ids):
         print('** remove_ids: ids are not unique')
         return 1

      # check that all remove_ids exist, and fail if not
      all_ids = [subj.sid for subj in self.subjects]
      missing = 0
      for sid in remove_ids:
         if sid not in all_ids:
            if self.verb > 1:
               print("** remove_ids: cannot remove missing ID '%s'"%sid)
            missed_id = sid
            missing += 1
      if missing and (require or self.verb > 1):
         print("** remove_ids: missing %d of %d IDs" \
               % (missing,len(remove_ids)))
         print("   IDs look like: %s" % ' '.join(all_ids[:3]))
         print("   missing IDs look like: %s" % missed_id)
         # if required, this is fatal
         if require:
            return 1
         else:
            print("-- remove_ids: allowing %d missing IDs..." % missing)

      # generate a new subject list
      new_subjs = []
      for sindex, sid in enumerate(all_ids):
         if sid not in remove_ids:
            new_subjs.append(self.subjects[sindex])

      # apply remove list
      self.subjects = new_subjs

      return 0

   def sort(self, key=None, order=1):
      if len(self.subjects) == 0: return
      # sort() has no cmp keyword in python3, use key method
      # Subject._sort_key = key     # None or otherwise
      # Subject._order = order      # 1 for small first, -1 for reverse
      # self.subjects.sort(cmp=subj_compare)

      g_subject_sort_key = key
      self.subjects.sort(key=subj_compare_key)
      if order < 0: self.subjects.reverse()

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
      if verb > 1: print('-- make_anova2_command: have prefix %s' % prefix)

      if bsubs == None:
         print('** missing sub-brick selection list')
         return None
      if len(bsubs) < 2:
         print('** anova2_command: need at least 2 sub-bricks (have %d)' \
               % len(bsubs))
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
         print('++ no contrast options given, adding simple: %s' % opt)

      if prefix.find('/') >= 0: pp = prefix
      else:                     pp = './%s' % prefix
      cmd += '%*s-bucket %s\n' % (indent, '', pp)

      cmd += '\n'

      return cmd

   def make_datatable_text(self, subjlists, condlists=[], bsubs=None, 
                           tsvfile='', sep='  ', shell=0, verb=1):
      """create text for a -dataTable file

            subjlists      - one list, or one list per condition
                           * in condition-major order (first cond is slowest)
          * condlists      - NEW: if present, 1 list per factor, of that length
                           - option (multiple): -factors SET NAME1 NAME2 ...
                           - cond-major order, first moves slowest, last fastest
            bsubs          - beta sub-bricks (1 list of sub-brick selectors)
            tsvfile        - TSV-style file that to be used to:
                             1. restrict subjects (use only matching subjects)
                             2. include table line in output datatable
                             ** MUST have a header line, and subjects first
            sep            - column separator for table
                             (if has tab, no uniformity)
            shell          - if shell form, include line wrapper
            verb           - verbose level

    *** decide on -dsets (subjlists), -factors (factors), -subs_betas (bsubs)

        - make a fully factorialized condition table
           - must match order of -dsets and condition-major order labels
           - there will be one such table of rows, per subject
             (unless that subject is missing data for specific factor levels)
        - ntcond = total number of conditions = product of condition lengths
                 = length of full condition table

        - make a complete list of all subjects (across all conditions)

        - create complete datatable
           - for each subject
              - for each cond table row
                 - if that subject is in the row, add that dataset
                   (or if just one set of subject datasets)

                   SUBJ  factors ... for this row (cond table row)  DSET

        - if ntcond == 1:
          - must have len(subjlists) == 1
          - omit any condition column(s)
          - do not need condlists
          - bsubs can be empty or have 1 value

        - if len(subjlists) == 1: all conditions in one dset
             - so len(bsubs) must equal ntcond
        - else: len(subjlists) > 1: one condition/beta per subj list
             - must have len(subjlists) == ntcond
             - if len(bsubs) == 0: omit sub-brick selection
             - else if len(bsubs) == 1: use same selector for every volume
             - else: must have: len(bsubs) == ntcond == len(subjlists)

      todo: resulting format len(factors) rows per subject:
                subj    factor  input
      todo: ponder data path (since no variable can be in table)
            - ponder set_common_dir
      todo: import table from Justin
            - convert to subject dictionary
            - be sure each such subject is in full list
            - restrict union subject list to this table
            - insert these columns into the full table

         return None on failure, -dataTable on success
      """

      if verb > 1:
         print('-- make_datatable_text, %d slist, %d clists' \
               % (len(subjlists), len(condlists)))

      if bsubs == None: pass # okay here
      if len(subjlists) == 0:
         print("** make_datatable: have no subject lists")
         return None

      dtable = self.make_datatable(subjlists, condlists=condlists,
                       bsubs=bsubs, tsvfile=tsvfile, verb=verb)
      if len(dtable) == 0:
         if verb > 1: print("** failed make_datatable")
         return ''

      # to make this pretty, dupe table based on max col lengths
      # (change any \t character pairs to actual tabs)
      # (if tab in sep, do not make uniform)
      sep = sep.replace('\\t', '\t')
      if '\t' in sep:
         dunif = dtable
      else:
         dunif = _make_uniform_col_widths(dtable)

      lines = []
      if shell: wstr = '%s\\' % sep
      else:     wstr = ''
      for row in dunif:
         lines.append(sep.join(row) + wstr)

      return '\n'.join(lines)

   def make_datatable(self, subjlists, condlists=[], bsubs=None, tsvfile='',
                      verb=1):
      """return a 2-D list: subject x condition (N == product of condl lengths)
         ([] on failure)

            subjlists   : array of SubjectList instances (for IDs and dsets)
            condlists   : lists of [TYPE, c1, c2, ...] factor lists
            bsubs       : beta sub-brick labels for the factor combinations
            tsvfile     : file name for Subj and extra subject columns
            verb        : verbosity level (def 1)

         - get complete, sorted list of subjects
         - if tsvfile, restrict complete list to those in tsv
         - convert subjlists to list of dictionaries { subj:dataset }
         - get fully factorialized condition table
            - ntcond = total number of cases = product of condition lengths

         - verify condition table size against len(bsubs) len(subjlists)

         - create complete datatable
            - each subject has ntcond rows, where they have data
              row:  SUBJ  TSVDATA  factors for this row (cond table row)  DSET
                    SUBJ    : one of subj_all
                    TSVDATA : if tsvfile, include subject columns
                    factors : all factors from condlists
                    DSET    : actual input DSET
      """

      if verb > 1: print('-- make_datatable' )

      if len(subjlists) == 0:
         print("** make_datatable: have no subject lists")
         return []

      # create full union subject list
      subj_all = self.get_all_subjs_from_lists(subjlists, verb=verb)
      if len(subj_all) == 0:
         print("** make_datatable: failed get_all_subjs_from_lists")
         return []

      # if TSV, restrict subj_all, and get TSV data
      # (subj_all subjects missing from TSV is an error)
      TSV = None
      if tsvfile != '':
         subj_all, TSV = self.reconcile_tsv_w_subjects(subj_all, tsvfile,
                                                       verb=verb)
         if len(subj_all) == 0:
            print("** make_datatable: failed reconcile_tsv_w_subjects")
            return []

      # create subject dictionary list, similar array of { subj:dataset } dicts
      SDL = self.slists2dictlist(subjlists)
      if len(SDL) == 0:
         print("** make_datatable: failed slists2dictlist")
         return []

      # now make a complete 2-D table of contrast labels
      clabs, CT = get_factorial_cond_table(condlists, skip0=1, verb=verb)
      if len(CT) == 0:
         print("** make_datatable: failed get_factorial_cond_table")
         return []

      # check CT length: match either # subjlists or # bsubs
      if self.check_CT_len(CT, subjlists, bsubs, verb=verb):
         return []

      # merge, each subject tries to insert an entire condition table
      DT = self.combine_subjects_n_factors(subj_all, SDL, TSV, clabs, CT,
                                           bsubs, verb=verb)

      return DT

   def reconcile_tsv_w_subjects(self, subjects, tsvfile, verb=1):
      """return updated subject list and TSV table with header and subjects

         - read tsvfile (must have header, column 0 must be subjects)
         - new subjects list will be those in tsvfile
           (each in tsvfile must currently be in subjects list)
      """

      tsvdata = UTIL.read_tsv_file(tsvfile, verb=verb)
      ntrows = len(tsvdata)
      if ntrows == 0:
         print("** reconcile: TSV file for datatable seems empty")
         return [], []
      if ntrows == 1:
         print("** reconcile: TSV file for datatable has only a header row?!?")
         return [], []
      ntcols = len(tsvdata[0])
      if ntcols < 1:
         print("** reconcile: TSV file for datatable has no columns")
         return [], []

      if verb > 1:
         print("-- reconcile: have %d x %d TSV table" % (ntrows, ntcols))

      # be sure every subject is in current subjects list
      newsubjects = []
      for tind, trow in enumerate(tsvdata):
         if tind == 0:
            continue
         if trow[0] not in subjects:
            print("** reconcile: TSV subj %s (#%d) not found from dsets" \
                  % (trow[0], tind))
            return [], []
         newsubjects.append(trow[0])

      if verb > 1:
         print("-- reconcile: reducing %d dset subjects down to %d TSV ones" \
               % (len(subjects), len(newsubjects)))

      # all subjects are found, return info from TSV file
      return newsubjects, tsvdata

   def combine_subjects_n_factors(self, subj_all, SDL, TSV, clabs, CT, bsubs,
                                  shell=0, verb=1):
      """for each subject x condition, create a row of text items
         - include the TSV entry columns, if given

          shell  : make the output in shell form
      """

      if verb > 2:
         print("-- combining subjects and factor table, have TSV = %s" \
               % (TSV is not None))

      # create key lists for quick access
      keys = [d.keys() for d in SDL]

      # if 0 or 1 bsub, make note
      if bsubs is None: nb = 0
      else:             nb = len(bsubs)
      if   nb == 0: select = ''
      elif nb == 1:
         if shell:  select = '"[%s]"' % bsubs[0]
         else:      select = '[%s]' % bsubs[0]
      else:         select = 'eatmorecheese'

      # count missing subjects per condition set
      missing = [0] * len(CT)

      # make_DT_header, init with TSV header or else just 'Subj'
      # - these 3 pieces match
      if TSV is not None:
         header = TSV[0] # 2 pieces, 'Subj' plus TSV extras
      else:
         header = ['Subj']
      header.extend(clabs)
      header.append('InputFile')
      if verb > 2: print("++ DT header: %s" % header)

      DT = [header]
      nslists = len(SDL)
      for sind, subj in enumerate(subj_all):
         for ic, cline in enumerate(CT):
            # try to find the dataset
            if nslists == 1:
               if subj in keys[0]: dset = SDL[0][subj]
               else:               dset = ''
            else:
               if subj in keys[ic]: dset = SDL[ic][subj]
               else:                dset = ''
            # if no data, skip this row
            if dset == '':
               missing[ic] += 1
               if verb > 2: print("-- no data for %s, conds %s" % (subj, cline))
               continue

            # do we want a volume selection?
            if nb < 2:  dset += select
            elif shell: dset += '"[%s]"' % bsubs[ic]
            else:       dset += '[%s]' % bsubs[ic]

            # formulate row: subj, TSV_cols..., factor_labels..., inputfile
            drow = [subj]

            # insert TSV columns
            if TSV is not None:
               if subj != TSV[sind+1][0]:
                  print("** TSV/data mismatch for subj %d = %s" % (sind, subj))
                  return []
               drow.extend(TSV[sind+1][1:])

            drow.extend(cline)
            drow.append(dset)
            DT.append(drow)

      if verb > 1:
         print("-- num missing subjects per factor set: %s" \
               % ', '.join(['%s' % m for m in missing]))

      return DT

   def check_CT_len(self, CT, subjlists, bsubs, verb=1):
      """check total number of condition sets against subject lists or bsubs

        - if ntcond == 1:
          - need len(subjlists) == 1
          - bsubs can be empty or have 1 value

        - else
          - if len(subjlists) == 1: all conditions in one dset
               - so need len(bsubs) == ntcond
          - else: len(subjlists) > 1: one condition/beta per subj list
               - must have len(subjlists) == ntcond
               - if len(bsubs) == 0: omit sub-brick selection
               - else if len(bsubs) == 1: use same selector for every volume
               - else: must have: len(bsubs) == ntcond == len(subjlists)

        return 0 on success, else error
      """
      ntcond = len(CT)
      nslist = len(subjlists)
      if bsubs is None: nbsubs = 0
      else:             nbsubs = len(bsubs)

      if verb > 1: print("-- check_CT_len: have %d cond, %s slists, %s bsubs" \
                         % (ntcond, nslist, nbsubs))

      # have one condition/factor
      if ntcond == 1:
         if nslist != 1:
            print("** check_CT_len: 1 cond set but %d subj lists" % nslist)
            return 1
         if nbsubs > 1:
            print("** check_CT_len: 1 cond set but %d beta subs" % nbsubs)
            return 1
         return 0 # seems okay

      # we have multiple condition/factor sets...
      if nslist == 1:
         # check bsubs, since all condition volumes in each subject dataset
         if nbsubs != ntcond:
            print("** check_CT_len: %d cond sets but %d beta subs" \
                  % (ntcond, nbsubs))
            return 1
         return 0 # seems okay

      # multiple subjlists and multiple condition sets
      if ntcond != nslist:
         print("** check_CT_len: %d cond sets but %d subj lists" \
               % (ntcond, nslist))
         return 1

      # verify nbsubs
      if nbsubs != 0 and nbsubs != 1 and nbsubs != ntcond:
         print("** check_CT_len: %d cond sets but %d bsubs" \
               % (ntcond, nbsubs))
         return 1

      return 0

   def slists2dictlist(self, subjlists):
      """convert the subject lists to a list of {sid:dset} dictionaries

         not soooo crypically terse
      """
      SDL = []
      for slist in subjlists:
         SDL.append( {subj.sid:subj.dset for subj in slist.subjects} )
      return SDL

   def get_all_subjs_from_lists(self, subjlists, verb=1):
      """given a list of SubjectList instances, return a combined list of
         all subjects across all SubjectLists
      """
      everyone = []
      for slist in subjlists:
         everyone.extend([subj.sid for subj in slist.subjects])
      ulist = UTIL.get_unique_sublist(everyone)
      ulist.sort()
      del(everyone)

      if verb > 1:
         slens = [str(len(ss.subjects)) for ss in subjlists]
         print("-- subjlist lengths = %s" % ' '.join([str(c) for c in slens]))
         print("   total unique subjects = %s" % len(ulist))
         if verb > 3:
            print("== all subjects\n   %s" % ' '.join(ulist))

      return ulist

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
      if verb > 1: print('-- make_anova2_command: have prefix %s' % prefix)

      if bsubs == None:
         print('** missing sub-brick selection list')
         return None
      if len(bsubs) < 2:
         print('** anova3_command: need at least 2 sub-bricks (have %d)' \
               % len(bsubs))
         return None

      ncond = len(factors)
      ngroups = len(subjlists)

      atype = 0
      if ngroups > 1: atype = 5
      elif ncond == 2: atype = 4

      if atype == 4:
         if ngroups != 1:
            print('** anova3_cmd: -type 4 requires only 1 dset group')
            return None
         if ncond != 2:
            print('** anova3_cmd: -type 4 requires 2 factor lengths')
            print('               (product should be length -subs_betas)')
            return None
         if factors[0]*factors[1] != len(bsubs):
            print('** anova3_cmd: -type 4 factor mismatch')
            print('               (%d x %d design requires %d betas, have %d' \
                  % (factors[0], factors[1], factors[0]*factors[1], len(bsubs)))
            return None
      elif atype == 5:
         if ngroups < 2:
            print('** anova3_cmd: -type 5 requires >= 2 subject lists')
            return None
         if ncond > 1:
            print('** anova3_cmd: -type 5 should not have sets of factors')
            return None
      else:
         print('** anova3_cmd: cannot detect -type 4 or -type 5, seek -help!')
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
      cstr = sfunc(bsubs, subjlists, factors, indent)
      if cstr == None: return None
      cmd += '3dANOVA3 -type %d \\\n' '%s' % (atype, cstr)

      if len(options) > 0: cmd += '%*s%s \\\n' % (indent,'', ' '.join(options))
      else:     # add some basic options
         opt = '-amean 1 amean1 -bmean 1 bmean1'
         cmd += '%*s-amean 1 amean1 \\\n' \
                '%*s-bmean 1 bmean1 \\\n' % (indent, '', indent, '')
         print('++ no contrast options given, adding simple: %s' % opt)

      if prefix.find('/') >= 0: pp = prefix
      else:                     pp = './%s' % prefix
      cmd += '%*s-bucket %s\n' % (indent, '', pp)

      cmd += '\n'

      return cmd

   def make_generic_command(self, command, bsubs=None, subjlist2=None,
                            prefix=None, options=''):
      """create a generic command

         This basically allows one to create a generic command that takes
         AFNI-style inputs.

         Note: this is almost identical to make_mema_command, except for use 
               of tsubs.

         attach options before subject lists
         bsubs can be of length 1 even with 2 set_labs, in that case:
            - length 1: must have subjlist2 set, and apply to it
            - length 2: no subjlist2, use with subjlist 1

         return None on failure, command on success
      """

      if not command: return

      # do we have a second slist?
      s2 = subjlist2    # so much typing...

      # make sure we have sub-brick selectors for possibly both lists
      if bsubs == None: bsubs = [None]
      if s2 and len(bsubs) == 1: bsubs.append(bsubs[0])

      # ready for work, maybe note status
      if self.verb > 1:
         print('-- make_generic_command: %s -prefix %s' % (command, prefix))
         print('                         s2 = %d, bsubs = %s'%(s2!=None,bsubs))

      # initialize directories and variables
      rv, cmd = self.set_data_dirs(subjlist2=s2)
      if rv: return

      # append actual command, prefix and options
      indent = len(command)+1

      # having no prefix is valid, skip that part
      if prefix: cmd += '%s -prefix %s \\\n' % (command, prefix)
      else:      cmd += '%s \\\n'            % (command)

      if len(options) > 0:
         cmd += '%*s%s \\\n' % (indent, '', ' '.join(options))

      # if s2: cmd += '%*s# dataset list 1 \\\n' % (indent,'')
      cmd += self.make_generic_set_list(bsubs[0], indent)
      if s2:
         cmd += '\\\n' + s2.make_generic_set_list(bsubs[1], indent)
      elif len(bsubs) > 1:
         cmd += '\\\n' + self.make_generic_set_list(bsubs[1], indent)

      # strip trailing backslash (must dupe memory)
      if cmd[-2:] == '\\\n': cmd = cmd[0:-2]

      cmd += '\n\n'

      return cmd

   def set_data_dirs(self, subjlist2=None):
      """Given 1 or 2 file lists, set directories and return initial
         script to apply them with variables.

         return status and script text

         if there are 0 or 1 common dirs use them
         if there are 2 and they are different, adjust var names
      """
      s2 = subjlist2  # so much typing...
      self.set_common_data_dir()
      uses2dir = 0
      diffdirs = 0
      if s2 != None:
         s2.set_common_data_dir()
         uses2dir = not UTIL.is_trivial_dir(s2.common_dir)
         if not UTIL.is_trivial_dir(s2.common_dir)   and \
            not UTIL.is_trivial_dir(self.common_dir) and \
            self.common_dir != s2.common_dir:
               # differentiate the directory variable names
               self.common_dname = 'data1'
               s2.common_dname = 'data2'
               diffdirs = 1

      cmd = ''
      if not UTIL.is_trivial_dir(self.common_dir) or uses2dir:
         cmd += '# apply any data directories with variables\n'

      if not UTIL.is_trivial_dir(self.common_dir):
         cmd += 'set %s = %s\n' % (self.common_dname, self.common_dir)

      if diffdirs:
         cmd += 'set %s = %s\n' % (s2.common_dname, s2.common_dir)

      if cmd: cmd += '\n'

      return 0, cmd


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
      if verb > 1: print('-- make_ttest++_command: have prefix %s' % prefix)
      s2 = subjlist2    # sooooo much typing...

      if set_labs == None:
         if s2 == None: set_labs = ['setA']
         else:                 set_labs = ['setA', 'setB']
         if verb > 2: print('-- tt++_cmd: adding default set labels')
      if bsubs == None: bsubs, tsubs = ['0'], ['1']

      indent = 3  # minimum indent: spaces to following -set option

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
      cmd += '3dttest++ \\\n'           \
             '%*s-prefix %s \\\n'       \
             '%s'                       \
             '%*s-setA %s \\\n%s'       \
             % (indent, ' ', prefix, copt, 
                indent, ' ', set_labs[0],
                self.make_ttpp_set_list(bsubs[0], indent+3))

      # maybe add second set of subject files
      if len(set_labs) > 1:
         if verb > 2: print('-- tt++_cmd: have labels for second subject set')

         # separate tests for bad test types
         if comp_dir == None:
            print('** make_tt++_cmd: missing test type, should be in %s' \
                  % g_ttpp_tests)
            return      # failure
         if comp_dir not in g_ttpp_tests:
            print('** make_tt++_cmd: comp_dir (%s) must be in the list %s' \
                  % (comp_dir, g_ttpp_tests))
            return      # failure

         # note subject list and sub-brick labels
         if s2 != None:
            S = s2
            if verb > 2: print('-- second subject list was passed')
         else:
            S = self
            if verb > 2: print('-- no second subject list, using same list')
         if len(bsubs) > 1: b = bsubs[1]
         else:
            if S != s2:
               print('** make_tt++_cmd: same subject list in comparison')
            b = bsubs[0]
         cmd += '%*s-setB %s \\\n%s' % \
                (indent, ' ', set_labs[1], S.make_ttpp_set_list(b, indent+3))

      if len(options) > 0: cmd += '%*s%s' % (indent, '', ' '.join(options))

      # strip trailing backslash (must dupe memory)
      if cmd[-2:] == '\\\n': cmd = cmd[0:-2]

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
            ttype          - used for a 2-sample test, to distinguish between
                             paired and unpaired  (results is using either
                             -conditions (paired) or -group (un-))
                             ** ttype == paired is no longer valid
            options        - other options added to the 3dMEMA command
            verb           - verbose level

         return None on failure, command on success
      """

      if prefix == '' or prefix == None: prefix = 'mema_result'
      if verb > 1: print('++ make_mema_cmd: have prefix %s' % prefix)
      s2 = subjlist2    # sooooo much typing...

      if set_labs == None:
         if s2 == None: set_labs = ['setA']
         else:                 set_labs = ['setA', 'setB']
         if verb > 2: print('++ mema_cmd: adding default set labels')
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
         if verb > 2: print('-- mema_cmd: have labels for second subject set')
         # note subject list and sub-brick labels
         if s2 != None:
            S = s2
            if verb > 2: print('-- second subject list was passed')
         else:
            S = self
            if verb > 2: print('-- no second subject list, using same list')
         if len(bsubs) > 1: b, t = bsubs[1], tsubs[1]
         else:
            if S != s2:
               print('** make_mema_cmd: same subject list in comparison')
            b, t = bsubs[0], tsubs[0]
         cmd += '%7s-set %s \\\n%s' % \
                (' ', set_labs[1], S.make_mema_set_list(b, t, 10))

         # either 2-sample or paired
         if ttype not in g_mema_tests:
            print("** invalid 3dMEMA test %s, not in %s" % (ttype,g_mema_tests))
            return None
         if ttype == 'paired':
            print('** 3dMEMA -type paired: no longer valid\n' \
                  '   (input contrast and t-stat from original regression)')
            return None
         else: opt = '-groups'
         cmd += '%7s%s %s %s \\\n' % ('', opt, set_labs[0], set_labs[1])

      if len(options) > 0: cmd += '%7s%s' % ('', ' '.join(options))

      # strip trailing backslash (must dupe memory)
      if cmd[-2:] == '\\\n': cmd = cmd[0:-2]

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

   def make_generic_set_list(self, bsub, indent=0):
      """return a multi-line string of the form:
                "dset1[bsub]"
                "dset2[bsub]"
                ...
         indent is per-line indentation
      """
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
         if bsub == None:
            sstr += '%*s%s \\\n' % (indent, '', dset)
         else: # use bsub
            sstr += '%*s"%s[%s]" \\\n' % (indent, '', dset, bsub)

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
         print('** MAt4SL: bad subject list count = %d' % len(subjlists))
         return None

      nA = factors[0]
      nB = factors[1]
      slist = subjlists[0]
      if nA*nB != len(bsub):
         print('** MAt4SL: bad factor count: %d, %d, %d' % (nA, nB, len(bsub)))
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
            print('** subject list %d length differs from SL 1 (%d != %d)\n'\
                  % (ilist+1, len(slist.subjects), slen0))
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

# general functions, could go elsewhere
def get_factorial_cond_table(condlists, skip0=0, verb=1):
   """given a list of condition levels, return a fully factorized table,
      plus the condition labels
      (so of length the product of the conditions)

      - the table should be in condition-major order (first to last)
      - if skip0: element 0 of each list should be skipped, as it is a label
                  (if skip0 == 0, returned label list is empty)

      for example:
        condlists = [ ['color', 'blue', 'red', 'orange'],
                      ['sex',   'M', 'F'],
                      ['emot',  'happy', 'sad'] ]
      return 
        [     'color',  'sex', 'emot' ],

        [
            [ 'blue',   'M',   'happy' ],
            [ 'blue',   'M',   'sad' ],
            [ 'blue',   'F',   'happy' ],
            [ 'blue',   'F',   'sad' ],
            [ 'red',    'M',   'happy' ],
            [ 'red',    'M',   'sad' ],
            ...
            [ 'orange', 'M',   'happy' ],
            [ 'orange', 'M',   'sad' ],
            ...
        ]
   """
   # chat
   if verb > 1:
      clens = [len(cc) for cc in condlists]
      print("-- clist lengths = %s" % ' '.join([str(c-1) for c in clens]))
      if verb > 2:
         print("-- all condition lists:")
         for clist in condlists:
            print('   %s : %s' % (clist[0], ', '.join(clist[1:])))

   # real work
   CT = []
   clens = [len(cc) for cc in condlists]
   row = [0] * len(condlists) # computational 
   _dt_nest_conds(CT, condlists, len(condlists), clens, 0, row, skip0)

   # and get labels
   if skip0: labels = [clist[0] for clist in condlists]
   else:     lebels = []

   # chat
   if verb > 1:
      print("-- have complete factorization table, length %s" % len(CT))
      if verb > 2:
         for row in CT:
            print(row)

   return labels, CT

def _dt_nest_conds(CT, clist, nclist, clens, cind, row, skip0):
   """recursive: if done, append row to table
                 else, for each factor level, insert this factor and recur

      recursively fill CT with every (ordered) combination of factor levels
   """
   # if we are fully nested, add the current row to the table
   # (we *could* move this to the loop, but it seems more readable this way)
   if cind == nclist:
      CT.append(row[:])
      return
  
   # else, for each factor level, set at row position and recur to next posn
   # (skip position 0)
   if skip0: flist = clist[cind][1:]
   else:     flist = clist[cind]
   for level in flist:
      row[cind] = level
      _dt_nest_conds(CT, clist, nclist, clens, cind+1, row, skip0)

   return

def _make_uniform_col_widths(dtable):
   """return a new table where every string is expanded to the max col width
   """
   maxlens = [0] * len(dtable[0])
   for row in dtable:
      for ind, val in enumerate(row):
         l = len(val)
         if l > maxlens[ind]:
            maxlens[ind] = l

   newtable = []
   for row in dtable:
      newrow = []
      for ind, val in enumerate(row):
         newrow.append("%-*s" % (maxlens[ind], val))
      newtable.append(newrow)

   return newtable

if __name__ == '__main__':
   print('** this is not a main program')

