#!/usr/bin/env python

# general functions for use by uber*.py

import sys, os
from time import asctime
import glob

import afni_util as UTIL
import lib_subjects as SUBJ

DEF_UBER_DIR = 'uber_results'        # top directory for output

g_history = """
  uber_subject.py history

    0.0  Feb 14, 2011: initial revision
         - functional GUI for anat/epi/stim, generates simple AP command
    0.1  Feb 15, 2011:
         - added CLI (command line interface)
         - additional help
    0.2  Feb 16, 2011: reorg (move files and functions around)
    0.3  Feb 16, 2011: epi or stim list from command line can init order/labels
"""

g_version = '0.3'

# ----------------------------------------------------------------------
# global definition of default processing blocks
g_def_blocks      = ['tshift', 'volreg', 'blur', 'mask', 'scale', 'regress']
g_def_blocks_anat = ['tshift', 'align', 'tlrc', 'volreg', 'blur', 'mask',
                     'scale', 'regress']

# ----------------------------------------------------------------------
# a global definition of subject defaults for single subject analysis
g_subj_defs = SUBJ.VarsObject("Single Subject Dialog defaults")
g_subj_defs.uber_dir      = DEF_UBER_DIR
g_subj_defs.blocks        = []
g_subj_defs.sid           = ''          # subject ID    (no spaces - required)
g_subj_defs.gid           = ''          # group ID      (no spaces)
g_subj_defs.anat          = ''          # anat dset name (probably .HEAD)
g_subj_defs.get_tlrc      = 0           # toggle, include anat+tlrc
g_subj_defs.epi           = []          # EPI dset name list
g_subj_defs.epi_wildcard  = 0           # use wildcard form for EPIs
g_subj_defs.stim          = []          # EPI dset name list
g_subj_defs.stim_wildcard = 0           # use wildcard form for EPIs
g_subj_defs.stim_label    = []          # label for each stim file
g_subj_defs.stim_basis    = []          # basis functions: empty=GAM,
                                        #   valid lengths: 0, 1, len(stim)

# note: short vars (e.g. with epi)
#   use_dirs      - should we set any directory at all
#   use_tdir      - if it will be used as a parent to short_epi_dir
#   var_edir      - if EPI dir variable should be used ($epi or $top or '')
#   epi_dir       - common full dir prefix to all epi
#   short_epi_dir - either epi_dir or that under top_dir
#   epi           - list of full epi dset names
#   short_epi     - epi names under epi_dir

class AP_Subject(object):
   """subject for single-subject analysis scripting by afni_proc.py"""
   def __init__(self, svars=None, verb=1):

      # for now, leave errors, warnings and ap_command out of LV

      self.LV = SUBJ.VarsObject("local AP_Subject vars")
      self.LV.verb = verb
      self.LV.indent = 8             # default indent for AP options

      self.svars = g_subj_defs.copy()   # start with default subject vars
      self.svars.merge(svars)           # expand to include those passed

      self.set_blocks()                 # choose processing blocks

      self.set_directories()            # data dirs: anat, epi, stim
                                        # (also sets use_dirs, use_adir, etc.)

      self.set_short_names()            # short data dirs: e.g. short_anat

      if self.LV.verb > 3: self.LV.show('ready to start script')

      self.set_ap_command()             # fill ap_command, warnings, errors

   def set_ap_command(self):
      """attempt to generate an afni_proc.py command in ap_command
            - keep a list of warnings and errors
            - if there are errors, ap_command might not be filled
      """

      self.errors = []                  # list of error strings
      self.warnings = []                # list of warning strings

      # first assign directories
      self.ap_command = self.script_init()
      self.ap_command += self.script_set_dirs()
      self.ap_command += self.script_set_vars()

      self.ap_command += self.script_ap_init()
      self.ap_command += self.script_ap_blocks()
      self.ap_command += self.script_ap_anat()
      self.ap_command += self.script_ap_epi()
      self.ap_command += self.script_ap_stim()
      self.ap_command += self.script_ap_stim_labels()
      self.ap_command += self.script_ap_stim_basis()

      # alter ap_command, removing last '\'
      self.ap_command = self.script_ap_nuke_last_LC(self.ap_command)

      if len(self.errors) > 0: return   # if any errors so far, give up

      self.ap_command = UTIL.add_line_wrappers(self.ap_command)

      return

   def script_ap_nuke_last_LC(self, cmd):
      """Find last useful character (not in {space, newline, '\\'}).
         That should end the command (insert newline).
      """

      clen = len(cmd)
      ind = clen-1
      skipchars = [' ', '\t', '\n', '\\']
      while ind > 0 and cmd[ind] in skipchars: ind -= 1

      return cmd[0:ind+1]+'\n\n'

   def script_ap_stim_basis(self):
      slen = len(self.svars.stim_basis)
      if slen == 0: return ''

      if UTIL.vals_are_constant(self.svars.stim_basis):
         return "%*s-regress_basis '%s' \\\n"  \
                   % (self.LV.indent, '', self.svars.stim_basis[0])

      if slen != len(self.svars.stim):
         self.errors.append('** error: num stim files != num stim basis')
         return ''
         
      return "%*s-regress_basis_multi \\\n%*s%s \\\n" %         \
                (self.LV.indent, '', self.LV.indent+4, '',
                ' '.join(["'%s'"%b for b in self.svars.stim_basis]))

   def script_ap_stim_labels(self):
      slen = len(self.svars.stim_label)
      if slen == 0: return ''

      if slen != len(self.svars.stim):
         self.errors.append('** error: num stim files != num stim labels')
         return ''
         
      return "%*s-regress_stim_labels \\\n%*s%s \\\n" %         \
                (self.LV.indent, '', self.LV.indent+4, '',
                 ' '.join(["%s"%b for b in self.svars.stim_label]))

   def script_ap_stim(self):
      """- check for existence of stimulus timing files
         - if wildcard is requested, check that the actual wildcard glob
              matches the list of stim names (else warning)
      """
      if not self.svars.stim:
         self.errors.append('** error: no stim timing files given')
         return ''
      if len(self.svars.stim) == 0:
         self.errors.append('** error: no stim timing files given')
         return ''

      # if wildcard, input files must exist, and expansion must match list
      if self.svars.stim_wildcard:
         self.LV.stim_wildform = UTIL.glob_form_from_list(self.LV.short_stim)
         if self.check_wildcard_errors('stim', self.svars.stim): return ''
         if self.LV.var_sdir:
            cstr = '%s/%s' % (self.LV.var_sdir, self.LV.stim_wildform)
         else: cstr = self.LV.stim_wildform

         return '%*s-regress_stim_times %s \\\n' % (self.LV.indent, '', cstr)

      # no wildcarding, so check for just one stim
      if len(self.svars.stim) == 1:
         if self.LV.var_sdir:
            cstr = '%s/%s' % (self.LV.var_sdir, self.LV.short_stim[0])
         else: cstr = self.LV.short_stim[0]
         return '%*s-regress_stim_times %s \\\n' % (self.LV.indent, '', cstr)

      # so we have multiple stim file, use just one per line
      cmd = '%*s-regress_stim_times \\\n' % (self.LV.indent, '')
      indent = self.LV.indent + 4
      for name in self.LV.short_stim:
         if self.LV.var_sdir:
            cmd += ('%*s%s/%s \\\n' % (indent,'',self.LV.var_sdir, name))
         else: cmd += ('%*s%s \\\n' % (indent,'',name))

      return cmd

   def script_ap_epi(self):
      """- check for existence of EPI datasets
         - if wildcard is requested, check that the actual wildcard glob
              matches the list of EPI names (else warning)
      """
      if not self.svars.epi:
         self.errors.append('** error: no EPI datasets given')
         return ''

      # if wildcard, input files must exist, and expansion must match list
      if self.svars.epi_wildcard:
         self.LV.epi_wildform=UTIL.glob_form_from_list(self.LV.short_epi)
         if self.check_wildcard_errors('EPI', self.svars.epi): return ''
         if self.LV.var_edir: cstr = '$epi_dir/%s' % self.LV.epi_wildform
         else:                cstr = self.LV.epi_wildform

         return '%*s-dsets %s \\\n' % (self.LV.indent, '', cstr)

      # no wildcarding, so check for just one EPI
      if len(self.svars.epi) == 1:
         if self.LV.var_edir:
            cstr = '%s/%s' % (self.LV.var_edir, self.LV.short_epi[0])
         else: cstr = self.LV.short_epi[0]
         return '%*s-dsets %s \\\n' % (self.LV.indent, '', cstr)

      # so we have multiple EPI datasets, use just one per line
      cmd = '%*s-dsets \\\n' % (self.LV.indent, '')
      indent = self.LV.indent + 4
      for name in self.LV.short_epi:
         if self.LV.var_edir:
            cmd += ('%*s%s/%s \\\n' % (indent, '', self.LV.var_edir, name))
         else: cmd += ('%*s%s \\\n' % (indent,'',name))

      return cmd

   def script_ap_blocks(self):
      if not self.svars.blocks: return ''

      return '%*s-blocks %s \\\n' \
             % (self.LV.indent, '', ' '.join(self.svars.blocks))

   def script_ap_anat(self):
      if not self.svars.anat: return ''

      anat = self.svars.anat
      if self.svars.get_tlrc:   # require existence and remove +orig extension
         plus = anat.rfind('+orig')
         if plus < 0:
            print '** missing +orig in anat %s, cannot copy tlrc dset' % anat
         else:
            anat = anat[0:plus]
         # check for +tlrc dset?  (or let afni_proc.py?)

         # rcr - here
         # ?? how to tell if anat+tlrc is from manual or @auto_tlrc?

      if self.LV.var_adir:
         file = '%s/%s' % (self.LV.var_adir, os.path.basename(anat))
      else: file = anat

      return '%*s-copy_anat %s \\\n' % (self.LV.indent, '', file)

   def check_wildcard_errors(self, name, flist):
      """if any error, report the error and return
         - files must exist
         - files must be unique
         - files must be sorted
         - wildcard form must match files

         return 0 on success, 1 on errors
      """

      # existence
      missing = missing_files(flist)
      if len(missing) > 0:
         err = '** cannot use %s wildcard form with missing files: \n' % name
         for file in missing: err += ('      %s\n' % file)
         self.errors.append(err)
         return 1

      # unique (no accidental duplicates)
      if not UTIL.vals_are_unique(flist):
         self.errors.append('** %s filenames are not unique\n' % name)
         return 1

      # sorted
      if not UTIL.vals_are_sorted(flist):
         self.errors.append('** %s filenames are not in alphabetical order\n'\
                            '   (so wildcard order would differ)\n' % name)
         return 1

      # now check that expansion matches files
      # (make wildcard form, then expand it and compare with flist)
      if not UTIL.glob_form_matches_list(flist):
         self.errors.append(                                              \
                '** %s wildcard form does not exactly match file list\n'  \
                '   (so use of wildcard form is not appropriate)\n' % name)
         return 1

      return 0

   def script_ap_init(self):
      cmd  = '# run afni_proc.py to create a single subject processing script\n'
      cmd += 'afni_proc.py -subj_id $subj \\\n'

      return cmd

   def script_set_vars(self):
      if not self.svars.sid:
         # use SUBJ, but warn user
         self.svars.sid = 'SUBJ'
         warn = "** missing subject ID, using default %s" % self.svars.sid
         self.warnings.append(warn)

      cmd  = '# set subject and group identifiers\n'
      if self.svars.sid: cmd += 'set subj      = %s\n' % self.svars.sid
      if self.svars.gid: cmd += 'set group     = %s\n' % self.svars.gid
      if cmd != '': return cmd + '\n'
      else:         return cmd

   def script_set_dirs(self):
      if not self.LV.use_dirs: return ''

      cmd = '# set data directories\n'

      if self.LV.use_tdir:
         # then set var and apply to children
         cmd += 'set top_dir   = %s\n' % self.LV.top_dir
         tstr = '$top_dir/'
      else: tstr = ''

      if self.LV.var_adir and self.LV.var_adir != '$top_dir':
         cmd += 'set anat_dir  = %s%s\n' % (tstr, self.LV.short_anat_dir)
      if self.LV.var_edir and self.LV.var_edir != '$top_dir':
         cmd += 'set epi_dir   = %s%s\n' % (tstr, self.LV.short_epi_dir)
      if self.LV.var_sdir and self.LV.var_sdir != '$top_dir':
         cmd += 'set stim_dir  = %s%s\n' % (tstr, self.LV.short_stim_dir)

      return cmd + '\n'

   def script_init(self):
      return '#!/usr/bin/env tcsh\n\n'           \
             '# created by uber_subject.py: version %s, %s\n\n' \
                % (g_version, asctime())

   def use_dir(self, dir):
      if dir and dir != '.': return 1
      return 0

   def set_blocks(self):
      if len(self.svars.blocks) > 0: return     # use what is given

      if self.svars.anat:
         blocks = default_block_order(anat=1)
         if self.svars.get_tlrc: blocks.remove('tlrc')
      else:
         blocks = default_block_order(anat=0)

      self.svars.blocks = blocks

   def set_directories(self):
      """set top_dir and anat_dir, epi_dir and stim_dir as sub-directories
         also set var_adir, edir and sdir, as the directory variables
      """

      all_files = []
      if self.svars.anat:          all_files.append(self.svars.anat)
      if len(self.svars.epi)  > 0: all_files.extend(self.svars.epi)
      if len(self.svars.stim) > 0: all_files.extend(self.svars.stim)

      self.LV.top_dir  = UTIL.common_dir(all_files)
      self.LV.anat_dir = UTIL.common_dir([self.svars.anat])
      self.LV.epi_dir  = UTIL.common_dir(self.svars.epi)
      self.LV.stim_dir = UTIL.common_dir(self.svars.stim)

      # init vars: if not '', then apply them as directories
      self.LV.var_adir = self.LV.var_edir = self.LV.var_sdir = ''
      if self.use_dir(self.LV.anat_dir): self.LV.var_adir = '$anat_dir'
      if self.use_dir(self.LV.epi_dir):  self.LV.var_edir = '$epi_dir'
      if self.use_dir(self.LV.stim_dir): self.LV.var_sdir = '$stim_dir'

      # make short versions, but preserve long ones
      # maybe $epi_dir should be replaced with $top_dir
      if self.LV.top_dir.count('/') > 1:
         self.LV.use_tdir       = 1
         self.LV.short_anat_dir = self.child_dir_name(self.LV.anat_dir)
         if self.LV.short_anat_dir == '.': self.LV.var_adir = '$top_dir'

         self.LV.short_epi_dir  = self.child_dir_name(self.LV.epi_dir)
         if self.LV.short_epi_dir == '.': self.LV.var_edir = '$top_dir'

         self.LV.short_stim_dir = self.child_dir_name(self.LV.stim_dir)
         if self.LV.short_stim_dir == '.': self.LV.var_sdir = '$top_dir'
      else:
         self.LV.use_tdir       = 0
         self.LV.short_anat_dir = self.LV.anat_dir
         self.LV.short_epi_dir  = self.LV.epi_dir
         self.LV.short_stim_dir = self.LV.stim_dir

      # note whether we are using any directory at all
      if self.use_dir(self.LV.top_dir) or self.LV.var_adir or \
         self.LV.var_edir or self.LV.var_sdir: self.LV.use_dirs = 1
      else: self.LV.use_dirs = 0

      if self.LV.verb > 3:
         print ("++ APS.top_dir = %s\n   adir = %s\n   edir = %s\n   sdir = %s"\
                % (self.LV.top_dir, self.LV.anat_dir,
                   self.LV.epi_dir, self.LV.stim_dir))

   def set_short_names(self):
      """set short_anat, _epi, _stim from respective directories
         --> these are the names used in the script
             e.g. '$anat_dir/%s' % short_anat
      """
      # rcr - maybe this is not needed

      # anat (one name)
      if self.LV.var_adir:
         dlen = len(self.LV.anat_dir)
         self.LV.short_anat = self.svars.anat[dlen+1:]
      else: # no directory
         self.LV.short_anat = self.svars.anat

      # EPI (list)
      if self.LV.var_edir:
         dlen = len(self.LV.epi_dir)
         self.LV.short_epi = [epi[dlen+1:] for epi in self.svars.epi]
      else: # no directory
         self.LV.short_epi = [epi[:] for epi in self.svars.epi]

      # stim (list)
      if self.LV.var_sdir:
         dlen = len(self.LV.stim_dir)
         self.LV.short_stim = [stim[dlen+1:] for stim in self.svars.stim]
      else: # no directory
         self.LV.short_stim = [stim[:] for stim in self.svars.stim]

   def child_dir_name(self, child, parent=''):
      if parent == '': parent = self.LV.top_dir
      if parent == '' or child == '': return child
      plen = len(parent)
      clen = len(child)
      if child[0:plen] != parent: return child

      # return everything after the separator
      if clen < plen + 2: return '.'
      else:               return child[plen+1:]

   def get_ap_command(self):
      """return status, message
             status = number of error message

             if 0: message = command
             else: message = error string

         requests for warnings must be made separately, since they would
         not be fatal
      """

      # if we have error messages, return them
      if len(self.errors) > 0:
         return 1, self.make_message_list_string(self.errors, "errors")

      return 0, self.ap_command

   def get_ap_warnings(self):
      """return the number of warnings and a warnings string"""
      return len(self.warnings), \
             self.make_message_list_string(self.warnings, "warnings")

   def make_message_list_string(self, mlist, title):
      if len(mlist) == 0: return ''
      mesg = ''
      for mm in mlist: mesg += (mm + '\n')
      return mesg

def get_dir_and_glob_form(flist):
   """return the common directory and glob form string of a list"""
   fdir = UTIL.common_dir(flist)
   dlen = len(fdir)
   if dlen > 0: short_list = [name[dlist+1:] for name in flist]
   else:        short_list = flist

   return fdir, UTIL.glob_form_from_list(short_list)

def get_uber_results_dir(topdir=None):
   """if we are in a DEF_UBER_DIR, return it
      else return cwd/DEF_UBER_DIR

      if topdir is passed, use that in place of the current directory
   """
   if topdir: cwd = topdir[:]
   else:      cwd = os.getcwd()
   base = os.path.basename(cwd)

   if base == DEF_UBER_DIR: return cwd
   else:                    return '%s/%s' % (cwd, DEF_UBER_DIR)

# ===========================================================================
# end class AP_Subject
# ===========================================================================

def objs_are_equal(obj1, obj2):
   """- check for None
      - check for simple types
         - if so, check equality
         - else if lists
            - compare lengths and recur
         - else, just check equality

      return 1 if equal (completely), else 0
   """
   # check None
   if obj1 == None and obj2 == None: return 1
   if obj1 == None  or obj2 == None: return 0   # only 1

   # check simple types
   st1 = is_simple_type(obj1)
   st2 = is_simple_type(obj2)
   if st1 and st2:      # simple equality check
      if obj1 == obj2: return 1
      else:            return 0
   if st1 or st2:      return 0  # only 1 after previous test

   # check for type difference
   if type(obj1) != type(obj2): return 0

   # check lists
   if type(obj1) == type([]):
      len1 = len(obj1)
      len2 = len(obj2)
      if len1 != len2: return 0
      if len1 == 0:    return 1  # both empty

      # so non-empty lists of the same lengths: recur
      for ind in range(len1):
         if not objs_are_equal(obj1[ind], obj2[ind]): return 0
      return 1

   # some other type
   if obj1 == obj2: return 1
   else:            return 0

def default_block_order(anat=1):
   """rcr - reconcile with afni_proc.py, maybe make single interface"""

   if anat: return g_def_blocks_anat[:]
   else:    return g_def_blocks[:]

def is_simple_type(val):
   if type(val) in [int, float, str]: return 1
   else:                              return 0

def missing_files(flist):
   """return a sub-list of the files that are missing"""
   missing = []
   for file in flist:
      if not os.path.isfile(file): missing.append(file)
   return missing

def ap_command_from_svars(svars, verb=1):
   """create an afni_proc.py command

      This might be run from the GUI or command line.

      return status, warnings and error string
        status =  0 on success
               =  1 if warnings
               = -1 if errors
   """

   # create command, save it (init directory tree?), show it
   apsubj = AP_Subject(svars, verb=verb)
   nwarn, wstr = apsubj.get_ap_warnings()
   status, mesg = apsubj.get_ap_command()

   if status == 0:
      if nwarn > 0: status = 1  # have warnings but no errors
   else:
      status = -1               # have errors

   return status, wstr, mesg

def update_svars_from_special(name, svars, check_sort=0):
   """in special cases, a special svar might need updates, and might suggest
      making other updates

      if check_sort, attempt to sort known file name lists by their
      implied indices

        epi:    - sort by index list (if check_sort)
        stim:   - sort by index list (if check_sort)
                - if labels are not yet set, try to init from file names

      return the number of applied changes
   """

   # quick check for field to work with
   if not name in ['epi', 'stim'] : return 0

   changes = 0

   if name == 'epi':
      fnames = svars.epi
      nf = len(fnames)
      if nf < 2: return 0       # nothing to do

      if check_sort: # try to sort by implied index list
         dir, snames, gstr = flist_to_table_pieces(fnames)
         indlist = UTIL.list_minus_glob_form(snames)
         apply = 0
         try:
            indlist = [int(val) for val in indlist]
            apply = 1
         except: pass
         # might as well check if already sorted
         if apply and UTIL.vals_are_increasing(indlist): apply = 0
         if apply and UTIL.vals_are_unique(indlist):
            # attach index and name in 2-D array, sort, extract names
            vlist = [[indlist[ind], fnames[ind]] for ind in range(nf)]
            vlist.sort()
            svars.set_var(name, [val[1] for val in vlist])
            changes += 1

   elif name == 'stim':
      fnames = svars.stim
      nf = len(fnames)
      if nf < 2: return 0               # nothing to do

      # stim file names are more complex...
      dir, snames, gstr = flist_to_table_pieces(fnames)
      stable = UTIL.parse_as_stim_list(snames)

      if len(stable) != nf: return 0    # nothing to do

      # if sorting, extract and process indlist
      if check_sort: # try to sort by implied index list
         indlist = [entry[0] for entry in stable]
         apply = 0
         try:
            indlist = [int(val) for val in indlist]
            apply = 1
         except: pass
         # might as well check if already sorted
         if apply and UTIL.vals_are_increasing(indlist): apply = 0
         if apply and UTIL.vals_are_unique(indlist):
            # attach index and name in 2-D array, sort, extract names
            vlist = [[indlist[ind], fnames[ind]] for ind in range(nf)]
            vlist.sort()
            svars.set_var(name, [val[1] for val in vlist])
            changes += 1

      # apply labels unless some already exist
      apply = 1
      if svars.valid('stim_label'):
         if len(svars.stim_label) > 0: apply = 0

      if apply:
         labs = [entry[1] for entry in stable]
         if not '' in labs:
            svars.set_var('stim_label', labs)
            changes += 1

   return changes

def flist_to_table_pieces(flist):
      """return:
           - common directory name
           - short dlist names (after removing directory name)
           - glob string of short names
         note: short names will be new data (not pointers to flist)
      """
      if len(flist) == 0: return '', [], ''

      ddir = UTIL.common_dir(flist)
      dirlen = len(ddir)
      if dirlen > 0: snames = [dset[dirlen+1:] for dset in flist]
      else:          snames = [dset[:]         for dset in flist]

      globstr = UTIL.glob_form_from_list(snames)

      return ddir, snames, globstr


# ===========================================================================
# help strings accessed both from command-line and GUI
# ===========================================================================

helpstr_usubj_gui = """
===========================================================================
uber_subject.py GUI             - a graphical interface to afni_proc.py

   purposes:

      o  to run a single subject analysis or generate processing scripts
      o  to help teach users:
            - how to process data, including new methods or tools
            - scripting techniques
            - where to get more help

   required inputs:

      o  EPI datasets (in AFNI or NIfTI format)
      o  stimulus timing files (time=0.0 refers to start of steady state)

   optional inputs:

      o  anatomical dataset
      o  stim file labels and basis functions
      o  whether to use wildcards
      o  many processing options

---------------------------------------------------------------------------
Overview:

   One generally goes through the following steps:

      0. specify overview variables (subject and group ID codes)
      1. specify input files (anat, EPI, stim timing)
      2. specify some additional options
      3. view the resulting afni_proc.py command
      4. process the subject

   The graphical interface is set up for users to specify the most pertinent
   inputs first.  In the future, one should be able to initialize the interface
   based on a previous subject.

   Step 0. Optional subject and group ID codes are specified at the very top,
           in the 'general subject info' section.

              inputs: subject ID, group ID

   Step 1. Specify input files.  The anatomy, EPI and stimulus timing files can
           be specified via file browsers.  File names can be altered after
           being set.

           Be careful of the EPI and stimulus timing file orders.  The ordering
           can come from any column the user clicks the heading for.  If the
           indices are found from the file names, that order is likely to be
           appropriate.

              inputs: anat, EPI, stimulus timing files

   Step 2. Specify additional options as desired.  

   Step 3. click to generate the afni_proc.py command

   Step 4. click to process the subject (run AP command and execute proc script)

- R Reynolds  Feb, 2011
===========================================================================
"""

