#!/usr/bin/env python

# python3 status: started

# afni_util.py : general utilities for python programs

import sys, os, math
import afni_base as BASE
import lib_textdata as TD
import glob
import pdb
import re

# global lists for basis functions
basis_known_resp_l = ['GAM', 'BLOCK', 'dmBLOCK', 'dmUBLOCK', 'SPMG1',
                      'WAV', 'MION']
basis_one_regr_l   = basis_known_resp_l[:]
basis_one_regr_l.append('MION')
stim_types_one_reg = ['file', 'AM1', 'times']

# this file contains various afni utilities   17 Nov 2006 [rickr]

def change_path_basename(orig, prefix='', suffix='', append=0):
    """given a path (leading directory or not) swap the trailing
       filename with the passed prefix and suffix
          e.g. C_P_B('my/dir/pickles.yummy','toast','.1D')
                 --> 'my/dir/toast.1D' 
       or with append...
          e.g. C_P_B('my/dir/pickles.yummy','toast','.1D', append=1)
                 --> 'my/dir/toastpickles.yummy.1D'
       or maybe we want another dot then
          e.g. C_P_B('my/dir/pickles.yummy','toast.','.1D', append=1)
                 --> 'my/dir/toast.pickles.yummy.1D'
    """
    if not orig: return ''
    if not prefix and not suffix: return orig

    (head, tail) = os.path.split(orig)
    if append: tail = '%s%s%s' % (prefix, tail, suffix)
    else:      tail = '%s%s' % (prefix, suffix)

    if head == '': return tail
    return "%s/%s" % (head, tail)

# write text to a file
def write_text_to_file(fname, tdata, mode='w', wrap=0, wrapstr='\\\n', exe=0):
    """write the given text (tdata) to the given file
          fname   : file name to write (or append) to
          dtata   : text to write
          mode    : optional write mode 'w' or 'a' [default='w']
          wrap    : optional wrap flag [default=0]
          wrapstr : optional wrap string: if wrap, apply this string
          exe     : whether to make file executable

       return 0 on success, 1 on error
    """

    if not tdata or not fname:
        print("** WTTF: missing text or filename")
        return 1

    if wrap: tdata = add_line_wrappers(tdata, wrapstr)
    
    if fname == 'stdout':   fp = sys.stdout
    elif fname == 'stderr': fp = sys.stderr
    else:
       try:
           fp = open(fname, mode)
       except:
           print("** failed to open text file '%s' for writing" % fname)
           return 1

    fp.write(tdata)

    if fname != 'stdout' and fname != 'stderr':
       fp.close()
       if exe:
           try: code = eval('0o755')
           except: code = eval('0755')
           try:
               os.chmod(fname, code)
           except:
               print("** failed chmod 755 on %s" % fname)

    return 0

def wrap_file_text(infile='stdin', outfile='stdout'):
   """make a new file with line wrappers                14 Mar 2014

      The default parameters makes it easy to process as a stream:

          afni_util.py -eval 'wrap_file_text()' < INPUT > OUTPUT
                or
          afni_util.py -eval 'wrap_file_text("INPUT", "OUTPUT")'
                or
          afni_util.py -eval "wrap_file_text('$f1', '$f2')"
   """

   tdata = read_text_file(fname=infile, lines=0, strip=0)
   if tdata != '': write_text_to_file(outfile, tdata, wrap=1)
   

def read_text_file(fname='stdin', lines=1, strip=1, verb=1):
   """return the text text from the given file as either one string
      or as an array of lines"""

   if fname == 'stdin' or fname == '-': fp = sys.stdin
   else:
      try: fp = open(fname, 'r')
      except:
        if verb: print("** read_text_file: failed to open '%s'" % fname)
        if lines: return []
        else:     return ''

   if lines:
      tdata = fp.readlines()
      if strip: tdata = [td.strip() for td in tdata]
   else:
      tdata = fp.read()
      if strip: tdata.strip()

   fp.close()

   return tdata

def read_top_lines(fname='stdin', nlines=1, strip=0, verb=1):
   """use read_text_file, but return only the first 'nlines' lines"""
   tdata = read_text_file(fname, strip=strip, verb=verb)
   if nlines != 0: tdata = tdata[0:nlines]
   return tdata

def write_data_as_json(data, fname='stdout', indent=3, sort=1, newline=1):
   """dump to json file; check for stdout or stderr
      return 0 on success
   """
   # import locally, unless it is needed in at least a few functions
   # (will make default in afni_proc.py, so be safe)
   try:
      import json
   except:
      print("** afni_util.py: 'json' python library is missing")
      return 1

   if fname == 'stdout' or fname == '-':
      fp = sys.stdout
   elif fname == 'stderr':
      fp = sys.stderr
   else:
      try: fp = open(fname, 'w')
      except:
         print("** write_as_json: could not open '%s' for writing" % fname)
         return 1

   # actual write
   json.dump(data, fp, sort_keys=sort, indent=indent)

   if newline: fp.write('\n')
   if fp != sys.stdout and fp != sys.stderr:
      fp.close()

   return 0

def read_AFNI_version_file(vdir='', vfile='AFNI_version.txt', delim=', '):
   """read AFNI_version.txt from vdir (else executable_dir)
      return comma-delimited form
   """

   if vdir == '': vdir = executable_dir()
   if vdir == '': return ''

   vpath = '%s/%s' % (vdir, vfile)

   if not os.path.isfile(vpath): return ''

   vdata = read_text_file(vpath, verb=0)
   if vdata == '': return ''

   return delim.join(vdata)

def write_to_timing_file(data, fname='', nplaces=-1, verb=1):
   """write the data in stim_times format, over rows
      (this is not for use with married timing, but for simple times)"""

   if fname == '': return

   fp = open(fname, 'w')
   if not fp:
      print("** failed to open '%s' for writing timing" % fname)
      return 1

   if verb > 0:
      print("++ writing %d timing rows to %s" % (len(data), fname))

   fp.write(make_timing_data_string(data, nplaces=nplaces, flag_empty=1,
                                    verb=verb))
   fp.close()

   return 0

def make_timing_data_string(data, row=-1, nplaces=3, flag_empty=0,
                            mesg='', verb=1):
   """return a string of row data, to the given number of decimal places
      if row is non-negative, return a string for the given row, else
      return a string of all rows"""

   if verb > 2:
      print('++ make_data_string: row = %d, nplaces = %d, flag_empty = %d' \
            % (row, nplaces, flag_empty))

   if row >= 0:
      return make_single_row_string(data[row], row, nplaces, flag_empty)

   # make it for all rows
   if len(mesg) > 0: rstr = "%s :\n" % mesg
   else:             rstr = ''
   for ind in range(len(data)):
      rstr += make_single_row_string(data[ind], ind, nplaces, flag_empty)

   return rstr

def make_single_row_string(data, row, nplaces=3, flag_empty=0):
   """return a string of row data, to the given number of decimal places
      if row is non-negative, return a string for the given row"""

   rstr = ''

   # if flagging an empty run, use '*' characters
   if len(data) == 0 and flag_empty:
      if row == 0: rstr += '* *'
      else:        rstr += '*'

   for val in data:
      if nplaces >= 0: rstr += '%.*f ' % (nplaces, val)
      else:            rstr += '%g ' % (val)

   return rstr + '\n'

def quotize_list(inlist, opt_prefix='', skip_first=0, quote_wild=0,
                 quote_chars='', ok_chars=''):
    """given a list of text elements, return a new list where any existing
       quotes are escaped, and then if there are special characters, put the
       whole string in single quotes

       if the first character is '-', opt_prefix will be applied
       if skip_first, do not add initial prefix
       if quote_wild, quotize any string with '*' or '?', too

       add quote_chars to quote list, remove ok_chars
    """
    if not inlist or len(inlist) < 1: return inlist

    # okay, we haven't yet escaped any existing quotes...

    # default to ignoring wildcards, can always double-nest if needed
    if quote_wild: qlist = "[({*? "
    else:          qlist = "[({ "

    for c in quote_chars:
        if not c in qlist: qlist += c
    for c in ok_chars:
        posn = qlist.find(c)
        if posn >= 0: qlist = qlist[0:posn]+qlist[posn+1:]

    newlist = []
    first = 1   # ugly, but easier for option processing
    for qstr in inlist:
        prefix = ''
        if skip_first and first: first = 0       # use current (empty) prefix
        elif len(qstr) == 0: pass
        elif qstr[0] == '-': prefix = opt_prefix

        quotize = 0
        for q in qlist:
            if q in qstr:
                quotize = 1
                break
        if quotize: newlist.append("'%s%s'" % (prefix,qstr))
        else:       newlist.append("%s%s" % (prefix,qstr))

    return newlist

def args_as_command(args, prefix='', suffix=''):
    """given an argument list (such as argv), create a command string,
       including any prefix and/or suffix strings"""

    if len(args) < 1: return

    cstr = "%s %s" % (os.path.basename(args[0]),
                            ' '.join(quotize_list(args[1:],'')))
    fstr = add_line_wrappers('%s%s%s' % (prefix,cstr,suffix))

    return fstr

def show_args_as_command(args, note='command:'):
     """print the given argument list as a command
        (this allows users to see wildcard expansions, for example)"""

     print(args_as_command(args,
     "----------------------------------------------------------------------\n"
     "%s\n\n  " % note,
     "\n----------------------------------------------------------------------"
     ))

def exec_tcsh_command(cmd, lines=0, noblank=0, showproc=0):
    """execute cmd via: tcsh -cf "cmd"
       return status, output
          if status == 0, output is stdout
          else            output is stderr+stdout

       if showproc, show command and text output, and do not return any text

       if lines: return a list of lines
       if noblank (and lines): omit blank lines
    """

    # if showproc, show all output immediately
    if showproc: capture = 0
    else:        capture = 1

    # do not re-process .cshrc, as some actually output text
    cstr = 'tcsh -cf "%s"' % cmd
    if showproc:
       print("%s" % cmd)
       sys.stdout.flush()
    status, so, se = BASE.simple_shell_exec(cstr, capture=capture)

    if not status: otext = so
    else:          otext = se+so
    if lines:
       slines = otext.splitlines()
       if noblank: slines = [line for line in slines if line != '']
       return status, slines

    return status, otext

def limited_shell_exec(command, nlines=0):
   """run a simple shell command, returning the top nlines"""
   st, so, se = BASE.shell_exec2(command, capture=1)
   if nlines > 0:
      so = so[0:nlines]
      se = se[0:nlines]
   return st, so, se

def write_afni_com_history(fname, length=0, wrap=1):
   """write the afni_com history to the given file

      if length > 0: limit to that number of entries
   """
   com = BASE.shell_com('hi there')
   hist = com.shell_history()
   script = '\n'.join(hist)+'\n'
   write_text_to_file(fname, script, wrap=wrap)

def get_process_depth(pid=-1, prog=None, fast=1):
   """print stack of processes up to init"""

   pstack = get_process_stack(pid=pid, fast=fast)

   if prog == None: return len(pstack)

   pids = [pp[0] for pp in pstack if pp[3] == prog]
   return len(pids)

# get/show_process_stack(), get/show_login_shell()   28 Jun 2013 [rickr]
def get_process_stack(pid=-1, fast=1, verb=1):
   """the stack of processes up to init

      return an array of [pid, ppid, user, command] elements
      --> element 0 should always be init
  """
   def get_pid_index(pids, plist, pid):
      try:
         pind = pids.index(pid)
      except:
         if verb > 1:
            print('** GPS pid %s not in pid list:\n' % pid)
            print('%s' % plist)
         return -1
      return pind

   def get_ancestry_indlist(pids, ppids, plist, pid=-1):
      """return bad status if index() fails"""
      if pid >= 0: mypid = pid
      else:        mypid = os.getpid()
      pind = get_pid_index(pids, plist, mypid)
      if pind < 0: return 1, []
      indtree = [pind]
      while mypid > 1:
         mypid = ppids[pind]
         pind = get_pid_index(pids, plist, mypid)
         if pind < 0: return 1, []
         indtree.append(pind)
      return 0, indtree

   if not fast:
      stack = get_process_stack_slow(pid=pid)
      stack.reverse()
      return stack

   if verb < 2: cmd = 'ps -eo pid,ppid,user,comm'
   else:        cmd = 'ps -eo pid,ppid,user,args'
   ac = BASE.shell_com(cmd, capture=1)
   ac.run()
   if ac.status:
      print('** GPS command failure for: %s\n' % cmd)
      print('error output:\n%s' % '\n'.join(ac.se))
      return []

   plist = [ll.split() for ll in ac.so]
   names = plist[0]
   plist = plist[1:]

   try:
      pids = [int(psinfo[0]) for psinfo in plist]
      ppids = [int(psinfo[1]) for psinfo in plist]
   except:
      print('** GPS type failure in plist\n%s' % plist)
      return []

   # maybe the ps list is too big of a buffer, so have a backup plan
   rv, indlist = get_ancestry_indlist(pids, ppids, plist, pid=pid)
   # if success, set stack, else get it from backup function
   if rv == 0: stack = [plist[i] for i in indlist]
   else:       stack = get_process_stack_slow(pid=pid)
   stack.reverse()

   return stack

def get_process_stack_slow(pid=-1, verb=1):
   """use repeated calls to get stack:
        ps h -o pid,ppid,user,comm -p PID

        if pid >= 0, use as seed, else use os.getpid()
   """
   if verb > 1: base_cmd = 'ps h -o pid,ppid,user,args -p'
   else:        base_cmd = 'ps h -o pid,ppid,user,comm -p'

   def get_cmd_entries(cmd):
      ac = BASE.shell_com(cmd, capture=1)
      ac.run()
      if ac.status:
         print('** GPSS command failure for: %s\n' % cmd)
         print('error output:\n%s' % '\n'.join(ac.se))
         return 1, []
      ss = ac.so[0]
      entries = ss.split()
      if len(entries) == 0: return 1, []
      
      return 0, entries

   def get_ppids(cmd, entries):
      try:
         pid = int(entries[0])
         ppid = int(entries[1])
      except:
         print('** bad GPSS entries for cmd: %s\n  %s' % (cmd, entries))
         return 1, -1, -1
      return 0, pid, ppid

   # get pid and ppid
   if pid >= 0: mypid = pid
   else:        mypid = os.getpid()
   cmd = '%s %s' % (base_cmd, mypid)
   rv, entries = get_cmd_entries(cmd)
   if rv:
      if mypid == pid: print('** process ID %d not found' % pid)
      else:            print('** getpid() process ID %d not found' % pid)
      return []
   rv, mypid, ppid = get_ppids(cmd, entries)
   if rv: return []

   stack = [entries] # entries is valid, so init stack
   while mypid > 1:
      cmd = '%s %s' % (base_cmd, ppid)
      rv, entries = get_cmd_entries(cmd)
      if rv: return []
      rv, mypid, ppid = get_ppids(cmd, entries)
      if rv: return []
      stack.append(entries)

   return stack

def show_process_stack(pid=-1,fast=1,verb=1):
   """print stack of processes up to init"""
   pstack = get_process_stack(pid=pid,fast=fast,verb=verb)
   if len(pstack) == 0:
      print('** empty process stack')
      return
   ulist = [pp[2] for pp in pstack]
   ml = max_len_in_list(ulist)
   header = '   PID   PPID  [USER]'
   dashes = '  ----   ----  ------'
   form = '%6s %6s  [%s]'
   ilen = len(form)+4+ml

   print('%-*s : %s' % (ilen, header, 'COMMAND'))
   print('%-*s   %s' % (ilen, dashes, '-------'))
   for row in pstack:
      ss = form % (row[0], row[1], row[2])
      if len(row) > 3: rv = ' '.join(row[3:])
      else:            rv = row[3]
      print('%-*s : %s' % (ilen, ss, rv))

def get_login_shell():
   """return the apparent login shell
      from get_process_stack(), search down s[3] until a shell is found
   """
   shells = ['csh','tcsh','sh','bash','zsh']
   dshells = ['-%s' % s for s in shells]

   pstack = get_process_stack()
   if len(pstack) == 0:
      print('** cannot detect shell: empty process stack')
      return

   # start from init and work down to find first valid shell
   for pline in pstack:
      if pline[3] not in shells and pline[3] not in dshells: continue
      shell = pline[3]
      if shell[0] == '-': shell = shell[1:]      # strip any leading '-'
      return shell

   return 'SHELL_NOT_DETECTED'

def get_current_shell():
   """return the apparent login shell
      from get_process_stack(), search down s[3] until a shell is found
   """
   shells = ['csh','tcsh','sh','bash','zsh']
   dshells = ['-%s' % s for s in shells]

   pstack = get_process_stack()
   if len(pstack) == 0:
      print('** cannot detect shell: empty process stack')
      return
   pstack.reverse()

   # start from init and work down to find first valid shell
   for pline in pstack:
      if pline[3] not in shells and pline[3] not in dshells: continue
      shell = pline[3]
      if shell[0] == '-': shell = shell[1:]      # strip any leading '-'
      return shell

   return 'SHELL_NOT_DETECTED'

def show_login_shell(verb=0):
   """print the apparent login shell

      from get_process_stack(), search down s[3] until a shell is found
   """
   shells = ['csh','tcsh','sh','bash','zsh']
   dshells = ['-%s' % s for s in shells]

   pstack = get_process_stack()
   if len(pstack) == 0:
      print('** cannot detect shell: empty process stack')
      return

   # start from init and work down to find first valid shell
   shell = ''
   for pline in pstack:
      if pline[3] not in shells and pline[3] not in dshells: continue
      shell = pline[3]
      if shell[0] == '-': shell = shell[1:]      # strip any leading '-'
      if verb: print('apparent login shell: %s' % shell)
      else: print('%s' % shell)
      break

   if shell == '':
      if verb:
         print('** failed to determine login shell, see process stack...\n')
         show_process_stack()
         return

   # in verbose mode, see if parent shell is different from login
   if verb:
      pstack.reverse()
      for pline in pstack:
         if pline[3] not in shells and pline[3] not in dshells: continue
         sh = pline[3]
         if sh[0] == '-': sh = sh[1:]      # strip any leading '-'
         if sh != shell: print('differs from current shell: %s' % sh)
         break

def get_unique_sublist(inlist, keep_order=1):
    """return a copy of inlist, but where elements are unique

       if keep_order, the order is not altered (first one found is kept)
          (easy to code, but maybe slow)
       else, sort (if needed), and do a linear pass

       tested with:
          llist = [3, 4, 7, 4, 5,5,5, 4, 7, 9]
          get_unique_sublist()
          get_unique_sublist(keep_order=0)
          llist.sort()
          get_unique_sublist(keep_order=0)
    """

    if len(inlist) == 0: return []

    # if keep_order, be slow
    if keep_order:
       newlist = []
       for val in inlist:
           if not val in newlist: newlist.append(val)
       return newlist

    # else, sort only if needed
    if vals_are_sorted(inlist):
       slist = inlist
    else:
       slist = inlist[:]
       slist.sort()

    newlist = slist[0:1]
    for ind in range(len(slist)-1):
       # look for new values
       if slist[ind+1] != slist[ind]: newlist.append(slist[ind+1])

    return newlist

def uniq_list_as_dsets(dsets, byprefix=0, whine=0):
    """given a list of text elements, create a list of afni_name elements,
       and check for unique prefixes"""

    if not dsets or len(dsets) < 2: return 1

    if type(dsets[0]) == str:
       anlist = [BASE.afni_name(dset) for dset in dsets]
    elif isinstance(dsets[0], BASE.afni_name):
       anlist = dsets
    else:
       print('** ULAD: invalid type for dset list, have value %s' % dsets[0])
       return 0

    if byprefix:
       plist = [an.prefix for an in anlist]
    else:
       plist = dsets[:]
    plist.sort()

    # iterate over dsets, searching for matches
    uniq = 1
    for ind in range(len(plist)-1):
       if anlist[ind].prefix == anlist[ind+1].prefix:
          uniq = 0
          break

    if not uniq and whine:
      print("-----------------------------------------------------------\n" \
          "** dataset names are not unique\n\n"                             \
          "   (#%d == #%d, '%s' == '%s')\n\n"                               \
          "   note: if using a wildcard, please specify a suffix,\n"        \
          "         otherwise datasets may be listed twice\n"               \
          "            e.g.  bad use:    ED_r*+orig*\n"                     \
          "            e.g.  good use:   ED_r*+orig.HEAD\n"                 \
          "-----------------------------------------------------------\n"   \
          % (ind, ind+1, anlist[ind].pve(), anlist[ind+1].pve()))

    return uniq

def uniq_list_as_dset_names(dsets, whine=0):
    """like as_dsets, but the input is a simlpe array of names"""
    asets = list_to_datasets(dsets, whine=whine)
    if not asets: return 0
    return uniq_list_as_dsets(asets, whine=whine)

def list_to_datasets(words, whine=0):
    """given a list, return the list of afni_name elements
         - the list can include wildcarding
         - they must be valid names of existing datasets
         - return None on error"""

    if not words or len(words) < 1: return []
    dsets = []
    wlist = []
    errs  = 0
    for word in words:
        glist = glob.glob(word)  # first, check for globbing
        if glist:
            glist.sort()
            wlist += glist
        else: wlist.append(word)
    # now process all words
    for word in wlist:
        dset = BASE.afni_name(word)
        if dset.exist():
            dsets.append(dset)
        else:
            if whine: print("** no dataset match for '%s'" % word)
            errs = 1

    if errs:
        if whine: print('') # for separation
        return None
    return dsets

def dset_prefix_endswith(dname, suffix):
    """return 1 if an afni_name dset based on dname has a prefix
       that ends with suffix"""
    aname = BASE.afni_name(dname)
    rv = aname.prefix.endswith(suffix)
    del(aname)
    return rv

def basis_has_known_response(basis, warn=0):
    """given a string, if the prefix is either GAM or BLOCK, then the basis
       function has a known response curve

       if warn, warn users about any basis function peculiarities"""
    if not basis: return 0

    if starts_with_any_str(basis, basis_known_resp_l): return 1
    return 0

def basis_is_married(basis):
    """if the given basis function is known to require married times, return 1
    """
    if not basis: return 0

    if starts_with(basis, 'dmBLOCK'): return 1
    else:                             return 0

def basis_has_one_reg(basis, st='times'):
    """if the given basis function is known to have 1 regressor, return 1
    """
    if not basis: return 0

    # only 'times', 'file' and 'AM1' are acceptable
    if not st in stim_types_one_reg: return 0

    if starts_with_any_str(basis, basis_one_regr_l): return 1

    return 0

def starts_with(word, sstr):
    """return 1 if word starts with sstr"""
    slen = len(sstr)
    if word[0:slen] == sstr: return 1
    return 0

def starts_with_any_str(word, slist):
    """return 1 if word starts with anything in slist"""
    for sstr in slist:
       slen = len(sstr)
       if word[0:slen] == sstr: return 1
    return 0

def get_default_polort(tr, reps):
    """compute a default run polort, as done in 3dDeconvolve
       (leave run_time as TR*reps, rather than TR*(reps-1), to match 3dD)
    """

    if tr <= 0 or reps <= 0:
        print("** cannot guess polort from tr = %f, reps = %d" % (tr,reps))
        return 2        # return some default

    return run_time_to_polort(tr*reps)

def run_time_to_polort(run_time):
    """direct computation: 1+floor(run_time/150)"""
    return 1+math.floor(run_time/150.0)

def index_to_run_tr(index, rlens, rstyle=1, whine=1):
    """given index and a list of run lengths,
       return the corresponding run and TR index

       rstyle: 0/1 whether the run index is 0-based or 1-based

       any negative return indicates an error
    """
    if index < 0:
       if whine: print('** ind2run_tr: illegal negative index: %d' % index)
       return 1, index
    if len(rlens) == 0:
       if whine: print('** ind2run_tr: missing run lengths')
       return 1, index

    # if there is only 1 run and it is short, compute modulo
    rlengths = rlens
    if len(rlens) == 1 and index >= rlens[0]:
       rind = index // rlens[0]
       cind = index % rlens[0]
       if rstyle: return rind+1, cind
       else:      return rind, cind

    cind = index
    for rind, nt in enumerate(rlengths):
       if nt < 0:
          if whine:
             print('** ind2run_tr: have negative run length in %s' % rlengths)
          return -1, -1
       if cind < nt:
          if rstyle: return rind+1, cind
          else:      return rind, cind
       cind -= nt

    if whine: print('** ind2run_tr, index %d outside run list %s' \
                    % (index, rlengths))
    return rind, cind+nt


def get_num_warp_pieces(dset, verb=1):
    """return the number of pieces in the the WARP_DATA transformation
       for this dataset

       Note that 12 (30 float) pieces would imply manual tlrc, while 1 piece
       would suggest auto, +acpc, or perhaps an external transform.

       dset  : (string) name of afni dataset

       if len is a multiple of 30, return len(WARP_DATA)//30
       else return 0"""

    err, result = get_typed_dset_attr_list(dset, 'WARP_DATA', float, verb=verb)

    if err: return 0            # errors printed in called function

    nvals = len(result)
    npieces = nvals//30
    if npieces * 30 != nvals:
        print('** GNWP: invalid WARP_DATA length %d' % nvals)
        return 0

    if verb > 1: print('-- dset %s has a %d-piece warp' % (dset, npieces))

    del(result)

    return npieces

def get_typed_dset_attr_list(dset, attr, atype, verb=1):
    """given an AFNI dataset, return err (0=success), [typed attr list]

       dset  : (string) name of afni dataset
       attr  : (string) attribute name
       atype : (string) return type of attribute list
       verb  : verbose level

       This ends up running 3dAttribute for the given dataset name.
    """

    alist = BASE.read_attribute(dset, attr, verb=verb)
    if alist == None and verb > 0:
        print("** GTDAL: failed to read dset attr %s, dset = %s" % (attr,dset))
        return 1, []

    err = 0
    try: result = [atype(val) for val in alist]
    except:
        if verb > 0:
           print("** GTDAL: failed to convert attr %s to type %s for dset %s"%\
                 (attr, atype, dset))
        err = 1
        result = []

    return err, result

def get_dset_history(dname, lines=1):
   """run 3dinfo -history on 'dname' and return the list of lines"""
   command = '3dinfo -history %s' % dname
   status, olines = exec_tcsh_command(command, lines=lines, noblank=1)
   if status: return []
   return olines

def get_last_history_command(dname, substr):
   """run 3dinfo -history on 'dname' and return the last line
          containing 'substr'
      return one text line, or '' on failure
   """
   olines = get_dset_history(dname)
   olen = len(olines)
   if olen == 0: return ''

   # work backwards
   for ind in range(olen-1, -1, -1):
      if olines[ind].find(substr) >= 0: return olines[ind]

   return ''

def get_last_history_ver_pack(dname):
   """run 3dinfo -history on 'dname' and return the last line
          containing a valid version
      return status and the version and package strings
             status = 0 on success, 1 on error
   """
   olines = get_dset_history(dname)
   olen = len(olines)
   if olen == 0: return ''

   # work backwards and extract the first valid version found
   for ind in range(olen-1, -1, -1):
      st, vstrs = find_afni_history_version(olines[ind])
      if st == 0:
         return 0, vstrs[0], vstrs[1]

   return 1, '', ''

def get_last_history_version(dname):
   """get_last_history_version(DSET_NAME):
      return the last AFNI version from the HISTORY
      return an empty string on error
   """
   st, aver, package = get_last_history_ver_pack(dname)
   return aver

def get_last_history_package(dname):
   """get_last_history_package(DSET_NAME):
      return the last AFNI package from the HISTORY
      return an empty string on error
   """
   st, aver, package = get_last_history_ver_pack(dname)
   return package

def find_afni_history_version(av_str):
   """given {AFNI_A.B.C:PACKAGE},
      return the status and [AFNI_A.B.C, PACKAGE] pair as a list
      return 1, [] on error
   """
   re_format = '{(AFNI_\d+\.\d+\.\d+):(.*)}'

   try:    match = re.search(re_format, av_str)
   except: return 1, []

   if match is None:
      return 1, []

   # we have a match, just verify that we found all 4 pieces
   rv = list(match.groups())

   if len(rv) != 2: return 1, []

   return 0, rv

def parse_afni_version(av_str):
   """given 'AFNI_18.2.10', return status 0 and the int list [18,2,10]
      return 1, [] on error
   """
   re_format = 'AFNI_(\d+)\.(\d+)\.(\d+)'

   try:    match = re.search(re_format, av_str)
   except: return 1, []

   if match is None:
      return 1, []

   # we have a match, convert to ints
   rv = list(match.groups())

   if len(rv) != 3: return 1, []

   # return what we have
   try: return 0, [int(val) for val in rv]
   except: return 1, []

def get_3dinfo(dname, lines=0, verb=0):
   """run 3dinfo, possibly -verb or -VERB
      if lines, splitlines
      return text, or None on failure (applies to string or lines)
   """
   vstr = ' '
   if verb == 1: vstr = ' -verb'
   elif verb > 1: vstr = ' -VERB'
   command = '3dinfo%s %s' % (vstr, dname)
   status, output = exec_tcsh_command(command, lines=lines, noblank=1)
   if status: return None

   return output

def get_3dinfo_nt(dname, verb=1):
   """run 3dinfo -nt

      return 0 on failure (>= 0 on success)
   """
   command = '3dinfo -nt %s' % dname
   status, output, se = limited_shell_exec(command, nlines=1)
   if status or len(output) == 0:
      if verb: print('** 3dinfo -nt failure: message is:\n%s%s\n' % (se,output))
      return 0

   output = output[0].strip()
   if output == 'NO-DSET' :
      if verb: print('** 3dinfo -nt: no dataset %s' % dname)
      return 0

   nt = 0
   try: nt = int(output)
   except:
      if verb: print('** 3dinfo -nt: cannot get NT from %s, for dset %s' \
                     % (output, dname))
      return 0

   return nt

def get_3dinfo_val(dname, val, vtype, verb=1):
   """run 3dinfo -val, and convert to vtype (also serves as a test)

      return vtype(0) on failure
   """
   command = '3dinfo -%s %s' % (val, dname)
   status, output, se = limited_shell_exec(command, nlines=1)
   if status or len(output) == 0:
      if verb:
         print('** 3dinfo -%s failure: message is:\n%s%s\n' % (val, se, output))
      return 0

   output = output[0].strip()
   if output == 'NO-DSET' :
      if verb: print('** 3dinfo -%s: no dataset %s' % (val, dname))
      return 0

   dval = 0
   try: dval = vtype(output)
   except:
      # allow conversion from float to int as a backup
      fail = 0
      if vtype == int:
         try:
            dval = float(output)
            dval = vtype(dval)
         except:
            fail = 1
      if verb and fail:
         print("** 3dinfo -%s: cannot get val from %s, for dset %s" \
               % (val, output, dname))
      if fail: return vtype(0)

   return dval

def get_3dinfo_val_list(dname, val, vtype, verb=1):
   """run 3dinfo -val, and convert to vtype (also serves as a test)

      return None on failure, else a list
   """
   command = '3dinfo -%s %s' % (val, dname)
   status, output, se = limited_shell_exec(command, nlines=1)
   if status or len(output) == 0:
      if verb:
         print('** 3dinfo -%s failure: message is:\n%s%s\n' % (val, se, output))
      return None

   output = output[0].strip()
   if output == 'NO-DSET' :
      if verb: print('** 3dinfo -%s: no dataset %s' % (val, dname))
      return None

   dlist = string_to_type_list(output, vtype)
   if dlist == None and verb:
      print("** 3dinfo -%s: cannot get val list from %s, for dset %s" \
            % (val, output, dname))

   return dlist

def dset_view(dname):
   """return the AFNI view for the given dset"""
   command = '3dinfo -av_space %s' % dname
   status, output = exec_tcsh_command(command)
   if status: return ''
   return output.replace('\n', '')

def get_3d_statpar(dname, vindex, statcode='', verb=0):
   """return a single stat param at the given sub-brick index
      if statcode, verify
      return -1 on error
   """
   ilines = get_3dinfo(dname, lines=1, verb=1)
   if ilines == None:
      print('** failed get_3dinfo(%s)' % dname)
      return -1

   N = len(ilines)

   # find 'At sub-brick #v' line
   sstr = 'At sub-brick #%d' % vindex
   posn = -1
   for ind, line in enumerate(ilines):
      posn = line.find(sstr)
      if posn >= 0: break

   if posn < 0:
      print('** 3d_statpar: no %s[%d]' % (dname, vindex))
      return -1       # failure

   # check statcode?
   lind = ind + 1
   if lind >= N:
      if verb > 1: print('** 3d_statpar: no space for statpar line')
      return -1

   sline = ilines[lind]
   plist = sline.split()
   if statcode: 
      olist = find_opt_and_params(sline, 'statcode', 2)
      if len(olist) < 3:
         print('** 3d_statpar: missing expected statcode')
         return -1
      code = olist[2]
      if code[-1] == ';': code = code[:-1]
      if code != statcode:
         print('** 3d_statpar: statcode %s does not match expected %s'\
               % (code, statcode))
         return -1
      if verb > 2: print('-- found %s' % olist)

   # now get something like "statpar = 32 x x"
   olist = find_opt_and_params(sline, 'statpar', 4)
   if len(olist) < 3:
      if verb: print('** 3d_statpar: missing expected statpar')
      if verb > 2: print('   found %s in %s' % (olist, sline))
      return -1 
   if verb > 2: print('-- found %s' % olist)

   par = -1
   try: par = int(olist[2])
   except:
      if verb: print('** 3d_statpar: bad stat par[2] in %s' % olist)
      return -1 

   return par

def find_opt_and_params(text, opt, nopt=0):
   """given some text, return the option with that text, as well as
      the following 'nopt' parameters (truncated list if not found)"""
   tlist = text.split()

   if not opt in tlist: return []

   tind = tlist.index(opt)

   return tlist[tind:tind+1+nopt]

def get_truncated_grid_dim(dset, verb=1):
    """return a new (isotropic) grid dimension based on the current grid
       - given md = min(DELTAS), return md truncated to 3 significant bits
                    (first integer this affects is 9->8, then 11->10, etc.)
       - return <= 0 on failure
    """
    err, dims = get_typed_dset_attr_list(dset, 'DELTA', float)
    if err: return -1
    if len(dims) < 1: return -1
    for ind in range(len(dims)):
        dims[ind] = abs(dims[ind])
    md = min(dims)
    # changed 2 -> 4  19 Mar 2010 
    if md >= 4.0: return math.floor(md)
    if md <= 0:
        print('** failed to get truncated grid dim from %s' % dims)
        return 0

    return truncate_to_N_bits(md, 3, verb=verb, method='r_then_t')

def truncate_to_N_bits(val, bits, verb=1, method='trunc'):
    """truncate the real value to most significant N bits
       allow for any real val and positive integer bits

       method   trunc           - truncate to 'bits' significant bits
                round           - round to 'bits' significant bits
                r_then_t        - round to 2*bits sig bits, then trunc to bits
    """

    # allow any real val
    if val == 0.0: return 0.0
    if val < 0.0: sign, fval = -1, -float(val)
    else:         sign, fval =  1,  float(val)

    if verb > 2: print('T2NB: applying sign=%d, fval=%g' % (sign,fval))

    # if r_then_t, start by rounding to 2*bits, then continue to truncate
    meth = method
    if method == 'r_then_t':
        fval = truncate_to_N_bits(val,2*bits,verb,'round')
        meth = 'trunc'

    if bits <= 0 or type(bits) != type(1):
        print("** truncate to N bits: bad bits = ", bits)
        return 0.0

    # find integer m s.t.  2^(bits-1) <= 2^m * fval < 2^bits
    log2 = math.log(2.0)
    m    = int(math.ceil(bits-1 - math.log(fval)/log2))
    pm   = 2**m

    # then (round or) truncate to an actual integer in that range
    # and divide by 2^m (cannot be r_then_t here)
    if meth == 'round': ival = round(pm * fval)
    else:               ival = math.floor(pm * fval)
    retval = sign*float(ival)/pm
    
    if verb > 2:
        print('-- T2NB: 2^%d <= 2^%d * %g < 2^%d' % (bits-1,m,fval,bits))
        print('         ival = %g, returning %g' % (ival,retval))

    return retval

def test_truncation(top=10.0, bot=0.1, bits=3, e=0.0000001):
    """starting at top, repeatedly truncate to bits bits, and subtract e,
       while result is greater than bot"""

    print('-- truncating from %g down to %g with %d bits' % (top,bot,bits))
    val = top
    while val > bot:
        trunc = truncate_to_N_bits(val,bits)
        print(val, ' -> ', trunc)
        val = trunc - e
    
def get_dset_reps_tr(dset, notr=0, verb=1):
    """given an AFNI dataset, return err, reps, tr

       if notr: use 3dinfo -nt

       err  = error code (0 = success, else failure)
       reps = number of TRs in the dataset
       tr   = length of TR, in seconds
    """

    # use 3dinfo directly, instead of TAXIS attributes  30 Jun 2016

    reps = get_3dinfo_val(dset, 'nt', int, verb=verb)
    tr = get_3dinfo_val(dset, 'tr', float, verb=verb)

    # store timing info in a list (to get reps and timing units)
    if reps == 0:
        print("** failed to find the number of TRs from dset '%s'" % dset)
        return 1, None, None

    if verb > 1: print('-- dset %s : reps = %d, tr = %ss' % (dset, reps, tr))

    return 0, reps, tr

def gaussian_width_to_fwhm(width, mode):
    """convert the given 'width' of gaussian 'mode' to FWHM
       (full width at half max)

       mode can be one of: fwhm, rms, sigma

            conversion based on valid mode is:
                rms   = 0.42466090 * fwhm
                sigma = 0.57735027 * rms

            implying:
                fwhm = 2.354820 * rms
                fwhm = 4.078668 * sigma

        return 0 on failure or error"""

    if width <= 0.0: return 0.0
    if mode == 'fwhm':  return width
    if mode == 'rms':   return width * 2.354820
    if mode == 'sigma': return width * 4.078668

    print("** GW2F: illegal mode '%s'" % mode)

    return 0.0

def attr_equals_val(object, attr, val):
    """return 1 of the object has attribute attr and it equals val"""

    rv = 0
    try:
       oval = getattr(object, attr)
       if oval == val: rv = 1
    except: pass

    return rv

# ----------------------------------------------------------------------
# begin matrix functions

def num_cols_1D(filename):
    """return the number of columns in a 1D file"""
    mat = TD.read_1D_file(filename)
    if not mat or len(mat) == 0: return 0
    return len(mat[0])

def num_rows_1D(filename):
    """return the number of columns in a 1D file"""
    mat = TD.read_1D_file(filename)
    if not mat: return 0
    return len(mat)

def max_dim_1D(filename):
    """return the larger of the number of rows or columns"""
    mat = TD.read_1D_file(filename)
    if not mat: return 0
    rows = len(mat)
    cols = len(mat[0])
    if rows >= cols: return rows
    else:            return cols

def transpose(matrix):
    """transpose a 2D matrix, returning the new one"""
    rows = len(matrix)
    cols = len(matrix[0])
    newmat = []
    for c in range(cols):
        newrow = []
        for r in range(rows):
            newrow.append(matrix[r][c])
        newmat.append(newrow)
    return newmat

def derivative(vector, in_place=0, direct=0):
    """take the derivative of the vector, setting d(t=0) = 0

        in_place: if set, the passed vector will be modified
        direct:   if 0, use backward difference, if 1 use forward

       return v[t]-v[t-1] for 1 in {1,..,len(v)-1}, d[0]=0"""

    if type(vector) != type([]):
        print("** cannot take derivative of non-vector '%s'" % vector)
        return None

    if in_place: vec = vector    # reference original
    else:        vec = vector[:] # start with copy
    
    # count from the end to allow memory overwrite
    if direct:  # forward difference
       vlen = len(vec)
       for t in range(vlen-1):
           vec[t] = vec[t+1] - vec[t]
       vec[vlen-1] = 0
    else:       # backward difference
       for t in range(len(vec)-1, 0, -1):
           vec[t] -= vec[t-1]
       vec[0] = 0

    return vec

def make_timing_string(data, nruns, tr, invert=0):
   """evaluating the data array as boolean (zero or non-zero), return
      non-zero entries in stim_times format

      data      : single vector (length must be multiple of nruns)
      nruns     : number of runs (must divide len(data))
      tr        : TR in seconds, with data viewed as per TR

      invert    : (optional) invert the boolean logic (write 0 times)

      return err code (0 on success), stim_times string"""

   if not data:
      print("** make_timing_string: missing data")
      return 1, ''
   if not type(data) == type([]):
      print("** make_timing_string: data is not a list")
      return 1, ''

   nvals = len(data)
   rlen  = nvals // nruns

   if nruns * rlen != nvals:
      print("** make_timing_str: nruns %d does not divide nvals %d"%(rlen,nvals))
      return 1, ''
   if tr <= 0.0:
      print("** make_timing_string: bad tr = %g" % tr)
      return 1, ''

   rstr = ''

   for run in range(nruns):
      bot = run*rlen
      if invert: rvals = [1*(data[i] == 0) for i in range(bot,bot+rlen)]
      else:      rvals = [1*(data[i] != 0) for i in range(bot,bot+rlen)]
      # if the run is empty, print 1 or 2 '*'
      nzero = rvals.count(0)
      if nzero == rlen:
         if run == 0: rstr += '* *'
         else:        rstr += '*'
      else:
         rstr += ' '.join(['%g'%(i*tr) for i in range(rlen) if rvals[i]])

      # if run0 and exactly 1 non-zero value, print a trailing '*'
      if run == 0 and nzero == rlen-1: rstr += ' *'
      rstr += '\n'

   return 0, rstr

def make_CENSORTR_string(data, nruns=0, rlens=[], invert=0, asopt=0, verb=1):
   """evaluating the data array as boolean (zero or non-zero), return
      non-zero entries in CENSORTR format

      data      : single vector (length must be multiple of nruns)
      nruns     : number of runs (must divide len(data))
                  (ignored if rlens is provided)
      rlens     : run lengths (required if run lengths vary)
      asopt     : if set, return as complete -CENSORTR option

      invert    : (optional) invert the boolean logic (write 0 TRs)

      return err code (0 on success), CENSORTR string"""

   if not data:
      print("** CENSORTR_str: missing data")
      return 1, ''
   if not type(data) == type([]):
      print("** CENSORTR_str: data is not a list")
      return 1, ''

   nvals = len(data)

   # we must have either nruns or a valid rlens list
   if nruns <= 0 and len(rlens) < 1:
      print('** make_CENSORTR_string: neither nruns nor rlens')
      return 1, ''

   if rlens:
      rlist = rlens
      runs  = len(rlist)
   else:
      rlist = [(nvals//nruns) for run in range(nruns)]
      runs  = nruns

   if verb > 1:
      print('-- CENSORTR: applying run lengths (%d) : %s' % (runs, rlist))

   if loc_sum(rlist) != nvals:
      print("** CENSORTR_str: sum of run lengths %d != nvals %d" \
            % (loc_sum(rlist),nvals))
      return 1, ''

   rstr = ''

   bot = 0
   for run in range(runs):
      rlen = rlist[run]
      if invert: rvals = [1*(data[i] == 0) for i in range(bot,bot+rlen)]
      else:      rvals = [1*(data[i] != 0) for i in range(bot,bot+rlen)]
      bot += rlen  # adjust bottom index for next run

      # if the run is empty, print 1 or 2 '*'
      nzero = rvals.count(0)
      if nzero == rlen: continue

      # make a ',' and '..' string listing TR indices
      estr = encode_1D_ints([i for i in range(rlen) if rvals[i]])

      # every ',' separated piece needs to be preceeded by RUN:
      rstr += "%d:%s " % (run+1, estr.replace(',', ',%d:'%(run+1)))

   if asopt and rstr != '': rstr = "-CENSORTR %s" % rstr

   return 0, rstr


# end matrix functions
# ----------------------------------------------------------------------
# index encode/decode functions

def encode_1D_ints(ilist):
   """convert a list of integers to a ',' and '..' separated string"""
   if not ilist: return ''
   if len(ilist) < 1: return ''

   text = '%d' % ilist[0]
   prev = ilist[0]
   ind  = 1
   while ind < len(ilist):
      ncontinue = consec_len(ilist, ind-1) - 1
      if ncontinue <= 1:     # then no '..' continuation, use ','
         text = text + ',%d' % ilist[ind]
         ind += 1
      else:
         text = text + '..%d' % ilist[ind+ncontinue-1]
         ind += ncontinue

   return text

def consec_len(ilist, start):
   """return the length of consecutive integers - always at least 1"""
   prev = ilist[start]
   length = len(ilist)
   for ind in range(start+1, length+1):
      if ind == length: break
      if ilist[ind] != prev + 1:
         break
      prev = ilist[ind]
   if ind == start:  length = 1
   else:             length = ind-start

   return length

def restrict_by_index_lists(dlist, ilist, base=0, nonempty=1, verb=1):
    """restrict elements of dlist by indices in ilist

        ilist    : can be string or list of strings
                  (require unique composite list)
        base     : can be 0 or 1 (0-based or 1-based)
        nonempty : if set, sub-lists are not allowed to be empty
        verb     : verbose level, default is to only report errors

       return status, sub-list
              status = 0 on success, 1 on error
    """

    # if either object is empty, there is nothing to do
    if not ilist or not dlist: return 0, []

    if type(ilist) == str: ilist = [ilist]

    if base not in [0,1]:
        if verb: print('** restrict_by_index_list: bad base = %d' % base)
        return 1, []

    # set imax to correctly imply '$' index
    if base: imax = len(dlist)          # 1-based
    else:    imax = len(dlist)-1        # 0-based

    composite = []
    for ind, istr in enumerate(ilist):
        if type(istr) != str:
            print('** RBIL: bad index selector %s' % istr)
            return 1, []
        curlist = decode_1D_ints(istr, verb=verb, imax=imax)
        if not curlist and nonempty:
            if verb: print("** empty index list for istr[%d]='%s'" % (ind,istr))
            return 1, []
        composite.extend(curlist)
        if verb > 3: print('-- index %d, ilist %s' % (ind, curlist))

    if not vals_are_unique(composite):
        if verb: print('** RBIL: composite index list elements are not unique')
        return 1, []

    cmin = min(composite)
    cmax = max(composite)
    if cmin < 0:
        if verb: print('** RBIL: cannot choose negative indices')
        return 1, []
    elif base and cmin == 0:
        if verb: print('** RBIL: 1-based index list seems 0-based')
        return 1, []
    elif cmax > imax:
        if verb: print('** RBIL: index value %d exceeds %d-based limit %d' \
                       % (cmax, base, imax))
        return 1, []

    # now convert 1-based to 0-based, if needed
    if base: clist = [v-1 for v in composite]
    else:    clist = composite

    # the big finish
    return 0, [dlist[ind] for ind in clist]

def decode_1D_ints(istr, verb=1, imax=-1):
    """Decode a comma-delimited string of ints, ranges and A@B syntax,
       and AFNI-style sub-brick selectors (including A..B(C)).
       If the A..B format is used, and B=='$', then B gets 'imax'.
       If the list is enclosed in [], <> or ##, strip those characters.
       - return a list of ints"""

    newstr = strip_list_brackets(istr, verb)
    slist = newstr.split(',')
    if len(slist) == 0:
        if verb > 1: print("-- empty 1D_ints from string '%s'" % istr)
        return []
    elif verb > 3: print("-- decoding stripped list '%s'" % newstr)
    ilist = []                  # init return list
    for s in slist:
        try:
            if s.find('@') >= 0:        # then expect "A@B"
                [N, val] = [n for n in s.split('@')]
                N = int(N)
                val = to_int_special(val, '$', imax)
                ilist.extend([val for i in range(N)])
            elif s.find('..') >= 0:     # then expect "A..B"
                pos = s.find('..')
                if s.find('(', pos) > 0:    # look for "A..B(C)"
                   [v1, v2] = [n for n in s.split('..')]
                   v1 = to_int_special(v1, '$', imax)
                   [v2, step] = v2.split('(')
                   v2 = to_int_special(v2, '$', imax)
                   # have start and end values, get step
                   step, junk = step.split(')')
                   step = int(step)
                   if   step > 0: inc = 1
                   elif step < 0: inc = -1
                   else:
                        print("** decode: illegal step of 0 in '%s'" % istr)
                        return []
                   ilist.extend([i for i in range(v1, v2+inc, step)])
                else:
                   [v1, v2] = [n for n in s.split('..')]
                   v1 = to_int_special(v1, '$', imax)
                   v2 = to_int_special(v2, '$', imax)
                   if v1 < v2 : step = 1
                   else:        step = -1
                   ilist.extend([i for i in range(v1, v2+step, step)])
            else:
                ilist.extend([int(s)])
        except:
            print("** cannot decode_1D '%s' in '%s'" % (s, istr))
            return []
    if verb > 3: print('++ ilist: %s' % ilist)
    del(newstr)
    return ilist

def to_int_special(cval, spec, sint):
   """basically return int(cval), but if cval==spec, return sint

        cval: int as character string
        spec: special value as string
        sint: special value as int"""
   if cval == spec: return sint
   else:            return int(cval)

def extract_subbrick_selection(sstring):
   """search sstring for something like: [DIGITS*($|DIGITS)]
      - do not consider all DIGITS, '..', ',', '(DIGITS)' pieces,
        just let '*' refer to anything but another '['
   """
   import re
   res = re.search('\[\d+[^\[]*]', sstring)
   if res == None: return ''
   return res.group(0)

def strip_list_brackets(istr, verb=1):
   """strip of any [], {}, <> or ## surrounding this string
        - assume only one pair
        - allow the trailing character to be missing
      return the remaining string"""

   # strip any of these pairs
   for pairs in [ ['[',']'],  ['{','}'],  ['<','>'],  ['#','#'] ]:

      ind0 = istr.find(pairs[0])
      if ind0 >= 0:
         ind1 = istr.find(pairs[1], ind0+1)
         if verb > 1: print('-- stripping %s%s at %d,%d in %s' % \
                            (pairs[0],pairs[1],ind0,ind1,istr))
         if ind1 > ind0: return istr[ind0+1:ind1]
         else:           return istr[ind0+1:]

   if verb > 2: print("-- nothing to strip from '%s'" % istr)

   return istr

def replace_n_squeeze(instr, oldstr, newstr):
   """like string.replace(), but remove all spaces around oldstr
      (so probably want some space in newstr)"""
   # while oldstr is found
   #   find last preceeding keep posn (before oldstr and spaces)
   #   find next following keep posn (after oldstr and spaces)
   #   set result = result[0:first] + newstr + result[last:]
   newlen = len(newstr)
   result = instr
   posn = result.find(oldstr)
   while posn >= 0:
      rlen = len(result)
      start = posn-1
      while start >= 0 and result[start] == ' ': start -= 1
      if start >= 0: newres = result[0:start+1] + newstr
      else:          newres = newstr
      end = posn + newlen
      while end < rlen and result[end] == ' ': end += 1
      if end < rlen: newres += result[end:]

      result = newres
      posn = result.find(oldstr)

   return result

# ----------------------------------------------------------------------
# line wrapper functions

# add line wrappers ('\'), and align them all
def add_line_wrappers(commands, wrapstr='\\\n', verb=1):
    """wrap long lines with 'wrapstr' (probably '\\\n' or just '\n')
       if '\\\n', align all wrapstr strings"""
    new_cmd = ''
    posn = 0

    while needs_wrapper(commands, 78, posn):
            
        end = find_command_end(commands, posn)

        if not needs_wrapper(commands, 78, posn, end): # command is okay
            if end < 0: new_cmd = new_cmd + commands[posn:]
            else      : new_cmd = new_cmd + commands[posn:end+1]
            posn = end+1
            continue

        # command needs wrapping
        new_cmd += insert_wrappers(commands,posn,end,wstring=wrapstr,verb=verb)

        posn = end + 1     # else, update posn and continue

    result = new_cmd + commands[posn:]

    # wrappers are in, now align them
    if wrapstr == '\\\n': return align_wrappers(result)
    else:                 return result

def align_wrappers(command):
    """align all '\\\n' strings to be the largest offset
       from the previous '\n'"""

    # first, find the maximum offset
    posn = 0
    max  = -1
    while 1:
        next = command.find('\n',posn)
        if next < 0: break
        if next > posn and command[next-1] == '\\':  # check against max
            width = next - 1 - posn
            if width > max: max = width
        posn = next + 1 # look past it

    if max < 0: return command  # none found

    # repeat the previous loop, but adding appropriate spaces
    new_cmd = ''
    posn = 0
    while 1:
        next = command.find('\n',posn)
        if next < 0: break
        if next > posn and command[next-1] == '\\':  # check against max
            width = next - 1 - posn
            if width < max:     # then insert appropriate spaces
                new_cmd += command[posn:next-1] + ' '*(max-width) + '\\\n'
                posn = next + 1
                continue

        # just duplicate from the previous posn
        new_cmd += command[posn:next+1]
        posn = next + 1 # look past it

    if posn < len(command): new_cmd += command[posn:]

    return new_cmd

def insert_wrappers(command, start=0, end=-1, wstring='\\\n', verb=1):
    """insert any '\\' chars for the given command
         - insert between start and end positions
         - apply specified wrap string wstring
       return a new string, in any case"""

    global wrap_verb

    if end < 0: end = len(command) - start - 1

    nfirst = num_leading_line_spaces(command,start,1) # note initial indent
    prefix = get_next_indentation(command,start,end)
    sskip  = nfirst             # number of init spaces expected
    plen   = len(prefix)
    maxlen = 78
    newcmd = ''
    cur    = start

    if verb > 1: print("+d insert wrappers: nfirst=%d, prefix='%s', plen=%d" \
                       % (nfirst, prefix, plen))

    #pdb.set_trace()

    # rewrite: create new command strings after each wrap     29 May 2009
    while needs_wrapper(command,maxlen,cur,end):
        endposn = command.find('\n',cur)
        if needs_wrapper(command,maxlen,cur,endposn):  # no change on this line

            lposn = find_last_space(command, cur+sskip, endposn, maxlen-sskip)

            # if the last space is farther in than next indent, wrap
            # (adjust initial skip for any indent)
            if sskip+cur < lposn:   # woohoo, wrap away (at lposn)
                newcmd = newcmd + command[cur:lposn+1] + wstring
                # modify command to add prefix, reset end and cur
                command = prefix + command[lposn+1:]
                end = end + plen - (lposn+1)
                sskip = nfirst + plen   # now there is a prefix to skip
                cur = 0
                continue

        # no change:
        # either the line does not need wrapping, or there is no space to do it
        if endposn < 0: endposn = end     # there may not be a '\n'
        newcmd += command[cur:endposn+1]
        cur = endposn + 1

    if cur <= end: newcmd += command[cur:end+1]   # add remaining string

    return newcmd

def get_next_indentation(command,start=0,end=-1):
    """get any '#' plus leading spaces, from beginning or after first '\\\n'"""
    if end < 0: end = len(command) - start - 1

    spaces = num_leading_line_spaces(command,start,1)
    prefix = command[start:start+spaces]+'    ' # grab those spaces, plus 4
    # now check for an indention prefix
    posn = command.find('\\\n', start)
    pn = command.find('\n', start)      # but don't continue past current line
    if posn >= 0 and posn < pn:
        spaces = num_leading_line_spaces(command,posn+2,1)
        if posn > start and spaces >= 2:
            prefix = command[posn+2:posn+2+spaces] # grab those spaces

    return prefix

def needs_wrapper(command, maxlen=78, start=0, end=-1):
    """does the current string need line wrappers

       a string needs wrapping if there are more than 78 characters between
       any previous newline, and the next newline, wrap, or end"""

    if end < 0: end_posn = len(command) - 1
    else:       end_posn = end

    cur_posn = start
    remain = end_posn - cur_posn
    while remain > maxlen:
        
        # find next '\\\n'
        posn = command.find('\\\n', cur_posn)
        if 0 <= posn-cur_posn <= maxlen: # adjust and continue
            cur_posn = posn + 2
            remain = end_posn - cur_posn
            continue

        # else find next '\n'
        posn = command.find('\n', cur_posn)
        if 0 <= posn-cur_posn <= maxlen: # adjust and continue
            cur_posn = posn + 1
            remain = end_posn - cur_posn
            continue

        # otherwise, space means wrap, else not
        if find_next_space(command, cur_posn, 1) > cur_posn: return 1
        return 0

    return 0        # if we get here, line wrapping is not needed

def find_command_end(command, start=0):
    """find the next '\n' that is not preceeded by '\\', or return the
       last valid position (length-1)"""

    length = len(command)
    end = start-1
    while 1:
        start = end + 1
        end = command.find('\n',start)

        if end < 0: return length-1   # not in command
        elif end > start and command[end-1] == '\\':
            if length > end+1 and command[start] == '#'   \
                              and command[end+1] != '#':
                return end      # since comments cannot wrap
            else: continue 
        return end              # found

def num_leading_line_spaces(istr,start,pound=0):
    """count the number of leading non-whitespace chars
       (newline chars are not be counted, as they end a line)
       if pound, skip any leading '#'"""

    length = len(istr)
    if start < 0: start = 0
    if length < 1 or length <= start: return 0
    posn = start
    if pound and istr[posn] == '#': posn += 1

    while posn < length and istr[posn].isspace() and istr[posn] != '\n':
        posn += 1

    if posn == length: return 0   # none found
    return posn-start             # index equals num spaces from start

def find_next_space(istr,start,skip_prefix=0):
    """find (index of) first space after start that isn't a newline
       (skip any leading indendation if skip_prefix is set)
       return -1 if none are found"""

    length = len(istr)
    index  = start
    if skip_prefix: index += num_leading_line_spaces(istr,start,1)
    
    while 1:
        if index >= length: break
        if istr[index] != '\n' and istr[index].isspace(): break
        index += 1

    if index >= length : return -1
    return index

def find_last_space(istr,start,end,max_len=-1,stretch=1):
    """find (index of) last space in current line range that isn't a newline
       if stretch and not found, search towards end
       return start-1 if none are found"""

    if end < 0: end = len(istr) - 1
    if max_len >= 0 and end-start >= max_len: index = start+max_len-1
    else:                                     index = end

    posn = index        # store current position in case of stretch
    
    while posn >= start and (istr[posn] == '\n' or not istr[posn].isspace()):
        posn -= 1

    if posn < start and stretch:       # then search towards end
        posn = index
        while posn <= end and (istr[posn] == '\n' or not istr[posn].isspace()):
            posn += 1
        if posn > end: posn = start-1 # still failed

    return posn   # for either success or failure

def nuke_final_whitespace(script, skipchars=[' ', '\t', '\n', '\\'],
                                append='\n\n'):
    """replace final white characters with newlines"""
    slen = len(script)
    ind = slen-1
    while ind > 0 and script[ind] in skipchars: ind -= 1

    return script[0:ind+1]+append

# end line_wrapper functions
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# other functions

# 17 May, 2008 [rickr]
def vals_are_multiples(num, vals, digits=4):
    """decide whether every value in 'vals' is a multiple of 'num'
       (vals can be a single float or a list of them)

       Note, 'digits' can be used to specify the number of digits of accuracy
       in the test to see if a ratio is integral.  For example:
           vals_are_multiples(1.1, 3.3001, 3) == 1
           vals_are_multiples(1.1, 3.3001, 4) == 0

       return 1 if true, 0 otherwise (including error)"""

    if num == 0.0: return 0

    try:
        l = len(vals)
        vlist = vals
    except:
        vlist = [vals]

    for val in vlist:
        rat = val/num
        rem = rat - int(rat)

        if round(rem,digits) != 0.0: return 0

    return 1

def vals_are_constant(vlist, cval=None):
   """determine whether every value in vlist is equal to cval
      (if cval == None, use vlist[0])"""

   if vlist == None: return 1
   if len(vlist) < 2: return 1

   if cval == None: cval = vlist[0]

   for val in vlist:
      if val != cval: return 0
   return 1

def vals_are_positive(vlist):
   """determine whether every value in vlist is positive"""
   for val in vlist:
      if val <= 0: return 0
   return 1

def vals_are_0_1(vlist):
   """determine whether every value in vlist is either 0 or 1"""
   for val in vlist:
      if val != 0 and val != 1: return 0
   return 1

def vals_are_sorted(vlist, reverse=0):
   """determine whether values non-decreasing (or non-inc if reverse)"""
   if vlist == None: return 1
   if len(vlist) < 2: return 1

   rval = 1
   try:
      for ind in range(len(vlist)-1):
         if reverse:
            if vlist[ind] < vlist[ind+1]:
               rval = 0
               break
         else:
            if vlist[ind] > vlist[ind+1]:
               rval = 0
               break
   except:
      print("** failed to detect sorting in list: %s" % vlist)
      rval = 0
      
   return rval

def vals_are_increasing(vlist, reverse=0):
   """determine whether values strictly increasing (or dec if reverse)"""
   if vlist == None: return 1
   if len(vlist) < 2: return 1

   rval = 1
   try:
      for ind in range(len(vlist)-1):
         if reverse:
            if vlist[ind] <= vlist[ind+1]:
               rval = 0
               break
         else: # verify increasing
            if vlist[ind] >= vlist[ind+1]:
               rval = 0
               break
   except:
      print("** failed to detect sorting in list: %s" % vlist)
      rval = 0
      
   return rval

def vals_are_unique(vlist, dosort=1):
   """determine whether (possibly unsorted) values are unique
      - use memory to go for N*log(N) speed"""

   if vlist == None: return 1
   if len(vlist) < 2: return 1

   # copy and sort
   dupe = vlist[:]
   if dosort: dupe.sort()

   rval = 1
   try:
      for ind in range(len(dupe)-1):
         if dupe[ind] == dupe[ind+1]:
            rval = 0
            break
   except:
      print("** uniq: failed to compare list elements in %s" % vlist)
      rval = 0

   del(dupe)
      
   return rval

def lists_are_same(list1, list2, epsilon=0, doabs=0):
   """return 1 if the lists have similar values, else 0

      similar means difference <= epsilon
   """
   if not list1 and not list2: return 1
   if not list1: return 0
   if not list2: return 0
   if len(list1) != len(list2): return 0

   for ind in range(len(list1)):
      if doabs:
         v1 = abs(list1[ind])
         v2 = abs(list2[ind])
      else:
         v1 = list1[ind]
         v2 = list2[ind]
      if v1 != v2: return 0
      if epsilon:
         if abs(v1-v2) > epsilon: return 0

   return 1

def string_to_float_list(fstring):
   """return a list of floats, converted from the string
      return None on error
   """

   if type(fstring) != str: return None
   slist = fstring.split()

   if len(slist) == 0: return []

   try: flist = [float(sval) for sval in slist]
   except: return None

   return flist

def string_to_type_list(sdata, dtype=float):
   """return a list of dtype, converted from the string
      return None on error
   """

   if type(sdata) != str: return None
   slist = sdata.split()

   if len(slist) == 0: return []

   # if going to int, use float as an intermediate step
   if dtype == int:
      try: slist = [float(sval) for sval in slist]
      except: return None

   try: dlist = [dtype(sval) for sval in slist]
   except: return None

   return dlist

def float_list_string(vals, nchar=7, ndec=3, nspaces=2, mesg='', left=0):
   """return a string to display the floats:
        vals    : the list of float values
        nchar   : [7] number of characters to display per float
        ndec    : [3] number of decimal places to print to
        nspaces : [2] number of spaces between each float
   """

   if left: form = '%-*.*f%*s'
   else:    form = '%*.*f%*s'

   istr = mesg
   for val in vals: istr += form % (nchar, ndec, val, nspaces, '')

   return istr

def gen_float_list_string(vals, mesg='', nchar=0, left=0):
   """mesg is printed first, if nchar>0, it is min char width"""

   istr = mesg

   if left: form = '%-'
   else:    form = '%'

   if nchar > 0:
      form += '*g '
      for val in vals: istr += form % (nchar, val)
   else:
      form += 'g '
      for val in vals: istr += form % val

   return istr

def int_list_string(ilist, mesg='', nchar=0, sepstr=' '):
   """like float list string, but use general printing
      (mesg is printed first, if nchar>0, it is min char width)"""

   istr = mesg

   if nchar > 0: slist = ['%*d' % (nchar, val) for val in ilist]
   else:         slist = ['%d' % val for val in ilist]
   istr += sepstr.join(slist)

   return istr

def invert_int_list(ilist, top=-1, bot=0):
   """invert the integer list with respect to bot and top
      i.e. return a list of integers from bot to top that are not in
           the passed list
   """
   if top < bot:
      print('** invert_int_list requires bot<=top (have %d, %d)' % (bot, top))
      return []

   return [ind for ind in range(bot, top+1) if not ind in ilist]

def is_valid_int_list(ldata, imin=0, imax=-1, whine=0):
   """check whether:
        o  ldata is a of type []
        o  values are of type int
        o  values are in within imin..imax (only if imin <= imax)
        o  if whine: complain on error
      return 1 on true, 0 on false"""

   if not ldata or type(ldata) != type([]):
      if whine: print("** not valid as a list: '%s'" % ldata)

   for ind in range(len(ldata)):
      val = ldata[ind]
      if type(val) != type(0):
         if whine: print("** non-int value %d in int list (@ %d)" % (val,ind))
         return 0
      if imin <= imax: # then also test bounds
         if val < imin:
            if whine: print("** list value %d not in [%d,%d]" %(val,imin,imax))
            return 0
         elif val > imax:
            if whine: print("** list value %d not in [%d,%d]" %(val,imin,imax))
            return 0
   return 1

def data_to_hex_str(data):
   """convert raw data to hex string in groups of 4 bytes"""

   if not data: return ''

   dlen = len(data)             # total length in bytes
   groups = (dlen+3) // 4       # number of 4-byte blocks to create
   remain = dlen
   retstr = ''  # return string

   for group in range(groups):
      if group > 0: retstr += ' '
      retstr += '0x'
      if remain >= 4: llen = 4
      else:           llen = remain

      for ind in range(llen):
         retstr += '%02x' % data[dlen-remain+ind]

      remain -= llen

   return retstr

def section_divider(hname='', maxlen=74, hchar='=', endchar=''):
    """return a title string of 'hchar's with the middle chars set to 'hname'
       if endchar is set, put at both ends of header
       e.g. section_divider('volreg', endchar='##') """
    if len(hname) > 0: name = ' %s ' % hname
    else:              name = ''

    if endchar != '': maxlen -= 2*len(endchar)
    rmlen = len(name)
    if rmlen >= maxlen:
        print("** S_DIV_STR, rmlen=%d exceeds maxlen=%d" % (rmlen, maxlen))
        return name
    prelen  = (maxlen - rmlen) // 2     # basically half the chars
    postlen = maxlen - rmlen - prelen   # other 'half'

    return endchar + prelen*hchar + name + postlen*hchar + endchar

def get_command_str(args=[], preamble=1, comment=1, quotize=1, wrap=1):
    """return a script generation command

        args:           arguments to apply
        preample:       start with "script generated by..."
        comment:        have text '#' commented out
        quotize:        try to quotize any arguments that need it
        wrap:           add line wrappers
    """

    if args == []: args = sys.argv

    if preamble: hstr = '\n# %s\n# script generated by the command:\n#\n' \
                        % section_divider()
    else:        hstr = ''

    if comment: cpre = '# '
    else:       cpre = ''

    # note command and args
    cmd = os.path.basename(args[0])
    if quotize: args = ' '.join(quotize_list(args[1:],''))
    else:       args = ' '.join(args[1:],'')

    cstr = '%s%s%s %s\n' % (hstr, cpre, cmd, args)

    if wrap: return add_line_wrappers(cstr)
    else:    return cstr

def max_len_in_list(vlist):
    mval = 0
    try:
       for v in vlist:
          if len(v) > mval: mval = len(v)
    except:
       print('** max_len_in_list: cannot compute lengths')
    return mval

def get_rank(data, style='dense', reverse=0, uniq=0):
   """return the rank order of indices given values,
      i.e. for each value, show its ordered index
      e.g. 3.4 -0.3 4.9 2.0   ==>   2 0 3 1

      Sort the data, along with indices, then repeat on the newly
      sorted indices.  If not uniq, set vals to minimum for repeated set.
      Note that sorting lists sorts on both elements, so first is smallest.

      styles:  (e.g. given input -3 5 5 6, result is ...)

         competition:  competition ranking (result 1 2 2 4 )
         dense:        dense ranking (result 1 2 2 3 )
                       {also default from 3dmerge and 3dRank}

      return status (0=success) and the index order
   """

   dlen = len(data)

   # maybe reverse the sort order
   if reverse:
      maxval = max(data)
      dd = [maxval-val for val in data]
   else: dd = data

   # sort data and original position
   dd = [[dd[ind], ind] for ind in range(dlen)]
   dd.sort()

   # invert postion list by repeating above, but with index list as data
   # (bring original data along for non-uniq case)
   dd = [[dd[ind][1], ind, dd[ind][0]] for ind in range(dlen)]

   # deal with repeats: maybe modify d[1] from ind, depending on style
   if not uniq:
      if style == 'dense':
         cind = dd[0][1] # must be 0
         for ind in range(dlen-1):      # compare next to current
            if dd[ind+1][2] == dd[ind][2]:
               dd[ind+1][1] = cind
            else:
               cind += 1
               dd[ind+1][1] = cind
      elif style == 'competition':
         for ind in range(dlen-1):      # compare next to current
            if dd[ind+1][2] == dd[ind][2]:
               dd[ind+1][1] = dd[ind][1]
      else:
         print("** UTIL.GR: invalid style '%s'" % style)
         return 1, []

   dd.sort()

   return 0, [dd[ind][1] for ind in range(dlen)]

# ----------------------------------------------------------------------
# wildcard construction functions
# ----------------------------------------------------------------------

def first_last_match_strs(slist):
   """given a list of strings, return the first and last consistent strings
      (i.e. all strings have the form first*last)

        e.g. given ['subj_A1.txt', 'subj_B4.txt', 'subj_A2.txt' ]
             return 'subj_' and '.txt'
   """

   if type(slist) != list:
      print('** FL match strings requires a list')
      return '', ''

   if not slist: return '', ''

   maxlen = len(slist[0])
   hmatch = maxlen              # let them shrink
   tmatch = maxlen
   for sind in range(1, len(slist)):
      if slist[0] == slist[sind]: continue

      hmatch = min(hmatch, len(slist[sind]))
      tmatch = min(tmatch, len(slist[sind]))

      # find first left diff
      i = 0
      while i < hmatch:
         if slist[sind][i] != slist[0][i]: break
         i += 1
      hmatch = min(hmatch, i)

      # find first right diff (index from 1)
      i = 1
      while i <= tmatch:
         if slist[sind][-i] != slist[0][-i]: break
         i += 1
      tmatch = min(tmatch, i-1)

   if hmatch+tmatch > maxlen:           # weird, but constructable
      tmatch = maxlen - hmatch          # so shrink to fit

   # list[-0:] is not empty but is the whole list
   if tmatch > 0: tstr = slist[0][-tmatch:]
   else:          tstr = ''

   return slist[0][0:hmatch], tstr

def glob2stdout(globlist):
   """given a list of glob forms, print all matches to stdout

      This is meant to be a stream workaround to shell errors
      like, "Argument list too long".

      echo 'd1/*.dcm' 'd2/*.dcm' | afni_util.py -listfunc glob2stdout -
   """
   for gform in globlist:
      for fname in glob.glob(gform):
         print(fname)

def glob_form_from_list(slist):
   """given a list of strings, return a glob form

        e.g. given ['subjA1.txt', 'subjB4.txt', 'subjA2.txt' ]
             return 'subj*.txt'

      Somewhat opposite list_minus_glob_form().
   """

   if len(slist) == 0: return ''
   if vals_are_constant(slist): return slist[0]

   first, last = first_last_match_strs(slist)
   if not first and not last: return '' # failure
   globstr = '%s*%s' % (first,last)

   return globstr

def glob_form_matches_list(slist, ordered=1):
   """given a list of strings, make a glob form, and then test that against
      the actual files on disk

      if ordered: files must match exactly (i.e. slist must be sorted)
      else:       slist does not need to be sorted
   """

   slen = len(slist)

   # check trivial cases of lengths 0 and 1
   if slen == 0: return 1
   if slen == 1:
      if os.path.isfile(slist[0]): return 1
      else:                        return 0

   globstr = glob_form_from_list(slist)
   glist = glob.glob(globstr)
   glist.sort()

   # quick check: lengths must match
   if len(glist) != slen: return 0

   if ordered:
      inlist = slist
   else: 
      inlist = slist[:]
      inlist.sort()

   # now files must match exactly (between inlist and glist)
   for ind in range(slen):
      if glist[ind] != inlist[ind]: return 0

   # they must match
   return 1
   

def list_minus_glob_form(inlist, hpad=0, tpad=0, keep_dent_pre=0, strip=''):
   """given a list of strings, return the inner part of the list that varies
      (i.e. remove the consistent head and tail elements)

        e.g. given ['subjA1.txt', 'subjB4.txt', 'subjA2.txt' ]
             return [ 'A1', 'B4', 'A2' ]

      hpad NPAD         : number of characters to pad at prefix
      tpad NPAD         : number of characters to pad at suffix
      keep_dent_pre Y/N : (flag) keep entire prefix from directory entry
      strip             : one of ['', 'dir', 'file', 'ext', 'fext']

      If hpad > 0, then pad with that many characters back into the head
      element.  Similarly, tpad pads forward into the tail.

        e.g. given ['subjA1.txt', 'subjB4.txt', 'subjA2.txt' ]
             if hpad = 926 (or 4 :) and tpad = 1,
             return [ 'subjA1.', 'subjB4.', 'subjA2.' ]

      If keep_dent_pre is set, then (if '/' is found) decrement hlen until 
      that '/'.

        e.g. given ['dir/subjA1.txt', 'dir/subjB4.txt', 'dir/subjA2.txt' ]
                -> return = [ 'A1.', 'B4.', 'A2.' ]
             with keep_dent_pre == 1:
                -> return = [ 'subjA1.', 'subjB4.', 'subjA2.' ]

      Somewhat opposite glob_form_from_list().
   """

   if len(inlist) <= 1: return inlist

   # init with original
   slist = inlist

   # maybe make a new list of stripped elements
   stripnames = ['dir', 'file', 'ext', 'fext']
   if strip != '' and strip not in stripnames:
      print('** LMGF: bad strip %s' % strip)
      strip = ''

   if strip in stripnames:
      ss = []
      for inname in inlist:
         if strip == 'dir':
            dname, fname = os.path.split(inname)
            ss.append(fname)
         elif strip == 'file':
            dname, fname = os.path.split(inname)
            ss.append(dname)
         elif strip == 'ext':
            fff, ext = os.path.splittext(inname)
            ss.append(fff)
         elif strip == 'fext':
            fff, ext = os.path.splittext(inname)
            ss.append(fff)
         else:
            print('** LMGF: doubly bad strip %s' % strip)
            break
      # check for success
      if len(ss) == len(slist): slist = ss

   if hpad < 0 or tpad < 0:
      print('** list_minus_glob_form: hpad/tpad must be non-negative')
      hpad = 0 ; tpad = 0

   # get head, tail and note lengths
   head, tail = first_last_match_strs(slist)
   hlen = len(head)
   tlen = len(tail)

   # adjust by padding, but do not go negative
   if hpad >= hlen: hlen = 0
   else:            hlen -= hpad
   if tpad >= tlen: tlen = 0
   else:            tlen -= tpad

   # apply directory entry prefix, if requested
   if keep_dent_pre:
      s = slist[0]
      posn = s.rfind('/', 0, hlen)
      # if found, start at position to right of it
      # otherwise, use entire prefix
      if posn >= 0: hlen = posn + 1
      else:         hlen = 0

   # and return the list of center strings
   if tlen == 0: return [ s[hlen:]      for s in slist ]
   else:         return [ s[hlen:-tlen] for s in slist ]

def glob_list_minus_pref_suf(pref, suf):
   """just strip the prefix and suffix from string list elements
      (for now, assume they are there)
   """
   glist = glob.glob('%s*%s' % (pref, suf))

   plen = len(pref)
   slen = len(suf)

   return [d[plen:-slen] for d in glist]

def list_minus_pref_suf(slist, pref, suf, stripdir=1):
   """just strip the prefix and suffix from string list elements

      if stripdir, remove leading directories

      return status, stripped list

      status =  0 : all strings have prefix and suffix
                1 : not all do
               -1 : on error
   """

   plen = len(pref)
   slen = len(suf)

   # possibly strip of directory names
   if stripdir:
      flist = []
      for sname in slist:
         dd, ff = os.path.split(sname)
         flist.append(ff)
   else: flist = slist

   
   rv = 0
   rlist = []
   for fname in flist:
      if fname.startswith(pref): poff = plen
      else:                      poff = 0

      if fname.endswith(suf): soff = slen
      else:                   soff = 0

      if soff: rlist.append(fname[poff:-soff])
      else:    rlist.append(fname[poff:])

      if not poff or not soff: rv = 1

   return rv, rlist

def okay_as_lr_spec_names(fnames, verb=0):
   """check that names are okay as surface spec files, e.g. for afni_proc.py
        - must be 1 or 2 file names
        - must contain lh and rh, respectively
        - must otherwise be identical

        if verb, output why failure occurs
        return 1 if okay, 0 otherwise
   """
   nfiles = len(fnames)
   if nfiles == 0: return 1     # no problems, anyway
   if nfiles > 2:
      if verb: print('** only 1 or 2 spec files allowed (have %d)' % nfiles)
      return 0

   if nfiles == 1:
      if fnames[0].find('lh')>=0 or fnames[0].find('rh')>=0: return 1 # success
      # failure
      if verb: print("** spec file '%s' missing 'lh' or 'rh'" % fnames[0])
      return 0

   # so we have 2 files

   hlist = list_minus_glob_form(fnames, tpad=1)  # go after following 'h'

   for h in hlist:
      if h != 'rh' and h != 'lh':
         if verb: print('** multiple spec files must only differ in lh vs. rh')
         return 0

   return 1

def make_spec_var(fnames, vname='hemi'):
   """return a spec file variable and a list of replaced hemispheres

        e.g. make_spec_var(['surface.lh.spec', 'surface.rh.spec']) returns
                surface.${hemi}.spec, ['lh', 'rh']
      given 1 or 2 spec file names, return a single variable that
      represents them with $vname replacing the lh or rh

      return '' on failure
   """
   if not okay_as_lr_spec_names(fnames): return '', []

   nfiles = len(fnames)
   if nfiles == 0 or nfiles > 2: return '', []

   sfile = fnames[0]

   if nfiles == 1:
      # just find lh or rh and replace it
      hh = 'lh'
      posn = sfile.find(hh)
      if posn < 0:
         hh = 'rh'
         posn = sfile.find(hh)
      if posn < 0: return '', [] # should not happen

      return sfile[0:posn] + '${%s}'%vname + sfile[posn+2:], [hh]

   # so nfiles == 2, use glob

   head, tail = first_last_match_strs(fnames)
   hlen = len(head)

   hemi = sfile[hlen:hlen+2]
   if hemi != 'lh' and hemi != 'rh':
      print('** MSV: bad lh/rh search from spec files: %s' % fnames)
      return '', []

   return sfile[0:hlen] + '${%s}'%vname + sfile[hlen+2:], ['lh', 'rh']

def parse_as_stim_list(flist):
   """parse filename list as PREFIX.INDEX.LABEL.SUFFIX, where the separators
        can be '.', '_' or nothing (though ignore PREFIX and SUFFIX, as well
        as those separators)

        - strip PREFIX and SUFFIX (those are garbage)
          (if SUFFIX has a '.' after position 0, adjust the SUFFIX)
        - strip any leading digits as INDEXes

      return Nx2 table where if one column entry is filled, they all are
             (None on failure for form a complete table)
             (note: blank INDEX or LABEL is okay, but they must all be)
   """

   if len(flist) < 1: return []

   # first get PREFIX and SUFFIX
   prefix, suffix = first_last_match_strs(flist)

   # if suffix contains an extension, make the suffix into the extension
   dot = suffix.find('.')
   if dot < 0: dot = 0

   # strip prefix, suffix: might include part of 'suffix' in label
   inner_list = list_minus_glob_form(flist, tpad=dot)

   # then make table of the form <NUMBER><SEP><LABEL>
   s_table = [list(_parse_leading_int(name)) for name in inner_list]

   # if any number does not exist, just return inner_list as LABELs
   for entry in s_table:
      if entry[0] < 0: return [[-1, label] for label in inner_list]

   # return INDEX and LABEL (no SEP)
   return [[entry[0], entry[2]] for entry in s_table]

def _parse_leading_int(name, seplist=['.','_','-']):
   """assuming name is a string starting with digits, parse name into
      val, sep, suffix

        val    = -1 if name does not start with a digit
        sep    = one of {'.', '_', '-', ''}
        suffix = whatever remains after 'sep'
   """

   nlen = len(name)

   if nlen < 1: return -1, '', ''

   # first strip of any leading (non-negative) integer
   posn = 0     # count leading digits
   for char in name:
      if char.isdigit(): posn += 1
      else:              break

   if posn == 0: val = -1
   else:
      try: val = int(name[0:posn])
      except:
         print("** _parse_leading_int: can't parse int from %s" % name)
         return

   # if only a number, we're outta here
   if posn == nlen: return val, '', ''

   # note any separator
   if name[posn] in seplist:
      sep = name[posn]
      posn += 1
   else:
      sep = ''

   # aaaaaand, we're done
   return val, sep, name[posn:]

def glob_form_has_match(form):
   """see if anything at all exists according to this glob form"""
   glist = glob.glob(form)
   glen = len(glist)
   del(glist)
   if glen > 0: return 1
   return 0

def executable_dir(ename=''):
   """return the directory whre the ename program is located
      (by default, use argv[0])"""
   if ename == '': ee = sys.argv[0]
   else:           ee = ename

   dname = os.path.dirname(ee)
   return os.path.abspath(dname)

def common_dir(flist):
   """return the directory name that is common to all files (unless trivial)"""
   dir, junk = first_last_match_strs(flist)
   if len(dir) > 0 and dir[-1] == '/': dir = dir[0:-1]
   if not os.path.isdir(dir): dir = os.path.dirname(dir)

   if is_trivial_dir(dir): return ''
   return dir

def common_parent_dirs(flists):
   """return parent directories

      flists = lists of file names (each element should be a list)

      return:
         top_dir    (common to all parents (files), '' if not used)
         parent_dir (for each flist, common parent)
         short_dir  (for each flist, common parent under top_dir)
         short_name (for each flist, file names under parent dirs)

      if top_dir has at least 2 levels, use it
   """
   if type(flists) != list:
      print('** common_parent_dirs: bad flists type')
      return None
   for ind in range(len(flists)):
      flist = flists[ind]
      if type(flist) != list:
         print('** common_parent_dirs: bad flist[%d] type' % ind)
         return None, None, None, None

   # get top_dir and parents
   all_pars    = []
   par_dirs    = []
   short_names = []
   for flist in flists:
      # track parent dirs
      parent = common_dir(flist)
      if parent == '/' or is_trivial_dir(parent): parent = ''
      par_dirs.append(parent)

      # and make short names
      plen = len(parent)
      if plen > 0: start = plen+1
      else:        start = 0
      short_names.append([fname[start:] for fname in flist])

   # top is common to all parents
   top_dir = common_dir(par_dirs)
   if top_dir.count('/') <= 1: top_dir = ''

   # now get all short dir names, under top dir
   if top_dir == '': short_dirs = par_dirs
   else: short_dirs = [child_dir_name(top_dir, pdir) for pdir in par_dirs]

   return top_dir, par_dirs, short_dirs, short_names

def child_dir_name(parent, child):
   """return the child directory name truncated under the parent"""
   if parent == '' or child == '': return child
   plen = len(parent)
   clen = len(child)

   if child[0:plen] != parent: return child     # not a proper child

   # return everything after separator
   if clen < plen + 2: return '.'               # trivial as child
   else:               return child[plen+1:]    # remove parent portion

def is_trivial_dir(dname):
   """input a string
      return 1 if dname is empty or '.'
      else return 0
   """
   if dname == None: return 1
   if dname == '' or dname == '.' or dname == './' : return 1

   return 0

def flist_to_table_pieces(flist):
   """dissect a file list
      input: list of file names
      output:
        - common directory name
        - short name list (names after common directory)
        - glob string from short names
      note: short names will be new data, never just references to input
   """
   if len(flist) == 0: return '', [], ''

   ddir = common_dir(flist)
   dirlen = len(ddir)
   if dirlen > 0: snames = [dset[dirlen+1:] for dset in flist]
   else:          snames = [dset[:]         for dset in flist]

   globstr = glob_form_from_list(snames)

   return ddir, snames, globstr

def get_ids_from_dsets(dsets, prefix='', suffix='', hpad=0, tpad=0, verb=1):
   """return a list of subject IDs corresponding to the datasets

      Make sure we have afni_name objects to take the prefix from.
      If simple strings, turn into afni_names.

      Try list_minus_glob_form on the datasets.  If that fails, try
      on the directories.

      prefix, suffix: attach these to the resulting IDs
      hpad, tpad:     padding to pass to list_minus_glob_form

      return None on failure
   """
   if hpad < 0 or tpad < 0:
      print('** get_ids_from_dsets: will not apply negative padding')
      hpad, tpad = 0, 0

   if len(dsets) == 0: return None

   # be more aggressive, use dataset prefix names
   # dlist = [dset.split('/')[-1] for dset in dsets]
   if type(dsets[0]) == str: 
      nlist = [BASE.afni_name(dset) for dset in dsets]
   elif isinstance(dsets[0], BASE.afni_name):
      nlist = dsets
   else:
      print('** GIFD: invalid type for dset list, have value %s' % dsets[0])
      return None

   dlist = [dname.prefix for dname in nlist]

   # if nothing to come from file prefixes, try the complete path names
   if vals_are_constant(dlist): dlist = dsets

   slist = list_minus_glob_form(dlist, hpad, tpad)

   # do some error checking
   for val in slist:
      if '/' in val:            # no directories
         if verb > 0: print('** GIFD: IDs would have directories')
         return None

   if len(slist) != len(dsets): # appropriate number of entries
      if verb > 0: print('** GIFD: length mis-match getting IDs')
      return None

   if not vals_are_unique(slist):
      if verb > 0: print('** GIFD: final IDs are not unique')
      return None

   return slist

def insensitive_word_pattern(word):
   """replace each alphabetical char with [Ul], an upper/lower search pair
      use of 'either' suggested by G Irving at stackoverflow.com
   """
   def either(c):
      if c.isalpha: return '[%s%s]'%(c.lower(),c.upper())
      else:         return c
   return ''.join(map(either,word))
   
def insensitive_glob(pattern):
   """return glob.glob, but where every alphabetic character is
      replaced by lower/upper pair tests
   """
   return glob.glob(insensitive_word_pattern(pattern))


def search_path_dirs(word, mtype=0, casematch=1):
   """return status and list of matching files

      Could be more efficient, esp. with mtype=exact and casematch set, but
      I will strive for simplicity and consistency and see how it goes.

        mtype     : 0 = match any sub-word (i.e. look for DIR/*word*)
                    1 = exact match (i.e. no wildcard, look for DIR/word)
                    2 = prefix match (i.e. look for DIR/word*)
        casematch : flag: if set, case must match
                          else, 'word' letters can be either case
   """
   try:
      plist = os.environ['PATH'].split(':')
   except:
      print('** search_path_dirs: no PATH var')
      return 1, []

   # if no casematch, look for upper/lower pairs
   if casematch: wpat = word
   else:         wpat = insensitive_word_pattern(word)

   # if not exact, surround with wildcard pattern
   if   mtype == 0: form = '%s/*%s*'    # any sub-word
   elif mtype == 1: form = '%s/%s'      # exact match
   elif mtype == 2: form = '%s/%s*'     # prefix match
   else:
      print('** search_path_dirs: illegal mtype = %s' % mtype)
      return 1, []

   # now just search for matches
   rlist = []
   for pdir in plist:
      glist = glob.glob(form % (pdir, wpat))
      glist.sort()
      if len(glist) > 0: rlist.extend(glist)

   # make a new list based on os.path.realpath, to avoid links
   rlist = [os.path.realpath(pfile) for pfile in rlist]

   return 0, get_unique_sublist(rlist)

def num_found_in_path(word, mtype=0, casematch=1):
   """a simple wrapper to print search_path_dirs results

      Search for given 'word' in path, and print out list elements
      with element prefix of 'indent'.

        mtype     : 0 = match any sub-word (i.e. look for DIR/*word*)
                    1 = exact match (i.e. no wildcard, look for DIR/word)
                    2 = prefix match (i.e. look for DIR/word*)
        casematch : flag: if set, case must match
                          else, 'word' letters can be either case
        indent    : prefix/separator string for list elements
   """
   rv, rlist = search_path_dirs(word, mtype=mtype, casematch=casematch)
   if rv: return 0
   return len(rlist)

def show_found_in_path(word, mtype=0, casematch=1, indent='\n   '):
   """a simple wrapper to print search_path_dirs results

      Search for given 'word' in path, and print out list elements
      with element prefix of 'indent'.

        mtype     : 0 = match any sub-word (i.e. look for DIR/*word*)
                    1 = exact match (i.e. no wildcard, look for DIR/word)
                    2 = prefix match (i.e. look for DIR/word*)
        casematch : flag: if set, case must match
                          else, 'word' letters can be either case
        indent    : prefix/separator string for list elements
   """
   rv, rlist = search_path_dirs(word, mtype=mtype, casematch=casematch)
   if not rv: print(indent+indent.join(rlist))

# ----------------------------------------------------------------------
# mathematical functions:
#    vector routines: sum, sum squares, mean, demean
# ----------------------------------------------------------------------

def loc_sum(vals):
   """in case 'sum' does not exist, such as on old machines"""

   try: tot = sum(vals)
   except:
      tot = 0
      for val in vals: tot += val
   return tot

def sumsq(vals):
   """return the sum of the squared values"""
   ssq = 0
   for val in vals: ssq += (val*val)
   return ssq

def euclidean_norm(vals):
   """name is toooooo long"""

   if len(vals) < 1: return 0.0
   return math.sqrt(sumsq(vals))

def L2_norm(vals):
   return euclidean_norm(vals)

def weighted_enorm(vals, weights):

   if len(vals) < 1: return 0.0
   if len(vals) != len(weights): return 0.0

   sum = 0.0
   for ind in range(len(vals)):
      vv = vals[ind]*weights[ind]
      sum += vv*vv
   return math.sqrt(sum)

def dotprod(v1,v2):
   """compute the dot product of 2 vectors"""
   try: dsum = loc_sum([v1[i]*v2[i] for i in range(len(v1))])
   except:
      print('** cannot take dotprod() of these elements')
      dsum = 0
   return dsum

def affine_to_params_6(avec, verb=1):
   """convert rotation/shift affine "matrix" to 6 parameters
      (e.g. as in 3dvolreg 1Dmatrix format to 1Dfile format)

      matvec: length 12+ vector (row major order)
      return: length 6 param vector:
        roll, pitch, yaw, dx, dy, dz
   """

   rvec = [0.0]*6

   if len(avec) < 12:
      print('** affine_to_params_6: requires length 12+ vector, have %d' \
            % len(avec))
      return rvec

   # rotations
   rvec[0] = 180.0/math.pi * math.atan2(avec[9], avec[10])
   rvec[1] = 180.0/math.pi *-math.asin (avec[8])
   rvec[2] = 180.0/math.pi * math.atan2(avec[4], avec[0])

   # deltas
   rvec[3] = avec[3]
   rvec[4] = avec[7]
   rvec[5] = avec[11]

   return rvec

def maxabs(vals):
   """convenience function for the maximum of the absolute values"""
   if len(vals) == 0: return 0
   return max([abs(v) for v in vals])

def ndigits_lod(num, base=10):
   """return the number of digits to the left of the decimal"""
   anum = abs(num)
   if base == 10: return 1+int(math.log10(anum))
   else:          return 1+int(math.log10(anum)/math.log10(base))

# almost identical to demean, but just return the mean
def mean(vec, ibot=-1, itop=-1):
    """return the vector mean, from index ibot to index itop

        if ibot == -1, ibot will be 0
        if itop == -1, itop will be len-1"""

    if not vec: return 0.0
    if ibot > itop:
        print('** afni_util.mean: ibot (%d) > itop (%d)' % (ibot, itop))
        return 0.0

    vlen = len(vec)

    if ibot < 0: ibot = 0
    if ibot > vlen-1: ibot = vlen-1
    if itop < 0: itop = vlen-1
    if itop > vlen-1: itop = vlen-1

    tot = 0.0
    for ind in range(ibot,itop+1):
       tot += vec[ind]

    return tot/(itop-ibot+1)

# ----------------------------------------------------------------------
# vector manipulation functions
# ----------------------------------------------------------------------

# almost identical to mean, but subtract the mean instead of returning it
def demean(vec, ibot=-1, itop=-1):
    """demean the vector (in place), from index ibot to index itop

        if ibot == -1, ibot will be 0
        if itop == -1, itop will be len-1
    
       return 0 on success, 1 on error"""

    if not vec: return 0
    if ibot > itop:
        print('** afni_util.demean: ibot (%d) > itop (%d)' % (ibot, itop))
        return 1

    vlen = len(vec)

    if ibot < 0: ibot = 0
    if ibot > vlen-1: ibot = vlen-1
    if itop < 0: itop = vlen-1
    if itop > vlen-1: itop = vlen-1

    # first compute the mean
    tot = 0.0
    for ind in range(ibot,itop+1):
       tot += vec[ind]
    mm = tot/(itop-ibot+1)

    # now subract it
    for ind in range(ibot,itop+1):
       vec[ind] -= mm

    return vec

def lin_vec_sum(s1, vec1, s2, vec2):
   """return s1*[vec1] + s2*[vec2]
      note: vec2 can be None"""

   if vec2 == None:
      return [s1*vec1[i] for i in range(len(vec1))]

   l1 = len(vec1)
   l2 = len(vec2)
   if l1 != l2:
      print('** LVC: vectors have different lengths (%d, %d)' % (l1, l2))
      return []

   return [s1*vec1[i]+s2*vec2[i] for i in range(l1)]

def proj_onto_vec(v1, v2, unit_v2=0):
   """return vector v1 projected onto v2

      unit_v2: flag denoting whether v2 is a unit vector

      return <v1,v2>/<v2,v2> * v2"""

   if unit_v2: scalar = dotprod(v1,v2)
   else:
      len2 = L2_norm(v2,v2)
      if len2 == 0:
         print('** cannot project onto <0> vector')
         return []
      scalar = dotprod(v1,v2) * 1.0 / len2

   return lin_vec_sum(scalar, v2, 0, None)

def proj_out_vec(v1, v2, unit_v2=0):
   """return vector v1 with v2 projected out
      (return the component of v1 that is orthogonal to v2)

      unit_v2: flag denoting whether v2 is a unit vector

      return v1 - (v1 projected onto v2)

      Note: y - proj(y,p)
          = y - <y,p>/<p,p> * pT        = y - yTp/pTp * pT
          = y - <y,p>/|p|^2 * pT
          = y - <y,p>*(pTp)^-1 * pT     (where (pTp)^-1*pT = pseudo-inverse)
          = (I - p (pTp)^-1 * pT) * y
   """

   return lin_vec_sum(1, v1, -1, proj_onto_vec(v1, v2, unit_v2))

# ----------------------------------------------------------------------
# statistical routines - stdev, variance, ttest
# ----------------------------------------------------------------------

def min_mean_max_stdev(data):
    """return 4 values for data: min, mean, max, stdev (unbiased)"""

    if not data: return 0,0,0,0
    length = len(data)
    if length <  1: return 0,0,0,0

    if type(data[0]) == str:
       try: dd = [float(val) for val in data]
       except:
          print('** bad data for min_mean_max_stdev')
          return 0, 0, 0, 0
    else: dd = data
    if length == 1: return dd[0], dd[0], dd[0], 0.0

    minval  = min(dd)
    maxval  = max(dd)
    meanval = loc_sum(dd)/float(length)

    return minval, meanval, maxval, stdev_ub(dd)

def interval_offsets(times, dur):
    """given a list of times and an interval duration (e.g. TR), return
       the offsets into respective intervals"""

    if not times or dur <= 0:
        print("** interval offsets: bad dur (%s) or times: %s" % (dur, times))
        return []

    length = len(times)
    if length <  1: return []

    fdur = float(dur)   # to make sure (e.g. avoid int division)

    try: offlist = [val % fdur for val in times]
    except:
        print("** interval offsets 2: bad dur (%s) or times: %s" % (dur, times))
        return []
   
    return offlist

def fractional_offsets(times, dur):
    """given a list of times and an interval duration (e.g. TR), return
       the fractional offsets into respective intervals

       i.e. similar to interval offsets, but times are divided by dur"""

    # rely on i_o for error checking
    olist = interval_offsets(times, dur)
    if len(olist) < 1 or dur <= 0: return []

    dur = float(dur)
    for ind, val in enumerate(olist):
        olist[ind] = val/dur

    return olist

def stdev_ub(data):
    """unbiased standard deviation (divide by len-1, not just len)
              stdev_ub = sqrt( (sumsq - N*mean^2)/(N-1) )
    """

    length = len(data)
    if length <  2: return 0.0

    meanval = loc_sum(data)/float(length)
    # compute standard deviation
    ssq = 0.0
    for val in data: ssq += val*val
    val = (ssq - length*meanval*meanval)/(length-1.0)

    # watch for truncation artifact
    if val < 0.0 : return 0.0
    return math.sqrt(val)

def stdev(data):
    """(biased) standard deviation (divide by len, not len-1)"""

    length = len(data)
    if length <  2: return 0.0

    meanval = loc_sum(data)/float(length)
    # compute standard deviation
    ssq = 0.0
    for val in data: ssq += val*val
    val = (ssq - length*meanval*meanval)/length

    # watch for truncation artifact
    if val < 0.0 : return 0.0
    return math.sqrt(val)

def variance_ub(data):
    """unbiased variance (divide by len-1, not just len)"""

    length = len(data)
    if length <  2: return 0.0

    meanval = loc_sum(data)/float(length)
    # compute standard deviation
    ssq = 0.0
    for val in data: ssq += val*val
    val = (ssq - length*meanval*meanval)/(length-1.0)

    # watch for truncation artifact
    if val < 0.0 : return 0.0
    return val

def variance(data):
    """(biased) variance (divide by len, not len-1)"""

    length = len(data)
    if length <  2: return 0.0

    meanval = loc_sum(data)/float(length)
    # compute standard deviation
    ssq = 0.0
    for val in data: ssq += val*val
    val = (ssq - length*meanval*meanval)/length

    # watch for truncation artifact
    if val < 0.0 : return 0.0
    return val

def covary(x, y):
    """return the raw covariance:
       sum[(xi - mean_x)*(yi - mean_y)] / (N-1)
    """

    ll = len(x)
    mx = mean(x)
    my = mean(y)

    vv = loc_sum([(x[i]-mx)*(y[i]-my) for i in range(ll)])

    if ll > 1: return 1.0 * vv / (ll-1.0)
    else:      return 0.0

def r(vA, vB, unbiased=0):
    """return Pearson's correlation coefficient

       for demeaned and unit vectors, r = dot product
       for unbiased correlation, return r*(1 + (1-r^2)/2N)

       note: correlation_p should be faster
    """
    la = len(vA)

    if len(vB) != la:
        print('** r (correlation): vectors have different lengths')
        return 0.0
    ma = mean(vA)
    mb = mean(vB)
    dA = [v-ma for v in vA]
    dB = [v-mb for v in vB]
    sA = L2_norm(dA) # is float
    sB = L2_norm(dB)
    dA = [v/sA for v in dA]
    dB = [v/sB for v in dB]

    r = dotprod(dA,dB)

    if unbiased: return r * (1.0 + (1-r*r)/(2.0*la))
    return r

def linear_fit(vy, vx=None):
   """return slope and intercept for linear fit to data

      if vx is not provided (i.e. no scatterplot fit), then return
      fit to straight line (i.e. apply as vx = 1..N, demeaned)

      slope = N*sum(xy) - (sum(x)*sum(y)]
              ---------------------------
              N*sum(x*x) - (sum(x))^2

      inter = 1/N * (sum(y) - slope*sum(x))

      note: we could compute slope = covary(x,y)/covary(x,x)
   """

   N = len(vy)
   mn = (N-1.0)/2

   # maybe use demeaned, slope 1 line
   if vx == None: vx = [i-mn for i in range(N)]
   else:
      if len(vx) != N:
         print('** cannot fit vectors of different lengths')
         return 0.0, 0.0

   sx   = loc_sum(vx)
   sy   = loc_sum(vy)
   ssx  = dotprod(vx, vx)
   ssxy = dotprod(vy, vx)

   slope = (1.0 * N * ssxy - sx * sy) / (N * ssx - sx*sx )
   inter = 1.0 * (sy - slope * sx) / N

   return slope, inter


def eta2(vA, vB):
    """return eta^2 (eta squared - Cohen, NeuroImage 2008

                        SUM[ (a_i - m_i)^2 + (b_i - m_i)^2 ]
         eta^2 =  1  -  ------------------------------------
                        SUM[ (a_i - M  )^2 + (b_i - M  )^2 ]

         where  a_i and b_i are the vector elements
                m_i = (a_i + b_i)/2
                M = mean across both vectors

    """

    length = len(vA)
    if len(vB) != length:
        print('** eta2: vectors have different lengths')
        return 0.0
    if length < 1: return 0.0

    ma = mean(vA)
    mb = mean(vB)
    gm = 0.5*(ma+mb)

    vmean = [(vA[i]+vB[i])*0.5 for i in range(length)]

    da = [vA[i] - vmean[i] for i in range(length)]
    db = [vB[i] - vmean[i] for i in range(length)]
    num = sumsq(da) + sumsq(db)

    da = [vA[i] - gm       for i in range(length)]
    db = [vB[i] - gm       for i in range(length)]
    denom = sumsq(da) + sumsq(db)

    if num < 0.0 or denom <= 0.0 or num >= denom:
        print('** bad eta2: num = %s, denom = %s' % (num, denom))
        return 0.0
    return 1.0 - float(num)/denom

def correlation_p(vA, vB, demean=1, unbiased=0):
    """return the Pearson correlation between the 2 vectors
       (allow no demean for speed)
    """

    la = len(vA)
    if len(vB) != la:
        print('** correlation_pearson: vectors have different lengths')
        return 0.0

    if la < 2: return 0.0

    if demean:
       ma = mean(vA)
       mb = mean(vB)
       dA = [v-ma for v in vA]
       dB = [v-mb for v in vB]
    else:
       dA = vA
       dB = vB

    sAB = dotprod(dA, dB)
    ssA = sumsq(dA)
    ssB = sumsq(dB)

    if demean: del(dA); del(dB)

    if ssA <= 0.0 or ssB <= 0.0: return 0.0
    else:
       r = sAB/math.sqrt(ssA*ssB)
       if unbiased: return r * (1 + (1-r*r)/(2*la))
       return r

def ttest(data0, data1=None):
    """just a top-level function"""

    if data1: return ttest_2sam(data0, data1)
    return ttest_1sam(data0)

def ttest_1sam(data, m0=0.0):
    """return (mean-m0) / (stdev_ub(data)/sqrt(N)),

              where stdev_ub = sqrt( (sumsq - N*mean^2)/(N-1) )

       or faster, return: (sum-N*m0)/(sqrt(N)*stdev_ub)

       note: move 1/N factor to denominator
    """

    # check for short length
    N = len(data)
    if N < 2: return 0.0

    # check for division by 0
    sd = stdev_ub(data)
    if sd <= 0.0: return 0.0

    # and return, based on any passed expected mean
    if m0: t = (loc_sum(data) - N*m0)/(math.sqrt(N)*sd)
    else:  t =  loc_sum(data)        /(math.sqrt(N)*sd)

    return t

def ttest_paired(data0, data1):
    """easy: return 1 sample t-test of the difference"""

    N0 = len(data0)
    N1 = len(data1)
    if N0 < 2 or N1 < 2: return 0.0
    if N0 != N1:
        print('** ttest_paired: unequal vector lengths')
        return 0.0

    return ttest_1sam([data1[i] - data0[i] for i in range(N0)])

def ttest_2sam(data0, data1, pooled=1):
    """if not pooled, return ttest_2sam_unpooled(), otherwise

       return (mean1-mean0)/sqrt(PV * (1/N0 + 1/N1))

              where PV (pooled_variance) = ((N0-1)*V0 + (N1-1)*V1)/(N0+N1-2)

       note: these lists do not need to be of the same length
       note: the sign is as with 3dttest (second value(s) minus first)
    """

    if not pooled: return ttest_2sam_unpooled(data0, data1)

    N0 = len(data0)
    N1 = len(data1)
    if N0 < 2 or N1 < 2: return 0.0

    m0 = loc_sum(data0)/float(N0)
    v0 = variance_ub(data0)

    m1 = loc_sum(data1)/float(N1)
    v1 = variance_ub(data1)

    pv = ((N0-1)*v0 + (N1-1)*v1) / (N0+N1-2.0)
    if pv <= 0.0: return 0.0

    return (m1-m0)/math.sqrt(pv * (1.0/N0 + 1.0/N1))

def ttest_2sam_unpooled(data0, data1):
    """return (mean1-mean0)/sqrt(var0/N0 + var1/N1)

       note: these lists do not need to be of the same length
       note: the sign is as with 3dttest (second value(s) minus first)
    """

    N0 = len(data0)
    N1 = len(data1)
    if N0 < 2 or N1 < 2: return 0.0

    m0 = loc_sum(data0)/float(N0)
    v0 = variance_ub(data0)

    m1 = loc_sum(data1)/float(N1)
    v1 = variance_ub(data1)

    if v0 <= 0.0 or v1 <= 0.0: return 0.0

    return (m1-m0)/math.sqrt(v0/N0 + v1/N1)


def p2q(plist, do_min=1, verb=1):
    """convert list of p-value to a list of q-value, where
         q_i = minimum (for m >= i) of N * p_m / m
       if do min is not set, simply compute q-i = N*p_i/i

       return q-values in increasing significance
              (i.e. as p goes large to small, or gets more significant)
    """

    q = plist[:]
    q.sort()
    N = len(q)

    # work from index N down to 0 (so index using i-1)
    min = 1
    for i in range(N,0,-1):
       ind = i-1
       q[ind] = N * q[ind] / i
       if do_min:
          if q[ind] < min: min = q[ind]
          if min < q[ind]: q[ind] = min

    # and flip results
    q.reverse()

    return q

def argmax(vlist, absval=0):
   if len(vlist) < 2: return 0
   if absval: vcopy = [abs(val) for val in vlist]
   else:      vcopy = vlist

   mval = vcopy[0]
   mind = 0
   for ind, val in enumerate(vlist):
      if val > mval:
         mval = val
         mind = ind

   return mind

def argmin(vlist, absval=0):
   if len(vlist) < 2: return 0
   if absval: vcopy = [abs(val) for val in vlist]
   else:      vcopy = vlist

   mval = vcopy[0]
   mind = 0
   for ind, val in enumerate(vlist):
      if val < mval:
         mval = val
         mind = ind

   return mind

# ----------------------------------------------------------------------
# random list routines: shuffle, merge, swap, extreme checking
# ----------------------------------------------------------------------

def swap_2(vlist, i1, i2):
    if i1 != i2:
       val = vlist[i2]
       vlist[i2] = vlist[i1]
       vlist[i1] = val

def shuffle(vlist, start=0, end=-1):
    """randomize the order of list elements, where each permutation is
       equally likely

       - akin to RSFgen, but do it with equal probabilities
         (search for swap in [index,N-1], not in [0,N-1])
       - random.shuffle() cannot produce all possibilities, don't use it
       - start and end are indices to work with
    """

    # if we need random elsewhere, maybe do it globally
    import random

    vlen = len(vlist)

    # check bounds
    if start < 0 or start >= vlen: return
    if end >= 0 and end <= start:  return

    # so start is valid and end is either < 0 or big enough
    if end < 0 or end >= vlen: end = vlen-1

    nvals = end-start+1

    # for each index, swap with random other towards end
    for index in range(nvals-1):
        rind = int((nvals-index)*random.random())
        swap_2(vlist, start+index, start+index+rind)
        continue

    # return list reference, though usually ignored
    return vlist

def shuffle_blocks(vlist, bsize=-1):
    """like shuffle, but in blocks of given size"""

    vlen = len(vlist)

    if bsize < 0 or bsize >= vlen:
       shuffle(vlist)
       return

    if bsize < 2: return

    nblocks = vlen // bsize
    nrem    = vlen  % bsize

    boff = 0
    for ind in range(nblocks):
       shuffle(vlist, boff, boff+bsize-1)
       boff += bsize
    shuffle(vlist, boff, boff+nrem-1)
        
    return vlist

def random_merge(list1, list2):
    """randomly merge 2 lists (so each list stays in order)

       shuffle a list of 0s and 1s and then fill from lists
    """

    # if we need random elsewhere, maybe do it globally
    import random

    mlist = [0 for i in range(len(list1))] + [1 for i in range(len(list2))]
    shuffle(mlist)

    i1, i2 = 0, 0
    for ind in range(len(mlist)):
        if mlist[ind] == 0:
            mlist[ind] = list1[i1]
            i1 += 1
        else:
            mlist[ind] = list2[i2]
            i2 += 1

    return mlist

def show_sum_pswr(nT, nR):
    cp = 0.0
    prev = 0
    for r in range(nR+1):
       # already float, but be clear
       p = float(prob_start_with_R(nT,nR,r))
       cp += p
       # print 'prob at %3d = %g (cum %g)' % (r, p, cp)
       if prev == 0: prev = p
       print(p, p/prev)
       prev = p
    print('cum result is %g' % cp)


def prob_start_with_R(nA, nB, nS):
    """return the probability of starting nS (out of nB) class B elements
       should equal: choose(nB, nS)*nS! * nA *(nB+nA-nS-1)! / (nA+nB)!
       or: factorial(nB, init=nB-nS+1) * nA / fact(nA+nB, init=nA+nB-nS)

       or: choose(nB,nS)/choose(nA+nB,nS) * nA/(nA+nB-nS)
       
    """
    return 1.0 * nA * factorial(nB,    init=nB-nS+1) \
                    / factorial(nA+nB, init=nA+nB-nS)

def choose(n,m):
    """return n choose m = n! / (m! * (n-m)!)"""
    # integral division (or use floats, to better handle overflow)
    return factorial(n,init=n-m+1) / factorial(m)

def factorial(n, init=1):
    prod = 1
    val = init
    while val <= n:
       prod *= val
       val += 1
    return prod

def swap2(data):
    """swap data elements in pairs"""
    
    size  = 2
    nsets = len(data)//size
    if nsets <= 0: return

    for ind in range(nsets):
        off = ind*size
        v           = data[off]     # swap d[0] and d[1]
        data[off]   = data[off+1]
        data[off+1] = v

def swap4(data):
    """swap data elements in groups of 4"""
    
    size  = 4
    nsets = len(data)//size
    if nsets <= 0: return

    for ind in range(nsets):
        off = ind*size
        v           = data[off]     # swap d[0] and d[3]
        data[off]   = data[off+3]
        data[off+3] = v
        v           = data[off+1]   # swap d[1] and d[2]
        data[off+1] = data[off+2]
        data[off+2] = v

def vec_extremes(vec, minv, maxv, inclusive=0):
   """return a integer array where values outside bounds are 1, else 0

      if inclusive, values will also be set if they equal a bound

      return error code, new list
             success: 0, list
             error  : 1, None"""

   if not vec: return 1, None

   if minv > maxv:
      print('** extremes: minv > maxv (', minv, maxv, ')') 
      return 1, None

   if inclusive:
      elist = [1*(vec[t]>=maxv or vec[t]<=minv) for t in range(len(vec))]
   else:
      elist = [1*(vec[t]> maxv or vec[t]< minv) for t in range(len(vec))]

   return 0, elist

def vec_moderates(vec, minv, maxv, inclusive=1):
   """return a integer array where values inside bounds are 1, else 0

      if inclusive, values will also be set if they equal a bound

      return error code, new list
             success: 0, list
             error  : 1, None"""

   if not vec: return 1, None

   if minv > maxv:
      print('** moderates: minv > maxv (', minv, maxv, ')') 
      return 1, None

   if inclusive:
      elist = [1*(vec[t]>=minv and vec[t]<=maxv) for t in range(len(vec))]
   else:
      elist = [1*(vec[t]> minv and vec[t]< maxv) for t in range(len(vec))]

   return 0, elist

def vec_range_limit(vec, minv, maxv):
   """restrict the values to [minv, maxv]

      This function modifies the past vector.

      return 0 on success, 1 on error"""

   if not vec: return 0

   if minv > maxv:
      print('** range_limit: minv > maxv (', minv, maxv, ')')
      return 1

   for ind in range(len(vec)):
      if   vec[ind] < minv: vec[ind] = minv
      elif vec[ind] > maxv: vec[ind] = maxv

   return 0

# for now, make 2 vectors and return their correlation
def test_polort_const(ntrs, nruns, verb=1):
    """compute the correlation between baseline regressors of length ntrs*nruns
       - make vectors of 11...10000...0 and 00...011...100..0 that are as the
         constant polort terms of the first 2 runs
       - return their correlation

       - note that the correlation is easily provable as -1/(N-1) for N runs
    """

    if ntrs <= 0 or nruns <= 2: return -1  # flag

    # lazy way to make vectors
    v0 = [1] * ntrs + [0] * ntrs + [0] * (ntrs * (nruns-2))
    v1 = [0] * ntrs + [1] * ntrs + [0] * (ntrs * (nruns-2))

    if verb > 1:
        print('++ test_polort_const, vectors are:\n' \
              '   v0 : %s \n'                        \
              '   v1 : %s' % (v0, v1))

    return correlation_p(v0, v1)

# for now, make 2 vectors and return their correlation
def test_tent_vecs(val, freq, length):
    a = []
    b = []
    for i in range(length):
        if (i%freq) == 0:
            a.append(val)
            b.append(1-val)
        elif ((i-1)%freq) == 0:
            a.append(0.0)
            b.append(val)
        else:
            a.append(0.0)
            b.append(0.0)

    return correlation_p(a,b)

_g_main_help = """
afni_util.py: not really intended as a main program

   However, there is some functionality for devious purposes...

   options:

      -help             : show this help

      -eval STRING      : evaluate STRING in the context of afni_util.py
                          (i.e. STRING can be function calls or other)

         This option is used to simply execute the code in STRING.

         Examples for eval:

            afni_util.py -eval "show_process_stack()"
            afni_util.py -eval "show_process_stack(verb=2)"
            afni_util.py -eval "show_process_stack(pid=1000)"


      -exec STRING      : execute STRING in the context of afni_util.py

         This option is used to simply execute the code in STRING.

         Examples for exec:

            afni_util.py -exec "y = 3+4 ; print y"
            afni_util.py -exec "import PyQt4"
            afni_util.py -exec "show_process_stack()"

      -funchelp FUNC    : print the help for afni_util.py function FUNC

         Pring the FUNC.__doc__ text, if any.

         Example:

            afni_util.py -funchelp wrap_file_text

      -print STRING     : print the result of executing STRING

         Akin to -eval, but print the results of evaluating STRING.

         Examples for print:

            afni_util.py -print "get_last_history_ver_pack('DSET+tlrc')"
            afni_util.py -print "get_last_history_version('DSET+tlrc')"

      -lprint STRING    : line print: print result list, one element per line

         The 'l' stands for 'line' (or 'list').  This is akin to -print,
         but prints a list with one element per line.

      -listfunc [SUB_OPTS] FUNC LIST ... : execute FUNC(LIST)

         With this option, LIST is a list of values to be passed to FUNC().
         Note that LIST can be simply '-' or 'stdin', in which case the
         list values are read from stdin.

         This is similar to eval, but instead of requiring:
            -eval "FUNC([v1,v2,v3,...])"
         the list values can be left as trailing arguments:
            -listfunc FUNC v1 v2 v3 ...
         (where LIST = v1 v2 v3 ...).

         SUB_OPTS sub-options:

                -float  : convert the list to floats before passing to FUNC()
                -print  : print the result
                -join   : print the results join()'d together

         Examples for listfunc:

            afni_util.py -listfunc        min_mean_max_stdev 1 2 3 4 5
            afni_util.py -listfunc -print min_mean_max_stdev 1 2 3 4 5
            afni_util.py -listfunc -join  min_mean_max_stdev 1 2 3 4 5

            afni_util.py -listfunc -join -float demean 1 2 3 4 5

            afni_util.py -listfunc -join shuffle `count -digits 4 1 124`
            count -digits 4 1 124 | afni_util.py -listfunc -join shuffle -
            afni_util.py -listfunc glob2stdout 'EPI_run1/8*'

            afni_util.py -listfunc -joinc list_minus_glob_form *HEAD

            afni_util.py -listfunc -join -float linear_fit 2 3 5 4 8 5 8 9


         Also, if LIST contains -list2, then 2 lists can be input to do
         something like:
            -eval "FUNC([v1,v2,v3], [v4,v5,v6])"

         Examples with -list2:

            afni_util.py -listfunc -print -float ttest 1 2 3 4 5 \\
                                                -list2 2 2 4 6 8

            afni_util.py -listfunc -print -float ttest_paired   \\
                          1 2 3 4 5 -list2 2 4 5 6 8

            afni_util.py -listfunc -join -float linear_fit      \\
                         `cat y.1D` -list2 `cat x.1D`

"""

def process_listfunc(argv):
   """see the -help description"""

   if argv[1] != '-listfunc': return 1

   if len(argv) <= 3 :
      print('** -listfunc usage requires at least 3 args')
      return 1

   do_join = 0
   do_joinc = 0 # join with commas
   do_float = 0
   do_print = 0
   argbase = 2

   while argv[argbase] in ['-join', '-joinc', '-print', '-float']:
      if argv[argbase] == '-join':
         do_join = 1
         argbase += 1
      elif argv[argbase] == '-joinc':
         do_joinc = 1
         argbase += 1
      elif argv[argbase] == '-print':
         do_print = 1
         argbase += 1
      elif argv[argbase] == '-float':
         do_float = 1
         argbase += 1
      else: break # should not happen

   # note function
   func = eval(argv[argbase])

   # get args, and check for -list2
   # (allow for - to read all data into array)
   args1 = argv[argbase+1:]
   if len(args1) == 1:
      if args1[0] == '-' or args1[0] == 'stdin':
         fvals = read_text_file()
         args1 = []
         for fv in fvals: args1.extend(fv.split())

   args2 = []
   if '-list2' in args1:
      l2ind = args1.index('-list2')
      args2 = args1[l2ind+1:]
      args1 = args1[0:l2ind]

   if do_float:
      try: vals1 = [float(v) for v in args1]
      except:
         print('** list1 is not all float')
         return 1
      try: vals2 = [float(v) for v in args2]
      except:
         print('** list2 is not all float')
         return 1
   else:
      vals1 = args1
      vals2 = args2

   if len(vals2) > 0: ret = func(vals1, vals2)
   else:              ret = func(vals1)
   
   if   do_join:  print(' '.join(str(v) for v in ret))
   elif do_joinc: print(','.join(str(v) for v in ret))
   elif do_print: print(ret)
   # else do nothing special
   return 0

def show_function_help(flist):
   for func in flist:
      print(section_divider('help for: %s' % func))
      try:
         fn = eval(func)
         print(fn.__doc__)
      except:
         print("** not a valid function '%s'" % func)

def main():
   argv = sys.argv
   if '-help' in argv:
      print(_g_main_help)
      return 0
   if len(argv) > 2:
      if argv[1] == '-eval':
         eval(' '.join(argv[2:]))
         return 0
      elif argv[1] == '-exec':
         exec(' '.join(argv[2:]))
         return 0
      elif argv[1] == '-funchelp':
         show_function_help(argv[2:])
         return 0
      elif argv[1] == '-lprint':
         ret = eval(' '.join(argv[2:]))
         print('\n'.join(['%s'%rent for rent in ret]))
         return 0
      elif argv[1] == '-print':
         print(eval(' '.join(argv[2:])))
         return 0
      elif argv[1] == '-listfunc':
         return process_listfunc(argv)

   print('afni_util.py: not intended as a main program')
   return 1

if __name__ == '__main__':
   sys.exit(main())

