#!/usr/bin/env python

# Timing library, accessed via timing_tool.py.
#
# Offers classes AfniTiming and AfniMarriedTiming.
#
# R Reynolds    December, 2008

import sys
import module_test_lib
g_testlibs = ['sys', 'math', 'copy']
if module_test_lib.num_import_failures(g_testlibs): sys.exit(1)
   
# import libraries
import math
import copy
import afni_util as UTIL

# ----------------------------------------------------------------------
# two timing classes: AfniTiming and AfniMarriedTiming
# ----------------------------------------------------------------------

class AfniTiming:
   def __init__(self, filename="", dur=0, from_a1d=0, a1d=None, verb=1):
      """AFNI stimulus timing class"""

      # main variables
      self.data    = []           # actual data (python list of floats)
      self.dur     = dur          # optional stimulus duration
      self.fname   = ''           # name of timing file
      self.ready   = 0            # have a matrix

      self.nrows   = 0

      self.verb    = verb

      # initialize...
      if filename: self.init_from_1D(filename)

   def add_rows(self, brows):
      """add brows.nrows of data (additional runs), return 0 on success"""
      if not self.ready or not brows.ready:
         print '** Timing elements not ready for adding rows (%d,%d)' % \
               (self.ready, brows.ready)
         return 1

      if self.verb > 1: print '++ Timing: adding %d rows' % brows.nrows

      self.data.extend(brows.data)
      self.nrows += brows.nrows

      return 0

   def make_single_row_string(self, row=-1, nplaces=3, flag_empty=0):
      """return a string of row data, to the given number of decimal places
         if row is non-negative, return a string for the given row"""
      if not self.ready: return ''
      if row < 0 or row >= self.nrows:
         if self.verb > 0: print '** row %d out of range for printing' % row
         return ''

      data = self.data[row]
      rstr = ''
      if self.verb > 2 and not flag_empty: rstr += 'run %02d : ' % (row+1)

      # if flagging an empty run, use '*' characters
      if len(data) == 0 and flag_empty:
         if row == 0: rstr += '* *'
         else:        rstr += '*'

      for val in data:
         rstr += '%.*f ' % (nplaces, val)

      return rstr + '\n'

   def make_data_string(self, row=-1, nplaces=3, flag_empty=0, mesg=''):
      """return a string of row data, to the given number of decimal places
         if row is non-negative, return a string for the given row, else
         return a string of all rows"""

      if row >=0:
         return self.make_single_row_string(row, nplaces, flag_empty)

      # make it for all rows
      if len(mesg) > 0: rstr = "%s :\n" % mesg
      else:             rstr = ''
      for ind in range(self.nrows):
         rstr += self.make_single_row_string(ind, nplaces, flag_empty)

      return rstr

   def write_times(self, fname, nplaces=3):
      """write the current timing out, with nplaces right of the decimal"""
      if not self.ready:
         print '** Timing: not ready to write'
         return 1

      fp = open(fname, 'w')
      if not fp:
         print "** failed to open '%s' for writing timing" % fname
         return 1

      if self.verb > 0:
         print "++ writing %d timing rows to %s" % (self.nrows, fname)

      fp.write(self.make_data_string(nplaces=nplaces, flag_empty=1))
      fp.close()

      return 0

   def extend_rows(self, brows):
      """extend each row by the corresponding row of brows"""
      if not self.ready or not brows.ready:
         print '** Timing elements not ready for extending rows (%d,%d)' % \
               (self.ready, brows.ready)
         return 1

      if self.nrows != brows.nrows:
         print '** Timing nrows differ for extending (%d, %d)' % \
               (self.nrows,brows.nrows)
         return 1

      if self.verb > 1: print '++ Timing: extending %d rows' % self.nrows

      for ind in range(self.nrows):
         self.data[ind].extend(brows.data[ind])

      return 0

   def sort(self, rev=0):
      """sort each row (optionally reverse order)"""
      if not self.ready: return 1

      if self.verb > 1: print '-- sorting ATiming ...'

      for row in self.data:
         if rev: row.sort(reverse=True)
         else:   row.sort()

   def is_rect(self):
      if not self.ready: return 0

      nrows = len(self.data)
      if nrows == 0: return 0

      rlen = len(self.data[0])
      for row in self.data:
         if len(row) != rlen: return 0

      return 1

   def transpose(self):
      """the tranpose operation requires rectangular data"""
      if not self.ready: return 1
      if not self.is_rect():
         print '** cannot take transpose, data is not rectangular'
         return 1

      if self.verb > 1: print '-- Timing: taking transpose...'

      newdata = []
      for col in range(len(self.data[0])):
         newdata.append([self.data[row][col] for row in range(self.nrows)])

      del(self.data)
      self.data = newdata
      self.nrows = len(self.data)

      return 0

   def add_val(self, val):
      """add the given value to each element"""
      if not self.ready: return 1

      if type(val) == type('hi'):
         try: val = float(val)
         except:
            print "** invalid value to add to timing: '%s'" % val
            return 1

      if self.verb > 1: print '-- Timing: adding %f to data...' % val

      for row in self.data:
         for ind in range(len(row)):
            row[ind] += val

      return 0

   def copy(self):
      """return a complete (deep)copy of the current AfniTiming"""
      return copy.deepcopy(self)

   def show(self, mesg=''):
      print self.make_show_str(mesg)

   def make_show_str(self, mesg=''):
      if not self.ready: return "++ data     : <unset>"

      rect = self.is_rect()
      rstr = ''
      if rect: rstr = ' (row length %d)' % len(self.data[0])

      if len(mesg) > 0: umesg = '%s : ' % mesg
      else:             umesg = ''

      mstr = "----------- %stiming element ------------\n"   \
             "   fname    : %s\n" \
             "   ready    : %d\n" \
             "   nrows    : %d\n" \
             "   dur      : %f\n" \
             "   rect     : %d%s\n" \
             "   verb     : %d\n" % \
             (umesg, self.fname, self.ready, self.nrows, self.dur, rect, rstr,
              self.verb)

      return mstr

   def init_from_1D(self, fname):
      """initialize AfniTiming from a 1D file"""
      self.fname  = fname
      data, clines = read_timing_file(fname)
      if not data: return
      self.data  = data
      self.nrows = len(data)
      if self.nrows > 1:
         length = len(data[0])
         for ary in data:
            if len(ary) != length:
               break
      self.ready = 1

      if self.verb > 1:
         print '++ initialized timing from file %s' % fname
         if self.verb > 3: self.show()

def read_timing_file(fname):
   """read 1D file, returning the data in a matrix, and comments in clines"""
   try: fp = open(fname, 'r')
   except:
      print "** failed to open 1D file '%s'" % fname
      return None, None

   data = []         # data lines
   clines = []       # comment lines

   lind = 0
   for line in fp.readlines():
      lind += 1
      lary = line.split()
      if len(lary) == 0: continue
      if lary[0] == '#':
         clines.append(line)
         continue

      # so this should be data
      # rcr - consider looking for a:b or a*b formats

      # check for '*'
      #if lary[0] == '*':
      #   data.append([])
      #   continue

      try: data.append([float(x) for x in lary if x != '*'])
      except:
         print "** failed to convert line to floats: %s" % line
         return None, None

   return data, clines

# AfniMarriedTiming class - for stim times that are married with
#                           either time intervals or magnitudes
MTYPE_INT = 1   # interval
MTYPE_MAG = 2   # magnitude

class AfniMarriedTiming:
   def __init__(self, filename="", from_at=0, at=None, verb=1):
      """AFNI married stimulus timing class"""

      # main variables
      self.data    = None         # actual data (nruns x nstim x 2)
      self.fname   = ''           # name of timing file
      self.ready   = 0            # have a matrix

      self.nrows   = 0
      self.mtype   = 0            # MTYPE_INT or MTYPE_MAG
      self.int_len = -1           # if MTYPE_INT, length of equal intervals

      self.verb    = verb

      # initialize...
      if from_at: self.init_from_at(at)
      # todo: elif filename: self.init_from_1D(filename)

   def init_from_at(self, timing):
      """initialize from AfniTiming element (implies MTYPE_INT)"""
      if not timing.ready:
         print '** initializing from unready AfniTiming'
         return 1

      self.fname = timing.fname
      self.nrows = timing.nrows
      self.verb  = timing.verb

      self.mtype = MTYPE_INT

      self.data = []
      for row in timing.data:
         self.data.append([[val,val+timing.dur] for val in row])

      self.int_len = timing.dur

      self.ready = 1

      if self.verb > 1:
         print "++ init MarriedTiming from '%s' Timing" % timing.fname
      if self.verb > 3:
         self.show()

   def extend_rows(self, brows):
      """extend each row by the corresponding row of brows"""
      if not self.ready or not brows.ready:
         print '** MTiming elements not ready for extending rows (%d,%d)' % \
               (self.ready, brows.ready)
         return 1

      if self.nrows != brows.nrows:
         print '** MTiming nrows differ for extending (%d, %d)' % \
               (self.nrows,brows.nrows)
         return 1

      if self.mtype != brows.mtype:
         print '** MTiming elements differ in mtype (%d, %d)' % \
               (self.mtype,brows.mtype)
         return 1

      if self.verb > 1: print '++ MTiming: extending %d rows' % self.nrows

      for ind in range(self.nrows):
         self.data[ind].extend(brows.data[ind])

      # if interval lengths are equal, keep it, else clear it
      if self.int_len != brows.int_len: self.int_len = -1

      return 0

   def copy(self):
      """return a complete (deep)copy of the current AfniMarriedTiming"""
      return copy.deepcopy(self)

   def sort(self, rev=0):
      """sort each row (optionally reverse order)"""
      if not self.ready: return 1

      if self.verb > 1: print '-- sorting AMTiming ...'

      for row in self.data:
         if rev: row.sort(reverse=True)
         else:   row.sort()

   def is_rect(self):
      if not self.ready: return 0

      nrows = len(self.data)
      if nrows == 0: return 0

      rlen = len(self.data[0])
      for row in self.data:
         if len(row) != rlen: return 0

      return 1

   def show(self, mesg=''):
      print self.make_show_str(mesg)

   def make_show_str(self, mesg=''):
      if not self.ready: return "++ data     : <unset>"

      rect = self.is_rect()
      rstr = ''
      if rect: rstr = ' (row length %d)' % len(self.data[0])

      if len(mesg) > 0: umesg = '%s : ' % mesg
      else:             umesg = ''

      mstr = "----------- %smarried timing element ------------\n"   \
             "   fname    : %s\n" \
             "   ready    : %d\n" \
             "   nrows    : %d\n" \
             "   rect     : %d%s\n" \
             "   mtype    : %d\n" \
             "   int_len  : %s\n" \
             "   verb     : %d\n" % \
             (umesg, self.fname, self.ready, self.nrows, rect, rstr,
              self.mtype, self.int_len, self.verb)

      return mstr

   def make_single_row_string(self, row=-1, nplaces=3, flag_empty=0, 
                              check_simple=1):
      """return a string of row data, to the given number of decimal places
         if row is non-negative, return a string for the given row"""
      if not self.ready: return ''
      if row < 0 or row >= self.nrows:
         if self.verb > 0: print '** row %d out of range for printing' % row
         return ''

      if check_simple and self.int_len == 0.0: simple = 1
      else:                                    simple = 0

      data = self.data[row]
      rstr = ''
      if self.verb > 2 and not flag_empty: rstr += 'run %02d : ' % (row+1)

      # if flagging an empty run, use '*' characters
      if len(data) == 0 and flag_empty:
         if row == 0: rstr += '* *'
         else:        rstr += '*'

      if self.mtype == MTYPE_MAG: mch = '*'
      if self.mtype == MTYPE_INT: mch = ':'

      for val in data:
         if simple: rstr += '%.*f ' % (nplaces, val[0])
         else: rstr += '%.*f%s%.*f ' % (nplaces, val[0], mch, nplaces, val[1])

      return rstr + '\n'

   def make_data_string(self, row=-1, nplaces=3, flag_empty=0, check_simple=1,
                        mesg=''):
      """return a string of row data, to the given number of decimal places
         if row is non-negative, return a string for the given row, else
         return a string of all rows
            row          : make a string for just the single row
            nplaces      : number of decimal places to show
            flag_empty   : if empty row, use the '*' format
            check_simple : if set and int_len=0, use timing format
            mesg         : display the message before data
      """

      # init return string based on message
      if len(mesg) > 0: rstr = "%s :\n" % mesg
      else:             rstr = ''

      if row >=0:
         return rstr+self.make_single_row_string(row, nplaces, flag_empty,
                                                 check_simple)

      # make it for all rows
      for ind in range(self.nrows):
         rstr += self.make_single_row_string(ind, nplaces, flag_empty,
                                             check_simple)

      return rstr

   def write_times(self, fname, nplaces=3):
      """write the current M timing out, with nplaces right of the decimal"""
      if not self.ready:
         print '** M Timing: not ready to write'
         return 1

      fp = open(fname, 'w')
      if not fp:
         print "** failed to open '%s' for writing Mtiming" % fname
         return 1

      if self.verb > 0:
         print "++ writing %d MTiming rows to %s" % (self.nrows, fname)

      fp.write(self.make_data_string(nplaces=nplaces, flag_empty=1))
      fp.close()

      return 0

   def show_isi_stats(self, mesg='', run_len=[]):
      """display ISI timing statistics

            mesg        : display the user message first
            run_len     : can be empty, length 1 or length nrows

         display these statistics:
            - total time, total stim time, total rest time

            - total time per run
            - total stim time per run
            - total rest time per run

            - pre-stim rest per run
            - post-stim response rest per run (if run_len is given)
            - total ISI rest per run

            - min/mean/max (stdev) of stim duration
            - min/mean/max (stdev) of ISI rest
      """

      if not self.ready:
         print '** M Timing: nothing to compute ISI stats from'
         return 1

      if self.mtype != MTYPE_INT:
         print '** M Timing: cannot compute stats without interval mtype'
         return 1

      if self.nrows != len(self.data):
         print '** bad MTiming, nrows=%d, datalen=%d, failing...' % \
               (self.nrows, len(self.data))

      # make a sorted copy
      scopy = self.copy()
      scopy.sort()

      # make an updated run lengths list
      if len(run_len) == 0:
         rlens = [0 for rind in range(self.nrows)]
      elif len(run_len) == 1:
         rlens = [run_len[0] for rind in range(self.nrows)]
      elif len(run_len) == self.nrows:
         rlens = run_len
      else:     # failure
         print '** invalid len(run_len)=%d, must be one of 0,1,%d' % \
               (len(run_len), self.nrows)

      if self.verb > 3:
         print scopy.make_data_string(nplaces=1, flag_empty=0, check_simple=0,
                        mesg='scopy data')
      
      all_stim  = []    # all stimulus durations (one row per run)
      all_isi   = []    # all isi times (one row per run)
      pre_time  = []    # pre-stim, per run
      post_time = []    # pose-stim, per run
      run_time  = []    # total run time, per run
      for rind in range(self.nrows):
         run  = scopy.data[rind]
         rlen = rlens[rind]

         if len(run) == 0:      # empty run
            all_stim.append([])
            all_isi.append([])
            pre_time.append(rlen)
            post_time.append(0.0)
            run_time.append(rlen)
            continue

         # if no run len specified, use end of last stimulus
         if rlen == 0.0: rlen = run[-1][1]
         elif rlen < run[-1][1]:
            print '** run %d: given length = %s, last stim ends at %s' % \
                  (rind+1, rlen, run[-1][1])
            return 1

         # pre- and post-stim times are set
         pre = run[0][0]
         post = rlen - run[-1][1]

         if pre < 0:
            print '** ISI error: first stimulus of run %d at negative time %s'%\
                  (rind+1, run[sind][0])
            return 1

         # init accumulation vars
         stimes = [run[0][1] - run[0][0]]
         itimes = []

         # for each following index, update stim and isi times
         # (check for bad overlap)
         for sind in range(1, len(run)):
            if run[sind][0] < run[sind-1][1]:
               print '** ISI error: stimuli overlap at run %d, time %s' % \
                     (rind+1, run[sind][0])
               return 1
            stimes.append(run[sind][1]-run[sind][0])
            itimes.append(run[sind][0]-run[sind-1][1])

         # store results
         all_stim.append(stimes)
         all_isi.append(itimes)
         pre_time.append(pre)
         post_time.append(post)
         run_time.append(rlen)

      # tally the results
      rtot_stim = [] ; rtot_isi = [] ; rtot_rest = []
      stim_list = [] ; isi_list = [] ; nstim_list = []
      for rind in range(self.nrows):
         rtot_stim.append(UTIL.loc_sum(all_stim[rind]))
         rtot_rest.append(pre_time[rind] + UTIL.loc_sum(all_isi[rind]) +
                          post_time[rind])
         rtot_isi.append(UTIL.loc_sum(all_isi[rind]))
         stim_list.extend(all_stim[rind])
         isi_list.extend(all_isi[rind])
         nstim_list.append(len(all_stim[rind]))

      if mesg: mstr = '(%s) ' % mesg
      else:    mstr = ''
      print '\nISI statistics %s:\n' % mstr

      print '                        total      per run'
      print '                       ------      ------------------------------'
      print '    total time         %6.1f     %s'   % \
                 (UTIL.loc_sum(run_time), float_list_string(run_time, ndec=1))
      print '    total time: stim   %6.1f     %s'   % \
                 (UTIL.loc_sum(rtot_stim),float_list_string(rtot_stim,7,ndec=1))
      print '    total time: rest   %6.1f     %s'   % \
                 (UTIL.loc_sum(rtot_rest),float_list_string(rtot_rest,7,ndec=1))
      print ''
      print '    rest: total isi    %6.1f     %s'   % \
                 (UTIL.loc_sum(rtot_isi), float_list_string(rtot_isi,7,ndec=1))
      print '    rest: pre stim     %6.1f     %s'   % \
                 (UTIL.loc_sum(pre_time), float_list_string(pre_time,7,ndec=1))
      print '    rest: post stim    %6.1f     %s'   % \
                 (UTIL.loc_sum(post_time),float_list_string(post_time,7,ndec=1))
      print ''
      print '    num stimuli      %6d     %s'   % \
            (UTIL.loc_sum(nstim_list), float_list_string(nstim_list,7,ndec=0))
      print '\n'

      print '                         min      mean     max     stdev'
      print '                       -------  -------  -------  -------'

      print '    rest: pre-stim     %7.3f  %7.3f  %7.3f  %7.3f' % \
            (UTIL.min_mean_max_stdev(pre_time))
      print '    rest: post-stim    %7.3f  %7.3f  %7.3f  %7.3f' % \
            (UTIL.min_mean_max_stdev(post_time))
      print ''

      for ind in range(self.nrows):
         m0, m1, m2, s = UTIL.min_mean_max_stdev(all_isi[ind])
         print '    rest: run #%d ISI   %7.3f  %7.3f  %7.3f  %7.3f' % \
                    (ind, m0, m1, m2, s)

      print ''
      print '    all runs: ISI      %7.3f  %7.3f  %7.3f  %7.3f' % \
            (UTIL.min_mean_max_stdev(isi_list))
      print '    all runs: stimuli  %7.3f  %7.3f  %7.3f  %7.3f' % \
            (UTIL.min_mean_max_stdev(stim_list))
      print ''

      # clean up, just to be kind
      del(all_stim); del(all_isi); del(pre_time); del(post_time); del(run_time)
      
      del(rtot_stim); del(rtot_isi); del(rtot_rest); del(stim_list)
      del(isi_list); del(nstim_list)

      del(scopy)

def float_list_string(vals, nchar=7, ndec=3, nspaces=2):
   str = ''
   for val in vals: str += '%*.*f%*s' % (nchar, ndec, val, nspaces, '')

   return str

