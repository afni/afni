#!/usr/bin/env python

# Timing library, accessed via timing_tool.py.
#
# - offers class AfniTiming
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
import lib_afni1D as LD

# ----------------------------------------------------------------------
# AfniTiming class - for stim times that are married with
#                    either time intervals or magnitudes

class AfniTiming(LD.AfniData):
   def __init__(self, filename="", dur=-1, verb=1):
      """AFNI married stimulus timing class"""

      super(AfniTiming, self).__init__(filename, verb=verb)

      # additional variables (on top of those in AfniData)
      self.dur_len = -1           # if MTYPE_DUR, length of equal intervals
                                  # (rcr: if constant)

      # if from_at and not filename: self.init_from_at(at)

      # initialize duration data
      if self.ready: self.init_durations(dur)

      if self.verb > 3: self.show()

   # old accessor functions from AfniTiming
   def add_rows(self, newdata):
      """add newdata.nrows of data (additional runs), return 0 on success"""
      if not self.ready or not newdata.ready:
         print '** AMTiming elements not ready for adding rows (%d,%d)' % \
               (self.ready, newdata.ready)
         return 1

      if self.verb > 1: print '++ Timing: adding %d rows' % newdata.nrows

      if self.mtype != newdata.mtype:
         print '** add rows: mis-match mtypes (%d vs. %d)' \
               % (self.mtype, newdata.mtype)

      # get data and mdata
      edata = copy.deepcopy(newdata.data)
      self.data.extend(edata)

      edata = copy.deepcopy(newdata.mdata)
      self.mdata.extend(edata)

      self.nrows += newdata.nrows

      if self.row_lens and newdata.row_lens:
         self.row_lens.extend(newdata.row_lens)

      return 0

   def init_durations(self, dur=-1):
      """initialize ddata as format [start_time, end_time]"""

      # start with general format
      if not self.ready: return

      # if dur is passed, first apply it
      if dur >= 0:
         for row in self.mdata:
            for entry in row: entry[2] = dur

      self.ddata = []
      for row in self.mdata:
         self.ddata.append([[val[0],val[0]+val[2]] for val in row])

      if   dur  > 0: self.mtype |= LD.MTYPE_DUR # set bit
      elif dur == 0: self.mtype &= LD.MTYPE_AMP # only allow AMP to go through
      # else leave as is

      self.dur_len = dur

   def set_dur_len(self):
      self.dur_len = self.get_duration()

   def extend_rows(self, newdata):
      """extend each row by the corresponding row of brows"""

      self.extend_data_rows(newdata)

      # if interval lengths are not equal, must clear it
      if self.dur_len != newdata.dur_len: self.dur_len = -1

      return 0

   def get_start_end_timing(self, sort=0):
      """return a 2D array of [start, finish] times (so 3D object)"""
      sftimes = []
      for row in self.mdata:
         times = [[e[0],e[0]+e[2]] for e in row]
         if sort: times.sort()
         sftimes.append(times)
      return sftimes

   def partition(self, part_file, prefix):
      """partition timing based on part_file labels, write each part to
         prefix_label.1D"""

      if not self.ready:
         print '** Timing element not ready for partitioning'
         return 1

      labels = read_value_file(part_file)
      if labels == None:
         print "** failed to read partition label file '%s'" % part_file
         return 1

      nlabr = len(labels)

      if self.verb > 3: print '-- partition: %d labels: %s' % (nlabr, labels)

      # first test nrows, then test lengths per row
      if self.nrows != nlabr:
         print '** Timing nrows differs from partition nrows (%d, %d)' % \
               (self.nrows,nlabr)
         return 1
      for ind in range(self.nrows):
         if len(self.data[ind]) != len(labels[ind]):
            print "** timing and label row lengths differ at line %d"%ind+1
            return 1

      # make unique label list
      ulabs = []
      for line in labels:
         ulabs.extend(line)
      ulabs = UTIL.get_unique_sublist(ulabs)
      if self.verb > 2: print '-- partition: unique label list: %s' % ulabs
      if ulabs.count('0'): ulabs.remove('0')

      if self.verb > 1: print '++ Timing: partitioning with %s' % part_file

      # ------------------------------------------------------------
      # do the work, starting with copy:
      # for each label, extract those times and write out as timing file
      dupe = self.copy()        # keep results in new class instance
      for lab in ulabs:
         # extract timing for this label 'lab'
         mdata = []
         for r in range(nlabr):
            drow = []           # make one row of times for this label
            for c in range(len(labels[r])):
               if labels[r][c] == lab: drow.append(self.mdata[r][c])
            mdata.append(drow)  # and append the new row
         del(dupe.mdata)        # out with the old,
         dupe.mdata = mdata     # and in with the new
         dupe.write_times('%s_%s.1D' % (prefix,lab))    # and write, yay

      del(dupe)                 # nuke the temporary instance

      return 0

   def global_to_local(self, run_len):
      """convert global times to local, based in run_len array
         return 0 on success, 1 on any error"""

      if not self.ready:
         print '** global timing not ready'
         return 1

      if len(run_len) == 0:
         print '** global_to_local requires -run_len'
         return 1

      if not self.is_rect():
         print '** global timing is not rectangular'
         return 1

      rlen = len(self.mdata[0])         # note row length

      if rlen > 1:
         print '** global timing is not a single column'
         return 1

      if rlen < 1: return 0             # nothing to do
      if self.nrows < 2: return 0       # nothing to do

      # make just one row and sort
      self.transpose()
      self.sort()

      if self.verb > 2:
         self.show('global_to_local')
         print '-- run lengths : %s' % run_len

      if self.verb > 4:
         print '-- global start time matrix %s' % self.data

      # now walk through runs and insert times as we go
      newdata = []
      newmdata = []
      endtime = 0.0
      sind = 0
      stimes = self.mdata[0]
      ntimes = len(stimes)
      for etime in run_len:
         starttime = endtime
         endtime += etime
         if sind >= ntimes:  # only empty runs left
            newdata.append([])
            newmdata.append([])
            continue
         # times are left, decide which go for this run
         last = sind
         while last < ntimes and stimes[last][0] < endtime: last += 1
         mtimes = stimes[sind:last]
         for tentry in mtimes: tentry[0] -= starttime
         newdata.append([t[0] for t in mtimes])
         newmdata.append(mtimes)
         sind = last

      # insert any remaining times at end of last run(and warn user)
      if sind < ntimes:
         if self.verb > 0:
            print '** global to local: %d times after last run' % (ntimes-sind)
         mtimes = stimes[sind:]
         for tentry in mtimes: tentry[0] -= starttime
         newdata[-1].extend([t[0] for t in mtimes])
         newmdata[-1].extend(mtimes)

      del(self.data)
      del(self.mdata)
      self.data = newdata
      self.mdata = newmdata
      self.nrows = len(self.mdata)

      if self.verb > 4:
         print '-- local time matrix %s' % newdata

      return 0

   def local_to_global(self, run_len):
      """convert local times to global times, based in run_len array
         return 0 on success, 1 on any error"""

      if not self.ready:
         print '** local timing not ready'
         return 1

      if len(run_len) != self.nrows:
         print '** local_to_global: have %d run times but %d data rows' \
               % (len(run_len), self.nrows)
         return 1

      # make sure it is sorted for this
      self.sort()

      if self.verb > 2:
         self.show('local_to_global')
         print '-- run lengths : %s' % run_len

      if self.verb > 4:
         print '-- local time matrix %s' % self.data

      # now walk through runs and insert times as we go
      newdata = []
      newmdata = []
      runstart = 0.0
      for rind, rtime in enumerate(run_len):
         # each new time is a new row
         mrow = copy.deepcopy(self.mdata[rind])
         for ind, mtime in enumerate(mrow):
            mtime[0] += runstart
            newdata.append([mtime[0]])
            newmdata.append([mtime])
         runstart += rtime      # last one is useless

      del(self.data)
      self.data = newdata
      del(self.mdata)
      self.mdata = newmdata
      self.nrows = len(self.data)

      if self.verb > 4:
         print '-- global time matrix %s' % newdata

      return 0

   def add_val(self, val):
      """add the given value to each element"""
      if not self.ready: return 1

      if type(val) == str:
         try: val = float(val)
         except:
            print "** invalid value to add to timing: '%s'" % val
            return 1

      if self.verb > 1: print '-- Timing: adding %f to data...' % val

      for row in self.data:
         for ind in range(len(row)):
            row[ind] += val

      for row in self.mdata:
         for ind in range(len(row)):
            row[ind][0] += val

      return 0

   def shift_to_offset(self, offset=0):
      """shift all run times to start at the given offset
         (offset should be <= first values)"""

      if not self.ready: return 1

      if type(offset) == str:
         try: offset = float(offset)
         except:
            print "** invalid offset to add to timing: '%s'" % offset
            return 1

      if self.verb > 1: print '-- timing: setting offset to %f ...' % offset

      # make sure it is sorted for this
      self.sort()

      del(self.data)
      self.data = []

      for rind, row in enumerate(self.mdata):
         if len(row) < 1:
            self.data.append([])
            continue
         diff = row[0][0] - offset
         if diff < 0:
            print '** offset shift to %f too big for run %d' % (offset, rind)
            return 1
         for ind in range(len(row)):
            row[ind][0] -= diff
         self.data.append([e[0] for e in row])

      return 0

   def scale_val(self, val):
      """multiply the given value into each element"""
      if not self.ready: return 1

      if type(val) == type('hi'):
         try: val = float(val)
         except:
            print "** invalid value to scale into timing: '%s'" % val
            return 1

      if self.verb > 1: print '-- Timing: scaling data by %f ...' % val

      for row in self.data:
         for ind in range(len(row)):
            row[ind] *= val

      for row in self.mdata:
         for ind in range(len(row)):
            row[ind][0] *= val

      return 0

   def round_times(self, tr, round_frac=1.0):
      """round/truncate times to multiples of the TR

         round_frac : fraction of TR required to "round up"
                      e.g. 0.5  : normal rounding
                           1.0  : never round up, floor()
                           0.0  : always round up, ceil()
      """
      if not self.ready: return 1
      if tr <= 0:
         print "** truncate_times: invalid tr %s" % tr
         return 1

      # convert to fraction to add before truncation (and be sure of type)
      try: rf = 1.0 - round_frac
      except:
         print "** truncate_times: invalid round_frac '%s'" % round_frac
         return 1

      try: tr = float(tr)
      except:
         print "** truncate_times: invalid tr '%s'" % tr
         return 1

      if rf < 0.0 or rf > 1.0:
         print "** truncate_times: bad round_frac outside [0,1]: %s" % rf
         return 1

      if self.verb > 1:
         print '-- Timing: round times to multiples of %s (frac = %s)'%(tr,rf)

      # fr to use for floor and ceil (to assist with fractional TRs)
      if tr == math.floor(tr): tiny = 0.0
      else:                    tiny = 0.0000000001

      # scale mdata and re-create data
      del(self.data)
      self.data = []
      for row in self.mdata:
         for ind in range(len(row)):
            # start with TR index
            tind = row[ind][0]/tr
            # note that rf = 0 now means floor and 1 means ceil

            # add/subract a tiny fraction even for truncation
            if rf == 1.0   :
               if tind == 0: val = 0.0  # to avoid tiny negatives
               else:         val = math.ceil(tind-tiny) * tr
            elif rf == 0.0 : val = math.floor(tind+tiny) * tr
            else           : val = math.floor(tind+rf) * tr
            row[ind][0] = val
         self.data.append([e[0] for e in row])

      return 0

   def write_times(self, fname='', nplaces=-1):
      """write the current M timing out, with nplaces right of the decimal"""

      self.write_as_timing(fname, nplaces)

      return 0

   def timing_to_1D(self, run_len, tr, min_frac, per_run=0):
      """return a 0/1 array of trs surviving min_frac cutoff

                run_len   : list of run durations, in seconds
                            (each must encompass the last stimulus, of course)
                tr        : time resolution of output 1D file
                min_frac  : minimum TR fraction occupied by stimulus required
                            for a set value in output array
                            (must be in [0.0, 1.0])
                per_run   : if set, result is list of runs, else catenated

         return an error string and the 0/1 array
                (on success, the error string is '')
      """

      # maybe the user provided only one run length
      if self.nrows > 0 and len(run_len) == 1:
         run_len = [run_len[0]] * self.nrows

      errstr, result = self.timing_to_tr_frac(run_len, tr, per_run)

      if errstr != '' or len(result) < 1: return errstr, result

      if per_run:
         new_res = []
         for row in result:
            new_res.append(self.get_threshold_list(row, min_frac))
      else:
         new_res = self.get_threshold_list(result, min_frac)

      del(result)

      return '', new_res

   def get_threshold_list(self, data, min_val):
      """return a 0/1 copy of 'data', with each value set iff >= min_val"""
      result = [0 for ind in range(len(data))]
      for ind, val in enumerate(data):
         if val >= min_val: result[ind] = 1
      return result

   def timing_to_tr_frac(self, run_len, tr, per_run=0):
      """return an array of stim fractions, where is value is the fraction
         of the current TR occupied by stimulus

         --> if per_run is set, output will be one row per run

         The output will be of length sum(TRs per run) and will be in [0,1.0].
         Note that a single stimulus can span multiple TRs.

         ** save this comment for elsewhere
         ** save min_tr_frac for elsewhere
                min_tr_frac     : minimum fraction of a TR required to set it
                                  (must be in (0.0, 1.0])
         Note that if they are not TR-locked and min_tr_frac <= 0.5, then
         one stimulus lasting one TR but occuring on the half TR can result
         in a pair of consecutive 1s.
         ** end save

                run_len         : list of run durations, in seconds
                                  (each must encompass the last TR, of course)
                tr              : time resolution of output 1D file

         On success, the error string should be empty and stim_list should not.

         return error string, stim list

         ** testing **

         import math, copy
         import afni_util as UTIL, lib_timing as LT
         reload LT
         t = LT.AfniTiming('ch_fltr.txt')
         t.timing_to_tr_frac(run_len, 2.5)
         t.timing_to_1D(run_len, 2.5, 0.3)
      """

      if not self.ready:
         return '** M Timing: nothing to compute ISI stats from', []

      #if not self.mtype & LD.MTYPE_DUR:
      #   return '** M Timing: cannot compute stats without stim duration', []

      if self.nrows != len(self.data):
         return '** bad MTiming, nrows=%d, datalen=%d, failing...' % \
               (self.nrows, len(self.data)), []

      if self.nrows != len(run_len):
         return '** run_len list is %d of %d runs in timing_to_1D: %s'   \
               % (len(run_len), self.nrows, run_len), []

      if tr <= 0.0:
         return '** timing_to_tr, illegal TR <= 0: %g' % tr, []

      # make a sorted copy of format run x stim x [start,end], i.e. is 3-D
      tdata = self.get_start_end_timing(sort=1)

      if self.verb > 1:
         print 'timing_to_tr_fr, tr = %g, nruns = %d' % (tr,len(run_len))

      # need to check each run for being empty
      for ind, data in enumerate(tdata):
          if len(data) < 1: continue
          if data[-1][1] > run_len[ind] or run_len[ind] < 0:
              return '** run %d, stim ends after end of run' % (ind+1), []
          
      result = []
      # process one run at a time, first converting to TR indicies
      for rind, data in enumerate(tdata):
         if self.verb > 4:
            print '\n++ stimulus on/off times, run %d :' % (rind+1)
            print data

         for tind in range(len(data)):  # convert seconds to TRs
            data[tind][0] = round(data[tind][0]/tr,3)
            data[tind][1] = round(data[tind][1]/tr,3)

            if tind > 0 and data[tind][0] < data[tind-1][1]:
                return '** run %d, index %d, stimulus overlap with next' \
                       % (rind, tind), []
         if self.verb > 4:
            print '++ stimulus on/off TR times, run %d :' % (rind+1)
            print data
         if self.verb > 3:
            print '++ tr fractions, run %d :' % (rind+1)
            print [data[tind][1]-data[tind][0] for tind in range(len(data))]

         # do the real work, for each stimulus, fill appropriate tr fractions
         # init frac list with TR timing (and enough TRs for run)
         num_trs = int(math.ceil( run_len[rind]/tr ))
         if self.verb>2: print '-- TR frac: have %d TRs and %d events over run'\
                               % (num_trs, len(data))
         rdata = [0 for i in range(num_trs)]
         for sind in range(len(data)):
            start  = data[sind][0]      # initial TR (fractional) time
            end    = data[sind][1]      # end TR time
            startp = int(start)         # initial TR index
            endp   = int(end)           # end TR index

            # deal with easy case, else: startp, intermediates, endp

            # easy case : quick process of single TR result
            if endp == startp:
               rdata[startp] = end-start
               continue

            # startp, intermediates (between startp, endp, exclusive), endp
            rdata[startp] = round(1-(start-startp),3)
            for tind in range(startp+1,endp): rdata[tind] = 1.0
            rdata[endp] = round(end-endp, 3)

         if self.verb > 4:
            print '\n++ timing_to_tr_fr, result for run %d:' % (rind+1)
            print ' '.join(["%g" % rdata[ind] for ind in range(len(rdata))])

         if per_run: result.append(rdata)
         else:       result.extend(rdata)

      del(tdata)
      del(rdata)

      return '', result

   def show_isi_stats(self, mesg='', run_len=[], tr=0, rest_file=''):
      """display ISI timing statistics

            mesg        : display the user message first
            run_len     : can be empty, length 1 or length nrows
            tr          : if > 0: show mean/stdev for stimuli within TRs
                          (so 0 <= mean < tr)
            rest_file   : if set, save all rest durations

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

      if self.mtype != LD.MTYPE_DUR:
         print '** warning: computing stats without duration'

      if self.nrows != len(self.data):
         print '** bad MTiming, nrows=%d, datalen=%d, failing...' % \
               (self.nrows, len(self.data))
         return 1

      # make a sorted copy
      scopy = self.copy()
      scopy.sort()

      # make a copy of format run x stim x [start,end], i.e. is 3-D
      tdata = scopy.get_start_end_timing()

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
         return 1

      if self.verb > 1:
         print '-- show_isi_stats, run_len = %s, tr = %s, rest_file = %s' \
               % (run_len, tr, rest_file)

      if self.verb > 3:
         print scopy.make_data_string(nplaces=1, flag_empty=0, check_simple=0,
                        mesg='scopy data')
      
      all_stim  = []    # all stimulus durations (one row per run)
      all_isi   = []    # all isi times (one row per run)
      pre_time  = []    # pre-stim, per run
      post_time = []    # pose-stim, per run
      run_time  = []    # total run time, per run
      errs      = 0     # allow a few errors before failing
      max_errs  = 10
      for rind in range(self.nrows):
         run  = tdata[rind]
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
            errs += 1
            if errs > max_errs:
               print '** bailing...'
               return 1

         # pre- and post-stim times are set
         pre = run[0][0]
         post = rlen - run[-1][1]

         if pre < 0:
            print '** ISI error: first stimulus of run %d at negative time %s'%\
                  (rind+1, run[sind][0])
            errs += 1
            if errs > max_errs:
               print '** bailing...'
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
               errs += 1
               if errs > max_errs:
                  print '** bailing...'
                  return 1
            stimes.append(run[sind][1]-run[sind][0])
            itimes.append(run[sind][0]-run[sind-1][1])

         # store results
         all_stim.append(stimes)
         all_isi.append(itimes)
         pre_time.append(pre)
         post_time.append(post)
         run_time.append(rlen)

      if errs > 0: return 1

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

      # and possibly print out offset info
      if tr > 0: self.show_TR_offset_stats(tr, '')

      # maybe write all rest durations
      if rest_file:
         all_rest = copy.deepcopy(all_isi)
         for run, rest in enumerate(all_rest):
             rest[:0] = [pre_time[run]]
             rest.append(post_time[run])
         UTIL.write_to_timing_file(all_rest, rest_file)

      # clean up, just to be kind
      del(all_stim); del(all_isi); del(pre_time); del(post_time); del(run_time)
      
      del(rtot_stim); del(rtot_isi); del(rtot_rest); del(stim_list)
      del(isi_list); del(nstim_list)

      del(scopy)
      del(tdata)

   def get_TR_offset_stats(self, tr):
      """create a list of TR offsets (per-run and overall)

            tr : must be positive

         return: 6 values in a list:
                    mean, maxabs, stdev of absolute and fractional offsets
                 empty list on error
      """

      if not self.ready:
         print '** M Timing: nothing to compute ISI stats from'
         return []

      if self.nrows != len(self.data):
         print '** bad MTiming, nrows=%d, datalen=%d, failing...' % \
               (self.nrows, len(self.data))
         return []

      if tr < 0.0:
         print '** show_TR_offset_stats: invalid TR %s' % tr
         return []

      # make a copy of format run x stim x [start,end], i.e. is 3-D
      tdata = self.get_start_end_timing()

      offsets   = []    # stim offsets within given TRs
      for rind in range(self.nrows):
         run  = tdata[rind]
         if len(run) == 0: continue

         roffsets = UTIL.interval_offsets([val[0] for val in run], tr)
         offsets.extend(roffsets)

      if len(offsets) < 1: return []

      # get overall stats (absolute and fractional)
      # absolute
      m0, m1, m2, s = UTIL.min_mean_max_stdev(offsets)
      offm = m1; offs = s
      mn = abs(min(offsets))
      offmax = abs(max(offsets))
      if mn > offmax: offmax = mn       

      # fractional
      for ind, val in enumerate(offsets):
         offsets[ind] = val/tr
      m0, m1, m2, s = UTIL.min_mean_max_stdev(offsets)

      del(offsets)

      return [offm, offmax, offs, m1, offmax/tr, s]
      
   def show_TR_offset_stats(self, tr, mesg=''):
      """display statistics regarding within-TR offsets of stimuli

            tr          : show mean/stdev for stimuli within TRs
                          (so 0 <= mean < tr)
            mesg        : display the user message in the output
      """

      if not self.ready:
         print '** M Timing: nothing to compute ISI stats from'
         return 1

      if self.nrows != len(self.data):
         print '** bad MTiming, nrows=%d, datalen=%d, failing...' % \
               (self.nrows, len(self.data))
         return 1

      if tr < 0.0:
         print '** show_TR_offset_stats: invalid TR %s' % tr
         return 1

      off_means = []    # ... means per run
      off_stdev = []    # ... stdevs per run
      for rind in range(self.nrows):
         run  = self.data[rind]
         if len(run) == 0: continue

         # start with list of time remainders (offsets) within each TR
         roffsets = UTIL.interval_offsets([val for val in run], tr)

         m0, m1, m2, s = UTIL.min_mean_max_stdev(roffsets)
         off_means.append(m1)
         off_stdev.append(s)

      # and get overall stats (absolute and fractional)
      offs = self.get_TR_offset_stats(tr)

      # print out offset info
      if mesg: mstr = '(%s) ' % mesg
      else:    mstr = ''

      print '\nwithin-TR stimulus offset statistics %s:\n' % mstr

      if self.nrows > 1:
         print '                       per run'
         print '                       ------------------------------'
         print '    offset means       %s'%float_list_string(off_means, ndec=3)
         print '    offset stdevs      %s'%float_list_string(off_stdev, ndec=3)
         print ''
         print '    overall:     mean = %.3f  maxoff = %.3f  stdev = %.4f' \
               % (offs[0], offs[1], offs[2])
         print '    fractional:  mean = %.3f  maxoff = %.3f  stdev = %.4f\n' \
               % (offs[3], offs[4], offs[5])
      else:
         print '    one run:     mean = %.3f  maxoff = %.3f  stdev = %.4f' \
               % (offs[0], offs[1], offs[2])
         print '    fractional:  mean = %.3f  maxoff = %.3f  stdev = %.4f\n' \
               % (offs[3], offs[4], offs[5])

      # clean up, just to be kind
      del(off_means); del(off_stdev)
      
def float_list_string(vals, nchar=7, ndec=3, nspaces=2):
   str = ''
   for val in vals: str += '%*.*f%*s' % (nchar, ndec, val, nspaces, '')

   return str

def read_value_file(fname):
   """read value file, returning generic values in a matrix (no comments)"""
   try: fp = open(fname, 'r')
   except:
      print "** failed to open value file '%s'" % fname
      return None

   data = []         # data lines

   lind = 0
   for line in fp.readlines():
      lind += 1
      lary = line.split()
      if len(lary) == 0: continue
      if lary[0] == '#': continue

      data.append([x for x in lary if x != '*'])

   fp.close()

   return data

