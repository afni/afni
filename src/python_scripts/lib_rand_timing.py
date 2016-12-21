#!/usr/bin/env python

import sys, random, os, math
import afni_util as UTIL

gDEF_T_GRAN     = 0.1    # default time granularity, in seconds


# -add_timing_class stimA 3 3  3 decay 0.1
# -add_timing_class stimA 3 5 10
# -add_timing_class stimA 3 5  7 uniform 1 0.1

# class for Timing object
# - initialize with (name, min_dur required)
#       - name (required)
#       - min, mean and max duation
#         (min required, mean/max of -1 are unspecified)
#       - distribution type ('decay' or 'uniform')
#       - t_gran (granularity of time)
class TimingClass:
   def __init__(self, name, min_dur, mean_dur, max_dur,
                      dist_type='decay', params=[], verb=1):
      # required from the command line (name value -> set duration)
      self.name         = name

      self.min_dur      = min_dur       # ** required **
      self.mean_dur     = mean_dur      # implies total_time (-1 means unspec)
      self.max_dur      = max_dur       # (-1 means unspec)

      self.dist_type    = dist_type     # decay, uniform
      self.params       = params        # e.g. nbins for uniform
      self.verb         = verb

      # additional parameters (just t_gran, for now)
      self.t_gran       = gDEF_T_GRAN
      if len(params) > 0:       # first must be t_gran
         try: t_gran = float(params[0])
         except:
            print "** invalid %s timing class t_gran = %s" % (name, params[0])
            return None

      # computation parameters (computed during processing)
      self.total_time   = 0

   def show(self, mesg='', details=0):
      if mesg: mstr = '(%s) ' % mesg
      else:    mstr = ''
      print '-- TimingClass %s:' % mstr
      print '   name         : %s' % self.name
      print '   min_dur      : %s' % self.min_dur
      print '   mean_dur     : %s' % self.mean_dur
      print '   max_dur      : %s' % self.max_dur
      print '   dist_type    : %s' % self.dist_type
      print '   params       : %s' % self.params

      if details:
         print '   verb         : %s' % self.verb
         print '   total_time   : %s' % self.total_time

# stimulus event object, with 
class StimClass:
   def __init__(self, name, nreps, stname, rtname=None, verb=1):
      # optional from command line
      self.name         = name
      self.nreps        = nreps         # number of events (per-/across- runs)
      self.stname       = stname        # name of stim TimingClass
      self.rtname       = rtname        # name of rest TimingClass

      self.verb         = verb

   def show(self, mesg='', details=0):
      if mesg: mstr = '(%s) ' % mesg
      else:    mstr = ''
      print '-- StimClass %s:' % mstr
      print '   name         : %s' % self.name
      print '   nreps        : %s' % self.nreps
      print '   stname       : %s' % self.stname
      print '   rtname       : %s' % self.rtname

      # no details yet

def create_duration_lists(slist, nruns, across_runs=0, verb=1):
   """for each class, create nreps event times
      if across_runs == 0: make nruns such lists per class

      return 0 on success, 1 on error
   """

   nsc = len(slist)
   if verb > 1:
      print '-- creating stim dur lists for %d classes, nruns=%d, across=%d' \
            % (nsc, nruns, across_runs)

   for sc in slist:
      nevents = sc.nreps
      if across_runs:
         pass
      else:
         pass

   return 1

random_duration_list(nevents, tclass, total_time):
   """create a length nevents list of durations according to the class

      if dur_mean > 0 and total_time > 0:
         warn if they imply different times (10%?)

      must have dur_mean >= 0 or total_time > 0
   """

   if nevents <= 0: return []

   if tclass.mean_dur == 0:
      ttime = 0.0
   elif tclass.mean_dur > 0:
      ttime = 1.0 * tclass.mean_dur * nevents

      if total_time > 0:
         if (ttime - total_time)/total_time < 0.95 or
            (ttime - total_time)/total_time > 1.05:
            print '** warning: random_duration_list has conflicting total time'
            print '   from mean = %s, from param = %s' % (ttime, total_time)


if __name__ == '__main__':
   print '** this is not a main module'
   sys.exit(1)

