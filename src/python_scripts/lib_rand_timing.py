#!/usr/bin/env python

import sys, random, os, math
import afni_util as UTIL

gDEF_T_GRAN     = 0.1    # default time granularity, in seconds

g_valid_dist_types = ['decay', 'uniform', 'INSTANT']

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

      # validate dist_type
   
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

      this function is not really returning error codes or whining
      (maybe that should change?)
   """

   if nevents <= 0:
      return []

   # ----------------------------------------------------------------------
   # quick check: if min == max >= 0, return list of those durations
   if tclass.min_dur >= 0 and tclass.min_dur == tclass.max_dur:
      return [tclass.min_dur] * nevents

   # other quick check: INSTANT type is list of duration 0 events
   if tclass.dist_type == 'INSTANT':
      return [0] * nevents

   # ----------------------------------------------------------------------
   # reconcile total time (ttime) and min duration (min_dur),
   # then initialize a minimum time list,
   # then note remaining time and an appropriate max_time
   if tclass.mean_dur == 0:
      ttime = 0.0
   elif tclass.mean_dur > 0:
      ttime = 1.0 * tclass.mean_dur * nevents

      if total_time > 0:
         if (ttime - total_time)/total_time < 0.95 or
            (ttime - total_time)/total_time > 1.05:
            print '** warning: random_duration_list has conflicting total time'
            print '   from mean = %s, from param = %s' % (ttime, total_time)
   elif total_time > 0:
      ttime = total_time
   else:
      ttime = 0

   # if total time is zero, we are done
   if ttime == 0:
      return [0] * nevents

   # quick check: if only 1 event, return
   if nevents == 1:
      return [ttime]

   # ----------------------------------------------------------------------
   # now note min duration
   min_dur = tclass.min_dur
   if min_dur <= 0: min_dur = 0

   # initialize a list of minimums, and set remain to the remaining time
   min_list = [min_dur] * nevents
   remain = ttime - min_dur * nevents

   # if time is already maxed out by the minimum, just return it
   if remain < tclass.t_gran
      return min_list

   # get appropriate max remaining time (beyond min_dur)
   if tclass.max_time > 0:
      max_time = tclass.max_time - min_dur
      if max_time <= 0: # just bail
         return min_list
      # if there is too much time left, everyone gets the max
      if remain >= (max_time * ntrials):
         return [tclass.max_time] * nevents
   else:
      # no maximum
      max_time = -1.0

   # ======================================================================
   # we have some time (remain) to distribute, possibly with a max time
   # ======================================================================

   # check for valid type
   if tclass.dist_type not in g_valid_dist_types:
      print '** invalid dist_type %s for TimingClass class %s' \
            % (tclass.dist_type, tclass.name)
      # just return average?
      return [ttime*1.0/nevents] * nevents

   # decay type:
   if tclass.dist_type == 'decay':
      dlist = decay_get_dur_list(nevents, remain, max_time, tclass.t_gran)

def decay_get_dur_list(nevents, tot_time, max_dur, t_gran):
   """return a list of durations of length nevents, such that tot_time is
      distributed with PDF decay (okay, the discrete version, which is less)
   """

   durlist = [0] * nevents

   # see how much rest there is to distribute
   nrest = tot_time / t_gran

   # if no rest, just return the 0-filled list
   if nrest < 1:
      return durlist

   # start by doing a simple random distribution at t_gran granularity
   # (partition time with nevents-1 dividers)
   elist = [1] * (nevents - 1)
   rlist = [0] * nrest

   elist.extend(rlist)

   UTIL.shuffle(elist)

   # now get counts
   posn = -1
   prev = posn
   for eind in range(nevents):
      # look for the next 1
      try: posn = elist.index(1, prev+1)
      except:
         print '** DGDL index failure, n = %d, elist = %s' % (nevents, elist)
         return durlist

      # rest count is number of zeros between prev and posn
      durlist[eind] = posn-prev-1
      prev = posn

   # if no maximum to deal with, we are done (scale by t_gran)
   if max_dur < 0:
      for dind in range(nevents):
         durlist[dind] *= t_gran
      return durlist

   # --------------------------------------------------
   # have a max duration, so redistribute the wealth

   nmax = int(max_dur / t_gran)

   decay_apply_max_limit(durlist, nmax)

   return durlist

def decay_apply_max_limit(dlist, nmax):
   """none of the (integer) entries in dlist should exceed nmax
      - modify the actual list
   """
   
   n2move = 0
   navail = 0
   spacelist = []

   for dind, dur in enumerate(dlist):
      # if there is space here, note the index and available space
      if dur < nmax:
         navail += nmax-dur
         spacelist.append(dind)
      # if this is too full, track count to move
      # (do not fix until we are sure we can)
      elif dur > nmax:
         n2move += dur-nmax

   # is there anything to fix?
   if n2move == 0:
      return

   # can we actually fix this?  (failure should already be prevented)
   if navail < n2move:
      print '** DAML space availability error'
      return

   # --------------------------------------------------
   # okay, start fixing things

   # first, truncate to max (skipped above, just to be sure)
   for dind, dur in enumerate(dlist):
      if dur > nmax:
         dlist[dind] = nmax

   # for each n2move, pick a place to move it and track space
   nspace = len(spacelist)

if __name__ == '__main__':
   print '** this is not a main module'
   sys.exit(1)

