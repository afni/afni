#!/usr/bin/env python

import sys, random, os, math
import afni_util as UTIL
import lib_afni1D as LD

gDEF_T_GRAN     = 0.01   # default time granularity, in seconds
                         # (OLD one in mrt.py is just 0.1)
gDEF_DEC_PLACES = 2      # decimal places when printing time (-1 ==> %g format)


g_valid_dist_types = ['decay', 'uniform_rand', 'uniform_grid',
                      'fixed', 'INSTANT']
g_fixed_dist_types = ['fixed', 'INSTANT']
g_valid_param_types= ['dist', 't_gran', 'max_consec']

# -add_timing_class stimA 3 3  3 dist=decay 0.1
# -add_timing_class stimA 3 5 10
# -add_timing_class stimA 3 5  7 dist=uniform_grid t_gran=0.1

# class for Timing object
# - initialize with (name, min_dur required)
#       - name (required)
#       - min, mean and max duation
#         (min required, mean/max of -1 are unspecified)
#       - params: of the form VAR=VAL
#          - dist:   distribution type ('decay' or 'uniform')
#          - t_gran: granularity of time, default 0.01
class TimingClass:
   def __init__(self, name, min_dur, mean_dur, max_dur, params=[], verb=1):
      # required from the command line (name value -> set duration)
      self.name         = name

      self.min_dur      = min_dur       # ** required **
      self.mean_dur     = mean_dur      # implies total_time (-1 means unspec)
      self.max_dur      = max_dur       # (-1 means unspec)

      # list of parameters
      self.params       = params[:]     # e.g. nbins for uniform
      self.verb         = verb

      # additional parameters (just t_gran, for now)
      self.dist_type    = 'decay'       # see g_valid_dist_types[]
      self.t_gran       = gDEF_T_GRAN
      self.max_consec   = 0             # >0 to apply

      # computation parameters (computed during processing)
      self.total_time   = 0
      self.status       = 0             # note errors this way

      # --------------------------------------------------
      # apply any extra parameters given
      if len(params) > 0:
         if self.apply_params(params):
            self.status = 1
            return

      # --------------------------------------------------
      # any extra steps...

      # round min and max times to t_gran (actually, truncate towards mean)
      # (but make sure we do not barely miss some t_gran multiple)
      tg = self.t_gran
      if self.min_dur < tg:
         self.min_dur = 0.0
      else:
         self.min_dur = tg * math.ceil (self.min_dur / tg - 0.01)
      if self.max_dur < 0.0:
         self.max_dur = -1.0
      elif self.max_dur < tg:
         self.max_dur = 0.0
      else:
         self.max_dur = tg * math.floor(self.max_dur / tg + 0.01)


   def apply_params(self, params):
      """list of VAR=VAL, verify and apply"""
      for pp in params:
          # store an error string
          errstr = "** TimingClass %s, poorly formed param string '%s'" \
                   % (self.name, pp)

          pv = pp.split('=')
          if len(pv) != 2:
             print errstr
             return 1
          pvar = pv[0]
          pval = pv[1]

          # now check one type at a time, else fail
          if pvar == 'dist':
             if pval not in g_valid_dist_types:
                print errstr
                print '** invalid dist = %s in %s' % (pval, pp)
                print '   must be one of: %s' % ', '.join(g_valid_dist_types)
                return 1
             # apply
             self.dist_type = pval
          elif pvar == 't_gran':
             try: t_gran = float(pval)
             except:
                print errstr
                print "** bad t_gran = %s in %s" % (pval, pp)
                return 1
             if t_gran <= 0:
                print "** invalid t_gran = %s in %s" % (pval, pp)
                return 1
             # apply
             self.t_gran = t_gran
          elif pvar == 'max_consec':
             try: mc = int(pval)
             except:
                print errstr
                print "** bad max_consec = %s in %s" % (pval, pp)
                return 1
             if mc <= 0:
                print "** invalid max_consec = %s in %s" % (pval, pp)
                return 1
             # apply
             self.max_consec = mc
          else:
             print "** TimingClass %s, invalid param name %s in '%s'" \
                   % (self.name, pvar, pp)
             print '   must be one of: %s' % ', '.join(g_valid_param_types)
             return 1

      return 0

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

      print

   def show_durlist_stats(self, durlist, mesg='', details=0, sort=0):
      if mesg != '': mstr = '(%s) ' % mesg
      else:          mstr = ''
      nevents = len(durlist)
      total   = sum(durlist)
      print "=== %sstats for TimingClass %s ===" % (mstr, self.name)
      print "    (%d events, total time %g)" % (nevents, total)

      print '            min       mean      max      stdev'
      print '------    -------   -------   -------   -------'

      print 'expected %7.3f   %7.3f   %7.3f     %s' % \
            (self.min_dur, self.mean_dur, self.max_dur, self.dist_type)

      mmin,mmean,mmax,mstdev = UTIL.min_mean_max_stdev(durlist)
      print 'actual   %7.3f   %7.3f   %7.3f   %7.3f\n' % \
            (mmin, mmean, mmax, mstdev)

      if not details: return

      digs = gDEF_DEC_PLACES

      if sort:
         dlist = durlist[:]
         dlist.sort
         sstr = ' (sorted)'
      else:
         dlist = durlist
         sstr = ''

      print '-- TimingClass %s%s event durations:'  % (self.name,sstr)
      dstr = ['%.*f'%(digs, dd) for dd in dlist]
      print '   %s\n' % ' '.join(dstr)

   def get_one_val(self):
      return random_duration_list(1, self)

   def decay_get_dur_list(self, nevents, tot_time, max_dur):
      """return a list of durations of length nevents, such that tot_time is
         distributed with PDF decay (okay, the discrete version, which is less)
      """

      durlist = [0] * nevents

      # see how much rest there is to distribute
      nrest = int(tot_time / self.t_gran)

      # if no rest, just return the 0-filled list
      if nrest < 1:
         return durlist

      # start by doing a simple random distribution at t_gran granularity
      # (partition time with nevents-1 dividers)
      elist = [1] * (nevents - 1)
      rlist = [0] * nrest

      elist.extend(rlist)

      UTIL.shuffle(elist)
      # add a trailer to count the final duration (since only n-1 dividers)
      elist.append(1)

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
            durlist[dind] *= self.t_gran
         return durlist

      # --------------------------------------------------
      # have a max duration, so redistribute the wealth

      nmax = int(max_dur / self.t_gran)

      self.decay_apply_max_limit(durlist, nmax)

      # and finally, scale by t_gran
      for dind in range(len(durlist)):
         durlist[dind] *= self.t_gran

      return durlist

   def decay_apply_max_limit(self, dlist, nmax):
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
         print '** DAML space availability error for class %s' % self.name
         return

      # --------------------------------------------------
      # okay, start fixing things

      # first, truncate to max (skipped above, just to be sure)
      for dind, dur in enumerate(dlist):
         if dur > nmax:
            dlist[dind] = nmax

      # for each n2move, pick a place to move it and track space
      nspace = len(spacelist)

      for sind in range(n2move):
         if nspace == 0:
            print '** DAML: no more space entries in class %s' % self.name
            return
         mind = int(random.uniform(0,nspace))
         if mind == nspace: mind -= 1  # probability close to 0
         sind = spacelist[mind]
         dlist[sind] += 1
         # maybe this event is maxed out
         if dlist[sind] >= nmax:
            nspace -= 1
            spacelist.remove(sind)
            
      return

   def urand_get_dur_list(self, nevents, tot_time):
      """return a list of durations, distributed uniformly in [0,max_dur]
         (but still on t_gran)

         - mean time is tot/n, max is 2*tot/n

         - get int(n/2) random times
         - fill with max-val (so pairs average at mean)
         - if n is odd, append one mean duration
         - shuffle
      """

      if nevents <= 0: return []
      if nevents == 1: return [tot_time]

      max_dur = tot_time * 2.0 / nevents

      # maximum num t_gran possibile (equates to 0..max_dur in time)
      # (+0.01 is to avoid a close trunction miss)
      nmax = int(max_dur / self.t_gran + 0.01)

      durlist = []
      for ind in range(nevents//2):
         ngran = int(random.uniform(0,nmax+1))
         if ngran > nmax: ngran = nmax  # to be safe
         # add time and balancing time
         durlist.append(ngran * self.t_gran)
         durlist.append((nmax - ngran) * self.t_gran)

      if nevents % 2:
         durlist.append(max_dur/2.0)

      UTIL.shuffle(durlist)

      return durlist

   def ugrid_get_dur_list(self, nevents, tot_time):
      """return a list of durations, distributed evenly along [0,max_dur],
         but truncated to the t_gran grid

         EXPECTS: minimum event time == 0, i.e. old min was subtracted out
         
         - like urand, but create a list of evenly divided times, 
           without any randomness

         - mean time is tot/n, max is 2*tot/n

         - make int(n/2) lower grid times
            - compute exact time, then convert to t_gran indices (val)
         - fill with max-val (so pairs average at mean)
         - if n is odd, append one mean duration
         - shuffle
      """

      if nevents <= 0: return []
      if nevents == 1: return [tot_time]

      max_dur = tot_time * 2.0 / nevents
      tspace = max_dur / (nevents - 1.0)

      # maximum num t_gran possibile (equates to 0..max_dur in time)
      # (+0.01 is to avoid a close trunction miss)
      nmax = int(max_dur / self.t_gran + 0.01)

      durlist = []
      for ind in range(nevents//2):
         actual_time = ind * tspace
         grid_time = self.t_gran * int(actual_time/self.t_gran)
         # add time and balancing time
         durlist.append(grid_time)
         durlist.append(max_dur - grid_time)

      # if odd number, include a mean dur
      if nevents % 2:
         durlist.append(max_dur/2.0)

      UTIL.shuffle(durlist)

      return durlist

# stimulus event object, with 
class StimClass:
   def __init__(self, name, nreps, sclass, rclass, verb=1):
      # optional from command line
      self.name         = name
      self.nreps        = nreps         # number of events (per-/across- runs)
      self.sclass       = sclass        # name of stim TimingClass
      self.rclass       = rclass        # name of rest TimingClass

      if not isinstance(sclass, TimingClass):
         print '** StimClass stim timing is not a TimingClass'
      if not isinstance(rclass, TimingClass):
         print '** StimClass rest timing is not a TimingClass'

      self.verb         = verb

      self.durlist      = []            # durations per run
      self.sdata        = None          # AfniData instance

   def show(self, mesg='', details=0):
      if mesg: mstr = '(%s) ' % mesg
      else:    mstr = ''
      print '-- StimClass %s:' % mstr
      print '   name         : %s' % self.name
      print '   nreps        : %s' % self.nreps
      print '   sclass       : %s' % self.sclass.name
      print '   rclass       : %s' % self.rclass.name

      if details:
         print '   verb         : %s' % self.verb
         self.sclass.show('stim class for %s'%self.name)
         self.rclass.show('rest class for %s'%self.name)

      print

   def show_durlist_stats(self, mesg='', details=0):
      tc = self.sclass
      durlist = self.durlist

      if mesg != '': mstr = '(%s) ' % mesg
      else:          mstr = ''
      print "=== %sstats for StimClass %s ===" % (mstr, self.name)

      print 'run         #      min       mean      max      stdev'
      print '------    -----  -------   -------   -------   -------'
      print 'expected        %7.3f   %7.3f   %7.3f     %s' % \
               (tc.min_dur, tc.mean_dur, tc.max_dur, tc.dist_type)
      for rind, durs in enumerate(durlist):
         mmin,mmean,mmax,mstdev = UTIL.min_mean_max_stdev(durs)
         print '%2d         %3d  %7.3f   %7.3f   %7.3f   %7.3f' % \
               (rind, len(durs), mmin, mmean, mmax, mstdev)
      print

      if details:
         digs = gDEF_DEC_PLACES
         print '-- StimClass %s event durations:'  % self.name
         for rind, durs in enumerate(durlist):
            dstr = ['%.*f'%(digs, dd) for dd in durs]
            print '   run %02d: %s' % (rind, ' '.join(dstr))
         print

      print

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
      if across_runs:
         sc.durlist = random_duration_list(sc.nreps, sc.sclass, 0)
      else:
         sc.durlist = []
         for rind in range(nruns):
            dlist = random_duration_list(sc.nreps, sc.sclass, 0)
            sc.durlist.append(dlist)
      if verb > 2: sc.show_durlist_stats(details=(verb//4))

   return 0

def random_duration_list(nevents, tclass, total_time=-1.0, force_total=0):
   """create a length nevents list of durations according to the class

      if dur_mean > 0 and total_time > 0:
         warn if they imply different times (10%?)

      must have dur_mean >= 0 or total_time > 0

      if force_total: total_time overrides mean

      this function is not really returning error codes or whining
      (maybe that should change?)
   """

   if nevents <= 0:
      return []

   # ----------------------------------------------------------------------
   # quick check: if min == max >= 0, return list of those durations
   if tclass.min_dur >= 0 and tclass.min_dur == tclass.max_dur:
      return [tclass.min_dur] * nevents

   # other quick checks:
   #
   # INSTANT type is list of duration 0 events
   if tclass.dist_type == 'INSTANT':
      return [0.0] * nevents

   # fixed type is list of duration min_dur events
   if tclass.dist_type == 'fixed':
      return [tclass.min_dur] * nevents

   # ----------------------------------------------------------------------
   # reconcile total time (ttime) and min duration (min_dur),
   # then initialize a minimum time list,
   # then note remaining time and an appropriate max_dur
   if force_total and total_time >= 0:
      ttime = total_time
   elif tclass.mean_dur == 0:
      ttime = 0.0
   elif tclass.mean_dur > 0:
      ttime = 1.0 * tclass.mean_dur * nevents

      if total_time > 0:
         if (ttime - total_time)/total_time < 0.95 or \
            (ttime - total_time)/total_time > 1.05:
            print '** warning: random_duration_list has conflicting total time'
            print '   from mean = %s, from param = %s, type = %s' \
                  % (ttime, total_time, tclass.name)
   elif total_time >= 0:
      ttime = total_time
   else:
      # we do not know a mean or a total, try to return minimum
      print '** RDL: do not know mean or total time, returning min for %s' \
            % tclass.name
      min_dur = tclass.t_gran * math.ceil(tclass.min_dur/tclass.t_gran)
      return [min_dur] * nevents

   # if total time is zero, we are done
   if ttime == 0:
      return [0] * nevents

   # truncate ttime to t_gran grid (allow a little leeway)
   max_dur = ttime * math.floor(ttime/tclass.t_gran + 0.001)

   # quick check: if only 1 event, return
   if nevents == 1:
      return [ttime]

   # ----------------------------------------------------------------------
   # now note min duration
   min_dur = tclass.min_dur
   if min_dur <= 0: min_dur = 0

   # round min_dur up to nearest t_gran
   min_dur = tclass.t_gran * math.ceil(min_dur/tclass.t_gran)

   # initialize a list of minimums, and set remain to the remaining time
   remain = ttime - min_dur * nevents

   # if time is already maxed out by the minimum, just return it
   if remain < tclass.t_gran:
      return [min_dur] * nevents

   # get appropriate max remaining time (beyond min_dur)
   if tclass.max_dur > 0:
      # start by taking floor
      max_dur = tclass.t_gran * math.floor(tclass.max_dur/tclass.t_gran)
      max_dur = tclass.max_dur - min_dur
      if max_dur <= 0: # just bail (return minimums)
         return [min_dur] * nevents
      # if there is too much time left, everyone gets the max
      if remain >= (max_dur * nevents):
         return [tclass.max_dur] * nevents
   else:
      # no maximum
      max_dur = -1.0

   # ======================================================================
   # we have some time (remain) to distribute, possibly with a max time
   # ======================================================================

   # ----------------------------------------------------------------------
   # g_valid_dist_types = ['decay', 'uniform_rand', 'uniform_grid', 'INSTANT']

   # get list based on distribution type (INSTANT was done above)
   dtype = tclass.dist_type
   if dtype == 'decay':
      dlist = tclass.decay_get_dur_list(nevents, remain, max_dur)
   elif dtype == 'uniform_rand':
      dlist = tclass.urand_get_dur_list(nevents, remain)
   elif dtype == 'uniform_grid':
      dlist = tclass.ugrid_get_dur_list(nevents, remain)
   elif dtype == 'INSTANT' or dtype == 'fixed':
      print "** RDL: dist_type '%s' should not be processed here" % dtype
      return [0.0] * nevents
   else:
      print '** RDL: unknown dist type %s, return ave time' % dtype
      return [ttime*1.0/nevents] * nevents

   # ----------------------------------------------------------------------
   # add min_dur back in

   for dind in range(nevents):
      dlist[dind] += min_dur

   return dlist

if __name__ == '__main__':
   print '** this is not a main module'
   sys.exit(1)

