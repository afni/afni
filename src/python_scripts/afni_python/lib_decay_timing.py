#!/usr/bin/env python

# python3 status: working

import sys
# import module_test_lib
# g_testlibs = ['sys', 'math']
# if module_test_lib.num_import_failures(g_testlibs): sys.exit(1)
   
# import libraries
import math
# import afni_util as UTIL
# import lib_afni1D as LD

def decay_mean(a,b):
   """integral of xe^-x on [a,b] = (a+1)e^-a - (b+1)e^-b
   """
   return (a+1.0)*math.exp(-a) - (b+1.0)*math.exp(-b)

def decay_e3_Ex(a,b):
   """mean x (expected value of x) from PDF over [a,b]

      integral of xe^-x on [a,b]     (a+1)e^-a - (b+1)e^-b
      --------------------------  =  ---------------------
      integral of  e^-x on [a,b]          e^-a - e^-b

      Note: the denomiator scales e^-x values to probabilities,
            i.e.  e^-x / (e^-a - e^-b) is a PDF on [a,b].

      This is a @ a and quicly approaches the upper limit of a+1.0.
      On (0,L], this is 0 @ 0.0 and quickly approaches 1.0.
   """
   if a <  0.0: return 0
   if b <= a  : return a

   ea = math.exp(-a)
   eb = math.exp(-b)

   return ((a+1.0) * ea - (b+1.0) * eb) / (ea - eb)

def decay_e4_frac_L(L):
   """F(L) = 1/L * expected value of x = E(x)/L, for x in (0,L]
      E(x) = (1-(L+1)e^-L) / (1-e^-L) = 1 - L/(e^L - 1).
      So E4(x) = E(x)/L = 1/L - 1/(e^L - 1).

      This is the principle function we want to invert, using
      piecewise invertible approximations.

      Note: as L->0, E4(L) -> 0.5.
            if L>100, e^L ~= 10^43, so forget that term
   """

   if L < 0: return 0
   if L == 0: return 0.5
   # set an upper bound to avoid useless math.exp errors
   if L >= 100: return 1.0/L
   
   try: rv = 1.0/L - 1.0/(math.exp(L) - 1.0)
   except:
      print('decay_e4_frac_L failure for L = %f' % L)
      if L < 1.0: rv = 0.5
      else:       rv = 1.0/L
   return rv

def decay_e4_gen(A, B, step=0.1):
   """generater function for equation 4"""
   cur = A
   while cur <= B:
      yield decay_e4_frac_L(cur)
      cur += step


def decay_f1(L):
   """Linear approximation to F(L), f1(L) = 0.5 - 0.73*L
      This should be applied on [0,3].
   """
   b = 0
   t = 2
   if L < b or L > t:
      print('** decay_f1_lin(L) with L = %s outside [%s,%s]' % (L,b,t))
   return 0.5 - 0.1565*L/2.0

def decay_f2(L):
   """scaled 1/L approximation to F(L) on (2,3)

      f2(L) = 1 / (1.1174 + 0.8138 * L)
   """
   b = 2
   t = 3
   if L < b or L > t:
      print('** decay_f2(L) with L = %s outside [%s,%s]' % (L,b,t))
   return 1.0 / (1.6154 + 0.6478*L)

def decay_f3(L):
   """scaled 1/L approximation to F(L) on (3,6)

      f2(L) = 1 / (1.1174 + 0.8138 * L)
   """
   b = 3
   t = 6
   if L < b or L > t:
      print('** decay_f2(L) with L = %s outside [%s,%s]' % (L,b,t))
   return 1.0 / (1.1174 + 0.8138 * L)

def decay_f4(L):
   """1/L approximation to F(L) on [6,inf)

      f3(L) = 1/L
   """
   b = 6
   if L < b:
      print('** decay_f3(L) with L = %s outside [%s,inf)' % (L,b))
   return 1.0 / L

def decay_e4_approx(L):
   """if L in [0,3],   return 0.5 - .073*L
      if L in (3,6),   return 1.0/(1.1174 + 0.8138 * L)
      if L in [6,inf), return 1/L

      okay, those are old-fangled approximations, but you get the picture
   
      check continuity and key points...
      A(0)  = 0.5
      A(3)  = 0.281
      A(3+) = 0.281
      A(6-) = 1/6
      A(6+) = 1/6
   """
   if L <  0.0: return 0
   if L <= 2:   return decay_f1(L)
   if L <  3:   return decay_f2(L)
   if L <  6:   return decay_f3(L)
   if L >= 6:   return decay_f4(L)

def decay_e4_approx_gen(A, B, step=0.1):
   cur = A
   while cur <= B:
      yield decay_e4_approx(cur)
      cur += step

def decay_ident_gen(A, B, step=0.1):
   cur = A
   while cur <= B:
      yield cur
      cur += step

def decay_guess_inv(L):
   """VERY simple approximation to e4(x) as a line and 1/L
      that match at (0, 0.5), (4, 0.25).
      Inverse is 0.5 - L/16, for L <= 4, and 1/L for L > 4.
   """
   if L <  0: return 0.5
   if L <= 4: return 0.5 - L/16
   else:      return 1.0/L

def decay_guess(m):
   """VERY simple approximation to e4_inverse(x) inverting a line and 1/L
      that match at (0, 0.5), (4, 0.25).
      Apply inverse of 0.5 - L/16, for L <= 4, and 1/L for L > 4.

      For small m, use 1/m.  For m > 0.25, use linear approximation.
   """
   if m <= 0:    return 0.5
   if m <  0.25: return 1.0/m
   if m <  0.5:  return 8.0 - 16.0*m

   return 0.0

def decay_newton_step(fn, y_goal, x0, dx):
   """Solve with Newton's method.  Use inverse slope (delta x / delta  y)
      to scale change in y to change in x.
   """
   y0 = fn(x0)
   dy = fn(x0+dx) - y0
   if dy == 0.0:
      print('** decay_newton_step: called with dy == 0.0')
      return x0
   return x0 + (y_goal-y0) * dx/dy

def decay_solve(fn, y_goal, prec, maxind=100, verb=1):
   """find x s.t. |fn(x) - y_goal| < prec

      use linear search: x' = x0 + delta_y * dx/dy
   """
   x0 = decay_guess(y_goal)
   fx = fn(x0)
   if verb > 2: print('-- decay_solve x0 = %s, fx = %s' % (x0, fx))

   ind = 0
   while abs(fx - y_goal) > prec and ind < maxind:
      x = decay_newton_step(fn, y_goal, x0, prec)
      if x < 0.0:
         print('** apparent failure in decay_newton_step, x = %g' % x)
         return x0
      fx = fn(x)
      if verb > 2: print('   x0 = %s, x = %s, fx = %s' % (x0, x, fx))
      x0 = x

   if verb > 2:
      print('++ solved: given y = %g, approx with e^-%g = %g' \
            % (y_goal, x0, fx))

   return x0

# ======================================================================
# misc

def plot_data(pair_list, labs=[], y0=0):
   import numpy as N
   import matplotlib.pyplot as plt

   # a lot of work to include 0 in the y axis...
   if y0:
      mnx = min([pair[0][0] for pair in pair_list])
      mxx = max([pair[0][-1] for pair in pair_list])
      mxy = max([pair[1][0] for pair in pair_list])
      mxy1 = max([pair[1][-1] for pair in pair_list])
      mxy = max([mxy, mxy1])

   plt.figure("pizza")
   for ind, pair in enumerate(pair_list):
      if len(labs) > 0:
          plt.plot(pair[0], pair[1], label=labs[ind])
      else:
          plt.plot(pair[0], pair[1])
   if len(labs) > 0:
      plt.legend()

   # use default bounds, excecpt bound minimum y at 0
   if y0:
      bounds = [mnx, 1.1*mxx, 0, 1.1*mxy]
      print('== plot bounds %s' % bounds)
      plt.axis(bounds)

   plt.show()

def e_Lx(a,L,N):
   """return b such that int_a_b[e^-x] = 1/N * Exp[X]/N"""

   off = (1.0-math.exp(-L))/N   # full integral / N
   enb = math.exp(-a) - off     # must be e^-b
   return -math.log(enb)        # return b

def decay_show_PDF_times(L,N):
   """return a list of times distributed as e^-x on [0,L] such that
      the mean of the values matches the continuous mean

      To do so, partition [0,L] into N segments such that the integral over
      each segment is 1/N of the entire one, i.e., each integral would be
      p_int(L,N) = (1-e^-L)/N.
      Then on each such segment [a,b), find E[x], which would mean that 
      E[x] * (a-b) = p_int(L,N), and so their sum would equal E[x] on [0,L].
   """
   off = (1.0-math.exp(-L))/N
   
   a = 0
   sa = 0
   for ind in range(N):
      b = e_Lx(a, L, N)
      f = math.exp(-a) - math.exp(-b)  # should equal off
      ex = decay_e3_Ex(a,b)
      print('%3s %0.6f  off=%0.6f, f=%0.6f, E(x)=%0.6f' % (ind, b, off, f, ex))
      sa += ex
      a = b
   print('length L=%s, theor mean/L = %s, sa/N/L = %s' \
         % (L, decay_e3_Ex(0,L)/L, sa/N/L))

def decay_get_PDF_times(L,N):
   """return a list of times distributed as e^-x on [0,L] such that
      the mean of the values matches the continuous mean

      To do so, partition [0,L] into N segments such that the integral over
      each segment is 1/N of the entire one, i.e., each integral would be
      p_int(L,N) = (1-e^-L)/N.
      Then on each such segment [a,b), find E[x], which would mean that 
      E[x] * (a-b) = partition_integral(L,N) over [a,b], and so their sum
      would equal E[x] on [0,L].
   """
   if L <= 0 or N <= 0: return []

   off = (1.0-math.exp(-L))/N

   times = []
   
   a = 0
   for ind in range(N):
      b = e_Lx(a, L, N)
      ex = decay_e3_Ex(a,b)
      times.append(ex)
      a = b
   return times

def decay_get_PDF_bins(vals, nbin, scale=1, verb=1):
   """to evaluate, get lots of PDF vals and bin them to see if they
      follow e^-x"""
   N = len(vals)
   if nbin <= 0 or N <= 0:
      print('** get_PDF_bins: inputs must be positive')
      return []

   if verb > 2: print('PDF_times: min = %s, max = %s' % (vals[0],vals[-1]))

   bcounts = [0] * nbin
   v0 = min(vals)
   bsize = (max(vals)-min(vals))/float(nbin)
   if bsize <= 0: return []

   bover = 0
   for v in vals:
      bind = int((v-v0)/float(bsize))
      # check whether we overstep out bounds
      if bind >= nbin:
         bover += 1
         bind = nbin - 1
      bcounts[bind] += 1

   if bover > 0 and verb > 1:
      print('== get_PDF_bins: bover = %d' % bover)

   if scale:
       b0 = float(max(bcounts))
       for bind in range(nbin):
          bcounts[bind] /= b0
   return bcounts

def show_times_PDF(L,N,nbin, y0=0):
   """get_PDF_times, count the number in each bin of length L/nbin, and
      get list of count/N
   """
   times = decay_get_PDF_times(L, N)
   btimes = decay_get_PDF_bins(times, nbin)
   if len(btimes) < 1: return

   bsize = L*1.0/nbin

   xo = [i * bsize for i in range(nbin)]
   yo = [math.exp(-(i*bsize)) for i in range(nbin)]
   
   plot_data([[xo,btimes], [xo,yo]], labs=['btimes', 'e^-x'], y0=y0)

def plot_pdf_from_file(fname, nbin, scale=1, L=0):
  
   import lib_textdata as TD
   data = TD.read_1D_file(fname)[0]
   data.sort()
   show_val_bins(data, nbin, scale=scale, L=L)

def show_val_bins(vals, nbin, scale=1, L=0, verb=1, y0=0):
   """get_PDF_times, count the number in each bin of length L/nbin, and
      get list of count/N
   """
   N = len(vals)
   if N < nbin or nbin <= 0: return

   btimes = decay_get_PDF_bins(vals, nbin, scale=scale, verb=verb)
   if len(btimes) < 1: return

   # bsize = 1.00001*(vals[-1]-vals[0])/float(nbin)
   bsize = (vals[-1]-vals[0])/float(nbin)

   print('== SVB: btimes[0,1,-1] = %f, %f, %f'%(btimes[0],btimes[1],btimes[-1]))
   print('        nbin %d, bsize %f' % (nbin, bsize))

   xo = [i * bsize for i in range(nbin)]
   dlist = [[xo, btimes]]
   labs = ['btimes']

   if scale:
      # include e^-x
      if L == 0:
         xscale = 1.0
      else:
         # L < 0 means we should flip the exp distribution
         xscale = abs(L)/float(vals[-1]-vals[0])
         if L > 0: ilist = range(nbin)
         else:     ilist = range(nbin-1,-1,-1)
      # yo = [math.exp(-(i*bsize*xscale)) for i in range(nbin)]
      yo = [math.exp(-(i*bsize*xscale)) for i in ilist]
      dlist.append([xo, yo])
      labs.append('e^-x')
   
   plot_data(dlist, labs=labs, y0=y0)

# ======================================================================
# main calling routine

def decay_pdf_get_ranged_times(A, B, M, N, t_grid=0.001, verb=1):
   """Return N times between A and B, with mean M and following a scaled
      version of PDF e^x.

      a. preparation:

         -  truncate A, B onto t_grid (multiples of it)
         -  require 0 <= A < B, N > 3, A < M < B
         -  define F(L) = 1/L - 1/(e^L - 1)
         -  if (M-A)/(B-A) ~= 0.5, suggest uniform distribution, instead

      b. given m = (M-A)/(B-A), solve m = F(L) for L
         - if m > 0.5: process as 1-m, and apply as B-time rather than A+time
      c. get N decay times from e^-x on [0,L] with mean m
      d. reflect if necessary (if orig m>0.5), and scale to [A,B] with mean M
      e. round to t_grid

      Note: this does not need to require A >= 0, except in the context of
            event durations.  Consider allowing A < 0 with a flag.

      Note: times are currently sorted, not randomized.
   """

   # ----------------------------------------------------------------------
   # a. preaparation

   if verb > 1:
      print('-- decay_pdf_GRT: A=%s, M=%s, B=%s, N=%s, t_grid=%s' \
            % (A, M, B, N, t_grid))
   
   # truncate times inward (allow for slightly missing multiple of t_grid)
   A = truncate_to_grid(A, t_grid, up=1)
   B = truncate_to_grid(B, t_grid, up=0)

   # this also checks if m is too close to 0.5
   if not decay_pdf_inputs_ok(A, B, M, N, t_grid):
      return []


   # ----------------------------------------------------------------------
   # b. from m = fractional mean, get L to restrict e^-x over [0,L],
   #    so the expected value of x given PDF e^-x is m

   # set m, and note whether to reflect results about the mean
   m = (M-A)/float(B-A)
   reflect = (m > 0.5)
   if reflect:
      m = 1 - m
   
   # note: F(L) = decay_e4_frac_L(L) = 1/L - 1/(e^L - 1)
   L = decay_solve(decay_e4_frac_L, m, t_grid/1000.0, verb=verb)

   if verb > 1: print('-- decay: m = %s, L = %s' % (m, L))

   # ----------------------------------------------------------------------
   # c. get N decay times from e^-x on [0,L] with mean m
   times = decay_get_PDF_times(L, N)
   if verb > 3: decay_show_PDF_times(L, N)
   if times == []: return times

   # ----------------------------------------------------------------------
   # d. reflect if necessary, and scale to [A,B] with mean M
   if reflect:
      # times are in [0,L], so reflecting means applying L-time
      times = [L-t for t in times]
      m = 1-m  # do we care anymore?

   # scale t from [0,L] to [0,B-A] and offset by A
   lrat = (B-A)/float(L)
   times = [A + t*lrat for t in times]

   # ----------------------------------------------------------------------
   # e. round times to multiples of t_grid
   times = cumulative_round_to_grid(times, t_grid)

   return times

def cumulative_round_to_grid(vals, delta, verb=1):
   """round values to multiples of delta, but try to keep the cumulative sum
      basically unchanged

      * apply in place

      Maintain CS = cumulative sum of differences between the exact and rounded
      times.  If the new abs(CS) >= prec (precision), round in the opposite
      direction, to maintain abs(CS) < prec, which should be sufficient.  We
      *could* require abs(CS) < prec/2, but that might mean more would be
      rounded in the "wrong" direction.  Please discuss amongst yourselves...
   """
   dsum = 0.0   # cumulative sum of diffs between original and rounded vals
   for ind, val in enumerate(vals):
      rval = round(val/delta)*delta
      rdiff = rval-val
      if verb > 3: print('-- CR2G: val %d = %s, rdiff = %s'%(ind, val, rdiff))
      if rdiff == 0.0:
         continue

      # keep from straying
      if abs(dsum + rdiff) > delta:
         if rdiff > 0: rd = rdiff-delta
         else:         rd = rdiff+delta
         if verb>3: print('   alter rdiff %s to %s, dsum %s' % (rdiff,rd,dsum))
         rdiff = rd
         rval = val+rdiff

      # apply
      vals[ind] = rval
      dsum += rdiff

      if verb > 3:
         print('   val %s, rval %s, rdiff %s, dsum %s' % (val,rval,rdiff,dsum))

   return vals

def decay_pdf_inputs_ok(A, B, M, N, t_grid):
   """check all inputs for reasonableness, return 1 if okay, 0 otherwise"""
   fname = 'decay_pdf_get_ranged_times'
   if t_grid <= 0:
      print('** %s: illegal t_grid < 0, %s' % (fname, t_grid))
      return 0

   if A < 0.0 or A >= B:
      print('** %s requires 0 <= A < B' % fname)
      print('   have A = %s, B = %s' % (A,B))
      return 0

   if N <= 3:
      print('** %s requires N > 3, have N = %d' % (fname, N))
      return 0

   if A >= M or B <= M:
      print('** %s requires A < M < B, have M = %s' % (fname, M))
      return 0

   # fractional m should not be too close to 0.5
   m = (M-A)/float(B-A)
   if abs(m - 0.5) < 0.001:
      print('** decay PDF mean too close to midpoint, consider uniform PDF')
      print('** FAILING for mean m = %s' % m)
      return 0

   return 1

def truncate_to_grid(val, grid, up=1):
   """return val truncated to a multiple of grid

      if up, val should not decrease, else it should not increase
      BUT, allow for val to *almost* be a multiple of grid
         - allow for missing by 0.01*grid
         - e.g. T2G(val=5.32101, grid=0.001, up=1) should return 5.321,
           not 5.322, since it is close enough, say

      require grid > 0
   """
   if grid <= 0.0: return 0.0

   if up:
      return math.ceil(val/grid-0.01)*grid
   else:
      return math.floor(val/grid+0.01)*grid


# ======================================================================
# demonstration functions
# ======================================================================

def demo1(A=3, nd=50):
   print(decay_mean(A,A+1))
   print(decay_mean(A,A+2))
   for ind in range(nd):
      print(decay_mean(A,A+(float(nd)-ind)/nd))
   return 0

def demo_plot_bins(A=0, B=4, M=1, N=10000, t_grid=0.001, verb=3, y0=0):
   tlist = decay_pdf_get_ranged_times(A,B,M,N,t_grid=t_grid,verb=verb)

   min_val = min(tlist)
   max_val = max(tlist)
   sum_val = sum(tlist)
   print('++ final stats:  min %.4f, mean %.4f, max %.4f' \
         % (min_val, sum_val/float(N), max_val))
   print('                 sum %.4f, expect %.4f' % (sum_val, N*M))

   m = (M-A)/float(B-A)
   if m < 0.5:
      L = decay_solve(decay_e4_frac_L, m, t_grid, verb=0)
   else:
      L = decay_solve(decay_e4_frac_L, 1-m, t_grid, verb=0)
      L = -L
   print('-- plotting... L = %s' % L)
   show_val_bins(tlist, 100, scale=1, L=L, verb=2, y0=y0)

   return 0
      
def demo_plot_e4_N_inv(A=0, B=10, step=0.1):
   gen = decay_e4_gen
   orig = [v for v in gen(A,B,step=step)]

   gen = decay_e4_approx_gen
   approx = [v for v in gen(A,B,step=step)]

   #gen = decay_FM_approx_inv_gen
   #inv = [v for v in gen(0.1,0.499,step=0.001)]

   xo = [v for v in decay_ident_gen(A,B,step=step)]
   #xi = [v for v in decay_ident_gen(0.1,0.499,step=0.001)]
   yo = [decay_guess_inv(v) for v in xo]

   # get inverses of all orig y values
   xi = [decay_solve(decay_e4_frac_L, v, 0.0001) for v in orig]

   # plot_data([[xo,orig], [xo, approx], [inv, xi]])
   # plot_data([[xo,orig], [xo, approx], [xo,yo], [xi, orig]],
   #           labs=['e4', 'approx','N', 'inv'])
   plot_data([[xo,orig], [xo,yo], [xi, orig]], labs=['e4', 'N', 'inv'])

   return 0

def run_demo(demo_ind):

   print('== running demo #%d ...\n' % demo_ind)

   rv = 0

   if demo_ind == 1:
      rv = demo1()

   elif demo_ind == 2:
      show_times_PDF(5,10000,100)
      rv = 0

   elif demo_ind in [3,4,5,6]:
      if   demo_ind == 4: rv = demo_plot_bins(M=1.95, y0=1)
      elif demo_ind == 5: rv = demo_plot_bins(M=2.05, y0=1)
      elif demo_ind == 6: rv = demo_plot_bins(M=3,    y0=1)
      else:               rv = demo_plot_bins()

   elif demo_ind == 7:
      rv = demo_plot_e4_N_inv()

   else:
      print('** no such demo')
      rv = 1

   return rv

# *** note the nubmer of demos
g_num_demos = 7

# end demo functions
# ======================================================================

def main():
   # for testing, consider options to plot or compute times, etc
   # (those won't exist at the higher level, in MRT.py)

   # for now, just run fixed demos

   demo_ind = 3

   if len(sys.argv) > 1:
      if sys.argv[1][0:2] == '-h':
         print('you cannot get help here, but please get help somewhere')
         print('okay, this can be passed a demo index starting at 1,')
         print('or a negative N, to do 1..N, or 0 to do all')
         return 0
      else:
         try: demo_ind = int(sys.argv[1])
         except: demo_ind = 2

   # if zero, set to do all
   if demo_ind == 0:
      demo_ind = -g_num_demos

   if demo_ind > 0:
      run_demo(demo_ind)
   else:
      demo_ind = -demo_ind
      print('== running all demos from 1 .. %d' % (demo_ind))
      for dind in range(demo_ind):
         if run_demo(dind+1):
            return 1

   return 0

# main
if __name__ == '__main__':
   sys.exit(main())
