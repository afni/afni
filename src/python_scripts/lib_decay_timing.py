#!/usr/bin/env python

# ===========================================================================
# functions for defining decay curves
# 
# Notes:
# Given A,M,B,N = desired min,mean,max for list of N (pseudo-randomly ?)
#    generated numbers that follow a decay type distribution (e^-x),
#    generate such a list.
#  
# Basic equations:
#  
#    (1) integral [e^-x] on (a,b) = e^-a - e^-b
#    (2) integral [xe^-x] on (a,b) = (a+1)e^-a - (b+1)e^-b
#    (3) E[x] = int[xe^-x] / int[e^-x] on (a,b) = (a+1)e^-a - (b+1)e^-b
#                                                 ---------------------
#                                                     e^-a - e^-b

#    Since E[x] on [0,L] = (1-(L+1)e^-L)/(1-e^-L) = 1 - L/(e^L - 1), define
#
#    (4) F(L) = E[x]/L = 1/L - 1/(e^L - 1)
#
# * This is the principle equation we want to invert.  Given mean M, converted
#   to a fractional offset mean m = (M-A)/(B-A), we can solve m = F(L) for L,
#   defining a decay function with the given fractional mean.  So if e^-x on
#   [0,L] has mean m, x in the function can be shifted and scaled to be on
#   [A,B] with the given mean M.
#   
#
# a) Approximation games...
#
#    Given A,M,B, find L such that E[x]/L = (M-a)/(b-a).
#    So if m is the fractional mean on [a,b], solve m = F(L) = E[x]/L for m.
#  
#    F(L) can be reasonably well approximated by invertible functions on
#    (0,3], (3,6) and [6,inf), being linear, scaled 1/L and 1/L.
#  
#          F(0) -> 0.5     (limit as L->0 of F(L))
#          F(3) ~= 0.281
#          F(6) ~= 0.164 ~= 1/6 (not great, consider using F(8)=.125)
#  
#       i) on (0,3], approximate F(L) with line:
#  
#          F1(L)  = 0.5 - 0.73*L
#          F1I(m) = (0.5 - m)/0.73
#  
#       ii) on (3,6), map 3 -> 1/f1(3) = 3.5587,  map 6 -> 6
#           can use F2_0(x) = 1.1174 + .8138*x
#           So define F2(x) = 1/F2_0(x).
#  
#          F2(L)  = 1 / (1.1174 + 0.8138 * L)
#          F2I(L) = (1.0/m - 1.1174) / 0.8138
#  
#       iii) on [6,inf), approximate F(L) with 1/L
#  
#          F3(L)  = 1/L     (is invertible and is its own inverse)
#          F3I(L) = F3(L)
#
#
#    Better approximation:
#
#       on [0,2]   f1(L) = 0.5 - 0.1565*L/2.0
#       on (2,3]   f2(L) = 1.0 / (1.6154 + 0.6478*L)
#       on [3,6)   f3(L) = 1.0 / (1.1174 + 0.8138 * L)
#       on [6,inf) f4(L) = 1.0 / L
#  
# b) Forget approximations.
#
#    F(L) is very smooth, behaving like a line for L<2 and then scaled
#    versions of 1/L beyond that.  So iterate to invert, which should be
#    quick and have any desired accuracy.
#
#    Find a very approximate solution, then apply Newton's method where
#    the next iteration of x1 uses the preivious one and the approximate
#    slope at x0, slope ~= (f(x+h)-f(x))/h for small h.
#
#    i) define basic approximation by pivot at L=4 as initial guess
#
#          1.0/m , if m <= 0.25
#          8-16*m, if 0.25 < m < 0.5
#
#    ii) define a step to get the next iteration value
#
#       Use inverse slope (delta x / delta  y) to scale change in y to
#       change in x.
#
#          decay_newton_step(fn, y_goal, x0, dx):
#             y0 = fn(x0)
#             dxdy = dx / (fn(x0+dx) - y0)
#             return x0 + (y_goal-y0) * dxdy
#
#    iii) solve by taking initial guess and iterating until close
#
#       find x s.t. fn(x) ~= y_goal, where |fn(x) - y_goal| < prec
#
#          decay_solve(fn, y_goal, prec, maxind=100):
#             x = decay_guess(y_goal)
#             f = fn(x0)
#             while abs(f - y_goal) > prec
#                x = decay_newton_step(fn, y_goal, x, prec)
#                f = fn(x)
#
# Do not forget to range check m.  If m <= 0 or m == 0.5, use uniform dist.
# If m > 0.5, solve using 1-m and reflect the resulting values about the mean.
#
# c) Get a list of exact durations that follow the PDF and have mean m.
#
#    This can still apply to [0,inf) with fractional mean m in (0,0.5).
#
#       i)   break
#
# d) Scale, truncate and adjust the times.
#
#    Now that inversion is done, given A, M, B, N, apply it to find L
#    from (M-A)/(B-A) and map 0->L to A->B to get mean M.  Use this to
#    get N values following that PDF with a mean of M.
#
# e) Truncate and adjust the times.
#



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
   """

   if L < 0: return 0
   if L == 0: return 0.5
   return 1.0/L - 1.0/(math.exp(L) - 1.0)

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
   """Use inverse slope (delta x / delta  y) to scale change in y to
      change in x, to solve with Newton's method.
   """
   y0 = fn(x0)
   dxdy = dx / (fn(x0+dx) - y0)
   return x0 + (y_goal-y0) * dxdy

def decay_solve(fn, y_goal, prec, maxind=100):
   """find x s.t. fn(x) ~= y_goal, where |fn(x) - y_goal| < prec

      use linear search: x' = x0 
   """
   x0 = decay_guess(y_goal)
   f0 = fn(x0)

   ind = 0
   while abs(f0 - y_goal) > prec and ind < maxind:
      x = decay_newton_step(fn, y_goal, x0, prec)
      fx = fn(x)
      print('x0 = %s, x1 = %s, f0 = %s, f1 = %s' % (x0, x, f0, fx))
      x0 = x
      f0 = fx

   return x0

# ======================================================================
# misc

def plot_data(pair_list, labs=[]):
   import numpy as N
   import matplotlib.pyplot as plt

   plt.figure("pizza")
   for ind, pair in enumerate(pair_list):
      if len(labs) > 0:
          plt.plot(pair[0], pair[1], label=labs[ind])
      else:
          plt.plot(pair[0], pair[1])
   if len(labs) > 0:
      plt.legend()
   plt.show()

def e_Lx(a,L,N):
   """return b such that int_a_b[e^-x] = 1/N * int_0_L[e^-x]"""

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
      print '%3s %0.6f  off=%0.6f, f=%0.6f, E(x)=%0.6f' % (ind, b, off, f, ex)
      sa += ex
      a = b
   print 'length L=%s, theor mean = %s, sa/N = %s' % (L, decay_e3_Ex(0,L), sa/N)

def decay_get_PDF_times(L,N):
   """return a list of times distributed as e^-x on [0,L] such that
      the mean of the values matches the continuous mean

      To do so, partition [0,L] into N segments such that the integral over
      each segment is 1/N of the entire one, i.e., each integral would be
      p_int(L,N) = (1-e^-L)/N.
      Then on each such segment [a,b), find E[x], which would mean that 
      E[x] * (a-b) = p_int(L,N), and so their sum would equal E[x] on [0,L].
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

def decay_get_PDF_bins(L, N, nbin, verb=0):
   """to evaluate, get lots of PDF times and bin them to see if they
      follow e^-x"""
   if nbin <= 0 or N <= 0 or L <= 0:
      print('** get_PDF_bins: inputs must be positive')
      return []

   times = decay_get_PDF_times(L,N)
   bcounts = [0] * nbin
   bsize = L*1.0/nbin

   if verb: print('PDF_times: min = %s, max = %s' % (times[0],times[-1]))

   for tt in times:
      bcounts[int(tt*1.0/bsize)] += 1
   b0 = float(bcounts[0])
   for bind in range(nbin):
      bcounts[bind] /= b0
   return bcounts

def show_times_PDF(L,N,nbin):
   """get_PDF_times, count the number in each bin of length L/nbin, and
      get list of count/N
   """
   btimes = decay_get_PDF_bins(L,N,nbin)
   if len(btimes) < 1: return

   bsize = L*1.0/nbin

   xo = [i * bsize for i in range(nbin)]
   yo = [math.exp(-(i*bsize)) for i in range(nbin)]
   
   plot_data([[xo,btimes], [xo,yo]], labs=['btimes', 'e^-x'])


# ======================================================================
def main():
   L = 10
   step = 0.1

   if 0:
      A = 3
      nd = 50
      print decay_mean(A,A+1)
      print decay_mean(A,A+2)
      for ind in range(nd):
         print decay_mean(A,A+(1.0*nd-ind)/nd)
   elif 0:
      show_times_PDF(5,10000,100)

   elif 1:
       A = 0; B = 10; step = 0.1

       gen = decay_e4_gen
       orig = [v for v in gen(A,B,step=step)]

       gen = decay_e4_approx_gen
       approx = [v for v in gen(A,B,step=step)]

       #gen = decay_FM_approx_inv_gen
       #inv = [v for v in gen(0.1,0.499,step=0.001)]

       xo = [v for v in decay_ident_gen(A,B,step=step)]
       #xi = [v for v in decay_ident_gen(0.1,0.499,step=0.001)]
       yo = [decay_guess_inv(v) for v in xo]

       # plot_data([[xo,orig], [xo, approx], [inv, xi]])
       plot_data([[xo,orig], [xo, approx], [xo,yo]], labs=['e4', 'approx','N'])


# main
if __name__ == '__main__':
   sys.exit(main())
