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
#       on [0,2]   0.5 - 0.1565*L/2.0
#       on (2,3]   1.0 / (1.6154 + 0.6478*L)
#       on [3,6)   1.0 / (1.1174 + 0.8138 * L)
#       on [6,inf) 1.0 / L
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
# decay_step(fn, x_goal, x0, prec):
#    f0 = fn(x0)
#    m = prec / (fn(x0+prec) - f0)
#    return x0 + (x_goal-f0) * m
#
#


import sys
# import module_test_lib
# g_testlibs = ['sys', 'math']
# if module_test_lib.num_import_failures(g_testlibs): sys.exit(1)
   
# import libraries
import math
# import afni_util as UTIL
# import lib_afni1D as LD

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

def decay_f1i(m):
   """Inverse of linear approximation to F(L), f1(L) = 0.5 - 0.73*L.

      f1i(m) = (0.5 - m)/0.73
   """
   return (0.5 - m)/0.73

def decay_f2(L):
   """scaled 1/L approximation to F(L) on (2,3)

      f2(L) = 1 / (1.1174 + 0.8138 * L)
   """
   b = 2
   t = 3
   if L < b or L > t:
      print('** decay_f2(L) with L = %s outside [%s,%s]' % (L,b,t))
   return 1.0 / (1.6154 + 0.6478*L)

def decay_f2i(m):
   """Inverse of scaled 1/L approximation to F(L) on (3,6).

      f2i(m) = (1.0/m - 1.1174) / 0.8138
   """
   if m <= 0: return 0
   return (1.0/m - 1.1174) / 0.8138

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

def decay_f4i(m):
   """Inverse of 1/L approximation to F(L) on [6,inf).

      f3i(m) = 1.0/L

      written for completeness
   """
   if m <= 0: return 0
   return 1.0/L


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

def decay_e4_approx_inv(m):
   """stepwise inverse of decay_FM_approx

      Given fractional mean m, return limit L, such that the mean x
      with PDF e^-x on interval [0,L] is m/L.

      The resulting [0,L] interval can then be scaled 

          m <= 0      , ILLEGAL (use constant minimum?)
          m <= 1/6    , invert 1/L
          m <  0.281  , invert 1.0/(1.1174 + 0.8138*L)
                        I(m) = (1.0/m - 1.1174) / 0.8138
          m <  0.5    , invert 0.5 - .073*L
                        I(m) = (0.5 - m)/0.073
          m == 0.5    , ILLEGAL (use uniform)
          m >  0.5    , ILLEGAL (use 1-m and flip)
          m >= 1.0    , ILLEGAL (use constant maximum)
   """
   if m <= 0.0 or m >= 0.5:
      print('** decay_e4_approx_inv: illegal fraction mean %s' % m)
      return 0
   if m <= 1.0/6: return 1.0/m
   if m <  0.281: return (1.0/m - 1.1174) / 0.8138
   if m <  0.5:   return (0.5 - m)/0.073

def decay_e4_approx_inv_gen(A, B, step=0.1):
   cur = A
   while cur <= B:
      yield decay_e4_approx_inv(cur)
      cur += step

def decay_ident_gen(A, B, step=0.1):
   cur = A
   while cur <= B:
      yield cur
      cur += step

def decay_guess(m):
   """VERY simple approximation to e4(x), going after endpoints and 1/x"""
   if m <= 0: return 0.5
   if m < 4 : return 0.5 -0.0625 * m
   return 1.0/m

def decay_step(fn, val, x0, prec):
   f0 = fn(x0)
   m = prec / (fn(x0+prec) - f0)
   return x0 + (val-f0) * m

def decay_solve(fn, val, prec, maxind=100):
   """find x s.t. fn(x) ~= val, where |fn(x) - val| < prec

      use linear search: x' = x0 
   """
   x0 = decay_guess(val)
   f0 = fn(x0)

   ind = 0
   while abs(f0 - val) > prec and ind < maxind:
      x = decay_step(fn, val, x0, prec)
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
   off = (1.0-math.exp(-L))/N
   diff = math.exp(-a) - off
   return -math.log(diff)

def show_int(L,N):
   off = (1.0-math.exp(-L))/N
   
   a = 0
   sa = 0
   for ind in range(N):
      b = e_Lx(a, L, N)
      f = math.exp(-a) - math.exp(-b)  # should equal off
      ex = decay_mean(a,b)
      print '%3s %0.6f  off=%0.6f, f=%0.6f, E(x)=%0.6f' % (ind, b, off, f, ex)
      sa += ex
      a = b
   print 'length L=%s, theor mean = %s, sa/N = %s' % (L, decay_mean_frac(L), sa/N)

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
      show_int(4, 10)
      show_int(4, 25)
      show_int(4, 50)

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
       yo = [decay_guess(v) for v in xo]

       # plot_data([[xo,orig], [xo, approx], [inv, xi]])
       plot_data([[xo,orig], [xo, approx], [xo,yo]], labs=['e4', 'approx','N'])


# main
if __name__ == '__main__':
   sys.exit(main())
