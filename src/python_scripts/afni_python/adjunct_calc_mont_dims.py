#!/usr/bin/env python

# written by PA Taylor (NIMH, NIH), Feb 2017
#
# find out a good ratio to make montage dimensions; if necessary, add
# some to get closer to golden ratio, but we have a reasonable
# tolerance, as well.

### [PT: July 17, 2018] CONVERTED__python__2to3

### [PT: July 25, 2018] 
# + fixed upper limit to allow square values to be found.
#
### [PT: Dec 5, 2018] 
# + reduce depencies:  no scipy or numpy needed anymore
#
# --------------------------------------------------------------------

import sys as sys

help_string = '''
Just a simple helper function for the fat_proc* scripts.  

Nuthin' to see here, folks!
'''

GOLD = 1.618033988749895 # golden_ratio

#MAX_NCOL = 8

check_dist = 4
OKOK_dist  = 0.4   # allowable distance, to minimize adding

def get_arg(aa):
    Narg = len(aa)
    if Narg < 2:
        print(help_string)
        sys.exit(0)
    elif aa[1] == '-help':
        print(help_string)
        sys.exit(0)
    elif Narg == 2:
        sys.exit("** ERROR: too few args! Need exactly one integer\n"
                 "   and an output filename (and one optional integer\n"
                 "   the max num of columns output).")
    elif Narg > 4:
        sys.exit("** ERROR: too many args! Need exactly one integer\n"
                 "   and an output filename (and one optional integer\n"
                 "   the max num of columns output).")
    else:
        aaint = int(aa[1])
        apref = aa[2]
        print("++ OK, have my integer argument:", aaint)

    return aaint, apref

def get_facs(AA):
    AA_facs1 = []
    AA_facs2 = []
    # [PT: Dec 5, 2018] new def of this val, no numpy used; also
    # adjust upper limit of fac_ceil bc of this
    fac_ceil = int(AA**0.5) + 1 
    for i in range(1, fac_ceil):
        if not(AA % i): 
            AA_facs1.append(i)
            AA_facs2.append(AA // i)

    return AA_facs1, AA_facs2

def calc_ratios(AA_h, AA_l):
    La = len(AA_h)
    AA_rat = []
    for i in range(La):
        AA_rat.append( float(AA_h[i])/float(AA_l[i]) )
    return AA_rat

def calc_rat_dist_from_gold(x):
    y = float(x)/GOLD
    return abs(y-1.)

def find_best_rat( AA_rat ):
    dist = 10.**10
    idx = -1
    La = len(AA_rat)
    for i in range(La):
        rr = AA_rat[i]
        dd = calc_rat_dist_from_gold(rr)
        if dd < dist :
            idx = i
            dist = dd

    if idx < 0:
        sys.exit("**ERROR! negative index in find_best_rat()")

    return i, dist

# now use lists instead of arrays
def get_best_rat(AA):

    idx_fin = -1
    min_dist = 10.**10
    all_rats = [0] * check_dist  # np.zeros(check_dist)
    all_vals = []                # np.zeros((check_dist,3), dtype=int)
    for i in range(check_dist):
        all_vals.append([0]*3) 

    for j in range(check_dist):
        (AA_l, AA_h) = get_facs(AA+j)
        (idx, dist) = find_best_rat( calc_ratios(AA_h, AA_l) )
        print(AA_l)
        print(AA_h)
        print(idx, AA_h[idx], AA_l[idx], dist)
        all_rats[j] = dist
        all_vals[j][0] = idx
        all_vals[j][1] = AA_h[idx]
        all_vals[j][2] = AA_l[idx]
        if all_rats[j] < min_dist:
            idx_fin = j
            min_dist = all_rats[j]
        # put this in as a "good enough" ratio, even if not ideal
        # within dist, so we don't needlessly add vols
        if min_dist < OKOK_dist:
            print("++ OK! Good enough")
            break

    if idx < 0:
        sys.exit("**ERROR! negative index in find_best_rat()")

    return AA+idx_fin, all_vals[idx_fin]

''' # old version: used numpy/arrays
def get_best_rat(AA):

    idx_fin = -1
    min_dist = 10.**10
    all_rats = np.zeros(check_dist)
    all_vals = np.zeros((check_dist,3), dtype=int)

    for j in range(check_dist):
        (AA_l, AA_h) = get_facs(AA+j)
        (idx, dist) = find_best_rat( calc_ratios(AA_h, AA_l) )
        print(AA_l)
        print(AA_h)
        print(idx, AA_h[idx], AA_l[idx], dist)
        all_rats[j] = dist
        all_vals[j, 0] = idx
        all_vals[j, 1] = AA_h[idx]
        all_vals[j, 2] = AA_l[idx]
        if all_rats[j] < min_dist:
            idx_fin = j
            min_dist = all_rats[j]
        # put this in as a "good enough" ratio, even if not ideal
        # within dist, so we don't needlessly add vols
        if min_dist < OKOK_dist:
            print("++ OK! Good enough")
            break

    if idx < 0:
        sys.exit("**ERROR! negative index in find_best_rat()")

    return AA+idx_fin, all_vals[idx_fin, :]
'''

if __name__=="__main__":

    print("++ Command line:", sys.argv)

    (AA, Apref)     = get_arg(sys.argv)
    (Afinal, Arats) = get_best_rat(AA)

    Apad = Afinal - AA
    Acol = Arats[1]
    Arow = Arats[2]

    print("\n---------------------------------------------")
    print("++ final Nslices = "+str(Afinal)+" (pad = "+str(Apad)+")")
    print("++ -> Ncol x Nrow = "+str(Acol)+" x "+str(Arow))
    print("---------------------------------------------\n")

    f = open(Apref, 'w')
    f.write('# %10s %10s %10s %10s\n' % ("Final", "Pad", "Ncol", "Nrow"))
    f.write('  %10d %10d %10d %10d' % (Afinal, Apad, Acol, Arow))

    f.close()
    sys.exit(0)
