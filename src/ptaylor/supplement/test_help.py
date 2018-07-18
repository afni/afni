#!/usr/bin/env python2.7 

# put an encoded string of ints into a text file

import afni_util as au
import sys       as sys
import numpy     as np

if len(sys.argv) - 4 :
    print "\n\nBAD! Need 3 arguments!\n\n"
    sys.exit(1)

ofile = sys.argv[1]
N = int(sys.argv[2])
f = float(sys.argv[3])

x = np.arange(N)
L = int(f*N)

np.random.shuffle(x)
y = list(x[:L])
y.sort()

enums = au.encode_1D_ints( y )

print enums

f = open(ofile, 'w')
f.write(enums+'\n')
f.close()
