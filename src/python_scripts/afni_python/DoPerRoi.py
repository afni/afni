#!/usr/bin/env python
import os, sys, string
import afni_base
from pydoc import help

test = 1

#define rules for options
oplist = []
op = afni_base.comopt('-areas', -2, \
   ["CA_N27_MPM", "area_6", "area_4a", "area_4p"])
oplist.append(op)
op = afni_base.comopt('-areas_2', -2, \
   ["CA_ZOUZOU", "areea_6", "areea_4a", "areea_4p"])
oplist.append(op)
op = afni_base.comopt('-dsets', -1, [])
oplist.append(op)
op = afni_base.comopt('-sides', -1, ['left', 'right'])
oplist.append(op)
op = afni_base.comopt('-command', -1, [])
oplist.append(op)


opts = afni_base.getopts2(sys.argv, oplist)
if opts == None:
   sys.exit()
afni_base.show_opts2(opts)

#find keys with -areas_
areas = []
for key in opts.keys():
   if key.find('-areas') > -1:  #here's one set of areas
      areas.append(key) 

#Now do the deed
for ak in areas:
   op = opts[ak]  #Get the areas_* option
   atlas = op.parlist[0]
   alist = op.parlist[1:]  #the areas
   for side in opts['-sides'].parlist:
      for ar in alist:
         com = "rm -f %s_%s* >& /dev/null" % (ar, side)
         if test == 0:
            so, se = afni_base.shell_exec(com)
         else:
            print com
         com = "3dcalc -a %s:%s:%s -expr 'step(a)' -prefix %s_%s" \
               % (atlas, side, ar, ar, side)
         if test == 0:
            so, se = afni_base.shell_exec(com)
         else:
            print com
         com = "3dresample -inset %s_%s+tlrc -master %s -rmode Li -prefix %s_%s_rtmp" \
                  % (ar, side, opts['-dsets'].parlist[0], ar, side)
         if test == 0:
            so, se = afni_base.shell_exec(com)
         else:
            print com
            
         #Remove mask interpolation artifacts
         com = "3dcalc -a %s_%s_rtmp+tlrc -expr (step(a-0.5)) -prefix %s_%s_resam " \
                  % ( ar, side, ar, side )
         if test == 0:
            so, se = afni_base.shell_exec(com)
         else:
            print com
         for dset in opts['-dsets'].parlist:
            #Mask the input dset and remove mask interpolation artifacts
            com = "3dcalc -b %s -a %s_%s_resam+tlrc -expr (b*a) -prefix %s_%s_mskd " \
                     % ( dset, ar, side, ar, side )
            if test == 0:
               so, se = afni_base.shell_exec(com)
            else:
               print com
            #Now run the user's command
            ucom = string.join(opts['-command'].parlist,' ')
            ucomr = ucom.replace('MASKED_DSET', dset)
            ucom = ucomr.replace('DSET', "%s_%s_mskd+tlrc" % (ar, side))
            ucomr = ucom.replace('ROI', "%s_%s_resam+tlrc" % (ar, side))
            if test == 0:
               so, se = afni_base.shell_exec(ucomr)
            else:
               print ucomr
            
#help(afni_base.getopts2)

