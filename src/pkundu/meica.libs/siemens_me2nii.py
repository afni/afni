#!/usr/bin/python

import sys
from os import system,getcwd,mkdir,chdir,popen
import os.path
from string import rstrip,split
from optparse import OptionParser,OptionGroup
import glob

#Configure options and help dialog
parser=OptionParser()
parser.add_option('-p',"",dest='prefix',help="Multi-Echo Filename Prefix",default='me')
parser.add_option('-a',"",dest='anat',help="Anatomical (MPRAGE) directory",default='')
(options,args) = parser.parse_args()

startdir=rstrip(popen('pwd').readlines()[0])

if options.anat!='':
	os.chdir(options.anat)
	os.system('dcm2nii *')
	os.system('mv %s %s/anat.nii.gz' % (glob.glob('*nii.gz')[0],startdir))
	os.chdir(startdir)

for arg_ii in range(len(args)):
	arg = args[arg_ii]
	thedir = glob.glob('%s*' % (arg))[0]
	os.chdir(thedir)
	os.system("dcm2nii *")
	niifile = glob.glob("*nii.gz")[0]
	os.system("mv %s %s/%s_e%i.nii.gz" % (niifile,startdir,options.prefix,arg_ii+1))
	os.chdir(startdir)
