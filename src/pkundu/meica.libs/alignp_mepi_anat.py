#!/usr/bin/env python
__version__="v2.5 beta6"
"""
# Multi-Echo ICA, Version %s
# See http://dx.doi.org/10.1016/j.neuroimage.2011.12.028
#
# Kundu, P., Brenowitz, N.D., Voon, V., Worbe, Y., Vertes, P.E., Inati, S.J., Saad, Z.S., 
# Bandettini, P.A. & Bullmore, E.T. Integrated strategy for improving functional 
# connectivity mapping using multiecho fMRI. PNAS (2013).
#
# Kundu, P., Inati, S.J., Evans, J.W., Luh, W.M. & Bandettini, P.A. Differentiating 
#   BOLD and non-BOLD signals in fMRI time series using multi-echo EPI. NeuroImage (2011).
#
# meica.py version 2.5 (c) 2014 Prantik Kundu
# PROCEDURE 1c : Preprocess multi-echo datasets and apply multi-echo ICA based on spatial concatenation
# -Calculation of functional-anatomical coregistration using EPI gray matter + local Pearson correlation method
#
#alignp_mepi_anat.py V.1.0 
#Compute warp parameters for co-registration from anatomical (skullstripped) to S0 and T2* volume (not skullstripped)
#S0 and T2* volumes should be exactly in register
#
""" % (__version__)

import sys
import commands
from re import split as resplit
import re
from os import system,getcwd,mkdir,chdir,popen
import os.path
from string import rstrip,split
from optparse import OptionParser,OptionGroup

parser=OptionParser()
parser.add_option('-t',"",dest='t2s',help="T2* volume",default='')
parser.add_option('-s',"",dest='s0',help="Skull-stripped S0 weighted volume, optional, for masking T2*",default='')
parser.add_option('-a',"",dest='anat',help="Anatomical volume",default='')
parser.add_option('-p',"",dest='prefix',help="Alignment matrix prefix" ,default='')
parser.add_option('',"--cmass",action='store_true',dest='cmass',help="Align cmass before main co-registration" ,default=False)
parser.add_option('',"--maxrot",dest='maxrot',help="Maximum rotation, default 30" ,default='30')
parser.add_option('',"--maxshift",dest='maxshf',help="Maximum shift, default 30" ,default='30')
parser.add_option('',"--maxscl",dest='maxscl',help="Maximum shift, default 30" ,default='1.2')
(options,args) = parser.parse_args()

"""
Set up and cd into directory
"""
sl = []
startdir = os.path.abspath(os.path.curdir)
walignp_dirname = "alignp.%s" % (options.prefix)
sl.append("rm -rf %s " % walignp_dirname)
sl.append("mkdir %s " % walignp_dirname)
sl.append("cd %s " % walignp_dirname)

def dsprefix(idn):
	def prefix(datasetname):
		return split(datasetname,'+')[0]
	if len(split(idn,'.'))!=0:
		if split(idn,'.')[-1]=='HEAD' or split(idn,'.')[-1]=='BRIK' or split(idn,'.')[-2:]==['BRIK','gz']:
			return prefix(idn)
		elif split(idn,'.')[-1]=='nii' and not split(idn,'.')[-1]=='nii.gz':
			return '.'.join(split(idn,'.')[:-1])
		elif split(idn,'.')[-2:]==['nii','gz']:
			return '.'.join(split(idn,'.')[:-2])
		else:
			return prefix(idn)
	else:
		return prefix(idn)

def dssuffix(idna):
	suffix = idna.split(dsprefix(idna))[-1]
	#print suffix
	spl_suffix=suffix.split('.')
	#print spl_suffix
	if len(spl_suffix[0])!=0 and spl_suffix[0][0] == '+': return spl_suffix[0]
	else: return suffix

def niibrik(nifti): return dsprefix(nifti)+'+orig'

def import_datasets(dsets):
	"""
	Base is functional
	Dset is anatomical
	"""
	outnames = []
	for dset in dsets:
		if dset!='' and dset!=None: 
			indir=''
			if dset[0]!='/': indir = startdir
			sl.append("3dcopy -overwrite %s/%s %s/%s/%s" % (indir,dset,startdir,walignp_dirname,dsprefix(os.path.basename(dset))))
			sl.append("3drefit -view orig `ls %s/%s/%s+*.HEAD`" % (startdir,walignp_dirname,dsprefix(os.path.basename(dset))))
			impdset = '%s+orig' % dsprefix(os.path.basename(dset))
			outnames.append(niibrik(impdset))	
		else: 
			outnames.append('')
	return outnames

def align_centers(base,dset):
	sl.append("@Align_Centers -base %s+orig -dset %s+orig" % (dsprefix(base),dsprefix(dset)))
	volname = dsprefix(dset) + '_shft+orig'
	matname = dsprefix(dset) + '_shft.1D'
	return volname,matname

def makeflat(dset):
	flatname = "%s_flat" % (dsprefix(dset))
	sl.append("3dcalc -a %s -expr 'step(a)' -prefix %s" % (dset,flatname) )
	return niibrik(flatname)

def graywt(t2sname,s0name):
	basevol='align_base.nii.gz'
	weightvol='align_weight.nii.gz'
	if s0name!='': 
		sl.append("3dcalc -overwrite -a %s -b %s -expr 'a*ispositive(a)*step(b)' -prefix t2svm_ss.nii.gz" % (t2sname,s0name) )
		t2sname='t2svm_ss.nii.gz'
	sl.append("3dBrickStat -mask %s -percentile 50 1 50 %s > graywt_thr.1D" %  (t2sname,t2sname) )
	sl.append("maxthr=`cat graywt_thr.1D`; maxthra=($maxthr); vmax=${maxthra[1]}" )
	sl.append("3dcalc -overwrite -a %s -expr \"a*isnegative(a-2*${vmax})\" -prefix t2svm_thr.nii.gz" % (t2sname) )
	sl.append("3dUnifize -overwrite -prefix align_base.nii.gz t2svm_thr.nii.gz")
	sl.append("3dSeg -prefix Segsy.t2svm -anat %s -mask %s" % (basevol,basevol))
	sl.append("3dcalc -overwrite -a 'Segsy.t2svm/Posterior+orig[2]' -prefix %s -expr 'a' " % weightvol)
	return basevol,weightvol

def allineate(sourcevol, weight, targetvol, prefix, maxrot, maxshf,maxscl,do_cmass):
	"""
	source should be anatomical
	target should be T2* 
	"""
	outvol_prefix = "%s_al"  % prefix
	outmat_prefix = "%s_al_mat"  % prefix
	cmass_opt = ''
	if do_cmass: cmass_opt = '-cmass'
	align_opts = "-lpc -weight %s -maxshf %s -maxrot %s -maxscl %s %s" % (weightvol, maxrot, maxshf,maxscl,cmass_opt)
	sl.append("3dAllineate -overwrite -weight_frac 1.0 -VERB -warp aff -source_automask+2 -master SOURCE -source %s -base %s -prefix ./%s -1Dmatrix_save ./%s %s " \
		% (sourcevol,targetvol,outvol_prefix,outmat_prefix,align_opts))
	outvol_name = niibrik(outvol_prefix)
	outmat_name = outmat_prefix + 'blah'
	return outvol_name, outmat_name

def aligntest(target,source,testname):
	sl.append("3dresample -overwrite -master %s -inset %s -prefix altestA -rmode NN" % (target,source))
	sl.append("echo `3ddot altestA+orig %s` > %s.txt" % (target,testname))

def runproc(script_prefix, scrlines):
	script_name = "_walignp_mepi_anat_%s.sh" % script_prefix
	ofh = open(script_name,'w')
	ofh.write("\n".join(scrlines))
	ofh.close()
	os.system("bash %s" % script_name)

"""Import datasets"""
t2s_name,s0_name,anat_name = import_datasets([options.t2s,options.s0,options.anat])

"""Filter and segment T2* volume to produce T2* base and weight volumes"""
basevol,weightvol = graywt(t2s_name,s0_name)

"""Run 3dAllineate"""
allin_volume,allin_matrix = allineate(anat_name,weightvol,basevol,options.prefix,options.maxrot,options.maxshf,options.maxscl,options.cmass)

"""Run procedure"""
runproc(options.prefix, sl)










