#!/usr/bin/env python

"""
# Multi-Echo ICA, Version 2.0
# See http://dx.doi.org/10.1016/j.neuroimage.2011.12.028
# Kundu, P., Inati, S.J., Evans, J.W., Luh, W.M. & Bandettini, P.A. Differentiating 
#   BOLD and non-BOLD signals in fMRI time series using multi-echo EPI. NeuroImage (2011).
#
# meica.py version 2.0 (c) 2012 Prantik Kundu
# PROCEDURE 1 : Preprocess multi-echo datasets and apply multi-echo ICA based on spatial concatenation
# -Check arguments, input filenames, and filesystem for dependencies
# -Calculation of motion parameters based on images with highest constrast
# -Calcluation of functional-anatomical coregistration using EPI gray matter + local Pearson correlation method
# -Application of motion correction and coregistration parameters
# -Misc. EPI preprocessing (temporal alignment, smoothing, etc) in appropriate order
# -Compute PCA and ICA in conjuction with TE-dependence analysis
"""

import sys
import commands
from re import split as resplit
import re
from os import system,getcwd,mkdir,chdir,popen
import os.path
from string import rstrip,split
from optparse import OptionParser,OptionGroup

#Filename parser for NIFTI and AFNI files
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

#Run dependency check
def dep_check():
	print '++ Checking system for dependencies...'
	fails=0
	numpy_installed = 0
	python_version_ok = 0
	global grayweight_ok
	grayweight_ok = 0
	print " + Python version: %s" % ('.'.join([str(v) for v in sys.version_info[0:3]]))
	if sys.version_info < (2,6) or sys.version_info > (3,0):
		print "*+ Python 2.x is <2.6, please upgrade to Python 2.x >= 2.6 & Numpy >= 1.5.x."
		fails+=1
	else:
		python_version_ok = 1
	try:
		import numpy
		numpy_installed = 1
	except:
		print "*+ Can't import Numpy! Please check Numpy installation for this Python version."
		fails+=1
	if numpy_installed:
		print " + Numpy version: %s" % (numpy.__version__)
		if float('.'.join(numpy.__version__.split('.')[0:2]))<1.5:
			fails+=1
			print "*+ Numpy version is too old! Please upgrade to Numpy >=1.5.x!"
		import numpy.__config__ as nc
		if nc.blas_opt_info == {}:
			fails+=1
			print "*+ Numpy is not linked to BLAS! Please check Numpy installation."
	afnicheck = commands.getstatusoutput("3dinfo")
	fslfastcheck = commands.getstatusoutput("fast")
	afnisegcheck = commands.getstatusoutput("3dSeg -help")
	if afnicheck[0]!=0:
		print "*+ Can't run AFNI binaries. Make sure AFNI is on the path!"
		fails+=1
	elif not afnicheck[1].__contains__('Alternate Alternative Usage') and len(afnisegcheck[1]) < 1000:
		print "*+ This seems like an old version of AFNI. Please upgrade to latest version of AFNI."
		fails+=1
	if afnisegcheck[0]==0 and len(afnisegcheck[1]) >= 1000:
		print " + Using AFNI 3dSeg for gray matter weighted anatomical-functional coregistration"
		grayweight_ok = 1
	elif fslfastcheck[0]==256:
		try:
			if int(fslfastcheck[1].split('\n')[1].split()[4].strip().strip(')'))>=414:
				print " + Using FSL FAST for gray matter weighted anatomical-functional coregistration"
				grayweight_ok = 2
		except:
			pass
	if grayweight_ok==0:
		print "*+ WARNING: Neither AFNI 3dSeg nor FSL FAST >=4.1 available for gray matter weighted coregistration. See README."
	if fails==0:
		print " + Dependencies OK."
	else:
		print "*+ EXITING. Please see error messages."
		sys.exit()

#Configure options and help dialog
parser=OptionParser()
parser.add_option('-e',"",dest='tes',help="ex: -e 14.5,38.5,62.5  Echo times (in ms)",default='')
parser.add_option('-d',"",dest='dsinputs',help="ex: -d \"PREFIX[2,1,3]ETC.nii.gz\"  TE index of base is first. Note quotes.",default='')
parser.add_option('-f',"",dest='FWHM',help="ex: -f 3mm  Target dataset smoothness (3dBlurToFWHM). Default to 0mm. ",default='0mm')
parser.add_option('-a',"",dest='anat',help="ex: -a mprage.nii.gz  Anatomical dataset (optional)",default='')
parser.add_option('-b',"",dest='basetime',help="ex: -b 10  Time to steady-state equilibration in seconds. Default 0. ",default=0)
extopts=OptionGroup(parser,"Extended preprocessing options")
extopts.add_option('',"--t2salign",action="store_true",dest='t2salign',help="T2* weighted affine anatomical coregistration",default=False)
extopts.add_option('',"--no_skullstrip",action="store_true",dest='no_skullstrip',help="Anatomical is already skullstripped (if -a provided)",default=False)
extopts.add_option('',"--maskpeels",dest='maskpeels',help="Functional masking factor, increase for more aggressive masking, default 3",default=3)
extopts.add_option('',"--tlrc",dest='tlrc',help="Normalize to Talairach space, specify base, ex: --tlrc TT_N27+tlrc",default=False)
extopts.add_option('',"--align_args",dest='align_args',help="Additional arguments for 3dAllineate EPI-anatomical alignment",default='')
extopts.add_option('',"--align_base",dest='align_base',help="Explicitly specify base dataset for volume registration",default='')
extopts.add_option('',"--TR",dest='TR',help="The TR. Default read from input datasets",default='')
extopts.add_option('',"--tpattern",dest='tpattern',help="Slice timing (i.e. alt+z, see 3dTshift -help). Default from header. (N.B. This is important!)",default='')
extopts.add_option('',"--highpass",dest='highpass',help="Highpass filter in Hz (recommended default 0.0)",default=0.0)
extopts.add_option('',"--detrend",dest='detrend',help="Polynomial detrend order (i.e. an integer, see 3dDetrend -help)")
parser.add_option_group(extopts)
icaopts=OptionGroup(parser,"Extended ICA options (see tedana.py -h")
icaopts.add_option('',"--daw",dest='daw',help="Dimensionality increase weight. Default 10. For low tSNR data, use -1",default='10')
icaopts.add_option('',"--initcost",dest='initcost',help="Initial cost for ICA",default='pow3')
icaopts.add_option('',"--finalcost",dest='finalcost',help="Final cost for ICA",default='pow3')
icaopts.add_option('',"--sourceTEs",dest='sourceTEs',help="Source TEs for ICA",default='0')
parser.add_option_group(icaopts)
runopts=OptionGroup(parser,"Run optipns")
runopts.add_option('',"--cpus",dest='cpus',help="Maximum number of CPUs (OpenMP threads) to use. Default 2.",default='2')
runopts.add_option('',"--label",dest='label',help="Label to tag ME-ICA analysis folder.",default='')
runopts.add_option('',"--prefix",dest='prefix',help="Prefix for final ME-ICA output datasets.",default='')
runopts.add_option('',"--test_proc",action="store_true",dest='test_proc',help="Align and preprocess 1 dataset then exit, for testing",default=False)
runopts.add_option('',"--script_only",action="store_true",dest='script_only',help="Generate script only, then exit",default=0)
runopts.add_option('',"--pp_only",action="store_true",dest='pp_only',help="Preprocess only, then exit.",default=False)
runopts.add_option('',"--keep_int",action="store_true",dest='keep_int',help="Keep preprocessing intermediates. Default delete.",default=False)
runopts.add_option('',"--skip_check",action="store_true",dest='skip_check',help="Skip dependency checks during initialization.",default=False)
runopts.add_option('',"--OVERWRITE",dest='overwrite',action="store_true",help="If meica.xyz directory exists, overwrite. ",default=False)
parser.add_option_group(runopts)
(options,args) = parser.parse_args()

#Welcome line
print """\n-- Multi-Echo Independent Components Analysis (ME-ICA) v2.0 --

Please cite: 
Kundu, P., Inati, S.J., Evans, J.W., Luh, W.M. & Bandettini, P.A. Differentiating 
BOLD and non-BOLD signals in fMRI time series using multi-echo EPI. NeuroImage (2011).
"""

#Parse dataset input names
if options.dsinputs=='' or options.TR==0:
	dep_check()
	print "*+ Need at least dataset inputs and TE. Try meica.py -h"
	sys.exit()
if os.path.abspath(os.path.curdir).__contains__('meica.'):
	print "*+ You are inside a ME-ICA directory! Please leave this directory and rerun."
	sys.exit()
dsinputs=dsprefix(options.dsinputs)
prefix=resplit(r'[\[\],]',dsinputs)[0]
datasets=resplit(r'[\[\],]',dsinputs)[1:-1]
trailing=resplit(r'[\]+]',dsinputs)[-1]
isf= dssuffix(options.dsinputs)
tes=split(options.tes,',')
if len(options.tes.split(','))!=len(datasets):
	#print len(options.tes.split(',')), len(datasets)
	print "*+ Number of TEs and input datasets must be equal. Or try double quotes around -d argument."
	sys.exit()
setname=prefix+''.join(datasets)+trailing+options.label
startdir=rstrip(popen('pwd').readlines()[0])
meicadir=os.path.dirname(sys.argv[0])
sl = []				 #Script command list
sl.append('#'+" ".join(sys.argv).replace('"',r"\""))
sl.append('set -e')
osf='.nii.gz'			   #Using NIFTI outputs

#Check if input files exist
notfound=0
for ds in datasets: 
	if commands.getstatusoutput('3dinfo %s%s%s%s' % (prefix,ds,trailing,isf))[0]!=0:
		print "*+ Can't find/load dataset %s%s%s%s !" % (prefix,ds,trailing,isf)
		notfound+=1
if options.anat!='' and commands.getstatusoutput('3dinfo %s' % (options.anat))[0]!=0:
	print "*+ Can't find/load anatomical dataset %s !" % (options.anat)
	notfound+=1
if notfound!=0:
	print "++ EXITING. Check dataset names."
	sys.exit()

#Check dependencies
grayweight_ok = 0
if not options.skip_check: 
	dep_check()
	print "++ Continuing with preprocessing."
else:
	print "*+ Skipping dependency checks."
	grayweight_ok = 1

#Parse timing arguments
if options.TR!='':tr=float(options.TR)
else: 
	tr=float(os.popen('3dinfo -tr %s%s%s%s' % (prefix,datasets[0],trailing,isf)).readlines()[0].strip())
	options.TR=str(tr)
timetoclip=float(options.basetime)
basebrik=int(timetoclip/tr)

#Parse alignment options
align_base = basebrik
align_interp='cubic'
oblique_epi_read = 0 
oblique_anat_read = 0
zeropad_opts = " -I %s -S %s -A %s -P %s -L %s -R %s " % (tuple([1]*6))
if options.anat!='':
	oblique_anat_read = int(os.popen('3dinfo -is_oblique %s' % (options.anat)).readlines()[0].strip())
	epicm = [float(coord) for coord in os.popen("3dCM %s%s%s%s" % (prefix,datasets[0],trailing,isf)).readlines()[0].strip().split()]
	anatcm = [float(coord) for coord in os.popen("3dCM %s" % (options.anat)).readlines()[0].strip().split()]
	maxvoxsz = float(os.popen("3dinfo -dk %s%s%s%s" % (prefix,datasets[0],trailing,isf)).readlines()[0].strip())
	cmdif =  max(abs(epicm[0]-anatcm[0]),abs(epicm[1]-anatcm[1]),abs(epicm[2]-anatcm[2]))
	addslabs = abs(int(cmdif/maxvoxsz))+10
   	zeropad_opts=" -I %s -S %s -A %s -P %s -L %s -R %s " % (tuple([addslabs]*6))
oblique_epi_read = int(os.popen('3dinfo -is_oblique %s%s%s%s' % (prefix,datasets[0],trailing,isf)).readlines()[0].strip())
if oblique_epi_read or oblique_anat_read: 
	oblique_mode = True
	sl.append("echo Oblique data detected.")
else: oblique_mode = False

#Prepare script and enter MEICA directory
sl.append('export OMP_NUM_THREADS=%s' % (options.cpus))
sl.append('export MKL_NUM_THREADS=%s' % (options.cpus))
if options.overwrite: 
	sl.append('rm -rf meica.%s' % (setname))
else: 
	sl.append("if [[ -e meica.%s/stage ]]; then echo ME-ICA directory exists, exiting; exit; fi" % (setname))
sl.append('mkdir -p meica.%s' % (setname))
sl.append("cp _meica_%s.sh meica.%s/" % (setname,setname))
sl.append("cd meica.%s" % setname)
thecwd= "%s/meica.%s" % (getcwd(),setname)

ica_datasets = sorted(datasets)

#Parse anatomical processing options, process anatomical
if options.anat != '':
	nsmprage = options.anat
	anatprefix=dsprefix(nsmprage)
	pathanatprefix="%s/%s" % (startdir,anatprefix)
	if oblique_mode:
		sl.append("if [ ! -e %s_do.nii.gz ]; then 3dWarp -overwrite -prefix %s_do.nii.gz -deoblique %s/%s; fi" % (pathanatprefix,pathanatprefix,startdir,nsmprage))
		nsmprage="%s_do.nii.gz" % (anatprefix)
	if not options.no_skullstrip: 
		sl.append("if [ ! -e %s_ns.nii.gz ]; then 3dSkullStrip -overwrite -prefix %s_ns.nii.gz -input %s/%s; 3dAutobox -overwrite -prefix %s_ns.nii.gz %s_ns.nii.gz; fi" % (pathanatprefix,pathanatprefix,startdir,nsmprage,pathanatprefix,pathanatprefix))
		#sl.append("if [ ! -e %s_ns.nii.gz ]; then 3dSkullStrip -overwrite -prefix %s_ns.nii.gz -input %s/%s; fi" % (pathanatprefix,pathanatprefix,startdir,nsmprage))
		nsmprage="%s_ns.nii.gz" % (anatprefix)

# Copy in datasets as NIFTI (if not), calculate rigid body alignment
vrbase=prefix+datasets[0]+trailing
sl.append("echo 1 > stage" )
if isf==osf:
	for ds in datasets: sl.append("cp %s/%s%s%s%s ./%s%s%s%s" % (startdir,prefix,ds,trailing,isf,prefix,ds,trailing,osf))
else:
	for ds in datasets: sl.append("3dcalc -float -a %s/%s%s%s%s -expr 'a' -prefix ./%s%s%s%s" % (startdir,prefix,ds,trailing,isf,prefix,ds,trailing,osf))
vrAinput = "./%s%s" % (vrbase,osf)
if options.align_base!='':
	if options.align_base.isdigit():
		basevol = '%s[%s]' % (vrAinput,options.align_base)
	else:
		basevol = options.align_base
else: 
	basevol = '%s[%s]' % (vrAinput,basebrik)
sl.append("3dvolreg -tshift -quintic  -prefix ./%s_vrA%s -base %s -dfile ./%s_vrA.1D -1Dmatrix_save ./%s_vrmat.aff12.1D %s" % \
		  (vrbase,osf,basevol,vrbase,prefix,vrAinput))
vrAinput = "./%s_vrA%s" % (vrbase,osf)
if oblique_mode: 
	if options.anat!='': sl.append("3dWarp -verb -card2oblique %s[0] -overwrite  -newgrid 1.000000 -prefix ./%s_ob.nii.gz %s/%s | \grep  -A 4 '# mat44 Obliquity Transformation ::'  > %s_obla2e_mat.1D" % (vrAinput,anatprefix,startdir,nsmprage,prefix))
	else: sl.append("3dWarp -overwrite -prefix %s -deoblique %s" % (vrAinput,vrAinput))
sl.append("1dcat './%s_vrA.1D[1..6]{%s..$}' > motion.1D " % (vrbase,basebrik))
sl.append("3dcalc -float -expr 'a' -a %s[%s] -prefix ./_eBmask%s" % (vrAinput,align_base,osf))
sl.append("3dAutomask -prefix eBmask%s -dilate 1 -peels 3 _eBmask%s" % (osf,osf))
e2dsin = prefix+datasets[0]+trailing

# Calculate affine anatomical warp if anatomical provided, then combine motion correction and coregistration parameters 
if options.anat!='':
	if options.t2salign:
		dss = datasets
		dss.sort()
		stackline=""
		for echo_ii in range(len(dss)):
			echo = datasets[echo_ii]
			dsin = 'e'+echo+trailing
			indata = prefix+echo+trailing+osf
			stackline+=" %s[%i..%i]" % (indata,int(options.basetime),int(options.basetime)+5)
		sl.append("3dZcat -prefix basestack.nii.gz %s" % (stackline))
		sl.append("3dcalc -float -a basestack.nii.gz -expr 'a' -overwrite")
		sl.append("%s %s -d basestack.nii.gz -e %s" % (sys.executable, '/'.join([meicadir,'meica.libs','t2smap.py']),options.tes))
		sl.append("3dBrickStat -mask s0v.nii -percentile 98 1 98 s0v.nii > maxs0.1D")
		sl.append("maxs0=`cat maxs0.1D`; maxs0a=($maxs0); vmax=${maxs0a[1]}; p20=`ccalc ${vmax}*.20`" )
		sl.append("3dcalc -a s0v.nii -b t2sv.nii -expr \"step(a-${p20})*b\" -prefix t2svm.nii.gz" )
		sl.append("3dBrickStat -mask t2svm.nii.gz -percentile 98 1 98 t2svm.nii.gz > maxt2svm.1D")
		sl.append("maxt2svm=`cat maxt2svm.1D`; maxt2svma=($maxt2svm); vmax=${maxt2svma[1]}; p20=`ccalc ${vmax}*.20`" )
		sl.append("3dcalc -a t2svm.nii.gz -expr \"step(a-${p20})*a\" -overwrite -prefix t2svm.nii.gz" )
		sl.append("3dSeg -anat t2svm.nii.gz -mask t2svm.nii.gz")
		sl.append("sc0=`3dBrickStat -mask Segsy/Classes+orig. -mvalue 1 -count Segsy/Classes+orig.`;sc1=`3dBrickStat -mask Segsy/Classes+orig. -mvalue 2 -count Segsy/Classes+orig.`;sc2=`3dBrickStat -mask Segsy/Classes+orig. -mvalue 3 -count Segsy/Classes+orig.`")
		sl.append("cc=\"$sc0 $sc1 $sc2\"")
		sl.append("IC=(`tr ' ' '\\n' <<<$cc | cat -n | sort -k2,2nr | head -n1`)")
		sl.append("mb=$(($IC-1))")
		sl.append("3dcalc -float -a Segsy/Posterior+orig[${mb}] -expr 'a' -prefix epigraywt.nii.gz")
		sl.append("3dcalc -float -a Segsy/AnatUB+orig -b Segsy/Classes+orig -expr 'a*step(b)' -prefix eBbase%s" % (osf))
		sl.append("3dmerge -1clust 4 100 -1erode 80 -1dindex 0 -1tindex 0 -1dilate -overwrite -prefix epigraywt.nii.gz epigraywt.nii.gz")
		weightvol = "epigraywt%s" % osf
		weightline = ' -lpc -weight %s -base eBbase%s ' % (weightvol,osf)
	elif grayweight_ok == 1:
		sl.append("3dSeg -mask eBmask%s -anat _eBmask%s" % (osf,osf))
		sl.append("3dcalc -float -a Segsy/Posterior+orig[2] -expr 'a' -prefix epigraywt%s" % (osf))
		sl.append("3dcalc -float -a Segsy/AnatUB+orig -b Segsy/Classes+orig -expr 'a*step(b)' -prefix eBbase%s" % (osf))
		weightvol = "epigraywt%s" % osf
		weightline = ' -lpc -weight epigraywt%s -base eBbase%s ' % (osf,osf)
	elif grayweight_ok == 2:
		sl.append("3dcalc -float -overwrite -prefix eBmask%s -a eBmask%s -b _eBmask%s -expr 'a*b'" % (osf,osf,osf))
		sl.append("fast -t 2 -n 3 -H 0.1 -I 4 -l 20.0 -b -o eBmask eBmask%s" % (osf)) 
		sl.append("3dcalc -float -a eBmask%s -b eBmask_bias%s -expr 'a/b' -prefix eBbase%s" % ( osf, osf, osf))
		weightvol = "eBmask_pve_0.nii.gz"		
		weightline = ' -lpc -weight eBmask_pve_0.nii.gz -base eBbase%s ' % (osf)
	else: weightline = ' -lpc+ZZ -automask -autoweight -base _eBmask%s ' % (osf)
	sl.append("cp %s/%s* ." % (startdir,nsmprage))	
	abmprage = nsmprage
	if options.tlrc:
		sl.append("afnibinloc=`which 3dSkullStrip`")
		sl.append("templateloc=${afnibinloc%/*}")
		atnsmprage = "%s_at.nii" % (dsprefix(nsmprage))
		if not dssuffix(nsmprage).__contains__('nii'): sl.append("3dcalc -float -a %s -expr 'a' -prefix %s.nii.gz" % (nsmprage,dsprefix(nsmprage)))
		sl.append("if [ ! -e %s ]; then \@auto_tlrc -no_ss -base ${templateloc}/%s -input %s.nii.gz -suffix _at; fi " % (atnsmprage,options.tlrc,dsprefix(nsmprage)))
		sl.append("3dcopy %s %s" % (atnsmprage,dsprefix(atnsmprage)))
		sl.append("3drefit -view orig %s+tlrc " % dsprefix(atnsmprage) )
		sl.append("cp %s %s" % (atnsmprage,startdir))
		sl.append("gzip -f %s/%s" % (startdir,atnsmprage))
		abmprage = atnsmprage
	align_args=""

	if options.align_args!="": align_args=options.align_args
	elif oblique_mode: align_args = " -maxrot 10 -maxshf 10 "
	else: align_args=" -maxrot 20 -maxshf 20 -parfix 7 1  -parang 9 0.83 1.0 "
	if oblique_mode: alnsmprage = "./%s_ob.nii.gz" % (anatprefix)
	else: alnsmprage = "%s/%s" % (startdir,nsmprage)
	sl.append("3dAllineate -weight_frac 1.0 -VERB -warp aff %s -source_automask -cmass -master SOURCE -source %s -prefix ./%s_al -1Dmatrix_save %s_al_mat %s" % (weightline,alnsmprage, anatprefix,anatprefix,align_args))
	if options.tlrc: tlrc_opt = "%s::WARP_DATA -I" % (atnsmprage)
	else: tlrc_opt = ""
	if oblique_mode: oblique_opt = "%s_obla2e_mat.1D" % prefix
	else: oblique_opt = ""
	sl.append("cat_matvec -ONELINE  %s %s %s_al_mat.aff12.1D -I  %s_vrmat.aff12.1D  > %s_wmat.aff12.1D" % (tlrc_opt,oblique_opt,anatprefix,prefix,prefix))

else: sl.append("cp %s_vrmat.aff12.1D %s_wmat.aff12.1D" % (prefix,prefix))

#Preprocess datasets
datasets.sort()
for echo_ii in range(len(datasets)):
	echo = datasets[echo_ii]
	#dsin = prefix+echo+trailing
	dsin = 'e'+echo+trailing
	indata = prefix+echo+trailing

	if options.tpattern!='':
		tpat_opt = ' -tpattern %s ' % options.tpattern
	else:
		tpat_opt = ''
	sl.append("3dTshift -slice 0 -heptic %s -prefix ./%s_ts+orig %s%s" % (tpat_opt,dsin,indata,osf) )
	
	if oblique_mode and options.anat=="":
		sl.append("3dWarp -overwrite -deoblique -prefix ./%s_ts+orig ./%s_ts+orig" % (dsin,dsin))

	sl.append("3drefit -deoblique -TR %s %s_ts+orig" % (options.TR,dsin))

	if echo_ii == 0: 
		if zeropad_opts!="" : sl.append("3dZeropad %s -prefix _eBvrmask.nii.gz %s_ts+orig[%s]" % (zeropad_opts,dsin,basebrik))
		sl.append("3dAllineate -overwrite -final %s -%s -float -1Dmatrix_apply %s_wmat.aff12.1D -base _eBvrmask.nii.gz -input _eBvrmask.nii.gz -prefix ./_eBvrmask.nii.gz" % \
			(align_interp,align_interp,prefix))
		sl.append("3dAutomask -dilate 1 -peels %s -overwrite -prefix eBvrmask%s _eBvrmask%s" % (str(options.maskpeels),osf,osf))
		sl.append("3dBrickStat -mask eBvrmask.nii.gz -percentile 50 1 50  _eBvrmask.nii.gz > gms.1D" )
		if options.anat!='':
			sl.append("3dresample -overwrite -rmode NN -dxyz `3dinfo -ad3 eBvrmask.nii.gz` -inset eBvrmask.nii.gz -prefix eBvrmask.nii.gz -master %s" % (abmprage))
		else:
			sl.append("3dAutobox -overwrite -prefix eBvrmask%s eBvrmask%s" % (osf,osf) )
		sl.append("3dcalc -float -a eBvrmask.nii.gz -expr 'notzero(a)' -overwrite -prefix eBvrmask.nii.gz")
	
	sl.append("3dAllineate -final %s -%s -float -1Dmatrix_apply %s_wmat.aff12.1D -base eBvrmask%s -input  %s_ts+orig -prefix ./%s_vr%s" % \
		(align_interp,align_interp,prefix,osf,dsin,dsin,osf))
	
	if options.FWHM=='0mm': 
		sl.append("3dcalc -float -overwrite -a eBvrmask.nii.gz -b ./%s_vr%s[%i..$] -expr 'step(a)*b' -prefix ./%s_sm%s " % (dsin,osf,basebrik,dsin,osf))
	else: 
		sl.append("3dBlurInMask -fwhm %s -mask eBvrmask%s -prefix ./%s_sm%s ./%s_vr%s[%i..$]" % (options.FWHM,osf,dsin,osf,dsin,osf,basebrik))
	sl.append("gms=`cat gms.1D`; gmsa=($gms); p50=${gmsa[1]}")
	sl.append("3dcalc -float -overwrite -a ./%s_sm%s -expr \"a*10000/${p50}\" -prefix ./%s_sm%s" % (dsin,osf,dsin,osf))
	sl.append("3dTstat -prefix ./%s_mean%s ./%s_sm%s" % (dsin,osf,dsin,osf))
	if options.detrend: sl.append("3dDetrend -polort %s -overwrite -prefix ./%s_sm%s ./%s_sm%s " % (options.detrend,dsin,osf,dsin,osf) )
	if options.highpass: sl.append("3dBandpass -prefix ./%s_in%s %f 99 ./%s_sm%s " % (dsin,osf,float(options.highpass),dsin,osf) )
	else: sl.append("mv %s_sm%s %s_in%s" % (dsin,osf,dsin,osf))
	sl.append("3dcalc -float -overwrite -a ./%s_in%s -b ./%s_mean%s -expr 'a+b' -prefix ./%s_in%s" % (dsin,osf,dsin,osf,dsin,osf))
	sl.append("3dTstat -stdev -prefix ./%s_std%s ./%s_in%s" % (dsin,osf,dsin,osf))
	if options.test_proc: sl.append("exit")
	if not (options.test_proc or options.keep_int): sl.append("rm -f %s_ts+orig* %s_vr%s %s_sm%s" % (dsin,dsin,osf,dsin,osf))

sl.append("echo 2 > stage")

#Concatenate for ICA
if len(ica_datasets)==1:
	dsin = ''.join(ica_datasets)+trailing
	ica_prefix = dsin
	ica_input="./%s_in%s" % (prefix,''.join(ica_datasets),trailing)
	ica_mask="eBvrmask.nii.gz"
else:
	ica_input = "zcat_ffd.nii.gz" 
	ica_mask = "zcat_mask.nii.gz"
	zcatstring=""
	for echo in ica_datasets: 
		dsin ='e'+echo+trailing
		zcatstring = "%s ./%s_in%s" % (zcatstring,dsin,osf)
	sl.append("3dZcat -overwrite -prefix %s  %s" % (ica_input,zcatstring) )
	sl.append("3dcalc -float -overwrite -a %s[0] -expr 'notzero(a)' -prefix %s" % (ica_input,ica_mask))

if options.pp_only: tedflag='#'
else: tedflag = ''

if os.path.exists('%s/meica.libs' % (meicadir)): tedanapath = 'meica.libs/tedana.py'
else: tedanapath = 'tedana.py'
sl.append("#\n#TE-Dependence analysis command:")
sl.append("%s%s %s -e %s  -d %s --sourceTEs=%s --kdaw=%s --rdaw=1 --initcost=%s --finalcost=%s --OC --conv=2.5e-5" % (tedflag,sys.executable, '/'.join([meicadir,tedanapath]),options.tes,ica_input,options.sourceTEs,options.daw,options.initcost,options.finalcost))
sl.append("#")
if options.prefix=='': options.prefix=setname
sl.append("#Copying results to start directory")
"""
sl.append("%scp TED/ts_OC.nii %s/%s_tsoc.nii" % (tedflag,startdir,options.prefix))
sl.append("%scp TED/hik_ts_OC.nii %s/%s_medn.nii" % (tedflag,startdir,options.prefix))
sl.append("%scp TED/betas_hik_OC.nii %s/%s_mefc.nii" % (tedflag,startdir,options.prefix))
sl.append("%scp TED/comp_table.txt %s/%s_ctab.txt" % (tedflag,startdir,options.prefix))
if options.anat!='' and options.tlrc!=False:
	sl.append("%snifti_tool -mod_hdr -mod_field sform_code 2 -mod_field qform_code 2 -infiles %s/%s_tsoc.nii -overwrite" % (tedflag,startdir,options.prefix))
	sl.append("%snifti_tool -mod_hdr -mod_field sform_code 2 -mod_field qform_code 2 -infiles %s/%s_medn.nii -overwrite" % (tedflag,startdir,options.prefix))
	sl.append("%snifti_tool -mod_hdr -mod_field sform_code 2 -mod_field qform_code 2 -infiles %s/%s_mefc.nii -overwrite" % (tedflag,startdir,options.prefix))
hist_line = "%s" % (" ".join(sys.argv).replace('"',r"\""))
note_line = "Denoised timeseries, produced by ME-ICA v2.0 (r5)"
sl.append("%s3dNotes -h \'%s (%s)\' %s/%s_medn.nii" % (tedflag,hist_line,note_line,startdir,options.prefix))
note_line = "Denoised ICA coeff. set for seed-based FC analysis, produced by ME-ICA v2.0 (r1)"
sl.append("%s3dNotes -h \'%s (%s)\' %s/%s_mefc.nii" % (tedflag,hist_line,note_line,startdir,options.prefix))
note_line = "T2* weighted average of ME time series, produced by ME-ICA v2.0 (r5)"
sl.append("%s3dNotes -h \'%s (%s)\' %s/%s_tsoc.nii" % (tedflag,hist_line,note_line,startdir,options.prefix))
sl.append("%sgzip -f %s/%s_medn.nii %s/%s_mefc.nii %s/%s_tsoc.nii" % (tedflag,startdir,options.prefix,startdir,options.prefix,startdir,options.prefix))
"""

sl.append("%scp TED/ts_OC.nii TED/%s_tsoc.nii" % (tedflag,options.prefix))
sl.append("%scp TED/hik_ts_OC.nii TED/%s_medn.nii" % (tedflag,options.prefix))
sl.append("%scp TED/betas_hik_OC.nii TED/%s_mefc.nii" % (tedflag,options.prefix))
sl.append("%scp TED/comp_table.txt %s/%s_ctab.txt" % (tedflag,startdir,options.prefix))
hist_line = "%s" % (" ".join(sys.argv).replace('"',r"\""))
note_line = "Denoised timeseries, produced by ME-ICA v2.0 (r5)"
sl.append("%s3dNotes -h \'%s (%s)\' TED/%s_medn.nii" % (tedflag,hist_line,note_line,options.prefix))
note_line = "Denoised ICA coeff. set for seed-based FC analysis, produced by ME-ICA v2.0 (r1)"
sl.append("%s3dNotes -h \'%s (%s)\' TED/%s_mefc.nii" % (tedflag,hist_line,note_line,options.prefix))
note_line = "T2* weighted average of ME time series, produced by ME-ICA v2.0 (r5)"
sl.append("%s3dNotes -h \'%s (%s)\' TED/%s_tsoc.nii" % (tedflag,hist_line,note_line,options.prefix))
if options.anat!='' and options.tlrc!=False:
	sl.append("%snifti_tool -mod_hdr -mod_field sform_code 2 -mod_field qform_code 2 -infiles TED/%s_tsoc.nii -overwrite" % (tedflag,options.prefix))
	sl.append("%snifti_tool -mod_hdr -mod_field sform_code 2 -mod_field qform_code 2 -infiles TED/%s_medn.nii -overwrite" % (tedflag,options.prefix))
	sl.append("%snifti_tool -mod_hdr -mod_field sform_code 2 -mod_field qform_code 2 -infiles TED/%s_mefc.nii -overwrite" % (tedflag,options.prefix))
sl.append("%sgzip -f TED/%s_medn.nii TED/%s_mefc.nii TED/%s_tsoc.nii" % (tedflag,options.prefix,options.prefix,options.prefix))
sl.append("%smv TED/%s_medn.nii.gz TED/%s_mefc.nii.gz TED/%s_tsoc.nii.gz %s" % (tedflag,options.prefix,options.prefix,options.prefix,startdir))


#Write the preproc script and execute it
ofh = open('_meica_%s.sh' % setname ,'w')
print "++ Writing script file: _meica_%s.sh" % (setname)
ofh.write("\n".join(sl)+"\n")
ofh.close()
if not options.script_only: 
	print "++ Executing script file: _meica_%s.sh" % (setname)
	system('bash _meica_%s.sh' % setname)
