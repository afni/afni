#!/usr/bin/env python
__version__="v2.5 beta9"
welcome_block="""
# Multi-Echo ICA, Version %s
#
# Kundu, P., Brenowitz, N.D., Voon, V., Worbe, Y., Vertes, P.E., Inati, S.J., Saad, Z.S., 
# Bandettini, P.A. & Bullmore, E.T. Integrated strategy for improving functional 
# connectivity mapping using multiecho fMRI. PNAS (2013).
#
# Kundu, P., Inati, S.J., Evans, J.W., Luh, W.M. & Bandettini, P.A. Differentiating 
#   BOLD and non-BOLD signals in fMRI time series using multi-echo EPI. NeuroImage (2011).
# http://dx.doi.org/10.1016/j.neuroimage.2011.12.028
#
# meica.py version 2.5 (c) 2014 Prantik Kundu
# PROCEDURE 1 : Preprocess multi-echo datasets and apply multi-echo ICA based on spatial concatenation
# -Check arguments, input filenames, and filesystem for dependencies
# -Calculation of motion parameters based on images with highest constrast
# -Application of motion correction and coregistration parameters
# -Misc. EPI preprocessing (temporal alignment, smoothing, etc) in appropriate order
# -Compute PCA and ICA in conjuction with TE-dependence analysis
""" % (__version__)

import sys
import commands
from re import split as resplit
import re
from os import system,getcwd,mkdir,chdir,popen
import os.path
from string import rstrip,split
from optparse import OptionParser,OptionGroup,SUPPRESS_HELP

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
	scipy_installed = 0
	python_version_ok = 0
	global grayweight_ok
	grayweight_ok = 0
	print " + Python version: %s" % ('.'.join([str(v) for v in sys.version_info[0:3]]))
	if sys.version_info >= (3,0):
		print "*+ meica.py requires Python 2.x, not 3.x!"
		fails+=1
	elif sys.version_info < (2,6):
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
	try:
		import scipy
		scipy_installed = 1
	except:
		print "*+ Can't import Scipy! Please check Scipy installation for this Python version."
		fails+=1
	if numpy_installed:
		print " + Numpy version: %s" % (numpy.__version__)
		if map(int, numpy.__version__.split('.')) < (1, 5):
			fails+=1
			print "*+ Numpy version is too old! Please upgrade to Numpy >=1.5.x!"
		import numpy.__config__ as nc
		if nc.blas_opt_info == {}:
			fails+=1
			print "*+ Numpy is not linked to BLAS! Please check Numpy installation."
	if scipy_installed:
		print " + Scipy version: %s" % (scipy.__version__)
		if map(int, scipy.__version__.split('.')) < (0, 11):
			fails+=1
			print "*+ Scipy version is too old! Please upgrade to Scipy >=0.11.x!"
	afnicheck = commands.getstatusoutput("3dinfo")
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
	if grayweight_ok==0:
		print "*+ WARNING: AFNI 3dSeg not available for gray matter weighted coregistration. See README."
	if fails==0:
		print " + Dependencies OK."
	else:
		print "*+ EXITING. Please see error messages."
		sys.exit()

def getdsname(e_ii,prefixonly=False):
	if shorthand_dsin: dsname = '%s%s%s%s' % (prefix,datasets[e_ii],trailing,isf)
	else: dsname =  datasets_in[e_ii]
	if prefixonly: return dsprefix(dsname)
	else: return dsname

def logcomment(comment,level=3): 
	majmark='\n'
	leading='--------'
	if level==3: majmark=''
	if level==1: 
		leading="+* "
		sl.append("""echo "\n++++++++++++++++++++++++" """)
		majmark=''
	sl.append("""%secho %s"%s" """ % (majmark,leading,comment))

#Configure options and help dialog
parser=OptionParser()
parser.add_option('-e',"",dest='tes',help="ex: -e 14.5,38.5,62.5  Echo times (in ms)",default='')
parser.add_option('-d',"",dest='dsinputs',help="ex: -d RESTe1.nii.gz,RESTe2.nii.gz,RESTe3.nii.gz",default='')
parser.add_option('-a',"",dest='anat',help="ex: -a mprage.nii.gz  Anatomical dataset (optional)",default='')
parser.add_option('-b',"",dest='basetime',help="ex: -b 10s OR -b 10v  Time to steady-state equilibration in seconds(s) or volumes(v). Default 0. ",default='0')
parser.add_option('',"--MNI",dest='mni',action='store_true',help="Warp to MNI space using high-resolution template",default=False)
extopts=OptionGroup(parser,"Additional processing options")
extopts.add_option('',"--qwarp",dest='qwarp',action='store_true',help="Nonlinear anatomical normalization to MNI (or --space template) using 3dQWarp, after affine",default=False)
extopts.add_option('',"--fres",dest='fres',help="Specify functional voxel dim. in mm (iso.) for resampling during preprocessing. Default none. ex: --fres=2.5", default=False)
extopts.add_option('',"--space",dest='space',help="Path to specific standard space template for affine anatomical normalization",default=False)
extopts.add_option('',"--no_skullstrip",action="store_true",dest='no_skullstrip',help="Anatomical is already intensity-normalized and skull-stripped (if -a provided)",default=False)
extopts.add_option('',"--no_despike",action="store_true",dest='no_despike',help="Do not de-spike functional data. Default is to de-spike, recommended.",default=False)
extopts.add_option('',"--no_axialize",action="store_true",dest='no_axialize',help="Do not re-write dataset in axial-first order. Default is to axialize, recommended.",default=False)
extopts.add_option('',"--mask_mode",dest='mask_mode',help="Mask functional with help from anatomical or standard space images: use 'anat' or 'template' ",default='func')
extopts.add_option('',"--coreg_mode",dest='coreg_mode',help="Coregistration with Local Pearson and T2* weights (default), or use align_epi_anat.py (edge method): use 'lp-t2s' or 'aea'",default='lp-t2s')
extopts.add_option('',"--smooth",dest='FWHM',help="Data FWHM smoothing (3dBlurInMask). Default off. ex: --smooth 3mm ",default='0mm')
extopts.add_option('',"--align_base",dest='align_base',help="Explicitly specify base dataset for volume registration",default='')
#extopts.add_option('',"--uni_when",dest='uni_when',help="When to unifize anatomical relative to skullstrip: pre,post,never. Default pre.",default='pre')
extopts.add_option('',"--TR",dest='TR',help="The TR. Default read from input dataset header",default='')
extopts.add_option('',"--tpattern",dest='tpattern',help="Slice timing (i.e. alt+z, see 3dTshift -help). Default from header. (N.B. This is important!)",default='')
extopts.add_option('',"--daw",dest='daw',help="Weight to increase ICA dimensionality, default 10. Use -1 for low tSNR data",default='10')
extopts.add_option('',"--align_args",dest='align_args',help="Additional arguments to anatomical-functional co-registration routine",default='')
extopts.add_option('',"--ted_args",dest='ted_args',help="Additional arguments to TE-dependence analysis routine",default='')
extopts.add_option('',"--tlrc",dest='space',help=SUPPRESS_HELP,default=False) #For backwards compat. with existing scripts
extopts.add_option('',"--highpass",dest='highpass',help=SUPPRESS_HELP,default=0.0)
extopts.add_option('',"--detrend",dest='detrend',help=SUPPRESS_HELP,default=0.)
extopts.add_option('',"--initcost",dest='initcost',help=SUPPRESS_HELP,default='tanh')
extopts.add_option('',"--finalcost",dest='finalcost',help=SUPPRESS_HELP,default='tanh')
extopts.add_option('',"--sourceTEs",dest='sourceTEs',help=SUPPRESS_HELP,default='-1')
parser.add_option_group(extopts)
runopts=OptionGroup(parser,"Run optipns")
runopts.add_option('',"--prefix",dest='prefix',help="Prefix for final ME-ICA output datasets.",default='')
runopts.add_option('',"--cpus",dest='cpus',help="Maximum number of CPUs (OpenMP threads) to use. Default 2.",default='2')
runopts.add_option('',"--label",dest='label',help="Label to tag ME-ICA analysis folder.",default='')
runopts.add_option('',"--test_proc",action="store_true",dest='test_proc',help="Align and preprocess 1 dataset then exit, for testing",default=False)
runopts.add_option('',"--script_only",action="store_true",dest='script_only',help="Generate script only, then exit",default=0)
runopts.add_option('',"--pp_only",action="store_true",dest='pp_only',help="Preprocess only, then exit.",default=False)
runopts.add_option('',"--keep_int",action="store_true",dest='keep_int',help="Keep preprocessing intermediates. Default delete.",default=False)
runopts.add_option('',"--skip_check",action="store_true",dest='skip_check',help="Skip dependency checks during initialization.",default=False)
runopts.add_option('',"--OVERWRITE",dest='overwrite',action="store_true",help="If meica.xyz directory exists, overwrite. ",default=False)
parser.add_option_group(runopts)
(options,args) = parser.parse_args()

#Welcome line
print """\n-- Multi-Echo Independent Components Analysis (ME-ICA) %s --

Please cite: 
Kundu, P., Brenowitz, N.D., Voon, V., Worbe, Y., Vertes, P.E., Inati, S.J., Saad, Z.S., 
Bandettini, P.A. & Bullmore, E.T. Integrated strategy for improving functional 
connectivity mapping using multiecho fMRI. PNAS (2013).

Kundu, P., Inati, S.J., Evans, J.W., Luh, W.M. & Bandettini, P.A. Differentiating 
BOLD and non-BOLD signals in fMRI time series using multi-echo EPI. NeuroImage (2011).
""" % (__version__)

#Parse dataset input names
if options.dsinputs=='' or options.TR==0:
	dep_check()
	print "*+ Need at least dataset inputs and TE. Try meica.py -h"
	sys.exit()
if os.path.abspath(os.path.curdir).__contains__('meica.'):
	print "*+ You are inside a ME-ICA directory! Please leave this directory and rerun."
	sys.exit()

#Parse shorthand input file specification and TEs
tes=split(options.tes,',')
outprefix=options.prefix
if '[' in options.dsinputs:
	shorthand_dsin = True
	dsinputs=dsprefix(options.dsinputs)
	prefix=resplit(r'[\[\],]',dsinputs)[0]
	datasets=resplit(r'[\[\],]',dsinputs)[1:-1]
	trailing=resplit(r'[\]+]',dsinputs)[-1]
	isf= dssuffix(options.dsinputs)
	setname=prefix+''.join(datasets)+trailing+options.label
else:
	#Parse longhand input file specificiation
	shorthand_dsin = False
	datasets_in = options.dsinputs.split(',')
	datasets = [str(vv+1) for vv in range(len(tes))]
	prefix = dsprefix(datasets_in[0])
	isf = dssuffix(datasets_in[0])
	if '.nii' in isf: isf='.nii'
	trailing=''
	setname=prefix+options.label
	
if not shorthand_dsin and len(datasets)!=len(datasets_in):
	print "*+ Can't understand dataset specification. Try double quotes around -d argument."
	sys.exit()

if len(options.tes.split(','))!=len(datasets):
	print "*+ Number of TEs and input datasets must be equal and matched in order. Or try double quotes around -d argument."
	sys.exit()

#Prepare script
startdir=rstrip(popen('pwd').readlines()[0])
meicadir=os.path.dirname(sys.argv[0])
sl = []	#Script command list
sl.append('#'+" ".join(sys.argv).replace('"',r"\""))
sl.append(welcome_block)
osf='.nii.gz' #Using NIFTI outputs

#Check if input files exist
notfound=0
for ds_ii in range(len(datasets)): 
	if commands.getstatusoutput('3dinfo %s' % (getdsname(ds_ii)))[0]!=0:
		print "*+ Can't find/load dataset %s !" % (getdsname(ds_ii))
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
	tr=float(os.popen('3dinfo -tr %s' % (getdsname(0))).readlines()[0].strip())
	options.TR=str(tr)
if 'v' in str(options.basetime): 
	basebrik = int(options.basetime.strip('v'))
else:
	timetoclip=0
	timetoclip = float(options.basetime.strip('s'))
	basebrik=int(round(timetoclip/tr))

#Misc. command parsing
if options.mni: options.space='MNI_caez_N27+tlrc'
if options.qwarp and (options.anat=='' or not options.space):
	print "*+ Can't specify Qwarp nonlinear coregistration without anatomical and SPACE template!"
	sys.exit()

if not options.mask_mode in ['func','anat','template']:
	print "*+ Mask mode option '%s' is not recognized!" % options.mask_mode
	sys.exit()

#Parse alignment options
if options.coreg_mode == 'aea': options.t2salign=False
elif 'lp' in options.coreg_mode : options.t2salign=True
align_base = basebrik
align_interp='cubic'
oblique_epi_read = 0 
oblique_anat_read = 0
zeropad_opts = " -I %s -S %s -A %s -P %s -L %s -R %s " % (tuple([1]*6))
if options.anat!='':
	oblique_anat_read = int(os.popen('3dinfo -is_oblique %s' % (options.anat)).readlines()[0].strip())
	epicm = [float(coord) for coord in os.popen("3dCM %s" % (getdsname(0))).readlines()[0].strip().split()]
	anatcm = [float(coord) for coord in os.popen("3dCM %s" % (options.anat)).readlines()[0].strip().split()]
	maxvoxsz = float(os.popen("3dinfo -dk %s" % (getdsname(0))).readlines()[0].strip())
	deltas = [abs(epicm[0]-anatcm[0]),abs(epicm[1]-anatcm[1]),abs(epicm[2]-anatcm[2])]
	cmdist = 20+sum([dd**2. for dd in deltas])**.5
	cmdif =  max(abs(epicm[0]-anatcm[0]),abs(epicm[1]-anatcm[1]),abs(epicm[2]-anatcm[2]))
	addslabs = abs(int(cmdif/maxvoxsz))+10
   	zeropad_opts=" -I %s -S %s -A %s -P %s -L %s -R %s " % (tuple([addslabs]*6))
oblique_epi_read = int(os.popen('3dinfo -is_oblique %s' % (getdsname(0))).readlines()[0].strip())
if oblique_epi_read or oblique_anat_read: 
	oblique_mode = True
	sl.append("echo Oblique data detected.")
else: oblique_mode = False
if options.fres:
	if options.qwarp: qwfres="-dxyz %s" % options.fres
	else: alfres = "-mast_dxyz %s" % options.fres
else: 
	if options.qwarp: qwfres="-dxyz ${voxsize}" #See section called "Preparing functional masking for this ME-EPI run"
	else: alfres="-mast_dxyz ${voxsize}"
if options.anat=='' and options.mask_mode!='func':
	print "*+ Can't do anatomical-based functional masking without an anatomical!"
	sys.exit()

#Prepare script and enter MEICA directory
logcomment("Set up script run environment",level=1)
sl.append('set -e')
sl.append('export OMP_NUM_THREADS=%s' % (options.cpus))
sl.append('export MKL_NUM_THREADS=%s' % (options.cpus))
sl.append('export AFNI_3dDespike_NEW=YES')
if options.overwrite: 
	sl.append('rm -rf meica.%s' % (setname))
else: 
	sl.append("if [[ -e meica.%s ]]; then echo ME-ICA directory exists, exiting; exit; fi" % (setname))
sl.append('mkdir -p meica.%s' % (setname))
sl.append("cp _meica_%s.sh meica.%s/" % (setname,setname))
sl.append("cd meica.%s" % setname)
thecwd= "%s/meica.%s" % (getcwd(),setname)

ica_datasets = sorted(datasets)

#Parse anatomical processing options, process anatomical
if options.anat != '':
	logcomment("Deoblique, unifize, skullstrip, and/or autobox anatomical, in starting directory (may take a little while)", level=1)
	nsmprage = options.anat
	anatprefix=dsprefix(nsmprage)
	pathanatprefix="%s/%s" % (startdir,anatprefix)
	if oblique_mode:
		sl.append("if [ ! -e %s_do.nii.gz ]; then 3dWarp -overwrite -prefix %s_do.nii.gz -deoblique %s/%s; fi" % (pathanatprefix,pathanatprefix,startdir,nsmprage))
		nsmprage="%s_do.nii.gz" % (anatprefix)
	if not options.no_skullstrip: 
		sl.append("if [ ! -e %s_ns.nii.gz ]; then 3dUnifize -overwrite -prefix %s_u.nii.gz %s/%s; 3dSkullStrip  -shrink_fac_bot_lim 0.3 -orig_vol -overwrite -prefix %s_ns.nii.gz -input %s_u.nii.gz; 3dAutobox -overwrite -prefix %s_ns.nii.gz %s_ns.nii.gz; fi" % (pathanatprefix,pathanatprefix,startdir,nsmprage,pathanatprefix,pathanatprefix,pathanatprefix,pathanatprefix))
		nsmprage="%s_ns.nii.gz" % (anatprefix)
	
# Copy in functional datasets as NIFTI (if not in NIFTI already), calculate rigid body alignment
vrbase=getdsname(0,True)
logcomment("Copy in functional datasets, reset NIFTI tags as needed", level=1)
for e_ii in range(len(datasets)):
	ds = datasets[e_ii]
	sl.append("3dcalc -a %s/%s -expr 'a' -prefix ./%s.nii" % (startdir,getdsname(e_ii),getdsname(e_ii,True) )   )
	if '.nii' in isf: 
		sl.append("nifti_tool -mod_hdr -mod_field sform_code 1 -mod_field qform_code 1 -infiles ./%s.nii -overwrite" % (  getdsname(e_ii,True)  ))
isf = '.nii'
		
logcomment("Calculate and save motion and obliquity parameters, despiking first if not disabled, and separately save and mask the base volume",level=1)
#Determine input to volume registration
vrAinput = "./%s%s" % (vrbase,isf)
#Compute obliquity matrix
if oblique_mode: 
	if options.anat!='': sl.append("3dWarp -verb -card2oblique %s[0] -overwrite  -newgrid 1.000000 -prefix ./%s_ob.nii.gz %s/%s | \grep  -A 4 '# mat44 Obliquity Transformation ::'  > %s_obla2e_mat.1D" % (vrAinput,anatprefix,startdir,nsmprage,prefix))
	else: sl.append("3dWarp -overwrite -prefix %s -deoblique %s" % (vrAinput,vrAinput))
#Despike and axialize
if not options.no_despike:
	sl.append("3dDespike -overwrite -prefix ./%s_vrA%s %s "  % (vrbase,osf,vrAinput))
	vrAinput = "./%s_vrA%s" % (vrbase,osf)
if not options.no_axialize: 
	sl.append("3daxialize -overwrite -prefix ./%s_vrA%s %s" % (vrbase,osf,vrAinput))
	vrAinput = "./%s_vrA%s" % (vrbase,osf)
#Set eBbase
external_eBbase=False
if options.align_base!='':
	if options.align_base.isdigit():
		basevol = '%s[%s]' % (vrAinput,options.align_base)
	else:
		basevol = options.align_base
		external_eBbase=True
else: 
	basevol = '%s[%s]' % (vrAinput,basebrik)
sl.append("3dcalc -a %s  -expr 'a' -prefix eBbase.nii.gz "  % (basevol) )
if external_eBbase:
	if oblique_mode: sl.append("3dWarp -overwrite -deoblique eBbase.nii.gz eBbase.nii.gz")
	if not options.no_axialize: sl.append("3daxialize -overwrite -prefix eBbase.nii.gz eBbase.nii.gz")
#Compute motion parameters
sl.append("3dvolreg -overwrite -tshift -quintic  -prefix ./%s_vrA%s -base eBbase.nii.gz -dfile ./%s_vrA.1D -1Dmatrix_save ./%s_vrmat.aff12.1D %s" % \
		  (vrbase,osf,vrbase,prefix,vrAinput))
vrAinput = "./%s_vrA%s" % (vrbase,osf)
sl.append("1dcat './%s_vrA.1D[1..6]{%s..$}' > motion.1D " % (vrbase,basebrik))
e2dsin = prefix+datasets[0]+trailing

logcomment("Preliminary preprocessing of functional datasets: despike, tshift, deoblique, and/or axialize",level=1)
#Do preliminary preproc for this run
if shorthand_dsin: datasets.sort()
for echo_ii in range(len(datasets)):
	#Determine dataset name
	echo = datasets[echo_ii]
	indata = getdsname(echo_ii)
	dsin = 'e'+echo
	if echo_ii==0: e1_dsin = dsin
	logcomment("Preliminary preprocessing dataset %s of TE=%sms to produce %s_ts+orig" % (indata,str(tes[echo_ii]),dsin) )
	#Pre-treat datasets: De-spike, RETROICOR in the future?
	intsname = "%s%s" % (dsprefix(indata),isf)
	if not options.no_despike:
		intsname = "./%s_pt.nii.gz" % dsprefix(indata)
		sl.append("3dDespike -overwrite -prefix %s %s%s" % (intsname,dsprefix(indata),isf))
	#Time shift datasets
	if options.tpattern!='':
		tpat_opt = ' -tpattern %s ' % options.tpattern
	else:
		tpat_opt = ''
	sl.append("3dTshift -heptic %s -prefix ./%s_ts+orig %s" % (tpat_opt,dsin,intsname) )
	if oblique_mode and options.anat=="":
		sl.append("3dWarp -overwrite -deoblique -prefix ./%s_ts+orig ./%s_ts+orig" % (dsin,dsin))
	#Axialize functional dataset
	if not options.no_axialize:
		sl.append("3daxialize  -overwrite -prefix ./%s_ts+orig ./%s_ts+orig" % (dsin,dsin))
	if oblique_mode: sl.append("3drefit -deoblique -TR %s %s_ts+orig" % (options.TR,dsin))
	else: sl.append("3drefit -TR %s %s_ts+orig" % (options.TR,dsin))
#Compute grand mean scaling factor
sl.append("3dBrickStat -mask eBbase.nii.gz -percentile 50 1 50 %s_ts+orig[%i] > gms.1D" % (e1_dsin,basebrik))
sl.append("gms=`cat gms.1D`; gmsa=($gms); p50=${gmsa[1]}")

#Compute T2*, S0, and OC volumes from raw data
logcomment("Prepare T2* and S0 volumes for use in functional masking and (optionally) anatomical-functional coregistration (takes a little while).",level=1)
dss = datasets
dss.sort()
stackline=""
for echo_ii in range(len(dss)):
	echo = datasets[echo_ii]
	dsin = 'e'+echo
	sl.append("3dAllineate -overwrite -final NN -NN -float -1Dmatrix_apply %s_vrmat.aff12.1D'{%i..%i}' -base eBbase.nii.gz -input %s_ts+orig'[%i..%i]' -prefix %s_vrA.nii.gz" % \
				(prefix,int(basebrik),int(basebrik)+5,dsin,int(basebrik),int(basebrik)+5,dsin))
	stackline+=" %s_vrA.nii.gz" % (dsin)
sl.append("3dZcat -prefix basestack.nii.gz %s" % (stackline))
sl.append("%s %s -d basestack.nii.gz -e %s" % (sys.executable, '/'.join([meicadir,'meica.libs','t2smap.py']),options.tes))
sl.append("3dUnifize -prefix ./ocv_uni+orig ocv.nii")
sl.append("3dSkullStrip -prefix ./ocv_ss.nii.gz -overwrite -input ocv_uni+orig")
sl.append("3dcalc -overwrite -a t2svm.nii -b ocv_ss.nii.gz -expr 'a*ispositive(a)*step(b)' -prefix t2svm_ss.nii.gz" )
sl.append("3dcalc -overwrite -a s0v.nii -b ocv_ss.nii.gz -expr 'a*ispositive(a)*step(b)' -prefix s0v_ss.nii.gz" )
if not options.no_axialize:
	sl.append("3daxialize -overwrite -prefix t2svm_ss.nii.gz t2svm_ss.nii.gz")
	sl.append("3daxialize -overwrite -prefix ocv_ss.nii.gz ocv_ss.nii.gz")
	sl.append("3daxialize -overwrite -prefix s0v_ss.nii.gz s0v_ss.nii.gz")

# Calculate affine anatomical warp if anatomical provided, then combine motion correction and coregistration parameters 
if options.anat!='':
	#Copy in anatomical and make sure its in +orig space
	logcomment("Copy anatomical into ME-ICA directory and process warps",level=1)
	sl.append("cp %s/%s* ." % (startdir,nsmprage))
	abmprage = nsmprage
	refanat = nsmprage
	if options.space:
		sl.append("afnibinloc=`which 3dSkullStrip`")
		if '/' in options.space: 
			sl.append("ll=\"%s\"; templateloc=${ll%%/*}/" % options.space)
			options.space=options.space.split('/')[-1]
  		else:
			sl.append("templateloc=${afnibinloc%/*}")
		atnsmprage = "%s_at.nii.gz" % (dsprefix(nsmprage))
		if not dssuffix(nsmprage).__contains__('nii'): sl.append("3dcalc -float -a %s -expr 'a' -prefix %s.nii.gz" % (nsmprage,dsprefix(nsmprage)))
		logcomment("If can't find affine-warped anatomical, copy native anatomical here, compute warps (takes a while) and save in start dir. ; otherwise link in existing files")
		sl.append("if [ ! -e %s/%s ]; then \@auto_tlrc -no_ss -init_xform AUTO_CENTER -base ${templateloc}/%s -input %s.nii.gz -suffix _at" % (startdir,atnsmprage,options.space,dsprefix(nsmprage)))
		sl.append("cp %s.nii %s" % (dsprefix(atnsmprage),startdir))
		sl.append("gzip -f %s/%s.nii" % (startdir,dsprefix(atnsmprage)))
		sl.append("else ln -s %s/%s ." % (startdir,atnsmprage))
		refanat = '%s/%s' % (startdir,atnsmprage)
		sl.append("fi")
		sl.append("3dcopy %s/%s.nii.gz %s" % (startdir,dsprefix(atnsmprage),dsprefix(atnsmprage)))
		sl.append("3drefit -view orig %s+tlrc " % dsprefix(atnsmprage) )
		sl.append("3dAutobox -prefix ./abtemplate.nii.gz ${templateloc}/%s" % options.space)
		abmprage = 'abtemplate.nii.gz'
		if options.qwarp:
			logcomment("If can't find non-linearly warped anatomical, compute, save back; otherwise link")
			nlatnsmprage="%s_atnl.nii.gz" % (dsprefix(nsmprage))
			sl.append("if [ ! -e %s/%s ]; then " % (startdir,nlatnsmprage))
			logcomment("Compute non-linear warp to standard space using 3dQwarp (get lunch, takes a while) ")
			sl.append("3dUnifize -overwrite -GM -prefix ./%su.nii.gz %s/%s" % (dsprefix(atnsmprage),startdir,atnsmprage))  
			sl.append("3dQwarp -iwarp -overwrite -resample -useweight -blur 2 2 -workhard -base ${templateloc}/%s -prefix %s/%snl.nii.gz -source ./%su.nii.gz" % (options.space,startdir,dsprefix(atnsmprage),dsprefix(atnsmprage)))
			sl.append("fi")
			sl.append("ln -s %s/%s ." % (startdir,nlatnsmprage))
			refanat = '%s/%snl.nii.gz' % (startdir,dsprefix(atnsmprage))
	
	#Set anatomical reference for anatomical-functional co-registration
	if oblique_mode: alnsmprage = "./%s_ob.nii.gz" % (anatprefix)
	else: alnsmprage = "%s/%s" % (startdir,nsmprage)
	if options.coreg_mode=='lp-t2s': 
		logcomment("Using alignp_mepi_anat.py to drive T2*-map weighted anatomical-functional coregistration")
		ama_alnsmprage = alnsmprage
		if not options.no_axialize:
			ama_alnsmprage = os.path.basename(alnsmprage)
			sl.append("3daxialize -overwrite -prefix ./%s %s" % (ama_alnsmprage,alnsmprage))
		t2salignpath = 'meica.libs/alignp_mepi_anat.py'
		sl.append("%s %s -t t2svm_ss.nii.gz -a %s -p mepi %s" % \
			(sys.executable, '/'.join([meicadir,t2salignpath]),ama_alnsmprage,options.align_args))
		sl.append("cp alignp.mepi/mepi_al_mat.aff12.1D ./%s_al_mat.aff12.1D" % anatprefix)
	elif options.coreg_mode=='aea':
		logcomment("Using AFNI align_epi_anat.py to drive anatomical-functional coregistration ")
		sl.append("3dcopy %s ./ANAT_ns+orig " % alnsmprage)
		sl.append("align_epi_anat.py -anat2epi -volreg off -tshift off -deoblique off -anat_has_skull no -save_script aea_anat_to_ocv.tcsh -anat ANAT_ns+orig -epi ocv_uni+orig -epi_base 0 %s" % (options.align_args) )
		sl.append("cp ANAT_ns_al_mat.aff12.1D %s_al_mat.aff12.1D" % (anatprefix))
	if options.space: 
		tlrc_opt = "%s/%s::WARP_DATA -I" % (startdir,atnsmprage)
		sl.append("cat_matvec -ONELINE %s > %s/%s_ns2at.aff12.1D" % (tlrc_opt,startdir,anatprefix))
	else: tlrc_opt = ""
	if oblique_mode: oblique_opt = "%s_obla2e_mat.1D" % prefix
	else: oblique_opt = ""
	sl.append("cat_matvec -ONELINE  %s %s %s_al_mat.aff12.1D -I > %s_wmat.aff12.1D" % (tlrc_opt,oblique_opt,anatprefix,prefix))
	sl.append("cat_matvec -ONELINE  %s %s %s_al_mat.aff12.1D -I  %s_vrmat.aff12.1D  > %s_vrwmat.aff12.1D" % (tlrc_opt,oblique_opt,anatprefix,prefix,prefix))

else: sl.append("cp %s_vrmat.aff12.1D %s_vrwmat.aff12.1D" % (prefix,prefix))

#Preprocess datasets
if shorthand_dsin: datasets.sort()
logcomment("Extended preprocessing of functional datasets",level=1)
for echo_ii in range(len(datasets)):

	#Determine dataset name
	echo = datasets[echo_ii]
	indata = getdsname(echo_ii)
	dsin = 'e'+echo

	if echo_ii == 0: 
		logcomment("Preparing functional masking for this ME-EPI run",2 )
		if options.anat: almaster="-master %s" % abmprage
		else: almaster=""
		sl.append("3dZeropad %s -prefix eBvrmask.nii.gz ocv_ss.nii.gz[0]" % (zeropad_opts))
		sl.append("voxsize=`ccalc $(3dinfo -voxvol eBvrmask.nii.gz)**.33`") #Set voxel size
		#Create base mask
		if options.anat and options.space and options.qwarp: 
                        # merged -affter into -nwarp   21 Nov 2014 [rickr]
			sl.append("3dNwarpApply -overwrite -nwarp '%s/%s_WARP.nii.gz %s_wmat.aff12.1D' %s %s -source eBvrmask.nii.gz -interp %s -prefix ./eBvrmask.nii.gz " % \
			(startdir,dsprefix(nlatnsmprage),prefix,almaster,qwfres,'NN'))
			if options.t2salign or options.mask_mode!='func':
				sl.append("3dNwarpApply -overwrite -nwarp '%s/%s_WARP.nii.gz %s_wmat.aff12.1D' %s %s -source t2svm_ss.nii.gz -interp %s -prefix ./t2svm_ss_vr.nii.gz " % \
				(startdir,dsprefix(nlatnsmprage),prefix,almaster,qwfres,'NN'))
				sl.append("3dNwarpApply -overwrite -nwarp '%s/%s_WARP.nii.gz %s_wmat.aff12.1D' %s %s -source ocv_uni+orig -interp %s -prefix ./ocv_uni_vr.nii.gz " % \
				(startdir,dsprefix(nlatnsmprage),prefix,almaster,qwfres,'NN'))
				sl.append("3dNwarpApply -overwrite -nwarp '%s/%s_WARP.nii.gz %s_wmat.aff12.1D' %s %s -source s0v_ss.nii.gz -interp %s -prefix ./s0v_ss_vr.nii.gz " % \
				(startdir,dsprefix(nlatnsmprage),prefix,almaster,qwfres,'NN'))
		elif options.anat:
			sl.append("3dAllineate -overwrite -final %s -%s -float -1Dmatrix_apply %s_wmat.aff12.1D -base eBvrmask.nii.gz -input eBvrmask.nii.gz -prefix ./eBvrmask.nii.gz %s %s" % \
			('NN','NN',prefix,almaster,alfres))
			if options.t2salign or options.mask_mode!='func':
				sl.append("3dAllineate -overwrite -final %s -%s -float -1Dmatrix_apply %s_wmat.aff12.1D -base eBvrmask.nii.gz -input t2svm_ss.nii.gz -prefix ./t2svm_ss_vr.nii.gz %s %s" % \
				('NN','NN',prefix,almaster,alfres))
				sl.append("3dAllineate -overwrite -final %s -%s -float -1Dmatrix_apply %s_wmat.aff12.1D -base eBvrmask.nii.gz -input ocv_uni+orig -prefix ./ocv_uni_vr.nii.gz %s %s" % \
				('NN','NN',prefix,almaster,alfres))
				sl.append("3dAllineate -overwrite -final %s -%s -float -1Dmatrix_apply %s_wmat.aff12.1D -base eBvrmask.nii.gz -input s0v_ss.nii.gz -prefix ./s0v_ss_vr.nii.gz %s %s" % \
				('NN','NN',prefix,almaster,alfres))

		if options.anat and options.mask_mode != 'func':
			if options.space and options.mask_mode == 'template':
				sl.append("3dfractionize -template eBvrmask.nii.gz -input abtemplate.nii.gz -prefix ./anatmask_epi.nii.gz -clip 1")
				logcomment("Preparing functional mask using information from standard space template (takes a little while)")
			if options.mask_mode == 'anat':
				sl.append("3dfractionize -template eBvrmask.nii.gz -input %s -prefix ./anatmask_epi.nii.gz -clip 0.5" % (refanat) )
				logcomment("Preparing functional mask using information from anatomical (takes a little while)")
			sl.append("3dBrickStat -mask eBvrmask.nii.gz -percentile 50 1 50 t2svm_ss_vr.nii.gz > t2s_med.1D")
			sl.append("3dBrickStat -mask eBvrmask.nii.gz -percentile 50 1 50 s0v_ss_vr.nii.gz > s0v_med.1D")
			sl.append("t2sm=`cat t2s_med.1D`; t2sma=($t2sm); t2sm=${t2sma[1]}")
			sl.append("s0vm=`cat s0v_med.1D`; s0vma=($s0vm); s0vm=${s0vma[1]}")
			sl.append("3dcalc -a ocv_uni_vr.nii.gz -b anatmask_epi.nii.gz -c t2svm_ss_vr.nii.gz -d s0v_ss_vr.nii.gz -expr \"a-a*equals(equals(b,0)+isnegative(c-${t2sm})+ispositive(d-${s0vm}),3)\" -overwrite -prefix ocv_uni_vr.nii.gz ")
			sl.append("3dSkullStrip -overwrite -input ocv_uni_vr.nii.gz -prefix eBvrmask.nii.gz ")
			if options.fres: resstring = "-dxyz %s %s %s" % (options.fres,options.fres,options.fres)
			else: resstring = "-dxyz ${voxsize} ${voxsize} ${voxsize}"
			sl.append("3dresample -overwrite -master %s %s -input eBvrmask.nii.gz -prefix eBvrmask.nii.gz" % (abmprage,resstring))

		if options.anat=='':
			logcomment("Trim empty space off of mask dataset and/or resample")
			sl.append("3dAutobox -overwrite -prefix eBvrmask%s eBvrmask%s" % (osf,osf) )
			if options.fres: 
				resstring = "-dxyz %s %s %s" % (options.fres,options.fres,options.fres)
				sl.append("3dresample -overwrite -master eBvrmask.nii.gz %s -input eBvrmask.nii.gz -prefix eBvrmask.nii.gz" % (resstring))
		
		sl.append("3dcalc -float -a eBvrmask.nii.gz -expr 'notzero(a)' -overwrite -prefix eBvrmask.nii.gz")

	#logcomment("Extended preprocessing dataset %s of TE=%sms to produce %s_in.nii.gz" % (indata,str(tes[echo_ii]),dsin),level=2 )
	logcomment("Apply combined normalization/co-registration/motion correction parameter set to %s_ts+orig" % dsin)
	if options.qwarp: sl.append("3dNwarpApply -nwarp '%s/%s_WARP.nii.gz %s_vrwmat.aff12.1D' -master eBvrmask.nii.gz -source %s_ts+orig -interp %s -prefix ./%s_vr%s " % \
			(startdir,dsprefix(nlatnsmprage),prefix,dsin,align_interp,dsin,osf))
	else: sl.append("3dAllineate -final %s -%s -float -1Dmatrix_apply %s_vrwmat.aff12.1D -base eBvrmask%s -input  %s_ts+orig -prefix ./%s_vr%s" % \
		(align_interp,align_interp,prefix,osf,dsin,dsin,osf))
	
	if options.FWHM=='0mm': 
		sl.append("3dcalc -float -overwrite -a eBvrmask.nii.gz -b ./%s_vr%s[%i..$] -expr 'step(a)*b' -prefix ./%s_sm%s " % (dsin,osf,basebrik,dsin,osf))
	else: 
		sl.append("3dBlurInMask -fwhm %s -mask eBvrmask%s -prefix ./%s_sm%s ./%s_vr%s[%i..$]" % (options.FWHM,osf,dsin,osf,dsin,osf,basebrik))
	sl.append("3dcalc -float -overwrite -a ./%s_sm%s -expr \"a*10000/${p50}\" -prefix ./%s_sm%s" % (dsin,osf,dsin,osf))
	sl.append("3dTstat -prefix ./%s_mean%s ./%s_sm%s" % (dsin,osf,dsin,osf))
	if options.detrend: sl.append("3dDetrend -polort %s -overwrite -prefix ./%s_sm%s ./%s_sm%s " % (options.detrend,dsin,osf,dsin,osf) )
	if options.highpass: sl.append("3dBandpass -prefix ./%s_in%s %f 99 ./%s_sm%s " % (dsin,osf,float(options.highpass),dsin,osf) )
	else: sl.append("mv %s_sm%s %s_in%s" % (dsin,osf,dsin,osf))
	sl.append("3dcalc -float -overwrite -a ./%s_in%s -b ./%s_mean%s -expr 'a+b' -prefix ./%s_in%s" % (dsin,osf,dsin,osf,dsin,osf))
	sl.append("3dTstat -stdev -prefix ./%s_std%s ./%s_in%s" % (dsin,osf,dsin,osf))
	if options.test_proc: sl.append("exit")
	if not (options.test_proc or options.keep_int): sl.append("rm -f %s_ts+orig* %s_vr%s %s_sm%s" % (dsin,dsin,osf,dsin,osf))

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
logcomment("Perform TE-dependence analysis (takes a good while)",level=1)
sl.append("%s%s %s -e %s  -d %s --sourceTEs=%s --kdaw=%s --rdaw=1 --initcost=%s --finalcost=%s --conv=2.5e-5 %s" % (tedflag,sys.executable, '/'.join([meicadir,tedanapath]),options.tes,ica_input,options.sourceTEs,options.daw,options.initcost,options.finalcost,options.ted_args))
sl.append("#")
if outprefix=='': outprefix=setname

logcomment("Copying results to start directory",level=1)

sl.append("%scp TED/ts_OC.nii TED/%s_tsoc.nii" % (tedflag,outprefix))
sl.append("%scp TED/dn_ts_OC.nii TED/%s_medn.nii" % (tedflag,outprefix))
sl.append("%scp TED/betas_hik_OC.nii TED/%s_mefc.nii" % (tedflag,outprefix))
sl.append("%scp TED/betas_OC.nii TED/%s_mefl.nii" % (tedflag,outprefix))
sl.append("%scp TED/comp_table.txt %s/%s_ctab.txt" % (tedflag,startdir,outprefix))
hist_line = "%s" % (" ".join(sys.argv).replace('"',r"\""))
note_line = "Denoised timeseries (including thermal noise), produced by ME-ICA v2.5"
sl.append("%s3dNotes -h \'%s (%s)\' TED/%s_medn.nii" % (tedflag,hist_line,note_line,outprefix))
note_line = "Denoised ICA coeff. set for ME-ICR seed-based FC analysis, produced by ME-ICA v2.5"
sl.append("%s3dNotes -h \'%s (%s)\' TED/%s_mefc.nii" % (tedflag,hist_line,note_line,outprefix))
note_line = "Full ICA coeff. set for component assessment, produced by ME-ICA v2.5"
sl.append("%s3dNotes -h \'%s (%s)\' TED/%s_mefc.nii" % (tedflag,hist_line,note_line,outprefix))
note_line = "T2* weighted average of ME time series, produced by ME-ICA v2.5"
sl.append("%s3dNotes -h \'%s (%s)\' TED/%s_tsoc.nii" % (tedflag,hist_line,note_line,outprefix))
if options.anat!='' and options.space!=False:
	sl.append("%snifti_tool -mod_hdr -mod_field sform_code 2 -mod_field qform_code 2 -infiles TED/%s_tsoc.nii -overwrite" % (tedflag,outprefix))
	sl.append("%snifti_tool -mod_hdr -mod_field sform_code 2 -mod_field qform_code 2 -infiles TED/%s_medn.nii -overwrite" % (tedflag,outprefix))
	sl.append("%snifti_tool -mod_hdr -mod_field sform_code 2 -mod_field qform_code 2 -infiles TED/%s_mefc.nii -overwrite" % (tedflag,outprefix))
	sl.append("%snifti_tool -mod_hdr -mod_field sform_code 2 -mod_field qform_code 2 -infiles TED/%s_mefl.nii -overwrite" % (tedflag,outprefix))
sl.append("%sgzip -f TED/%s_medn.nii TED/%s_mefc.nii TED/%s_tsoc.nii TED/%s_mefl.nii" % (tedflag,outprefix,outprefix,outprefix,outprefix))
sl.append("%smv TED/%s_medn.nii.gz TED/%s_mefc.nii.gz TED/%s_tsoc.nii.gz TED/%s_mefl.nii.gz %s" % (tedflag,outprefix,outprefix,outprefix,outprefix,startdir))


#Write the preproc script and execute it
ofh = open('_meica_%s.sh' % setname ,'w')
print "++ Writing script file: _meica_%s.sh" % (setname)
ofh.write("\n".join(sl)+"\n")
ofh.close()
if not options.script_only: 
	print "++ Executing script file: _meica_%s.sh" % (setname)
	system('bash _meica_%s.sh' % setname)
