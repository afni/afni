#!/usr/bin/env python

"""
# Multi-Echo ICA, Version 2.0
# See http://dx.doi.org/10.1016/j.neuroimage.2011.12.028
# Kundu, P., Inati, S.J., Evans, J.W., Luh, W.M. & Bandettini, P.A. Differentiating 
#	BOLD and non-BOLD signals in fMRI time series using multi-echo EPI. NeuroImage (2011).
#
# tedana.py version 2.0 	(c) 2012 Prantik Kundu, Noah Brenowitz, Souheil Inati
# tedana.py version 1.0		(c) 2012 Noah Brenowitz, Prantik Kundu, Souheil Inati
# tedana.py version 0.5		(c) 2011 Prantik Kundu, Souheil Inati
#
# PROCEDURE 2 : Computes ME-PCA and ME-ICA
# -Computes T2* map 
# -Computes PCA of concatenated ME data, then computes TE-dependence of PCs
# -Comcputes ICA of TE-dependence PCs
# -Identifies TE-dependent ICs, outputs high-\kappa (BOLD) component
#    and denoised time series
# -or- Computes TE-dependence of each component of a general linear model
#    specified by input (includes MELODIC FastICA mixing matrix)
"""

import os
from optparse import OptionParser
import numpy as np
import nibabel as nib
from sys import stdout
#import pdb
#import ipdb

F_MAX=500
Z_MAX = 8

def _interpolate(a, b, fraction):
    """Returns the point at the given fraction between a and b, where
    'fraction' must be between 0 and 1.
    """
    return a + (b - a)*fraction;

def scoreatpercentile(a, per, limit=(), interpolation_method='fraction'):
    """
    This function is grabbed from scipy

    """
    values = np.sort(a, axis=0)
    if limit:
        values = values[(limit[0] <= values) & (values <= limit[1])]

    idx = per /100. * (values.shape[0] - 1)
    if (idx % 1 == 0):
        score = values[idx]
    else:
        if interpolation_method == 'fraction':
            score = _interpolate(values[int(idx)], values[int(idx) + 1],
                                 idx % 1)
        elif interpolation_method == 'lower':
            score = values[np.floor(idx)]
        elif interpolation_method == 'higher':
            score = values[np.ceil(idx)]
        else:
            raise ValueError("interpolation_method can only be 'fraction', " \
                             "'lower' or 'higher'")
    return score

def niwrite(data,affine, name , header=None):
	stdout.write(" + Writing file: %s ...." % name) 
	
	thishead = header
	if thishead == None:
		thishead = head.copy()
		thishead.set_data_shape(list(data.shape))

	outni = nib.Nifti1Image(data,affine,header=thishead)
	outni.to_filename(name)
	print 'done.'
	
	



def cat2echos(data,Ne):
	"""
	cat2echos(data,Ne)

	Input:
	data shape is (nx,ny,Ne*nz,nt)
	"""
	nx,ny = data.shape[0:2]
	nz = data.shape[2]/Ne
	if len(data.shape) >3:
		nt = data.shape[3]
	else:
		nt = 1

	return np.reshape(data,(nx,ny,nz,Ne,nt),order='F')

"""
def cat2echos(data,Ne):
	"
	Input:
	data shape is nx,ny,nz*ne,nt

	Output:
	data shape is nx,ny,nz,ne,nt 
	"
	return np.concatenate([data[:,:,ii*nz:(ii+1)*nz-1,np.newaxis,:] for ii in range(Ne)],axis=3)
"""

def makemask(cdat):

	nx,ny,nz,Ne,nt = cdat.shape

	mask = np.ones((nx,ny,nz),dtype=np.bool)

	for i in range(Ne):
		tmpmask = (cdat[:,:,:,i,:] != 0).prod(axis=-1,dtype=np.bool)
		mask = mask & tmpmask

	return mask

def fmask(data,mask):
	"""
	fmask(data,mask)

	Input:
	data shape is (nx,ny,nz,...)
	mask shape is (nx,ny,nz)

	Output:
	out shape is (Nm,...)
	"""

	s = data.shape
	sm = mask.shape

	N = s[0]*s[1]*s[2]
	news = []
	news.append(N)

	if len(s) >3:
		news.extend(s[3:])

	tmp1 = np.reshape(data,news)
	fdata = tmp1.compress((mask > 0 ).ravel(),axis=0)

	return fdata.squeeze()

def unmask (data,mask):
	"""
	unmask (data,mask)

	Input:

	data has shape (Nm,nt)
	mask has shape (nx,ny,nz)

	"""
	M = (mask != 0).ravel()
	Nm = M.sum()

	nx,ny,nz = mask.shape

	if len(data.shape) > 1:
		nt = data.shape[1]
	else:
		nt = 1

	out = np.zeros((nx*ny*nz,nt),dtype=data.dtype)
	out[M,:] = np.reshape(data,(Nm,nt))

	return np.reshape(out,(nx,ny,nz,nt))

def t2smap(catd,mask,tes):
	"""
	t2smap(catd,mask,tes)

	Input:

	catd  has shape (nx,ny,nz,Ne,nt)
	mask  has shape (nx,ny,nz)
	tes   is a 1d numpy array
	"""
	nx,ny,nz,Ne,nt = catd.shape
	N = nx*ny*nz

	echodata = fmask(catd,mask)
	Nm = echodata.shape[0]

	#Do Log Linear fit
	B = np.reshape(np.abs(echodata), (Nm,Ne*nt)).transpose()
	B = np.log(B)
	x = np.array([np.ones(Ne),-tes])
	X = np.tile(x,(1,nt))
	X = np.sort(X)[:,::-1].transpose()

	beta,res,rank,sing = np.linalg.lstsq(X,B)
	t2s = 1/beta[1,:].transpose()
	s0  = np.exp(beta[0,:]).transpose()

	out = unmask(t2s,mask)

	return out

	head.set_data_shape((nx,ny,nz,2))
	vecwrite(out,'t2s.nii',aff)

def get_coeffs(data,mask,X):
	"""
	get_coeffs(data,X)

	Input:

	data has shape (nx,ny,nz,nt)
	mask has shape (nx,ny,nz)
	X    has shape (nt,nc)

	Output:

	out  has shape (nx,ny,nz,nc)
	""" 
	mdata = fmask(data,mask).transpose()
	tmpbetas = np.linalg.lstsq(X,mdata)[0].transpose()
	out = unmask(tmpbetas,mask)

	return out


def fitmodels(betas,t2s,mu,mask,tes,cc=-1,sig=None,fout=None,pow=2.):
	"""
	Usage:

	fitmodels(betas,t2s,mu,mask,tes)

	Input:
	betas,mu,mask are all (nx,ny,nz,Ne,nc) ndarrays
	t2s is a (nx,ny,nz) ndarray
	tes is a 1d array
	"""
	fouts  = []
	nx,ny,nz,Ne,nc = betas.shape
	Nm = mask.sum()

	tes = np.reshape(tes,(Ne,1))
	mumask   = fmask(mu,mask)
	t2smask  = fmask(t2s,mask)
	betamask = fmask(betas,mask)

	comptab = np.zeros((nc,3))

	#Compute variacnes
	betasm = fmask(betas,mask)
	betasm = betasm.reshape([np.prod(betasm.shape[0:2]),betasm.shape[2]])
	#Compute postive variance
	betasms = betasm.copy()
	betasms[betasms<0] = 0
	posvars = (betasms**2).sum(0)
	#Compute negative variance
	betasms = betasm.copy()
	betasms[betasms>0] = 0
	negvars = (betasms**2).sum(0)
	#Compute and apply sign changes
	sfacs = np.ones(posvars.shape)
	sfacs[negvars>posvars]*=-1
	varlist = np.vstack([posvars,negvars])
	varlist/=np.sum(varlist)
	varlist = varlist.T

	#Setup Xmats

	#Model 1
	X1 = mumask.transpose()

	#Model 2
	X2 = np.tile(tes,(1,Nm))*mumask.transpose()/t2smask.transpose()

	if cc!=-1: comps=[cc]
	else: comps=range(nc)
	
	if sig!=None: sigmask=fmask(sig,mask).transpose()
	
	for i in comps:

		#size of B is (nc, nx*ny*nz)
		B = betamask[:,:,i].transpose()
		alpha = (np.abs(B)**2).sum(axis=0)

		#S0 Model
		coeffs_S0 = (B*X1).sum(axis=0)/(X1**2).sum(axis=0)
		SSE_S0 = (B - X1*np.tile(coeffs_S0,(Ne,1)))**2
		SSE_S0 = SSE_S0.sum(axis=0)

		F_S0 = (alpha - SSE_S0)*2/(SSE_S0)
		#F_S0 = (SSTR_S0)*2/(alpha - SSTR_S0)

		#R2 Model
		coeffs_R2 = (B*X2).sum(axis=0)/(X2**2).sum(axis=0)
		SSE_R2 = (B - X2*np.tile(coeffs_R2,(Ne,1)))**2
		SSE_R2 = SSE_R2.sum(axis=0)
		F_R2 = (alpha - SSE_R2)*2/(SSE_R2)

		Bn = B/sigmask
		Bz=(Bn-np.tile(Bn.mean(axis=-1),(Bn.shape[1],1)).T)/np.tile(Bn.std(axis=-1),(Bn.shape[1],1)).T
		wts = Bz.mean(axis=0)*sfacs[i]
		
		
		wts=np.abs(wts)
		#wts[wts<0]=0
		wts[wts>Z_MAX]=Z_MAX
		wts = np.power(wts,pow)
		F_R2[F_R2>F_MAX]=F_MAX
		F_S0[F_S0>F_MAX]=F_MAX
		
		kappa = np.average(F_R2,weights = wts)
		rho   = np.average(F_S0,weights = wts)		

		print "Comp %d Kappa: %f Rho %f" %(i,kappa,rho)
		comptab[i,0] = i
		comptab[i,1] = kappa
		comptab[i,2] = rho

		# Output Voxelwise F-Stats. "fout" is the affine transformation
		if fout is not None:
			
			out = np.zeros((nx,ny,nz,3))
			name = "cc%.3d.nii"%i
			
			Bpsc = 100.*B.T[:,options.e2d-1]/mumask[:,options.e2d-1]
			
			out[:,:,:,0] = np.squeeze(unmask(Bpsc,mask))*sfacs[i]
			out[:,:,:,1] = np.squeeze(unmask(F_R2,mask))
			out[:,:,:,2] = np.squeeze(unmask(F_S0,mask))

			niwrite(out,fout,name)
			os.system('3drefit -sublabel 0 PSC -sublabel 1 F_R2  -sublabel 2 F_SO %s 2> /dev/null > /dev/null'%name)

		#debug
		if cc!=-1: return [F_R2,alpha]
		
	comptab = np.hstack([comptab,varlist])
	return comptab

def andb(arrs):
	result = np.zeros(arrs[0].shape)
	for aa in arrs: result+=np.array(aa,dtype=np.int)
	return result

	
def selcomps(comptable,hifm=True):

	fmin,fmid,fmax = getfbounds(ne)
	ctb = comptable[comptable[:,1].argsort()[::-1],:]

	accvprez = ctb[ctb[:,1]>100,3:5].sum(axis=1)
	accvprez = (accvprez-accvprez.mean())/accvprez.std()
	accrprez = ctb[ctb[:,1]>100,2]
	accrprez = (accrprez-accrprez.mean())/accrprez.std()
	accspre = np.array(accvprez+accrprez>3,dtype=np.int)+np.array(ctb[ctb[:,1]>100,3:5].sum(axis=1)>5,dtype=np.int)
	
	print accvprez,accrprez

	acc = list(ctb[ctb[:,1]>100,0][accspre<2])
	rej = list(ctb[ctb[:,2]>fmid,0])
	mid = list(set(range(ctb.shape[0]))-set(acc)-set(rej))
	
	if len(acc) <= 15 and ctb[1,1]<150: lowq=1
	else: lowq=0
		
	ctbm = comptable[mid]
	ctbm = ctbm[ctbm[:,1].argsort()[::-1],:]
	rej+=list(ctbm[getelbow(ctbm[:,1])-1:,0])
	mid=list(ctbm[:getelbow(ctbm[:,1])-1,0])

	betasm = fmask(betas,mask)
	vars = (betasm.reshape(np.prod(betasm.shape[0:2]),betasm.shape[2])**2).sum(0)
	varpers  = vars/vars.sum()
	midvarthr = vars[acc].mean()
	if lowq==0: midrhothr = comptable[acc,2].mean()+3*comptable[acc,2].std()
	if lowq==1: midrhothr = min(comptable[:,2].mean(),fmid,comptable[getelbow(comptable[:,2][::-1])-1,1])

	#import ipdb
	#ipdb.set_trace()

	addacc = np.array(mid)[andb([comptable[mid,2]<midrhothr,vars[mid]<midvarthr])==2]
	if len(addacc)!=0: acc+=list(addacc)
	mid = list(set(range(ctb.shape[0]))-set(acc)-set(rej))
	
	np.savetxt('accepted.txt',acc,fmt='%d',delimiter=',')
	np.savetxt('rejected.txt',rej,fmt='%d',delimiter=',')
	np.savetxt('midk_rejected.txt',mid,fmt='%d',delimiter=',')
	
	return acc,rej,mid
	

def dvars(dv,mud):
	nx,ny,nz,nt = dv.shape
	d = dv.reshape([nx*ny*nz,nt]).T

	#Compute mean and mask
	if len(mud.shape) == 4:  
		mud = mud.reshape([nx*ny*nz,nt]).T
		d_mu = mud.mean(0).reshape([nx*ny*nz])
	elif len(mud.shape) ==3 :  d_mu = mud.reshape([nx*ny*nz]) 
	else: print "Can't figure out mean dataset dimensions. Goodbye."
	d_beta = d
	
	d_mask = d_mu!=0
	d_mask = (d_mu > scoreatpercentile(d_mu[d_mask],3)) & (d_mu < scoreatpercentile(d_mu[d_mask],98) )
	dp =   (d_beta[:,d_mask]/d_mu[d_mask])*100
	dpdt = np.abs(dp[1:]-dp[0:-1])+0.0000001

	#Condition distribution of dp/dt's
	dpdt_thr = np.log10(dpdt).mean(0)
	dpdt_max = pow(10,scoreatpercentile(dpdt_thr,98))
	dpdt_mask = dpdt.mean(0) < dpdt_max
	 
	#Threshold differentials with extreme values, compute DVARS
	return np.sqrt(np.mean(dpdt[:,dpdt_mask]**2,1))

	
def optcom(data,t2s,tes,mask):
	"""
	out = optcom(data,t2s)


	Input:

	data.shape = (nx,ny,nz,Ne,Nt)
	t2s.shape  = (nx,ny,nz)
	tes.shape  = (Ne,)

	Output:

	out.shape = (nx,ny,nz,Nt)
	"""
	nx,ny,nz,Ne,Nt = data.shape 

	fdat = fmask(data,mask)
	ft2s = fmask(t2s,mask)
	
	tes = tes[np.newaxis,:]
	ft2s = ft2s[:,np.newaxis]
	
	alpha = tes * np.exp(-tes /ft2s)
	alpha = np.tile(alpha[:,:,np.newaxis],(1,1,Nt))

	fout  = np.average(fdat,axis = 1,weights=alpha)
	out = unmask(fout,mask)
	print 'Out shape is ', out.shape
	return out

def getelbow(ks):
	nc = ks.shape[0]
	coords = np.array([np.arange(nc),ks])
	p  = coords - np.tile(np.reshape(coords[:,0],(2,1)),(1,nc))
	b  = p[:,-1] 
	b_hat = np.reshape(b/np.sqrt((b**2).sum()),(2,1))
	proj_p_b = p - np.dot(b_hat.T,p)*np.tile(b_hat,(1,nc))
	d = np.sqrt((proj_p_b**2).sum(axis=0))
	k_min_ind = d.argmax()
	k_min  = ks[k_min_ind]
	return k_min_ind

def getfbounds(ne):
	F05s=[None,None,18.5,10.1,7.7,6.6,6.0,5.6,5.3,5.1,5.0]
	F025s=[None,None,38.5,17.4,12.2,10,8.8,8.1,7.6,7.2,6.9]
	F01s=[None,None,98.5,34.1,21.2,16.2,13.8,12.2,11.3,10.7,10.]
	return F05s[ne-1],F025s[ne-1],F01s[ne-1]

def eimask(dd,ees=None):
	if ees==None: ees=range(dd.shape[1])
	imask = np.zeros([dd.shape[0],len(ees)])
	for ee in ees:
		print ee
		lthr = 0.2*scoreatpercentile(dd[:,ee,:].flatten(),98)
		hthr = 5*scoreatpercentile(dd[:,ee,:].flatten(),98)
		print lthr,hthr
		imask[dd[:,ee,:].mean(1) > lthr,ee]=1
		imask[dd[:,ee,:].mean(1) > hthr,ee]=0
	return imask


def tedpca(ste=0):
	nx,ny,nz,ne,nt = catd.shape
	ste = np.array([int(ee) for ee in str(ste).split(',')])
	if len(ste) == 1 and ste[0]==-1:
		print "-Computing PCA of optimally combined multi-echo data"
		OCcatd = optcom(catd,t2s,tes,mask)
		d = np.float64(fmask(OCcatd,mask))
		eim = eimask(d[:,np.newaxis,:])
		d = d[np.array(np.squeeze(eim),dtype=int)]
	elif len(ste) == 1 and ste[0]==0:
		print "-Computing PCA of spatially concatenated multi-echo data"
		ste = np.arange(ne)
		d = np.float64(fmask(catd,mask))
		eim = eimask(d)==1
		d = d[eim]
	else:
		print "-Computing PCA of TE #%s" % ','.join([str(ee) for ee in ste])
		d = np.float64(np.concatenate([fmask(catd[:,:,:,ee,:],mask)[:,np.newaxis,:] for ee in ste-1],axis=1))
		eim = eimask(d)==1
		d = d[eim]
	
	dz = ((d.T-d.T.mean(0))/d.T.std(0)).T #Variance normalize timeseries
	dz = (dz-dz.mean())/dz.std() #Variance normalize everything
	
	##Do PC dimension selection
	#Get eigenvalue cutoff
	
	#print 'Saving for MATLAB'
	#import scipy.io
	#scipy.io.savemat('dz.mat',{'dz':dz})
	
	#import ipdb
	#ipdb.set_trace()
	
	u,s,v = np.linalg.svd(dz,full_matrices=0)
	sp = s/s.sum()
	eigelb = sp[getelbow(sp)]
	
	spdif = np.abs(sp[1:]-sp[:-1])
	spdifh = spdif[spdif.shape[0]/2:]
	spdmin = spdif.min()
	spdthr = np.mean([spdifh.max(),spdmin])
	spmin = sp[(spdif.shape[0]/2)+(np.arange(spdifh.shape[0])[spdifh>=spdthr][0])+1]
	spcum = []
	spcumv = 0
	for sss in sp:
		spcumv+=sss
		spcum.append(spcumv)
	spcum = np.array(spcum)
		
	#Compute K and Rho for PCA comps
	eimum = np.array(np.squeeze(unmask(np.array(eim,dtype=np.int).prod(1),mask)),dtype=np.bool)
	betasv = get_coeffs(catim.get_data()-catim.get_data().mean(-1)[:,:,:,np.newaxis],np.tile(eimum,(1,1,Ne)),v.T)
	betasv = cat2echos(betasv,Ne)
	
	ctb = fitmodels(betasv,t2s,mu,eimum,tes,sig=sig,fout=None,pow=2)
	ctb = np.vstack([ctb.T[0:3],sp]).T
	
	np.savetxt('comp_table_pca.txt',ctb[ctb[:,1].argsort(),:][::-1])
	np.savetxt('mepca_mix.1D',v[ctb[:,1].argsort()[::-1],:].T)
	kappas = ctb[ctb[:,1].argsort(),1]
	rhos = ctb[ctb[:,2].argsort(),2]
	fmin,fmid,fmax = getfbounds(ne)
	kappa_thr = np.average(sorted([fmin,kappas[getelbow(kappas)]/2,fmid]),weights=[kdaw,1,1])
	rho_thr = np.average(sorted([fmin,rhos[getelbow(rhos)]/2,fmid]),weights=[rdaw,1,1])
	if int(kdaw)==-1:
		kappas_lim = kappas[andb([kappas<fmid,kappas>fmin])==2]
		#kappas_lim = kappas[andb([kappas<kappas[getelbow(kappas)],kappas>fmin])==2]
		kappa_thr = kappas_lim[getelbow(kappas_lim)]
		rhos_lim = rhos[andb([rhos<fmid,rhos>fmin])==2]
		rho_thr = rhos_lim[getelbow(rhos_lim)]
		options.stabilize=True
	if int(kdaw)!=-1 and int(rdaw)==-1:
		rhos_lim = rhos[andb([rhos<fmid,rhos>fmin])==2]
		rho_thr = rhos_lim[getelbow(rhos_lim)]
	#import ipdb
	#ipdb.set_trace()
	if options.stabilize:
		pcscore = (np.array(ctb[:,1]>kappa_thr,dtype=np.int)+np.array(ctb[:,2]>rho_thr,dtype=np.int)+np.array(ctb[:,3]>eigelb,dtype=np.int))*np.array(ctb[:,3]>spmin,dtype=np.int)*np.array(spcum<0.95,dtype=np.int)*np.array(ctb[:,2]>fmin,dtype=np.int)*np.array(ctb[:,1]>fmin,dtype=np.int)*np.array(ctb[:,1]!=F_MAX,dtype=np.int)*np.array(ctb[:,2]!=F_MAX,dtype=np.int) 
	else:
		pcscore = (np.array(ctb[:,1]>kappa_thr,dtype=np.int)+np.array(ctb[:,2]>rho_thr,dtype=np.int)+np.array(ctb[:,3]>eigelb,dtype=np.int))*np.array(ctb[:,3]>spmin,dtype=np.int)*np.array(ctb[:,1]!=F_MAX,dtype=np.int)*np.array(ctb[:,2]!=F_MAX,dtype=np.int)
	pcsel = pcscore > 0 
	pcrej = np.array(pcscore==0,dtype=np.int)*np.array(ctb[:,3]>spmin,dtype=np.int) > 0
	dd = u.dot(np.diag(s*np.array(pcsel,dtype=np.int))).dot(v)
	nc = s[pcsel].shape[0]
	print pcsel
	print "--Selected %i components. Minimum Kappa=%0.2f Rho=%0.2f" % (nc,kappa_thr,rho_thr)
	
	dd = ((dd.T-dd.T.mean(0))/dd.T.std(0)).T #Variance normalize timeseries
	dd = (dd-dd.mean())/dd.std() #Variance normalize everything

	return nc,dd

def tedica(dd,cost):
	"""
	Input is dimensionally reduced spatially concatenated multi-echo time series dataset from tedpca()
	Output is comptable, mmix, smaps from ICA, and betas from fitting catd to mmix
	"""
	#Do ICA
	climit = float("%s" % options.conv)
	#icanode = mdp.nodes.FastICANode(white_comp=nc, white_parm={'svd':True},approach='symm', g=cost, fine_g=options.finalcost, limit=climit, verbose=True)
	icanode = mdp.nodes.FastICANode(white_comp=nc,approach='symm', g=cost, fine_g=options.finalcost, primary_limit=climit*100, limit=climit, verbose=True)
	icanode.train(dd)
	smaps = icanode.execute(dd)
	mmix = icanode.get_recmatrix().T
	mmix = (mmix-mmix.mean(0))/mmix.std(0)
	
	#Compute K and Rho for ICA compos
	eim = eimask(fmask(cat2echos(catim.get_data(),ne),mask))==1
	eimum = np.array(np.squeeze(unmask(np.array(eim,dtype=np.int).prod(1),mask)),dtype=np.bool)
	betas = get_coeffs(catim.get_data()-catim.get_data().mean(-1)[:,:,:,np.newaxis],np.tile(mask,(1,1,Ne)),mmix)
	betas = cat2echos(betas,ne)
	comptable = fitmodels(betas,t2s,mu,eimum,tes,sig=sig,fout=options.fout,pow=2)
	
	#Resort components by Kappa
	comptable = comptable[comptable[:,1].argsort()[::-1],:]
	betas = betas[:,:,:,:,list(comptable[:,0])]
	mmix = mmix[:,list(comptable[:,0])]
	
	#Make everything net positive variance
	varlist = comptable[:,3:5]
	sfacs = np.ones(varlist.shape[0])
	sfacs[varlist[:,1]>varlist[:,0]]*=-1
	mmix*=sfacs
	betas*=sfacs
	
	#Sort pos/neg variances and reassemble comptable
	varlist.sort(1)
	comptable = np.vstack([np.arange(comptable.shape[0]),comptable[:,1:3].T,varlist[:,1],varlist[:,0]  ]).T
	
	return comptable,mmix,smaps,betas

def write_split_ts(data,comptable,mmix,suffix=''):
	mdata = fmask(data,mask)
	betas = fmask(get_coeffs(unmask((mdata.T-mdata.T.mean(0)).T,mask),mask,mmix),mask)
	acc,rej,midk = selcomps(comptable)
	niwrite(unmask(betas[:,acc].dot(mmix.T[acc,:]),mask),aff,'_'.join(['hik_ts',suffix])+'.nii')
	if len(midk)!=0: niwrite(unmask(betas[:,midk].dot(mmix.T[midk,:]),mask),aff,'_'.join(['midk_ts',suffix])+'.nii')
	niwrite(unmask(betas[:,rej].dot(mmix.T[rej,:]),mask),aff,'_'.join(['lowk_ts',suffix])+'.nii')
	niwrite(unmask(fmask(data,mask)-betas.dot(mmix.T),mask),aff,'_'.join(['resid_ts',suffix])+'.nii')

def split_ts(data,comptable,mmix):
	cbetas = get_coeffs(data-data.mean(-1)[:,:,:,np.newaxis],mask,mmix)
	acc,rej,midk = selcomps(comptable)
	betas = fmask(cbetas,mask)
	hikts=unmask(betas[:,acc].dot(mmix.T[acc,:]),mask)
	return hikts,data-hikts

def writefeats(cbetas,comptable,mmix,suffix=''):
	#Write signal changes (dS)
	acc,rej,midk = selcomps(comptable)
	niwrite(cbetas[:,:,:,:],aff,'_'.join(['betas',suffix])+'.nii')
	niwrite(cbetas[:,:,:,acc],aff,'_'.join(['betas_hik',suffix])+'.nii')
	#Compute features (dS/S)
	if options.e2d==None: e2d=np.floor(ne/2)+1
	edm = fmask(catd[:,:,:,e2d-1,:],mask)
	edms = edm/edm.std(-1)[:,np.newaxis]
	edms[edm<1]=0
	hik,noise = split_ts(unmask(edms,mask),comptable,mmix)
	noise = noise-noise.mean(-1)[:,:,:,np.newaxis]

	zfac = 1./(mmix.shape[0]-len(acc)-1)*(noise**2).sum(-1) #noise scaling
	niwrite(zfac,aff,'zfac.nii')
	
	cbetam = fmask(cbetas[:,:,:,acc],mask)
	cbetam = (cbetam-cbetam.mean(0))/cbetam.std(0)
	cbetam = cbetam/fmask(zfac,mask)[:,np.newaxis]
	cbetam[edm.mean(-1)<1,:] = 0
	
	#Compute positive variance
	#cbetams = cbetam.copy()
	#cbetams[cbetams<0] = 0
	#posvars = (cbetams**2).sum(0)
	#Compute positive variance
	#cbetams = cbetam.copy()
	#cbetams[cbetams>0] = 0
	#negvars = (cbetams**2).sum(0)	
	
	#sfacs = np.ones(posvars.shape)
	#sfacs[negvars>posvars]*=-1
	#cbetam*=sfacs
	
	niwrite(unmask(cbetam,mask),aff,'_'.join(['feats',suffix])+'.nii')
	

def writect(comptable,ctname=''):
	nc = comptable.shape[0]
	sortab = comptable[comptable[:,1].argsort()[::-1],:]
	if ctname=='': ctname = 'comp_table.txt'
	with open(ctname,'w') as f:
		f.write("#	comp	Kappa	Rho	+Var%	-Var%\n")
		for i in range(nc):
			f.write('%d\t%f\t%f\t%.2f\t%.2f\n'%(sortab[i,0]+1,sortab[i,1],sortab[i,2],sortab[i,3]*100,sortab[i,4]*100))
	
def writemcct(ct,costnames):
	nc = ct.shape[0]
	ctname = 'comp_table_multicost.txt'
	with open(ctname,'w') as f:
		f.write("#"+'\t'.join(costnames)+"\n")
		for i in range(nc):
			f.write('\t'.join(["%.02f" % ct[i,cc] for cc in range(ct.shape[1])])+"\n")

	
###################################################################################################
# 						Begin Main
###################################################################################################

if __name__=='__main__':

	parser=OptionParser()
	parser.add_option('-d',"--orig_data",dest='data',help="Spatially Concatenated Multi-Echo Dataset",default=None)
	parser.add_option('-e',"--TEs",dest='tes',help="Echo times (in ms) ex: 15,39,63",default=None)
	parser.add_option('',"--mix",dest='mixm',help="Mixing matrix. If not provided, ME-PCA & ME-ICA (MDP) is done.",default=None)
	parser.add_option('',"--sourceTEs",dest='ste',help="Source TEs for models. ex: -ste 2,3 ; -ste 0 for all, -1 for opt. com. Default 0.",default=0)	
	parser.add_option('',"--denoiseTE",dest='e2d',help="TE to denoise. Default middle",default=None)	
	parser.add_option('',"--repave",dest='repave',help="Repeat ME-ICA n times and average hi K time series.",default=1)
	parser.add_option('',"--multicost",dest='multicost',help="Repeat ME-ICA with mult. initial costs and average. ex: --multicost pow3,gaus,skew",default="")
	parser.add_option('',"--initcost",dest='initcost',help="Initial cost func. for ICA: pow3,tanh(default),gaus,skew",default='tanh')
	parser.add_option('',"--finalcost",dest='finalcost',help="Final cost func, same opts. as initial",default='tanh')	
	parser.add_option('',"--stabilize",dest='stabilize',action='store_true',help="Stabilize convergence by reducing dimensionality, for low quality data",default=False)	
	parser.add_option('',"--conv",dest='conv',help="Convergence limit. Default 1e-5",default='1e-5')
	parser.add_option('',"--kdaw",dest='kdaw',help="Dimensionality augmentation weight (Kappa). Default 0. -1 for low-dimensional ICA",default=0.)
	parser.add_option('',"--rdaw",dest='rdaw',help="Dimensionality augmentation weight (Rho). Default 0. -1 for low-dimensional ICA",default=0.)
	parser.add_option('',"--simpel",dest='simpel',help="Kappa/Rho threshold from simple elbow detection. Good for low tSNR.",action='store_true',default=False)
	parser.add_option('',"--slign",dest='slign',help="Slices to ignore. 0-indexed. ex: --slign=0,1,2. Default none.",default=None)
	parser.add_option('',"--OC",dest='OC',help="Output optimally combined time series and features",action="store_true",default=False)
	parser.add_option('',"--fout",dest='fout',help="Output Voxelwise Kappa/Rho Maps",action="store_true",default=False)
	parser.add_option('',"--label",dest='label',help="Label for output directory.",default=None)

	(options,args) = parser.parse_args()

	print "-- ME-PCA/ME-ICA Component for ME-ICA v2.0 --"

	if options.tes==None or options.data==None: 
		print "*+ Need at least data and TEs, use -h for help."		
		sys.exit()

	print "++ Loading Data"
	tes = np.fromstring(options.tes,sep=',',dtype=np.float32)
	ne = tes.shape[0]
	repave = int(options.repave)
	catim  = nib.load(options.data)	
	head   = catim.get_header()
	head.extensions = []
	head.set_sform(head.get_sform(),code=1)
	aff = catim.get_affine()
	catd = cat2echos(catim.get_data(),ne)
	nx,ny,nz,Ne,nt = catd.shape
	if options.slign!=None:
		slign = [int(ss) for ss in options.slign.split(',')]
		catd[:,:,slign,:,:] = 0
	mu  = catd.mean(axis=-1)
	sig  = catd.std(axis=-1)
	if options.fout: options.fout = aff
	else: options.fout=None
	#if options.e2d == None: options.e2d = np.floor(ne/2)+1
	#else: options.e2d = int(options.e2d)
	kdaw = float(options.kdaw)
	rdaw = float(options.rdaw)
	if options.label!=None: dirname='%s' % '.'.join(['TED',options.label])
	else: dirname='TED'
	os.system('mkdir %s' % dirname)
	os.chdir(dirname)
	
	print "++ Computing Mask"
	mask  = makemask(catd)

	print "++ Computing T2* map"
	t2s   = t2smap(catd,mask,tes) 
	niwrite(t2s,aff,'t2sv.nii')
	
	if options.mixm == None:
		print "++ Doing ME-PCA and ME-ICA with MDP"
		import mdp
		nc,dd = tedpca(options.ste)
		comptable,mmix,smaps,betas = tedica(dd,cost=options.initcost)
		np.savetxt('meica_mix.1D',mmix)
		if repave>1:
			print "Doing repeated average ME-ICA"
			mean_hik_ts,mean_other_ts = split_ts(catd[:,:,:,options.e2d-1,:],comptable,mmix)
			for ii in range(repave-1):
				comptable,mmix,smaps,betas = tedica(dd,cost=options.initcost)
				tmp_hik_ts,tmp_other_ts = split_ts(catd[:,:,:,options.e2d-1,:],comptable,mmix)
				mean_hik_ts+=tmp_hik_ts
				mean_other_ts+=tmp_other_ts
			mean_hik_ts/=repave
			mean_other_ts/=repave
			#import pdb
			#pdb.set_trace()
			niwrite(mean_hik_ts,aff,'hik_ts_e%i_mean%i.nii' % (int(options.e2d),int(options.repave)))
			niwrite(mean_other_ts,aff,'other_ts_e%i_mean%i.nii' % (int(options.e2d),int(options.repave)))
		if options.multicost != "":
			incosts = options.multicost.split(',')
			print "Doing multi-cost average ME-ICA"
			mc_comptable = comptable[comptable[:,1].argsort()[::-1],1]
			mean_hik_ts,mean_other_ts = split_ts(catd[:,:,:,options.e2d-1,:],comptable,mmix)
			for acost in sorted(set(incosts)-set([options.initcost])):
				tmp_comptable,mmix,smaps,betas = tedica(dd,cost=acost)
				mc_comptable = np.vstack([mc_comptable,tmp_comptable[tmp_comptable[:,1].argsort()[::-1],1]])
				tmp_hik_ts,tmp_other_ts = split_ts(catd[:,:,:,options.e2d-1,:],comptable,mmix)
				mean_hik_ts+=tmp_hik_ts
				mean_other_ts+=tmp_other_ts
			mean_hik_ts/=len(incosts)
			mean_other_ts/=len(incosts)
			#import ipdb
			#ipdb.set_trace()
			mc_comptable = mc_comptable.T
			writemcct(mc_comptable,[options.initcost]+incosts)
			niwrite(mean_hik_ts,aff,'hik_ts_e%i_multicost.nii' % (int(options.e2d)))
			niwrite(mean_other_ts,aff,'other_ts_e%i_multicost.nii' % (int(options.e2d)))
		del dd
	else:
		mmix = np.loadtxt(options.mixm)
		eim = eimask(np.float64(fmask(catd,mask)))==1
		eimum = np.array(np.squeeze(unmask(np.array(eim,dtype=np.int).prod(1),mask)),dtype=np.bool)
		betas = get_coeffs(catim.get_data(),np.tile(mask,(1,1,Ne)),mmix)
		betas = cat2echos(betas,Ne)
		comptable = fitmodels(betas,t2s,mu,eimum,tes,sig=sig,fout=options.fout,pow=2)
	
	print "++ Writing component table"
	writect(comptable,'comp_table.txt')
	
	if options.e2d!=None:
		options.e2d=int(options.e2d)
		print "++ Writing Kappa-filtered TE#%i timeseries" % (options.e2d)
		write_split_ts(catd[:,:,:,options.e2d-1,:],comptable,mmix,'e%i' % options.e2d)
		print "++ Writing high-Kappa TE#%i  features" % (options.e2d)
		writefeats(betas[:,:,:,options.e2d-1,:],comptable,mmix,'e%i' % options.e2d)
	
	if options.OC == True:
		print "++ Writing optimally combined time series"
		ts = optcom(catd,t2s,tes,mask)
		niwrite(ts,aff,'ts_OC.nii')
		print "++ Writing Kappa-filtered optimally combined timeseries"
		write_split_ts(ts,comptable,mmix,'OC')
		print "++ Writing optimally combined high-Kappa features"
		cbetas = optcom(betas,t2s,tes,mask)
		writefeats(cbetas,comptable,mmix,'OC')
	






	


	




