#!/usr/bin/env python

"""
# Multi-Echo ICA, Version 2.5 beta1
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

def uncat2echos(data,Ne):
	"""
	uncat2echos(data,Ne)

	Input:
	data shape is (nx,ny,Ne,nz,nt)
	"""
    	nx,ny = data.shape[0:2]
	nz = data.shape[2]*Ne
	if len(data.shape) >4:
		nt = data.shape[4]
	else:
		nt = 1
	return np.reshape(data,(nx,ny,nz,nt),order='F')

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

	out = unmask(t2s,mask),unmask(s0,mask)

	return out

	head.set_data_shape((nx,ny,nz,2))
	vecwrite(out,'t2s.nii',aff)

def get_coeffs(data,mask,X,add_const=False):
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
        
        X=np.atleast_2d(X)
        if X.shape[0]==1: X=X.T
        Xones = np.atleast_2d(np.ones(np.min(mdata.shape))).T
        if add_const: X = np.hstack([X,Xones])

	tmpbetas = np.linalg.lstsq(X,mdata)[0].transpose()
        if add_const: tmpbetas = tmpbetas[:,:-1]
	out = unmask(tmpbetas,mask)

	return out


def andb(arrs):
	result = np.zeros(arrs[0].shape)
	for aa in arrs: result+=np.array(aa,dtype=np.int)
	return result


def fitmodels2__(catd,mmix,mask,t2s,tes,fout=None,reindex=False):
	"""
   	Usage:
   	
   	fitmodels2(fout)
	
   	Input:
   	fout is flag for output of per-component TE-dependence maps
   	t2s is a (nx,ny,nz) ndarray
   	tes is a 1d array
   	"""

	#Compute optimally combined time series and demean
	tsoc = np.array(optcom(catd,t2s,tes,mask),dtype=float)[mask]
	tsoc_dm = tsoc-tsoc.mean(axis=-1)[:,np.newaxis]
	#Fit mmix to tsoc
	tsoc_B = get_coeffs(unmask(tsoc_dm,mask),mask,mmix)[mask]
	#Compute residuals
	tsoc_resid = tsoc_dm - np.dot(tsoc_B,mmix.T)
	#Compute per-voxel weights for Kappa and Rho (inverse variance normalization)
	WTS = tsoc_B/tsoc_resid.std(axis=-1)[:,np.newaxis]
	#Compute skews to determine signs, fix mmix signs
	from scipy.stats import skew
	signs = skew(WTS,axis=0)
	signs /= np.abs(signs)
	mmix = mmix.copy()
	mmix*=signs
	tsoc_B*=signs
	WTS*=signs
	#Compute total variance
	totvar = (WTS**2).sum()
	#Compute per-voxel percent signal changes
	PSC = tsoc_B/tsoc.mean(axis=-1)[:,np.newaxis]*100

	#Compute Betas and means over TEs for TE-dependence analysis
	Ne = tes.shape[0]
	betas = cat2echos(get_coeffs(uncat2echos(catd,Ne),np.tile(mask,(1,1,Ne)),mmix),Ne)
	nx,ny,nz,Ne,nc = betas.shape
	Nm = mask.sum()
	mu = catd.mean(axis=-1)
	tes = np.reshape(tes,(Ne,1))

	#Mask arrays
	mumask   = fmask(mu,mask)
	t2smask  = fmask(t2s,mask)
	betamask = fmask(betas,mask)
	
	#Set up component table
	KRp_start = 1.5; KRp_stop=5; KRsamps = 8 
	KRps=np.unique(np.hstack([np.linspace(KRp_start,KRp_stop,KRsamps),[2.,4.]]))
	KRps.sort()
	KRd=np.zeros([nc,2,KRsamps])
	comptab2 = np.zeros((nc,5))

	#Setup Xmats
	#Model 1
	X1 = mumask.transpose()
	
	#Model 2
	X2 = np.tile(tes,(1,Nm))*mumask.transpose()/t2smask.transpose()
	
	for i in range(nc):
		
		#Compute percent variance explained
		comptab2[i,0] = i
		comptab2[i,3] = (WTS[:,i]**2).sum()/totvar*100.
		comptab2[i,4] = scoreatpercentile(np.abs(PSC[:,i]),90)

		#size of B is (nc, nx*ny*nz)
		B = np.atleast_3d(betamask)[:,:,i].transpose()
		alpha = (np.abs(B)**2).sum(axis=0)
		
		#S0 Model
		coeffs_S0 = (B*X1).sum(axis=0)/(X1**2).sum(axis=0)
		SSE_S0 = (B - X1*np.tile(coeffs_S0,(Ne,1)))**2
		SSE_S0 = SSE_S0.sum(axis=0)
		F_S0 = (alpha - SSE_S0)*2/(SSE_S0)
		
		#R2 Model
		coeffs_R2 = (B*X2).sum(axis=0)/(X2**2).sum(axis=0)
		SSE_R2 = (B - X2*np.tile(coeffs_R2,(Ne,1)))**2
		SSE_R2 = SSE_R2.sum(axis=0)
		F_R2 = (alpha - SSE_R2)*2/(SSE_R2)
		
		wtsZ=np.abs((WTS[:,i]-WTS[:,i].mean())/WTS[:,i].std())
		wtsZ[wtsZ>Z_MAX]=Z_MAX
		for KRp_i in range(len(KRps)):
			KRp = KRps[KRp_i]
			wtsZp = np.power(wtsZ,KRp)
			F_R2[F_R2>F_MAX]=F_MAX
			F_S0[F_S0>F_MAX]=F_MAX
			kappa = np.average(F_R2,weights = wtsZp)
			rho   = np.average(F_S0,weights = wtsZp)
			print "Power: %.1f Comp %d Kappa: %f Rho %f" %(KRp, i,kappa,rho)		
			if KRp==2:
				comptab2[i,1] = kappa
				comptab2[i,2] = rho
			KRd[i,0,KRp_i] = kappa
			KRd[i,1,KRp_i] = rho

		if fout is not None:
			out = np.zeros((nx,ny,nz,4))
			name = "cc%.3d.nii"%i
			out[:,:,:,0] = np.squeeze(unmask(PSC[:,i],mask))
			out[:,:,:,1] = np.squeeze(unmask(F_R2,mask))
			out[:,:,:,2] = np.squeeze(unmask(F_S0,mask))
			out[:,:,:,3] = np.squeeze(unmask(wtsZ,mask))
			niwrite(out,fout,name)
			os.system('3drefit -sublabel 0 PSC -sublabel 1 F_R2  -sublabel 2 F_SO %s 2> /dev/null > /dev/null'%name)
	
	comptab = comptab2

	if reindex:
		comptab = comptab2[comptab2[:,1].argsort()[::-1],:]
		mmix = mmix[:,comptab[:,0].tolist()]
		betas = betas[:,:,:,:,comptab[:,0].tolist()]
		KRd = KRd[comptab[:,0].tolist(),:,:]
		comptab = np.hstack([np.atleast_2d(np.arange(nc)).T,comptab[:,1:]])

	return comptab, KRd, betas, mmix

def fitmodels2(catd,mmix,mask,t2s,tes,fout=None,reindex=False,mmixN=None):
	"""
   	Usage:
   	
   	fitmodels2(fout)
	
   	Input:
   	fout is flag for output of per-component TE-dependence maps
   	t2s is a (nx,ny,nz) ndarray
   	tes is a 1d array
   	"""

   	#Compute opt. com. raw data
	tsoc = np.array(optcom(catd,t2s,tes,mask),dtype=float)[mask]
	tsoc_dm = tsoc-tsoc.mean(axis=-1)[:,np.newaxis]
	
	#Compute un-normalized weight dataset (features)
	if mmixN == None: mmixN=mmix
	WTS = computefeats2(unmask(tsoc,mask),mmixN,mask,normalize=False)

	#Compute skews to determine signs based on unnormalized weights, correct mmix & WTS signs based on spatial distribution tails
	from scipy.stats import skew
	signs = skew(WTS,axis=0)
	signs /= np.abs(signs)
	mmix = mmix.copy()
	mmix*=signs
	WTS*=signs
	totvar = (WTS**2).sum()

	#Compute PSC dataset
	tsoc_B = get_coeffs(unmask(tsoc_dm,mask),mask,mmix)[mask]
	PSC = tsoc_B/tsoc.mean(axis=-1)[:,np.newaxis]*100
	
	#Compute Betas and means over TEs for TE-dependence analysis
	Ne = tes.shape[0]
	betas = cat2echos(get_coeffs(uncat2echos(catd,Ne),np.tile(mask,(1,1,Ne)),mmix),Ne)
	nx,ny,nz,Ne,nc = betas.shape
	Nm = mask.sum()
	mu = catd.mean(axis=-1)
	tes = np.reshape(tes,(Ne,1))

	#Mask arrays
	mumask   = fmask(mu,mask)
	t2smask  = fmask(t2s,mask)
	betamask = fmask(betas,mask)
	
	#Set up component table
	KRp_start = 1.5; KRp_stop=5; KRsamps = 8 
	KRps=np.unique(np.hstack([np.linspace(KRp_start,KRp_stop,KRsamps),[2.,4.]]))
	KRps.sort()
	KRd=np.zeros([nc,2,KRsamps])
	comptab2 = np.zeros((nc,5))

	#Setup Xmats
	#Model 1
	X1 = mumask.transpose()
	
	#Model 2
	X2 = np.tile(tes,(1,Nm))*mumask.transpose()/t2smask.transpose()
	
	for i in range(nc):
		
		#Compute percent variance explained
		comptab2[i,0] = i
		comptab2[i,3] = (WTS[:,i]**2).sum()/totvar*100.
		comptab2[i,4] = scoreatpercentile(np.abs(PSC[:,i]),90)

		#size of B is (nc, nx*ny*nz)
		B = np.atleast_3d(betamask)[:,:,i].transpose()
		alpha = (np.abs(B)**2).sum(axis=0)
		
		#S0 Model
		coeffs_S0 = (B*X1).sum(axis=0)/(X1**2).sum(axis=0)
		SSE_S0 = (B - X1*np.tile(coeffs_S0,(Ne,1)))**2
		SSE_S0 = SSE_S0.sum(axis=0)
		F_S0 = (alpha - SSE_S0)*2/(SSE_S0)
		
		#R2 Model
		coeffs_R2 = (B*X2).sum(axis=0)/(X2**2).sum(axis=0)
		SSE_R2 = (B - X2*np.tile(coeffs_R2,(Ne,1)))**2
		SSE_R2 = SSE_R2.sum(axis=0)
		F_R2 = (alpha - SSE_R2)*2/(SSE_R2)
		
		#Grab weights
		wtsZ=np.abs((WTS[:,i]-WTS[:,i].mean())/WTS[:,i].std())
		wtsZ[wtsZ>Z_MAX]=Z_MAX
		
		for KRp_i in range(len(KRps)):
			KRp = KRps[KRp_i]
			wtsZp = np.power(wtsZ,KRp)
			F_R2[F_R2>F_MAX]=F_MAX
			F_S0[F_S0>F_MAX]=F_MAX
			kappa = np.average(F_R2,weights = wtsZp)
			rho   = np.average(F_S0,weights = wtsZp)
			print "Power: %.1f Comp %d Kappa: %f Rho %f" %(KRp, i,kappa,rho)		
			if KRp==2:
				comptab2[i,1] = kappa
				comptab2[i,2] = rho
			KRd[i,0,KRp_i] = kappa
			KRd[i,1,KRp_i] = rho

		if fout is not None:
			out = np.zeros((nx,ny,nz,4))
			name = "cc%.3d.nii"%i
			out[:,:,:,0] = np.squeeze(unmask(PSC[:,i],mask))
			out[:,:,:,1] = np.squeeze(unmask(F_R2,mask))
			out[:,:,:,2] = np.squeeze(unmask(F_S0,mask))
			out[:,:,:,3] = np.squeeze(unmask(wtsZ,mask))
			niwrite(out,fout,name)
			os.system('3drefit -sublabel 0 PSC -sublabel 1 F_R2  -sublabel 2 F_SO %s 2> /dev/null > /dev/null'%name)
	
	comptab = comptab2

	if reindex:
		comptab = comptab2[comptab2[:,1].argsort()[::-1],:]
		mmix = mmix[:,comptab[:,0].tolist()]
		betas = betas[:,:,:,:,comptab[:,0].tolist()]
		KRd = KRd[comptab[:,0].tolist(),:,:]
		comptab = np.hstack([np.atleast_2d(np.arange(nc)).T,comptab[:,1:]])

	return comptab, KRd, betas, mmix

def selcomps(comptable):
	fmin,fmid,fmax = getfbounds(ne)
	ctb = comptable[ comptable[:,1].argsort()[::-1],:]
	ks = ctb[:,1]
	rs =ctb[ ctb[:,2].argsort()[::-1],2]
	k_t = ks[getelbow(ks)+1]
	r_t = rs[getelbow(rs)+1]
	sel = andb([ctb[:,1]>=k_t, ctb[:,2]<r_t])
	acc = ctb[sel==2,0]
	rej = ctb[sel!=2,0]
	mid = np.array([])
	#ipdb.set_trace()
	#(ctb[sel==2,1]).argsort().argsort()-(ctb[sel==2,3:5].sum(axis=1)*100).argsort().argsort()
	open('accepted.txt','w').write(','.join([str(int(cc)) for cc in acc]))
	open('rejected.txt','w').write(','.join([str(int(cc)) for cc in rej]))
	open('midk_rejected.txt','w').write(','.join([str(int(cc)) for cc in mid]))
	return acc.tolist(),rej.tolist(),mid.tolist()


def selcomps_voter(ctb,KRd,margin=0.75,extend=False):
	fmin,fmid,fmax = getfbounds(ne)
	nc = KRd.shape[0]
	inds = np.atleast_2d(np.arange(nc)).T
	acc_list = []
	rej_list = []
	for ppp in range(KRd.shape[2]):
		ct = np.hstack([inds,KRd[:,:,ppp]])
		ct = ct[ct[:,1].argsort()[::-1],:]
		ks = ct[:,1]
		rs = ct[:,2]
		rs = rs[rs.argsort()[::-1]]
		k_t = ks[getelbow(ks)+1]
		r_t = np.min([rs[getelbow(rs)+1],fmid])
		print k_t,r_t
		if extend: acc_list.append(ct[ct[:,2]<r_t,0])	
		else:
			sel = andb([ct[:,1]>=k_t, ct[:,2]<r_t])
			acc_list.append(ct[sel==2,0])
	#Find accepted with voting scheme
	accs = []
	for aa in acc_list: accs+=list(aa)
	accsu = np.unique(accs)
	accsd = dict()
	for aa in accsu: accsd[aa]=0
	for aa in acc_list: 
		for cc in aa: accsd[cc]+=1
	accsu_values = np.array([accsd[cc] for cc in accsu])
	accf = accsu[accsu_values>=np.floor(margin*KRd.shape[2])]
	accf_mid = accsu[andb([accsu_values>=np.floor(margin*KRd.shape[2]),accsu_values<KRd.shape[2] ])==2 ]
	#Find mid-Kappa high variance components (based on percent signal change)
	ctb = ctb[ctb[:,0].argsort(),:]
	psc_thr = np.median(ctb[accf.tolist(),4])*2
	kappa_med = scoreatpercentile(ctb[accf.tolist(),1],2./3*100)
	mid = list(accf[andb([ctb[accf.tolist(),4]>=psc_thr,ctb[accf.tolist(),1]<kappa_med])==2])
	acc = list(set(accf)-set(mid))
	#Find mid-Kappa components with low votes and outlier variance (1.5 x expected)
	vartr = np.linalg.lstsq(np.array([np.ones(len(acc)),ctb[acc,1]]).T,ctb[acc,3])[0]
	for cc in accf_mid:
		if ctb[cc,1] <= fmax and ctb[cc,3] > vartr[0]+ctb[cc,1]*vartr[1]*1.5: mid+=[cc]
	mid = list(set(mid))
	#Remove mid from acc
	acc = list(set(accf)-set(mid))
	rej = list(set(np.arange(nc))-set(accf)-set(mid))
	return acc,rej,mid


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

	#ipdb.set_trace()
	vTmix = v.T
	vTmixN =((vTmix.T-vTmix.T.mean(0))/vTmix.T.std(0)).T
	ctb,KRd,betasv,v_T = fitmodels2(catd,v.T,eimum,t2s,tes,mmixN=vTmixN)
	#ctb,KRd,betasv,v_T = fitmodels2__(catd,v.T,eimum,t2s,tes)
	#ctb = fitmodels(betasv,t2s,mu,eimum,tes,sig=sig,fout=None,pow=2)
	ctb = ctb[ctb[:,0].argsort(),:]
	ctb = np.vstack([ctb.T[0:3],sp]).T
	
	np.savetxt('comp_table_pca.txt',ctb[ctb[:,1].argsort(),:][::-1])
	np.savetxt('mepca_mix.1D',v[ctb[:,1].argsort()[::-1],:].T)
	kappas = ctb[ctb[:,1].argsort(),1]
	rhos = ctb[ctb[:,2].argsort(),2]
	fmin,fmid,fmax = getfbounds(ne)
	#kappa_thr = 18.5
	#rho_thr = 18.5
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
	return mmix

def write_split_ts(data,comptable,mmix,suffix=''):
	mdata = fmask(data,mask)
	betas = fmask(get_coeffs(unmask((mdata.T-mdata.T.mean(0)).T,mask),mask,mmix),mask)
	niwrite(unmask(betas[:,acc].dot(mmix.T[acc,:]),mask),aff,'_'.join(['hik_ts',suffix])+'.nii')
	if len(midk)!=0: niwrite(unmask(betas[:,midk].dot(mmix.T[midk,:]),mask),aff,'_'.join(['midk_ts',suffix])+'.nii')
	niwrite(unmask(betas[:,rej].dot(mmix.T[rej,:]),mask),aff,'_'.join(['lowk_ts',suffix])+'.nii')
	niwrite(unmask(fmask(data,mask)-betas.dot(mmix.T),mask),aff,'_'.join(['resid_ts',suffix])+'.nii')

def split_ts(data,comptable,mmix):
	cbetas = get_coeffs(data-data.mean(-1)[:,:,:,np.newaxis],mask,mmix)
	betas = fmask(cbetas,mask)
	hikts=unmask(betas[:,acc].dot(mmix.T[acc,:]),mask)
	return hikts,data-hikts

def writefeats(cbetas,comptable,mmix,suffix=''):
	#Write signal changes (dS)
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
	
	niwrite(unmask(cbetam,mask),aff,'_'.join(['feats',suffix])+'.nii')
	
def computefeats2(data,mmix,mask,normalize=True):
	#Write feature versions of components 
	data = data[mask]
	data_vn = (data-data.mean(axis=-1)[:,np.newaxis])/data.std(axis=-1)[:,np.newaxis]
	data_R = get_coeffs(unmask(data_vn,mask),mask,mmix)[mask]
	data_R[data_R<-.999] = -0.999
	data_R[data_R>.999] = .999
	data_Z = np.arctanh(data_R)
	if normalize:
		#data_Z2 = ((data_Z.T-data_Z.mean(0)[:,np.newaxis])/data_Z.std(0)[:,np.newaxis]).T
		data_Z = (((data_Z.T-data_Z.mean(0)[:,np.newaxis])/data_Z.std(0)[:,np.newaxis])  + (data_Z.mean(0)/data_Z.std(0))[:,np.newaxis]).T
	return data_Z

def writefeats2(data,mmix,mask,suffix=''):
	#Write feature versions of components 
	feats = computefeats2(data,mmix,mask)
	niwrite(unmask(feats,mask),aff,'_'.join(['feats',suffix])+'.nii')

def writect(comptable,ctname=''):
	nc = comptable.shape[0]
	sortab = comptable[comptable[:,1].argsort()[::-1],:]
	if ctname=='': ctname = 'comp_table.txt'
	with open(ctname,'w') as f:
		f.write("#	comp	Kappa	Rho	+Var%	-Var%\n")
		for i in range(nc):
			f.write('%d\t%f\t%f\t%.2f\t%.2f\n'%(sortab[i,0]+1,sortab[i,1],sortab[i,2],sortab[i,3]*100,sortab[i,4]*100))

def writect2(comptable,ctname=''):
	nc = comptable.shape[0]
	sortab = comptable[comptable[:,1].argsort()[::-1],:]
	if ctname=='': ctname = 'comp_table_2.txt'
	open('accepted.txt','w').write(','.join([str(int(cc)) for cc in acc]))
	open('rejected.txt','w').write(','.join([str(int(cc)) for cc in rej]))
	open('midk_rejected.txt','w').write(','.join([str(int(cc)) for cc in midk]))
	with open(ctname,'w') as f:
		f.write("#	comp	Kappa	Rho	+Var%	\n")
		for i in range(nc):
			f.write('%d\t%f\t%f\t%.2f\t%.2f\n'%(sortab[i,0],sortab[i,1],sortab[i,2],sortab[i,3],sortab[i,4]))

def filtermix_fourier(xmat):
	#Save xmat
	if len(xmat.shape)==1: xmat = np.atleast_2d(xmat).T
	np.savetxt('xmat.1D',xmat)
	os.system("waver -dt `3dinfo -tr ../%s` -GAM -inline 1@1 > GammaHR.1D" % (options.data)  )
	#Deconvolve
	os.system("3dTfitter -RHS %s -FALTUNG GammaHR.1D %s_dc 012 0"  % ('xmat.1D','xmat') )
	xmat_dc = np.atleast_2d(np.loadtxt("xmat_dc.1D")).T
	#Build Fourier basis
	os.system("1dBport -band 0 99 -input ../%s  > bport.1D" % options.data)
	bpb = np.loadtxt("bport.1D")
	#Initialize new xmat 
	xmat_r = np.zeros(xmat.shape)
	#For each component, find outlier time points, fit limited 
	#Fourier basis, and expand for Fourier interpolation
	for cc in range(xmat_dc.shape[1]):
		xz = (xmat_dc[:,cc]-np.mean(xmat_dc[:,cc]))/np.std(xmat_dc[:,cc])
		xz_l = xmat_dc[andb([xz>=-2,xz<2])==2,cc]
		bpb_l = bpb[andb([xz>=-2,xz<2])==2,:]
		lss = np.linalg.lstsq(bpb_l,xz_l)
		x_r = bpb.dot(lss[0])
		np.savetxt('_x_r.1D',x_r)
		os.system("waver -dt `3dinfo -tr ../%s` -GAM -input _x_r.1D > _x_rc.1D" % (options.data))
		x_rc = np.loadtxt('_x_rc.1D')[:xmat.shape[0]].T
		x_rc = (x_rc-x_rc.mean())/x_rc.std()
		xmat_r[:,cc] = x_rc
	return xmat_r

	
###################################################################################################
# 						Begin Main
###################################################################################################

if __name__=='__main__':

	parser=OptionParser()
	parser.add_option('-d',"--orig_data",dest='data',help="Spatially Concatenated Multi-Echo Dataset",default=None)
	parser.add_option('-e',"--TEs",dest='tes',help="Echo times (in ms) ex: 15,39,63",default=None)
	parser.add_option('',"--mix",dest='mixm',help="Mixing matrix. If not provided, ME-PCA & ME-ICA (MDP) is done.",default=None)
	parser.add_option('',"--dmix",dest='dmix',help="Design matrix to test for TE-dep. after denoising (serial/greedy simple regression)",default='')
	parser.add_option('',"--extend",dest='extend',action='store_true',help="Extend selection for non-offending components with low-Kappa)",default=False)
	parser.add_option('',"--sourceTEs",dest='ste',help="Source TEs for models. ex: -ste 2,3 ; -ste 0 for all, -1 for opt. com. Default 0.",default=0)	
	parser.add_option('',"--denoiseTE",dest='e2d',help="TE to denoise. Default middle",default=None)	
	parser.add_option('',"--initcost",dest='initcost',help="Initial cost func. for ICA: pow3,tanh(default),gaus,skew",default='tanh')
	parser.add_option('',"--finalcost",dest='finalcost',help="Final cost func, same opts. as initial",default='tanh')	
	parser.add_option('',"--stabilize",dest='stabilize',action='store_true',help="Stabilize convergence by reducing dimensionality, for low quality data",default=False)	
	parser.add_option('',"--conv",dest='conv',help="Convergence limit. Default 1e-5",default='1e-5')
	parser.add_option('',"--kdaw",dest='kdaw',help="Dimensionality augmentation weight (Kappa). Default 0. -1 for low-dimensional ICA",default=0.)
	parser.add_option('',"--rdaw",dest='rdaw',help="Dimensionality augmentation weight (Rho). Default 0. -1 for low-dimensional ICA",default=0.)
	parser.add_option('',"--OC",dest='OC',help="Output optimally combined time series and features",action="store_true",default=False)
	parser.add_option('',"--fout",dest='fout',help="Output Voxelwise Kappa/Rho Maps",action="store_true",default=False)
	parser.add_option('',"--label",dest='label',help="Label for output directory.",default=None)

	(options,args) = parser.parse_args()

	print "-- ME-PCA/ME-ICA Component for ME-ICA v2.5 beta1--"

	if options.tes==None or options.data==None: 
		print "*+ Need at least data and TEs, use -h for help."		
		sys.exit()

	print "++ Loading Data"
	tes = np.fromstring(options.tes,sep=',',dtype=np.float32)
	ne = tes.shape[0]
	catim  = nib.load(options.data)	
	head   = catim.get_header()
	head.extensions = []
	head.set_sform(head.get_sform(),code=1)
	aff = catim.get_affine()
	catd = cat2echos(catim.get_data(),ne)

	nx,ny,nz,Ne,nt = catd.shape
	mu  = catd.mean(axis=-1)
	sig  = catd.std(axis=-1)
	if options.fout: options.fout = aff
	else: options.fout=None
	kdaw = float(options.kdaw)
	rdaw = float(options.rdaw)
	if options.label!=None: dirname='%s' % '.'.join(['TED',options.label])
	else: dirname='TED'
	os.system('mkdir %s' % dirname)
	os.chdir(dirname)
	
	print "++ Computing Mask"
	mask  = makemask(catd)

	print "++ Computing T2* map"
	t2s,s0   = t2smap(catd,mask,tes)
	#Condition values
	cap_t2s = scoreatpercentile(t2s.flatten(),99.5)
	t2s[t2s>cap_t2s*10]=cap_t2s 
	niwrite(s0,aff,'s0v.nii')
	niwrite(t2s,aff,'t2sv.nii')
	
	if options.mixm == None:
		print "++ Doing ME-PCA and ME-ICA with MDP"
		import mdp
		nc,dd = tedpca(options.ste)
		mmix_orig = tedica(dd,cost=options.initcost)
		comptable,KRd,betas,mmix = fitmodels2(catd,mmix_orig,mask,t2s,tes,options.fout,reindex=True)
		acc,rej,midk = selcomps_voter(comptable,KRd,extend=options.extend)
		np.savetxt('meica_mix.1D',mmix)
		del dd
	else:
		mmix_orig = np.loadtxt(options.mixm)
		eim = eimask(np.float64(fmask(catd,mask)))==1
		eimum = np.array(np.squeeze(unmask(np.array(eim,dtype=np.int).prod(1),mask)),dtype=np.bool)
		comptable,KRd,betas,mmix = fitmodels2(catd,mmix_orig,mask,t2s,tes,options.fout)
		acc,rej,midk = selcomps_voter(comptable,KRd,extend=options.extend)

        if options.dmix!='':
            dndata = np.dot(betas[:,:,:,:,acc],(mmix[:,acc].T))
            dndata = uncat2echos(dndata,Ne)
            dmix = np.atleast_2d(np.loadtxt(options.dmix))
            if np.min(dmix.shape)==1 and dmix.shape[1]!=1: dmix = dmix.T
            dbetas = get_coeffs(dndata,np.tile(mask,(1,1,Ne)),dmix,add_const=True)
            dbetas = cat2echos(dbetas,Ne)
            
            #import ipdb;ipdb.set_trace()
            dcomptable = fitmodels(dbetas[:,:,:,0:,:],t2s,mu[:,:,:,0:],eimum,tes[0:],sig=sig[:,:,:,0:],fout=options.fout,pow=2)
            writect(dcomptable,'comp_table_dmix.txt')
            print "++ Writing design matrix component table"
            ks = dcomptable[:,1]
            svars = dcomptable[:,3:5].sum(1)*100.
            dacc = dcomptable[ks>100,0].tolist()
            
            rhos = dcomptable[dacc,2].copy()
            rhos.sort()
            rho_thr = rhos[min(getelbow(rhos)+1,rhos.shape[0]-1)]
            rhos = dcomptable[:,2]

            dacc += dcomptable[andb([ks<=100,ks>np.percentile(ks,66)/2.,svars<100./svars.shape[0]])==3,0].tolist()
            dacc += dcomptable[andb([ks<=100,rhos<=rho_thr,svars<100./svars.shape[0]])==3,0].tolist()
            dacc = set(dacc)
            #import ipdb; ipdb.set_trace()
            drej = list(set(range(dmix.shape[1])).difference(set(dacc)))
            open('dmix_accepted.txt','w').write(','.join([str(int(cc)) for cc in sorted(dacc)]))
            open('dmix_midk.txt','w').write(','.join([str(int(cc)) for cc in sorted(drej)]))
            
	
	print "++ Writing component table"
	writect2(comptable,'comp_table.txt')
	
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
		print "++ Writing signal versions of components"
		#ipdb.set_trace()
		ts_B = get_coeffs(ts,mask,mmix)
		niwrite(ts_B[:,:,:,:],aff,'_'.join(['betas','OC'])+'.nii')
		niwrite(ts_B[:,:,:,acc],aff,'_'.join(['betas_hik','OC'])+'.nii')
		print "++ Writing optimally combined high-Kappa features"
		writefeats2(split_ts(ts,comptable,mmix)[0],mmix[:,acc],mask,'OC2')
	






	


	




