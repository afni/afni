
import os
from optparse import OptionParser
import numpy as np
import nibabel as nib
from sys import stdout
#import pdb
#import ipdb

def t2smap(catd,tes):
	"""
	t2smap(catd,mask,tes)
	Input:
	catd  has shape (nV,Ne,nt)
	tes   is a 1d numpy array
	Output:
	[t2s,s0] = [(nV,1),(nV,1)]
	"""
	echodata = catd
	Nm,Ne,nt = echodata.shape
	#Compute log linear model
	B = np.reshape(np.abs(echodata), (Nm,Ne*nt)).transpose()
	B = np.log(B)
	x = np.array([np.ones(Ne),-tes])
	X = np.tile(x,(1,nt))
	X = np.sort(X)[:,::-1].transpose()
	#Do fit
	beta,res,rank,sing = np.linalg.lstsq(X,B)
	t2s = 1/beta[1,:].transpose()
	s0  = np.exp(beta[0,:]).transpose()
	#Return
	out = t2s,s0
	return out

def optcom(data,t2s,tes):
	"""
	tsoc = optcom(data,t2s)
	Input:
	data.shape = (nV,Ne,Nt)
	t2s.shape  = (nV,)
	tes.shape  = (Ne,)
	Output:
	tsoc.shape = (nV,ne,Nt)
	"""
	nV,Ne,Nt = data.shape 
	fdat = data
	ft2s = t2s
	tes = tes[np.newaxis,:]
	ft2s = ft2s[:,np.newaxis]
	#Compute weights for average
	alpha = tes * np.exp(-tes /ft2s)
	alpha = np.tile(alpha[:,:,np.newaxis],(1,1,Nt))
	#Average and return
	tsoc  = np.average(fdat,axis = 1,weights=alpha)
	return tsoc

parser=OptionParser()
parser.add_option('-e',"--TEs",dest='tes',help="Echo times (in ms) ex: 15,39,63",default=None)
parser.add_option('-p',"--prefix",dest='prefix',help="Label for output directory.",default=None)
(options,args) = parser.parse_args()

tes = np.array([float(te) for te in options.tes.split(',')])
surf_fns = args
surf_dss = []
com_nodes = None

#Load data and find common nodes
for sds_ii in range(len(surf_fns)):
	sds = np.loadtxt(surf_fns[sds_ii])
	surf_dss.append(sds)
	if sds_ii==0: 
		com_nodes = sds[:,0]
	else:
		com_nodes = np.intersect1d(sds[:,0],com_nodes)

#Load data at common nodes
surf_cat = None
Nv = len(com_nodes)
Nt = sds.shape[1]-1
Ne = len(surf_dss)
for ei in range(Ne): 
	sds = surf_dss[ei]	
	if ei==0: surf_cat = np.zeros([Nv,Ne,Nt])
	sel = np.array([vv in com_nodes for vv in sds[:,0]])
	surf_cat[:,ei,:] = sds[sel,1:]
	
#Compute T2* map
t2s,s0 = t2smap(surf_cat,tes)
tsoc = optcom(surf_cat,t2s,tes)
tsocds = np.hstack([np.atleast_2d(com_nodes).T,tsoc])

#Save
np.savetxt(options.prefix,tsocds)
