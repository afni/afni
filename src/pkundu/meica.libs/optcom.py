
import os
from optparse import OptionParser
import numpy as np
import nibabel as nib
from sys import stdout
#import pdb
import ipdb

def t2smap(catd,mask,tes):
	"""
	t2smap(catd,mask,tes)

	Input:

	catd  has shape (nV,Ne,nt)
	tes   is a 1d numpy array
	"""
	echodata = catd
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

	out = t2s,s0

	return out

def optcom(data,t2s,tes,mask):
	"""
	out = optcom(data,t2s)


	Input:

	data.shape = (nV,Ne,Nt)
	t2s.shape  = (nV,)
	tes.shape  = (Ne,)

	Output:

	out.shape = (nx,ny,nz,Nt)
	"""
	nV,Ne,Nt = data.shape 

	fdat = data
	ft2s = t2s
	
	tes = tes[np.newaxis,:]
	ft2s = ft2s[:,np.newaxis]
	
	alpha = tes * np.exp(-tes /ft2s)
	alpha = np.tile(alpha[:,:,np.newaxis],(1,1,Nt))

	fout  = np.average(fdat,axis = 1,weights=alpha)
	out = unmask(fout,mask)
	print 'Out shape is ', out.shape
	return out

parser=OptionParser()
parser.add_option('',"--data",dest='data',help="Comma separated list of surface files",default=None)
parser.add_option('',"--TEs",dest='tes',help="Echo times (in ms) ex: 15,39,63",default=None)
parser.add_option('',"--prefix",dest='prefix',help="Label for output directory.",default=None)
(options,args) = parser.parse_args()

surf_datasets = options.data.split(',')
surf_cat = None
for sds_ii in range(len(surf_datasets)):
	sds = np.loadtxt(surf_datasets[sds_ii])
	if sds_ii==0: 
		surf_shape = sds.shape
		surf_cat = np.zeros([surf_shape[0],surf_shape[1],len(surf_datasets)])
	surf_cat[:,sds_ii,:] = sds





