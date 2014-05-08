import numpy as np

def fitmodels_direct(catd,mmix,mask,t2s,tes,fout=None,reindex=False,mmixN=None,full_sel=True,debugout=False):
	"""
   	Usage:
   	
   	fitmodels_direct(fout)
	
   	Input:
   	fout is flag for output of per-component TE-dependence maps
   	t2s is a (nx,ny,nz) ndarray
   	tes is a 1d array
   	"""

   	#Compute opt. com. raw data
	tsoc = np.array(optcom(catd,t2s,tes,mask),dtype=float)[mask]
	tsoc_mean = tsoc.mean(axis=-1)
	tsoc_dm = tsoc-tsoc_mean[:,np.newaxis]
	
	#Compute un-normalized weight dataset (features)
	if mmixN == None: mmixN=mmix
	WTS = computefeats2(unmask(tsoc,mask),mmixN,mask,normalize=False)

	#Compute PSC dataset - shouldn't have to refit data
	tsoc_B = get_coeffs(unmask(tsoc_dm,mask),mask,mmix)[mask]
	tsoc_Babs = np.abs(tsoc_B)
	PSC = tsoc_B/tsoc.mean(axis=-1)[:,np.newaxis]*100

	#Compute skews to determine signs based on unnormalized weights, correct mmix & WTS signs based on spatial distribution tails
	from scipy.stats import skew
	signs = skew(WTS,axis=0)
	signs /= np.abs(signs)
	mmix = mmix.copy()
	mmix*=signs
	WTS*=signs
	PSC*=signs
	totvar = (tsoc_B**2).sum()
	totvar_norm = (WTS**2).sum()

	#Compute Betas and means over TEs for TE-dependence analysis
	Ne = tes.shape[0]
	betas = cat2echos(get_coeffs(uncat2echos(catd,Ne),np.tile(mask,(1,1,Ne)),mmix),Ne)
	nx,ny,nz,Ne,nc = betas.shape
	Nm = mask.sum()
	mu = catd.mean(axis=-1)
	tes = np.reshape(tes,(Ne,1))
	fmin,fmid,fmax = getfbounds(ne)

	#Mask arrays
	mumask   = fmask(mu,mask)
	t2smask  = fmask(t2s,mask)
	betamask = fmask(betas,mask)

	if debugout: fout=aff

	#Setup Xmats
	#Model 1
	X1 = mumask.transpose()
	
	#Model 2
	X2 = np.tile(tes,(1,Nm))*mumask.transpose()/t2smask.transpose()
	
	#Tables for component selection
	Kappas = np.zeros([nc])
	Rhos = np.zeros([nc])
	varex = np.zeros([nc])
	varex_norm = np.zeros([nc])
	Z_maps = np.zeros([Nm,nc])
	F_R2_maps = np.zeros([Nm,nc])
	F_S0_maps = np.zeros([Nm,nc])
	Z_clmaps = np.zeros([Nm,nc])
	F_R2_clmaps = np.zeros([Nm,nc])
	F_S0_clmaps = np.zeros([Nm,nc])
	Br_clmaps_R2 = np.zeros([Nm,nc])
	Br_clmaps_S0 = np.zeros([Nm,nc])

	for i in range(nc):

		#size of B is (nc, nx*ny*nz)
		B = np.atleast_3d(betamask)[:,:,i].transpose()
		alpha = (np.abs(B)**2).sum(axis=0)
		varex[i] = (tsoc_B[:,i]**2).sum()/totvar*100.
		varex_norm[i] = (WTS[:,i]**2).sum()/totvar_norm*100.

		#S0 Model
		coeffs_S0 = (B*X1).sum(axis=0)/(X1**2).sum(axis=0)
		SSE_S0 = (B - X1*np.tile(coeffs_S0,(Ne,1)))**2
		SSE_S0 = SSE_S0.sum(axis=0)
		F_S0 = (alpha - SSE_S0)*2/(SSE_S0)
		F_S0_maps[:,i] = F_S0

		#R2 Model
		coeffs_R2 = (B*X2).sum(axis=0)/(X2**2).sum(axis=0)
		SSE_R2 = (B - X2*np.tile(coeffs_R2,(Ne,1)))**2
		SSE_R2 = SSE_R2.sum(axis=0)
		F_R2 = (alpha - SSE_R2)*2/(SSE_R2)
		F_R2_maps[:,i] = F_R2

		#Compute weights as Z-values
		wtsZ=(WTS[:,i]-WTS[:,i].mean())/WTS[:,i].std()
		wtsZ[np.abs(wtsZ)>Z_MAX]=(Z_MAX*(np.abs(wtsZ)/wtsZ))[np.abs(wtsZ)>Z_MAX]
		Z_maps[:,i] = wtsZ 

		#Compute Kappa and Rho
		F_S0[F_S0>F_MAX] = F_MAX
		F_R2[F_R2>F_MAX] = F_MAX
		Kappas[i] = np.average(F_R2,weights=np.abs(wtsZ)**2.)
		Rhos[i] = np.average(F_S0,weights=np.abs(wtsZ)**2.)

	#Tabulate component values
	comptab_pre = np.vstack([np.arange(nc),Kappas,Rhos,varex,varex_norm]).T
	if reindex:
		#Re-index all components in Kappa order
		comptab = comptab_pre[comptab_pre[:,1].argsort()[::-1],:]
		Kappas = comptab[:,1]; Rhos = comptab[:,2]; varex = comptab[:,3]; varex_norm = comptab[:,4]
		nnc = np.array(comptab[:,0],dtype=np.int)
		mmix_new = mmix[:,nnc]
		F_S0_maps = F_S0_maps[:,nnc]; F_R2_maps = F_R2_maps[:,nnc]; Z_maps = Z_maps[:,nnc]
		WTS = WTS[:,nnc]; PSC=PSC[:,nnc]; tsoc_B=tsoc_B[:,nnc]; tsoc_Babs=tsoc_Babs[:,nnc]
		comptab[:,0] = np.arange(comptab.shape[0])
	else:
		comptab = comptab_pre
		mmix_new = mmix

	#Full selection including clustering criteria
	seldict=None
	if full_sel: 
		for i in range(nc):	

			#Save out files
			out = np.zeros((nx,ny,nz,4))
			if fout!=None: 
				ccname = "cc%.3d.nii" % i
			else: ccname = ".cc_temp.nii.gz"

			out[:,:,:,0] = np.squeeze(unmask(PSC[:,i],mask))
			out[:,:,:,1] = np.squeeze(unmask(F_R2_maps[:,i],mask))
			out[:,:,:,2] = np.squeeze(unmask(F_S0_maps[:,i],mask))
			out[:,:,:,3] = np.squeeze(unmask(Z_maps[:,i],mask))
			niwrite(out,fout,ccname)
			os.system('3drefit -sublabel 0 PSC -sublabel 1 F_R2  -sublabel 2 F_SO -sublabel 3 Z_sn %s 2> /dev/null > /dev/null'%ccname)

			csize = np.max([int(Nm*0.0005)+5,20])

			#Do simple clustering on F
			os.system("3dcalc -overwrite -a %s[1..2] -expr 'a*step(a-%i)' -prefix .fcl_in.nii.gz -overwrite" % (ccname,fmin))
			os.system('3dmerge -overwrite -dxyz=1 -1clust 1 %i -doall -prefix .fcl_out.nii.gz .fcl_in.nii.gz' % (csize))
			sel = fmask(nib.load('.fcl_out.nii.gz').get_data(),mask)!=0
			sel = np.array(sel,dtype=np.int)
			F_R2_clmaps[:,i] = sel[:,0]
			F_S0_clmaps[:,i] = sel[:,1]

			#Do simple clustering on Z at p<0.05
			sel = spatclust(None,mask,csize,1.95,head,aff,infile=ccname,dindex=3,tindex=3)
			Z_clmaps[:,i] = sel

			#Do simple clustering on ranked signal-change map
			countsigFR2 = F_R2_clmaps[:,i].sum()
			countsigFS0 = F_S0_clmaps[:,i].sum()
			Br_clmaps_R2[:,i] = spatclust(rankvec(tsoc_Babs[:,i]),mask,csize,max(tsoc_Babs.shape)-countsigFR2,head,aff)
			Br_clmaps_S0[:,i] = spatclust(rankvec(tsoc_Babs[:,i]),mask,csize,max(tsoc_Babs.shape)-countsigFS0,head,aff)

		seldict = {}
		selvars = ['Kappas','Rhos','WTS','varex','Z_maps','F_R2_maps','F_S0_maps',\
			'Z_clmaps','F_R2_clmaps','F_S0_clmaps','tsoc_B','Br_clmaps_R2','Br_clmaps_S0']
		for vv in selvars:
			seldict[vv] = eval(vv)
		
		if debugout or ('DEBUGOUT' in args):
			#Package for debug
			import cPickle as cP
			import zlib
			try: os.system('mkdir compsel.debug')
			except: pass
			selvars = ['Kappas','Rhos','WTS','varex','Z_maps','Z_clmaps','F_R2_clmaps','F_S0_clmaps','Br_clmaps_R2','Br_clmaps_S0']
			for vv in selvars:
				with open('compsel.debug/%s.pkl.gz' % vv,'wb') as ofh:
					print "Writing debug output: compsel.debug/%s.pkl.gz" % vv
					ofh.write(zlib.compress(cP.dumps(eval(vv))))
					ofh.close()

	return seldict,comptab,betas,mmix_new


def selcomps(seldict,debug=False,olevel=1,oversion=99):

	#import ipdb

	#Dump dictionary into variable names
	for key in seldict.keys(): exec("%s=seldict['%s']" % (key,key))

	#List of components
	midk = []
	empty = []
	nc = np.arange(len(Kappas))
	ncl = np.arange(len(Kappas))

	#If user has specified 
	try:
		if options.manacc:
			acc = sorted([int(vv) for vv in options.manacc.split(',')])
			midk = []
			rej = sorted(np.setdiff1d(ncl,acc))
			return acc,rej,midk #Add string for empty
	except: 
		pass

	"""
	Do some tallies for no. of significant voxels
	"""
	countsigZ = Z_clmaps.sum(0)
	countsigFS0 = F_S0_clmaps.sum(0)
	countsigFR2 = F_R2_clmaps.sum(0)
	countnoise = np.zeros(len(nc))
	countsvn = np.zeros(len(nc))

	"""
	Make table of dice values
	"""
	dice_table = np.zeros([nc.shape[0],2])
	csize = np.max([int(mask.sum()*0.0005)+5,20])
	for ii in ncl:
		dice_FR2 = dice(Br_clmaps_R2[:,ii],F_R2_clmaps[:,ii])
		dice_FS0 = dice(Br_clmaps_S0[:,ii],F_S0_clmaps[:,ii])
		dice_table[ii,:] = [dice_FR2,dice_FS0] #step 3a here and above
	dice_table[np.isnan(dice_table)]=0

	"""
	Make table of noise gain
	"""
	tt_table = np.zeros([len(nc),4])
	counts_FR2_Z = np.zeros([len(nc),2])
	for ii in nc:
		comp_noise_sel = andb([np.abs(Z_maps[:,ii])>1.95,Z_clmaps[:,ii]==0])==2
		countnoise[ii] = np.array(comp_noise_sel,dtype=np.int).sum()
		countsvn[ii] = np.array(Z_clmaps[:,ii]==1,dtype=np.int).sum()/countnoise[ii]
		noise_FR2_Z = np.log10(np.unique(F_R2_maps[comp_noise_sel,ii]))
		signal_FR2_Z  = np.log10(np.unique(F_R2_maps[Z_clmaps[:,ii]==1,ii]))
		counts_FR2_Z[ii,:] = [len(signal_FR2_Z),len(noise_FR2_Z)]
		tt_table[ii,:2] = stats.ttest_ind(signal_FR2_Z,noise_FR2_Z,equal_var=False) 
	
	"""
	Assemble decision table
	"""
	d_table_full = np.vstack([Kappas,Rhos,dice_table.T,tt_table[:,0]]).T
	d_table_rank = np.vstack([len(nc)-rankvec(Kappas), len(nc)-rankvec(dice_table[:,0]), \
		 len(nc)-rankvec(tt_table[:,0]), rankvec(countnoise) ]).T
	d_table_score = d_table_rank.sum(1)

	"""
	Step 1: Reject anything that's obviously an artifact
	a. Estimate a null variance
	"""
	rej = ncl[andb([Rhos*1.1>Kappas,countsigFS0*1.1>countsigFR2])>0]
	rej = np.union1d(rej,ncl[andb([dice_table[:,1]>dice_table[:,0],varex>np.median(varex)])==2])
	ncl = np.setdiff1d(ncl,rej)
	varex_ub_p = np.median(varex)

	"""
	Step 2: Make a  guess for what the good components are, and good component properties
	a. Not outlier variance
	b. Kappa>kappa_elbow
	c. Rho<Rho_elbow
	d. High R2* dice compared to S0 dice
	e. Gain of F_R2 in clusters vs noise
	f. Estimate a low and high variance, and number of sig. voxels
	"""
	ncls = ncl.copy()
	for nn in range(3): ncls = ncls[1:][(varex[ncls][1:]-varex[ncls][:-1])<varex_ub_p] #Step 2a
	Kappas_lim = Kappas[Kappas<100]
	Kappas_elbow = min(scoreatpercentile(F_R2_maps.flatten(),95),Kappas[getelbow(Kappas)],Kappas_lim[getelbow(Kappas_lim)])
	Rhos_lim = Rhos[ncls]
	Rhos_elbow = Rhos_lim[getelbow(Rhos_lim)]
	good_guess = ncls[andb([Kappas[ncls]>=Kappas_elbow, Rhos[ncls]<Rhos_elbow, dice_table[ncls,0]>2*dice_table[ncls,1],tt_table[ncls,0]>0 ])==4]
	varex_lb = scoreatpercentile(varex[good_guess],25)
	varex_ub = scoreatpercentile(varex[good_guess],90)
	countsigZ_lb = scoreatpercentile(countsigZ[rej],25)

	"""
	Step 3: Get rid of drift components based on emptiness and 
	 ultra high variance
	"""
	midkadd = ncl[varex[ncl]>7.5*varex_ub]
	midk = np.union1d(midkadd, ncl[andb([varex[ncl]>3*varex_ub, countsigZ[ncl]<countsigZ_lb])==2  ]  )
	ncl = np.setdiff1d(ncl,midk)

	if debug:
		import ipdb
		ipdb.set_trace()

	"""
	Step 4: Get rid of midk components which have higher than max decision score and high variance
	"""
	max_good_d_score = 1.25*len(good_guess)*d_table_rank.shape[1] #25% leniency factor
	midkadd = ncl[andb([d_table_score[ncl] > max_good_d_score, varex[ncl] > varex_ub])==2]
	midkadd = np.intersect1d(ncl[varex[ncl]>varex_lb],ncl[tt_table[ncl,0]<0])
	midk = np.union1d(midkadd, midk  )
	ncl = np.setdiff1d(ncl,midk)

	"""
	Step 5: Find components to ignore
	"""
	emptycand = np.setdiff1d(ncl,np.union1d(good_guess, ncl[varex[ncl]>varex_lb]))
	emptycand = np.setdiff1d(emptycand, emptycand[d_table_score[emptycand]<scoreatpercentile(d_table_score[good_guess],90)]) 
	empty = np.array(np.union1d(empty,emptycand),dtype=np.int)
	ncl = np.setdiff1d(ncl,empty)

	if debug:
		import ipdb
		ipdb.set_trace()

	"""
	Step 6: Scrub the tail and remove low Kappa spike components
	"""
	if len(ncl)>len(good_guess):
		tail = ncl[len(good_guess):]
		midkadd = np.intersect1d(tail,ncl[rankvec(varex[ncl])-rankvec(Kappas[ncl])>len(ncl)/2])
		try:
			kurts = stats.kurtosis(mmix)
			tail_scrub = np.intersect1d(tail,ncl[kurts[ncl]>4*scoreatpercentile(kurts[good_guess],90)])
			midkadd = np.union1d(midkadd,tail_scrub)
		except:
			pass
	midk = np.union1d(midk,midkadd)
	ncl = np.setdiff1d(ncl,midk)

	if debug:
		import ipdb
		ipdb.set_trace()

	return list(sorted(ncl)),list(sorted(rej)),list(sorted(midk)),list(sorted(empty))

