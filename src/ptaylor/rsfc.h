#ifndef _RSFC_HEADER_
#define _RSFC_HEADER_


#define MAX_R (0.9999999999999999)
#undef  BOBatanhf
#define BOBatanhf(x) ( ((x)<-0.999329f) ? -4.0f                \
                      :((x)>+0.999329f) ? +4.0f : atanhf(x) )
#undef  BOBatanhd
#define BOBatanhd(x) ( ((x)<-0.999329) ? -4.0                \
                      :((x)>+0.999329) ? +4.0 : atanh(x) )

/*
  Fisher Z transform of correlation value R
*/
//float FisherZ( double Rcorr);  outdated-- use other DEF
  

/*
  need to change time series values to ranks, per voxel, and also
  calculate any ties that may occur
*/
//int CalcRanksForReHo(float **IND, THD_3dim_dataset *T, int *NTIE,
//							int ***MASK,int *DIM);
int CalcRanksForReHo(float *IND, int idx, THD_3dim_dataset *T, int *NTIE,
							int TDIM);

/*
  average time series of an ROI
*/
int CalcAveRTS(int *LIST, double *RAT, THD_3dim_dataset *T,
              int *DIM, int *Nv);

/*
  takes neigbor radius, calcs int version, and number of vox therein,
  returned as an int; other one gets shape indices
*/
int IntSpherVol(int *RD, float *NR);
int IntSpherSha(int **HS,int *RD, float *NR);


/*
  Find small neighborhood per voxel, make LIST have list of vox indices
*/
int FindVoxHood(int *LIST, int **HS,
					 int *iam,int *DIM,
					 int ***MASK,int VN, int *realHOOD );

/* 
	name says it all: take in LIST of vox and their RANKS&TIEDs, and 
	calc reho.
*/
float ReHoIt(int *LIST, float **RANKS, int *TIED, int *DIM, 
				 int *realHOOD);

int WB_netw_corr(int Do_r, 
                 int Do_Z,
                 int HAVE_ROIS, 
                 char *prefix, 
                 int NIFTI_OUT,
                 int *NROI_REF,
                 int *Dim,
                 double ***ROI_AVE_TS,
                 int **ROI_LABELS_REF,
                 THD_3dim_dataset *insetTIME,
                 byte *mskd2,
                 int Nmask,
                 int argc,
                 char *argv[]);

/* 
   Take a MxM matrix of Pearson correlation values, invert it, and
   calculate the partial correlation matrix (both r-like and beta-like).
*/
int CalcPartCorrMatr( float **OUT, float **OUTB, float **IN, int M);



#endif /* _RSFC_HEADER_ */
