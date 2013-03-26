#ifndef _RSFC_HEADER_
#define _RSFC_HEADER_

/*
  Fisher Z transform of correlation value R
*/
float FisherZ( double Rcorr);
  

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




#endif /* _RSFC_HEADER_ */
