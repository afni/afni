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
int CalcRanksForReHo(float **IND, THD_3dim_dataset *T, int *NTIE,
							int ***MASK,int *DIM);

/*
  average time series of an ROI
*/
int CalcAveRTS(int *LIST, double *RAT, THD_3dim_dataset *T,
              int *DIM, int *Nv);


/*
  Find small neighborhood per voxel, make LIST have list of vox indices
*/
int FindVoxHood(int *LIST, 
					 int *iam,int *DIM,
					 int ***MASK,int HOOD, int *realHOOD );

/* 
	name says it all: take in LIST of vox and their RANKS&TIEDs, and 
	calc reho.
*/
float ReHoIt(int *LIST, float **RANKS, int *TIED, int *DIM, 
				 int ***MASK, int *realHOOD);




#endif /* _RSFC_HEADER_ */
