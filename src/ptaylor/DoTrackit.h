#ifndef _DOTRACKIT_HEADER_
#define _DOTRACKIT_HEADER_

   /* PT, you've got this variable defined in two .h files 
      and with differing values, a very bad idea. 
      It is best you give the constants differing names if
      they are to have different values or one name defined
      in one .h file only. */
#define EPS_V (0.000001) // for eigvec 'vel' to not have badness dividing
#define CONV (3.141592654/180)
#define EPS_MASK (0.001) // theshold for masked data to be ignored
#define EPS_L (0.00001)
#define PIo2 (3.141592654/2)
#define MINEIG (0) // minimum allowed eigenvalue

int ViveLeRoi(THD_3dim_dataset *REF, int **ROILIST, int **INVLIST, 
				  int *NUMROI, int *INVROI);

int CheckNotMask(int id, int br, short **amask, int AO);

int ScoreTrackGrid(float ****PG,int idx, int h, int C, int B, 
						 THD_3dim_dataset *FA, THD_3dim_dataset *MD, 
						 THD_3dim_dataset *L1);

int TrackIt(float ****CC, int *IND, float *PHYSIND, 
            float *Edge, int *dim, float minFA, 
	    float maxAng, int arrMax, 
            int **T, float **flT, int FB, float *physL);
int TrackItP(float **CC, int *IND, float *PHYSIND, 
	     float *Edge, int *dim, float minFA, 
	     float maxAng, int arrMax, 
	     int **T, float **flT, int FB, float *physL,
	     int ***ID2);

int WriteBasicProbFiles(int N_nets, int Ndata, int Nvox, 
								char *prefix,THD_3dim_dataset *insetFA,
								int *TV_switch,char *voxel_order,int *NROI,
								int ****NETROI,int ***mskd,int ***INDEX2,int *Dim,
								THD_3dim_dataset *dsetn,int argc, char *argv[]);

int WriteIndivProbFiles(int N_nets, int Ndata, int Nvox, int ***Prob_grid,
								char *prefix,THD_3dim_dataset *insetFA,
								int *TV_switch,char *voxel_order,int *NROI,
								int ****NETROI,int ***mskd,int ***INDEX2,int *Dim,
								THD_3dim_dataset *dsetn,int argc, char *argv[],
								float ****Param_grid, int DUMP_TYPE,
								int DUMP_ORIG_LABS, int **ROI_LABELS, int POST_IT);


#endif /* _DOTRACKIT_HEADER_ */








