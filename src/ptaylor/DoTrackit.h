#ifndef _DOTRACKIT_HEADER_
#define _DOTRACKIT_HEADER_

   /* PT, you've got this variable defined in two .h files 
      and with differing values, a very bad idea. 
      It is best you give the constants differing names if
      they are to have different values or one name defined
      in one .h file only. */
#define EPS_V (0.000001) // for eigvec 'vel' to not have badness dividing
#define CONV (PI/180)
#define PIo2 (PI/2)
#define TWOPI (PI*2)
#define EPS_MASK (0.001) // theshold for masked data to be ignored
#define EPS_L (0.00001)
#define MINEIG (0) // minimum allowed eigenvalue

int ViveLeRoi(THD_3dim_dataset *REF, int **ROILIST, int **INVLIST, 
				  int *NUMROI, int *INVROI);

int CheckNotMask(int id, int br, short **amask, int AO);

int ScoreTrackGrid( float ****PG,int idx, int h, int C, int B, 
                    THD_3dim_dataset *FA, THD_3dim_dataset *MD, 
                    THD_3dim_dataset *L1);

int TrackIt( float ****CC, int *IND, float *PHYSIND, 
             float *Edge, int *dim, float minFA, 
             float maxAng, int arrMax, 
             int **T, float **flT, int FB, float *physL);

int TrackItP_NEW( float **CC, int *IND, float *PHYSIND, 
                  float *Edge, int *dim, float minFA, 
                  float maxAng, int arrMax, 
                  int **T, float **flT, int FB, float *physL,
                  int ***ID2);

int TrackItP_NEW_M( int NHAR, short *DirPerVox, int SEL, float **CC,
                    int *IND, float *PHYSIND, 
                    float *Edge, int *dim, float minFA, 
                    float maxAng, int arrMax, 
                    int **T, float **flT, int FB, float *physL,
                    int ***ID2);

int DTI_Setup_Labels_Indices_Unc( int *Dim, int ***mskd, int ***INDEX, 
                                  int ***INDEX2, float **UNC,
                                  float **coorded, float **copy_coorded, 
                                  THD_3dim_dataset *insetFA, 
                                  THD_3dim_dataset *insetV1, 
                                  THD_3dim_dataset *insetV2, 
                                  THD_3dim_dataset *insetV3,
                                  THD_3dim_dataset *insetUC,
                                  float unc_minei_std, float unc_minfa_std,
                                  int N_nets, int *NROI,
                                  THD_3dim_dataset *mset1, int **MAPROI, 
                                  int **INV_LABELS, int ****NETROI);

int HARDI_Setup_Ndir_per_vox( int N_HAR, int *Dim, int ***mskd,
                              int ***INDEX, 
                              int ***INDEX2,
                              THD_3dim_dataset *insetHARDIR,
                              short *DirPerVox);

int DTI_Perturb( int *Dim, int ***mskd, int ***INDEX, int ***INDEX2,
                 float **UNC, float **coorded, float **copy_coorded, 
                 gsl_rng *r, 
                 THD_3dim_dataset *insetFA, THD_3dim_dataset *insetV1, 
                 THD_3dim_dataset *insetV2, THD_3dim_dataset *insetV3);

int HARDI_Perturb( int *Dim, int ***mskd, int ***INDEX, int ***INDEX2,
                   float **UNC, float **coorded, float **copy_coorded, 
                   gsl_rng *r, short *DirPerVox, 
                   THD_3dim_dataset *insetFA,
                   THD_3dim_dataset *insetHARDIR );

int Two_DOF_Rot( int NN, float *X, float *Y, 
                 double POL, double AZIM, float rot[3][3] );

int WriteBasicProbFiles( int N_nets, int Ndata, int Nvox, 
                         char *prefix, THD_3dim_dataset *insetFA,
                         int *TV_switch, char *voxel_order, int *NROI,
                         int ****NETROI, int ***mskd, int ***INDEX2, int *Dim,
                         THD_3dim_dataset *dsetn, int argc, char *argv[],
                         int **roi_labs);

int WriteIndivProbFiles( int N_nets, int Ndata, int Nvox, int ***Prob_grid,
                         char *prefix, THD_3dim_dataset *insetFA,
                         int *TV_switch, char *voxel_order, int *NROI,
                         int ****NETROI, int ***mskd,int ***INDEX2, int *Dim,
                         THD_3dim_dataset *dsetn, int argc, char *argv[],
                         float ****Param_grid, int DUMP_TYPE,
                         int DUMP_ORIG_LABS, int **ROI_LABELS, int POST_IT);


#endif /* _DOTRACKIT_HEADER_ */








