#ifndef _DOTRACKIT_HEADER_
#define _DOTRACKIT_HEADER_

#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_statistics_double.h>


#define EPS_V (0.000001) // for eigvec 'vel' to not have badness dividing
#define CONV (PI/180)
#define PIo2 (PI/2)
#define TWOPI (PI*2)
#define EPS_MASK (0.001) // theshold for masked data to be ignored
#define EPS_L (0.00001)
#define MINEIG (0) // minimum allowed eigenvalue

#ifdef XORG7
   #define CORR_FUN(a,b,n) (THD_pearson_corrd((n),(a),(b)))	
   #define GSL_RAN gsl_ran_gaussian
#else
   #define CORR_FUN(a,b,n) (gsl_stats_correlation((a), 1, (b), 1, (n)))
   #define GSL_RAN gsl_ran_gaussian_ziggurat
#endif

// Added in Feb,2015.  
int ByeByeBundle( int A,
                  int B,
                  int NET,
                  int **Prob_grid,
                  float ***Prob_grid_L,
                  float ***Param_grid,
                  int L_paramgrid,
                  int ***NETROI,
                  int L_netroi,
                  int *NROI );

int MatrInd_to_FlatUHT_DIAG_P1(int i, int j, int N);
int MatrInd_to_FlatUHT_DIAG_M(int i, int j, int N);

int MatrInd_to_FlatUHT(int i, int j, int N);
int FlatUHT_Len(int N);

int ViveLeRoi(THD_3dim_dataset *REF, int **ROILIST, int **INVLIST, 
				  int *NUMROI, int *INVROI);

int CheckNotMask(int id, int br, short **amask, int AO);

int ScoreTrackGrid_M( float ***PG,int idx, int h, int C,
                      THD_3dim_dataset **inset, int bot, int top);

int TrackItP_NEW_M( int NHAR, short *DirPerVox, int SEL, float **CC,
                    int *IND, float *PHYSIND, 
                    float *Edge, int *dim, float minFA, 
                    float maxAng, int arrMax, 
                    int **T, float **flT, int FB, float *physL,
                    int ***ID2);

int Setup_Labels_Indices_Unc_M_both(int *Dim, int ***mskd, int ***INDEX, 
                                    int ***INDEX2, float **UNC,
                                    float **coorded, float **copy_coorded, 
                                    THD_3dim_dataset *insetFA, 
                                    short *DirPerVox,
                                    int N_HAR,
                                    THD_3dim_dataset **insetV, 
                                    THD_3dim_dataset *insetUC,
                                    float unc_minei_std, float unc_minfa_std,
                                    int N_nets, int *NROI,
                                    THD_3dim_dataset *mset1, int **MAPROI, 
                                    int **INV_LABELS, int ***NETROI);

int Setup_Ndir_per_vox( int N_HAR, int *Dim, int ***mskd,
                        int ***INDEX, 
                        int ***INDEX2,
                        THD_3dim_dataset **insetHARDIR,
                        short *DirPerVox);

int DTI_Perturb_M( int *Dim, int ***mskd, int ***INDEX, int ***INDEX2,
                   float **UNC, float **coorded, float **copy_coorded, 
                   gsl_rng *r, 
                   THD_3dim_dataset **insetV);

int HARDI_Perturb( int *Dim, int ***mskd, int ***INDEX, int ***INDEX2,
                   float **UNC, float **coorded, float **copy_coorded, 
                   gsl_rng *r, short *DirPerVox);

int Two_DOF_Rot( float *X, float *Y, 
                 double POL, double AZIM, float rot[3][3] );

int WritebasicProbFiles( int N_nets, int Ndata, int Nvox, 
                         char *prefix, THD_3dim_dataset *insetFA,
                         int *TV_switch, char *voxel_order, int *NROI,
                         int ***NETROI, int ***mskd, int ***INDEX2, int *Dim,
                         THD_3dim_dataset *dsetn, int argc, char *argv[],
                         char ***ROI_STR_LAB, int NameLabelsOut,
                         Dtable *roi_table,
                         int **roi_labs, int PAIR_POWERON,
                         int NIFTI_OUT);

int WriteIndivProbFiles( int N_nets, int Ndata, int Nvox, int **Prob_grid,
                         char *prefix, THD_3dim_dataset *insetFA,
                         int *TV_switch, char *voxel_order, int *NROI,
                         int ***NETROI, int ***mskd,int ***INDEX2, int *Dim,
                         THD_3dim_dataset *dsetn, int argc, char *argv[],
                         float ***Param_grid, int DUMP_TYPE,
                         int DUMP_ORIG_LABS, int **ROI_LABELS, int POST_IT,
                         char ***ROI_STR_LAB, int NameLabelsOut,
                         int NIFTI_OUT);

// lazy now, someday will write these unified/smarter.
//int Write_Running_Opts_DET(logic );
//int Write_Running_Opts_MINIP( );
//int Write_Running_Opts_PROB( );

#endif /* _DOTRACKIT_HEADER_ */








